const std = @import("std");
const parsing = @import("wff-parsing.zig");
const debug = std.debug;

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};


pub fn WffParser(comptime Parser: type) type {
    const expected_signature = "pub fn parse(self, std.mem.Allocator, []const u8) !ParseTree(...)";
    if (!@hasDecl(Parser, "parse")) {
        @compileError("Parser type must have a parse method with signature " ++ expected_signature);
    }
    // TODO: Check Parser has necessary method

    //const OldParser = parsing.TestParserType;
    const OldParseTree = parsing.ParseTree(parsing.TestToken);

    const NewParseTree = void;
    const ParseReturnType = @typeInfo(@typeInfo(@TypeOf(Parser.parse)).Fn.return_type.?).ErrorUnion.payload;
    if (ParseReturnType != OldParseTree and ParseReturnType != NewParseTree) {
        @compileError("Cannot use this parser: there is no function to convert its parse tree to a WffTree");
    }

    return struct {
        const Self = @This();

        parser: Parser,

        pub fn init(parser: Parser) Self {
            return Self{ .parser = parser };
        }

        pub fn parse(self: Self, allocator: std.mem.Allocator, wff_string: []const u8) !Wff {
            const parse_tree = try self.parser.parse(allocator, wff_string);
            defer parse_tree.deinit();

            const wff_tree = try ret: { 
                if (ParseReturnType == OldParseTree) {
                    break :ret fromOldGrammar(allocator, parse_tree);
                } else if (ParseReturnType == NewParseTree) {
                    break :ret fromNewGrammar(allocator, parse_tree);
                } else {
                    unreachable;
                }
            };
            errdefer wff_tree.deinit();

            return Wff { .allocator = allocator, .wff_tree = wff_tree };
        }

        fn oldOperatorToWffBinaryOperator(operator: parsing.TestToken) WffTree.BinaryOperator {
            return switch(operator) {
                .Operator => |op| switch(op) {
                    .Not => std.debug.panic("oldOperatorToWffBinaryOperator called with unary operator", .{}),
                    .And => .and_,
                    .Or => .or_,
                    .Cond => .cond,
                    .Bicond => .bicond,
                },
                else => std.debug.panic("oldOperatorToWffBinaryOperator called with non operator token argument", .{}),
            };
        }

        // TODO: defer / cleanup on error
        fn fromOldGrammar(allocator: std.mem.Allocator, parse_tree: OldParseTree) !WffTree {
            const wff_root = try WffTree.Node.initKindUndefined(allocator, null);

            var parse_iter = parse_tree.iterPreOrder();
            var wff_iter = wff_root.iterPreOrder();
            var wff_node: ?*WffTree.Node = wff_iter.next().?;
            while (parse_iter.next()) |parse_node| {
                switch(parse_node.kind) {
                    .nonleaf => |children| {
                        // Get operator
                        const operatorToken = ret: {
                            if (children.len == 5) {
                                break :ret children[2].kind.leaf;
                            } else if (children.len == 2) {
                                break :ret children[0].kind.leaf;
                            // In case of 1 child, this indicates the next node 
                            // is a temrinal / proposition, so we'll simply 
                            // advance to it
                            } else if (children.len == 1) {
                                continue;
                            } else {
                                std.debug.panic("nonleaf parse tree node has unexpected number of children", .{});
                            }
                        };
                        //wff_node = wff_iter.next().?;
                        // Assign operator to current wff node
                        switch(operatorToken) {
                            .Operator => |op| switch(op) {
                                .Not => wff_node.?.kind = .{
                                    .unary_operator = .{ 
                                        .operator = .not,
                                        .arg = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                    },
                                },
                                .And, .Or, .Cond, .Bicond => wff_node.?.kind = .{
                                    .binary_operator = .{
                                        .operator = oldOperatorToWffBinaryOperator(operatorToken),
                                        .arg1 = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                        .arg2 = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                    }
                                }
                            },
                            else => std.debug.panic("nonleaf parse node does not have an operator in the expected position in its children", .{})
                        }
                        wff_node = wff_iter.next();
                    },
                    .leaf => |token| {
                        //wff_node = wff_iter.next().?;
                        switch(token) {
                            .Proposition => |prop| {
                                wff_node.?.kind = .{ .proposition_variable = try allocator.dupe(u8, prop.string) };
                                wff_node = wff_iter.next();
                            },
                            else => {}
                        }
                    }
                }
            }
            return WffTree{ .allocator = allocator, .root = wff_root };
        }

        fn fromNewGrammar(allocator: std.mem.Allocator, parse_tree: NewParseTree) !WffTree {
            _ = allocator;
            _ = parse_tree;
            return WffError.BadSubstitution;
        }
    };
}

test "WffParser.fromOldGrammar: ((a v b) ^ ~c)" {
    const allocator = std.testing.allocator;

    // Build expected tree for (a v b) ^ ~c
    var root = WffTree.Node {
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ 
        .binary_operator = .{
            .operator = .or_,
            .arg1 = &a,
            .arg2 = &b,
        }
    };

    var right_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node {
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ 
        .unary_operator = .{
            .operator = .not,
            .arg = &c,
        }
    };

    root.kind = .{
        .binary_operator = .{
            .operator = .and_,
            .arg1 = &left_branch,
            .arg2 = &right_branch,
        }
    };

    const expected_tree = WffTree { .allocator = allocator, .root = &root };
    
    const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "((a v b) ^ ~c)");
    defer parse_tree.deinit();
    const actual_tree = try WffParser(parsing.TestParserType).fromOldGrammar(allocator, parse_tree);
    defer actual_tree.deinit();

    var expected_it = expected_tree.iterPreOrder();
    var actual_it = actual_tree.iterPreOrder();
    while (expected_it.next()) |expected_node| {
        const actual_node = actual_it.next().?;
        try std.testing.expectEqual(std.meta.activeTag(expected_node.kind), std.meta.activeTag(actual_node.kind));
        switch(expected_node.kind) {
            .unary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.unary_operator.operator),
            .binary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.binary_operator.operator),
            .proposition_variable => |s| try std.testing.expectEqualStrings(s, actual_node.kind.proposition_variable)
        }
    }
}

pub const WffTree = struct {
    const Self = @This();

    const UnaryOperator = enum {
        not,
    };

    const BinaryOperator = enum {
        and_,
        or_,
        cond,
        bicond,
    };

    pub const PreOrderIterator = struct {
        start: *Node,
        current: ?*Node,
        done: bool,

        // Traverse back up the tree and return the first unvisited node to the
        // right. If we're traversing up the rightmost branch and reach the root
        // return null.
        fn backtrack(self: PreOrderIterator) ?*Node {
            var node = self.current.?;
            while (node.parent) |parent| {
                switch(parent.kind) {
                    .unary_operator => node = parent,
                    .binary_operator => |op| {
                        if (node == op.arg1) {
                            return op.arg2;
                        } else {
                            node = parent;
                        }
                    },
                    .proposition_variable => unreachable,
                }
            }
            return null;
        }

        pub fn next(self: *PreOrderIterator) ?*Node {
            if (self.done) return null;

            if (self.current == null) {
                self.current = self.start;
                return self.current;
            }

            self.current = switch(self.current.?.kind) {
                .unary_operator => |op| op.arg,
                .binary_operator => |op| op.arg1,
                .proposition_variable => self.backtrack(),
            };

            if (self.current == null) {
                self.done = true;
            }

            return self.current;
        }
    };

    pub const PostOrderIterator = struct {
        root: *Node,
        next_node: ?*Node,

        fn isLastSiblingNode(node: *Node) bool {
            const parent = node.parent orelse return true; 

            return switch(parent.kind) {
                .unary_operator => true,
                .binary_operator => |op| node == op.arg2,
                .proposition_variable => unreachable,
            };
        }


        pub fn next(self: *PostOrderIterator) ?*Node {
            const current_node = self.next_node orelse return null;
            
            if (isLastSiblingNode(current_node)) {
                self.next_node = current_node.parent;
                return current_node;
            }

            // We know parent is not null, else we already would have returned.
            self.next_node = switch(current_node.parent.?.kind) {
                .binary_operator => |op| op.arg2,
                .unary_operator, .proposition_variable => unreachable,
            }; 

            while (true) {
                switch(self.next_node.?.kind) {
                    .unary_operator => |op| self.next_node = op.arg,
                    .binary_operator => |op| self.next_node = op.arg1,
                    .proposition_variable => break,
                }
            }

            return current_node;
        }
    };

    pub const Node = struct {
        parent: ?*Node,
        kind: union(enum) {
            unary_operator: struct {
                operator: UnaryOperator,
                arg: *Node,
            },
            binary_operator: struct {
                operator: BinaryOperator,
                arg1: *Node,
                arg2: *Node,
            },
            proposition_variable: []const u8
        },

        fn initKindUndefined(allocator: std.mem.Allocator, parent: ?*Node) !*Node {
            var node = try allocator.create(Node);
            errdefer allocator.destroy(node);

            node.parent = parent;
            return node;
        }

        fn iterPreOrder(self: *Node) PreOrderIterator {
            return PreOrderIterator{ .start = self, .current = null, .done = false };
        }

        fn iterPostOrder(self: *Node) PostOrderIterator {
            var start = self;
            while (true) {
                switch(start.kind) {
                    .unary_operator => |op| start = op.arg,
                    .binary_operator => |op| start = op.arg1,
                    .proposition_variable => break,
                }
            }
            return PostOrderIterator{ .root = self, .next_node = start };
        }
    };

    allocator: std.mem.Allocator,
    root: *Node,

    pub fn deinit(self: Self) void {
        var it = self.iterPostOrder();
        while (it.next()) |node| {
            switch(node.kind) {
                .proposition_variable => |s| self.allocator.free(s),
                .unary_operator, .binary_operator => {},
            }
            self.allocator.destroy(node);
        }
    }

    pub fn iterPreOrder(self: Self) PreOrderIterator {
        return self.root.iterPreOrder();
    } 

    pub fn iterPostOrder(self: Self) PostOrderIterator {
        return self.root.iterPostOrder();
    }
};

test "WffTreePreOrderIterator: ((a v b) ^ ~c)" {
    // (a v b) ^ ~c
    var root = WffTree.Node {
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ 
        .binary_operator = .{
            .operator = .or_,
            .arg1 = &a,
            .arg2 = &b,
        }
    };

    var right_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node {
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ 
        .unary_operator = .{
            .operator = .not,
            .arg = &c,
        }
    };

    root.kind = .{
        .binary_operator = .{
            .operator = .and_,
            .arg1 = &left_branch,
            .arg2 = &right_branch,
        }
    };
    
    var it = root.iterPreOrder();

    try std.testing.expectEqual(&root, it.next().?);
    try std.testing.expectEqual(&left_branch, it.next().?);
    try std.testing.expectEqual(&a, it.next().?);
    try std.testing.expectEqual(&b, it.next().?);
    try std.testing.expectEqual(&right_branch, it.next().?);
    try std.testing.expectEqual(&c, it.next().?);
    try std.testing.expectEqual(null, it.next());
}

test "WffTreePostOrderIterator: ((a v b) ^ ~c)" {
    // (a v b) ^ ~c
    var root = WffTree.Node {
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node {
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ 
        .binary_operator = .{
            .operator = .or_,
            .arg1 = &a,
            .arg2 = &b,
        }
    };

    var right_branch = WffTree.Node {
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node {
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ 
        .unary_operator = .{
            .operator = .not,
            .arg = &c,
        }
    };

    root.kind = .{
        .binary_operator = .{
            .operator = .and_,
            .arg1 = &left_branch,
            .arg2 = &right_branch,
        }
    };
    
    var it = root.iterPostOrder();

    try std.testing.expectEqual(&a, it.next().?);
    try std.testing.expectEqual(&b, it.next().?);
    try std.testing.expectEqual(&left_branch, it.next().?);
    try std.testing.expectEqual(&c, it.next().?);
    try std.testing.expectEqual(&right_branch, it.next().?);
    try std.testing.expectEqual(&root, it.next().?);
    try std.testing.expectEqual(null, it.next());
}

// TODO put in Wff namespace / scope
pub const Match = struct {
    const Self = @This();
    const WffType = Wff;
    const ParseTreeType = parsing.ParseTree(parsing.TestToken);

    wff: *const WffType,
    parent: *ParseTreeType.Node,
    matches: parsing.MatchHashMap,

    pub fn deinit(self: *Self) void {
        self.matches.deinit();
    }

    /// Lookup a match in the hashmap and build a new Wff for it if it exists
    pub fn getBuildWff(self: Self, allocator: std.mem.Allocator, key: []const u8) !?WffType {
        const node = self.matches.get(key) orelse return null;
        return try WffType.initFromNode(allocator, node);
    }

    pub fn replace(self: Self, pattern: WffType) !WffType {
        var result = try pattern.parse_tree.copy();

        // First we substitute variables into the pattern
        var it = result.iterPreOrder();
        while (it.next()) |node| switch (node.kind) {
            .leaf => |tok| switch (tok) {
                .Proposition => |prop| {
                    if (self.matches.get(prop.string)) |wff_node| {
                        defer result.allocator.free(prop.string);
                        const match_copy = try wff_node.copy(result.allocator);
                        defer result.allocator.destroy(match_copy);
                        const old_data = node.parent.?.kind.nonleaf;
                        defer result.allocator.free(old_data);

                        node.parent.?.kind = match_copy.kind;
                        for (node.parent.?.kind.nonleaf) |*child| {
                            child.parent = node.parent.?;
                        }
                    }
                },
                else => {},
            },
            .nonleaf => {},
        };

        // Then we copy the rest of the parse tree.
        const new_root = try self.parent.copyAbove(result.allocator, result.root.*);
        result.allocator.destroy(result.root);
        errdefer {
            // TODO: Clean up using node.deinit()
            (ParseTreeType{ .allocator = self.wff.allocator, .root = new_root }).deinit();
        }

        result.root = new_root;

        return WffType{
            .allocator = result.allocator,
            .parse_tree = result,
            .string = try result.toString(result.allocator),
        };
    }
};


pub const Wff = struct {
    const Self = @This();
    const ParseTree = parsing.ParseTree(parsing.TestToken);
    const MatchType = Match;

    allocator: std.mem.Allocator,
    parse_tree: ParseTree,
    string: []u8,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Self {
        const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
        defer parser.deinit();

        const tree = try parser.parse(allocator, wff_string);
        errdefer tree.deinit();

        return Self{
            .string = try tree.toString(allocator),
            .parse_tree = tree,
            .allocator = allocator,
        };
    }

    /// Create a new Wff instance from a nonterminal node. The node is used as
    /// the root of the new parse tree, and all nodes are copied, so this Wff
    /// shares no memory with the parse tree nodes it was created from.
    pub fn initFromNode(allocator: std.mem.Allocator, node: *ParseTree.Node) !Self {
        // The root node is always nonterminal.
        std.debug.assert(switch (node.kind) {
            .leaf => false,
            .nonleaf => true,
        });

        var tree = ParseTree{
            .allocator = allocator,
            .root = try node.copy(allocator),
        };
        errdefer tree.deinit();

        return Self{
            .allocator = allocator,
            .string = try tree.toString(allocator),
            .parse_tree = tree,
        };
    }

    pub fn deinit(self: Self) void {
        self.parse_tree.deinit();
        self.allocator.free(self.string);
    }

    pub fn copy(self: Self) !Self {
        return Self{
            .string = try self.allocator.dupe(u8, self.string),
            .parse_tree = try self.parse_tree.copy(),
            .allocator = self.allocator,
        };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.parse_tree.eql(other.parse_tree);
    }

    pub fn match(self: *const Self, pattern: Self) !?MatchType {
        return MatchType{ .wff = self, .parent = self.parse_tree.root, .matches = try self.parse_tree.root.match(self.allocator, pattern.parse_tree.root) orelse return null };
    }

    pub fn matchAll(self: *const Self, pattern: Self) !?std.ArrayList(MatchType) {
        var all_matches = std.ArrayList(MatchType).init(self.allocator);
        errdefer {
            for (all_matches.items) |*matches| {
                matches.deinit();
            }
            all_matches.deinit();
        }

        var it = self.parse_tree.iterPreOrder();

        while (it.next()) |node| {
            switch (node.kind) {
                .leaf => {},
                .nonleaf => {
                    if (try node.match(self.allocator, pattern.parse_tree.root)) |m| {
                        try all_matches.append(MatchType{ .wff = self, .parent = node, .matches = m });
                    }
                },
            }
        }
        if (all_matches.items.len > 0) {
            return all_matches;
        } else {
            all_matches.deinit();
            return null;
        }
    }
    };

test "Wff.equals" {
    const WffType = Wff;
    var wff1 = try WffType.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff1.deinit();
    var wff2 = try wff1.copy();
    defer wff2.deinit();
    var wff3 = try WffType.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff3.deinit();
    var wff4 = try WffType.init(std.testing.allocator, "(p => q)");
    defer wff4.deinit();

    try std.testing.expect(wff3.eql(wff1));
    try std.testing.expect(wff3.eql(wff2));
    try std.testing.expect(!wff3.eql(wff4));
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    const WffType = Wff;
    var wff = try WffType.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try WffType.init(std.testing.allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try WffType.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    var match = (try wff.match(pattern)).?;
    defer match.deinit();
    var new = (try match.replace(replace));
    defer new.deinit();

    var expected = try WffType.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.eql(new));
    try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    const WffType = Wff;
    var wff = try WffType.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try WffType.init(std.testing.allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try WffType.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(pattern) == null);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p ^ q) to (q ^ p)" {
    const WffType = Wff;
    var wff = try WffType.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try WffType.init(std.testing.allocator, "(p ^ q)");
    defer pattern.deinit();
    var replace = try WffType.init(std.testing.allocator, "(q ^ p)");
    defer replace.deinit();

    var matches = (try wff.matchAll(pattern)).?;
    defer {
        for (matches.items) |*m| m.deinit();
        matches.deinit();
    }
    try std.testing.expect(matches.items.len == 2);

    var right = matches.items[1];

    var expected = try WffType.init(std.testing.allocator, "((a ^ b) v (d ^ c))");
    defer expected.deinit();
    var new = (try right.replace(replace));
    defer new.deinit();

    try std.testing.expect(expected.eql(new));
}
