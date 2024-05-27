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
    // TODO: Check Parser has necessary method signature

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

        pub fn deinit(_: Self) void {
            // Nothing to do currently
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

            return Wff{ .allocator = allocator, .tree = wff_tree };
        }

        fn oldOperatorToWffBinaryOperator(operator: parsing.TestToken) WffTree.Node.BinaryOperator {
            return switch (operator) {
                .Operator => |op| switch (op) {
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
            while (parse_iter.next()) |parse_node| {
                var wff_node = wff_iter.peek();
                switch (parse_node.kind) {
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
                        switch (operatorToken) {
                            .Operator => |op| switch (op) {
                                .Not => wff_node.?.kind = .{
                                    .unary_operator = .{
                                        .operator = .not,
                                        .arg = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                    },
                                },
                                .And, .Or, .Cond, .Bicond => wff_node.?.kind = .{ .binary_operator = .{
                                    .operator = oldOperatorToWffBinaryOperator(operatorToken),
                                    .arg1 = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                    .arg2 = try WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                } },
                            },
                            else => std.debug.panic("nonleaf parse node does not have an operator in the expected position in its children", .{}),
                        }
                    },
                    .leaf => |token| {
                        switch (token) {
                            .Proposition => |prop| wff_node.?.kind = .{ .proposition_variable = try allocator.dupe(u8, prop.string) },
                            .True => wff_node.?.kind = .{ .logical_constant = .t },
                            .False => wff_node.?.kind = .{ .logical_constant = .f },
                            else => continue,
                        }
                    },
                }
                _ = wff_iter.next();
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
    var root = WffTree.Node{
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ .binary_operator = .{
        .operator = .or_,
        .arg1 = &a,
        .arg2 = &b,
    } };

    var right_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node{
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ .unary_operator = .{
        .operator = .not,
        .arg = &c,
    } };

    root.kind = .{ .binary_operator = .{
        .operator = .and_,
        .arg1 = &left_branch,
        .arg2 = &right_branch,
    } };

    const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "((a v b) ^ ~c)");
    defer parse_tree.deinit();
    const actual_tree = try WffParser(parsing.TestParserType).fromOldGrammar(allocator, parse_tree);
    defer actual_tree.deinit();

    var expected_it = root.iterPreOrder();
    var actual_it = actual_tree.iterPreOrder();
    while (expected_it.next()) |expected_node| {
        const actual_node = actual_it.next().?;
        try std.testing.expectEqual(std.meta.activeTag(expected_node.kind), std.meta.activeTag(actual_node.kind));
        switch (expected_node.kind) {
            .unary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.unary_operator.operator),
            .binary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.binary_operator.operator),
            .proposition_variable => |s| try std.testing.expectEqualStrings(s, actual_node.kind.proposition_variable),
            .logical_constant => |constant| try std.testing.expectEqual(constant, actual_node.kind.logical_constant),
        }
    }
}

test "WffParser.fromOldGrammar: a" {
    const allocator = std.testing.allocator;

    const root = WffTree.Node{ .parent = null, .kind = .{ .proposition_variable = "a" } };

    const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "a");
    defer parse_tree.deinit();
    const actual_tree = try WffParser(parsing.TestParserType).fromOldGrammar(allocator, parse_tree);
    defer actual_tree.deinit();

    try std.testing.expectEqualDeep(root, actual_tree.root.*);
}

test "WffParser.fromOldGrammar: T" {
    const allocator = std.testing.allocator;

    const root = WffTree.Node{ .parent = null, .kind = .{
        .logical_constant = .t,
    } };

    const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "T");
    defer parse_tree.deinit();
    const actual_tree = try WffParser(parsing.TestParserType).fromOldGrammar(allocator, parse_tree);
    defer actual_tree.deinit();

    try std.testing.expectEqualDeep(root, actual_tree.root.*);
}

test "WffParser.fromOldGrammar: (~T <=> (a ^ F))" {
    const allocator = std.testing.allocator;

    // Build expected tree for (a v b) ^ ~c
    var root = WffTree.Node{
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var t = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .logical_constant = .t },
    };

    left_branch.kind = .{ .unary_operator = .{
        .operator = .not,
        .arg = &t,
    } };

    var right_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node{
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var f = WffTree.Node{
        .parent = &right_branch,
        .kind = .{ .logical_constant = .f },
    };

    right_branch.kind = .{ .binary_operator = .{
        .operator = .and_,
        .arg1 = &a,
        .arg2 = &f,
    } };

    root.kind = .{ .binary_operator = .{
        .operator = .bicond,
        .arg1 = &left_branch,
        .arg2 = &right_branch,
    } };

    const parser = try parsing.TestParserType.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "(~T <=> (a ^ F))");
    defer parse_tree.deinit();
    const actual_tree = try WffParser(parsing.TestParserType).fromOldGrammar(allocator, parse_tree);
    defer actual_tree.deinit();

    var expected_it = root.iterPreOrder();
    var actual_it = actual_tree.iterPreOrder();
    while (expected_it.next()) |expected_node| {
        const actual_node = actual_it.next().?;
        try std.testing.expectEqual(std.meta.activeTag(expected_node.kind), std.meta.activeTag(actual_node.kind));
        switch (expected_node.kind) {
            .unary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.unary_operator.operator),
            .binary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.binary_operator.operator),
            .proposition_variable => |s| try std.testing.expectEqualStrings(s, actual_node.kind.proposition_variable),
            .logical_constant => |constant| try std.testing.expectEqual(constant, actual_node.kind.logical_constant),
        }
    }
}

test "WffParser.parse: ((a v b) ^ ~c)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();
    const parse_tree = try parser.parse(allocator, "((a v b) ^ ~c)");
    defer parse_tree.deinit();
    const expected_tree = try WffParser(Parser).fromOldGrammar(allocator, parse_tree);
    defer expected_tree.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();
    const actual_wff = try wff_parser.parse(allocator, "((a v b) ^ ~c)");
    defer actual_wff.deinit();

    var expected_it = expected_tree.iterPreOrder();
    var actual_it = actual_wff.tree.iterPreOrder();
    while (expected_it.next()) |expected_node| {
        const actual_node = actual_it.next().?;
        try std.testing.expectEqual(std.meta.activeTag(expected_node.kind), std.meta.activeTag(actual_node.kind));
        switch (expected_node.kind) {
            .unary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.unary_operator.operator),
            .binary_operator => |op| try std.testing.expectEqual(op.operator, actual_node.kind.binary_operator.operator),
            .proposition_variable => |s| try std.testing.expectEqualStrings(s, actual_node.kind.proposition_variable),
            .logical_constant => |constant| try std.testing.expectEqual(constant, actual_node.kind.logical_constant),
        }
    }
}

pub const WffTree = struct {
    const Self = @This();

    pub const PreOrderIterator = struct {
        start: *Node,
        next_node: ?*Node,

        // Traverse back up the tree and return the first unvisited node to the
        // right. If we're traversing up the rightmost branch and reach the root
        // return null.
        fn backtrack_next(self: PreOrderIterator, node: *Node) ?*Node {
            var next_node = node;
            while (next_node.parent) |parent| {
                // If we're copying from a subtree, we need to make sure we 
                // don't go past the start node, since its parent won't be null. 
                if (next_node == self.start) {
                    return null;
                }
                switch(parent.kind) {
                    .unary_operator => next_node = parent,
                    .binary_operator => |op| {
                        if (next_node == op.arg1) {
                            return op.arg2;
                        } else {
                            next_node = parent;
                        }
                    },
                    .proposition_variable, .logical_constant => unreachable,
                }
            }
            return null;
        }

        // From the given starting node, get the deepest node always branching
        // right.
        fn get_deepest_right(node: *Node) *Node {
            var deepest_right = node;
            while (true) {
                deepest_right = switch (deepest_right.kind) {
                    .unary_operator => |op| op.arg,
                    .binary_operator => |op| op.arg2,
                    .proposition_variable, .logical_constant => return deepest_right,
                };
            }
        }

        pub fn next(self: *PreOrderIterator) ?*Node {
            const current_node = self.next_node orelse return null;

            self.next_node = switch (current_node.kind) {
                .unary_operator => |op| op.arg,
                .binary_operator => |op| op.arg1,
                .proposition_variable, .logical_constant => self.backtrack_next(current_node),
            };

            return current_node;
        }

        pub fn peek(self: PreOrderIterator) ?*Node {
            return self.next_node;
        }

        pub fn hasNext(self: PreOrderIterator) bool {
            return self.peek() != null;
        }

        pub fn previous(self: *PreOrderIterator) ?*Node {
            if (self.next_node) |node| {
                if (node.parent) |parent| {
                    self.next_node = switch (parent.kind) {
                        .unary_operator => parent,
                        .binary_operator => |op| ret: {
                            if (node == op.arg2) {
                                break :ret get_deepest_right(op.arg1);
                            } else {
                                break :ret parent;
                            }
                        },
                        .proposition_variable, .logical_constant => unreachable,
                    };
                } else {
                    // Current node is root so we can't go back any further
                    return null;
                }
            } else {
                // Iterator was exhausted, so we get the last node returned.
                self.next_node = get_deepest_right(self.start);
            }
            return self.next_node;
        }

        pub fn skipChildren(self: *PreOrderIterator) void {
            const current = self.previous() orelse return;
            self.next_node = self.backtrack_next(current);
        }
    };

    pub const PostOrderIterator = struct {
        root: *Node,
        next_node: ?*Node,

        fn isLastSiblingNode(node: *Node) bool {
            const parent = node.parent orelse return true;

            return switch (parent.kind) {
                .unary_operator => true,
                .binary_operator => |op| node == op.arg2,
                .proposition_variable, .logical_constant => unreachable,
            };
        }

        pub fn next(self: *PostOrderIterator) ?*Node {
            const current_node = self.next_node orelse return null;

            if (isLastSiblingNode(current_node)) {
                self.next_node = current_node.parent;
                return current_node;
            }

            // We know parent is not null, else we already would have returned.
            self.next_node = switch (current_node.parent.?.kind) {
                .binary_operator => |op| op.arg2,
                .unary_operator, .proposition_variable, .logical_constant => unreachable,
            };

            while (true) {
                switch (self.next_node.?.kind) {
                    .unary_operator => |op| self.next_node = op.arg,
                    .binary_operator => |op| self.next_node = op.arg1,
                    .proposition_variable, .logical_constant => break,
                }
            }

            return current_node;
        }

        pub fn peek(self: PostOrderIterator) ?*Node {
            return self.next_node;
        }

        pub fn hasNext(self: PostOrderIterator) bool {
            return self.peek() != null;
        }
    };

    pub const Node = struct {
        const UnaryOperator = enum {
            not,
        };

        const BinaryOperator = enum {
            and_,
            or_,
            cond,
            bicond,
        };

        const LogicalConstant = enum {
            t,
            f,
        };

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
            proposition_variable: []const u8,
            logical_constant: LogicalConstant,
        },

        fn initKindUndefined(allocator: std.mem.Allocator, parent: ?*Node) !*Node {
            var node = try allocator.create(Node);
            errdefer allocator.destroy(node);

            node.parent = parent;
            return node;
        }

        // Free the memory for ONLY this node (i.e. does not free memory for
        // child nodes) using the supplied allocator which MUST be the same
        // allocator used to create the node.
        fn deinit(self: *Node, allocator: std.mem.Allocator) void {
            switch (self.kind) {
                .proposition_variable => |s| allocator.free(s),
                .unary_operator, .binary_operator, .logical_constant => {},
            }
            allocator.destroy(self);
        }

        fn iterPreOrder(self: *Node) PreOrderIterator {
            return PreOrderIterator{ .start = self, .next_node = self };
        }

        fn iterPostOrder(self: *Node) PostOrderIterator {
            var start = self;
            while (true) {
                switch (start.kind) {
                    .unary_operator => |op| start = op.arg,
                    .binary_operator => |op| start = op.arg1,
                    .proposition_variable, .logical_constant => break,
                }
            }
            return PostOrderIterator{ .root = self, .next_node = start };
        }

        fn eql(self: *Node, other: *Node) bool {
            var it = self.iterPreOrder();
            var other_it = other.iterPreOrder();

            while (it.hasNext() and other_it.hasNext()) {
                const node = it.next().?;
                const other_node = other_it.next().?;

                if (std.meta.activeTag(node.kind) != std.meta.activeTag(other_node.kind)) {
                    return false;
                }
                switch (node.kind) {
                    .unary_operator => |op| if (op.operator != other_node.kind.unary_operator.operator) return false,
                    .binary_operator => |op| if (op.operator != other_node.kind.binary_operator.operator) return false,
                    .proposition_variable => |s| if (!std.mem.eql(u8, s, other_node.kind.proposition_variable)) return false,
                    .logical_constant => |constant| if (constant != other_node.kind.logical_constant) return false,
                }
            }
            if (it.hasNext() or other_it.hasNext()) {
                return false;
            }
            return true;
        }

        fn copy(self: *Node, allocator: std.mem.Allocator) !*Node {
            var copy_root = try Node.initKindUndefined(allocator, null);
            var copy_it = copy_root.iterPreOrder();
            errdefer {
                var node = copy_it.peek().?;
                while (copy_it.previous()) |previous| {
                    node.deinit(allocator);
                    node = previous;
                }
                node.deinit(allocator);
            }

            var it = self.iterPreOrder();

            while (it.next()) |node| {
                var copy_node = copy_it.peek().?;

                copy_node.kind = switch (node.kind) {
                    .unary_operator => |op| .{
                        .unary_operator = .{
                            .operator = op.operator,
                            .arg = try Node.initKindUndefined(allocator, copy_node),
                        },
                    },
                    .binary_operator => |op| .{
                        .binary_operator = .{
                            .operator = op.operator,
                            .arg1 = try Node.initKindUndefined(allocator, copy_node),
                            .arg2 = try Node.initKindUndefined(allocator, copy_node),
                        },
                    },
                    .proposition_variable => |s| .{
                        .proposition_variable = try allocator.dupe(u8, s),
                    },
                    .logical_constant => |constant| .{
                        .logical_constant = constant,
                    },
                };
                _ = copy_it.next();
            }
            return copy_root;
        }

        // NOTE: The string keys in the returned hashmap are owned by the
        // pattern nodes. Make sure the pattern is not deallocated before the
        // returned hash map is done being used.
        fn match(self: *Node, allocator: std.mem.Allocator, pattern: *Node) !?Wff.MatchHashMap {
            var matches = Wff.MatchHashMap.init(allocator);
            errdefer matches.deinit();

            var it = self.iterPreOrder();
            var pattern_it = pattern.iterPreOrder();

            while (it.next()) |node| {
                const pattern_node = pattern_it.next() orelse break;

                const is_match = switch (pattern_node.kind) {
                    .unary_operator => |pattern_op| switch (node.kind) {
                        .unary_operator => |op| pattern_op.operator == op.operator,
                        else => false,
                    },
                    .binary_operator => |pattern_op| switch (node.kind) {
                        .binary_operator => |op| pattern_op.operator == op.operator,
                        else => false,
                    },
                    .logical_constant => |pattern_constant| switch (node.kind) {
                        .logical_constant => |constant| pattern_constant == constant,
                        else => false,
                    },
                    .proposition_variable => |variable| {
                        it.skipChildren();

                        // In a case where the same pattern variable is used
                        // more than once (e.g. (p ^ p)) we must check that
                        // both subwffs matching p are equal, otherwise it's
                        // not a match.
                        if (matches.get(variable)) |existing_match| {
                            if (!existing_match.eql(node)) {
                                _ = matches.remove(variable);
                            }
                        } else {
                            try matches.put(variable, node);
                        }

                        continue;
                    },
                };

                if (!is_match) {
                    matches.deinit();
                    return null;
                }
            }
            if (matches.count() > 0) {
                return matches;
            } else {
                matches.deinit();
                return null;
            }
        }

        /// When called on a nonleaf node, make a copy of the entire tree but
        /// with this node (subtree) replaced with a given other node which can
        /// be the root of a new subtree.
        /// When called on the root node of a tree, returns the given 
        /// replacement node. 
        fn copyAbove(self: *Node, allocator: std.mem.Allocator, replacement_root: *Node) !*Node {
            const root = ret: {
                var node = self;
                while (node.parent) |parent| {
                    node = parent;
                }
                break :ret node;
            };
            if (root == self) {
                replacement_root.parent = null;
                return replacement_root;
            }
            var it = root.iterPreOrder();

            var copy_root = try Node.initKindUndefined(allocator, null);
            var copy_it = copy_root.iterPreOrder();
            var first_node_after_replacement: ?*Node = null;
            var old_parent: ?*Node = null;
            errdefer {
                // Avoid freeing the replacement node and its children.
                if (first_node_after_replacement) |stop_node| {
                    var node = copy_it.peek().?;
                    while (copy_it.previous()) |previous| {
                        if (node == stop_node) {
                            break;
                        }
                        node.deinit(allocator);
                        node = previous;
                    }
                    node.deinit(allocator);

                    copy_it.next_node = replacement_root;
                    _ = copy_it.previous();
                }
                var node = copy_it.peek().?;
                while (copy_it.previous()) |previous| {
                    node.deinit(allocator);
                    node = previous;
                }
                node.deinit(allocator);

                // Undo changes to replacement node
                if (old_parent) |parent| {
                    replacement_root.parent = parent;
                }
            }

            while (it.next()) |node| {
                var copy_node = copy_it.peek().?;
                if (node == self) {
                    it.skipChildren();
                    // Do this before we deallocate copy_node.
                    copy_it.skipChildren();

                    const copy_parent = copy_node.parent.?;
                    // Have to use destroy instead of calling deinit 
                    // since node.kind is undefined.
                    allocator.destroy(copy_node);

                    switch(copy_parent.kind) {
                        .unary_operator => copy_parent.kind.unary_operator.arg = replacement_root,
                        .binary_operator => |op| {
                            if (node == op.arg1) {
                                copy_parent.kind.binary_operator.arg1 = replacement_root;
                            } else {
                                copy_parent.kind.binary_operator.arg2 = replacement_root;
                            }
                        },
                        .proposition_variable, .logical_constant => unreachable,
                    }
                    old_parent = replacement_root.parent;
                    replacement_root.parent = copy_parent;

                    first_node_after_replacement = copy_it.peek();
                } else {
                    copy_node.kind = switch (node.kind) {
                        .unary_operator => |op| .{
                            .unary_operator = .{
                                .operator = op.operator,
                                .arg = try Node.initKindUndefined(allocator, copy_node),
                            },
                        },
                        .binary_operator => |op| .{
                            .binary_operator = .{
                                .operator = op.operator,
                                .arg1 = try Node.initKindUndefined(allocator, copy_node),
                                .arg2 = try Node.initKindUndefined(allocator, copy_node),
                            },
                        },
                        .proposition_variable => |s| .{
                            .proposition_variable = try allocator.dupe(u8, s),
                        },
                        .logical_constant => |constant| .{
                            .logical_constant = constant,
                        },
                    };
                    _ = copy_it.next();
                }
            }
            return copy_root;
        }
    };

    allocator: std.mem.Allocator,
    root: *Node,

    pub fn deinit(self: Self) void {
        var it = self.iterPostOrder();
        while (it.next()) |node| {
            node.deinit(self.allocator);
        }
    }

    pub fn iterPreOrder(self: Self) PreOrderIterator {
        return self.root.iterPreOrder();
    }

    pub fn iterPostOrder(self: Self) PostOrderIterator {
        return self.root.iterPostOrder();
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.root.eql(other.root);
    }

    pub fn copy(self: Self, allocator: std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .root = try self.root.copy(allocator),
        };
    }
};

test "WffTreePreOrderIterator: ((a v b) ^ ~c)" {
    // (a v b) ^ ~c
    var root = WffTree.Node{
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ .binary_operator = .{
        .operator = .or_,
        .arg1 = &a,
        .arg2 = &b,
    } };

    var right_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node{
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ .unary_operator = .{
        .operator = .not,
        .arg = &c,
    } };

    root.kind = .{ .binary_operator = .{
        .operator = .and_,
        .arg1 = &left_branch,
        .arg2 = &right_branch,
    } };

    var it = root.iterPreOrder();

    try std.testing.expectEqual(&root, it.next().?);
    try std.testing.expectEqual(&left_branch, it.next().?);
    try std.testing.expectEqual(&a, it.next().?);
    try std.testing.expectEqual(&b, it.next().?);
    try std.testing.expectEqual(&right_branch, it.next().?);
    try std.testing.expectEqual(&c, it.next().?);
    try std.testing.expectEqual(null, it.next());

    try std.testing.expectEqual(&c, it.previous().?);
    try std.testing.expectEqual(&right_branch, it.previous().?);
    try std.testing.expectEqual(&b, it.previous().?);
    try std.testing.expectEqual(&a, it.previous().?);
    try std.testing.expectEqual(&left_branch, it.previous().?);
    try std.testing.expectEqual(&root, it.previous().?);
    try std.testing.expectEqual(null, it.previous());
}

test "WffTreePostOrderIterator: ((a v b) ^ ~c)" {
    // (a v b) ^ ~c
    var root = WffTree.Node{
        .parent = null,
        .kind = undefined,
    };

    var left_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var a = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "a" },
    };

    var b = WffTree.Node{
        .parent = &left_branch,
        .kind = .{ .proposition_variable = "b" },
    };

    left_branch.kind = .{ .binary_operator = .{
        .operator = .or_,
        .arg1 = &a,
        .arg2 = &b,
    } };

    var right_branch = WffTree.Node{
        .parent = &root,
        .kind = undefined,
    };

    var c = WffTree.Node{
        .parent = &right_branch,
        .kind = .{ .proposition_variable = "c" },
    };

    right_branch.kind = .{ .unary_operator = .{
        .operator = .not,
        .arg = &c,
    } };

    root.kind = .{ .binary_operator = .{
        .operator = .and_,
        .arg1 = &left_branch,
        .arg2 = &right_branch,
    } };

    var it = root.iterPostOrder();

    try std.testing.expectEqual(&a, it.next().?);
    try std.testing.expectEqual(&b, it.next().?);
    try std.testing.expectEqual(&left_branch, it.next().?);
    try std.testing.expectEqual(&c, it.next().?);
    try std.testing.expectEqual(&right_branch, it.next().?);
    try std.testing.expectEqual(&root, it.next().?);
    try std.testing.expectEqual(null, it.next());
}

test "ParseTree.copy: (p v q), ((a ^ b) v (c ^ d)), and p" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    const wff1 = try wff_parser.parse(allocator, "(p v q)");
    defer wff1.deinit();
    var tree_copy1 = try wff1.tree.copy(allocator);
    defer tree_copy1.deinit();

    const wff2 = try wff_parser.parse(allocator, "((a ^ b) v (c ^ d))");
    defer wff2.deinit();
    var tree_copy2 = try wff2.tree.copy(allocator);
    defer tree_copy2.deinit();

    const wff3 = try wff_parser.parse(allocator, "p");
    defer wff3.deinit();
    var copy_tree3 = try wff3.tree.copy(allocator);
    defer copy_tree3.deinit();

    try std.testing.expect(wff1.tree.eql(tree_copy1));
    try std.testing.expect(wff2.tree.eql(tree_copy2));
    try std.testing.expect(wff3.tree.eql(copy_tree3));
}

test "ParseTree.Node.match: (p v q) with pattern (p v q)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    const wff = try wff_parser.parse(allocator, "(p v q)");
    defer wff.deinit();
    const pattern_wff = try wff_parser.parse(allocator, "(p v q)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expectEqual(2, matches.count());
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg2, matches.get("q").?);
}

test "ParseTree.Node.match: ((a ^ b) v (c ^ d)) with pattern (p v q)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    const wff = try wff_parser.parse(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    const pattern_wff = try wff_parser.parse(allocator, "(p v q)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expectEqual(2, matches.count());
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg2, matches.get("q").?);
}

pub const Wff = struct {
    const Self = @This();
    const ParseTree = parsing.ParseTree(parsing.TestToken);
    const MatchHashMap = std.StringHashMap(*WffTree.Node);

    pub const Match = struct {
        source_wff: *const Wff,
        source_subtree_root: *WffTree.Node,
        matches: MatchHashMap,

        pub fn deinit(self: *Match) void {
            self.matches.deinit();
        }

        /// Lookup a match in the hashmap and build a new Wff for it if it exists
        pub fn getBuildWff(self: Match, allocator: std.mem.Allocator, key: []const u8) !?Wff {
            const node = self.matches.get(key) orelse return null;
            return try Wff.initFromNode(allocator, node);
        }

        // TODO: errdefer cleanup result if fail
        pub fn replace(self: Match, allocator: std.mem.Allocator, destination_pattern: Wff) !Wff {
            var substitution_tree = try destination_pattern.tree.copy(allocator);
            errdefer {
                substitution_tree.deinit();
            }

            // First we substitute any proposition variables in the destination 
            // pattern for which we have a match with a copy of the matched 
            // source subtree.
            var it = substitution_tree.iterPreOrder();
            while (it.next()) |result_node| {
                switch (result_node.kind) {
                    .unary_operator, .binary_operator, .logical_constant => {},
                    .proposition_variable => |variable| {
                        if (self.matches.get(variable)) |source_node| {
                            const source_copy = try source_node.copy(allocator);
                            // Free the top node (after its contents have been
                            // copied), keep the rest of the tree.
                            // Don't call source_copy.deinit() since we want 
                            // result_node to take ownership of its contents.
                            defer allocator.destroy(source_copy);

                            result_node.kind = source_copy.kind;

                            // We've overwritten the node containing this string
                            // so free it.
                            allocator.free(variable);

                            // Set the parent of any copied children to the new 
                            // address.
                            switch(result_node.kind) {
                                .unary_operator => |op| op.arg.parent = result_node,
                                .binary_operator => |op| {
                                    op.arg1.parent = result_node;
                                    op.arg2.parent = result_node;
                                },
                                .proposition_variable, .logical_constant => {},
                            }
                        }
                    }
                }
            }

            // Then we copy the rest of the source tree (if there is any).
            const new_root = try self.source_subtree_root.copyAbove(allocator, substitution_tree.root);

            // substitution_tree is now a subtree contained under new_root. So
            // the substitution is done, and our new tree is under new_root.
            substitution_tree.root = new_root;

            return Wff{
                .allocator = allocator,
                .tree = substitution_tree,
                //.string = try result.toString(result.allocator),
            };
        }
    };

    allocator: std.mem.Allocator,
    tree: WffTree,
    //string: []u8,

    // /// Create a new Wff instance from a nonterminal node. The node is used as
    // /// the root of the new parse tree, and all nodes are copied, so this Wff
    // /// shares no memory with the parse tree nodes it was created from.
    // pub fn initFromNode(allocator: std.mem.Allocator, node: *ParseTree.Node) !Self {
    //     // The root node is always nonterminal.
    //     std.debug.assert(switch (node.kind) {
    //         .leaf => false,
    //         .nonleaf => true,
    //     });

    //     var tree = ParseTree{
    //         .allocator = allocator,
    //         .root = try node.copy(allocator),
    //     };
    //     errdefer tree.deinit();

    //     return Self{
    //         .allocator = allocator,
    //         //.string = try tree.toString(allocator),
    //         .tree = tree,
    //     };
    // }

    pub fn deinit(self: Self) void {
        self.tree.deinit();
        //self.allocator.free(self.string);
    }

    pub fn copy(self: *Self) !Self {
        return Self{
            //.string = try self.allocator.dupe(u8, self.string),
            .allocator = self.allocator,
            .tree = try self.tree.copy(self.allocator),
        };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.tree.eql(other.tree);
    }

    pub fn match(self: *const Self, allocator: std.mem.Allocator, pattern: Self) !?Match {
        return Match{ .source_wff = self, .source_subtree_root = self.tree.root, .matches = try self.tree.root.match(allocator, pattern.tree.root) orelse return null };
    }

    pub fn matchAll(self: *const Self, allocator: std.mem.Allocator, pattern: Self) !?std.ArrayList(Match) {
        var all_matches = std.ArrayList(Match).init(allocator);
        errdefer {
            for (all_matches.items) |*matches| {
                matches.deinit();
            }
            all_matches.deinit();
        }

        var it = self.tree.iterPreOrder();

        while (it.next()) |node| {
            if (try node.match(allocator, pattern.tree.root)) |m| {
                try all_matches.append(Match{ .source_wff = self, .source_subtree_root = node, .matches = m });
            }
            // switch (node.kind) {
            //     .leaf => {},
            //     .nonleaf => {
            //         if (try node.match(self.allocator, pattern.tree.root)) |m| {
            //             try all_matches.append(Match{ .wff = self, .parent = node, .matches = m });
            //         }
            //     },
            // }
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
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    var wff1 = try wff_parser.parse(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff1.deinit();
    var wff2 = try wff1.copy();
    defer wff2.deinit();
    var wff3 = try wff_parser.parse(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff3.deinit();
    var wff4 = try wff_parser.parse(std.testing.allocator, "(p => q)");
    defer wff4.deinit();

    try std.testing.expect(wff3.eql(wff1));
    try std.testing.expect(wff3.eql(wff2));
    try std.testing.expect(!wff3.eql(wff4));
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    var wff = try wff_parser.parse(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.parse(allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try wff_parser.parse(allocator, "(p => q)");
    defer replace.deinit();

    var match = (try wff.match(allocator, pattern)).?;
    defer match.deinit();
    var new = (try match.replace(allocator, replace));
    defer new.deinit();

    var expected = try wff_parser.parse(allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.eql(new));
    //try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    var wff = try wff_parser.parse(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.parse(allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try wff_parser.parse(allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(allocator, pattern) == null);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p ^ q) to (q ^ p)" {
    const allocator = std.testing.allocator;
    const Parser = parsing.TestParserType;

    const parser = try Parser.init(allocator, parsing.test_grammar_1);
    defer parser.deinit();

    const wff_parser = WffParser(Parser).init(parser);
    defer wff_parser.deinit();

    var wff = try wff_parser.parse(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.parse(allocator, "(p ^ q)");
    defer pattern.deinit();
    var replace = try wff_parser.parse(allocator, "(q ^ p)");
    defer replace.deinit();

    var matches = (try wff.matchAll(allocator, pattern)).?;
    defer {
        for (matches.items) |*m| m.deinit();
        matches.deinit();
    }
    try std.testing.expect(matches.items.len == 2);

    var right = matches.items[1];

    var expected = try wff_parser.parse(allocator, "((a ^ b) v (d ^ c))");
    defer expected.deinit();
    var new = (try right.replace(allocator, replace));
    defer new.deinit();

    try std.testing.expect(expected.eql(new));
}
