const std = @import("std");
const slr = @import("slr-table-generator.zig");
const parser = @import("parser.zig");
const wfflib = @import("wff.zig");

const this_module = @This();

pub const LexError = error{
    UnexpectedToken,
    NoTokensFound,
};

// === PARSING USING OLD GRAMMAR === 

pub const OldParsing = struct {
    const GrammarTerminal = enum {
        const Self = @This();

        Proposition,
        Not,
        LParen,
        RParen,
        And,
        Or,
        Cond,
        Bicond,
        End,

        pub fn eql(self: Self, other: Self) bool {
            return self == other;
        }

        fn getString(self: Self) []const u8 {
            return switch (self) {
                .Proposition => "PROP",
                .Not => "~",
                .LParen => "(",
                .RParen => ")",
                .And => "^",
                .Or => "v",
                .Cond => "=>",
                .Bicond => "<=>",
                .End => "$",
            };
        }
    };
    const GrammarVariable = struct {
        const Self = @This();

        name: []const u8,

        fn fromString(string: []const u8) Self {
            return GrammarVariable{ .name = string };
        }

        fn getString(self: Self) []const u8 {
            return self.name;
        }

        pub fn eql(self: Self, other: Self) bool {
            return std.mem.eql(u8, self.name, other.name);
        }
    };
    const Grammar = slr.Grammar(GrammarVariable, GrammarTerminal);

    const Token = union(enum) {
        pub const PropositionVar = struct {
            string: []const u8,

            pub fn equals(self: PropositionVar, other: PropositionVar) bool {
                return self.string[0] == other.string[0];
            }
        };

        pub const WffOperator = enum {
            Not, // '~'
            And, // '^'
            Or, // 'v'
            Cond, // Conditional: "=>"
            Bicond, // Biconditional: "<=>"

            pub fn getString(self: WffOperator) []const u8 {
                return switch (self) {
                    .Not => "~",
                    .And => "^",
                    .Or => "v",
                    .Cond => "=>",
                    .Bicond => "<=>",
                };
            }
        };

        LParen,
        RParen,
        True,
        False,
        Proposition: PropositionVar,
        Operator: WffOperator,

        fn toTerminal(token: Token) ?GrammarTerminal {
            return switch (token) {
                .LParen => GrammarTerminal.LParen,
                .RParen => GrammarTerminal.RParen,
                .True, .False, .Proposition => GrammarTerminal.Proposition,
                .Operator => |op| switch (op) {
                    .Not => GrammarTerminal.Not,
                    .And => GrammarTerminal.And,
                    .Or => GrammarTerminal.Or,
                    .Cond => GrammarTerminal.Cond,
                    .Bicond => GrammarTerminal.Bicond,
                },
            };
        }

        pub fn copy(self: Token, allocator: std.mem.Allocator) !Token {
            return switch (self) {
                .Proposition => |prop| Token{ .Proposition = PropositionVar{ .string = try allocator.dupe(u8, prop.string) } },
                else => self,
            };
        }

        /// Compare two Token instances. They are equal if they are of the same
        /// sub-type, and in the case of Proposition and Operator, if their stored
        /// values are equivalent.
        pub fn eql(self: Token, other: Token) bool {
            return switch (self) {
                .LParen => switch (other) {
                    .LParen => true,
                    else => false,
                },
                .RParen => switch (other) {
                    .RParen => true,
                    else => false,
                },
                .True => switch(other) {
                    .True => true,
                    else => false,
                },
                .False => switch(other) {
                    .False => true,
                    else => false,
                },
                .Proposition => |prop| switch (other) {
                    .Proposition => |other_prop| prop.equals(other_prop),
                    else => false,
                },
                .Operator => |op| switch (other) {
                    .Operator => |other_op| op == other_op,
                    else => false,
                },
            };
        }

        pub fn getString(self: Token) []const u8 {
            return switch (self) {
                .LParen => "(",
                .RParen => ")",
                .Proposition => |prop| prop.string,
                .Operator => |op| op.getString(),
                .True => "T",
                .False => "F",
            };
        }
    };

    const ParseTree = parser.ParseTree(Token);

    pub const WffParser = this_module.WffParser(
        GrammarVariable,
        GrammarTerminal,
        Token,
        tokenize,
        Token.toTerminal,
        parseTreeToWffTree,
    );
    
    const grammar = ret: {
        const V = GrammarVariable.fromString; 
        break :ret Grammar.initFromTuples(
            .{
                .{ V("S"), .{V("wff")} },
                .{ V("wff"), .{GrammarTerminal.Proposition} },
                .{ V("wff"), .{ GrammarTerminal.Not, V("wff") } },
                .{ V("wff"), .{ GrammarTerminal.LParen, V("wff"), GrammarTerminal.And, V("wff"), GrammarTerminal.RParen } },
                .{ V("wff"), .{ GrammarTerminal.LParen, V("wff"), GrammarTerminal.Or, V("wff"), GrammarTerminal.RParen } },
                .{ V("wff"), .{ GrammarTerminal.LParen, V("wff"), GrammarTerminal.Cond, V("wff"), GrammarTerminal.RParen } },
                .{ V("wff"), .{ GrammarTerminal.LParen, V("wff"), GrammarTerminal.Bicond, V("wff"), GrammarTerminal.RParen } },
            },
            V("S"),
            GrammarTerminal.End,
        );
    };

    pub const wff_parser = @This().WffParser.initComptime(grammar);

    // Tokenize a string containing a WFF. Returns an ArrayList of Tokens if a valid
    // string is passed, else a LexError. Caller must free the returned ArrayList.
    fn tokenize(allocator: std.mem.Allocator, wff_string: []const u8) !std.ArrayList(Token) {
        const State = enum {
            None,
            Cond,
            BicondBegin,
            BicondEnd,
        };

        var tokens = std.ArrayList(Token).init(allocator);
        errdefer {
            for (tokens.items) |tok| switch (tok) {
                .Proposition => |prop| allocator.free(prop.string),
                else => {},
            };
            tokens.deinit();
        }

        var state: State = State.None;
        for (wff_string) |c| {
            // errdefer {
            //     debug.print("\n\tInvalid token: {c}\n\tProcessed tokens: {any}\n", .{ c, tokens.items });
            // }

            if (std.ascii.isWhitespace(c)) {
                continue;
            }

            // This is the main tokenizing logic. There are 3 outcomes of this
            // block:
            // (1) A token is correctly processed => a lex.Token struct is assigned to
            //     tok, code flow continues normally
            // (2) An intermediate token is processed (e.g. '=' in the "<=>"
            //     operator) => state is changed appropriately (e.g. to BicondEnd)
            //     and we continue right away to the next loop iteration without
            //     assigning a value to tok.
            // (3) An unexpected token is encountered => we return an error.
            const tok = try switch (state) {
                .None => switch (c) {
                    '~' => Token{ .Operator = Token.WffOperator.Not },
                    'v' => Token{ .Operator = Token.WffOperator.Or },
                    '^' => Token{ .Operator = Token.WffOperator.And },
                    '=' => {
                        state = State.Cond;
                        continue;
                    },
                    '<' => {
                        state = State.BicondBegin;
                        continue;
                    },

                    '(' => Token.LParen,
                    ')' => Token.RParen,

                    'T' => Token.True,
                    'F' => Token.False,

                    // TODO: Include v, T, F as an allowable variable name (currently it
                    // conflicts with OR operator, True, False tokens).
                    'a'...'u', 'w'...'z', 'A'...'E', 'G'...'S', 'U'...'Z' => |val| ret: {
                        var str = try allocator.alloc(u8, 1);
                        str[0] = val;
                        break :ret Token{ .Proposition = Token.PropositionVar{ .string = str } };
                    },

                    else => LexError.UnexpectedToken,
                },
                .Cond => switch (c) {
                    '>' => ret: {
                        state = State.None;
                        break :ret Token{ .Operator = Token.WffOperator.Cond };
                    },
                    else => LexError.UnexpectedToken,
                },
                .BicondBegin => switch (c) {
                    '=' => {
                        state = State.BicondEnd;
                        continue;
                    },
                    else => LexError.UnexpectedToken,
                },
                .BicondEnd => switch (c) {
                    '>' => ret: {
                        state = State.None;
                        break :ret Token{ .Operator = Token.WffOperator.Bicond };
                    },
                    else => LexError.UnexpectedToken,
                },
            };

            try tokens.append(tok);
        }
        if (tokens.items.len == 0) {
            return LexError.NoTokensFound;
        }
        return tokens;
    }

    fn oldOperatorToWffBinaryOperator(operator: Token) wfflib.WffTree.Node.BinaryOperator {
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
    fn parseTreeToWffTree(allocator: std.mem.Allocator, parse_tree: ParseTree) !wfflib.WffTree {
        const wff_root = try wfflib.WffTree.Node.initKindUndefined(allocator, null);

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
                                    .arg = try wfflib.WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                },
                            },
                            .And, .Or, .Cond, .Bicond => wff_node.?.kind = .{ .binary_operator = .{
                                .operator = oldOperatorToWffBinaryOperator(operatorToken),
                                .arg1 = try wfflib.WffTree.Node.initKindUndefined(allocator, wff_node.?),
                                .arg2 = try wfflib.WffTree.Node.initKindUndefined(allocator, wff_node.?),
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
        return wfflib.WffTree{ .allocator = allocator, .root = wff_root };
    }
};

test "OldParsing.Token.equals" {
    const Token = OldParsing.Token;

    const t1: Token = Token.LParen;
    const t2: Token = Token.LParen;
    const t3: Token = Token.RParen;

    const t4: Token = Token{ .Operator = Token.WffOperator.And };
    const t5: Token = Token{ .Operator = Token.WffOperator.And };
    const t6: Token = Token{ .Operator = Token.WffOperator.Or };

    try std.testing.expect(t1.eql(t2));
    try std.testing.expect(!t1.eql(t3));

    try std.testing.expect(t4.eql(t5));
    try std.testing.expect(!t4.eql(t6));

    try std.testing.expect(!t1.eql(t5));
}

test "OldParsing.tokenize: '(p v q)'" {
    const Token = OldParsing.Token;
    var result = try OldParsing.tokenize(std.testing.allocator, "(p v q)");
    defer {
        for (result.items) |tok| switch (tok) {
            .Proposition => |prop| std.testing.allocator.free(prop.string),
            else => {},
        };
        result.deinit();
    }
    const expected = [_]Token{
        Token.LParen,
        Token{ .Proposition = Token.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } },
        Token{ .Operator = Token.WffOperator.Or },
        Token{ .Proposition = Token.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "q") } },
        Token.RParen,
    };
    defer {
        std.testing.allocator.free(expected[1].Proposition.string);
        std.testing.allocator.free(expected[3].Proposition.string);
    }

    for (expected, 0..) |e, i| {
        try std.testing.expect(e.eql(result.items[i]));
    }
}

test "OldParsing.tokenize: ''" {
    try std.testing.expectError(LexError.NoTokensFound, OldParsing.tokenize(std.testing.allocator, ""));
}

test "OldParsing.tokenize: 'p'" {
    const Token = OldParsing.Token;
    var result = try OldParsing.tokenize(std.testing.allocator, "p");
    defer {
        for (result.items) |tok| switch (tok) {
            .Proposition => |prop| std.testing.allocator.free(prop.string),
            else => {},
        };
        result.deinit();
    }
    const expected = [_]Token{
        Token{ .Proposition = Token.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } },
    };
    defer std.testing.allocator.free(expected[0].Proposition.string);

    for (expected, 0..) |e, i| {
        try std.testing.expect(e.eql(result.items[i]));
    }
}

test "OldParsing.tokenize: p & q" {
    try std.testing.expectError(LexError.UnexpectedToken, OldParsing.tokenize(std.testing.allocator, "p & q"));
}

test "OldParsing.parseTreeToWffTree: ((a v b) ^ ~c)" {
    const allocator = std.testing.allocator;
    const WffTree = wfflib.WffTree;

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

    const parse_tree = try OldParsing.wff_parser.internal_parser.parse(allocator, "((a v b) ^ ~c)");
    defer parse_tree.deinit();
    const actual_tree = try OldParsing.parseTreeToWffTree(allocator, parse_tree);
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
    const wff_string = try actual_tree.makeString(allocator);
    defer allocator.free(wff_string);
    try std.testing.expectEqualStrings("((a v b) ^ ~c)", wff_string);
}

test "OldParsing.parseTreeToWffTree: a" {
    const allocator = std.testing.allocator;

    const root = wfflib.WffTree.Node{ .parent = null, .kind = .{ .proposition_variable = "a" } };

    const parse_tree = try OldParsing.wff_parser.internal_parser.parse(allocator, "a");
    defer parse_tree.deinit();
    const actual_tree = try OldParsing.parseTreeToWffTree(allocator, parse_tree);
    defer actual_tree.deinit();

    try std.testing.expectEqualDeep(root, actual_tree.root.*);

    const wff_string = try actual_tree.makeString(allocator);
    defer allocator.free(wff_string);
    try std.testing.expectEqualStrings("a", wff_string);
}

test "OldParsing.parseTreeToWffTree: T" {
    const allocator = std.testing.allocator;

    const root = wfflib.WffTree.Node{ .parent = null, .kind = .{
        .logical_constant = .t,
    } };

    const parse_tree = try OldParsing.wff_parser.internal_parser.parse(allocator, "T");
    defer parse_tree.deinit();
    const actual_tree = try OldParsing.parseTreeToWffTree(allocator, parse_tree);
    defer actual_tree.deinit();

    try std.testing.expectEqualDeep(root, actual_tree.root.*);

    const wff_string = try actual_tree.makeString(allocator);
    defer allocator.free(wff_string);
    try std.testing.expectEqualStrings("T", wff_string);
}

test "OldParsing.parseTreeToWffTree: (~T <=> (a ^ F))" {
    const allocator = std.testing.allocator;
    const WffTree = wfflib.WffTree;

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

    const parse_tree = try OldParsing.wff_parser.internal_parser.parse(allocator, "(~T <=> (a ^ F))");
    defer parse_tree.deinit();
    const actual_tree = try OldParsing.parseTreeToWffTree(allocator, parse_tree);
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

    const wff_string = try actual_tree.makeString(allocator);
    defer allocator.free(wff_string);
    try std.testing.expectEqualStrings("(~T <=> (a ^ F))", wff_string);
}

// const TestToken = union(TestTerminal) {
//     const Self = @This();

//     Proposition: []const u8,
//     Not: void,
//     LParen: void,
//     RParen: void,
//     And: void,
//     Or: void,
//     Cond: void,
//     Bicond: void,
//     End: void,

//     pub fn eql(self: Self, other: Self) bool {
//         if (!@as(TestTerminal, self).eql(other)) {
//             return false;
//         }
//         return switch(self) {
//             .Proposition => |s| std.mem.eql(u8, s, other.Proposition),
//             else => true,
//         };
//     }

//     fn getTerminal(self: Self) ?TestTerminal {
//         return @as(TestTerminal, self);
//     }

//     fn getString(self: Self) []const u8 {
//         return switch (self) {
//             .Proposition => |s| s,
//             .Not => "~",
//             .LParen => "(",
//             .RParen => ")",
//             .And => "^",
//             .Or => "v",
//             .Cond => "=>",
//             .Bicond => "<=>",
//             .End => "$",
//         };
//     }
// };

//#####################################




fn WffParser(
    comptime Variable: type,
    comptime Terminal: type,
    comptime Token: type,
    comptime tokenize: fn (std.mem.Allocator, []const u8) anyerror!std.ArrayList(Token),
    comptime terminalFromToken: fn (Token) ?Terminal,
    comptime parseTreeToWffTree: fn(std.mem.Allocator, parser.ParseTree(Token)) anyerror!wfflib.WffTree,
) type {
    return struct {
        const Self = @This();
        const InternalParser = parser.Parser(Variable, Terminal, Token, tokenize, terminalFromToken);

        internal_parser: InternalParser,

        pub fn init(allocator: std.mem.Allocator, grammar: InternalParser.Grammar) Self {
            return Self{ .internal_parser = InternalParser.init(allocator, grammar) };
        }

        pub fn initComptime(grammar: InternalParser.Grammar) Self {
            return Self{ .internal_parser = InternalParser.initComptime(grammar) };
        }

        /// Does not free the memory associated with the grammar it was 
        /// initialized with.
        pub fn deinit(self: Self) void {
            self.internal_parser.deinit();
        }

        pub fn parse(self: Self, allocator: std.mem.Allocator, wff_string: []const u8) !wfflib.Wff {
            const parse_tree = try self.internal_parser.parse(allocator, wff_string);
            defer parse_tree.deinit();

            const wff_tree = try parseTreeToWffTree(allocator, parse_tree);
            errdefer wff_tree.deinit();

            return wfflib.Wff{
                .allocator = allocator,
                .tree = wff_tree,
                .string = try wff_tree.makeString(allocator),
            };
        }        
    };
}

test "WffParser.parse: ((a v b) ^ ~c)" {
    const allocator = std.testing.allocator;

    const parse_tree = try OldParsing.wff_parser.internal_parser.parse(allocator, "((a v b) ^ ~c)");
    defer parse_tree.deinit();
    const expected_tree = try OldParsing.parseTreeToWffTree(allocator, parse_tree);
    defer expected_tree.deinit();

    const wff_parser = OldParsing.wff_parser;
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