const std = @import("std");
const debug = std.debug;

pub const LexError = error{
    UnexpectedToken,
    NoTokensFound,
};

pub const PropositionVar = struct {
    const Self = @This();

    string: []const u8,

    pub fn equals(self: Self, other: Self) bool {
        return self.string[0] == other.string[0];
    }
};

pub const WffOperator = enum {
    const Self = @This();

    Not, // '~'
    And, // '^'
    Or, // 'v'
    Cond, // Conditional: "=>"
    Bicond, // Biconditional: "<=>"

    pub fn getString(self: Self) []const u8 {
        return switch (self) {
            .Not => "~",
            .And => "^",
            .Or => "v",
            .Cond => "=>",
            .Bicond => "<=>",
        };
    }
};

pub const Token = union(enum) {
    const Self = @This();

    LParen,
    RParen,
    True,
    False,
    Proposition: PropositionVar,
    Operator: WffOperator,

    pub fn copy(self: Self, allocator: std.mem.Allocator) !Self {
        return switch (self) {
            .Proposition => |prop| Token{ .Proposition = PropositionVar{ .string = try allocator.dupe(u8, prop.string) } },
            else => self,
        };
    }

    /// Compare two Token instances. They are equal if they are of the same
    /// sub-type, and in the case of Proposition and Operator, if their stored
    /// values are equivalent.
    pub fn eql(self: Self, other: Self) bool {
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

    pub fn getString(self: Self) []const u8 {
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

test "Token.equals" {
    const t1: Token = Token.LParen;
    const t2: Token = Token.LParen;
    const t3: Token = Token.RParen;

    const t4: Token = Token{ .Operator = WffOperator.And };
    const t5: Token = Token{ .Operator = WffOperator.And };
    const t6: Token = Token{ .Operator = WffOperator.Or };

    try std.testing.expect(t1.eql(t2));
    try std.testing.expect(!t1.eql(t3));

    try std.testing.expect(t4.eql(t5));
    try std.testing.expect(!t4.eql(t6));

    try std.testing.expect(!t1.eql(t5));
}

// test "ParseTree.matchAll: (p v q)" {
//     var allocator = std.testing.allocator;
//     var tree = try ParseTree.init(allocator, "(p v q)");
//     defer tree.deinit();
//     var pattern = try ParseTree.init(allocator, "(p v q)");
//     defer pattern.deinit();

//     var all_matches = try tree.matchAll(pattern);
//     defer {
//         if (all_matches) |all| {
//             for (all.items) |*matches| {
//                 matches.deinit();
//             }
//             all.deinit();
//         }
//     }

//     try std.testing.expect(all_matches.?.items.len == 1);

//     var match1 = all_matches.?.items[0];

//     try std.testing.expect(match1.count() == 2);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], match1.get("p").?);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[3], match1.get("q").?);
// }

// Tokenize a string containing a WFF. Returns an ArrayList of Tokens if a valid
// string is passed, else a LexError. Caller must free the returned ArrayList.
pub fn tokenize(allocator: std.mem.Allocator, wff_string: []const u8) !std.ArrayList(Token) {
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
                '~' => Token{ .Operator = WffOperator.Not },
                'v' => Token{ .Operator = WffOperator.Or },
                '^' => Token{ .Operator = WffOperator.And },
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
                    break :ret Token{ .Proposition = PropositionVar{ .string = str } };
                },

                else => LexError.UnexpectedToken,
            },
            .Cond => switch (c) {
                '>' => ret: {
                    state = State.None;
                    break :ret Token{ .Operator = WffOperator.Cond };
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
                    break :ret Token{ .Operator = WffOperator.Bicond };
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

test "tokenize: '(p v q)'" {
    var result = try tokenize(std.testing.allocator, "(p v q)");
    defer {
        for (result.items) |tok| switch (tok) {
            .Proposition => |prop| std.testing.allocator.free(prop.string),
            else => {},
        };
        result.deinit();
    }
    const expected = [_]Token{
        Token.LParen,
        Token{ .Proposition = PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } },
        Token{ .Operator = WffOperator.Or },
        Token{ .Proposition = PropositionVar{ .string = try std.testing.allocator.dupe(u8, "q") } },
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

test "tokenize: ''" {
    try std.testing.expectError(LexError.NoTokensFound, tokenize(std.testing.allocator, ""));
}

test "tokenize: 'p'" {
    var result = try tokenize(std.testing.allocator, "p");
    defer {
        for (result.items) |tok| switch (tok) {
            .Proposition => |prop| std.testing.allocator.free(prop.string),
            else => {},
        };
        result.deinit();
    }
    const expected = [_]Token{
        Token{ .Proposition = PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } },
    };
    defer std.testing.allocator.free(expected[0].Proposition.string);

    for (expected, 0..) |e, i| {
        try std.testing.expect(e.eql(result.items[i]));
    }
}

test "tokenize: p & q" {
    try std.testing.expectError(LexError.UnexpectedToken, tokenize(std.testing.allocator, "p & q"));
}