const std = @import("std");
const debug = std.debug;

const MAX_PARSE_CHILDREN = 5;

pub const ParseError = error {
    UnexpectedToken,
    NoTokensFound,

    TerminalShiftError,
    NonterminalShiftError,
    ReduceError,
};

pub const PropositionVar = struct {
    string: u8,
};

pub const WffOperator = enum {
    Not,    // '~'
    And,    // '^'
    Or,     // 'v'
    Cond,   // Conditional: "=>"
    Bicond, // Biconditional: "<=>"
};

const TokenType = enum {
    LParen,
    RParen,
    Proposition,
    Operator,
};

pub const Token = union(TokenType) {
    LParen,
    RParen,
    Proposition: PropositionVar,
    Operator: WffOperator
};

pub const ParseTree = struct {
    const Self = @This();

    const NodeType = enum {
        Terminal,
        Nonterminal,
    };
    pub const Node = union(NodeType) {
        Terminal: Token,
        Nonterminal: [MAX_PARSE_CHILDREN]*Node,
    };

    root: ?Node = null,
    allocator: std.mem.Allocator,
};

// Tokenize a string containing a WFF. Returns an ArrayList of Tokens if a valid
// string is passed, else a ParseError. Caller must free the returned ArrayList.
fn tokenize(allocator: std.mem.Allocator, wff_string: []const u8) !std.TailQueue(Token) {
    const State = enum {
        None,
        Cond,
        BicondBegin,
        BicondEnd,
    };
    
    const List = std.TailQueue(Token);
    var tokens = List{};
    
    var state: State = State.None;
    for (wff_string) |c| {
        errdefer {
            debug.print("Invalid token: {c}\nString: {s}\n", .{c, wff_string});
            while (tokens.pop()) |node| {
                allocator.destroy(node);    
            }
        }

        if (std.ascii.isWhitespace(c)) {
            continue;
        }

        // This is the main tokenizing logic. There are 3 outcomes of this 
        // block:
        // (1) A token is correctly processed => a Token struct is assigned to
        //     tok, code flow continues normally
        // (2) An intermediate token is processed (e.g. '=' in the "<=>"
        //     operator) => state is changed appropriately (e.g. to BicondEnd)
        //     and we continue right away to the next loop iteration without
        //     assigning a value to tok.
        // (3) An unexpected token is encountered => we return an error.
        var tok = try switch(state) {
            .None => switch(c) {
                '~' => Token{.Operator = WffOperator.Not},
                'v' => Token{.Operator = WffOperator.Or},
                '^' => Token{.Operator = WffOperator.And},
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

                // TODO: Include v as an allowable variable name (currently it
                // conflicts with OR operator).
                'a'...'u', 'w'...'z', 'A'...'Z' => |val| Token{.Proposition = PropositionVar{.string = val}},

                else => ParseError.UnexpectedToken,   
            },
            .Cond => switch(c) {
                '>' => ret: {
                    state = State.None;
                    break :ret Token{.Operator = WffOperator.Cond};
                },
                else => ParseError.UnexpectedToken,
            },
            .BicondBegin => switch (c) {
                '=' => {
                    state = State.BicondEnd;
                    continue;
                },
                else => ParseError.UnexpectedToken,
            },
            .BicondEnd => switch (c) {
                '>' => ret: {
                    state = State.None;
                    break :ret Token{.Operator = WffOperator.Bicond};
                },
                else => ParseError.UnexpectedToken,
            },
        };

        var list_node = try allocator.create(List.Node);
        list_node.data = tok;
        tokens.append(list_node);
    }
    if (tokens.len == 0) {
        return ParseError.NoTokensFound;
    }
    return tokens;
}

test "tokenize: '(p v q)'" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    
    var result = try tokenize(arena.allocator(), "(p v q)");
    const expected = [_]Token {
        Token.LParen, 
        Token{.Proposition = PropositionVar{.string = 'p'}},
        Token{.Operator = WffOperator.Or},
        Token{.Proposition = PropositionVar{.string = 'q'}},
        Token.RParen,
    };

    try std.testing.expectEqual(expected.len, result.len);

    var iter = result.first;
    var i: usize = 0;
    while (iter) |node|: ({iter = node.next; i += 1;}) {
        try std.testing.expectEqual(expected[i], node.data);
    }
}

test "tokenize: ''" {
    try std.testing.expectError(ParseError.NoTokensFound, tokenize(std.testing.allocator, ""));
}

test "tokenize: 'p'" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var result = try tokenize(arena.allocator(), "p");
    const expected = [_]Token {
        Token{.Proposition = PropositionVar{.string = 'p'}}, 
    };

    try std.testing.expectEqual(expected.len, result.len);

    var iter = result.first;
    var i: usize = 0;
    while (iter) |node|: ({iter = node.next; i += 1;}) {
        try std.testing.expectEqual(expected[i], node.data);
    }
}

test "tokenize: p & q" {
    try std.testing.expectError(ParseError.UnexpectedToken, tokenize(std.testing.allocator, "p & q"));
}


fn parse(allocator: std.mem.Allocator, tokens: std.TailQueue(Token)) !ParseTree.Node {
    const StackSymbol = enum {
        Wff,
        Proposition,
        Not,
        LParen,
        RParen,
        And,
        Or,
        Cond,
        Bicond,
    };

    const Rule = enum {
        // R1: S -> wff
        R2, // wff -> Proposition
        R3, // wff -> Not    wff
        R4, // wff -> LParen wff And    wff RParen
        R5, // wff -> LParen wff Or     wff RParen
        R6, // wff -> LParen wff Cond   wff RParen
        R7  // wff -> LParen wff Bicond wff RParen
    };
    
    const State = enum {
        S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9, S10,
        S11, S12, S13, S14, S15, S16, S17, S18, S19,
    };

    const ParseState = struct {
        const Self = @This();

        state: State,
        node: ?ParseTree.Node,
    
        fn shift_terminal(state: State, symbol: StackSymbol) !State {
            return switch(state) {
                .S1, .S3, .S5, .S7, .S8, .S9, .S10 => switch(symbol) {
                    .Proposition => .S4,
                    .Not => .S3,
                    .LParen => .S5,
                    else => ParseError.TerminalShiftError,
                },
                .S6 => switch(symbol) {
                    .And => .S7,
                    .Or => .S8,
                    .Cond => .S9,
                    .Bicond => .S10,
                    else => ParseError.TerminalShiftError,
                },
                .S11 => switch(symbol) {
                    .RParen => .S12,
                    else => ParseError.TerminalShiftError,
                },
                .S13 => switch(symbol) {
                    .RParen => .S14,
                    else => ParseError.TerminalShiftError,
                },
                .S15 => switch(symbol) {
                    .RParen => .S16,
                    else => ParseError.TerminalShiftError,
                },
                .S17 => switch(symbol) {
                    .RParen => .S18,
                    else => ParseError.TerminalShiftError,
                },
                else => ParseError.TerminalShiftError,
            };
        }

        fn shift_nonterminal(state: State, symbol: StackSymbol) !State {
            return switch(symbol) {
                .Wff => switch(state) {
                    .S1 => .S2,
                    .S3 => .S19,
                    .S5 => .S6,
                    .S7 => .S11,
                    .S8 => .S13,
                    .S9 => .S15,
                    .S10 => .S17,
                    else => ParseError.NonterminalShiftError,
                },
                else => ParseError.NonterminalShiftError,
            };
        }

        fn reduce(stack: std.ArrayList(Self), rule: Rule) State {
            
        }
    };
}


