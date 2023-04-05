const std = @import("std");
const debug = std.debug;

const MAX_PARSE_CHILDREN = 5;

pub const ParseError = error {
    UnexpectedToken,
    NoTokensFound,

    // TerminalShiftError,
    // NonterminalShiftError,
    // ReduceError,
    InvalidSyntax,
    MatchError,
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

pub const Token = union(enum) {
    const Self = @This();

    LParen,
    RParen,
    Proposition: PropositionVar,
    Operator: WffOperator,

    /// Compare two Token instances. They are equal if they are of the same
    /// sub-type, and in the case of Proposition and Operator, if their stored
    /// values are equivalent.
    pub fn equals(self: Self, other: Self) bool {
        return switch(self) {
            .LParen => switch(other) {
                .LParen => true,
                else => false,
            },
            .RParen => switch(other) {
                .RParen => true,
                else => false,
            },
            .Proposition => |prop| switch(other) {
                .Proposition => |other_prop| prop.string == other_prop.string,
                else => false,
            },
            .Operator => |op| switch(other) {
                .Operator => |other_op| op == other_op,
                else => false,
            },
        };
    }
};

test "Token.equals" {
    var t1: Token = Token.LParen;
    var t2: Token = Token.LParen;
    var t3: Token = Token.RParen;
    
    var t4: Token = Token{.Operator = WffOperator.And};
    var t5: Token = Token{.Operator = WffOperator.And};
    var t6: Token = Token{.Operator = WffOperator.Or}; 

    try std.testing.expect(t1.equals(t2));
    try std.testing.expect(!t1.equals(t3));

    try std.testing.expect(t4.equals(t5));
    try std.testing.expect(!t4.equals(t6));

    try std.testing.expect(!t1.equals(t5));
}

pub const Match = struct {
    wff_node: *ParseTree.Node,
    pattern_node: *ParseTree.Node,
};


pub const ParseTreeBreadthFirstIterator = struct {
    const Self = @This();
    const Stack = std.SinglyLinkedList(*ParseTree.Node);

    stack: Stack,
    prev_child_count: usize = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tree_node: *ParseTree.Node) !Self {
        var stack = Stack{};
        var stack_node = try allocator.create(Stack.Node);
        stack_node.data = tree_node;
        stack.prepend(stack_node);

        return Self{.stack = stack, .allocator = allocator};
    }

    pub fn deinit(self: *Self) void {
        while(self.stack.popFirst()) |new_top| self.allocator.destroy(new_top);
    }

    pub fn next(self: *Self) !?*ParseTree.Node {
        const top = self.stack.popFirst() orelse return null;
        defer self.allocator.destroy(top);

        const next_node = top.data;
        switch(next_node.*) {
            .Nonterminal => |children| {
                const last = children.items.len - 1;
                var i: usize = 0;
                while (i < children.items.len): (i += 1) {
                    errdefer while(self.stack.popFirst()) |new_top| {
                        self.allocator.destroy(new_top);
                    };

                    var stack_node = try self.allocator.create(Stack.Node);
                    stack_node.data = children.items[last - i];
                    self.stack.prepend(stack_node);
                }
                self.prev_child_count = children.items.len;
            },
            else => {
                self.prev_child_count = 0;
            },
        }
        return next_node;
    }

    pub fn nextUnchecked(self: *Self) !*ParseTree.Node {
        std.debug.assert(self.hasNext());
        return (try self.next()).?;
    }

    pub fn peek(self: *Self) ?*ParseTree.Node {
        if (self.stack.first) |top| {
            return top.data;
        } else {
            return null;
        }
    }

    pub fn hasNext(self: *Self) bool {
        return self.peek() != null;
    }

    // Skip child nodes of the most recent nodes by popping them from the stack.
    // This means we won't visit the child nodes, nor their grandchildren etc.
    pub fn skipChildren(self: *Self) void {
        var i: usize = 0;
        while (i < self.prev_child_count): (i += 1) {
            self.allocator.destroy(self.stack.popFirst().?);
        }
    }
};

test "ParseTreeDepthFirstIterator" {
    var tree = try ParseTree.init(std.testing.allocator, "(p v q)");
    defer tree.deinit();

    var it = try tree.root.iterBreadthFirst(std.testing.allocator);

    var t1 = ParseTree.Node{.Terminal = Token.LParen};
    var t2 = ParseTree.Node{.Terminal = Token{.Proposition = PropositionVar{.string = 'p'}}};
    var t3 = ParseTree.Node{.Terminal = Token{.Operator = WffOperator.Or}};
    var t4 = ParseTree.Node{.Terminal = Token{.Proposition = PropositionVar{.string = 'q'}}};
    var t5 = ParseTree.Node{.Terminal = Token.RParen};

    _ = try it.next();

    try std.testing.expectEqual(t1, (try it.next()).?.*);
    _ = try it.next();
    try std.testing.expectEqual(t2, (try it.next()).?.*);
    try std.testing.expectEqual(t3, (try it.next()).?.*);
    _ = try it.next();
    try std.testing.expectEqual(t4, (try it.next()).?.*);
    try std.testing.expectEqual(t5, (try it.next()).?.*);
    try std.testing.expect((try it.next()) == null);
}


pub const ParseTree = struct {
    const Self = @This();

    pub const Node = union(enum) {
        Terminal: Token,
        Nonterminal: std.ArrayList(*Node),

        pub fn copy(self: *Self, allocator: std.mem.Allocator) ParseTree {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            var node_allocator = arena.allocator();

            var copy_root = try node_allocator.create(Node);

            // switch(self.*) {
            //     .Terminal => |tok| {
            //         copy_root.* = Node{.Terminal = tok};
            //         return copy_root;
            //     },
            //     .Nonterminal => |children| {
            //         copy_root.* = Node{.Nonterminal = try std.ArrayList(*Node).initCapacity(node_allocator, children.len)};
            //         var i: usize = 0;
            //         while (i < children.len): (i += 1) {
            //             try copy_root.Nonterminal.append(try node_allocator.create(Node));
            //         }
            //     },
            // }

            var it = try self.iterBreadthFirst(allocator);
            defer it.deinit();
            //_ = it.next();
            var copy_it = try copy_root.iterBreadthFirst(allocator);
            defer copy_it.deinit();
            //_ = copy_it.next();


            while (it.hasNext() and copy_it.hasNext()) {
                const node = try it.nextUnchecked();
                var copy_node = try copy_it.nextUnchecked();

                switch(node.*) {
                    .Terminal => |tok| copy_node.* = Node{.Terminal = tok},
                    .Nonterminal => |children| {
                        copy_node.* = Node{.Nonterminal = try std.ArrayList(*Node).initCapacity(node_allocator, children.len)};
                        var i: usize = 0;
                        while (i < children.len): (i += 1) {
                            try copy_root.Nonterminal.append(try node_allocator.create(Node));
                        }
                    }
                }
            }            

            return ParseTree{.root = copy_root, .allocator = allocator, .node_arena = arena};

            // var it = try self.iterBreadthFirst(stack_allocator);
            // defer it.deinit();
            // const Queue = std.TailQueue(*Node);
            // var nonterminals = Queue{};
            // errdefer while(nonterminals.pop()) |q_node| stack_allocator.destroy(q_node);
            
            // var parent = root;
            // while (it.hasNext()): (parent = nonterminals.popFirst().?) {
            //     var i: usize = 0;
            //     while (i < parent.Nonterminal.items.len): (i += 1) {
            //         var node = try node_allocator.create(Node);

            //         switch(try it.nextUnchecked()) {
            //             .Terminal => |tok| {
            //                 node.* = Node{.Terminal = tok};
            //             },
            //             .Nonterminal => |children| {
            //                 node.* = Node{.Nonterminal = try std.ArrayList(*Node).initCapacity(node_allocator, children.items.len)};
                            
            //                 var q_node = try stack_allocator.create(Queue.Node);
            //                 q_node.data = node;
            //                 nonterminals.append(q_node);
            //             }
            //         }
            //         try parent.Nonterminal.append(node);
            //     }
            // }
            // return root;
        }

        pub fn iterBreadthFirst(self: *Node, stack_allocator: std.mem.Allocator) !ParseTreeBreadthFirstIterator {
            return try ParseTreeBreadthFirstIterator.init(stack_allocator, self);
        }

        pub fn equals(self: *Node, stack_allocator: std.mem.Allocator, other: *Node) !bool {
            var it = try self.iterBreadthFirst(stack_allocator);
            defer it.deinit();
            var other_it = try other.iterBreadthFirst(stack_allocator);
            defer other_it.deinit();

            while (it.hasNext() and other_it.hasNext()) {
                const node = try it.nextUnchecked();
                const other_node = try other_it.nextUnchecked();

                switch(node.*) {
                    .Terminal => |tok| switch(other_node.*) {
                        .Terminal => |other_tok| if (!tok.equals(other_tok)) return false,
                        .Nonterminal => return false,
                    },
                    .Nonterminal => switch(other_node.*) {
                        .Terminal => return false,
                        .Nonterminal => continue,
                    }
                }
            }
            return it.peek() == other_it.peek();
        }

        fn match(self: *Node, stack_allocator: std.mem.Allocator, pattern: *Node) !?std.ArrayList(Match) {
            var matches = std.ArrayList(Match).init(stack_allocator);
            errdefer matches.deinit();

            var it = try self.iterBreadthFirst(stack_allocator);
            defer it.deinit();
            var pattern_it = try pattern.iterBreadthFirst(stack_allocator);
            defer pattern_it.deinit();

            while (it.hasNext() and pattern_it.hasNext()) {
                const node = try it.nextUnchecked();
                const pattern_node = try pattern_it.nextUnchecked();

                const is_match = switch(node.*) {
                    .Terminal => |tok| switch(pattern_node.*) {
                        .Terminal => |pattern_tok| tok.equals(pattern_tok),
                        .Nonterminal => false,
                    },
                    .Nonterminal => |children| switch(pattern_node.*) {
                        .Terminal => false,
                        .Nonterminal => |pattern_children| ret: {
                            if (pattern_children.items.len == 1) {
                                try matches.append(Match{.wff_node = node, .pattern_node = pattern_node});
                                it.skipChildren();
                                pattern_it.skipChildren();
                                break :ret true;
                            } else if (children.items.len == pattern_children.items.len) {
                                continue;
                            } else {
                                break :ret false;
                            }
                        },
                    }
                }; // outer switch

                if (!is_match) {
                    matches.deinit();
                    return null;
                }
            } // while
            return matches;
        }
    };

    root: *Node,
    allocator: std.mem.Allocator,
    node_arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !ParseTree {
        var arena = std.heap.ArenaAllocator.init(allocator);
        var parser = Parser.init(allocator, arena.allocator());
        defer parser.deinit();

        return ParseTree{.root = (try parser.parse(wff_string)).?, .allocator = allocator, .node_arena = arena};
    }

    pub fn deinit(self: *Self) void {
        self.node_arena.deinit();
    }

    /// Compare two ParseTree instances. They are equal if both have the same
    /// tree structure (determined by non-terminal nodes, which are the only 
    /// nodes with children) and if each pair of corresponding terminal nodes
    /// have equivalent tokens (see Token.equals).
    pub fn equals(self: *Self, other: *Self) !bool {
        return self.root.equals(other);
    }

    pub fn iterBreadthFirst(self: *Self) !ParseTreeBreadthFirstIterator {
        return self.root.iterBreadthFirst(self.allocator);
    }

    // !!!!!!!!!!!
    // TODO: Idea: .equals returns an iterator of pairs if equal, then we just
    // iterate over those pairs. We can then make assumptions that make this
    // matching simpler, since we know the trees are equal.
    /// Similar to ParseTree.equals, but in addition to checking the equality
    /// of the trees, we also record "matches". A match consists of a pointer
    /// to a proposition node in a pattern/template tree, and a pointer to a
    /// corresponding proposition or non-terminal (wff) in self.
    /// Assumes both parse trees are valid.
    /// Returns a list of match lists (ArrayList(ArrayList(Match))) if the trees
    /// are equal, else null.
    pub fn matchAll(self: *Self, pattern: *Self) !?std.ArrayList(std.ArrayList(Match)) {
        var all_matches = std.ArrayList(std.ArrayList(Match)).init(self.allocator);
        errdefer {
            for (all_matches.items) |matches| {
                matches.deinit();
            }
            all_matches.deinit();
        }
    
        var it = try self.iterBreadthFirst();
        defer it.deinit();
        var pattern_it = try pattern.iterBreadthFirst();
        defer pattern_it.deinit();

        while (it.hasNext() and pattern_it.hasNext()) {
            const node = try it.nextUnchecked();
            const pattern_node = try pattern_it.nextUnchecked();

            const is_match = switch(node.*) {
                .Terminal => |tok| switch(pattern_node.*) {
                    .Terminal => |pattern_tok| tok.equals(pattern_tok),
                    .Nonterminal => false,
                },
                .Nonterminal => switch(pattern_node.*) {
                    .Terminal => false,
                    .Nonterminal => ret: {
                        if (try node.match(self.allocator, pattern_node)) |matches| {
                            try all_matches.append(matches);
                            it.skipChildren();
                            pattern_it.skipChildren();
                            break :ret true;
                        } else {
                            continue;
                        }
                    }
                },
            };
            
            if (!is_match) {
                for (all_matches.items) |matches| {
                    matches.deinit();
                }
                all_matches.deinit();
                return null;
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

// TODO: fn to build trees more easily for testing larger expressions

test "ParseTree: ''" {
    try std.testing.expectError(ParseError.NoTokensFound, ParseTree.init(std.testing.allocator, ""));
}

test "ParseTree: ')q p v('" {
    try std.testing.expectError(ParseError.InvalidSyntax, ParseTree.init(std.testing.allocator, ")q p v ("));
}

test "ParseTree: '(p v q)'" {
    var allocator = std.testing.allocator;
    var tree = try ParseTree.init(allocator, "(p v q)");
    defer tree.deinit();

    // Build the expected tree manually
    var t1 = ParseTree.Node{.Terminal = Token.LParen};
    var t2 = ParseTree.Node{.Terminal = Token{.Proposition = PropositionVar{.string = 'p'}}};
    var t3 = ParseTree.Node{.Terminal = Token{.Operator = WffOperator.Or}};
    var t4 = ParseTree.Node{.Terminal = Token{.Proposition = PropositionVar{.string = 'q'}}};
    var t5 = ParseTree.Node{.Terminal = Token.RParen};

    var children1 = std.ArrayList(*ParseTree.Node).init(allocator);
    defer children1.deinit();
    try children1.append(&t2);
    var children2 = std.ArrayList(*ParseTree.Node).init(allocator);
    defer children2.deinit();
    try children2.append(&t4);
    var wff1 = ParseTree.Node{.Nonterminal = children1};
    var wff2 = ParseTree.Node{.Nonterminal = children2};
    var children3 = std.ArrayList(*ParseTree.Node).init(allocator);
    defer children3.deinit();
    try children3.appendSlice(&([_]*ParseTree.Node {&t1, &wff1, &t3, &wff2, &t5}));
    var wff3 = ParseTree.Node{.Nonterminal = children3};
   
    // Hardcoded equality test of tree
    try std.testing.expectEqual(t1, tree.root.Nonterminal.items[0].*);
    try std.testing.expectEqual(t2, tree.root.Nonterminal.items[1].Nonterminal.items[0].*);
    try std.testing.expectEqual(t3, tree.root.Nonterminal.items[2].*);
    try std.testing.expectEqual(t4, tree.root.Nonterminal.items[3].Nonterminal.items[0].*);
    try std.testing.expectEqual(t5, tree.root.Nonterminal.items[4].*);

    // Function equality test of tree
    try std.testing.expect(try tree.root.equals(allocator, &wff3));
}

test "ParseTree.Node.match: (p v q)" {
    var allocator = std.testing.allocator;
    var tree = try ParseTree.init(allocator, "(p v q)");
    defer tree.deinit();
    var pattern = try ParseTree.init(allocator, "(p v q)");
    defer pattern.deinit();

    const matches = try tree.root.match(allocator, pattern.root) orelse return ParseError.MatchError;
    defer matches.deinit();

    const expected = [_]Match {
        Match{
            .wff_node = tree.root.Nonterminal.items[1],
            .pattern_node = pattern.root.Nonterminal.items[1],
        },
        Match{
            .wff_node = tree.root.Nonterminal.items[3],
            .pattern_node = pattern.root.Nonterminal.items[3],
        },
    };

    try std.testing.expectEqualSlices(Match, &expected, matches.items);
}

test "ParseTree.Node.match: ((a ^ b) v (c ^ d))" {
    var allocator = std.testing.allocator;
    var tree = try ParseTree.init(allocator, "((a ^ b) v (c ^ d))");
    defer tree.deinit();
    var pattern = try ParseTree.init(allocator, "(p v q)");
    defer pattern.deinit();

    const matches = try tree.root.match(allocator, pattern.root) orelse return ParseError.MatchError;
    defer matches.deinit();

    const expected = [_]Match {
        Match{
            .wff_node = tree.root.Nonterminal.items[1],
            .pattern_node = pattern.root.Nonterminal.items[1],
        },
        Match{
            .wff_node = tree.root.Nonterminal.items[3],
            .pattern_node = pattern.root.Nonterminal.items[3],
        },
    };

    try std.testing.expectEqualSlices(Match, &expected, matches.items);
}

test "ParseTree.matchAll: (p v q)" {
    var allocator = std.testing.allocator;
    var tree = try ParseTree.init(allocator, "(p v q)");
    defer tree.deinit();
    var pattern = try ParseTree.init(allocator, "(p v q)");
    defer pattern.deinit();

    var all_matches = try tree.matchAll(&pattern);
    defer {
        if (all_matches) |all| {
            for (all.items) |matches| {
                    matches.deinit();
            }
            all.deinit();
        }
    }

    try std.testing.expect(all_matches.?.items.len == 1);

    var match1 = all_matches.?.items[0];

    const expected = [_]Match {
        Match{
            .wff_node = tree.root.Nonterminal.items[1],
            .pattern_node = pattern.root.Nonterminal.items[1],
        },
        Match{
            .wff_node = tree.root.Nonterminal.items[3],
            .pattern_node = pattern.root.Nonterminal.items[3],
        },
    };

    try std.testing.expectEqualSlices(Match, &expected, match1.items);
}

// Tokenize a string containing a WFF. Returns an ArrayList of Tokens if a valid
// string is passed, else a ParseError. Caller must free the returned ArrayList.
fn tokenize(allocator: std.mem.Allocator, wff_string: []const u8) !std.ArrayList(Token) {
    const State = enum {
        None,
        Cond,
        BicondBegin,
        BicondEnd,
    };
    
    var tokens = std.ArrayList(Token).init(allocator);
    
    var state: State = State.None;
    for (wff_string) |c| {
        errdefer {
            debug.print("\n\tInvalid token: {c}\n\tProcessed tokens: {any}\n", .{c, tokens.items});
            tokens.deinit();
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

        try tokens.append(tok);
    }
    if (tokens.items.len == 0) {
        return ParseError.NoTokensFound;
    }
    return tokens;
}

test "tokenize: '(p v q)'" {
    var result = try tokenize(std.testing.allocator, "(p v q)");
    defer result.deinit();
    const expected = [_]Token {
        Token.LParen, 
        Token{.Proposition = PropositionVar{.string = 'p'}},
        Token{.Operator = WffOperator.Or},
        Token{.Proposition = PropositionVar{.string = 'q'}},
        Token.RParen,
    };

    try std.testing.expectEqualSlices(Token, &expected, result.items);
}

test "tokenize: ''" {
    try std.testing.expectError(ParseError.NoTokensFound, tokenize(std.testing.allocator, ""));
}

test "tokenize: 'p'" {
    var result = try tokenize(std.testing.allocator, "p");
    defer result.deinit();
    const expected = [_]Token {
        Token{.Proposition = PropositionVar{.string = 'p'}}, 
    };

    try std.testing.expectEqualSlices(Token, &expected, result.items);
}

test "tokenize: p & q" {
    try std.testing.expectError(ParseError.UnexpectedToken, tokenize(std.testing.allocator, "p & q"));
}


/// Shift-Reduce parser for parsing WFF's. 
/// Based on these grammar rules:
/// R1: S   -> wff
/// R2: wff -> Proposition
/// R3: wff -> Not    wff
/// R4: wff -> LParen wff And    wff RParen
/// R5: wff -> LParen wff Or     wff RParen
/// R6: wff -> LParen wff Cond   wff RParen
/// R7: wff -> LParen wff Bicond wff RParen
const Parser = struct {
    const Self = @This();

    const Rule = enum {
        // R1: S -> wff  (we don't need this rule in practice)
        R2, // wff -> Proposition
        R3, // wff -> Not    wff
        R4, // wff -> LParen wff And    wff RParen
        R5, // wff -> LParen wff Or     wff RParen
        R6, // wff -> LParen wff Cond   wff RParen
        R7, // wff -> LParen wff Bicond wff RParen
    };

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
    
    const State = enum {
        S1,  S2,  S3,  S4,  S5,  S6,  S7,  S8,  S9,  S10,
        S11, S12, S13, S14, S15, S16, S17, S18, S19,
    };

    const StackItem = struct {
        state: State,
        node: ?*ParseTree.Node = null,
    };

    stack: std.ArrayList(StackItem),
    stack_allocator: std.mem.Allocator,
    tree_allocator: std.mem.Allocator,

    pub fn init(stack_allocator: std.mem.Allocator, tree_allocator: std.mem.Allocator) Self {
        return Self{
            .stack = std.ArrayList(StackItem).init(stack_allocator),
            .stack_allocator = stack_allocator,
            .tree_allocator = tree_allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    fn shift_terminal(state: State, symbol: StackSymbol) !State {
        return switch(state) {
            .S1, .S3, .S5, .S7, .S8, .S9, .S10 => switch(symbol) {
                .Proposition => .S4,
                .Not => .S3,
                .LParen => .S5,
                else => ParseError.InvalidSyntax,
            },
            .S6 => switch(symbol) {
                .And => .S7,
                .Or => .S8,
                .Cond => .S9,
                .Bicond => .S10,
                else => ParseError.InvalidSyntax,
            },
            .S11 => switch(symbol) {
                .RParen => .S12,
                else => ParseError.InvalidSyntax,
            },
            .S13 => switch(symbol) {
                .RParen => .S14,
                else => ParseError.InvalidSyntax,
            },
            .S15 => switch(symbol) {
                .RParen => .S16,
                else => ParseError.InvalidSyntax,
            },
            .S17 => switch(symbol) {
                .RParen => .S18,
                else => ParseError.InvalidSyntax,
            },
            else => ParseError.InvalidSyntax,
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
                else => ParseError.InvalidSyntax,
            },
            else => ParseError.InvalidSyntax,
        };
    }

    fn reduce(self: *Self, state: State) !?State {
        const rule = switch(state) {
            .S4 => Rule.R2,
            .S12 => Rule.R4,
            .S14 => Rule.R5,
            .S16 => Rule.R6,
            .S18 => Rule.R7,
            .S19 => Rule.R3,
            else => return null,
        };

        const child_count: usize = switch(rule) {
            .R2 => 1,
            .R3 => 2,
            .R4, .R5, .R6, .R7 => 5,
        };

        var children = std.ArrayList(*ParseTree.Node).init(self.tree_allocator);
        var i: usize = 0;
        while (i < child_count): (i += 1) {
            try children.append(self.stack.pop().node.?);
        }
        std.mem.reverse(*ParseTree.Node, children.items[0..child_count]);

        const new_top_state = self.stack.items[self.stack.items.len - 1].state;
        const reduced_state = try shift_nonterminal(new_top_state, StackSymbol.Wff);

        var new_node = try self.tree_allocator.create(ParseTree.Node);
        new_node.* = ParseTree.Node{.Nonterminal = children};
        try self.stack.append(StackItem{.state = reduced_state, .node = new_node});

        return reduced_state;
    }

    fn parse(self: *Self, wff_string: []const u8) !?*ParseTree.Node {
        // Tokenize given string
        var tokens = try tokenize(self.stack_allocator, wff_string);
        defer tokens.deinit();

        // Initialize parsing stack
        try self.stack.append(StackItem{.state = .S1}); // Initial state is S1

        // Parse tokens
        for (tokens.items) |token| {
            const top_state = self.stack.items[self.stack.items.len - 1].state;

            const stack_symbol = switch(token) {
                .LParen => StackSymbol.LParen,
                .RParen => StackSymbol.RParen,
                .Operator => |op| switch(op) {
                    .And => StackSymbol.And,
                    .Or => StackSymbol.Or,
                    .Not => StackSymbol.Not,
                    .Cond => StackSymbol.Cond,
                    .Bicond => StackSymbol.Bicond
                },
                .Proposition => StackSymbol.Proposition,
            }; 
            
            var new_state = try shift_terminal(top_state, stack_symbol);
            var new_node = try self.tree_allocator.create(ParseTree.Node);
            new_node.* = ParseTree.Node{.Terminal = token};
            try self.stack.append(StackItem{.state = new_state, .node = new_node});

            while (try self.reduce(new_state)) |result_state| {
                new_state = result_state;
            }
        }

        return self.stack.pop().node.?;
    }
};


test "Parse" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = Parser.init(arena.allocator(), arena.allocator());
    _ = try parser.parse("(p v q)");
}

