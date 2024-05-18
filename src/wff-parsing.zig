const std = @import("std");
const debug = std.debug;

const lex = @import("wff-lexing.zig");
const slr = @import("slr-table-generator.zig");

pub const ParseError = error{
    InvalidSyntax,
    UnexpectedToken,
};

const ParseTree = OldParseTree;

pub const MatchHashMap = std.StringHashMap(*OldParseTree.Node);

pub const ParseTreeDepthFirstIterator = struct {
    const Self = @This();

    start: *const OldParseTree.Node,
    current: ?[*]OldParseTree.Node,

    fn backtrack(self: Self) ?[*]OldParseTree.Node {
        var next_node = self.current.?;
        while (next_node[0].parent) |parent| {
            if (@as(@TypeOf(self.start), @ptrCast(next_node)) == self.start) return null;
            const siblings = parent.data.Nonterminal.items;
            if (&(next_node[0]) == &(siblings[siblings.len - 1])) {
                next_node = @ptrCast(parent);
            } else {
                return next_node + 1;
            }
        }
        return null;
    }

    pub fn next(self: *Self) ?*OldParseTree.Node {
        const current_node = &(self.current orelse return null)[0];

        self.current = switch (current_node.data) {
            .Terminal => self.backtrack(),
            .Nonterminal => |children| @ptrCast(&children.items[0]),
        };

        return current_node;
    }

    pub fn nextUnchecked(self: *Self) *OldParseTree.Node {
        std.debug.assert(self.hasNext());
        return self.next().?;
    }

    pub fn peek(self: Self) ?*OldParseTree.Node {
        if (self.current) |node| {
            return &node[0];
        } else {
            return null;
        }
    }

    pub fn hasNext(self: Self) bool {
        return self.peek() != null;
    }

    // This means we won't visit this node, its child nodes, nor their grandchildren etc.
    pub fn skipChildren(self: *Self) void {
        const current = &(self.current orelse return)[0];
        if (current.parent) |parent| {
            self.current = @ptrCast(parent);
            self.current = self.backtrack();
        } else {
            self.current = null;
        }
    }
};

// test "ParseTreeDepthFirstIterator" {
//     var tree = try OldParseTree.init(std.testing.allocator, "(p v q)");
//     defer tree.deinit();

//     try std.testing.expectEqual(tree.root, tree.root.data.Nonterminal.items[0].parent.?);
//     const c: *OldParseTree.Node = tree.root.data.Nonterminal.items[1].parent.?;
//     try std.testing.expectEqual(c, c.data.Nonterminal.items[0].parent.?);

//     var it = tree.root.iterDepthFirst();

//     var t1 = OldParseTree.Data{ .Terminal = lex.Token.LParen };
//     var t2 = OldParseTree.Data{ .Terminal = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } } };
//     defer std.testing.allocator.free(t2.Terminal.Proposition.string);
//     var t3 = OldParseTree.Data{ .Terminal = lex.Token{ .Operator = lex.WffOperator.Or } };
//     var t4 = OldParseTree.Data{ .Terminal = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "q") } } };
//     defer std.testing.allocator.free(t4.Terminal.Proposition.string);
//     var t5 = OldParseTree.Data{ .Terminal = lex.Token.RParen };

//     _ = it.next();
//     try std.testing.expect(t1.Terminal.eql(it.next().?.data.Terminal));
//     _ = it.next();
//     try std.testing.expect(t2.Terminal.eql(it.next().?.data.Terminal));
//     try std.testing.expect(t3.Terminal.eql(it.next().?.data.Terminal));
//     _ = it.next();
//     try std.testing.expect(t4.Terminal.eql(it.next().?.data.Terminal));
//     try std.testing.expect(t5.Terminal.eql(it.next().?.data.Terminal));
//     try std.testing.expect((it.next()) == null);
// }

pub const ParseTreePostOrderIterator = struct {
    const Self = @This();

    root: *const OldParseTree.Node,
    current: ?[*]OldParseTree.Node,

    // if terminal, go to parent
    //      if came from last child, set next=parent
    //      else, set next to next deepest sibling
    // else, get parent
    //      if parent is null, set next=null
    //      else if came from last child, set next=parent
    //      else, set next to next deepest sibling
    pub fn next(self: *Self) ?*OldParseTree.Node {
        const current_node = &(self.current orelse return null)[0];
        const parent = current_node.parent orelse {
            self.current = null;
            return current_node;
        };
        const siblings = parent.data.Nonterminal.items;

        if (current_node == self.root) {
            self.current = null;
            return current_node;
        } else if (current_node == &siblings[siblings.len - 1]) {
            self.current = @ptrCast(parent);
            return current_node;
        }

        var next_node = &(self.current.?[1]); // get sibling node
        while (true) {
            switch (next_node.data) {
                .Terminal => break,
                .Nonterminal => |children| next_node = &children.items[0],
            }
        }
        self.current = @ptrCast(next_node);

        return current_node;
    }

    pub fn nextUnchecked(self: *Self) *OldParseTree.Node {
        std.debug.assert(self.hasNext());
        return self.next().?;
    }

    pub fn peek(self: Self) ?*OldParseTree.Node {
        if (self.current) |node| {
            return &node[0];
        } else {
            return null;
        }
    }

    pub fn hasNext(self: Self) bool {
        return self.peek() != null;
    }
};

// test "ParseTreePostOrderIterator" {
//     var tree = try OldParseTree.init(std.testing.allocator, "(p v q)");
//     defer tree.deinit();

//     var it = tree.root.iterPostOrder();

//     var t1 = OldParseTree.Data{ .Terminal = lex.Token.LParen };
//     var t2 = OldParseTree.Data{ .Terminal = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } } };
//     defer std.testing.allocator.free(t2.Terminal.Proposition.string);
//     var t3 = OldParseTree.Data{ .Terminal = lex.Token{ .Operator = lex.WffOperator.Or } };
//     var t4 = OldParseTree.Data{ .Terminal = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "q") } } };
//     defer std.testing.allocator.free(t4.Terminal.Proposition.string);
//     var t5 = OldParseTree.Data{ .Terminal = lex.Token.RParen };

//     try std.testing.expect(t1.Terminal.eql(it.nextUnchecked().data.Terminal));
//     try std.testing.expect(t2.Terminal.eql(it.nextUnchecked().data.Terminal));
//     _ = it.next();
//     try std.testing.expect(t3.Terminal.eql(it.nextUnchecked().data.Terminal));
//     try std.testing.expect(t4.Terminal.eql(it.nextUnchecked().data.Terminal));
//     _ = it.next();
//     try std.testing.expect(t5.Terminal.eql(it.nextUnchecked().data.Terminal));
//     _ = it.next();
//     try std.testing.expect(it.next() == null);
// }

pub const OldParseTree = struct {
    const Self = @This();

    root: *Node,
    allocator: std.mem.Allocator,

    pub const Data = union(enum) {
        Terminal: lex.Token,
        Nonterminal: std.ArrayList(Node),
    };

    // Note: Any time you deal with nodes, you should probably deal with
    // pointers. Nodes are stored in a way that makes using a local copy
    // incorrect for many things (such as iteration).
    pub const Node = struct {
        parent: ?*Node = null,
        data: Data,

        pub fn copy(self: *Node, allocator: std.mem.Allocator) !*Node {            
            var copy_root = try allocator.create(Node);
            copy_root.parent = null;

            var it = self.iterDepthFirst();
            var copy_it = copy_root.iterDepthFirst();

            while (it.hasNext()) {
                const node = it.nextUnchecked();
                var copy_node = copy_it.peek().?;

                switch (node.data) {
                    .Terminal => |tok| copy_node.data = Data{ .Terminal = try tok.copy(allocator) },
                    .Nonterminal => |children| {
                        copy_node.data = Data{ .Nonterminal = try std.ArrayList(Node).initCapacity(allocator, children.items.len) };
                        copy_node.data.Nonterminal.items.len = children.items.len;
                        for (copy_node.data.Nonterminal.items) |*child| {
                            child.parent = copy_node;
                        }
                    },
                }
                _ = copy_it.next();
            }

            return copy_root;
        }

        pub fn copyAbove(self: *Node, allocator: std.mem.Allocator, partial: Node) !*Node {
            var node = self;
            var copy_node = partial;
            while (node.parent) |parent| {
                var copy_children = try std.ArrayList(Node).initCapacity(allocator, parent.data.Nonterminal.items.len);

                for (parent.data.Nonterminal.items) |*child| {
                    var copy_child: Node = undefined; 
                    if (child == node) {
                        copy_child = copy_node;
                    } else {
                        const c = try child.copy(allocator);
                        copy_child = c.*;
                        allocator.destroy(c);
                    }

                    copy_children.appendAssumeCapacity(copy_child);
                    
                    switch(copy_child.data) {
                        .Terminal => {},
                        .Nonterminal => |grandchildren| for (grandchildren.items) |*grandchild| {
                            grandchild.parent = &copy_children.items[copy_children.items.len - 1];
                        }
                    }
                }
                node = parent;
                copy_node = Node{.data = Data{.Nonterminal = copy_children}};
            }

            const copy_root = try allocator.create(Node);
            copy_root.* = copy_node;

            for (copy_root.data.Nonterminal.items) |*child| {
                child.parent = copy_root;
            }

            return copy_root;
        }

        pub fn iterDepthFirst(self: *Node) ParseTreeDepthFirstIterator {
            return ParseTreeDepthFirstIterator{ .start = self, .current = @ptrCast(self) };
        }

        pub fn iterPostOrder(self: *Node) ParseTreePostOrderIterator {
            var start_node = self;
            while (true) {
                switch (start_node.data) {
                    .Terminal => break,
                    .Nonterminal => |children| start_node = &children.items[0],
                }
            }
            return ParseTreePostOrderIterator{ .root = self, .current = @ptrCast(start_node) };
        }

        // TODO: Specify *const here and in other places
        // We would need to differentiate between mutable and immutable
        // iteration to make this possible.
        pub fn eql(self: *Node, other: *Node) bool {
            var it = self.iterPostOrder();
            var other_it = other.iterPostOrder();

            while (it.hasNext() and other_it.hasNext()) {
                const node = it.nextUnchecked();
                const other_node = other_it.nextUnchecked();

                switch (node.data) {
                    .Terminal => |tok| switch (other_node.data) {
                        .Terminal => |other_tok| if (!tok.eql(other_tok)) return false,
                        .Nonterminal => return false,
                    },
                    .Nonterminal => switch (other_node.data) {
                        .Terminal => return false,
                        .Nonterminal => continue,
                    },
                }
            }
            return true;
        }

        /// Allocator will be used to create the hashmap for storing matches
        pub fn match(self: *Node, allocator: std.mem.Allocator, pattern: *Node) !?MatchHashMap {
            var matches = MatchHashMap.init(allocator);
            errdefer matches.deinit();

            var it = self.iterDepthFirst();
            var pattern_it = pattern.iterDepthFirst();

            while (it.hasNext() and pattern_it.hasNext()) {
                const node = it.nextUnchecked();
                const pattern_node = pattern_it.nextUnchecked();

                const is_match = switch (node.data) {
                    .Terminal => |tok| switch (pattern_node.data) {
                        .Terminal => |pattern_tok| tok.eql(pattern_tok),
                        .Nonterminal => false,
                    },
                    .Nonterminal => |children| switch (pattern_node.data) {
                        .Terminal => false,
                        .Nonterminal => |pattern_children| ret: {
                            if (pattern_children.items.len == 1) {
                                it.skipChildren();
                                pattern_it.skipChildren();

                                const child_tok = pattern_children.items[0].data.Terminal;
                                const proposition_str = child_tok.getString();
                                if (matches.get(proposition_str)) |existing_match| {
                                    if (!existing_match.eql(node)) {
                                         _ = matches.remove(proposition_str);
                                         //allocator.free(pair.key);
                                    }
                                } else {
                                    try matches.put(proposition_str, node);
                                }
                                continue;
                            } else if (children.items.len == pattern_children.items.len) {
                                continue;
                            } else {
                                break :ret false;
                            }
                        },
                    },
                }; // outer switch

                if (!is_match) {
                    matches.deinit();
                    return null;
                }
            } // while
            if (matches.count() > 0) {
                return matches;
            } else {
                matches.deinit();
                return null;
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !OldParseTree {
        var parser = OldParser.init(allocator);
        defer parser.deinit();

        return OldParseTree{ .root = try parser.parse(wff_string), .allocator = allocator };
    }

    pub fn deinit(self: Self) void {
        var it = self.iterPostOrder();
        while (it.next()) |node| {
            const n = node;
            switch (n.data) {
                .Terminal => |tok| switch (tok) {
                    .Proposition => |prop| self.allocator.free(prop.string),
                    else => {},
                },
                .Nonterminal => |children| children.deinit(),
            }
        }
        self.allocator.destroy(self.root);
    }

    pub fn copy(self: Self) !OldParseTree {
        return OldParseTree{ .root = try self.root.copy(self.allocator), .allocator = self.allocator };
    }

    /// Compare two ParseTree instances. They are equal if both have the same
    /// tree structure (determined by non-terminal nodes, which are the only
    /// nodes with children) and if each pair of corresponding terminal nodes
    /// have equivalent tokens (see lex.Token.equals).
    pub fn eql(self: Self, other: Self) bool {
        return self.root.eql(other.root);
    }

    pub fn iterDepthFirst(self: Self) ParseTreeDepthFirstIterator {
        return self.root.iterDepthFirst();
    }

    pub fn iterPostOrder(self: Self) ParseTreePostOrderIterator {
        return self.root.iterPostOrder();
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) ![]u8 {
        var string_buf = std.ArrayList(u8).init(allocator);
        errdefer string_buf.deinit();

        var it = self.iterDepthFirst();
        while (it.next()) |node| {
            switch (node.data) {
                .Terminal => |*tok| {
                    switch (tok.*) {
                        .LParen => try string_buf.appendSlice(tok.getString()),
                        .RParen => {
                            _ = string_buf.pop();
                            try string_buf.appendSlice(tok.getString());
                            try string_buf.append(' ');
                        },
                        .Operator => |op| {
                            try string_buf.appendSlice(tok.getString());
                            switch (op) {
                                .And, .Or, .Cond, .Bicond => try string_buf.append(' '),
                                .Not => {},
                            }
                        },
                        .Proposition, .True, .False => {
                            try string_buf.appendSlice(tok.getString());
                            try string_buf.append(' ');
                        },
                    }
                },
                .Nonterminal => {},
            }
        }
        _ = string_buf.pop(); // remove trailing space
        return string_buf.toOwnedSlice();
    }
    
    // TODO: Matchall Idea: .equals returns an iterator of pairs if equal, then we just
    // iterate over those pairs. We can then make assumptions that make this
    // matching simpler, since we know the trees are equal.
   
};

pub fn NewParseTree(comptime Token: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        root: *Node,

        pub const Kind = union(enum) {
            leaf: Token,
            nonleaf: []Node,
        };

        // Note: Any time you deal with nodes, you should probably deal with
        // pointers. Nodes are stored in a way that makes using a local copy
        // incorrect for many things (such as iteration).
        pub const Node = struct {
            parent: ?*Node = null,
            kind: Kind,

            pub fn asString(self: Node) []const u8 {
                return switch(self.kind) {
                    .leaf => |t| t.getString(),
                    .nonleaf => "wff",
                };
            }

            // pub fn copy(self: *Node, allocator: std.mem.Allocator) !*Node {            
            //     var copy_root = try allocator.create(Node);
            //     copy_root.parent = null;

            //     var it = self.iterDepthFirst();
            //     var copy_it = copy_root.iterDepthFirst();

            //     while (it.hasNext()) {
            //         const node = it.nextUnchecked();
            //         var copy_node = copy_it.peek().?;

            //         switch (node.data) {
            //             .leaf => |tok| copy_node.data = Kind{ .Terminal = try tok.copy(allocator) },
            //             .nonleaf => |children| {
            //                 copy_node.data = Kind{ .Nonterminal = try std.ArrayList(Node).initCapacity(allocator, children.items.len) };
            //                 copy_node.data.Nonterminal.items.len = children.items.len;
            //                 for (copy_node.data.Nonterminal.items) |*child| {
            //                     child.parent = copy_node;
            //                 }
            //             },
            //         }
            //         _ = copy_it.next();
            //     }

            //     return copy_root;
            // }

            // pub fn copyAbove(self: *Node, allocator: std.mem.Allocator, partial: Node) !*Node {
            //     var node = self;
            //     var copy_node = partial;
            //     while (node.parent) |parent| {
            //         var copy_children = try std.ArrayList(Node).initCapacity(allocator, parent.kind.Nonterminal.items.len);

            //         for (parent.kind.Nonterminal.items) |*child| {
            //             var copy_child: Node = undefined; 
            //             if (child == node) {
            //                 copy_child = copy_node;
            //             } else {
            //                 const c = try child.copy(allocator);
            //                 copy_child = c.*;
            //                 allocator.destroy(c);
            //             }

            //             copy_children.appendAssumeCapacity(copy_child);
                        
            //             switch(copy_child.kind) {
            //                 .leaf => {},
            //                 .nonleaf => |grandchildren| for (grandchildren.items) |*grandchild| {
            //                     grandchild.parent = &copy_children.items[copy_children.items.len - 1];
            //                 }
            //             }
            //         }
            //         node = parent;
            //         copy_node = Node{.data = Kind{.Nonterminal = copy_children}};
            //     }

            //     const copy_root = try allocator.create(Node);
            //     copy_root.* = copy_node;

            //     for (copy_root.data.Nonterminal.items) |*child| {
            //         child.parent = copy_root;
            //     }

            //     return copy_root;
            // }

            // pub fn iterDepthFirst(self: *Node) ParseTreeDepthFirstIterator {
            //     return ParseTreeDepthFirstIterator{ .start = self, .current = @ptrCast(self) };
            // }

            // pub fn iterPostOrder(self: *Node) ParseTreePostOrderIterator {
            //     var start_node = self;
            //     while (true) {
            //         switch (start_node.kind) {
            //             .leaf => break,
            //             .nonleaf => |children| start_node = &children[0],
            //         }
            //     }
            //     return ParseTreePostOrderIterator{ .root = self, .current = @ptrCast(start_node) };
            // }

            // TODO: Specify *const here and in other places
            // We would need to differentiate between mutable and immutable
            // iteration to make this possible.
            // pub fn eql(self: *Node, other: *Node) bool {
            //     var it = self.iterPostOrder();
            //     var other_it = other.iterPostOrder();

            //     while (it.hasNext() and other_it.hasNext()) {
            //         const node = it.nextUnchecked();
            //         const other_node = other_it.nextUnchecked();

            //         switch (node.data) {
            //             .leaf => |tok| switch (other_node.data) {
            //                 .leaf => |other_tok| if (!tok.eql(other_tok)) return false,
            //                 .nonleaf => return false,
            //             },
            //             .nonleaf => switch (other_node.data) {
            //                 .leaf => return false,
            //                 .nonleaf => continue,
            //             },
            //         }
            //     }
            //     return true;
            // }

            // /// Allocator will be used to create the hashmap for storing matches
            // pub fn match(self: *Node, allocator: std.mem.Allocator, pattern: *Node) !?MatchHashMap {
            //     var matches = MatchHashMap.init(allocator);
            //     errdefer matches.deinit();

            //     var it = self.iterDepthFirst();
            //     var pattern_it = pattern.iterDepthFirst();

            //     while (it.hasNext() and pattern_it.hasNext()) {
            //         const node = it.nextUnchecked();
            //         const pattern_node = pattern_it.nextUnchecked();

            //         const is_match = switch (node.data) {
            //             .leaf => |tok| switch (pattern_node.data) {
            //                 .leaf => |pattern_tok| tok.eql(pattern_tok),
            //                 .nonleaf => false,
            //             },
            //             .nonleaf => |children| switch (pattern_node.data) {
            //                 .leaf => false,
            //                 .nonleaf => |pattern_children| ret: {
            //                     if (pattern_children.items.len == 1) {
            //                         it.skipChildren();
            //                         pattern_it.skipChildren();

            //                         const child_tok = pattern_children.items[0].data.leaf;
            //                         const proposition_str = child_tok.getString();
            //                         if (matches.get(proposition_str)) |existing_match| {
            //                             if (!existing_match.eql(node)) {
            //                                 _ = matches.remove(proposition_str);
            //                                 //allocator.free(pair.key);
            //                             }
            //                         } else {
            //                             try matches.put(proposition_str, node);
            //                         }
            //                         continue;
            //                     } else if (children.items.len == pattern_children.items.len) {
            //                         continue;
            //                     } else {
            //                         break :ret false;
            //                     }
            //                 },
            //             },
            //         }; // outer switch

            //         if (!is_match) {
            //             matches.deinit();
            //             return null;
            //         }
            //     } // while
            //     if (matches.count() > 0) {
            //         return matches;
            //     } else {
            //         matches.deinit();
            //         return null;
            //     }
            // }
        };

        pub fn init(allocator: std.mem.Allocator, root: *Node) !Self {
            return Self{.allocator = allocator, .root = root};
        }

        // pub fn deinit(self: Self) void {
        //     var it = self.iterPostOrder();
        //     while (it.next()) |node| {
        //         const n = node;
        //         switch (n.data) {
        //             .leaf => |tok| switch (tok) {
        //                 .Proposition => |prop| self.allocator.free(prop.string),
        //                 else => {},
        //             },
        //             .nonleaf => |children| children.deinit(),
        //         }
        //     }
        //     self.allocator.destroy(self.root);
        // }

        // pub fn copy(self: Self) !OldParseTree {
        //     return OldParseTree{ .root = try self.root.copy(self.allocator), .allocator = self.allocator };
        // }

        // /// Compare two ParseTree instances. They are equal if both have the same
        // /// tree structure (determined by non-terminal nodes, which are the only
        // /// nodes with children) and if each pair of corresponding terminal nodes
        // /// have equivalent tokens (see lex.Token.equals).
        // pub fn eql(self: Self, other: Self) bool {
        //     return self.root.eql(other.root);
        // }

        // pub fn iterDepthFirst(self: Self) ParseTreeDepthFirstIterator {
        //     return self.root.iterDepthFirst();
        // }

        // pub fn iterPostOrder(self: Self) ParseTreePostOrderIterator {
        //     return self.root.iterPostOrder();
        // }

        // pub fn toString(self: Self, allocator: std.mem.Allocator) ![]u8 {
        //     var string_buf = std.ArrayList(u8).init(allocator);
        //     errdefer string_buf.deinit();

        //     var it = self.iterDepthFirst();
        //     while (it.next()) |node| {
        //         switch (node.data) {
        //             .leaf => |*tok| {
        //                 switch (tok.*) {
        //                     .LParen => try string_buf.appendSlice(tok.getString()),
        //                     .RParen => {
        //                         _ = string_buf.pop();
        //                         try string_buf.appendSlice(tok.getString());
        //                         try string_buf.append(' ');
        //                     },
        //                     .Operator => |op| {
        //                         try string_buf.appendSlice(tok.getString());
        //                         switch (op) {
        //                             .And, .Or, .Cond, .Bicond => try string_buf.append(' '),
        //                             .Not => {},
        //                         }
        //                     },
        //                     .Proposition, .True, .False => {
        //                         try string_buf.appendSlice(tok.getString());
        //                         try string_buf.append(' ');
        //                     },
        //                 }
        //             },
        //             .nonleaf => {},
        //         }
        //     }
        //     _ = string_buf.pop(); // remove trailing space
        //     return string_buf.toOwnedSlice();
        // }   
    };
}

// TODO: fn to build trees more easily for testing larger expressions

// test "ParseTree: ~p" {
//     var tree = try OldParseTree.init(std.testing.allocator, "~p");
//     defer tree.deinit();
    
//     var t1 = lex.Token{.Operator = lex.WffOperator.Not};
//     var t2 = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") }};
//     defer std.testing.allocator.free(t2.Proposition.string);

//     var it = tree.iterDepthFirst();
//     _ = it.next();
//     try std.testing.expect(t1.eql(it.next().?.data.Terminal));
//     _ = it.next();
//     try std.testing.expect(t2.eql(it.next().?.data.Terminal));
//     try std.testing.expect(it.next() == null);
// }

// test "ParseTree.toString: (p v q)" {
//     var tree = try OldParseTree.init(std.testing.allocator, "(p v q)");
//     defer tree.deinit();

//     const string = try tree.toString(std.testing.allocator);
//     defer std.testing.allocator.free(string);

//     try std.testing.expectEqualStrings("(p v q)", string);
// }

// test "ParseTree.toString: ~((a ^ b) => (c ^ ~d))" {
//     var tree = try OldParseTree.init(std.testing.allocator, "~((a ^ b) => (c ^ ~d))");
//     defer tree.deinit();

//     const string = try tree.toString(std.testing.allocator);
//     defer std.testing.allocator.free(string);

//     try std.testing.expectEqualStrings("~((a ^ b) => (c ^ ~d))", string);
// }

// test "ParseTree.copy: (p v q)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "(p v q)");
//     defer tree.deinit();
//     var copy = try tree.copy();
//     defer copy.deinit();

//     try std.testing.expect(tree.eql(copy));
// }

// test "ParseTree.copy: ((a ^ b) v (c ^ d))" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "((a ^ b) v (c ^ d))");
//     defer tree.deinit();
//     var copy = try tree.copy();
//     defer copy.deinit();

//     try std.testing.expect(tree.eql(copy));
// }

// test "ParseTree.copy: p" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "p");
//     defer tree.deinit();
//     var copy = try tree.copy();
//     defer copy.deinit();

//     try std.testing.expect(tree.eql(copy));
// }

// test "ParseTree.Node.copyAbove" {
//     var allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "((a ^ b) v (c ^ d))");
//     defer tree.deinit();

//     var bottom_right = &tree.root.data.Nonterminal.items[3].data.Nonterminal.items[1];
//     const bottom_right_copy = try bottom_right.copy(allocator);
//     defer allocator.destroy(bottom_right_copy);
//     const root_copy = try bottom_right.copyAbove(allocator, bottom_right_copy.*);
//     var tree_copy = OldParseTree{.allocator = allocator, .root = root_copy};
//     defer tree_copy.deinit();

//     // var s1 = try tree.toString(allocator);
//     // defer allocator.free(s1);
//     // var s2 = try tree_copy.toString(allocator);
//     // defer allocator.free(s2);

//     // std.debug.print("\nExpected: {s}\nActual: {s}\n", .{s1, s2});

//     try std.testing.expect(tree.eql(tree_copy));
// }

// test "ParseTree.Node.match: (p v q) with pattern (p v q)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "(p v q)");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p v q)");
//     defer pattern.deinit();

//     var matches = (try tree.root.match(allocator, pattern.root)).?;
//     defer matches.deinit();

//     try std.testing.expect(matches.count() == 2);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], matches.get("p").?);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[3], matches.get("q").?);
// }

// test "ParseTree.Node.match: ((a ^ b) v (c ^ d)) with pattern (p v q)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "((a ^ b) v (c ^ d))");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p v q)");
//     defer pattern.deinit();

//     var matches = (try tree.root.match(allocator, pattern.root)).?;
//     defer matches.deinit();

//     try std.testing.expect(matches.count() == 2);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], matches.get("p").?);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[3], matches.get("q").?);
// }

// test "ParseTree.Node.match: (p v p) with pattern (p v p)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "(p v p)");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p v p)");
//     defer pattern.deinit();

//     var matches = (try tree.root.match(allocator, pattern.root)).?;
//     defer matches.deinit();

//     try std.testing.expect(matches.count() == 1);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], matches.get("p").?);
// }

// test "ParseTree.Node.match: (p v q) with pattern (p v p)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "(p v q)");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p v p)");
//     defer pattern.deinit();

//     try std.testing.expect(try tree.root.match(allocator, pattern.root) == null);
// }

// test "ParseTree.Node.match: ((a ^ b) => (a ^ b)) with pattern (p => p)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "((a ^ b) => (a ^ b))");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p => p)");
//     defer pattern.deinit();

//     var matches = (try tree.root.match(allocator, pattern.root)).?;
//     defer matches.deinit();

//     try std.testing.expect(matches.count() == 1);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], matches.get("p").?);
// }

// test "ParseTree.Node.match: ((a ^ b) => (a ^ a)) with pattern (p => p)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "((a ^ b) => (a ^ a))");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p => p)");
//     defer pattern.deinit();

//     try std.testing.expect(try tree.root.match(allocator, pattern.root) == null);
// }

// test "ParseTree.Node.match: (x <=> x) with pattern (p <=> q)" {
//     const allocator = std.testing.allocator;
//     var tree = try OldParseTree.init(allocator, "(x <=> x)");
//     defer tree.deinit();
//     var pattern = try OldParseTree.init(allocator, "(p <=> q)");
//     defer pattern.deinit();

//     var matches = (try tree.root.match(allocator, pattern.root)).?;
//     defer matches.deinit();

//     try std.testing.expect(matches.count() == 2);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[1], matches.get("p").?);
//     try std.testing.expectEqual(&tree.root.data.Nonterminal.items[3], matches.get("q").?);
// }

// TODO: Define error set of tokenize_func
// T must have .eql, .deinit, ...
fn NewParser(
    comptime Variable: type, 
    comptime Terminal: type,  
    comptime Token: type,
    comptime tokenize: fn (std.mem.Allocator, []const u8) anyerror!std.ArrayList(Token),
    comptime terminalFromToken: fn (Token) ?Terminal,
) type {
    return struct {
        const Self = @This();
        const Grammar = slr.Grammar(Variable, Terminal);
        const ParseTable = slr.ParseTable(Variable, Terminal);
        const ParseTreeType = NewParseTree(Token);

        const StackItem = struct {
            state: ParseTable.StateIdx,
            node: ParseTreeType.Node,
        };

        table: ParseTable,

        pub fn init(allocator: std.mem.Allocator, grammar: Grammar) !Self {
            return Self {
                .table = try ParseTable.init(allocator, grammar)
            };
        }

        /// Note: Does NOT free the memory associated with the grammar it was
        /// initialized with.
        pub fn deinit(self: Self) void {
            self.table.deinit();
        }

        /// Performs a reduction (modifies the stack) and returns true
        /// Returns false if no reduction is possible.
        fn reduce(
            self: Self, 
            allocator: std.mem.Allocator, 
            state_stack: *std.ArrayList(ParseTable.StateIdx), 
            node_stack: *std.ArrayList(ParseTreeType.Node),
            terminal: Terminal
        ) !bool {
            const top_state = state_stack.getLast();
            const rule_idx = switch(self.table.lookupTerminal(top_state, terminal) orelse return ParseError.UnexpectedToken) {
                .reduce => |idx| idx,
                else => return false,
            };
            const rule = self.table.grammar.getRule(rule_idx);
            
            var pushed_to_state_stack = false;
            var pushed_to_node_stack = false;
            var children = try allocator.alloc(ParseTreeType.Node, rule.rhs.len);
            errdefer {
                allocator.free(children);
                if (pushed_to_state_stack) {
                    _ = state_stack.pop();
                }
                if (pushed_to_node_stack) {
                    _ = node_stack.pop();
                }
            }

            // Insert nodes into children from right to left to maintain the 
            // same order as the stack.
            var i: usize = children.len;
            while (i > 0) {
                i -= 1;
                const child = node_stack.pop();
                _ = state_stack.pop();
                children[i] = child;
                switch(child.kind) {
                    .leaf => {}, 
                    .nonleaf => |grandchildren| for (grandchildren) |*grandchild| {
                        grandchild.parent = &children[i];
                    },
                }
            }

            const new_parent = ParseTreeType.Node{ .kind = .{.nonleaf = children}};

            const new_top_state = state_stack.getLast();
            const reduced_state = try switch(self.table.lookupSymbol(new_top_state, rule.lhs)) {
                .state => |state_num| state_num,
                else => ParseError.InvalidSyntax,
            };

            try state_stack.append(reduced_state);
            pushed_to_state_stack = true;
            try node_stack.append(new_parent);
            pushed_to_node_stack = true;

            return true;
        }

        // TODO: Change tokens to slice
        pub fn parse(self: Self, allocator: std.mem.Allocator, string: []const u8) !ParseTreeType {
            var tokens = try tokenize(allocator, string);
            defer tokens.deinit();
            errdefer {
                for (tokens.items) |tok| {
                    // tok.deinit();
                    switch(tok) {
                        .Proposition => |s| allocator.free(s.string),
                        else => {},
                    }
                }
            }
            
            // Initialize parsing stacks
            var state_stack = std.ArrayList(ParseTable.StateIdx).init(allocator);
            defer state_stack.deinit();
            try state_stack.append(self.table.getStartState());

            var node_stack = std.ArrayList(ParseTreeType.Node).init(allocator);
            defer node_stack.deinit();

            debug.print("\n", .{});
            debugPrintStacks(state_stack, node_stack);
            // Start parsing tokens
            for (tokens.items) |token| {
                // const next_token = if (i + 1 == tokens.items.len) 
                //         self.table.grammar.terminals[self.table.grammar.getEndTerminal().terminal]
                //     else 
                //         terminalFromToken(tokens.items[i + 1]) orelse return ParseError.UnexpectedToken;
                var current_state = state_stack.getLast();
                const terminal = terminalFromToken(token) orelse return ParseError.UnexpectedToken;
                
                while (try self.reduce(allocator, &state_stack, &node_stack, terminal)) {
                    debugPrintStacks(state_stack, node_stack);
                }

                // Current state may have changed during reduction.
                current_state = state_stack.getLast();

                const new_state: ParseTable.StateIdx = try switch(self.table.lookupTerminal(current_state, terminal) orelse return ParseError.UnexpectedToken) {
                    .state => |state_num| state_num,
                    else => ParseError.InvalidSyntax,
                };
                try state_stack.append(new_state);

                const new_node = ParseTreeType.Node{.kind = .{.leaf = token}};
                try node_stack.append(new_node);

                debugPrintStacks(state_stack, node_stack);   
            }

            // Perform any reductions possible once there are no more tokens.
            const end_terminal = self.table.grammar.terminals[self.table.grammar.getEndSymbol().terminal];
            while (try self.reduce(allocator, &state_stack, &node_stack, end_terminal)) {
                debugPrintStacks(state_stack, node_stack);
            }

            // Verify that the final resulting state is accepting.
            switch(self.table.lookupTerminal(state_stack.getLast(), end_terminal).?) {
                .accept => {},
                else => return ParseError.InvalidSyntax,
            }

            const root = try allocator.create(ParseTreeType.Node);
            errdefer allocator.destroy(root);

            root.* = node_stack.pop();
            for (root.kind.nonleaf) |*child| {
                child.parent = root;
            }
            return ParseTreeType{ .allocator = allocator, .root = root};
        }

        fn debugPrintStacks(state_stack: std.ArrayList(ParseTable.StateIdx), node_stack: std.ArrayList(ParseTreeType.Node)) void {
            debug.print("S{d}", .{state_stack.items[0]});
            for (state_stack.items[1..], node_stack.items) |state, node| {
                debug.print("<{s}> S{d}", .{node.asString(), state});
            }
            debug.print("\n", .{});
        }
    };
}

const TestTerminal = enum {
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

const TestVariable = struct {
    const Self = @This();

    name: []const u8,

    fn fromString(string: []const u8) Self {
        return TestVariable{ .name = string };
    }

    fn getString(self: Self) []const u8 {
        return self.name;
    }

    pub fn eql(self: Self, other: Self) bool {
        return std.mem.eql(u8, self.name, other.name);
    }
};

fn oldTokenToTerminal(token: lex.Token) ?TestTerminal {
    return switch(token) {
        .LParen => TestTerminal.LParen,
        .RParen => TestTerminal.RParen,
        .True, .False, .Proposition => TestTerminal.Proposition,
        .Operator => |op| switch(op) {
            .Not => TestTerminal.Not,
            .And => TestTerminal.And,
            .Or => TestTerminal.Or,
            .Cond => TestTerminal.Cond,
            .Bicond => TestTerminal.Bicond,
        },
    };
}

test "Parser.parse: ''" {
    const V = TestVariable.fromString;
    const G = slr.Grammar(TestVariable, TestTerminal);
    const grammar = comptime ret: {
        break :ret G.initFromTuples(
            .{
                .{ V("S"), .{V("wff")} },
                .{ V("wff"), .{TestTerminal.Proposition} },
                .{ V("wff"), .{ TestTerminal.Not, V("wff") } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.And, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Or, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Cond, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Bicond, V("wff"), TestTerminal.RParen } },
            },
            V("S"),
            TestTerminal.End,
        );
    };
    defer grammar.deinit();

    const ParserType = NewParser(
        TestVariable, 
        TestTerminal, 
        lex.Token,
        lex.tokenize,
        oldTokenToTerminal,
    );
    const parser = try ParserType.init(
        std.testing.allocator, 
        grammar
    );
    defer parser.deinit();
    
    try std.testing.expectError(
        lex.LexError.NoTokensFound, 
        parser.parse(std.testing.allocator, "")
    );
}

test "Parser.parse: '~)q p v('" {
    const V = TestVariable.fromString;
    const G = slr.Grammar(TestVariable, TestTerminal);
    const grammar = comptime ret: {
        break :ret G.initFromTuples(
            .{
                .{ V("S"), .{V("wff")} },
                .{ V("wff"), .{TestTerminal.Proposition} },
                .{ V("wff"), .{ TestTerminal.Not, V("wff") } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.And, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Or, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Cond, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Bicond, V("wff"), TestTerminal.RParen } },
            },
            V("S"),
            TestTerminal.End,
        );
    };
    defer grammar.deinit();

    const ParserType = NewParser(
        TestVariable, 
        TestTerminal, 
        lex.Token,
        lex.tokenize,
        oldTokenToTerminal,
    );
    const parser = try ParserType.init(
        std.testing.allocator, 
        grammar
    );
    defer parser.deinit();

    try std.testing.expectError(
        ParseError.InvalidSyntax, 
        parser.parse(std.testing.allocator, "~)q p v (")
    );
}

test "Parser.parse: '(p v q)'" {
    const allocator = std.testing.allocator;
    
    const V = TestVariable.fromString;
    const G = slr.Grammar(TestVariable, TestTerminal);
    const grammar = comptime ret: {
        break :ret G.initFromTuples(
            .{
                .{ V("S"), .{V("wff")} },
                .{ V("wff"), .{TestTerminal.Proposition} },
                .{ V("wff"), .{ TestTerminal.Not, V("wff") } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.And, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Or, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Cond, V("wff"), TestTerminal.RParen } },
                .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Bicond, V("wff"), TestTerminal.RParen } },
            },
            V("S"),
            TestTerminal.End,
        );
    };
    defer grammar.deinit();

    const ParserType = NewParser(
        TestVariable, 
        TestTerminal, 
        lex.Token,
        lex.tokenize,
        oldTokenToTerminal,
    );
    const parser = try ParserType.init(
        allocator, 
        grammar
    );
    defer parser.deinit();

    const tree = try parser.parse(allocator, "(p v q)");
    //defer tree.deinit();

    // Build the expected tree manually... it's pretty messy
    const ParseTreeType = NewParseTree(lex.Token);

    var wff1: ParseTreeType.Node = undefined;
    var wff2: ParseTreeType.Node = undefined;
    var root: ParseTreeType.Node = undefined;

    var children1 = try allocator.alloc(ParseTreeType.Node, 1);
    defer allocator.free(children1);
    var children2 = try allocator.alloc(ParseTreeType.Node, 1);
    defer allocator.free(children2);
    var root_children = try allocator.alloc(ParseTreeType.Node, 5);
    defer allocator.free(root_children);

    var t1 = ParseTreeType.Node{ .parent = &root,               .kind = .{ .leaf = lex.Token{ .LParen = {} } } };
    var t2 = ParseTreeType.Node{ .parent = &(root_children[1]), .kind = .{ .leaf = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "p") } } } };
    var t3 = ParseTreeType.Node{ .parent = &root,               .kind = .{ .leaf = lex.Token{ .Operator = lex.WffOperator.Or } } };
    var t4 = ParseTreeType.Node{ .parent = &(root_children[3]), .kind = .{ .leaf = lex.Token{ .Proposition = lex.PropositionVar{ .string = try std.testing.allocator.dupe(u8, "q") } } } };
    var t5 = ParseTreeType.Node{ .parent = &root,               .kind = .{ .leaf = lex.Token{ .RParen = {} } } };
    defer allocator.free(t2.kind.leaf.Proposition.string);
    defer allocator.free(t4.kind.leaf.Proposition.string);

    children1[0] = t2;
    children2[0] = t4;
    wff1 = ParseTreeType.Node{ .parent = &root, .kind = .{ .nonleaf = children1 } };
    wff2 = ParseTreeType.Node{ .parent = &root, .kind = .{ .nonleaf = children2 } };

    root_children[0] = t1;
    root_children[1] = wff1;
    root_children[2] = t3;
    root_children[3] = wff2;
    root_children[4] = t5;
    root = ParseTreeType.Node{ .kind = .{ .nonleaf = root_children } };

    // Hardcoded equality test of tree
    try std.testing.expect(t1.kind.leaf.eql(tree.root.kind.nonleaf[0].kind.leaf));
    try std.testing.expect(t2.kind.leaf.eql(tree.root.kind.nonleaf[1].kind.nonleaf[0].kind.leaf));
    try std.testing.expect(t3.kind.leaf.eql(tree.root.kind.nonleaf[2].kind.leaf));
    try std.testing.expect(t4.kind.leaf.eql(tree.root.kind.nonleaf[3].kind.nonleaf[0].kind.leaf));
    try std.testing.expect(t5.kind.leaf.eql(tree.root.kind.nonleaf[4].kind.leaf));

    // Function equality test of tree
    //try std.testing.expect(tree.root.eql(&root));
}

// test "Parser.parse: '(p v q)'" {
//     const allocator = std.testing.allocator;
    
//     const V = TestVariable.fromString;
//     const G = slr.Grammar(TestVariable, TestTerminal);
//     const grammar = G.initFromTuples(
//         .{
//             .{ V("S"), .{V("wff")} },
//             .{ V("wff"), .{TestTerminal.Proposition} },
//             .{ V("wff"), .{ TestTerminal.Not, V("wff") } },
//             .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.And, V("wff"), TestTerminal.RParen } },
//             .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Or, V("wff"), TestTerminal.RParen } },
//             .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Cond, V("wff"), TestTerminal.RParen } },
//             .{ V("wff"), .{ TestTerminal.LParen, V("wff"), TestTerminal.Bicond, V("wff"), TestTerminal.RParen } },
//         },
//         V("S"),
//         TestTerminal.End,
//     );
//     defer grammar.deinit();

//     const ParserType = NewParser(
//         TestVariable, 
//         TestTerminal, 
//         lex.Token,
//         lex.tokenize,
//         oldTokenToTerminal,
//     );
//     const parser = try ParserType.init(
//         allocator, 
//         grammar
//     );
//     defer parser.deinit();

//     const tree = try parser.parse(allocator, "(p v q)");
//     defer tree.deinit();

//     // Build the expected tree manually... it's pretty messy
//     var wff1: NewParseTree.Node = undefined;
//     var wff2: NewParseTree.Node = undefined;
//     var root: NewParseTree.Node = undefined;

//     var children1 = try allocator.alloc(NewParseTree.Node, 1);
//     defer allocator.free(children1);
//     var children2 = try allocator.alloc(NewParseTree.Node, 1);
//     defer allocator.free(children2);
//     var root_children = try allocator.alloc(NewParseTree.Node, 5);
//     defer allocator.free(root_children);

//     var t1 = NewParseTree.Node{ .parent = &root,               .kind = .{ .leaf = TestToken{ .LParen = {} } } };
//     var t2 = NewParseTree.Node{ .parent = &(root_children[1]), .kind = .{ .leaf = TestToken{ .Proposition = try std.testing.allocator.dupe(u8, "p") } } };
//     var t3 = NewParseTree.Node{ .parent = &root,               .kind = .{ .leaf = TestToken{ .Or = {} } } };
//     var t4 = NewParseTree.Node{ .parent = &(root_children[3]), .kind = .{ .leaf = TestToken{ .Proposition = try std.testing.allocator.dupe(u8, "q") } } };
//     var t5 = NewParseTree.Node{ .parent = &root,               .kind = .{ .laef = TestToken{ .RParen = {} } } };
//     defer std.testing.allocator.free(t2.data.Terminal.Proposition.string);
//     defer std.testing.allocator.free(t4.data.Terminal.Proposition.string);

//     try children1.append(t2);
//     try children2.append(t4);
//     wff1 = OldParseTree.Node{ .parent = &root, .kind = .{ .nonleaf = children1 } };
//     wff2 = OldParseTree.Node{ .parent = &root, .kind = .{ .nonleaf = children2 } };

//     root_children.items[0] = t1;
//     root_children.items[1] = wff1;
//     root_children.items[2] = t3;
//     root_children.items[3] = wff2;
//     root_children.items[4] = t5;
//     root = OldParseTree.Node{ .kind = .{ .nonleaf = root_children } };

//     // Hardcoded equality test of tree
//     try std.testing.expect(t1.kind.leaf.eql(tree.root.kind.nonleaf.items[0].kind.leaf));
//     try std.testing.expect(t2.kind.leaf.eql(tree.root.kind.nonleaf.items[1].kind.nonleaf[0].kind.leaf));
//     try std.testing.expect(t3.kind.leaf.eql(tree.root.kind.nonleaf.items[2].kind.leaf));
//     try std.testing.expect(t4.kind.leaf.eql(tree.root.kind.nonleaf.items[3].kind.nonleaf[0].kind.leaf));
//     try std.testing.expect(t5.kind.leaf.eql(tree.root.kind.nonleaf.items[4].kind.laf));

//     // Function equality test of tree
//     try std.testing.expect(tree.root.eql(&root));
// }


/// Shift-Reduce parser for parsing WFF's.
/// Based on these grammar rules:
/// R1: S   -> wff
/// R2: wff -> Proposition
/// R3: wff -> Not    wff
/// R4: wff -> LParen wff And    wff RParen
/// R5: wff -> LParen wff Or     wff RParen
/// R6: wff -> LParen wff Cond   wff RParen
/// R7: wff -> LParen wff Bicond wff RParen
const OldParser = struct {
    const Self = @This();

    const Rule = enum {
        // R1: S -> wff  (we don't need this rule unless we want more than just wffs)
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
        True,
        False,
    };

    const State = enum {
        S1,
        S2,
        S3,
        S4,
        S5,
        S6,
        S7,
        S8,
        S9,
        S10,
        S11,
        S12,
        S13,
        S14,
        S15,
        S16,
        S17,
        S18,
        S19,
    };

    const StackItem = struct {
        state: State,
        node: OldParseTree.Node,
    };

    stack: std.ArrayList(StackItem),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .stack = std.ArrayList(StackItem).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    fn shift_terminal(state: State, symbol: StackSymbol) !State {
        return switch (state) {
            .S1, .S3, .S5, .S7, .S8, .S9, .S10 => switch (symbol) {
                .Proposition, .True, .False => .S4,
                .Not => .S3,
                .LParen => .S5,
                else => ParseError.InvalidSyntax,
            },
            .S6 => switch (symbol) {
                .And => .S7,
                .Or => .S8,
                .Cond => .S9,
                .Bicond => .S10,
                else => ParseError.InvalidSyntax,
            },
            .S11 => switch (symbol) {
                .RParen => .S12,
                else => ParseError.InvalidSyntax,
            },
            .S13 => switch (symbol) {
                .RParen => .S14,
                else => ParseError.InvalidSyntax,
            },
            .S15 => switch (symbol) {
                .RParen => .S16,
                else => ParseError.InvalidSyntax,
            },
            .S17 => switch (symbol) {
                .RParen => .S18,
                else => ParseError.InvalidSyntax,
            },
            else => ParseError.InvalidSyntax,
        };
    }

    fn shift_nonterminal(state: State, symbol: StackSymbol) !State {
        return switch (symbol) {
            .Wff => switch (state) {
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
        const rule = switch (state) {
            .S4 => Rule.R2,
            .S12 => Rule.R4,
            .S14 => Rule.R5,
            .S16 => Rule.R6,
            .S18 => Rule.R7,
            .S19 => Rule.R3,
            else => return null,
        };

        const child_count: usize = switch (rule) {
            .R2 => 1,
            .R3 => 2,
            .R4, .R5, .R6, .R7 => 5,
        };

        var children = try std.ArrayList(OldParseTree.Node).initCapacity(self.allocator, child_count);
        children.items.len = child_count; // manually set len so we can insert in correct order
        var i: usize = 1;
        while (i <= child_count) : (i += 1) {
            const child = self.stack.pop().node;
            children.items[children.items.len - i] = child;
            switch (child.data) {
                .Nonterminal => |grandchildren| {
                    for (grandchildren.items) |*grandchild| {
                        // Insert back to front to maintain order
                        grandchild.parent = &(children.items[children.items.len - i]);
                    }
                },
                .Terminal => {},
            }
        }

        const new_parent = OldParseTree.Node{ .data = OldParseTree.Data{ .Nonterminal = children } };

        const new_top_state = self.stack.items[self.stack.items.len - 1].state;
        const reduced_state = try shift_nonterminal(new_top_state, StackSymbol.Wff);

        try self.stack.append(StackItem{ .state = reduced_state, .node = new_parent });
        return reduced_state;
    }

    fn parse(self: *Self, wff_string: []const u8) !*OldParseTree.Node {
        // Tokenize given string
        var tokens = try lex.tokenize(self.allocator, wff_string);
        defer {
            tokens.deinit();
        }
        errdefer {
            for (tokens.items) |tok| switch (tok) {
                .Proposition => |prop| self.allocator.free(prop.string),
                else => {},
            };
        }

        // Initialize parsing stack
        try self.stack.append(StackItem{ .state = .S1, .node = undefined }); // Initial state is S1

        // Parse the tokens
        for (tokens.items) |token| {
            const top_state = self.stack.items[self.stack.items.len - 1].state;

            const stack_symbol = switch (token) {
                .LParen => StackSymbol.LParen,
                .RParen => StackSymbol.RParen,
                .Operator => |op| switch (op) {
                    .And => StackSymbol.And,
                    .Or => StackSymbol.Or,
                    .Not => StackSymbol.Not,
                    .Cond => StackSymbol.Cond,
                    .Bicond => StackSymbol.Bicond,
                },
                .Proposition => StackSymbol.Proposition,
                .True => StackSymbol.True,
                .False => StackSymbol.False,
            };

            var new_state = try shift_terminal(top_state, stack_symbol);
            const new_node = OldParseTree.Node{ .data = OldParseTree.Data{ .Terminal = token } };
            try self.stack.append(StackItem{ .state = new_state, .node = new_node });

            while (try self.reduce(new_state)) |result_state| {
                new_state = result_state;
            }
        }
        const root = try self.allocator.create(OldParseTree.Node);
        root.* = self.stack.pop().node;
        for (root.data.Nonterminal.items) |*child| {
            child.parent = root;
        }
        return root;
    }
};
