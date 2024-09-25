const std = @import("std");

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};

pub const WffTree = struct {
    const Self = @This();

    pub const PreOrderIterator = struct {
        start: *Node,
        current_node: ?*Node,
        skipNext: bool = false,
        isExhausted: bool = false,

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
                switch (parent.kind) {
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
            return null; // TODO: I think this is unreachable
        }

        /// From the given starting node, get the deepest node always branching
        /// right.
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
            const next_node = self.peek();

            if (self.skipNext) {
                self.skipNext = false;
            }

            if (next_node == null) {
                self.isExhausted = true;
                return null;
            } else {
                self.current_node = next_node;
                return self.current_node;
            }
        }

        pub fn peek(self: PreOrderIterator) ?*Node {
            if (self.isExhausted) {
                return null;
            }
            const next_node: ?*Node = ret: {
                if (self.skipNext) {
                    break :ret self.current_node;
                } else {
                    if (self.current_node == null) {
                        break :ret self.start;
                    } else {
                        break :ret switch (self.current_node.?.kind) {
                            .unary_operator => |op| op.arg,
                            .binary_operator => |op| op.arg1,
                            .proposition_variable, .logical_constant => self.backtrack_next(self.current_node.?),
                        };
                    }
                }
            };

            return next_node;
        }

        pub fn hasNext(self: PreOrderIterator) bool {
            return self.peek() != null;
        }

        pub fn previous(self: *PreOrderIterator) ?*Node {
            if (self.isExhausted) {
                self.current_node = get_deepest_right(self.start);
                self.isExhausted = false;
            } else if (self.current_node) |node| {
                if (node == self.start) {
                    self.current_node = null;
                } else {
                    const parent = node.parent.?;
                    self.current_node = switch (parent.kind) {
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
                }
            }
            return self.current_node;
        }

        pub fn skipChildren(self: *PreOrderIterator) void {
            self.skipNext = true;

            if (self.current_node) |node| {
                self.current_node = self.backtrack_next(node);
            } else {
                self.isExhausted = true;
            }
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

            // We know parent is not null or unary, else we already would have
            // returned.
            self.next_node = current_node.parent.?.kind.binary_operator.arg2;

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

    pub const InOrderIterator = struct {
        stack: std.ArrayList(*Node),

        pub fn init(allocator: std.mem.Allocator, root: *Node) !InOrderIterator {
            var it = InOrderIterator{
                .stack = std.ArrayList(*Node).init(allocator),
            };
            try it.stack.append(root);
            try it.descendLeft();
            return it;
        }

        pub fn deinit(self: InOrderIterator) void {
            self.stack.deinit();
        }

        fn descendLeft(self: *InOrderIterator) !void {
            while (true) {
                switch (self.stack.getLast().kind) {
                    .binary_operator => |op| try self.stack.append(op.arg1),
                    .unary_operator, .proposition_variable, .logical_constant => break,
                }
            }
        }

        pub fn next(self: *InOrderIterator) !?*Node {
            if (self.stack.items.len == 0) {
                return null;
            }

            const next_node = self.stack.pop();
            switch (next_node.kind) {
                .unary_operator => |op| {
                    try self.stack.append(op.arg);
                    try self.descendLeft();
                },
                .binary_operator => |op| {
                    try self.stack.append(op.arg2);
                    try self.descendLeft();
                },
                .proposition_variable, .logical_constant => {},
            }

            return next_node;
        }

        pub fn peek(self: InOrderIterator) ?*Node {
            return self.stack.getLastOrNull();
        }
    };

    pub const Node = struct {
        pub const UnaryOperator = enum {
            not,
        };

        pub const BinaryOperator = enum {
            and_,
            or_,
            cond,
            bicond,
        };

        pub const LogicalConstant = enum {
            true,
            false,
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

            fn getString(self: @This()) []const u8 {
                return switch (self) {
                    .unary_operator => |op| switch (op.operator) {
                        .not => "~",
                    },
                    .binary_operator => |op| switch (op.operator) {
                        .and_ => "^",
                        .or_ => "v",
                        .cond => "=>",
                        .bicond => "<=>",
                    },
                    .proposition_variable => |s| s,
                    .logical_constant => |constant| switch (constant) {
                        .true => "true",
                        .false => "false",
                    },
                };
            }
        },

        pub fn initKindUndefined(allocator: std.mem.Allocator, parent: ?*Node) !*Node {
            var node = try allocator.create(Node);
            errdefer allocator.destroy(node);

            node.parent = parent;
            return node;
        }

        /// Free the memory for ONLY this node (i.e. does not free memory for
        /// child nodes) using the supplied allocator which MUST be the same
        /// allocator used to create the node.
        pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
            switch (self.kind) {
                .proposition_variable => |s| allocator.free(s),
                .unary_operator, .binary_operator, .logical_constant => {},
            }
            allocator.destroy(self);
        }

        pub fn iterPreOrder(self: *Node) PreOrderIterator {
            return PreOrderIterator{ .start = self, .current_node = null };
        }

        pub fn iterPostOrder(self: *Node) PostOrderIterator {
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

        pub fn eql(self: *Node, other: *Node) bool {
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

        pub fn copy(self: *Node, allocator: std.mem.Allocator) !*Node {
            var copy_root = try Node.initKindUndefined(allocator, null);
            var copy_it = copy_root.iterPreOrder();
            errdefer {
                var node = copy_it.current_node.?;
                while (copy_it.previous()) |previous| {
                    node.deinit(allocator);
                    node = previous;
                }
                node.deinit(allocator);
            }

            var it = self.iterPreOrder();

            while (it.next()) |node| {
                var copy_node = copy_it.next().?;

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
            }
            return copy_root;
        }

        // NOTE: The string keys in the returned hashmap are owned by the
        // pattern nodes. Make sure the pattern is not deallocated before the
        // returned hash map is done being used.
        pub fn match(self: *Node, allocator: std.mem.Allocator, pattern: *Node) !?Wff.MatchHashMap {
            var map = Wff.MatchHashMap.init(allocator);
            errdefer map.deinit();

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
                    .proposition_variable => |variable| ret: {
                        it.skipChildren();

                        // In a case where the same pattern variable is used
                        // more than once (e.g. (p ^ p)) we must check that
                        // both subwffs matching p are equal, otherwise it's
                        // not a match.
                        if (map.get(variable)) |existing_match| {
                            if (!existing_match.eql(node)) {
                                // _ = map.remove(variable);
                                break :ret false;
                            }
                        } else {
                            try map.put(variable, node);
                        }

                        continue;
                    },
                };

                if (!is_match) {
                    map.deinit();
                    return null;
                }
            }
            return map;
            // if (map.count() > 0) {
            //     return map;
            // } else {
            //     map.deinit();
            //     return null;
            // }
        }

        /// When called on a nonleaf node, make a copy of the entire tree but
        /// with this node (subtree) replaced with a given other node which can
        /// be the root of a new subtree.
        /// When called on the root node of a tree, returns the given
        /// replacement node.
        pub fn copyAbove(self: *Node, allocator: std.mem.Allocator, replacement_root: *Node) !*Node {
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
                    // TODO: We shouldn't call deinit on first node since it might be undefined
                    var node = copy_it.current_node.?;
                    while (copy_it.previous()) |previous| {
                        if (node == stop_node) {
                            break;
                        }
                        node.deinit(allocator);
                        node = previous;
                    }
                    node.deinit(allocator);

                    copy_it.current_node = replacement_root;
                    _ = copy_it.previous();
                }
                var node = copy_it.next().?;
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
                var copy_node = copy_it.next().?;
                if (node == self) {
                    it.skipChildren();
                    // Do this before we deallocate copy_node.
                    copy_it.skipChildren();

                    const copy_parent = copy_node.parent.?;
                    // Have to use destroy instead of calling deinit
                    // since node.kind is undefined.
                    allocator.destroy(copy_node);

                    switch (node.parent.?.kind) {
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

                    first_node_after_replacement = copy_it.current_node;
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
                }
            }
            return copy_root;
        }

        fn countBinaryAbove(self: *Node) usize {
            var count: usize = 0;
            var node = self;
            while (node.parent) |parent| {
                switch (parent.kind) {
                    .binary_operator => count += 1,
                    else => {},
                }
                node = parent;
            }
            return count;
        }

        fn isLeftOperand(self: *Node) bool {
            var operand = self;
            while (operand.parent) |parent| {
                switch (parent.kind) {
                    .unary_operator => operand = parent,
                    .binary_operator => |op| return operand == op.arg1,
                    .proposition_variable, .logical_constant => unreachable,
                }
            }
            return false;
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

    pub fn iterInOrder(self: Self, allocator: std.mem.Allocator) !InOrderIterator {
        return InOrderIterator.init(allocator, self.root);
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

    // TODO: Calling countBinaryAbove on every terminal is inefficient. It would
    // be better to make one pass through the tree and note the number of binary
    // nodes above each node.
    pub fn makeString(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        var string = std.ArrayList(u8).init(allocator);
        var term = std.ArrayList(u8).init(allocator);

        var it = try self.iterInOrder(allocator);
        defer it.deinit();

        // Special case if the wff is just a propositional variable / logical
        // constant
        // if (it.peek().?.parent == null) {
        //     try string.appendSlice(it.peek().?.kind.getString());
        //     return try string.toOwnedSlice();
        // }

        var open_brackets: usize = 0;
        while (try it.next()) |node| {
            switch (node.kind) {
                .proposition_variable, .logical_constant => {
                    if (node.isLeftOperand()) {
                        const new_open_brackets = node.countBinaryAbove();
                        for (0..(new_open_brackets - open_brackets)) |_| {
                            try string.append('(');
                        }
                        open_brackets = new_open_brackets;

                        try string.appendSlice(node.kind.getString());
                    } else {
                        try string.appendSlice(node.kind.getString());

                        const next_open_brackets = if (it.peek()) |next| next.countBinaryAbove() + 1 else 0;
                        for (0..(open_brackets - next_open_brackets)) |_| {
                            try string.append(')');
                        }
                        open_brackets = next_open_brackets;
                    }
                    term.clearRetainingCapacity();
                },
                .binary_operator => {
                    try string.append(' ');
                    try string.appendSlice(node.kind.getString());
                    try string.append(' ');
                },
                .unary_operator => {
                    const new_open_brackets = node.countBinaryAbove();
                    for (0..(new_open_brackets - open_brackets)) |_| {
                        try string.append('(');
                    }
                    open_brackets = new_open_brackets;

                    try string.appendSlice(node.kind.getString());
                },
            }
        }
        return try string.toOwnedSlice();
    }
};

test "WffTree.PreOrderIterator: ((a v b) ^ ~c)" {
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
    try std.testing.expectEqual(null, it.next());

    try std.testing.expectEqual(&c, it.previous().?);
    try std.testing.expectEqual(&right_branch, it.previous().?);
    try std.testing.expectEqual(&b, it.previous().?);
    try std.testing.expectEqual(&a, it.previous().?);
    try std.testing.expectEqual(&left_branch, it.previous().?);
    try std.testing.expectEqual(&root, it.previous().?);
    try std.testing.expectEqual(null, it.previous());
    try std.testing.expectEqual(null, it.previous());

    // Call skipChildren before starting iteration. Should skip entire tree.
    it = root.iterPreOrder();
    it.skipChildren();
    try std.testing.expectEqual(null, it.next());
    try std.testing.expectEqual(null, it.next());

    // Call skipChildren on root. Should skip entire tree.
    it = root.iterPreOrder();
    _ = it.next();
    it.skipChildren();
    try std.testing.expectEqual(null, it.next());
    try std.testing.expectEqual(null, it.next());

    // Call skipChildren on left branch. Should skip only (a v b).
    it = root.iterPreOrder();
    _ = it.next();
    _ = it.next();
    it.skipChildren();
    try std.testing.expectEqual(&right_branch, it.next());
    try std.testing.expectEqual(&c, it.next());
    try std.testing.expectEqual(null, it.next());
    try std.testing.expectEqual(null, it.next());
}

test "WffTree.PostOrderIterator: ((a v b) ^ ~c)" {
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

test "WffTree.InOrderIterator: ((a v b) ^ ~c)" {
    const allocator = std.testing.allocator;

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

    const tree = WffTree{
        .allocator = allocator,
        .root = &root,
    };

    var it = try tree.iterInOrder(allocator);
    defer it.deinit();

    try std.testing.expectEqual(&a, (try it.next()).?);
    try std.testing.expectEqual(&left_branch, (try it.next()).?);
    try std.testing.expectEqual(&b, (try it.next()).?);
    try std.testing.expectEqual(&root, (try it.next()).?);
    try std.testing.expectEqual(&right_branch, (try it.next()).?);
    try std.testing.expectEqual(&c, (try it.next()).?);
    try std.testing.expectEqual(null, try it.next());
}

test "WffTree.makeString: ((avb)^~   c)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "((avb)^~   c)");
    defer wff.deinit();

    try std.testing.expectEqualStrings("((a v b) ^ ~c)", wff.string);
}

test "WffTree.makeString: (a => (b => (a ^ b)))" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const string = "(a => (b => (a ^ b)))";

    const wff = try wff_parser.buildWff(allocator, string);
    defer wff.deinit();

    try std.testing.expectEqualStrings(string, wff.string);
}

test "WffTree.makeString: ~~((~~a => (b ^ (a v b))) <=> ~b)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const string = "~~((~~a => (b ^ (a v b))) <=> ~b)";

    const wff = try wff_parser.buildWff(allocator, string);
    defer wff.deinit();

    try std.testing.expectEqualStrings(string, wff.string);
}

test "WffTree.makeString: b" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const string = "b";

    const wff = try wff_parser.buildWff(allocator, string);
    defer wff.deinit();

    try std.testing.expectEqualStrings(string, wff.string);
}

test "WffTree.makeString: ~x" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const string = "~x";

    const wff = try wff_parser.buildWff(allocator, string);
    defer wff.deinit();

    try std.testing.expectEqualStrings(string, wff.string);
}

test "WffTree.copy: (p v q), ((a ^ b) v (c ^ d)), and p" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff1 = try wff_parser.buildWff(allocator, "(p v q)");
    defer wff1.deinit();
    var tree_copy1 = try wff1.tree.copy(allocator);
    defer tree_copy1.deinit();

    const wff2 = try wff_parser.buildWff(allocator, "((a ^ b) v (c ^ d))");
    defer wff2.deinit();
    var tree_copy2 = try wff2.tree.copy(allocator);
    defer tree_copy2.deinit();

    const wff3 = try wff_parser.buildWff(allocator, "p");
    defer wff3.deinit();
    var copy_tree3 = try wff3.tree.copy(allocator);
    defer copy_tree3.deinit();

    try std.testing.expect(wff1.tree.eql(tree_copy1));
    try std.testing.expect(wff2.tree.eql(tree_copy2));
    try std.testing.expect(wff3.tree.eql(copy_tree3));
}

test "WffTree.Node.match: (p v q) with pattern (p v q)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "(p v q)");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p v q)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expectEqual(2, matches.count());
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg2, matches.get("q").?);
}

test "WffTree.Node.match: ((a ^ b) v (c ^ d)) with pattern (p v q)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p v q)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expectEqual(2, matches.count());
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg2, matches.get("q").?);
}

test "WffTree.Node.match: (p v p) with pattern (p v p)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "(p v p)");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p v p)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expectEqual(1, matches.count());
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
}

test "WffTree.Node.match: (p v q) with pattern (p v p)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "(p v q)");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p v p)");
    defer pattern_wff.deinit();

    try std.testing.expectEqual(null, try wff.tree.root.match(allocator, pattern_wff.tree.root));
}

test "WffTree.Node.match: ((a ^ b) => (a ^ b)) with pattern (p => p)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "((a ^ b) => (a ^ b))");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p => p)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expect(matches.count() == 1);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
}

test "ParseTree.Node.match: ((a ^ b) => (a ^ a)) with pattern (p => p)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "((a ^ b) => (a ^ a))");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p => p)");
    defer pattern_wff.deinit();

    try std.testing.expect(try wff.tree.root.match(allocator, pattern_wff.tree.root) == null);
}

test "WffTree.Node.match: (x <=> x) with pattern (p <=> q)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const wff = try wff_parser.buildWff(allocator, "(x <=> x)");
    defer wff.deinit();
    const pattern_wff = try wff_parser.buildWff(allocator, "(p <=> q)");
    defer pattern_wff.deinit();

    var matches = (try wff.tree.root.match(allocator, pattern_wff.tree.root)).?;
    defer matches.deinit();

    try std.testing.expect(matches.count() == 2);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg1, matches.get("p").?);
    try std.testing.expectEqual(wff.tree.root.kind.binary_operator.arg2, matches.get("q").?);
}

pub const Wff = struct {
    const Self = @This();
    pub const MatchHashMap = std.StringHashMap(*WffTree.Node);

    pub const Match = struct {
        source_wff: *const Wff,
        source_subtree_root: *WffTree.Node,
        map: MatchHashMap,

        pub fn deinit(self: Match) void {
            var m = self.map;
            m.deinit();
        }

        /// Lookup a match in the hashmap and build a new Wff for it if it exists
        pub fn getBuildWff(self: Match, allocator: std.mem.Allocator, key: []const u8) !?Wff {
            const node = self.map.get(key) orelse return null;
            const tree = WffTree{ .allocator = allocator, .root = try node.copy(allocator) };
            return Self{
                .allocator = allocator,
                .tree = tree,
                .string = try tree.makeString(allocator),
            };
        }

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
                        if (self.map.get(variable)) |source_node| {
                            it.skipChildren();
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
                            switch (result_node.kind) {
                                .unary_operator => |op| op.arg.parent = result_node,
                                .binary_operator => |op| {
                                    op.arg1.parent = result_node;
                                    op.arg2.parent = result_node;
                                },
                                .proposition_variable, .logical_constant => {},
                            }
                        }
                    },
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
                .string = try substitution_tree.makeString(allocator),
            };
        }
    };

    allocator: std.mem.Allocator,
    tree: WffTree,
    string: []const u8,

    pub fn deinit(self: Self) void {
        self.tree.deinit();
        self.allocator.free(self.string);
    }

    pub fn copy(self: *Self, allocator: std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
            .tree = try self.tree.copy(allocator),
            .string = try allocator.dupe(u8, self.string),
        };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.tree.eql(other.tree);
    }

    pub fn match(self: *const Self, allocator: std.mem.Allocator, pattern: Self) !?Match {
        return Match{ .source_wff = self, .source_subtree_root = self.tree.root, .map = try self.tree.root.match(allocator, pattern.tree.root) orelse return null };
    }

    // TODO: Make this into method on tree or node and call it here instead
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
                try all_matches.append(Match{ .source_wff = self, .source_subtree_root = node, .map = m });
            }
        }
        if (all_matches.items.len > 0) {
            return all_matches;
        } else {
            all_matches.deinit();
            return null;
        }
    }

    pub fn isTrue(self: Self) bool {
        return std.mem.eql(u8, self.string, "T");
    }

    pub fn isFalse(self: Self) bool {
        return std.mem.eql(u8, self.string, "F");
    }

    pub fn isConstant(self: Self) bool {
        return self.isTrue() or self.isFalse();
    }
};

test "Wff.isTrue" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff1 = try wff_parser.buildWff(allocator, "T");
    defer wff1.deinit();
    var wff2 = try wff_parser.buildWff(allocator, "F");
    defer wff2.deinit();

    try std.testing.expect(wff1.isTrue());
    try std.testing.expect(!wff2.isTrue());
}

test "Wff.equals" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff1 = try wff_parser.buildWff(allocator, "((a ^ b) => (c ^ d))");
    defer wff1.deinit();
    var wff2 = try wff1.copy(allocator);
    defer wff2.deinit();
    var wff3 = try wff_parser.buildWff(allocator, "((a ^ b) => (c ^ d))");
    defer wff3.deinit();
    var wff4 = try wff_parser.buildWff(allocator, "(p => q)");
    defer wff4.deinit();

    try std.testing.expect(wff3.eql(wff1));
    try std.testing.expect(wff3.eql(wff2));
    try std.testing.expect(!wff3.eql(wff4));
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff = try wff_parser.buildWff(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.buildWff(allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try wff_parser.buildWff(allocator, "(p => q)");
    defer replace.deinit();

    var match = (try wff.match(allocator, pattern)).?;
    defer match.deinit();
    var new = (try match.replace(allocator, replace));
    defer new.deinit();

    var expected = try wff_parser.buildWff(allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.eql(new));
    //try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff = try wff_parser.buildWff(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.buildWff(allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try wff_parser.buildWff(allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(allocator, pattern) == null);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) to ((a ^ b) v (d ^ c)) using (p ^ q) to (q ^ p)" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff = try wff_parser.buildWff(allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try wff_parser.buildWff(allocator, "(p ^ q)");
    defer pattern.deinit();
    var replace = try wff_parser.buildWff(allocator, "(q ^ p)");
    defer replace.deinit();

    var matches = (try wff.matchAll(allocator, pattern)).?;
    defer {
        for (matches.items) |*m| m.deinit();
        matches.deinit();
    }
    try std.testing.expect(matches.items.len == 2);

    var right = matches.items[1];

    var expected = try wff_parser.buildWff(allocator, "((a ^ b) v (d ^ c))");
    defer expected.deinit();
    var new = (try right.replace(allocator, replace));
    defer new.deinit();

    try std.testing.expect(expected.eql(new));
}

test "Wff.replace: (T ^ ~T) using (a ^ ~a) to F [E15]" {
    const ParsingConfig = @import("wff-parsing.zig").NewParsing;
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var wff = try wff_parser.buildWff(allocator, "(T ^ ~T)");
    defer wff.deinit();
    var pattern = try wff_parser.buildWff(allocator, "(a ^ ~a)");
    defer pattern.deinit();
    var replace = try wff_parser.buildWff(allocator, "F");
    defer replace.deinit();

    var matches = (try wff.matchAll(allocator, pattern)).?;
    defer {
        for (matches.items) |*m| m.deinit();
        matches.deinit();
    }
    try std.testing.expect(matches.items.len == 1);

    const match = matches.items[0];

    var expected = try wff_parser.buildWff(allocator, "F");
    defer expected.deinit();
    var new = (try match.replace(allocator, replace));
    defer new.deinit();

    try std.testing.expect(expected.eql(new));
}
