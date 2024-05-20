const std = @import("std");
const parsing = @import("wff-parsing.zig");
const debug = std.debug;

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};

// TODO put in Wff namespace / scope
pub fn Match(comptime Token: type) type {
    return struct {
        const Self = @This();
        const WffType = Wff(Token);
        const ParseTreeType = parsing.ParseTree(Token);

        wff: *const WffType,
        parent: *ParseTreeType.Node,
        matches: parsing.MatchHashMap(Token),

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
            var it = result.iterDepthFirst();
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
}

pub fn Wff(comptime Token: type) type {
    return struct {
        const Self = @This();
        const ParseTree = parsing.ParseTree(Token);
        const MatchType = Match(Token);

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

            var it = self.parse_tree.iterDepthFirst();

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
}
test "Wff.equals" {
    const WffType = Wff(parsing.TestToken);
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
    const WffType = Wff(parsing.TestToken);
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
    const WffType = Wff(parsing.TestToken);
    var wff = try WffType.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try WffType.init(std.testing.allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try WffType.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(pattern) == null);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p ^ q) to (q ^ p)" {
    const WffType = Wff(parsing.TestToken);
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
