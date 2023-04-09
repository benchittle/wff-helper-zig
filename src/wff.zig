const std = @import("std");
const parsing = @import("parsing.zig");
const debug = std.debug;

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};

pub const Match = struct {
    const Self = @This();

    matches: parsing.MatchHashMap,

    pub fn deinit(self: *Self) void {
        self.matches.deinit();
    }

    /// Lookup a match in the hashmap and build a new Wff for it if it exists
    pub fn getBuildWff(self: Self, allocator: std.mem.Allocator, key: []const u8) !?Wff {
        var node = self.matches.get(key) orelse return null;
        return try Wff.initFromNode(allocator, node);
    }

    pub fn substitute(self: Self, replace: Wff) !?Wff {
        var result = try replace.copy();
        errdefer result.deinit();

        var it = result.parse_tree.iterDepthFirst();
        while (it.next()) |node| switch (node.data) {
            .Terminal => |tok| switch (tok) {
                .Proposition => |prop| {
                    if (self.matches.get(prop.string)) |wff_node| {
                        defer result.allocator.free(prop.string);
                        var match_copy = try wff_node.copy(result.allocator);
                        defer result.allocator.destroy(match_copy);
                        var old_data = node.parent.?.data.Nonterminal;
                        defer old_data.deinit();

                        node.parent.?.data = match_copy.data;
                        for (match_copy.data.Nonterminal.items) |*child| {
                            child.parent = node.parent.?;
                        }
                    }
                },
                else => {},
            },
            .Nonterminal => {},
        };

        result.allocator.free(result.string);
        result.string = try result.parse_tree.toString(result.allocator);

        return result;
    }
};

pub const Wff = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    string: []u8,
    parse_tree: parsing.ParseTree,


    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Self {
        return Wff{
            .string = try allocator.dupe(u8, wff_string),
            .parse_tree = try parsing.ParseTree.init(allocator, wff_string),
            .allocator = allocator,
        };
    }

    /// Create a new Wff instance from a nonterminal node. The node is used as 
    /// the root of the new parse tree, and all nodes are copied, so this Wff
    /// shares no memory with the parse tree nodes it was created from.
    pub fn initFromNode(allocator: std.mem.Allocator, node: *parsing.ParseTree.Node) !Self {
        // The root node is always nonterminal.
        std.debug.assert(switch (node.data) {
            .Terminal => false,
            .Nonterminal => true,
        });

        var tree = parsing.ParseTree{
            .allocator = allocator,
            .root = try node.copy(allocator),
        };
        errdefer tree.deinit();

        return Wff{
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
        return Wff{
            .string = try self.allocator.dupe(u8, self.string),
            .parse_tree = try self.parse_tree.copy(),
            .allocator = self.allocator,
        };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.parse_tree.eql(other.parse_tree);
    }

    pub fn match(self: Self, pattern: Self) !?Match {
        return Match {
            .matches = try self.parse_tree.root.match(self.allocator, pattern.parse_tree.root) orelse return null
        };
    }

    // TODO: Maybe ParseTree.matchall should return Match objects instead of HashMaps?
    pub fn matchAll(self: Self, pattern: Self) !?std.ArrayList(Match) {
        var match_maps = try self.parse_tree.matchAll(pattern.parse_tree) orelse return null;
        defer match_maps.deinit();
        errdefer {
            for (match_maps.items) |*m| m.deinit();
        }
        var matches = try std.ArrayList(Match).initCapacity(self.allocator, match_maps.items.len);
        for (match_maps.items) |m| {
            matches.appendAssumeCapacity(Match{.matches = m});
        }
        return matches;
    }
};

test "Wff.equals" {
    var wff1 = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff1.deinit();
    var wff2 = try wff1.copy();
    defer wff2.deinit();
    var wff3 = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff3.deinit();
    var wff4 = try Wff.init(std.testing.allocator, "(p => q)");
    defer wff4.deinit();

    try std.testing.expect(wff3.eql(wff1));
    try std.testing.expect(wff3.eql(wff2));
    try std.testing.expect(!wff3.eql(wff4));
}

test "Wff.substitute: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    var match = (try wff.match(pattern)).?;
    defer match.deinit();
    var new = (try match.substitute(replace)).?;
    defer new.deinit();

    var expected = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.eql(new));
    try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.substitute: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(pattern) == null);
}
