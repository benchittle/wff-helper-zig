const std = @import("std");
const parsing = @import("parsing.zig");
const debug = std.debug;

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};

pub const Wff = struct {
    const Self = @This();

    string: []u8,
    parse_tree: parsing.ParseTree,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Self {
        return Wff{
            .string = try allocator.dupe(u8, wff_string),
            .parse_tree = try parsing.ParseTree.init(allocator, wff_string),
            .allocator = allocator,
        };
    }

    pub fn initFromNode(allocator: std.mem.Allocator, node: *parsing.ParseTree.Node) !Self {
        std.debug.assert(
            switch(node.data) {
                .Terminal => false,
                .Nonterminal => true,
            }
        );

        var tree = parsing.ParseTree {
            .allocator = allocator,
            .root = try node.copy(allocator),
        };
        errdefer tree.deinit();

        return Wff {
            .allocator = allocator,
            .string = try tree.toString(allocator),
            .parse_tree = tree,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parse_tree.deinit();
        self.allocator.free(self.string);
    }

    pub fn copy(self: *Self) !Self {
        return Wff{
            .string = try self.allocator.dupe(u8, self.string),
            .parse_tree = try self.parse_tree.copy(),
            .allocator = self.allocator,
        };
    }

    pub fn equals(self: *Self, other: *Self) bool {
        return self.parse_tree.equals(&other.parse_tree);
    }


    pub fn match(self: *Self, pattern: *Self) !?parsing.MatchHashMap {
        return self.parse_tree.root.match(self.allocator, pattern.parse_tree.root);
    }

    // TODO: Match should return a pre built tree? Or wff?
    // TODO: a match should be a hashmap with the token string as the key, not 
    //       the token
    pub fn matchAll(self: *Self, pattern: *Self) !?std.ArrayList(parsing.MatchHashMap) {
        return self.parse_tree.matchAll(&pattern.parse_tree);
    }

    
    pub fn substitute(self: *Self, pattern: *Self, replace: *Self, index: usize) !Self {
        var result = try replace.copy();
        errdefer result.deinit();

        var candidates = try self.matchAll(pattern) orelse return result;
        defer {
            for (candidates.items) |*matches| {
                matches.deinit();
            }
            candidates.deinit();
        }
        if (index >= candidates.items.len) return WffError.SubOutOfBounds;

        var chosen_matches = candidates.items[index];

        var it = result.parse_tree.iterDepthFirst();
        while (it.next()) |node| switch (node.data) {
            .Terminal => |tok| switch(tok) {
                .Proposition => |prop| {
                    if (chosen_matches.get(&prop.string)) |wff_node| {
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

test "Wff.equals" {
    var wff1 = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff1.deinit();
    var wff2 = try wff1.copy();
    defer wff2.deinit();
    var wff3 = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer wff3.deinit();
    var wff4 = try Wff.init(std.testing.allocator, "(p => q)");
    defer wff4.deinit();

    try std.testing.expect(wff3.equals(&wff1));
    try std.testing.expect(wff3.equals(&wff2));
    try std.testing.expect(!wff3.equals(&wff4));
}

test "Wff.substitute: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    var new = try wff.substitute(&pattern, &replace, 0);
    defer new.deinit();

    var expected = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.equals(&new));
    try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.substitute: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    var new = try wff.substitute(&pattern, &replace, 0);
    defer new.deinit();

    var expected = try Wff.init(std.testing.allocator, "(p => q)");
    defer expected.deinit();

    try std.testing.expect(expected.equals(&new));
}
