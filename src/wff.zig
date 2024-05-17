const std = @import("std");
const parsing = @import("wff-parsing.zig");
const debug = std.debug;

pub const WffError = error{
    SubOutOfBounds,
    BadSubstitution,
};



pub const Match = struct {
    const Self = @This();

    wff: *const Wff,
    parent: *parsing.ParseTree.Node,
    matches: parsing.MatchHashMap,

    pub fn deinit(self: *Self) void {
        self.matches.deinit();
    }

    /// Lookup a match in the hashmap and build a new Wff for it if it exists
    pub fn getBuildWff(self: Self, allocator: std.mem.Allocator, key: []const u8) !?Wff {
        const node = self.matches.get(key) orelse return null;
        return try Wff.initFromNode(allocator, node);
    }

    pub fn replace(self: Self, pattern: Wff) !Wff {
        var result = try pattern.parse_tree.copy();

        // First we substitute variables into the pattern
        var it = result.iterDepthFirst();
        while (it.next()) |node| switch (node.data) {
            .Terminal => |tok| switch (tok) {
                .Proposition => |prop| {
                    if (self.matches.get(prop.string)) |wff_node| {
                        defer result.allocator.free(prop.string);
                        const match_copy = try wff_node.copy(result.allocator);
                        defer result.allocator.destroy(match_copy);
                        var old_data = node.parent.?.data.Nonterminal;
                        defer old_data.deinit();

                        node.parent.?.data = match_copy.data;
                        for (node.parent.?.data.Nonterminal.items) |*child| {
                            child.parent = node.parent.?;
                        }
                    }
                },
                else => {},
            },
            .Nonterminal => {},
        };
        
        // Then we copy the rest of the parse tree.
        const new_root = try self.parent.copyAbove(result.allocator, result.root.*);
        result.allocator.destroy(result.root);
        errdefer {
            // TODO: Clean up using node.deinit()
            (parsing.ParseTree{.allocator = self.wff.allocator, .root = new_root}).deinit();
        }

        result.root = new_root;
    

        return Wff {
            .allocator = result.allocator,
            .parse_tree = result,
            .string = try result.toString(result.allocator),
        };
    }
};

pub const Wff = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parse_tree: parsing.ParseTree,
    string: []u8,


    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Self {
        var tree = try parsing.ParseTree.init(allocator, wff_string);
        errdefer tree.deinit();
        return Wff{
            .string = try tree.toString(allocator),
            .parse_tree = tree,
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

    pub fn match(self: *const Self, pattern: Self) !?Match {
        return Match {
            .wff = self,
            .parent = self.parse_tree.root,
            .matches = try self.parse_tree.root.match(self.allocator, pattern.parse_tree.root) orelse return null
        };
    }

    pub fn matchAll(self: *const Self, pattern: Self) !?std.ArrayList(Match) {
        var all_matches = std.ArrayList(Match).init(self.allocator);
        errdefer {
            for (all_matches.items) |*matches| {
                matches.deinit();
            }
            all_matches.deinit();
        }

        var it = self.parse_tree.iterDepthFirst();

        while (it.next()) |node| {
            switch(node.data) {
                .Terminal => {},
                .Nonterminal => {
                    if (try node.match(self.allocator, pattern.parse_tree.root)) |m| {
                        try all_matches.append(Match{.wff = self, .parent = node, .matches = m});
                    } 
                }
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

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v q) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v q)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    var match = (try wff.match(pattern)).?;
    defer match.deinit();
    var new = (try match.replace(replace));
    defer new.deinit();

    var expected = try Wff.init(std.testing.allocator, "((a ^ b) => (c ^ d))");
    defer expected.deinit();

    try std.testing.expect(expected.eql(new));
    try std.testing.expectEqualStrings(expected.string, new.string);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p v p) to (p => q)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p v p)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    defer replace.deinit();

    try std.testing.expect(try wff.match(pattern) == null);
}

test "Wff.replace: ((a ^ b) v (c ^ d)) using (p ^ q) to (q ^ p)" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    defer wff.deinit();
    var pattern = try Wff.init(std.testing.allocator, "(p ^ q)");
    defer pattern.deinit();
    var replace = try Wff.init(std.testing.allocator, "(q ^ p)");
    defer replace.deinit();

    var matches = (try wff.matchAll(pattern)).?;
    defer {
        for (matches.items) |*m| m.deinit();
        matches.deinit();
    }
    try std.testing.expect(matches.items.len == 2);

    var right = matches.items[1];

    var expected = try Wff.init(std.testing.allocator, "((a ^ b) v (d ^ c))");
    defer expected.deinit();
    var new = (try right.replace(replace));
    defer new.deinit();

    try std.testing.expect(expected.eql(new));
}