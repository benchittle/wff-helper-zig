const std = @import("std");
const parsing = @import("parsing.zig");
const debug = std.debug;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const WffError = error {
    SubOutOfBounds,
    BadSubstitution,
};


const Wff = struct {
    const Self = @This();

    string: []const u8,
    parse_tree: parsing.ParseTree,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Self {
        return Wff{
            .string = wff_string, 
            .parse_tree = try parsing.ParseTree.init(allocator, wff_string), 
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parse_tree.deinit();
    }

    pub fn copy(self: *Self) !Self {
        return Wff{
            .string = self.string,
            .parse_tree = try self.parse_tree.copy(), 
            .allocator = self.allocator,
        };
    }

    pub fn equals(self: *Self, other: *Self) bool {
        return self.parse_tree.equals(&other.parse_tree);
    }

    pub fn matchAll(self: *Self, pattern: *Self) !?std.ArrayList(std.ArrayList(parsing.Match)) {
        return self.parse_tree.matchAll(&pattern.parse_tree);
    }

    pub fn substitute(self: *Self, pattern: *Self, replace: *Self, index: usize) !Self {
        var result = try replace.copy();
        errdefer result.deinit();

        var candidates = try self.matchAll(pattern) orelse return result;
        defer {
            for (candidates.items) |matches| {
                matches.deinit();
            }
            candidates.deinit();
        }
        if (index >= candidates.items.len) return WffError.SubOutOfBounds;

        var chosen_matches = candidates.items[index];

        var it = result.parse_tree.iterDepthFirst();
        while (it.next()) |node| switch(node.data) {
            .Terminal => |tok| switch(tok) {
                .Proposition => {
                    // TODO: Use hashmap for matches
                    for (chosen_matches.items) |match| {
                        if (tok.equals(match.pattern_node.data.Terminal)) {
                            var match_copy = try match.wff_node.copy(result.allocator); 
                            defer result.allocator.destroy(match_copy);
                            var old_data = node.parent.?.data.Nonterminal;
                            defer old_data.deinit();

                            node.parent.?.data = match_copy.data;
                            for (match_copy.data.Nonterminal.items) |*child| {
                                child.parent = node.parent.?;
                            }
                            break;
                        }
                    }
                },
                else => {},
            },
            .Nonterminal => {},
        };
        
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


test "Wff.substitute" {
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

    var new_str = try new.parse_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(new_str);

    var expected_str = try expected.parse_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(expected_str);

    //std.debug.print("\n\nexpected_given: '{s}'\nexpected: '{s}'\nactual: '{s}'\n\n", .{expected.string, expected_str, new_str});

    try std.testing.expect(expected.equals(&new));
}


pub fn main() !void {
    std.debug.print("Hello\n", .{});

    _ = try Wff.init(gpa.allocator(), "(p v q)");

    std.debug.print("Done\n", .{});
}