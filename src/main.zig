const std = @import("std");
const parsing = @import("parsing.zig");
const debug = std.debug;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub const WffError = error {
    SubOutOfBounds,
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
            // TODO: try self.parse_tree.copy(),
            .parse_tree = try parsing.ParseTree.init(self.allocator, self.string), 
            .allocator = self.allocator,
        };
    }

    pub fn matchAll(self: *Self, pattern: *Self) !?std.ArrayList(std.ArrayList(parsing.Match)) {
        return self.parse_tree.matchAll(&pattern.parse_tree);
    }

    pub fn substitute(self: *Self, pattern: *Self, replace: *Self, index: usize) !?Self {
        var candidates = try self.matchAll(pattern) orelse return null;
        defer {
            for (candidates.items) |matches| {
                matches.deinit();
            }
            candidates.deinit();
        }
        if (index >= candidates.items.len) return WffError.SubOutOfBounds;

        var chosen_matches = candidates.items[index];
        //defer chosen_matches.deinit();
        // errdefer {}  // clean up contents?

        var result = try replace.copy();
        errdefer result.deinit();

        for (chosen_matches.items) |*match| {
            // TODO HERE BEN
            match.wff_node = match.wff_node.copy(result.parse_tree.allocator);
        }



    }
};

test "Wff.substiture" {
    var wff = try Wff.init(std.testing.allocator, "((a ^ b) v (c ^ d))");
    var pattern = try Wff.init(std.testing.allocator, "(p v q)");
    var replace = try Wff.init(std.testing.allocator, "(p => q)");
    _ = try wff.substitute(&pattern, &replace, 0);
}


pub fn main() !void {
    std.debug.print("Hello\n", .{});

    _ = try Wff.init(gpa.allocator(), "(p v q)");

    std.debug.print("Done\n", .{});
}