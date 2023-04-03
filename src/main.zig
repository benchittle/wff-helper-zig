const std = @import("std");
const parsing = @import("parsing.zig");
const debug = std.debug;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};



const Wff = struct {
    string: []const u8,
    parse_tree: parsing.ParseTree,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) !Wff {
        return Wff{
            .string = wff_string, 
            .parse_tree = try parsing.ParseTree.init(allocator, wff_string), 
            .allocator = allocator
        };
    }
};


pub fn main() !void {
    std.debug.print("Hello\n", .{});

    _ = try Wff.init(gpa.allocator(), "(p v q)");

    std.debug.print("Done\n", .{});
}