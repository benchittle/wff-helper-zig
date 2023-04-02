const std = @import("std");
const parser = @import("parser.zig");
const debug = std.debug;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};



const Wff = struct {
    string: []const u8,
    parse_tree: ParseTree,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8) Wff {
        return Wff{
            .string = wff_string, 
            .parse_tree = ParseTree{.allocator = allocator}, 
            .allocator = allocator
        };
    }
};


pub fn main() !void {
    std.debug.print("Hello\n", .{});

    //_ = Wff.init(gpa.allocator(), "(p v q)");

    std.debug.print("Done\n", .{});
}