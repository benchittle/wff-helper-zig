const std = @import("std");

const MyUnion = union(enum) {
    choiceA: u32,
    choiceB: u64,
};

test "choiceA" {
    var allocator = std.testing.allocator;

    // Okay
    var u = MyUnion{.choiceA = 69};
    try std.testing.expectEqual(MyUnion{.choiceA = 69}, u);
    
    // Okay now
    var dynamic_u = try allocator.create(MyUnion);
    defer allocator.destroy(dynamic_u);

    dynamic_u.* = MyUnion{.choiceA = 69};
    try std.testing.expectEqual(MyUnion{.choiceA = 69}, dynamic_u.*);
}

test "defer1" {
    var i: usize = 0;
    while (i < 5): (i += 1) {
        defer std.debug.print("DEFER: i={d}\n", .{i});
        std.debug.print("REG: i={d}\n", .{i});
    }
}

test "defer2" {
    return 0;
    defer std.debug.print("still here :)\n");
}