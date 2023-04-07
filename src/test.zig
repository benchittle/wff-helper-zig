const std = @import("std");

const MyUnion = union(enum) {
    choiceA: u32,
    choiceB: u64,
};

const AnonStructTest = struct {
    x: struct {a: u32, b: u64},
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


//test "access" {
//    var a = MyUnion{.choiceA = 56};
//    if (a.choiceB) |c| {
//        std.debug.print("{d}\n", c);
//    }
//}

test "anon struct" {
    var a = AnonStructTest{.x = .{.a = 32, .b = 69}};
    std.debug.print("{any}\n", .{a});
}