const std = @import("std");

const MyUnion = union(enum) {
    choiceA: u32,
    choiceB: u64,
};

const AnonStructTest = struct {
    x: struct {a: u32, b: u64},
};


fn makeStruct(comptime allocator: std.mem.Allocator) type {
    return struct {
        const R1 = std.ArrayList(u32).init(allocator);
    };
}

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

test "copy string literal" {
    var a = try std.testing.allocator.dupe(u8, "hi");
    defer std.testing.allocator.free(a);
    var b = try std.testing.allocator.alloc(u8, 1);
    defer std.testing.allocator.free(b);
    b[0] = a[0];


    std.debug.print("{s} {s}\n", .{a, b});
}

test "const" {
    var a = [_]u32 {1, 2, 3, 4};
    var ptr: *const u32 = &a[0];

    ptr = &a[1];
    //ptr.* = 69;

}

test "struct init" {
    const mystruct = makeStruct(std.testing.allocator);
    var t = mystruct.R1;
    std.debug.print("\n\nt: {any}\n", .{t});
}

// test "array syntax" {
//     const arr = [_]u32 {
        
//     }
// }