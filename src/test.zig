const std = @import("std");
const stdout = std.io.getStdOut().writer();

// const MyUnion = union(enum) {
//     choiceA: u32,
//     choiceB: u64,
// };

// const AnonStructTest = struct {
//     x: struct {a: u32, b: u64},
// };

// fn makeStruct(comptime allocator: std.mem.Allocator) type {
//     return struct {
//         const R1 = std.ArrayList(u32).init(allocator);
//     };
// }

// test "choiceA" {
//     var allocator = std.testing.allocator;

//     // Okay
//     var u = MyUnion{.choiceA = 69};
//     try std.testing.expectEqual(MyUnion{.choiceA = 69}, u);

//     // Okay now
//     var dynamic_u = try allocator.create(MyUnion);
//     defer allocator.destroy(dynamic_u);

//     dynamic_u.* = MyUnion{.choiceA = 69};
//     try std.testing.expectEqual(MyUnion{.choiceA = 69}, dynamic_u.*);
// }

// test "defer1" {
//     var i: usize = 0;
//     while (i < 5): (i += 1) {
//         defer std.debug.print("DEFER: i={d}\n", .{i});
//         std.debug.print("REG: i={d}\n", .{i});
//     }
// }

// //test "access" {
// //    var a = MyUnion{.choiceA = 56};
// //    if (a.choiceB) |c| {
// //        std.debug.print("{d}\n", c);
// //    }
// //}

// test "anon struct" {
//     var a = AnonStructTest{.x = .{.a = 32, .b = 69}};
//     std.debug.print("{any}\n", .{a});
// }

// test "copy string literal" {
//     var a = try std.testing.allocator.dupe(u8, "hi");
//     defer std.testing.allocator.free(a);
//     var b = try std.testing.allocator.alloc(u8, 1);
//     defer std.testing.allocator.free(b);
//     b[0] = a[0];

//     std.debug.print("{s} {s}\n", .{a, b});
// }

// test "const" {
//     var a = [_]u32 {1, 2, 3, 4};
//     var ptr: *const u32 = &a[0];

//     ptr = &a[1];
//     //ptr.* = 69;

// }

// test "struct init" {
//     const mystruct = makeStruct(std.testing.allocator);
//     var t = mystruct.R1;
//     std.debug.print("\n\nt: {any}\n", .{t});
// }

// test "arraylist" {
//     var list = try std.ArrayList(u32).initCapacity(std.testing.allocator, 2);
//     defer list.deinit();
//     list.appendAssumeCapacity(44);
//     try std.testing.expect(list.items.len == 1);
// }

// // test "array syntax" {
// //     const arr = [_]u32 {

// //     }
// // }

// test "str size" {
//     var x = "";
//     try std.testing.expect(x.len == 0);
// }

// test "mem split" {
//     var str = "(p v q), 1, E10";
//     var it = std.mem.split(u8, str, ",");
//     while (it.next()) |s| {
//         std.debug.print("{s}\n", .{s});
//     }
// }

// // test "comptime split" {
// //     comptime {
// //         var s = "wff -> wff v wff";
// //         //@compileLog(s);
// //         var it = std.mem.split(u8, s, "->");
// //         while (it.next()) |chunk| {
// //             @compileLog(chunk);
// //         }
// //     }
// // }

// test "iterate anon struct" {
//     comptime {
//         // var args = .{
//         //     .{"wff", .{"wff", 10, "wff"}},
//         // };
//         // if (@typeInfo(args) != .Struct) {
//         //     @compileError("expected struct type");
//         // }

//         // const ArgsType = @TypeOf(args);
//         // const args_type_info = @typeInfo(ArgsType);
//         // const args_fields = args_type_info.Struct.fields;
//         // @compileLog(args_fields.len);

//         // const a1 = @field(args, args_fields[0].name);

//         // const A1Type = @TypeOf(a1);
//         // const a1_type_info = @typeInfo(A1Type);
//         // const a1_fields = a1_type_info.Struct.fields;
//         // const a1_lhs = @field(a1, a1_fields[0].name);
//         // const a1_rhs = @field(a1, a1_fields[1].name);

//         // @compileLog(a1_lhs);
//         // @compileLog(a1_rhs);

//     }
// }

// test "iterate anon struct 2" {
//     comptime {
//         // var h: []const u8 = "Steve";
//         // //var q: u8 = 10;
//         // @compileLog(@typeInfo(@TypeOf(h)));
//         //@compileLog(@TypeOf(h, q));
//         // var args = .{
//         //     .{"wff", .{"wff", 10, "wff"}},
//         // };
//         // if (@typeInfo(args) != .Struct) {
//         //     @compileError("expected struct type");
//         // }
//         // const ArgsType = @TypeOf(args);
//         // const args_type_info = @typeInfo(ArgsType);
//         // const args_fields = args_type_info.Struct.fields;
//         // switch(@typeInfo(@TypeOf(args))) {
//         //     .Struct => |info| @compileLog(info),
//         //     else => _ = 2,
//         // }

//         //std.fmt.comptimePrint("{any}", .{8});

//         // const A1Type = @TypeOf(a1);
//         // const a1_type_info = @typeInfo(A1Type);
//         // const a1_fields = a1_type_info.Struct.fields;
//         // const a1_lhs = @field(a1, a1_fields[0].name);
//         // const a1_rhs = @field(a1, a1_fields[1].name);

//         // @compileLog(a1_lhs);
//         // @compileLog(a1_rhs);

//     }
// }
const h = "jello";

fn getSlice() []const u8 {
    return &[_]u8{ 'a', 'b', 'c', 0 };
}

test "creating_slices" {
    // comptime {
    //     var s = getSlice();
    //     _ =s;
    //     //@compileLog(s);
    // }]
    var s2 = getSlice();
    const ptr = &s2;
    _ = ptr;
    const hh = h;
    _ = hh;
    try stdout.print("out: {s} {s}\n", .{h, s2});
    //std.debug.print("{s}", .{s2});
}

// test "comptime_alloc" {
//     comptime var allocator: std.mem.Allocator = undefined;
//     const s: std.ArrayList(u8) = undefined;
    
//     const space: [1000]u8 = undefined;
//     comptime {
//         var fba = std.heap.FixedBufferAllocator.init(@constCast(&space));
//         allocator = fba.allocator();
//         s = std.ArrayList(u8).initCapacity(allocator, 8) catch @compileError("Not enough space");
//         s.append('h') catch @compileError("Not enough space");
//         s.append(0) catch @compileError("Not enough space");
//     }
//     //const s_ = s;
//     //s_.items = s.items;
//     try stdout.print("s={s}\n", .{s.items});
//     allocator.free(s.items.ptr[0..s.capacity]);
    
    
// }

// test "comptime_alloc_0.12" {
//     comptime {
//         const a = std.testing.allocator;

//         var list = std.ArrayList(u8).init(a);
//         try list.append('a');
//         try list.append(0);
        
//         @compileLog(list);
//         //try stdout.print("list={s}\n", .{list.items});

//         list.deinit();
    // }
// }

test "comptime_array_0.12" {
    comptime var arr: []const u8 = "hi";
    comptime {
        for ('a'..'d') |i| {
            arr = arr ++ [_]u8 {i};
        }
    }

    const final = arr;
    try stdout.print("{s}\n", .{final});
}

fn getNum() u8 {
    return 5;
}

// test "comptime_array_0.12 again" {
//     comptime {
//         var arr: []const u8 = &[_]u8 {8};

//         while (arr.len > 0) {
//             const n = arr[arr.len - 1];
//             arr = arr[0..arr.len - 1];

//             if (n < 10) {
//                 arr = arr ++ [_]u8 {n + 1};
//             }
//             @compileLog(n);
//         }
//     }
// }

fn f1(comptime n: u8)  []const u8 {
    //const arr_slice: []const u8 = undefined;
    return comptime ret: {
        var arr: []const u8 = &[_]u8 {'a'} ** n;
        arr[0] = 'z';
        break :ret arr;
    };
}

fn f(comptime n: u8)  []const []const u8 {
    //const arr_slice: []const u8 = undefined;
    return comptime ret: {
        var arr = [_][n]u8 {
            [_]u8 {'a'} ** n
        } ** 2;
        arr[0][0] = 'z';
        const arr_const = arr;
        
        var mid = [_][]const u8 {undefined} ** arr_const.len;
        for (0..arr_const.len) |i| {
            mid[i] = &arr_const[i];
        }

        const mid_const = mid;

        break :ret &mid_const;
    };
}

test "comptime return array slices" {
    try stdout.print("{s}\n", .{f(5)});
}

fn ret_tuple() struct{u8, u8} {
    return .{2, 3};
}

test "unpacking" {
    const x, const y = ret_tuple();
    try stdout.print("{d} {d}\n", .{x, y});
}

// fn List(comptime T: type) struct {
//         const Self = @This();
//         elements: [5]T,

//         fn getFirst(self: Self) T {
//             return self.elements[0];
//         }
//     } {
//     return struct {
//         const Self = @This();
//         elements: [5]T,

//         fn getFirst(self: Self) T {
//             return self.elements[0];
//         }
//     };
// }
// test "fn type stuff" {
//     const ListType = List(u16);
//     var l = ListType {.elements = [5]u16 {1, 2, 3, 4, 5} };
//     try stdout.print("{d}\n", .{l.getFirst()});
// }

const ue = union(enum) {
    prop: []const u8,
    plus,
};

// test "union enum stuff" {
//     //@compileLog(@TypeOf(ue.prop));
//     const t = ue {.prop = "hi"};
//     const t2 = ue{.plus = {}};

//     @compileLog(@TypeOf(t) == ue);
//     @compileLog(@TypeOf(t2) == ue);
//     @compileLog(@intFromEnum(t));
//     // try switch(t) {
//     //     .plus => stdout.print("plus\n", .{}),
//     //     .prop => stdout.print("prop\n", .{}),
//     // };
// }

// test "for iteration" {
//     for(5..0) |i| {
//         stdout.print("{d}\n", .{i});
//     }
// }


test "comptime array init" {
    comptime var branches = [_][]const u8 {
        &[_]u8 {},
    } ** 3;


    branches[1] = branches[1] ++ &[_]u8 {2};
    try stdout.print("{any}\n", .{branches});
}

test "functions" {
    var l: u32 = 10;
    for (0..l) |i| {
        l -= 1;
        try stdout.print("{d}\n", .{i});
    }
}

const TestError = error {
    temp,
};

const t = struct {
    pub fn dosth(a: u32, b: u16) !u32 {
        return  (a + b < 100); // a + b else TestError.temp;
    }
};

fn do() void {
    return true;
}

test "comparing fn signatures" {
    // if (@TypeOf(t.dosth) != fn(u32, u16) u32) {
    //     @compileLog(@TypeOf(t.dosth));
    //     @compileError("Not same");
    // }

    const info = @typeInfo(@TypeOf(t.dosth));
    const info2 = info.Fn.return_type.?;
    const info3 = @typeInfo(info2);
    
    @compileLog(info3.ErrorUnion.);
}

