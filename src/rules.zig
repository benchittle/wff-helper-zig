const std = @import("std");

const wfflib = @import("wff.zig");
const ParsingConfig = @import("wff-parsing.zig").NewParsing;

pub const EquivalenceRule = struct {
    const Self = @This();
    const Wff = wfflib.Wff;

    lhs: Wff,
    rhs: Wff,
    num: usize,

    pub fn deinit(self: Self) void {
        self.lhs.deinit();
        self.rhs.deinit();
    }

    /// Returns true if a single application of the equivalence rule can
    /// transform `from` to `to`.
    pub fn canTransform(self: Self, allocator: std.mem.Allocator, from: Wff, to: Wff) !bool {
        if (try from.matchAll(allocator, self.lhs)) |left_to_right| {
            defer {
                for (left_to_right.items) |*m| m.deinit();
                left_to_right.deinit();
            }
            for (left_to_right.items) |match| {
                var result = try match.replace(allocator, self.rhs);
                defer result.deinit();
                if (result.eql(to)) {
                    return true;
                }
            }
        }
        if (try from.matchAll(allocator, self.rhs)) |right_to_left| {
            defer {
                for (right_to_left.items) |*m| m.deinit();
                right_to_left.deinit();
            }
            for (right_to_left.items) |match| {
                var result = try match.replace(allocator, self.lhs);
                defer result.deinit();
                if (result.eql(to)) {
                    return true;
                }
            }
        }
        return false;
    }
};

// test "EquivalenceRule.canTransform (expect error): (~p v ~q) to ~(p ^ q) using ~~a = a" {
//     const allocator = std.testing.allocator;

//     var from = try wfflib.old_wff_parser.parse(allocator, "(~p v ~q)");
//     defer from.deinit();
//     var to = try wfflib.old_wff_parser.parse(allocator, "~(p ^ q)");
//     defer to.deinit();

//     var rule = EquivalenceRule {
//         .lhs = try wfflib.old_wff_parser.parse(allocator, "~~a"),
//         .rhs = try wfflib.old_wff_parser.parse(allocator, "a"),
//         .num = undefined,
//     };
//     defer rule.deinit();
//     //try std.testing.expectError(try rule.canTransform(allocator, from, to));
//     _ = try rule.canTransform(allocator, from, to);
// }

test "EquivalenceRule.canTransform: (~p v q) to (p => q) using (~a v b) = (a => b)" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var from = try wff_parser.buildWff(allocator, "(~p v q)");
    defer from.deinit();
    var to = try wff_parser.buildWff(allocator, "(p => q)");
    defer to.deinit();
    var wrong = try wff_parser.buildWff(allocator, "(~q v p)");
    defer wrong.deinit();

    var rule = EquivalenceRule{
        .lhs = try wff_parser.buildWff(allocator, "(~a v b)"),
        .rhs = try wff_parser.buildWff(allocator, "(a => b)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(allocator, from, to));
    try std.testing.expect(try rule.canTransform(allocator, to, from));
    try std.testing.expect(!(try rule.canTransform(allocator, from, wrong)));
    try std.testing.expect(!(try rule.canTransform(allocator, wrong, to)));
}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((w v x) ^ (~y v z)) using (~a v b) = (a => b)" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var from = try wff_parser.buildWff(allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try wff_parser.buildWff(allocator, "((w v x) ^ (~y v z))");
    defer to.deinit();

    var rule = EquivalenceRule{
        .lhs = try wff_parser.buildWff(allocator, "(~a v b)"),
        .rhs = try wff_parser.buildWff(allocator, "(a => b)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(allocator, from, to));
    try std.testing.expect(try rule.canTransform(allocator, to, from));
}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((y => z) ^ (w v x)) using (a ^ b) = (b ^ a)" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var from = try wff_parser.buildWff(allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try wff_parser.buildWff(allocator, "((y => z) ^ (w v x))");
    defer to.deinit();

    var rule = EquivalenceRule{
        .lhs = try wff_parser.buildWff(allocator, "(a ^ b)"),
        .rhs = try wff_parser.buildWff(allocator, "(b ^ a)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(allocator, from, to));
    try std.testing.expect(try rule.canTransform(allocator, to, from));
}

test "EquivalenceRule.canTransform: ((y <=> z) ^ (w v x)) to (((y <=> z) ^ w) v ((y <=> z) ^ x)) using (a ^ (b v c)) = ((a ^ b) v (a ^ c))" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var from = try wff_parser.buildWff(allocator, "((y <=> z) ^ (w v x))");
    defer from.deinit();
    var to = try wff_parser.buildWff(allocator, "(((y <=> z) ^ w) v ((y <=> z) ^ x))");
    defer to.deinit();

    const rules = initEquivalenceRules(allocator, wff_parser);
    defer for (rules) |r| r.deinit();
    const e13 = rules[12];

    try std.testing.expect(try e13.canTransform(allocator, from, to));
    try std.testing.expect(try e13.canTransform(allocator, to, from));
}

test "!EquivalenceRule.canTransform: (T ^ ~T) to F using ~~a  = a" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var from = try wff_parser.buildWff(allocator, "(T ^ ~T)");
    defer from.deinit();
    var to = try wff_parser.buildWff(allocator, "F");
    defer to.deinit();

    const rules = initEquivalenceRules(allocator, wff_parser);
    defer for (rules) |r| r.deinit();
    const e15 = rules[14];

    try std.testing.expect(!try e15.canTransform(allocator, from, to));
    try std.testing.expect(!try e15.canTransform(allocator, to, from));
}

pub fn initEquivalenceRules(allocator: std.mem.Allocator, wff_builder: ParsingConfig.WffBuilder) [20]EquivalenceRule {

    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        wff_builder: ParsingConfig.WffBuilder,
        count: usize = 0,

        fn makeRule(self: *Self, comptime lhs: []const u8, comptime rhs: []const u8) EquivalenceRule {
            self.count += 1;
            return EquivalenceRule{
                .lhs = self.wff_builder.buildWff(self.allocator, lhs) catch {
                    std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                },
                .rhs = self.wff_builder.buildWff(self.allocator, rhs) catch {
                    std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                },
                .num = self.count,
            };
        }
    };
    var maker = Maker{ .allocator = allocator, .wff_builder = wff_builder };
    return [_]EquivalenceRule{
        maker.makeRule("(a v ~a)", "T"),
        maker.makeRule("(a ^ ~a)", "F"),
        maker.makeRule("(a ^ a)", "a"),
        maker.makeRule("(a v a)", "a"),
        maker.makeRule("(a ^ T)", "a"),
        maker.makeRule("(a v F)", "a"),
        maker.makeRule("(a ^ F)", "F"),
        maker.makeRule("(a v T)", "T"),
        maker.makeRule("(a ^ b)", "(b ^ a)"),
        maker.makeRule("(a v b)", "(b v a)"), // E10
        maker.makeRule("((a ^ b) ^ c)", "(a ^ (b ^ c))"),
        maker.makeRule("((a v b) v c)", "(a v (b v c))"),
        maker.makeRule("(a ^ (b v c))", "((a ^ b) v (a ^ c))"),
        maker.makeRule("(a v (b ^ c))", "((a v b) ^ (a v c))"),
        maker.makeRule("~~a", "a"),
        maker.makeRule("~(a ^ b)", "(~a v ~b)"),
        maker.makeRule("~(a v b)", "(~a ^ ~b)"),
        maker.makeRule("(a => b)", "(~a v b)"),
        maker.makeRule("(a => b)", "(~b => ~a)"),
        maker.makeRule("((a => b) ^ (b => a))", "(a <=> b)"),
        // maker.makeRule("T", "~F"),
        // maker.makeRule("~T", "F"),
    };
}

pub const InferenceRule = struct {
    const Self = @This();
    const Wff = wfflib.Wff;

    const Conditions = union(enum) {
        one: [1]Wff,
        two: [2]Wff,

        fn getSlice(self: *const @This()) []const Wff {
            switch (self.*) {
                .one => |*array| return array,
                .two => |*array| return array,
            }
        }
    };

    allocator: std.mem.Allocator,
    conditions: Conditions,
    result: Wff,
    num: usize,

    pub fn deinit(self: Self) void {
        for (self.conditions.getSlice()) |wff| {
            wff.deinit();
        }
        self.result.deinit();
    }

    // TODO: Why does this use nodes and stuff?
    pub fn canTransform(self: Self, allocator: std.mem.Allocator, from: []const Wff, to: Wff) !bool {
        if (from.len != self.conditions.getSlice().len) {
            return false;
        }
        var all_matches = wfflib.Wff.MatchHashMap.init(allocator);
        defer all_matches.deinit();

        for (from, self.conditions.getSlice()) |wff, condition| {
            var match = try wff.match(allocator, condition) orelse return false;
            defer match.deinit();

            var it = match.map.iterator();
            while (it.next()) |entry| {
                if (all_matches.get(entry.key_ptr.*)) |existing_wff_node| {
                    if (!existing_wff_node.eql(entry.value_ptr.*)) return false;
                } else {
                    try all_matches.put(entry.key_ptr.*, entry.value_ptr.*);
                }
            }
        }

        var result_match = try to.match(allocator, self.result) orelse return false;
        defer result_match.deinit();
        var it = result_match.map.iterator();
        while (it.next()) |entry| {
            if (all_matches.get(entry.key_ptr.*)) |existing_wff_node| {
                if (!existing_wff_node.eql(entry.value_ptr.*)) return false;
            } else {
                try all_matches.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }

        return true;
    }
};

test "InferenceRule.canTransform: p to (p v q) using I1" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var cond1 = try wff_parser.buildWff(allocator, "p");
    defer cond1.deinit();
    var conds = [_]wfflib.Wff{cond1};
    var result = try wff_parser.buildWff(allocator, "(p v q)");
    defer result.deinit();
    var wrong = try wff_parser.buildWff(allocator, "(q v p)");
    defer wrong.deinit();

    const rules = initInferenceRules(allocator, wff_parser);
    defer {
        for (rules) |r| {
            r.deinit();
        }
    }
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(allocator, &conds, result));
    try std.testing.expect(!(try infer1.canTransform(allocator, &conds, wrong)));
}

test "InferenceRule.canTransform: p to (p v (p => q)) using I1" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var cond1 = try wff_parser.buildWff(allocator, "p");
    defer cond1.deinit();
    var conds = [_]wfflib.Wff{cond1};
    var result = try wff_parser.buildWff(allocator, "(p v (p => q))");
    defer result.deinit();

    const rules = initInferenceRules(allocator, wff_parser);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(allocator, &conds, result));
}

test "InferenceRule.canTransform: (a ^ b) to ((a ^ b) v ~(a ^ b)) using I1" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var cond1 = try wff_parser.buildWff(allocator, "(a ^ b)");
    defer cond1.deinit();
    var conds = [_]wfflib.Wff{cond1};
    var result = try wff_parser.buildWff(allocator, "((a ^ b) v ~(a ^ b))");
    defer result.deinit();

    const rules = initInferenceRules(allocator, wff_parser);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(allocator, &conds, result));
}

test "InferenceRule.canTransform: (a ^ b), ((a ^ b) => ~x) to ~x using I3" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var cond1 = try wff_parser.buildWff(allocator, "(a ^ b)");
    defer cond1.deinit();
    var cond2 = try wff_parser.buildWff(allocator, "((a ^ b) => ~x)");
    defer cond2.deinit();
    var conds = [_]wfflib.Wff{ cond1, cond2 };
    var result = try wff_parser.buildWff(allocator, "~x");
    defer result.deinit();

    const rules = initInferenceRules(allocator, wff_parser);
    defer for (rules) |r| r.deinit();
    const infer3 = rules[2];

    try std.testing.expect(try infer3.canTransform(allocator, &conds, result));
}

pub fn initInferenceRules(allocator: std.mem.Allocator, wff_builder: ParsingConfig.WffBuilder) [6]InferenceRule {
    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        wff_builder: ParsingConfig.WffBuilder,
        count: usize = 0,

        fn makeRule(self: *Self, comptime cond1: []const u8, comptime cond2: []const u8, comptime result: []const u8) InferenceRule {
            self.count += 1;
            const conditions = ret: {
                if (cond2.len == 0) {
                    break :ret InferenceRule.Conditions{ .one = [1]wfflib.Wff{
                        self.wff_builder.buildWff(self.allocator, cond1) catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
                    } };
                } else {
                    break :ret InferenceRule.Conditions{ .two = [2]wfflib.Wff{
                        self.wff_builder.buildWff(self.allocator, cond1) catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
                        self.wff_builder.buildWff(self.allocator, cond2) catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
                    } };
                }
            };

            return InferenceRule{
                .allocator = self.allocator,
                .conditions = conditions,
                .result = self.wff_builder.buildWff(self.allocator, result) catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
                .num = self.count,
            };
        }
    };
    var maker = Maker{ .allocator = allocator, .wff_builder = wff_builder };
    return [_]InferenceRule{
        maker.makeRule("p", "", "(p v q)"),
        maker.makeRule("(p ^ q)", "", "p"),
        maker.makeRule("p", "(p => q)", "q"),
        maker.makeRule("~q", "(p => q)", "~p"),
        maker.makeRule("(p => q)", "(q => r)", "(p => r)"),
        maker.makeRule("p", "q", "(p ^ q)"),
    };
}
