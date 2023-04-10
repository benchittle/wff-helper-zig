const std = @import("std");
const w = @import("wff.zig");
const parsing = @import("parsing.zig");
const debug = std.debug;



const ProofError = error {
    ProofMethodError,
};


const EquivalenceRule = struct {
    const Self = @This();

    lhs: w.Wff, 
    rhs: w.Wff,

    pub fn deinit(self: Self) void {
        self.lhs.deinit();
        self.rhs.deinit();
    }

    /// Returns true if a single application of the equivalence rule can 
    /// transform `from` to `to`.
    pub fn canTransform(self: Self, from: w.Wff, to: w.Wff) !bool {
        if (try from.matchAll(self.lhs)) |left_to_right| {
            defer {
                for (left_to_right.items) |*m| m.deinit();
                left_to_right.deinit();
            }
            for (left_to_right.items) |matches| {
                var result = try matches.replace(self.rhs);
                defer result.deinit();
                if (result.eql(to)) {
                    return true;
                }
            }
        }
        if (try from.matchAll(self.rhs)) |right_to_left| {
            defer {
                for (right_to_left.items) |*m| m.deinit();
                right_to_left.deinit();
            }
            for (right_to_left.items) |matches| {
                var result = try matches.replace(self.lhs);
                defer result.deinit();
                if (result.eql(to)) {
                    return true;
                }
            }
        }
        return false;
    }
};

test "EquivalenceRule.canTransform: (~p v q) to (p => q) using (~a v b) = (a => b)" {
    var from = try w.Wff.init(std.testing.allocator, "(~p v q)");
    defer from.deinit();
    var to = try w.Wff.init(std.testing.allocator, "(p => q)");
    defer to.deinit();
    var wrong = try w.Wff.init(
        std.testing.allocator,
        "(~q v p)");
    defer wrong.deinit();

    var rule = EquivalenceRule {
        .lhs = try w.Wff.init(std.testing.allocator, "(~a v b)"),
        .rhs = try w.Wff.init(std.testing.allocator, "(a => b)"),
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
    try std.testing.expect(!(try rule.canTransform(from, wrong)));
    try std.testing.expect(!(try rule.canTransform(wrong, to)));

}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((w v x) ^ (~y v z)) using (~a v b) = (a => b)" {
    var from = try w.Wff.init(std.testing.allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try w.Wff.init(std.testing.allocator, "((w v x) ^ (~y v z))");
    defer to.deinit();

    var rule = EquivalenceRule {
        .lhs = try w.Wff.init(std.testing.allocator, "(~a v b)"),
        .rhs = try w.Wff.init(std.testing.allocator, "(a => b)"),
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((y => z) ^ (w v x)) using (a ^ b) = (b ^ a)" {
    var from = try w.Wff.init(std.testing.allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try w.Wff.init(std.testing.allocator, "((y => z) ^ (w v x))");
    defer to.deinit();

    var rule = EquivalenceRule {
        .lhs = try w.Wff.init(std.testing.allocator, "(a ^ b)"),
        .rhs = try w.Wff.init(std.testing.allocator, "(b ^ a)"),
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
}

test "EquivalenceRule.canTransform: ((y <=> z) ^ (w v x)) to (((y <=> z) ^ w) v ((y <=> z) ^ x)) using (a ^ (b v c)) = ((a ^ b) v (a ^ c))" {
    var from = try w.Wff.init(std.testing.allocator, "((y <=> z) ^ (w v x))");
    defer from.deinit();
    var to = try w.Wff.init(std.testing.allocator, "(((y <=> z) ^ w) v ((y <=> z) ^ x))");
    defer to.deinit();

    const rules = initEquivalenceRules(std.testing.allocator);
    defer for (rules) |r| r.deinit();
    const e13 = rules[12];

    try std.testing.expect(try e13.canTransform(from, to));
    try std.testing.expect(try e13.canTransform(to, from));
}

fn initEquivalenceRules(allocator: std.mem.Allocator) [20]EquivalenceRule {
    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,

        fn makeRule(self: Self, comptime lhs: []const u8, comptime rhs: []const u8) EquivalenceRule {
            return EquivalenceRule{
                .lhs = w.Wff.init(self.allocator, lhs)
                    catch {
                std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                }, 
                .rhs = w.Wff.init(self.allocator, rhs)
                    catch {
                std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                },
            };
        }
    };
    const maker = Maker{.allocator = allocator};
    return [_]EquivalenceRule {
        maker.makeRule("(a ^ ~a)",      "T"),
        maker.makeRule("(a v ~a)",      "F"),
        maker.makeRule("(a ^ a)",       "a"),
        maker.makeRule("(a v a)",       "a"),
        maker.makeRule("(a ^ T)",       "a"),
        maker.makeRule("(a v F)",       "a"),
        maker.makeRule("(a ^ F)",       "F"),
        maker.makeRule("(a v T)",       "T"),
        maker.makeRule("(a ^ b)",       "(b ^ a)"),
        maker.makeRule("(a v b)",       "(b v a)"),
        maker.makeRule("((a ^ b) ^ c)", "(a ^ (b ^ c))"),
        maker.makeRule("((a v b) v c)", "(a v (b v c))"),
        maker.makeRule("(a ^ (b v c))", "((a ^ b) v (a ^ c))"),
        maker.makeRule("(a v (b ^ c))", "((a v b) ^ (a v c))"),
        maker.makeRule("~~a",           "a"),
        maker.makeRule("~(a ^ b)",      "(~a v ~b)"),
        maker.makeRule("~(a v b)",      "(~a ^ ~b)"),
        maker.makeRule("(a => b)",      "(~a v b)"),
        maker.makeRule("(a => b)",      "(~b => ~a)"),
        maker.makeRule("((a => b) ^ (b => a))", "(a <=> b)"),
    };
}


const InferenceRule = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    conditions: []w.Wff,
    result: w.Wff,

    pub fn deinit(self: Self) void {
        for (self.conditions) |wff| wff.deinit();
        self.allocator.free(self.conditions);
        self.result.deinit();
    }

    pub fn canTransform(self: Self, from: []w.Wff, to: w.Wff) !bool {
        if (from.len != self.conditions.len) return false;
        var all_matches = parsing.MatchHashMap.init(self.allocator);
        defer all_matches.deinit();

        for (from) |wff, i| {
            var match = try wff.match(self.conditions[i]) orelse return false;
            defer match.deinit();

            var it = match.matches.iterator();
            while (it.next()) |entry| {
                if (all_matches.get(entry.key_ptr.*)) |existing_wff_node| {
                    if (!existing_wff_node.eql(entry.value_ptr.*)) return false;
                } else {
                    try all_matches.put(entry.key_ptr.*, entry.value_ptr.*);
                }
            }
        }

        var result_match = try to.match(self.result) orelse return false;
        defer result_match.deinit();
        var it = result_match.matches.iterator();
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
    var allocator = std.testing.allocator;
    var cond1 = try w.Wff.init(allocator, "p");
    defer cond1.deinit();
    var conds = [_]w.Wff {cond1};
    var result = try w.Wff.init(allocator, "(p v q)");
    defer result.deinit();
    var wrong = try w.Wff.init(allocator, "(q v p)");
    defer wrong.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
    try std.testing.expect(!(try infer1.canTransform(&conds, wrong)));
}

test "InferenceRule.canTransform: p to (p v (p => q)) using I1" {
    var allocator = std.testing.allocator;
    var cond1 = try w.Wff.init(allocator, "p");
    defer cond1.deinit();
    var conds = [_]w.Wff {cond1};
    var result = try w.Wff.init(allocator, "(p v (p => q))");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
}

test "InferenceRule.canTransform: (a ^ b) to ((a ^ b) v ~(a ^ b)) using I1" {
    var allocator = std.testing.allocator;
    var cond1 = try w.Wff.init(allocator, "(a ^ b)");
    defer cond1.deinit();
    var conds = [_]w.Wff {cond1};
    var result = try w.Wff.init(allocator, "((a ^ b) v ~(a ^ b))");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
}

test "InferenceRule.canTransform: (a ^ b), ((a ^ b) => ~x) to ~x using I3" {
    var allocator = std.testing.allocator;
    var cond1 = try w.Wff.init(allocator, "(a ^ b)");
    defer cond1.deinit();
    var cond2 = try w.Wff.init(allocator, "((a ^ b) => ~x)");
    defer cond2.deinit();
    var conds = [_]w.Wff {cond1, cond2};
    var result = try w.Wff.init(allocator, "~x");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer3 = rules[2];

    try std.testing.expect(try infer3.canTransform(&conds, result));
}

fn initInferenceRules(allocator: std.mem.Allocator) [6]InferenceRule {
    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,

        fn makeRule(self: Self, comptime cond1: []const u8, comptime cond2: []const u8, comptime result: []const u8) InferenceRule {
            const count = if (cond2.len == 0) 1 else 2;
            var conditions = self.allocator.alloc(w.Wff, count)
                catch std.debug.panic("Failed to initialize inference rules!\n", .{});

            conditions[0] = w.Wff.init(self.allocator, cond1)
                catch std.debug.panic("Failed to initialize inference rules!\n", .{});
            if (count > 1) {
                conditions[1] = w.Wff.init(self.allocator, cond2)
                    catch std.debug.panic("Failed to initialize inference rules!\n", .{});
            }
            
            return InferenceRule{
                .allocator = self.allocator,
                .conditions = conditions,
                .result = w.Wff.init(self.allocator, result) 
                    catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
            };
        }
    };
    const maker = Maker{.allocator = allocator};
    return [_]InferenceRule {
        maker.makeRule("p",         "",         "(p v q)"),
        maker.makeRule("(p ^ q)",   "",         "p"),
        maker.makeRule("p",         "(p => q)", "q"),
        maker.makeRule("~q",        "(p => q)", "~p"),
        maker.makeRule("(p => q)",  "(q => r)", "(p => r)"),
        maker.makeRule("p",         "q",        "(p ^ q)"),
    };
}

const Proof = struct {
    const Self = @This();

    const Method = enum {
        None,
        Direct,
    };

    const Justification = union(enum) {
        Equivalence: struct {
            rule: EquivalenceRule,
            from: *Step,
        },
        Inference: struct {
            rule: InferenceRule,
            from: [2]?*Step,
        },
        // Theorem: struct {
        //     wff: *w.Wff,
        //     from: ?*Step,
        // },
        // Assumption: *w.Wff,
        // True, // is True needed?
    };

    const Step = struct {
        wff: w.Wff,
        how: Justification, 

        pub fn isValid(self: Step) bool {
            switch(self.how) {
                .Equivalence => |e| {
                    return e.rule.canTransform(e.from, self.wff);
                },
                .Inference => |i| {
                    var conditions: [2]w.Wff = undefined;
                    inline for (i.from) |wff, j| conditions[j] = wff;
                    const count = if (i.from[1] == null) 1 else 2;
                    return i.rule.canTransform(conditions[0..count], self.wff);
                }
            }
        }
    };


    wff: w.Wff,
    method: Method,
    assumptions: std.ArrayList(w.Wff),
    goal: w.Wff,
    steps: std.ArrayList(Step),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, wff_string: []const u8, method: Method, assumption_wff_strings: ?[][]const u8) !Self {
        var wff = try w.Wff.init(allocator, wff_string);
        errdefer wff.deinit();
        var assumptions = std.ArrayList(w.Wff).init(allocator);
        errdefer {
            for (assumptions.items) |*a| {
                a.deinit();
            }
            assumptions.deinit();
        }

        if (assumption_wff_strings) |strings| {
            try assumptions.ensureTotalCapacityPrecise(strings.len + 1);
            for (strings) |assumption_string| {
                assumptions.appendAssumeCapacity(try w.Wff.init(allocator, assumption_string));
            }
        } else {
            try assumptions.ensureTotalCapacityPrecise(1);
        }

        var goal = switch(method) {
            .None => try wff.copy(),
            .Direct => ret: {
                var direct = try w.Wff.init(allocator, "(p => q)");
                defer direct.deinit();
                var match = try wff.match(direct) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                try assumptions.append((try match.getBuildWff(allocator, "p")).?);
                break :ret (try match.getBuildWff(allocator, "q")).?;
            }
        };
        errdefer goal.deinit();
        
        return Proof {
            .wff = wff,
            .method = method,
            .assumptions = assumptions,
            .goal = goal,
            .steps = std.ArrayList(Step).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.wff.deinit();
        for (self.assumptions.items) |*wff| {
            wff.deinit();
        }
        self.assumptions.deinit();
        self.goal.deinit();
        for (self.steps.items) |*step| {
            step.wff.deinit();
        }
        self.steps.deinit();
    }

};

test "proof: init" {
    var proof = try Proof.init(
        std.testing.allocator,
        "(a => (b => a))",
        Proof.Method.Direct,
        null,
    );

    proof.deinit();
}


test "equivalence rules" {
    var allocator = std.testing.allocator;
    const equivalence_rules = initEquivalenceRules(allocator);
    defer for (equivalence_rules) |rule| rule.deinit();
}