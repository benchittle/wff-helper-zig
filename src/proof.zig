const std = @import("std");
const wfflib = @import("wff.zig");
const parsing = @import("wff-parsing.zig");
const debug = std.debug;

const ProofError = error {
    ProofMethodError,
    InferenceRuleError,
};


const EquivalenceRule = struct {
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
    pub fn canTransform(self: Self, from: Wff, to: Wff) !bool {
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
    const Wff = wfflib.Wff;
    var from = try Wff.init(std.testing.allocator, "(~p v q)");
    defer from.deinit();
    var to = try Wff.init(std.testing.allocator, "(p => q)");
    defer to.deinit();
    var wrong = try Wff.init(
        std.testing.allocator,
        "(~q v p)");
    defer wrong.deinit();

    var rule = EquivalenceRule {
        .lhs = try Wff.init(std.testing.allocator, "(~a v b)"),
        .rhs = try Wff.init(std.testing.allocator, "(a => b)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
    try std.testing.expect(!(try rule.canTransform(from, wrong)));
    try std.testing.expect(!(try rule.canTransform(wrong, to)));

}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((w v x) ^ (~y v z)) using (~a v b) = (a => b)" {
    const Wff = wfflib.Wff;
    var from = try Wff.init(std.testing.allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try Wff.init(std.testing.allocator, "((w v x) ^ (~y v z))");
    defer to.deinit();

    var rule = EquivalenceRule {
        .lhs = try Wff.init(std.testing.allocator, "(~a v b)"),
        .rhs = try Wff.init(std.testing.allocator, "(a => b)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
}

test "EquivalenceRule.canTransform: ((w v x) ^ (y => z)) to ((y => z) ^ (w v x)) using (a ^ b) = (b ^ a)" {
    const Wff = wfflib.Wff;
    var from = try Wff.init(std.testing.allocator, "((w v x) ^ (y => z))");
    defer from.deinit();
    var to = try Wff.init(std.testing.allocator, "((y => z) ^ (w v x))");
    defer to.deinit();

    var rule = EquivalenceRule {
        .lhs = try Wff.init(std.testing.allocator, "(a ^ b)"),
        .rhs = try Wff.init(std.testing.allocator, "(b ^ a)"),
        .num = undefined,
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
    try std.testing.expect(try rule.canTransform(to, from));
}

test "EquivalenceRule.canTransform: ((y <=> z) ^ (w v x)) to (((y <=> z) ^ w) v ((y <=> z) ^ x)) using (a ^ (b v c)) = ((a ^ b) v (a ^ c))" {
    const Wff = wfflib.Wff;
    var from = try Wff.init(std.testing.allocator, "((y <=> z) ^ (w v x))");
    defer from.deinit();
    var to = try Wff.init(std.testing.allocator, "(((y <=> z) ^ w) v ((y <=> z) ^ x))");
    defer to.deinit();

    const rules = initEquivalenceRules(std.testing.allocator);
    defer for (rules) |r| r.deinit();
    const e13 = rules[12];

    try std.testing.expect(try e13.canTransform(from, to));
    try std.testing.expect(try e13.canTransform(to, from));
}

pub fn initEquivalenceRules(allocator: std.mem.Allocator) [22]EquivalenceRule {
    //const EquivalenceRuleType = EquivalenceRule;
    const Wff = wfflib.Wff;
    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        count: usize = 0,

        fn makeRule(self: *Self, comptime lhs: []const u8, comptime rhs: []const u8) EquivalenceRule {
            self.count += 1;
            return EquivalenceRule{
                .lhs = Wff.init(self.allocator, lhs)
                    catch {
                std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                }, 
                .rhs = Wff.init(self.allocator, rhs)
                    catch {
                std.debug.panic("Failed to initialize equivalence rules!\n", .{});
                },
                .num = self.count,
            };
        }
    };
    var maker = Maker{.allocator = allocator};
    return [_]EquivalenceRule {
        maker.makeRule("(a v ~a)",      "T"),
        maker.makeRule("(a ^ ~a)",      "F"),
        maker.makeRule("(a ^ a)",       "a"),
        maker.makeRule("(a v a)",       "a"),
        maker.makeRule("(a ^ T)",       "a"),
        maker.makeRule("(a v F)",       "a"),
        maker.makeRule("(a ^ F)",       "F"),
        maker.makeRule("(a v T)",       "T"),
        maker.makeRule("(a ^ b)",       "(b ^ a)"),
        maker.makeRule("(a v b)",       "(b v a)"), // E10
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
        maker.makeRule("T",              "~F"),
        maker.makeRule("~T",             "F")
    };
}


const InferenceRule = struct {
    const Self = @This();
    const Wff = wfflib.Wff;

    allocator: std.mem.Allocator,
    conditions: []Wff,
    result: Wff,
    num: usize,

    pub fn deinit(self: Self) void {
        for (self.conditions) |wff| wff.deinit();
        self.allocator.free(self.conditions);
        self.result.deinit();
    }

    pub fn canTransform(self: Self, from: []Wff, to: Wff) !bool {
        if (from.len != self.conditions.len) return ProofError.InferenceRuleError;
        var all_matches = parsing.MatchHashMap.init(self.allocator);
        defer all_matches.deinit();

        for (from, 0..) |wff, i| {
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
    const Wff = wfflib.Wff;
    const allocator = std.testing.allocator;
    var cond1 = try Wff.init(allocator, "p");
    defer cond1.deinit();
    var conds = [_]Wff {cond1};
    var result = try Wff.init(allocator, "(p v q)");
    defer result.deinit();
    var wrong = try Wff.init(allocator, "(q v p)");
    defer wrong.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
    try std.testing.expect(!(try infer1.canTransform(&conds, wrong)));
}

test "InferenceRule.canTransform: p to (p v (p => q)) using I1" {
    const Wff = wfflib.Wff;
    const allocator = std.testing.allocator;
    var cond1 = try Wff.init(allocator, "p");
    defer cond1.deinit();
    var conds = [_]Wff {cond1};
    var result = try Wff.init(allocator, "(p v (p => q))");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
}

test "InferenceRule.canTransform: (a ^ b) to ((a ^ b) v ~(a ^ b)) using I1" {
    const Wff = wfflib.Wff;
    const allocator = std.testing.allocator;
    var cond1 = try Wff.init(allocator, "(a ^ b)");
    defer cond1.deinit();
    var conds = [_]Wff {cond1};
    var result = try Wff.init(allocator, "((a ^ b) v ~(a ^ b))");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer1 = rules[0];

    try std.testing.expect(try infer1.canTransform(&conds, result));
}

test "InferenceRule.canTransform: (a ^ b), ((a ^ b) => ~x) to ~x using I3" {
    const Wff = wfflib.Wff;
    const allocator = std.testing.allocator;
    var cond1 = try Wff.init(allocator, "(a ^ b)");
    defer cond1.deinit();
    var cond2 = try Wff.init(allocator, "((a ^ b) => ~x)");
    defer cond2.deinit();
    var conds = [_]Wff {cond1, cond2};
    var result = try Wff.init(allocator, "~x");
    defer result.deinit();

    const rules = initInferenceRules(allocator);
    defer for (rules) |r| r.deinit();
    const infer3 = rules[2];

    try std.testing.expect(try infer3.canTransform(&conds, result));
}

pub fn initInferenceRules(allocator: std.mem.Allocator) [6]InferenceRule {
    const InferenceRuleType = InferenceRule;
    const Wff = wfflib.Wff;
    // Helper struct
    const Maker = struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        count: usize = 0,

        fn makeRule(self: *Self, comptime cond1: []const u8, comptime cond2: []const u8, comptime result: []const u8) InferenceRuleType {
            self.count += 1;
            const count = if (cond2.len == 0) 1 else 2;
            var conditions = self.allocator.alloc(Wff, count)
                catch std.debug.panic("Failed to initialize inference rules!\n", .{});

            conditions[0] = Wff.init(self.allocator, cond1)
                catch std.debug.panic("Failed to initialize inference rules!\n", .{});
            if (count > 1) {
                conditions[1] = Wff.init(self.allocator, cond2)
                    catch std.debug.panic("Failed to initialize inference rules!\n", .{});
            }
            
            return InferenceRuleType{
                .allocator = self.allocator,
                .conditions = conditions,
                .result = Wff.init(self.allocator, result) 
                    catch std.debug.panic("Failed to initialize inference rules!\n", .{}),
                .num = self.count,
            };
        }
    };
    var maker = Maker{.allocator = allocator};
    return [_]InferenceRuleType {
        maker.makeRule("p",         "",         "(p v q)"),
        maker.makeRule("(p ^ q)",   "",         "p"),
        maker.makeRule("p",         "(p => q)", "q"),
        maker.makeRule("~q",        "(p => q)", "~p"),
        maker.makeRule("(p => q)",  "(q => r)", "(p => r)"),
        maker.makeRule("p",         "q",        "(p ^ q)"),
    };
}

pub const Proof = struct {
    const Self = @This();
    const Wff = wfflib.Wff;
    const EquivalenceRuleType = EquivalenceRule;
    const InferenceRuleType = InferenceRule;

    pub const Method = enum {
        None,
        Direct,
        Indirect,
        Contradiction,

        pub fn getString(self: Method) []const u8 {
            return switch(self) {
                .None => "None",
                .Direct => "Direct",
                .Indirect => "Indirect",
                .Contradiction => "Contradiction",
            };
        }
    };

    pub const Justification = union(enum) {
        Equivalence: struct {
            rule: EquivalenceRuleType,
            from: *Step,
        },
        Inference: struct {
            rule: InferenceRuleType,
            from: [2]?*Step,
        },
        // Theorem: struct {
        //     wff: *w.Wff,
        //     from: ?*Step,
        // },
        Assumption: []const Wff,
        True, // is True needed?
    };

    pub const Step = struct {
        wff: Wff,
        how: Justification, 

        pub fn isValid(self: Step) !bool {
            switch(self.how) {
                .Equivalence => |e| {
                    return try e.rule.canTransform(e.from.wff, self.wff);
                },
                .Inference => |i| {
                    var conditions: [2]Wff = undefined;
                    inline for (i.from, 0..) |step, j| conditions[j] = if (step) |s| s.wff else undefined;
                    const count: u32 = if (i.from[1] == null) 1 else 2;
                    return try i.rule.canTransform(conditions[0..count], self.wff);
                },
                .Assumption => |assumption_list| {
                    for (assumption_list) |wff| {
                        if (self.wff.eql(wff)) return true;
                    }
                    return false;
                },
                .True => return true,
            }
        }
    };


    allocator: std.mem.Allocator,
    equivalence_rules: []EquivalenceRuleType,
    inference_rules: []InferenceRuleType,
    wff: *Wff,
    method: Method,
    assumptions: std.ArrayList(Wff),
    goal: Wff,
    steps: std.ArrayList(Step),

    pub fn init(allocator: std.mem.Allocator, wff: *Wff, method: Method, assumption_wff_strings: ?[][]const u8, equivalence_rules: []EquivalenceRuleType, inference_rules: []InferenceRuleType) !Self {
        var assumptions = std.ArrayList(Wff).init(allocator);
        errdefer {
            for (assumptions.items) |*a| {
                a.deinit();
            }
            assumptions.deinit();
        }

        if (assumption_wff_strings) |strings| {
            try assumptions.ensureTotalCapacityPrecise(strings.len + 1);
            for (strings) |assumption_string| {
                assumptions.appendAssumeCapacity(try Wff.init(allocator, assumption_string));
            }
        } else {
            try assumptions.ensureTotalCapacityPrecise(1);
        }

        var goal = switch(method) {
            .None => try wff.copy(),
            .Direct => ret: {
                var template = try Wff.init(allocator, "(p => q)");
                defer template.deinit();
                var match = try wff.match(template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                try assumptions.append((try match.getBuildWff(allocator, "p")).?);
                break :ret (try match.getBuildWff(allocator, "q")).?;
            },
            .Indirect => ret: {
                var template = try Wff.init(allocator, "(p => q)");
                defer template.deinit();
                var match = try wff.match(template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                var assumption_pattern = try Wff.init(allocator, "~q");
                defer assumption_pattern.deinit();
                try assumptions.append((try match.replace(assumption_pattern)));

                var goal_pattern = try Wff.init(allocator, "~p");
                defer goal_pattern.deinit();
                break :ret try match.replace(goal_pattern);
            },
            .Contradiction => ret: {
                var template = try Wff.init(allocator, "p");
                defer template.deinit();
                var match = try wff.match(template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                var assumption_pattern = try Wff.init(allocator, "~p");
                defer assumption_pattern.deinit();
                try assumptions.append((try match.replace(assumption_pattern)));

                break :ret try Wff.init(allocator, "F");
            }
        };
        errdefer goal.deinit();
        
        return Self {
            .wff = wff,
            .equivalence_rules = equivalence_rules,
            .inference_rules = inference_rules,
            .method = method,
            .assumptions = assumptions,
            .goal = goal,
            .steps = std.ArrayList(Step).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        //self.wff.deinit();
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

    pub fn verify(self: Self) !bool {
        for (self.steps.items) |step| {
            if (!(try step.isValid())) {
                return false;
            }
        }
        if (self.steps.items.len == 0) return false
        else return self.goal.eql(self.steps.items[self.steps.items.len - 1].wff);
    }

    pub fn toString(self: Self, allocator: std.mem.Allocator) ![]u8 {
        var strings = std.ArrayList([]u8).init(allocator);
        defer {
            for (strings.items) |s| allocator.free(s);
            strings.deinit();
        }
        
        try strings.append(try std.fmt.allocPrint(allocator, "Proving: {s}\nMethod: {s}\nAssumptions: ", .{self.wff.string, self.method.getString()}));

        if (self.assumptions.items.len == 0) {
            try strings.append(try allocator.dupe(u8, "None\n"));
        } else {
            for (self.assumptions.items[0..self.assumptions.items.len - 1]) |assumption_wff| {
                try strings.append(try std.fmt.allocPrint(allocator, "{s}, ", .{assumption_wff.string}));
            }
            try strings.append(try std.fmt.allocPrint(allocator, "{s}\n", .{self.assumptions.items[self.assumptions.items.len - 1].string}));
        }
        try strings.append(try std.fmt.allocPrint(allocator, "Goal: {s}\n", .{self.goal.string}));
        try strings.append(try allocator.dupe(u8, "Proof:\n"));
        for (self.steps.items, 0..) |step, i| {
            var justification_string: []u8 = undefined;
            switch(step.how) {
                .Equivalence => |e| {
                    var step_num: usize = 0;
                    while (e.from != &self.steps.items[step_num]) {
                        step_num += 1;
                    }
                    justification_string = try std.fmt.allocPrint(allocator, "{d}, E{d}\n", .{step_num + 1, e.rule.num});
                },
                .Inference => |inf| {
                    if (inf.from[1] == null) {
                        var step_num: usize = 0;
                        while (inf.from[0] != &self.steps.items[step_num]) {
                            step_num += 1;
                        }
                        justification_string = try std.fmt.allocPrint(allocator, "{d}, I{d}\n", .{step_num + 1, inf.rule.num});                        
                    } else {
                        var step_num1: usize = 0;
                        while (inf.from[0] != &self.steps.items[step_num1]) {
                            step_num1 += 1;
                        }
                        var step_num2: usize = 0;
                        while (inf.from[1] != &self.steps.items[step_num2]) {
                            step_num2 += 1;
                        }
                        justification_string = try std.fmt.allocPrint(allocator, "{d}, {d}, I{d}\n", .{step_num1, step_num2, inf.rule.num});
                    }
                },
                .Assumption => justification_string = try allocator.dupe(u8, "Assumption\n"),
                .True => justification_string = try allocator.dupe(u8, "\n"),
            }

            try strings.append(try std.fmt.allocPrint(allocator, "{d}. {s}      ", .{i + 1, step.wff.string}));
            try strings.append(justification_string);
        }

        return try std.mem.concat(allocator, u8, strings.items);
    }
};


test "proof: (a => (b => a))" {
    const Wff = wfflib.Wff;
    const ProofType = Proof;
    const allocator = std.testing.allocator;

    var equivalence_rules = initEquivalenceRules(allocator);
    defer for (equivalence_rules) |e| e.deinit();
    var inference_rules = initInferenceRules(allocator);
    defer for (inference_rules) |i| i.deinit();

    var wff = try Wff.init(allocator, "(a => (b => a))");
    defer wff.deinit();

    var proof = try ProofType.init(
        allocator,
        &wff,
        ProofType.Method.Direct,
        null,
        &equivalence_rules,
        &inference_rules,
    );
    defer proof.deinit();


    try std.testing.expect(proof.assumptions.items.len == 1);
    var a1 = try Wff.init(allocator, "a");
    defer a1.deinit();
    try std.testing.expect(a1.eql(proof.assumptions.items[0]));

    var goal = try Wff.init(allocator, "(b => a)");
    defer goal.deinit();
    try std.testing.expect(goal.eql(proof.goal));


    var step1 = ProofType.Step{
        .wff = try Wff.init(allocator, "a"),
        .how = ProofType.Justification{
            .Assumption = proof.assumptions.items,
        }
    };
    try proof.steps.append(step1);
    try std.testing.expect(try step1.isValid());
    try std.testing.expect(!try proof.verify());

    var step2 = ProofType.Step{
        .wff = try Wff.init(allocator, "(a v ~b)"),
        .how = ProofType.Justification{
            .Inference = .{
                .rule = inference_rules[0],
                .from = .{&proof.steps.items[proof.steps.items.len - 1], null}
            }
        }
    };
    try proof.steps.append(step2);
    try std.testing.expect(try step2.isValid());
    try std.testing.expect(!try proof.verify());

    var step3 = ProofType.Step{
        .wff = try Wff.init(allocator, "(~b v a)"),
        .how = ProofType.Justification{
            .Equivalence = .{
                .rule = equivalence_rules[9],
                .from = &proof.steps.items[proof.steps.items.len - 1],
            }
        }
    };
    try proof.steps.append(step3);
    try std.testing.expect(try step3.isValid());
    try std.testing.expect(!try proof.verify());

    var step4 = ProofType.Step{
        .wff = try Wff.init(allocator, "(b => a)"),
        .how = ProofType.Justification{
            .Equivalence = .{
                .rule = equivalence_rules[17],
                .from = &proof.steps.items[proof.steps.items.len - 1],
            }
        }
    };
    try proof.steps.append(step4);
    try std.testing.expect(try step4.isValid());

    try std.testing.expect(try proof.verify());
}


test "equivalence rules" {
    const allocator = std.testing.allocator;
    const equivalence_rules = initEquivalenceRules(allocator);
    defer for (equivalence_rules) |rule| rule.deinit();
}