const std = @import("std");
const debug = std.debug;

const wfflib = @import("wff.zig");
const rules = @import("rules.zig");
const ParsingConfig = @import("wff-parsing.zig").NewParsing;
// const step_builder = @import("step-builder.zig");

const WffParser = ParsingConfig.WffBuilder;

const ProofError = error{
    ProofMethodError,

    StepNumberInFuture,
    UnknownEquivalenceRule,
    UnknownInferenceRule,
};

pub const Proof = struct {
    const Self = @This();

    pub const MethodType = enum {
        none,
        direct,
        indirect,
        contradiction,

        pub fn getString(self: MethodType) []const u8 {
            return switch (self) {
                .none => "none",
                .direct => "direct",
                .indirect => "indirect",
                .contradiction => "contradiction",
            };
        }

        pub fn fromString(string: []const u8) ?MethodType {
            if (std.ascii.eqlIgnoreCase(string, "none")) {
                return .none;
            } else if (std.ascii.eqlIgnoreCase(string, "direct")) {
                return .direct;
            } else if (std.ascii.eqlIgnoreCase(string, "indirect")) {
                return .indirect;
            } else if (std.ascii.eqlIgnoreCase(string, "contradiction")) {
                return .contradiction;
            } else {
                return null;
            }
        }
    };

    const Method = union(MethodType) {
        none,
        direct: wfflib.Wff,
        indirect: wfflib.Wff,
        contradiction: wfflib.Wff,

        fn getString(self: Method) []const u8 {
            return @as(MethodType, self).getString();
        }
    };

    pub const Step = struct {
        pub const Justification = union(enum) {
            equivalence: struct {
                rule_index: usize,
                from: usize,
            },
            inference: struct {
                rule_index: usize,
                from: []const usize,
            },
            // Theorem: struct {
            //     wff: *w.Wff,
            //     from: ?*Step,
            // },
            assumption,
            hypothesis,
            axiom,

            pub fn deinit(self: Justification, allocator: std.mem.Allocator) void {
                switch (self) {
                    .inference => |inference| allocator.free(inference.from),
                    else => {},
                }
            }

            pub fn buildString(self: Justification, allocator: std.mem.Allocator) ![]const u8 {
                switch (self) {
                    .equivalence => |equivalence| return try std.fmt.allocPrint(allocator, "{d}, E{d}", .{ equivalence.from + 1, equivalence.rule_index + 1 }),
                    .inference => |inference| {
                        var string = std.ArrayList(u8).init(allocator);
                        for (inference.from) |step_number| {
                            try string.appendSlice(try std.fmt.allocPrint(allocator, "{d}, ", .{step_number + 1}));
                        }
                        try string.appendSlice(try std.fmt.allocPrint(allocator, "I{d}", .{inference.rule_index + 1}));
                        return try string.toOwnedSlice();
                    },
                    .assumption => return try allocator.dupe(u8, "From Gamma"),
                    .hypothesis => return try allocator.dupe(u8, "Hypothesis"),
                    .axiom => return try allocator.dupe(u8, "Axiom"),
                }
            }
        };

        allocator: std.mem.Allocator,
        wff: wfflib.Wff,
        justification: Justification,

        pub fn deinit(self: Step, deinit_wff: bool) void {
            if (deinit_wff) {
                self.wff.deinit();
            }
            self.justification.deinit(self.allocator);
        }
    };

    allocator: std.mem.Allocator,
    equivalence_rules: []const rules.EquivalenceRule,
    inference_rules: []const rules.InferenceRule,
    axiom_wff: wfflib.Wff,
    assumptions: std.ArrayList(wfflib.Wff),
    proving_wff: *wfflib.Wff,
    method: Method,
    steps: std.ArrayList(Step),
    goal: wfflib.Wff,

    pub fn init(allocator: std.mem.Allocator, wff_builder: WffParser, wff: *wfflib.Wff, method_type: MethodType, assumption_wff_strings: ?[][]const u8, equivalence_rules: []const rules.EquivalenceRule, inference_rules: []const rules.InferenceRule) !Self {
        var assumptions = std.ArrayList(wfflib.Wff).init(allocator);
        errdefer {
            for (assumptions.items) |*a| {
                a.deinit();
            }
            assumptions.deinit();
        }

        if (assumption_wff_strings) |strings| {
            try assumptions.ensureTotalCapacityPrecise(strings.len + 1);
            for (strings) |assumption_string| {
                assumptions.appendAssumeCapacity(try wff_builder.buildWff(allocator, assumption_string));
            }
        } else {
            try assumptions.ensureTotalCapacityPrecise(1);
        }

        const axiom_wff = try wff_builder.buildWff(allocator, "(w v ~w)");
        errdefer axiom_wff.deinit();

        var method = Method{ .none = {} };
        errdefer switch (method) {
            .direct, .indirect, .contradiction => |hypothesis_wff| hypothesis_wff.deinit(),
            .none => {},
        };
        var goal = switch (method_type) {
            .none => try wff.copy(allocator),
            .direct => ret: {
                var template = try wff_builder.buildWff(allocator, "(p => q)");
                defer template.deinit();
                var match = try wff.match(allocator, template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                method = .{ .direct = (try match.getBuildWff(allocator, "p")).? };
                break :ret (try match.getBuildWff(allocator, "q")).?;
            },
            .indirect => ret: {
                var template = try wff_builder.buildWff(allocator, "(p => q)");
                defer template.deinit();
                var match = try wff.match(allocator, template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                var hypothesis_pattern = try wff_builder.buildWff(allocator, "~q");
                defer hypothesis_pattern.deinit();
                method = .{ .indirect = try match.replace(allocator, hypothesis_pattern) };

                var goal_pattern = try wff_builder.buildWff(allocator, "~p");
                defer goal_pattern.deinit();
                break :ret try match.replace(allocator, goal_pattern);
            },
            .contradiction => ret: {
                var template = try wff_builder.buildWff(allocator, "p");
                defer template.deinit();
                var match = try wff.match(allocator, template) orelse return ProofError.ProofMethodError;
                defer match.deinit();

                var hypothesis_pattern = try wff_builder.buildWff(allocator, "~p");
                defer hypothesis_pattern.deinit();
                method = .{ .contradiction = try match.replace(allocator, hypothesis_pattern) };

                break :ret try wff_builder.buildWff(allocator, "F");
            },
        };
        errdefer goal.deinit();

        return Self{
            .allocator = allocator,
            .equivalence_rules = equivalence_rules,
            .inference_rules = inference_rules,
            .axiom_wff = axiom_wff,
            .assumptions = assumptions,
            .proving_wff = wff,
            .method = method,
            .steps = std.ArrayList(Step).init(allocator),
            .goal = goal,
        };
    }

    pub fn deinit(self: Self, deinit_proving_wff: bool) void {
        if (deinit_proving_wff) {
            self.proving_wff.deinit();
            self.allocator.destroy(self.proving_wff);
        }
        for (self.assumptions.items) |wff| {
            wff.deinit();
        }
        self.assumptions.deinit();
        self.axiom_wff.deinit();
        switch (self.method) {
            .direct, .indirect, .contradiction => |wff| wff.deinit(),
            .none => {},
        }
        self.goal.deinit();
        for (self.steps.items) |step| {
            step.deinit(true);
        }
        self.steps.deinit();
    }

    pub fn checkNewStep(self: Self, allocator: std.mem.Allocator, step: Step) !bool {
        return self.checkStep(allocator, step, self.steps.items.len);
    }

    pub fn checkStep(self: Self, allocator: std.mem.Allocator, step: Step, step_index: usize) !bool {
        std.debug.assert(step_index <= self.steps.items.len);

        switch (step.justification) {
            .equivalence => |equivalence| {
                if (equivalence.rule_index >= self.equivalence_rules.len) {
                    return ProofError.UnknownEquivalenceRule;
                }
                if (equivalence.from >= step_index) {
                    return ProofError.StepNumberInFuture;
                }
                const rule = self.equivalence_rules[equivalence.rule_index];
                const source_wff = self.steps.items[equivalence.from].wff;
                return try rule.canTransform(allocator, source_wff, step.wff);
            },
            .inference => |inference| {
                if (inference.rule_index >= self.inference_rules.len) {
                    return ProofError.UnknownInferenceRule;
                }
                for (inference.from) |index| {
                    if (index >= step_index) {
                        return ProofError.StepNumberInFuture;
                    }
                }
                const rule = self.inference_rules[inference.rule_index];
                var source_wffs = try std.ArrayList(wfflib.Wff).initCapacity(allocator, inference.from.len);
                defer source_wffs.deinit();
                for (inference.from) |index| {
                    source_wffs.appendAssumeCapacity(self.steps.items[index].wff);
                }
                const source_wffs_slice = try source_wffs.toOwnedSlice();
                defer allocator.free(source_wffs_slice);
                return try rule.canTransform(allocator, source_wffs_slice, step.wff);
            },
            .assumption => {
                for (self.assumptions.items) |wff| {
                    if (step.wff.eql(wff)) {
                        return true;
                    }
                }
                return false;
            },
            .hypothesis => {
                switch (self.method) {
                    .direct, .indirect, .contradiction => |hypothesis_wff| return step.wff.eql(hypothesis_wff),
                    .none => return false,
                }
            },
            .axiom => {
                if (try step.wff.match(allocator, self.axiom_wff)) |match| {
                    match.deinit();
                    return true;
                }
                return false;
            },
        }
    }

    pub fn isComplete(self: Self, allocator: std.mem.Allocator) !bool {
        if (self.steps.items.len == 0) return false;
        for (self.steps.items, 0..) |step, i| {
            if (!(try self.checkStep(allocator, step, i))) {
                return false;
            }
        }
        return self.goal.eql(self.steps.items[self.steps.items.len - 1].wff);
    }

    pub fn appendStepUnchecked(self: *Self, step: Step) !void {
        try self.steps.append(step);
    }

    pub fn buildString(self: Self, allocator: std.mem.Allocator) ![]u8 {
        var string = std.ArrayList(u8).init(allocator);
        defer string.deinit();

        try string.appendSlice(try std.fmt.allocPrint(allocator, "Proving: {s}\nMethod: {s}\nAssumptions: ", .{ self.proving_wff.string, self.method.getString() }));

        if (self.assumptions.items.len == 0) {
            try string.appendSlice(try allocator.dupe(u8, "None\n"));
        } else {
            for (self.assumptions.items[0 .. self.assumptions.items.len - 1]) |assumption_wff| {
                try string.appendSlice(try std.fmt.allocPrint(allocator, "{s}, ", .{assumption_wff.string}));
            }
            try string.appendSlice(try std.fmt.allocPrint(allocator, "{s}\n", .{self.assumptions.items[self.assumptions.items.len - 1].string}));
        }
        try string.appendSlice(try std.fmt.allocPrint(allocator, "Goal: {s}\n", .{self.goal.string}));
        try string.appendSlice(try allocator.dupe(u8, "Proof:\n"));
        for (self.steps.items, 0..) |step, i| {
            const justification_string = try step.justification.buildString(allocator);

            try string.appendSlice(try std.fmt.allocPrint(allocator, "{d}. {s}      ", .{ i + 1, step.wff.string }));
            try string.appendSlice(justification_string);
            try string.append('\n');
        }

        return try string.toOwnedSlice();
    }
};

test "Proof of (a => (b => a))" {
    const wff_builder = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    var equivalence_rules = rules.initEquivalenceRules(allocator, wff_builder);
    defer for (equivalence_rules) |e| e.deinit();
    var inference_rules = rules.initInferenceRules(allocator, wff_builder);
    defer for (inference_rules) |i| i.deinit();

    var wff = try wff_builder.buildWff(allocator, "(a => (b => a))");
    defer wff.deinit();

    var proof = try Proof.init(
        allocator,
        wff_builder,
        &wff,
        Proof.MethodType.direct,
        null,
        &equivalence_rules,
        &inference_rules,
    );
    defer proof.deinit(false);

    try std.testing.expect(proof.assumptions.items.len == 0);
    var hypothesis = try wff_builder.buildWff(allocator, "a");
    defer hypothesis.deinit();
    try std.testing.expect(hypothesis.eql(proof.method.direct));

    var goal = try wff_builder.buildWff(allocator, "(b => a)");
    defer goal.deinit();
    try std.testing.expect(goal.eql(proof.goal));

    const step1 = Proof.Step{ .allocator = allocator, .wff = try wff_builder.buildWff(allocator, "a"), .justification = Proof.Step.Justification{ .hypothesis = {} } };
    try proof.steps.append(step1);
    try std.testing.expect(try proof.checkStep(allocator, step1, 0));
    try std.testing.expect(!try proof.isComplete(allocator));

    const step2 = Proof.Step{ .allocator = allocator, .wff = try wff_builder.buildWff(allocator, "(a v ~b)"), .justification = Proof.Step.Justification{ .inference = .{
        .rule_index = 0,
        .from = try allocator.dupe(usize, &[1]usize{0}),
    } } };
    try proof.steps.append(step2);
    try std.testing.expect(try proof.checkStep(allocator, step2, 1));
    try std.testing.expect(!try proof.isComplete(allocator));

    const step3 = Proof.Step{
        .allocator = allocator,
        .wff = try wff_builder.buildWff(allocator, "(~b v a)"),
        .justification = Proof.Step.Justification{
            .equivalence = .{
                .rule_index = 9, // E10
                .from = 1,
            },
        },
    };
    try proof.steps.append(step3);
    try std.testing.expect(try proof.checkStep(allocator, step3, 2));
    try std.testing.expect(!try proof.isComplete(allocator));

    const step4 = Proof.Step{ .allocator = allocator, .wff = try wff_builder.buildWff(allocator, "(b => a)"), .justification = Proof.Step.Justification{ .equivalence = .{
        .rule_index = 17,
        .from = 2,
    } } };
    try proof.steps.append(step4);
    try std.testing.expect(try proof.checkStep(allocator, step4, 3));

    try std.testing.expect(try proof.isComplete(allocator));
}

test "equivalence rules" {
    const wff_parser = ParsingConfig.wff_builder;
    const allocator = std.testing.allocator;

    const equivalence_rules = rules.initEquivalenceRules(allocator, wff_parser);
    defer for (equivalence_rules) |rule| rule.deinit();
}
