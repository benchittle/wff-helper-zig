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

    pub fn canTransform(self: Self, from: w.Wff, to: w.Wff) !bool {
        if (try from.matchAll(self.lhs)) |left_to_right| {
            defer {
                for (left_to_right.items) |*m| m.deinit();
                left_to_right.deinit();
            }
            for (left_to_right.items) |matches| {
                if (try matches.substitute(self.rhs)) |result| {
                    defer result.deinit();
                    if (result.eql(to)) return true;
                }
            }
        }
        if (try from.matchAll(self.rhs)) |right_to_left| {
            defer {
                for (right_to_left.items) |*m| m.deinit();
                right_to_left.deinit();
            }
            for (right_to_left.items) |matches| {
                if (try matches.substitute(self.lhs)) |result| {
                    defer result.deinit();
                    if (result.eql(to)) return true;
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

    var rule = EquivalenceRule {
        .lhs = try w.Wff.init(std.testing.allocator, "(~a v b)"),
        .rhs = try w.Wff.init(std.testing.allocator, "(a => b)"),
    };
    defer rule.deinit();
    try std.testing.expect(try rule.canTransform(from, to));
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


const InferenceRule = enum {
    I1, I2, I3, I4, I5, I6,
};

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
        // Inference: struct {
        //     rule: InferenceRule,
        //     from: [2]*Step,
        // },
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