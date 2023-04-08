const std = @import("std");
const w = @import("wff.zig");
const parsing = @import("parsing.zig");
const debug = std.debug;

const ProofError = error {
    ProofMethodError,
};

const EquivalenceRule = enum {
    E1,  E2,  E3,  E4,  E5,  E6,  E7,  E8,  E9,  E10,
    E11, E12, E13, E14, E15, E16, E17, E18, E19, E20,
};

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
        Inference: struct {
            rule: InferenceRule,
            from: [2]*Step,
        },
        Theorem: struct {
            wff: *w.Wff,
            from: ?*Step,
        },
        Assumption: *w.Wff,
        True, // is True needed?
    };

    const Step = struct {
        wff: w.Wff,
        how: Justification, 
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

                try assumptions.append(try w.Wff.initFromNode(allocator, match.get("p").?));
                break :ret try w.Wff.initFromNode(allocator, match.get("q").?);
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

    // pub fn tryNext(self: *Self, step: Step) !bool {

    // }
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