const std = @import("std");

const wfflib = @import("wff.zig");
const WffParsingConfig = @import("wff-parsing.zig").NewParsing;
const prooflib = @import("proof.zig");

pub const StepLexingError = error{
    InvalidRuleNumber,
    InvalidStepNumber,
};

pub const StepParsingError = error{
    InvalidWff, 
    TooManyTokens, 
    UnexpectedToken, 
    InvalidStepNumber, 
    MissingStepNumber, 
    MissingWff, 
    MissingRule 
};

pub const StepBuildingError = error{ 
    InvalidEquivalenceRule, 
    InvalidInferenceRule, 
    InvalidStepNumber 
};


/// Remove leading and trailing whitespace from a string.
fn strStrip(ascii_string: []const u8) []const u8 {
    var start: usize = 0;
    for (ascii_string, 0..) |c, i| {
        if (!std.ascii.isWhitespace(c)) {
            start = i;
            break;
        }
    }
    var end: usize = start;
    for (ascii_string[start..], 0..) |c, i| {
        if (!std.ascii.isWhitespace(c)) {
            end = i + start + 1;
        }
    }

    return ascii_string[start..end];
}

pub const ParsedStep = struct {
    wff: wfflib.Wff,
    justification: JustificationData,

    /// allocator MUST be the same allocator used to allocate the memory
    pub fn deinit(self: @This(), allocator: std.mem.Allocator, deinit_wff: bool) void {
        if (deinit_wff) {
            self.wff.deinit();
        }
        self.justification.deinit(allocator);
    }
};

const JustificationType = enum {
    equivalence,
    inference,
    hypothesis,
    assumption,
    axiom,
};

pub const JustificationData = union(JustificationType) {
    equivalence: struct {
        rule_number: usize,
        from: usize,
    },
    inference: struct {
        rule_number: usize,
        from: []const usize,
    },
    hypothesis,
    assumption,
    axiom,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            .inference => |step_numbers| allocator.free(step_numbers.from),
            else => {},
        }
    }
};

const Token = union(enum) {
    wff: []const u8,
    step_number: usize,
    equivalence_rule: usize,
    inference_rule: usize,
    hypothesis,
    assumption,
    axiom,
};

fn tokenize(allocator: std.mem.Allocator, step_string: []const u8) ![]const Token {
    var token_list = std.ArrayList(Token).init(allocator);
    errdefer {
        token_list.deinit();
    }

    var it = std.mem.splitAny(u8, step_string, ",");
    while (it.next()) |slice| {
        const stripped = strStrip(slice);
        if (stripped.len == 0) {
            continue;
        }
        const token = ret: {
            // Equivalence rule
            if (stripped[0] == 'E' or stripped[0] == 'e') {
                if (std.fmt.parseInt(usize, stripped[1..], 10)) |rule_num| {
                    break :ret Token{ .equivalence_rule = rule_num };
                } else |err| switch (err) {
                    error.Overflow, error.InvalidCharacter => return StepLexingError.InvalidRuleNumber,
                }
            // Inference rule
            } else if (stripped[0] == 'I' or stripped[0] == 'i') {
                if (std.fmt.parseInt(usize, stripped[1..], 10)) |rule_num| {
                    break :ret Token{ .inference_rule = rule_num };
                } else |err| switch (err) {
                    error.Overflow, error.InvalidCharacter => return StepLexingError.InvalidRuleNumber,
                }
            // Step number
            } else if (std.ascii.isDigit(stripped[0])) {
                if (std.fmt.parseInt(usize, stripped, 10)) |step_num| {
                    break :ret Token{ .step_number = step_num };
                } else |err| switch (err) {
                    error.Overflow, error.InvalidCharacter => return StepLexingError.InvalidStepNumber,
                }
            // Axiom
            } else if (std.ascii.eqlIgnoreCase(stripped, "axiom")) {
                break :ret Token{ .axiom = {} };
            // Assumption
            } else if (std.ascii.eqlIgnoreCase(stripped, "assumption") or std.ascii.eqlIgnoreCase(stripped, "from gamma")) {
                break :ret Token{ .assumption = {} };
            // Hypothesis
            } else if (std.ascii.eqlIgnoreCase(stripped, "hypothesis")) {
                break :ret Token{ .hypothesis = {} };
            // Wff
            } else {
                break :ret Token{ .wff = stripped };
            }
        };

        try token_list.append(token);
    }
    return try token_list.toOwnedSlice();
}

test "tokenize: 'b v a, 1, E10'" {
    const allocator = std.testing.allocator;

    const step_string = "b v a, 1, E10";

    const expected_list = &[_]Token{
        Token{ .wff = step_string[0..5] },
        Token{ .step_number = 1 },
        Token{ .equivalence_rule = 10 },
    };
    const token_list = try tokenize(allocator, step_string);
    defer allocator.free(token_list);

    try std.testing.expectEqualSlices(Token, expected_list, token_list);
}

test "tokenize: 'gabagool, 100, E100'" {
    const allocator = std.testing.allocator;

    const step_string = "gabagool, 100, E100";

    const expected_list = &[_]Token{
        Token{ .wff = step_string[0..8] },
        Token{ .step_number = 100 },
        Token{ .equivalence_rule = 100 },
    };
    const token_list = try tokenize(allocator, step_string);
    defer allocator.free(token_list);

    try std.testing.expectEqualSlices(Token, expected_list, token_list);
}

test "tokenize: '7, 9, E6'" {
    const allocator = std.testing.allocator;

    const step_string = "7, 9, E6";

    const expected_list = &[_]Token{
        Token{ .step_number = 7 },
        Token{ .step_number = 9 },
        Token{ .equivalence_rule = 6 },
    };
    const token_list = try tokenize(allocator, step_string);
    defer allocator.free(token_list);

    try std.testing.expectEqualSlices(Token, expected_list, token_list);
}

test "tokenize: 'b v a, 1, E 10'" {
    const allocator = std.testing.allocator;

    const step_string = "b v a, 1, E 10";
    try std.testing.expectError(StepLexingError.InvalidRuleNumber, tokenize(allocator, step_string));
}

fn parse(allocator: std.mem.Allocator, wff_builder: WffParsingConfig.WffBuilder, step_string: []const u8) !ParsedStep {
    const token_list = try tokenize(allocator, step_string);
    defer allocator.free(token_list);

    // First token must be a valid WFF
    const wff = switch (token_list[0]) {
        .wff => |wff_string| wff_builder.buildWff(allocator, wff_string) catch |err| switch (err) {
            error.InvalidSyntax, error.NoTokensFound, error.UnexpectedToken => return StepParsingError.InvalidWff,
            else => |leftover_err| return leftover_err,
        },
        else => return StepParsingError.MissingWff,
    };
    errdefer wff.deinit();

    // The type and number of remaining tokens depend on the type of the last
    // token. They will form the justification for the step.
    const justification = switch (token_list[token_list.len - 1]) {
        .axiom => .axiom,
        .hypothesis => .hypothesis,
        .assumption => .assumption,
        .equivalence_rule => |rule_num| ret: {
            // An equivalence rule step should have exactly 3 tokens
            if (token_list.len < 3) {
                return StepParsingError.MissingStepNumber;
            } else if (token_list.len > 3) {
                return StepParsingError.TooManyTokens;
            }
            // The middle token should be a step number
            switch (token_list[1]) {
                .step_number => |step_num| break :ret JustificationData{ .equivalence = .{ .from = step_num, .rule_number = rule_num } },
                else => return StepParsingError.InvalidStepNumber,
            }
        },
        .inference_rule => |rule_num| ret: {
            // An inference rule step should have at least 3 tokens
            if (token_list.len < 3) {
                return StepParsingError.MissingStepNumber;
            }

            var step_numbers = try std.ArrayList(usize).initCapacity(allocator, token_list.len - 2);
            errdefer step_numbers.deinit();

            // The middle tokens should all be step numbers
            for (token_list[1..(token_list.len - 1)]) |token| {
                switch (token) {
                    .step_number => |step_num| step_numbers.appendAssumeCapacity(step_num),
                    else => return StepParsingError.InvalidStepNumber,
                }
            }
            break :ret JustificationData{ .inference = .{ .rule_number = rule_num, .from = try step_numbers.toOwnedSlice() } };
        },
        .wff => return StepParsingError.UnexpectedToken,
        .step_number => return StepParsingError.MissingRule,
    };

    return ParsedStep{ .wff = wff, .justification = justification };
}

test "parse: 'b v a, 1, E10'" {
    const allocator = std.testing.allocator;
    const wff_builder = WffParsingConfig.wff_builder;

    const step_string = "b v a, 1, E10";

    const expected = ParsedStep{ .wff = try wff_builder.buildWff(allocator, "b v a"), .justification = .{ .equivalence = .{ .from = 1, .rule_number = 10 } } };
    defer expected.wff.deinit();
    const step = try parse(allocator, wff_builder, step_string);
    defer step.wff.deinit();

    try std.testing.expect(expected.wff.eql(step.wff));
    try std.testing.expectEqual(expected.justification, step.justification);
}

/// Convert a string representation of a step in the proof to an internal
/// representation.
/// Returns an error if the step is syntactically invalid. Does not check
/// whether the step is semantically or logically valid within a proof. Use
/// Proof.checkNewStep for this.
pub fn buildStep(allocator: std.mem.Allocator, wff_builder: WffParsingConfig.WffBuilder, step_string: []const u8) !prooflib.Proof.Step {
    const parsed_step = try parse(allocator, wff_builder, step_string);
    defer parsed_step.deinit(allocator, false);
    errdefer parsed_step.wff.deinit();

    const justification = switch (parsed_step.justification) {
        .equivalence => |equivalence| ret: {
            if (equivalence.rule_number == 0) {
                return StepBuildingError.InvalidEquivalenceRule;
            }
            if (equivalence.from == 0) {
                return StepBuildingError.InvalidStepNumber;
            }
            break :ret prooflib.Proof.Step.Justification{ .equivalence = .{ .rule_index = equivalence.rule_number - 1, .from = equivalence.from - 1 } };
        },
        .inference => |inference| ret: {
            if (inference.rule_number == 0) {
                return StepBuildingError.InvalidInferenceRule;
            }
            for (inference.from) |step_number| {
                if (step_number == 0) {
                    return StepBuildingError.InvalidStepNumber;
                }
            }
            const source_steps = try allocator.dupe(usize, inference.from);
            errdefer allocator.free(source_steps);

            for (source_steps) |*step_number| {
                step_number.* = step_number.* - 1;
            }
            break :ret prooflib.Proof.Step.Justification{ .inference = .{ .rule_index = inference.rule_number - 1, .from = source_steps } };
        },
        .assumption => prooflib.Proof.Step.Justification{ .assumption = {} },
        .hypothesis => prooflib.Proof.Step.Justification{ .hypothesis = {} },
        .axiom => prooflib.Proof.Step.Justification{ .axiom = {} },
    };
    return prooflib.Proof.Step{
        .allocator = allocator,
        .wff = parsed_step.wff,
        .justification = justification,
    };
}

test "buildStep: a v f, 1, I1" {
    const allocator = std.testing.allocator;
    const wff_builder = WffParsingConfig.wff_builder;

    const step_string = "a v f, 1, I1";

    const expected = prooflib.Proof.Step{ 
        .allocator = allocator, 
        .wff = try wff_builder.buildWff(allocator, "a v f"), 
        .justification = .{ 
            .inference = .{ 
                .from = &[_]usize{0}, 
                .rule_index = 0 
            } 
        }
    };
    defer expected.wff.deinit();
    const step = try buildStep(allocator, wff_builder, step_string);
    defer step.deinit(true);

    try std.testing.expect(expected.wff.eql(step.wff));
    try std.testing.expectEqualDeep(expected.justification, step.justification);
}

test "buildStep: a => b, from gamma" {
    const allocator = std.testing.allocator;
    const wff_builder = WffParsingConfig.wff_builder;

    const step_string = "a => b, from gamma";

    const expected = prooflib.Proof.Step{ 
        .allocator = allocator, 
        .wff = try wff_builder.buildWff(allocator, "a => b"), 
        .justification = .{ .assumption = {} },
    };
    defer expected.wff.deinit();
    const step = try buildStep(allocator, wff_builder, step_string);
    defer step.deinit(true);

    try std.testing.expect(expected.wff.eql(step.wff));
    try std.testing.expectEqualDeep(expected.justification, step.justification);
}