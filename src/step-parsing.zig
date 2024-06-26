const std = @import("std");
const wfflib = @import("wff.zig");
const proofs = @import("proof.zig");

const StepParsingError = error {
    EmptyString,
    InvalidWff,
    UnexpectedToken,
    InvalidStepNumber,
    InvalidRuleNumber,
    NoTokensFound,

    InvalidFormat,
    InvalidEquivalenceRule,
    InvalidInferenceRule,
    TooManySteps,
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

const StepToken = union(enum) {
    Wff: wfflib.Wff,
    StepNumber: usize,
    Justification: union(enum) {
        Equivalence: usize,
        Inference: usize,
        Assumption,
    },
};


pub fn tokenizeJustification(string: []const u8) !StepToken {
    if (std.ascii.eqlIgnoreCase("assumption", string)) {
        return StepToken{.Justification = .Assumption};
    } else if (string[0] == 'E' or string[0] == 'e') {
        if (std.fmt.parseInt(usize, string[1..], 10)) |rule_num| {
            return StepToken{.Justification = .{.Equivalence = rule_num}};
        } else |err| switch(err) {
            error.Overflow => return StepParsingError.InvalidRuleNumber,
            error.InvalidCharacter => return StepParsingError.UnexpectedToken,
        }
    } else if (string[0] == 'I' or string[0] == 'i') {
        if (std.fmt.parseInt(usize, string[1..], 10)) |rule_num| {
            return StepToken{.Justification = .{.Inference = rule_num}};
        } else |err| switch(err) {
            error.Overflow => return StepParsingError.InvalidRuleNumber,
            error.InvalidCharacter => return StepParsingError.UnexpectedToken,
        }
    } else {
        return StepParsingError.UnexpectedToken;
    }
}


pub fn tokenizeStep(allocator: std.mem.Allocator, wff_parser: wfflib.WffParser, step_string: []const u8) !std.ArrayList(StepToken) {
    if (step_string.len == 0) {
        return StepParsingError.EmptyString;
    }    

    var tokens = std.ArrayList(StepToken).init(allocator);
    errdefer tokens.deinit();
    
    var it = std.mem.split(u8, step_string, ",");
    const wff_string = it.next().?;
    var wff = wff_parser.parse(allocator, wff_string) catch |err| switch(err) {
        error.OutOfMemory => return err,
        else => return StepParsingError.InvalidWff,
    };
    errdefer wff.deinit();
    try tokens.append(StepToken{.Wff = wff});

    while (it.next()) |str| {
        const stripped = strStrip(str);
        if (stripped.len == 0) {
            return StepParsingError.UnexpectedToken;
        }
        if (std.fmt.parseInt(usize, stripped, 10)) |step_num| {
            try tokens.append(StepToken{.StepNumber = step_num});
        } else |err| switch(err) {
            error.Overflow => {
                return StepParsingError.InvalidStepNumber;
            },
            error.InvalidCharacter => {
                const justification = try tokenizeJustification(stripped);
                try tokens.append(justification);
            },
        }
    }

    if (tokens.items.len == 0) {
        tokens.deinit();
        return StepParsingError.NoTokensFound;
    } else {
        return tokens;
    }
}


pub fn parseStep(allocator: std.mem.Allocator, wff_parser: wfflib.WffParser, step_string: []const u8, proof: proofs.Proof) !proofs.Proof.Step {
    var tokens = try tokenizeStep(allocator, wff_parser, step_string);
    defer tokens.deinit();
    errdefer for (tokens.items) |tok| {
        switch (tok) {
            .Wff => |wff| wff.deinit(),
            else => {},
        }
    };

    // First token must be wff
    var wff = switch(tokens.items[0]) {
        .Wff => |w| w,
        else => return StepParsingError.InvalidFormat,
    };

    // If wff is T, then there should be no other tokens.
    var wff_true = try wff_parser.parse(allocator, "T");
    defer wff_true.deinit();
    if (wff.eql(wff_true)) {
        if (tokens.items.len != 1) {
            return StepParsingError.InvalidFormat;
        }
        return proofs.Proof.Step{
            .wff = wff, 
            .how = .True,
        };
    }

    // Otherwise, last token must be justification
    const justification_data = switch(tokens.items[tokens.items.len - 1]) {
        .Justification => |data| data,
        else => return StepParsingError.InvalidFormat,
    };

    const justification = switch(justification_data) {
        .Equivalence => |rule_num| ret: {
            if (rule_num < 1 or rule_num > proof.equivalence_rules.len) {
                return StepParsingError.InvalidEquivalenceRule;
            }
            if (tokens.items.len != 3) {
                return StepParsingError.TooManySteps;
            }
            const step = switch(tokens.items[1]) {
                .StepNumber => |num| ret2: {
                    if (num < 1 or num > proof.steps.items.len) {
                        return StepParsingError.InvalidStepNumber;
                    }
                    break :ret2 &proof.steps.items[num - 1];
                },
                else => return StepParsingError.InvalidFormat,
            };
            break :ret proofs.Proof.Justification{
                .Equivalence = .{
                    .rule = proof.equivalence_rules[rule_num - 1],
                    .from = step,
                }
            };
        },
        .Inference => |rule_num| ret: {
            if (rule_num < 1 or rule_num > proof.inference_rules.len) {
                return StepParsingError.InvalidInferenceRule;
            }

            var fromSteps: proofs.Proof.Justification.InferenceSource = undefined;
            if (tokens.items.len == 3) {
                const step1 = switch(tokens.items[1]) {
                    .StepNumber => |num| ret2: {
                        if (num < 1 or num > proof.steps.items.len) {
                            return StepParsingError.InvalidStepNumber;
                        }
                        break :ret2 &proof.steps.items[num - 1];
                    },
                    else => return StepParsingError.InvalidFormat,
                };
                fromSteps = .{
                    .one = [1]*proofs.Proof.Step {step1},
                };
            } else if (tokens.items.len == 4) {
                const step1 = switch(tokens.items[1]) {
                    .StepNumber => |num| ret2: {
                        if (num < 1 or num > proof.steps.items.len) {
                            return StepParsingError.InvalidStepNumber;
                        }
                        break :ret2 &proof.steps.items[num - 1];
                    },
                    else => return StepParsingError.InvalidFormat,
                };
                const step2 = switch(tokens.items[2]) {
                    .StepNumber => |num| ret2: {
                        if (num < 1 or num > proof.steps.items.len) {
                            return StepParsingError.InvalidStepNumber;
                        }
                        break :ret2 &proof.steps.items[num - 1];
                    },
                    else => return StepParsingError.InvalidFormat,
                };
                fromSteps = .{
                    .two = [2]*proofs.Proof.Step {step1, step2},
                };
            } else {
                return StepParsingError.InvalidFormat;
            }

            break :ret proofs.Proof.Justification{
                .Inference = .{
                    .rule = proof.inference_rules[rule_num - 1],
                    .from = fromSteps,
                }
            };
        },
        .Assumption => ret: {
            if (tokens.items.len != 2) {
                return StepParsingError.InvalidFormat;
            }

            break :ret proofs.Proof.Justification{
                .Assumption = proof.assumptions.items
            };
        }
    };

    return proofs.Proof.Step{
        .wff = wff,
        .how = justification,
    };
}