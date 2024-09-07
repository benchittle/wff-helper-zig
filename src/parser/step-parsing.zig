const std = @import("std");
const wfflib = @import("wff.zig");
const proofs = @import("proof.zig");

const StepParsingError = error{
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

// a union holding an individual component of a step
const StepToken = union(enum) {
    Wff: wfflib.Wff,
    StepNumber: usize,
    Justification: union(enum) {
        Equivalence: usize,
        Inference: usize,
        Assumption,
    },
};

// a struct containing the step that was inputted
const ParsedStep = struct {
    Wff: wfflib.Wff, // this is the wff being presented
    FirstArgument: ?usize, // this is the first argument used for the justifications
    SecondArgument: ?usize, // the is the second argument (used in some inference rules)
    Justification: ?union(enum) { // TODO: Justifiation should always be present
        Equivalence: usize, // an integer indicating whihc equivelence rule this is
        Inference: usize, // an integer indicating whihc inference rule this is
        Assumption,
    },
};

// returns a StepToken with the justification set
pub fn tokenizeJustification(string: []const u8) !StepToken {
    if (std.ascii.eqlIgnoreCase("assumption", string)) {
        return StepToken{ .Justification = .Assumption };
    } else if (string[0] == 'E' or string[0] == 'e') {
        if (std.fmt.parseInt(usize, string[1..], 10)) |rule_num| {
            return StepToken{ .Justification = .{ .Equivalence = rule_num } };
        } else |err| switch (err) {
            error.Overflow => return StepParsingError.InvalidRuleNumber,
            error.InvalidCharacter => return StepParsingError.UnexpectedToken,
        }
    } else if (string[0] == 'I' or string[0] == 'i') {
        if (std.fmt.parseInt(usize, string[1..], 10)) |rule_num| {
            return StepToken{ .Justification = .{ .Inference = rule_num } };
        } else |err| switch (err) {
            error.Overflow => return StepParsingError.InvalidRuleNumber,
            error.InvalidCharacter => return StepParsingError.UnexpectedToken,
        }
    } else {
        return StepParsingError.UnexpectedToken;
    }
}

// returns a list of step tokens that make up a step, the array returned is an entire step
pub fn tokenizeStep(allocator: std.mem.Allocator, wff_parser: wfflib.WffParser, step_string: []const u8) !std.ArrayList(StepToken) {
    if (step_string.len == 0) {
        return StepParsingError.EmptyString;
    }

    var tokens = std.ArrayList(StepToken).init(allocator);
    errdefer tokens.deinit();

    // find the wff section of the step
    var it = std.mem.split(u8, step_string, ",");
    const wff_string = it.next().?;
    var wff = wff_parser.parse(allocator, wff_string) catch |err| switch (err) {
        error.OutOfMemory => return err,
        else => return StepParsingError.InvalidWff,
    };
    errdefer wff.deinit();
    try tokens.append(StepToken{ .Wff = wff });

    // find the other tokens
    while (it.next()) |str| {
        const stripped = strStrip(str);
        if (stripped.len == 0) {
            return StepParsingError.UnexpectedToken;
        }
        if (std.fmt.parseInt(usize, stripped, 10)) |step_num| { // tokenize line number
            try tokens.append(StepToken{ .StepNumber = step_num });
        } else |err| switch (err) {
            error.Overflow => {
                return StepParsingError.InvalidStepNumber;
            },
            error.InvalidCharacter => {
                const justification = try tokenizeJustification(stripped); // tokenize justification
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

// helper function to parse the wff out of the step
fn parseWff(wffToken: StepToken) !wfflib.Wff {
    return switch (wffToken) {
        .Wff => |w| w,
        else => return StepParsingError.InvalidFormat,
    };
}

// helper function to parse the Justification out of the step
fn parseJustification(justToken: StepToken) !StepToken.Justifiation {
    return switch (justToken) {
        .Justification => |data| data,
        else => return StepParsingError.InvalidFormat,
    };
}

// helper function to parse the step given
fn parseStepNumber(argToken: StepToken) !StepToken.StepNumber {
    return switch (argToken) {
        .StepNumber => |arg| arg,
        else => return StepParsingError.InvalidFormat,
    };
}

// returns true if the justification provided if an inference rule
fn isInference(justification: ParsedStep.Justifiation) bool {
    return switch (justification) {
        .Inference => true,
        else => false,
    };
}

// returns true if the justification provided if an inference rule
fn isAssumption(justification: ParsedStep.Justifiation) bool {
    return switch (justification) {
        .Assumption => true,
        else => false,
    };
}

// helper function to parse the steps used in the justification out of the step
// assumes parsedStep's justification field is populated
fn parseStepNumbers(parsedStep: ParsedStep, tokens: std.ArrayList(StepToken)) !ParsedStep {
    if (tokens.items.len > 2) { // if there are more than 2 tokens we can expect line numbers
        parsedStep.FirstArgument = try parseStepNumber(tokens[1]);
    }

    if (tokens.items.len > 3 and isInference(parseStep.Justifiation)) {
        if (parseStep.Justifiation >= 3 and parseStep.Justifiation <= 6) { // I3-I6 have 2 arguments
            parsedStep.SecondArgument = try parseStepNumber(tokens[2]);
        } else {
            // only inference rules I3-I6 have 2 arguments
            return StepParsingError.InvalidFormat;
        }
    } else if (tokens.items.len > 3) {
        // only inference rules have 2 arguments
        return StepParsingError.InvalidFormat;
    }

    return parsedStep;
}

pub fn parseStep(allocator: std.mem.Allocator, wff_parser: wfflib.WffParser, step_string: []const u8) !ParsedStep {
    var tokens = try tokenizeStep(allocator, wff_parser, step_string);
    defer tokens.deinit();
    errdefer for (tokens.items) |tok| {
        switch (tok) {
            .Wff => |wff| wff.deinit(),
            else => {},
        }
    };

    var parsedStep = ParsedStep{}; // this is the return value

    // First token must be wff
    if (tokens.len > 0) {
        parsedStep.Wff = try parseWff(tokens[0]);
    } else {
        return StepParsingError.NoTokensFound;
    }

    // TODO: this is not 2310 Compliant see page 21 for example or derivation definition
    // If wff is T, then there should be no other tokens.
    var wff_true = try wff_parser.parse(allocator, "T");
    defer wff_true.deinit();
    if (parsedStep.Wff.eql(wff_true)) {
        if (tokens.items.len != 1) {
            return StepParsingError.InvalidFormat;
        }
        return parsedStep;
    }

    // Otherwise, last token must be justification
    if (tokens.items.len > 1) {
        parsedStep.Justification = try parseJustification(tokens.getLast());
    } else {
        return StepParsingError.InvalidFormat;
    }

    // the rest must be line numbers
    if (isAssumption(parsedStep.Justification)) {
        if (tokens.items.len > 2) { // too many tokens for an assumption line
            return StepParsingError.InvalidFormat;
        }
    } else {
        parsedStep = try parseStepNumbers(parsedStep, tokens);
    }

    return parsedStep;
}
