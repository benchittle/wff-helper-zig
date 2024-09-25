const std = @import("std");

const wfflib = @import("wff.zig");
const WffParsingConfig = @import("wff-parsing.zig").NewParsing;
const rules = @import("rules.zig");
const prooflib = @import("proof.zig");
const step_processing = @import("step-processing.zig");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

var gpa = std.heap.GeneralPurposeAllocator(.{}){};

/// Prompt the user for a number. Invalid input will be ignored and the user will
/// be prompted again.
/// Returns: A valid u32 entered by the user, or null if EOF is entered.
fn getNumber(comptime T: type) !?T {
    //try resetPrompt();

    while (true) {
        var buf: [32]u8 = undefined;

        const buf_read = stdin.readUntilDelimiterOrEof(&buf, '\n') catch |err| switch (err) {
            error.StreamTooLong => {
                try printErrResetPrompt("Error: Input too long", .{});
                try stdin.skipUntilDelimiterOrEof('\n');
                continue;
            },
            else => return err,
        } orelse return null;

        return std.fmt.parseInt(T, buf_read, 10) catch |err| {
            switch (err) {
                error.InvalidCharacter => try printErrResetPrompt("Error: Invalid input", .{}),
                error.Overflow => try printErrResetPrompt("Error: Number too large", .{}),
            }
            continue;
        };
    }
}

pub fn getWff(allocator: std.mem.Allocator, wff_builder: WffParsingConfig.WffBuilder) !?wfflib.Wff {
    //try resetPrompt();

    var buf: [1024]u8 = undefined;
    while (true) {
        const buf_read = stdin.readUntilDelimiterOrEof(&buf, '\n') catch |err| switch (err) {
            error.StreamTooLong => {
                try printErrResetPrompt("Error: Max input length exceeded. Try entering a shorter wff or removing unnecessary whitespace.", .{});
                try stdin.skipUntilDelimiterOrEof('\n');
                continue;
            },
            else => return err,
        } orelse return null;

        const wff = wff_builder.buildWff(allocator, buf_read) catch |err| switch (err) {
            error.OutOfMemory => return err,
            else => {
                try printErrResetPrompt("Error: Invalid wff", .{});
                continue;
            },
        };
        return wff;
    }
}

pub fn getStep(allocator: std.mem.Allocator, wff_builder: WffParsingConfig.WffBuilder) !?prooflib.Proof.Step {
    var buf: [1024]u8 = undefined;

    while (true) {
        const buf_read = stdin.readUntilDelimiterOrEof(&buf, '\n') catch |err| switch (err) {
            error.StreamTooLong => {
                try printErrResetPrompt("Error: Max input length exceeded.", .{});
                try stdin.skipUntilDelimiterOrEof('\n');
                continue;
            },
            else => return err,
        } orelse return null;

        const step = step_processing.buildStep(allocator, wff_builder, buf_read) catch |err| switch (err) {
            error.OutOfMemory => return err,
            else => {
                try printErrResetPrompt("Error: Invalid input.", .{});
                continue;
            },
        };

        return step;
    }
}

fn clearScreen() !void {
    try stdout.print("\x1B[2J", .{});
}

fn moveCursorTopLeft() !void {
    try stdout.print("\x1B[2;1H", .{});
}

fn moveCursorBottomLeft() !void {
    try stdout.print("\x1B[999;1H", .{});
    try moveCursorUp();
}

fn moveCursorUp() !void {
    try stdout.print("\x1B[1A", .{});
}

fn moveCursorDown() !void {
    try stdout.print("\x1B[1E", .{});
}

fn clearInstructions() !void {
    try stdout.print("\x1B[2;1H", .{});
    try clearLine();
}

fn clearLine() !void {
    try stdout.print("\x1B[1G\x1B[2K", .{});
}

fn moveScreenDown() !void {
    try stdout.print("\x1B[1T", .{});
}

fn resetPromptClearError() !void {
    try moveCursorBottomLeft();
    try clearLine();
    try moveCursorUp();
    try clearLine();
    try moveCursorDown();
    try stdout.print("> ", .{});
}

fn resetPrompt() !void {
    try moveCursorBottomLeft();
    try clearLine();
    try stdout.print("> ", .{});
}

fn printErrResetPrompt(comptime format: []const u8, args: anytype) !void {
    //try moveScreenDown();
    try moveCursorUp();
    try moveCursorUp();
    try clearLine();
    try stdout.print(format, args);
    try moveCursorDown();
    try clearLine();
    try stdout.print("> ", .{});
}

pub fn main() !void {
    const wff_builder = WffParsingConfig.wff_builder;
    var allocator = gpa.allocator();

    try clearScreen();
    try moveCursorTopLeft();
    try stdout.print("Welcome to wff-helper!\n\n", .{});

    var equivalence_rules = rules.initEquivalenceRules(allocator, wff_builder);
    var inference_rules = rules.initInferenceRules(allocator, wff_builder);

    try stdout.print("Start by entering a wff to prove", .{});
    try resetPromptClearError();
    var wff = try getWff(allocator, wff_builder) orelse return;
    defer wff.deinit();

    var proof_methods: [4]prooflib.Proof.MethodType = undefined;
    var available_methods: []prooflib.Proof.MethodType = undefined;
    {
        var implication_form = try wff_builder.buildWff(allocator, "(p => q)");
        defer implication_form.deinit();
        if (try wff.match(allocator, implication_form)) |match| {
            var m = match;
            m.deinit();
            proof_methods = .{ prooflib.Proof.MethodType.none, prooflib.Proof.MethodType.direct, prooflib.Proof.MethodType.indirect, prooflib.Proof.MethodType.contradiction };
            available_methods = proof_methods[0..4];
        } else {
            proof_methods = .{ prooflib.Proof.MethodType.none, prooflib.Proof.MethodType.contradiction, undefined, undefined };
            available_methods = proof_methods[0..2];
        }
    }

    try clearScreen();
    try clearInstructions();
    try stdout.print("Proving: {s}\n\n", .{wff.string});
    try stdout.print("Available proof methods:\n", .{});
    for (available_methods, 0..) |method, i| {
        try stdout.print("{d}: {s}\n", .{ i + 1, method.getString() });
    }
    try stdout.print("\nSelect a proof method", .{});
    try resetPromptClearError();
    var choice = try getNumber(u32) orelse return;
    while (choice < 1 or choice > available_methods.len) {
        try printErrResetPrompt("Invalid input. Enter a number from 1 to {d}", .{available_methods.len});
        choice = try getNumber(u32) orelse return;
    }

    var proof = try prooflib.Proof.init(
        allocator,
        wff_builder,
        &wff,
        available_methods[choice - 1],
        null,
        &equivalence_rules,
        &inference_rules,
    );

    try clearScreen();
    var isInvalidStep = false;
    while (!(try proof.isComplete(allocator))) {
        try moveCursorTopLeft();
        const proof_str = try proof.buildString(allocator);
        defer allocator.free(proof_str);
        try stdout.print("{s}", .{proof_str});

        if (isInvalidStep) {
            try stdout.print("\nError: Invalid step", .{});
            isInvalidStep = false;
        }
        try stdout.print("\nEnter a step in the proof\n\n", .{});
        try resetPrompt();
        const step = try getStep(allocator, wff_builder) orelse return;
        const isValid = try proof.checkNewStep(allocator, step);
        if (!isValid) {
            isInvalidStep = true;
            continue;
        }

        try proof.steps.append(step);
        try clearScreen();
    }

    try moveCursorTopLeft();
    const proof_str = try proof.buildString(allocator);
    defer allocator.free(proof_str);
    try stdout.print("{s}", .{proof_str});
    try stdout.print("Proof complete!\n", .{});
}
