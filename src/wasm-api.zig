const std = @import("std");
const builtin = @import("builtin");
const debug = std.debug;

const wfflib = @import("wff.zig");
const ParsingConfig = @import("wff-parsing.zig").NewParsing;
const rules = @import("rules.zig");
const prooflib = @import("proof.zig");
const step_processing = @import("step-processing.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const api_allocator = ret: {
    if (builtin.target.isWasm()) {
        break :ret std.heap.page_allocator; // std.heap.wasm_allocator seemed to be causing a lot of bugs
    } else if (builtin.is_test) {
        break :ret std.heap.page_allocator; //std.testing.allocator;
    } else {
        break :ret gpa.allocator();
    }
};

var wff_builder: ParsingConfig.WffBuilder = undefined;
var equivalence_rules: []const rules.EquivalenceRule = undefined;
var inference_rules: []const rules.InferenceRule = undefined;

fn copyAllocOrNull(comptime T: type, allocator: std.mem.Allocator, object: T) ?*T {
    const copy = allocator.dupe(T, &[_]T{object}) catch return null;
    return &copy[0];
}

// Utility functions //

export fn init() bool {
    wff_builder = ParsingConfig.wff_builder;
    equivalence_rules = api_allocator.dupe(rules.EquivalenceRule, &rules.initEquivalenceRules(api_allocator, wff_builder)) catch return false; //([22]prooflib.EquivalenceRule, api_allocator, prooflib.initEquivalenceRules(api_allocator, wfflib.wff_parser));
    inference_rules = api_allocator.dupe(rules.InferenceRule, &rules.initInferenceRules(api_allocator, wff_builder)) catch return false;

    return true;
}

export fn deinit() void {
    for (equivalence_rules) |rule| {
        rule.deinit();
    }
    api_allocator.free(equivalence_rules);
    for (inference_rules) |rule| {
        rule.deinit();
    }
    api_allocator.free(inference_rules);
}

export fn alloc(len: usize) ?[*]u8 {
    if (api_allocator.alloc(u8, len)) |slice| {
        return slice.ptr;
    } else |_| {
        return null;
    }
}

export fn free(ptr: [*]u8, len: usize) void {
    api_allocator.free(ptr[0..len]);
}

export fn makeStringSlice(string: [*]u8, len: usize) ?*[]const u8 {
    var slice: []u8 = undefined;
    slice.ptr = string;
    slice.len = len;
    return copyAllocOrNull([]u8, api_allocator, slice);
}

export fn freeStringSlice(slice: *[]u8) void {
    api_allocator.free(slice.*);
    api_allocator.destroy(slice);
}

export fn getSliceLength(slice: *[]u8) usize {
    return slice.len;
}

export fn getSlicePtr(slice: *[]u8) [*]u8 {
    return slice.ptr;
}

export fn getByte(ptr: [*]u8) u8 {
    return ptr[0];
}

// Misc //

export fn getAvailableProofMethodsJson(wff: *wfflib.Wff) ?*[]const u8 {
    const implication_form = wff_builder.buildWff(api_allocator, "(p => q)") catch return null;
    defer implication_form.deinit();
    const proof_methods: []const prooflib.Proof.MethodType = if (wff.match(api_allocator, implication_form) catch return null) |match| ret: {
        match.deinit();
        break :ret &[_]prooflib.Proof.MethodType{ prooflib.Proof.MethodType.none, prooflib.Proof.MethodType.direct, prooflib.Proof.MethodType.indirect, prooflib.Proof.MethodType.contradiction };
    } else ret: {
        break :ret &[_]prooflib.Proof.MethodType{ prooflib.Proof.MethodType.none, prooflib.Proof.MethodType.contradiction };
    };

    var method_names = api_allocator.alloc([]const u8, proof_methods.len) catch return null;
    for (0..method_names.len) |i| {
        method_names[i] = proof_methods[i].getString();
    }

    const json = std.json.stringifyAlloc(api_allocator, method_names, .{}) catch return null;
    return copyAllocOrNull([]u8, api_allocator, json);
}

export fn buildStep(step_string: *const []const u8) ?*prooflib.Proof.Step {
    const step = step_processing.buildStep(api_allocator, wff_builder, step_string.*) catch return null;

    // const proofStep = prooflib.Proof.Step{
    //     .wff = step.wff,
    //     .justification = try proofJustification(step, proof),
    // };

    return copyAllocOrNull(prooflib.Proof.Step, api_allocator, step);
}

// export fn proofMethodGetString()

// Wff Interface //

export fn wffParse(wff_string: *const []const u8) ?*wfflib.Wff {
    const wff = wff_builder.buildWff(api_allocator, wff_string.*) catch return null;
    return copyAllocOrNull(wfflib.Wff, api_allocator, wff);
}

export fn wffDeinit(wff: *wfflib.Wff) void {
    wff.deinit();
    api_allocator.destroy(wff);
}

export fn wffFree(wff: *wfflib.Wff) void {
    api_allocator.destroy(wff);
}

export fn wffGetString(wff: *wfflib.Wff) ?*[]const u8 {
    return &wff.string;
}

// Proof Interface //

export fn proofInit(wff: *wfflib.Wff, method_string: *const []const u8) ?*prooflib.Proof {
    const proof_method = prooflib.Proof.MethodType.fromString(method_string.*) orelse return null;
    const proof = prooflib.Proof.init(api_allocator, wff_builder, wff, proof_method, null, equivalence_rules, inference_rules) catch return null;

    return copyAllocOrNull(prooflib.Proof, api_allocator, proof);
}

export fn proofDeinit(proof: *prooflib.Proof, deinit_proving_wff: bool) void {
    proof.deinit(deinit_proving_wff);
    api_allocator.destroy(proof);
}

export fn proofAddAssumption(proof: *prooflib.Proof, wff: *wfflib.Wff) i32 {
    proof.assumptions.append(wff.*) catch return -1;
    return 1;
}

export fn proofAppendStepUnchecked(proof: *prooflib.Proof, step: *prooflib.Proof.Step) i32 {
    proof.appendStepUnchecked(step.*) catch return -1;
    return 1;
}

export fn proofRemoveLastStep(proof: *prooflib.Proof) i32 {
    if (proof.steps.popOrNull()) |lastStep| {
        lastStep.wff.deinit();
        return 1;
    }
    return 0;
}

export fn proofToString(proof: *prooflib.Proof) ?*[]const u8 {
    const string = proof.buildString(api_allocator) catch return null;
    return copyAllocOrNull([]const u8, api_allocator, string);
}

export fn proofIsFinished(proof: *prooflib.Proof) i32 {
    return @intFromBool(proof.isComplete(api_allocator) catch return -1);
}

export fn proofGetProvingWff(proof: *prooflib.Proof) *wfflib.Wff {
    return proof.proving_wff;
}

export fn proofCheckNewStep(proof: *prooflib.Proof, step: *prooflib.Proof.Step) i32 {
    const is_valid = proof.checkNewStep(api_allocator, step.*) catch return -1;
    return @intFromBool(is_valid);
}

// Proof.Step Interface //

export fn proofStepGetJustificationString(step: *prooflib.Proof.Step) ?*[]const u8 {
    const string = step.justification.buildString(api_allocator) catch return null;
    return copyAllocOrNull([]const u8, api_allocator, string);
}

// Does NOT deinit, just frees ptr
export fn proofStepFree(step: *prooflib.Proof.Step) void {
    api_allocator.destroy(step);
}

export fn proofStepGetWff(step: *prooflib.Proof.Step) ?*wfflib.Wff {
    return &step.wff;
}


test "parseStep: a v b, 1, I1" {
    if (!init()) {
        @panic("");
    }
    defer deinit();

    const wff = wffParse(&"(a => (a v b))").?;

    const proof = proofInit(wff, &"direct").?;
    defer proofDeinit(proof, true);

    const step1 = buildStep(&"a, hypothesis").?;
    defer api_allocator.destroy(step1);
    try std.testing.expectEqual(1, proofCheckNewStep(proof, step1));
    try std.testing.expectEqual(1, proofAppendStepUnchecked(proof, step1));
    try std.testing.expect(!try proof.isComplete(api_allocator));

    const step2 = buildStep(&"a v b, 1, I1").?;
    defer api_allocator.destroy(step2);
    try std.testing.expectEqual(1, proofCheckNewStep(proof, step2));
    try std.testing.expectEqual(1, proofAppendStepUnchecked(proof, step2));
    try std.testing.expect(try proof.isComplete(api_allocator));

    // const s = try proof.buildString(api_allocator);
    // defer api_allocator.free(s);
    // debug.print("{s}\n", .{s});
}
