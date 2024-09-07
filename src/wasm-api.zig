const std = @import("std");
const builtin = @import("builtin");
const debug = std.debug;

const wfflib = @import("wff.zig");
const prooflib = @import("proof.zig");
const step_parsing = @import("step-parsing.zig");

const wff_parser = wfflib.wff_parser;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const api_allocator = ret: {
    if (builtin.target.isWasm()) {
        break :ret std.heap.page_allocator; // std.heap.wasm_allocator seemed to be causing a lot of bugs
    } else if (builtin.is_test) {
        break :ret std.heap.page_allocator;  //std.testing.allocator;
    } else {
        break :ret gpa.allocator();
    }
};

var equivalence_rules: []const prooflib.EquivalenceRule = undefined;
var inference_rules: []const prooflib.InferenceRule = undefined;

fn copyAllocOrNull(comptime T: type, allocator: std.mem.Allocator, object: T) ?*T {
    const copy = allocator.dupe(T, &[_]T {object}) catch return null;
    return &copy[0];
}


// Utility functions //

export fn init() bool {
    equivalence_rules = api_allocator.dupe(prooflib.EquivalenceRule, &prooflib.initEquivalenceRules(api_allocator, wfflib.wff_parser)) catch return false;//([22]prooflib.EquivalenceRule, api_allocator, prooflib.initEquivalenceRules(api_allocator, wfflib.wff_parser));
    inference_rules = api_allocator.dupe(prooflib.InferenceRule, &prooflib.initInferenceRules(api_allocator, wfflib.wff_parser)) catch return false;
    
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
    const implication_form = wff_parser.parse(api_allocator, "(p => q)") catch return null;
    defer implication_form.deinit();
    const proof_methods: []const prooflib.Proof.Method = if (wff.match(api_allocator, implication_form) catch return null) |match| ret: {
        match.deinit();
        break :ret &[_]prooflib.Proof.Method { prooflib.Proof.Method.None, prooflib.Proof.Method.Direct, prooflib.Proof.Method.Indirect, prooflib.Proof.Method.Contradiction };
    } else ret: {
        break :ret &[_]prooflib.Proof.Method { prooflib.Proof.Method.None, prooflib.Proof.Method.Contradiction };
    };

    var method_names = api_allocator.alloc([]const u8, proof_methods.len) catch return null;
    for (0..method_names.len) |i| {
        method_names[i] = proof_methods[i].getString();
    }

    const json = std.json.stringifyAlloc(api_allocator, method_names, .{}) catch return null;
    return copyAllocOrNull([]u8, api_allocator, json);
}

export fn parseStep(step_string: *const []const u8, proof: *prooflib.Proof) ?*prooflib.Proof.Step {
    const step = step_parsing.parseStep(api_allocator, wff_parser, step_string.*, proof.*) catch return null;
    return copyAllocOrNull(prooflib.Proof.Step, api_allocator, step);
}

// export fn proofMethodGetString()


// Wff Interface //

export fn wffParse(wff_string: *const []const u8) ?*wfflib.Wff {
    const wff = wff_parser.parse(api_allocator, wff_string.*) catch return null;
    return copyAllocOrNull(wfflib.Wff, api_allocator, wff);
}

export fn wffDeinit(wff: *wfflib.Wff) void {
    wff.deinit();
    api_allocator.destroy(wff);
}

export fn wffGetString(wff: *wfflib.Wff) ?*[]const u8 {
    return &wff.string;
}


// Proof Interface //

export fn proofInit(wff: *wfflib.Wff, method_string: *const []const u8) ?*prooflib.Proof {
    const proof_method = prooflib.Proof.Method.fromString(method_string.*) orelse return null;
    const proof = prooflib.Proof.init(
            api_allocator,
            wff_parser,
            wff,
            proof_method,
            null,
            equivalence_rules,
            inference_rules
        ) catch return null;

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

export fn proofAddStep(proof: *prooflib.Proof, step: *prooflib.Proof.Step) i32 {
    proof.appendStep(step.*) catch return -1;
    return 1;
}

export fn proofToString(proof: *prooflib.Proof) ?*[]const u8 {
    const string = proof.toString(api_allocator) catch return null;
    return copyAllocOrNull([]const u8, api_allocator, string);
}

export fn proofIsFinished(proof: *prooflib.Proof) i32 {
    return @intFromBool(proof.isComplete(api_allocator) catch return -1);
}

export fn proofGetProvingWff(proof: *prooflib.Proof) *wfflib.Wff {
    return proof.proving_wff;
}


// Proof.Step Interface //

export fn proofStepGetJustificationString(step: *prooflib.Proof.Step, proof: *prooflib.Proof) ?*[]const u8 {
    const string = step.how.getString(api_allocator, proof.*) catch return null;
    return copyAllocOrNull([]const u8, api_allocator, string);
}

// Does NOT deinit, just frees ptr
export fn proofStepFree(step: *prooflib.Proof.Step) void {
    api_allocator.destroy(step);
}

export fn proofStepGetWff(step: *prooflib.Proof.Step) ?*wfflib.Wff {
    return &step.wff;
}

export fn proofStepIsValid(step: *prooflib.Proof.Step) i32 {
    const is_valid = step.isValid(api_allocator) catch return -1;
    return @intFromBool(is_valid);
}

test "parseStep: a v b, 1, I1" {
    if (!init()) {
        @panic("");
    }
    defer deinit();

    const wff = wffParse(&"(a => (a v b))").?;

    const proof = proofInit(wff, &"direct").?;
    defer proofDeinit(proof, true);

    // const step1 = parseStep(&"T", proof).?;
    // defer api_allocator.destroy(step1);
    // if (!proofAddStep(proof, step1)) {
    //     @panic("");
    // }

    

    const step1 = parseStep(&"a, assumption", proof).?;
    defer api_allocator.destroy(step1);
    try std.testing.expectEqual(1, proofAddStep(proof, step1));

    try std.testing.expect(!try proof.isComplete(api_allocator));

    const step2 = parseStep(&"a v b, 1, I1", proof).?;
    defer api_allocator.destroy(step2);
    try std.testing.expectEqual(1, proofAddStep(proof, step2));

    try std.testing.expect(try proof.isComplete(api_allocator));

    const s = try proof.toString(api_allocator);
    defer api_allocator.free(s);
    debug.print("{s}\n", .{s});
}