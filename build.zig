const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });
    const optimize = b.standardOptimizeOption(.{});
    const api_source_file = b.path("src/wasm-api.zig");
    // const slr_source_file = b.path("src/slr-parser-generator/zig-slr.zig");

    // _ = b.addModule("zig-slr", .{
    //     .root_source_file = slr_source_file,
    // });

    const exe = b.addExecutable(.{
        .name = "wff-helper",
        .root_source_file = api_source_file,
        .target = target,
        .optimize = optimize,
    });

    // Exports any symbols marked with "export"
    exe.rdynamic = true;
    // Required to work properly
    exe.entry = .disabled;
    b.installArtifact(exe);
}
