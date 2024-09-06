const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "wff-helper",
        .root_source_file = b.path("src/wasm-api.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = b.standardOptimizeOption(.{}),
        
    });
    // Exports any symbols marked with "export"
    exe.rdynamic = true;
    // Required to work properly
    exe.entry = .disabled;
    b.installArtifact(exe);
}
