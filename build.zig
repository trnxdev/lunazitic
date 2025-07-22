const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_gpa = b.option(bool, "use-gpa", "Use the General Purpose Allocator") orelse (optimize == .Debug);

    const exe = b.addExecutable(.{
        .name = "lunazitic",
        .root_source_file = b.path("src/binary.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    const version = @embedFile("version.txt");

    // Validate the version string
    _ = std.SemanticVersion.parse(version) catch |e| {
        std.log.err("Error occured while parsing version ({s}) to semver: {}", .{ version, e });
        return;
    };

    const options = b.addOptions();
    options.addOption(bool, "use-gpa", use_gpa);
    options.addOption([]const u8, "version", version);
    exe.root_module.addOptions("build_options", options);

    if (!use_gpa) {
        exe.linkLibC();
    }

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
