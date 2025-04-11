const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_gpa = b.option(bool, "use-gpa", "Use the General Purpose Allocator") orelse (optimize == .Debug);

    const exe = b.addExecutable(.{
        .name = "lunazitic",
        .root_source_file = b.path("src/main.zig"),
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
        const jdz_dep = b.dependency("jdz", .{});
        exe.root_module.addImport("jdz", jdz_dep.module("jdz_allocator"));
    }

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
