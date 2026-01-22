const std = @import("std");
const builtin = @import("builtin");
const Compiler = @import("./vm/compiler.zig");
const jdz = @import("jdz");
const build_options = @import("build_options");

const lunazitic = @import("lunazitic.zig");

pub const UseGPA = build_options.@"use-gpa";
const MaxUsize = std.math.maxInt(usize);

pub fn deeperFmt(value: anytype, max_depth: usize) DeeperFmt(@TypeOf(value)) {
    return .{ .value = value, .max_depth = max_depth };
}
fn DeeperFmt(comptime T: type) type {
    return struct {
        value: T,
        max_depth: usize,

        pub fn format(
            self: @This(),
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            try std.fmt.formatType(self.value, fmt_str, options, writer, self.max_depth);
        }
    };
}

fn usage() void {
    const stdout = std.fs.File.stdout().deprecatedWriter();
    stdout.print(
        \\Usage: `lunazitic run <file>`
        \\For more help, run `lunazitic help`
        \\
    , .{}) catch @panic("Stdout not available.");
}

fn help() void {
    const stdout = std.fs.File.stdout().deprecatedWriter();
    stdout.print(
        \\Lunazitic - Lua 5.1 impl. in Zig
        \\
        \\Options:
        \\  run <file>: Runs a file
        \\  version: Prints the version
        \\  help: Guess what?
        \\
    , .{}) catch @panic("Stdout not available.");
}

fn repl(lz: *lunazitic) !u8 {
    var stdout = std.fs.File.stdout().deprecatedWriter();

    try stdout.writeAll(
        \\Lunazitic - Lua 5.1impl. in Zig (REPL)
        \\
    );
    while (true) {
        try stdout.writeAll("> ");

        const line = try std.fs.File.stdin().deprecatedReader().readUntilDelimiterOrEofAlloc(lz.allocator, '\n', std.math.maxInt(usize));

        if (line == null)
            break;

        const retd = lz.doString(line.?) catch |e| {
            std.log.err("{any}", .{e});
            continue;
        };

        if (retd) |ret|
            try stdout.print("{any}\n", .{ret});
    }

    return 0;
}

pub fn main() !u8 {
    var gpa = if (UseGPA) std.heap.DebugAllocator(.{}).init else void{};
    const allocator = if (UseGPA) gpa.allocator() else std.heap.raw_c_allocator;
    defer _ = if (UseGPA) gpa.deinit() else void{};

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    std.debug.assert(args.len >= 1);

    var lz = try lunazitic.init(allocator);
    defer lz.deinit();

    if (args.len < 2) {
        return try repl(&lz);
    }

    const Command = enum {
        run,
        help,
        version,
    };
    const command = std.meta.stringToEnum(Command, args[1]) orelse {
        std.log.err("Unkown command {s}", .{args[1]});
        usage();
        return 1;
    };
    switch (command) {
        Command.run => {
            if (args.len < 3) {
                std.log.err("No file given", .{});
                usage();
                return 1;
            }

            const path = args[2];
            _ = lz.doFile(path) catch |e| switch (e) {
                error.FileNotFound => {
                    std.log.err("File not found: {s}", .{path});
                    return 1;
                },
                else => return e,
            };
        },
        Command.help => {
            help();
            return 0;
        },
        Command.version => {
            std.fs.File.stdout().deprecatedWriter().writeAll(build_options.version ++ "\n") catch @panic("Stdout not available.");
            return 0;
        },
    }

    return 0;
}
