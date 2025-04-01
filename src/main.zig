const std = @import("std");
const builtin = @import("builtin");
const Compiler = @import("./vm/compiler.zig");
const jdz = @import("jdz_allocator");

const Token = @import("token.zig");
const TokenStream = @import("token_stream.zig");

const Parser = @import("parser.zig");
const VM = @import("./vm/vm.zig");
const Object = @import("./vm/object.zig");
const AST = @import("ast.zig");

pub const InDebug = true;
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

pub fn main() !void {
    const program_start = std.time.milliTimestamp();

    var gpa = if (InDebug) std.heap.GeneralPurposeAllocator(.{}){} else void{};
    const allocator = if (InDebug) gpa.allocator() else jdz;
    defer _ = if (InDebug) gpa.deinit() else void{};

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const path = args[1];

    // Absolute paths also work `fs.cwd().readFileAlloc`
    const file_content = try std.fs.cwd().readFileAlloc(
        allocator,
        path,
        MaxUsize,
    );
    defer allocator.free(file_content);

    var token_stream = TokenStream{
        .buffer = file_content,
        .location = .{},
        .allocator = allocator,
        // TODO: again. token stream should not need the arena allocator
        // so we do this temporary thing to use parser's arena stuff
    };

    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var parser = Parser.init(allocator, &token_stream);
    defer parser.deinit();

    // SHOULD NOT BE THERE
    token_stream.allocator = parser.arena_allocator();

    const root = try parser.parseRoot();
    const ast_file = try std.fs.cwd().createFile("lunazitic_ast.txt", .{});

    for (root) |stat| {
        try ast_file.writer().print("{any}\n", .{deeperFmt(stat, 100)});
    }

    ast_file.close();

    var compiler = Compiler.init(parser.arena_allocator());
    // defer compiler.deinit();

    const root_closure = try compiler.compileRoot(root);

    const bytecode_file = try std.fs.cwd().createFile("lunazitic_bytecode.txt", .{});
    for (root_closure.func.instructions) |vd| {
        try bytecode_file.writer().print("{any}\n", .{deeperFmt(vd, 100)});
    }

    try bytecode_file.writer().print("\n\n\n", .{});

    for (root_closure.func.constants) |constant| {
        try bytecode_file.writer().print("{any}\n", .{deeperFmt(constant, 100)});

        if (constant.isObjectOfType(.Function)) {
            const func = constant.asObjectOfType(.Function);
            for (func.instructions) |vd| {
                try bytecode_file.writer().print("{any}\n", .{deeperFmt(vd, 100)});
            }
        }
    }

    bytecode_file.close();

    const vm = try VM.init(parser.arena_allocator(), program_start);
    defer vm.deinit();

    vm.global_symbol_map = compiler.global_symbol_map.items;

    const scope: *VM.Scope = &vm.scopes[0];
    scope.* = .{};

    const start = std.time.milliTimestamp();
    try vm.runClosure(root_closure, scope);
    const end = std.time.milliTimestamp();

    std.debug.print("Took: {}ms\n", .{end - start});

    const vm_usage_file = try std.fs.cwd().createFile("lunazitic_vm_usage.txt", .{});

    for (vm.tags_ran[0..], 0..) |ran, i| {
        try vm_usage_file.writer().print("{s}:\t{d}\n", .{ @tagName(@as(std.meta.Tag(Compiler.Instruction), @enumFromInt(i))), ran });
    }

    vm_usage_file.close();
}
