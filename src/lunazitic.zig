const std = @import("std");
const builtin = @import("builtin");
const Compiler = @import("./vm/compiler.zig");
const jdz = @import("jdz");
const build_options = @import("build_options");
const Token = @import("token.zig");
const TokenStream = @import("token_stream.zig");
const Parser = @import("parser.zig");
const VM = @import("./vm/vm.zig");
const Object = @import("./vm/object.zig");
const AST = @import("ast.zig");

pub const Value = VM.Value;
pub const API = @This();

allocator: std.mem.Allocator,
parser: *Parser,
compiler: *Compiler,
vm: *VM,

//TODO: VM and compiler should not use arenas, but echwe dont have a gc yet.
pub fn init(allocator: std.mem.Allocator) !API {
    const parser = try allocator.create(Parser);
    parser.* = Parser.init(allocator, undefined);

    const compiler = try allocator.create(Compiler);
    compiler.* = Compiler.init(parser.arena_allocator());

    return .{
        .allocator = allocator,
        .parser = parser,
        .compiler = compiler,
        .vm = try VM.init(parser.arena_allocator(), 0),
    };
}

pub fn deinit(self: *API) void {
    self.vm.deinit();
    self.compiler.deinit();
    self.parser.deinit();

    self.allocator.destroy(self.compiler);
    self.allocator.destroy(self.parser);
}

pub fn doString(self: *API, string: []const u8) !?Value {
    const scope = &self.vm.scopes[0];
    scope.* = .{};

    const closure = try self.compileStringToClosure(string);
    defer closure.deinit(self.allocator);

    self.vm.global_symbol_map = self.compiler.global_symbol_map.items;

    try self.vm.runClosure(closure, scope);

    if (scope.return_slot) |rs| {
        if (rs.values.len > 1)
            return rs.object.asValue();

        return rs.values[0];
    }

    return null;
}

pub fn doFile(self: *API, path: []const u8) !?Value {
    const file_content = try std.fs.cwd().readFileAlloc(
        self.allocator,
        path,
        std.math.maxInt(usize),
    );
    defer self.allocator.free(file_content);

    return self.doString(file_content);
}

pub fn compileStringToClosure(self: *API, string: []const u8) !*Object.ObjClosure {
    var token_stream = TokenStream{
        .buffer = string,
        .location = .{},
        .allocator = self.parser.arena_allocator(),
    };

    self.parser.token_stream = &token_stream;
    const root = try self.parser.parseRoot();

    const root_closure = try self.compiler.compileRoot(root);
    return root_closure;
}
