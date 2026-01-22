const std = @import("std");
const Compiler = @import("./vm/compiler.zig");
const TokenStream = @import("token_stream.zig");
const Parser = @import("parser.zig");
const VM = @import("./vm/vm.zig");
const Object = @import("./vm/object.zig");

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
    compiler.* = Compiler.init(allocator);

    return .{
        .allocator = allocator,
        .parser = parser,
        .compiler = compiler,
        .vm = try VM.init(allocator, 0),
    };
}

pub fn deinit(self: *API) void {
    self.vm.deinit();
    self.compiler.deinit();
    self.parser.deinit();

    self.allocator.destroy(self.compiler);
    self.parser.arena.child_allocator.destroy(self.parser);
}

pub fn doString(self: *API, string: []const u8) !?Value {
    const root_scope = &self.vm.scopes[0];
    root_scope.* = .{};

    const closure = try self.compileStringToClosure(string);
    // defer closure.object.deinit(self.allocator);

    self.vm.global_symbol_map = self.compiler.global_symbol_map.items;

    try self.vm.runClosure(closure, root_scope);

    if (root_scope.return_slot) |rs| {
        if (rs.values.len > 1)
            return rs.object.asValue();

        return rs.values[0];
    }

    return null;
}

pub fn doFile(self: *API, path: []const u8) !?Value {
    const file_contents = try std.fs.cwd().readFileAlloc(
        self.allocator,
        path,
        std.math.maxInt(usize),
    );
    defer self.allocator.free(file_contents);

    return self.doString(file_contents);
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

pub fn getGlobal(self: *API, name: []const u8) ?Value {
    return self.vm.global_vars.fields.string_part.get(name);
}

pub fn setGlobal(self: *API, name: []const u8, val: Value) !void {
    const global_ptr = try self.vm.global_vars.fields.getWithStr(name);
    global_ptr.* = val;
}

test getGlobal {
    const allocator = std.testing.allocator;

    var lz = try API.init(allocator);
    defer lz.deinit();

    _ = try lz.doString(
        \\a = 19
        \\local b = 29
        \\
        \\local function in_a_new_scope()
        \\  c = 39
        \\  local d = 49
        \\end
        \\
        \\in_a_new_scope()
    );

    try std.testing.expectEqual(
        lz.getGlobal("a").?.asNumber(),
        19,
    );
    try std.testing.expectEqual(
        lz.getGlobal("b"),
        null,
    );
    try std.testing.expectEqual(
        lz.getGlobal("c").?.asNumber(),
        39,
    );
    try std.testing.expectEqual(
        lz.getGlobal("d"),
        null,
    );
}

test setGlobal {
    const allocator = std.testing.allocator;

    var lz = try API.init(allocator);
    defer lz.deinit();

    try lz.setGlobal("a", Value.initNumber(32));

    const retd = try lz.doString(
        \\return a * 2
    );

    try std.testing.expectEqual(
        retd.?.asNumber(),
        64,
    );
}
