const std = @import("std");
const VM = @import("../vm.zig");

pub fn init(vm: *VM) !VM.Value {
    const string = try VM.Object.ObjTable.create(vm);
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "byte")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &byte)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "len")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &len)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "lower")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &lower)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "upper")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &upper)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "sub")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &sub)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "reverse")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &reverse)).object.asValue());
    try string.fields.putWithKey((try VM.Object.ObjString.create(vm, "format")).object.asValue(), (try VM.Object.ObjNativeFunction.create(vm, &format)).object.asValue());
    return string.object.asValue();
}

pub fn byte(_: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isNumber() and !args[0].isObjectOfType(.String))
        return error.BadArgument;

    // 48 = '0' in ascii table
    // TODO: Is this faster than using asStringCastNum? Is it even worth it?
    // Are pancakes better than waffles?
    if (args[0].isNumber())
        return if (args[0].asNumber() < 0)
            VM.Value.initNumber('-')
        else
            VM.Value.initNumber(args[0].asNumber() + 48);

    const string = args[0].asObjectOfType(.String).value;

    // TODO: Lua 5.1 returns nothing, and when you try to get length of it, it panics.
    // Let's keep it nil.
    if (string.len == 0)
        return VM.Value.initNil();

    return VM.Value.initNumber(@floatFromInt(string[0]));
}

// Returns the substring of s that starts at i and continues until j; i and j can be negative.
// If j is absent, then it is assumed to be equal to -1 (which is the same as the string length).
// In particular, the call string.sub(s,1,j) returns a prefix of s with length j, and string.sub(s, -i) returns a suffix of s with length i.
pub fn sub(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    const string = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(string);

    var start = args[1].asNumber() - 1;
    var end = if (args.len > 2) args[2].asNumber() else -1;

    if (start < 0) start = @as(f64, @floatFromInt(string.len)) + start;
    if (end < 0) end = @as(f64, @floatFromInt(string.len)) + end + 1;

    if (start < 0 or start >= @as(f64, @floatFromInt(string.len)) or end < start or end > @as(f64, @floatFromInt(string.len)))
        return VM.Value.initNil();

    const substr = string[@as(usize, @intFromFloat(start))..@as(usize, @intFromFloat(end))];
    return (try VM.Object.ObjString.create(vm, substr)).object.asValue();
}

pub fn len(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const arg = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(arg);
    return VM.Value.initNumber(@floatFromInt(arg.len));
}

// zig fmt: off
pub const lower = makeSimpleStringFuncOnAllocd(struct { pub fn lower(str: []u8) void {
    for (str) |*char|
        char.* = std.ascii.toLower(char.*);
} }.lower);
pub const upper = makeSimpleStringFuncOnAllocd(struct { pub fn upper(str: []u8) void {
    for (str) |*char|
        char.* = std.ascii.toUpper(char.*);
} }.upper);
pub const reverse = makeSimpleStringFuncOnAllocd(struct { pub fn upper(str: []u8) void {
    std.mem.reverse(u8, str);
} }.upper);
// zig fmt: on

// Gives the caller an 100% allocated string that can be modified, it's automatically returned.
fn makeSimpleStringFuncOnAllocd(func: fn ([]u8) void) fn (*VM, *VM.Scope, []VM.Value) anyerror!VM.Value {
    return struct {
        fn innerfunc(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
            if (args.len < 1)
                return error.InvalidArgumentCount;

            var string = try args[0].asStringCastNum(vm.allocator);
            if (!args[0].isNumber()) string = try vm.allocator.dupe(u8, string);

            func(@constCast(string));

            return (try VM.Object.ObjString.createMoved(vm, @constCast(string))).object.asValue();
        }
    }.innerfunc;
}

fn readByteOrNull(reader: anytype) anyerror!?u8 {
    return reader.readByte() catch |e| switch (e) {
        error.EndOfStream => null,
        else => return e,
    };
}

pub fn format(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const format_string = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(format_string);

    var format_string_stream = std.io.fixedBufferStream(format_string);
    const format_string_reader = format_string_stream.reader();

    var formatted_string = std.ArrayList(u8).init(vm.allocator);
    defer formatted_string.deinit();

    var arg_index: usize = 1;

    while (try readByteOrNull(&format_string_reader)) |c| {
        if (c != '%') {
            try formatted_string.append(c);
            continue;
        }

        if (arg_index >= args.len)
            return error.InvalidArgumentCount;

        const arg = args[arg_index];
        arg_index += 1;

        switch ((try readByteOrNull(&format_string_reader)) orelse return error.InvalidOptionToFormat) {
            'd', 'i', 'f', 'u' => {
                if (!arg.isNumber())
                    return error.BadArgument;
                try formatted_string.writer().print("{d}", .{arg.asNumber()});
            },
            inline 'x', 'o', 'X' => |x| {
                if (!arg.isNumber())
                    return error.BadArgument;
                const fmt: []const u8 = comptime &.{ '{', x, '}' };
                try formatted_string.writer().print(fmt, .{@as(usize, @intFromFloat(arg.asNumber()))});
            },
            's' => {
                const str_arg = try arg.asStringCastNum(vm.allocator);
                defer if (arg.isNumber()) vm.allocator.free(str_arg);
                try formatted_string.writer().print("{s}", .{str_arg});
            },
            '%' => try formatted_string.append('%'),
            else => return vm.errorFmt(error.BadArgument, "Bad Argument {c}\n", .{c}),
        }
    }

    return (try VM.Object.ObjString.createMoved(vm, try formatted_string.toOwnedSlice())).object.asValue();
}
