const std = @import("std");
const VM = @import("../vm.zig");
const Pattern = @import("../pattern.zig");

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const string_table = try VM.Object.ObjTable.create(vm);
    try string_table.fields.putWithKeyObjectAuto("byte", try NativeFunction.create(vm, &byte));
    try string_table.fields.putWithKeyObjectAuto("len", try NativeFunction.create(vm, &len));
    try string_table.fields.putWithKeyObjectAuto("lower", try NativeFunction.create(vm, &lower));
    try string_table.fields.putWithKeyObjectAuto("upper", try NativeFunction.create(vm, &upper));
    try string_table.fields.putWithKeyObjectAuto("sub", try NativeFunction.create(vm, &sub));
    try string_table.fields.putWithKeyObjectAuto("reverse", try NativeFunction.create(vm, &reverse));
    try string_table.fields.putWithKeyObjectAuto("format", try NativeFunction.create(vm, &format));
    try string_table.fields.putWithKeyObjectAuto("rep", try NativeFunction.create(vm, &rep));
    try string_table.fields.putWithKeyObjectAuto("gmatch", try NativeFunction.create(vm, &gmatch));
    return string_table.object.asValue();
}

// string.byte (s [, i [, j]])
// Returns the internal numerical codes of the characters s[i], s[i+1], ···, s[j]. The default value for i is 1; the default value for j is i.
// Note that numerical codes are not necessarily portable across platforms.
pub fn byte(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const source = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(source);

    const i: f64 = if (args.len > 1 and args[1].isNumber()) args[1].asNumber() else @floatFromInt(1);
    const j: f64 = if (args.len > 2) args[2].asNumber() else i;

    const char_codes_ret = try vm.allocator.alloc(VM.Value, @intFromFloat(j - (i - 1)));

    for (@intFromFloat(i - 1)..@intFromFloat(j), 0..) |index, idx| {
        const char_code = source[index];
        char_codes_ret[idx] = VM.Value.initNumber(@floatFromInt(char_code));
    }

    return (try VM.Object.ObjTuple.createMoved(vm, char_codes_ret)).object.asValue();
}

// Returns the substring of s that starts at i and continues until j; i and j can be negative.
// If j is absent, then it is assumed to be equal to -1 (which is the same as the string length).
// In particular, the call string.sub(s,1,j) returns a prefix of s with length j, and string.sub(s, -i) returns a suffix of s with length i.
pub fn sub(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    const source = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(source);

    var start = args[1].asNumber() - 1;
    var end = if (args.len > 2) args[2].asNumber() else -1;

    if (start < 0) start = @as(f64, @floatFromInt(source.len)) + start;
    if (end < 0) end = @as(f64, @floatFromInt(source.len)) + end + 1;

    if (start < 0 or start >= @as(f64, @floatFromInt(source.len)) or end < start or end > @as(f64, @floatFromInt(source.len)))
        return VM.Value.initNil();

    const substr = source[@as(usize, @intFromFloat(start))..@as(usize, @intFromFloat(end))];
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

    const format_spec = try args[0].asStringCastNum(vm.allocator);
    defer if (args[0].isNumber()) vm.allocator.free(format_spec);

    var format_string_stream = std.io.fixedBufferStream(format_spec);
    const format_reader = format_string_stream.reader();

    var output = std.ArrayList(u8).empty;
    defer output.deinit(vm.allocator);

    var arg_index: usize = 1;

    while (try readByteOrNull(&format_reader)) |c| {
        if (c != '%') {
            try output.append(vm.allocator, c);
            continue;
        }

        if (arg_index >= args.len)
            return error.InvalidArgumentCount;

        const arg = args[arg_index];
        arg_index += 1;

        switch ((try readByteOrNull(&format_reader)) orelse return error.InvalidOptionToFormat) {
            'd', 'i', 'f', 'u' => {
                if (!arg.isNumber())
                    return error.BadArgument;
                try output.print(vm.allocator, "{d}", .{arg.asNumber()});
            },
            inline 'x', 'o', 'X' => |x| {
                if (!arg.isNumber())
                    return error.BadArgument;
                const fmt: []const u8 = comptime &.{ '{', x, '}' };
                try output.print(vm.allocator, fmt, .{@as(usize, @intFromFloat(arg.asNumber()))});
            },
            's' => {
                const str_arg = try arg.asStringCastNum(vm.allocator);
                defer if (arg.isNumber()) vm.allocator.free(str_arg);
                try output.print(vm.allocator, "{s}", .{str_arg});
            },
            '%' => try output.append(vm.allocator, '%'),
            else => return vm.errorFmt(error.BadArgument, "Bad Argument {c}\n", .{c}),
        }
    }

    return (try VM.Object.ObjString.createMoved(vm, try output.toOwnedSlice(vm.allocator))).object.asValue();
}

// string.rep (s, n) - Returns a string that is the concatenation of n copies of the string s.
pub fn rep(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    const source = try args[0].asStringCastNum(vm.allocator);
    const times: usize = @intFromFloat(try args[1].asNumberCast(.{}));

    // Fast case
    if (times == 0)
        return (try VM.Object.ObjString.create(vm, "")).object.asValue();

    var output = std.ArrayListUnmanaged(u8).empty;
    try output.writer(vm.allocator).writeBytesNTimes(source, times);

    return (try VM.Object.ObjString.createMoved(vm, try output.toOwnedSlice(vm.allocator))).object.asValue();
}

// string.gmatch (s, pattern) - Returns an iterator that returns all occurrences of the pattern in the string s.
// TODO: A better implementation
pub fn gmatch(vm: *VM, scope: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    const source = try args[0].asStringCastNum(vm.allocator);
    const pattern = try args[1].asStringCastNum(vm.allocator);

    // for now, just print all characters that match the pattern
    const pattern_iterator = Pattern.Iterator{
        .pattern = try Pattern.make(vm.allocator, pattern),
        .subject = source,
    };

    try scope.internals.put(
        vm.allocator,
        "gmatch_ptrniterator",
        (try VM.Object.ObjNativeValue.create(vm, pattern_iterator)).object.asValue(),
    );
    return (try VM.Object.ObjNativeFunction.create(vm, gmatch_inner)).object.asValue();
}

fn gmatch_inner(vm: *VM, scope: *VM.Scope, _: []VM.Value) anyerror!VM.Value {
    const pattern_iterator_native_value = (scope.internals.get("gmatch_ptrniterator") orelse unreachable).asObjectOfType(.NativeValue);
    const pattern_iterator: *Pattern.Iterator = pattern_iterator_native_value.asPtr(Pattern.Iterator);

    return if (try pattern_iterator.scan()) |match|
        (try VM.Object.ObjString.create(vm, match)).object.asValue()
    else
        VM.Value.initNil();
}
