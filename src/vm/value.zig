const std = @import("std");
const VM = @import("./vm.zig");
const Object = @import("object.zig");

data: u64,

const Value = @This();

const SIGN_BIT: u64 = 0x8000000000000000;
const QNAN: u64 = 0x7ffc000000000000;

const TAG_NIL = 1; // 01.
const TAG_FALSE = 2; // 10.
const TAG_TRUE = 3; // 11.

const NIL_VAL = Value{ .data = QNAN | TAG_NIL };
const TRUE_VAL = Value{ .data = QNAN | TAG_TRUE };
const FALSE_VAL = Value{ .data = QNAN | TAG_FALSE };

pub fn isBool(self: Value) bool {
    return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
}

pub fn isNil(self: Value) bool {
    return self.data == NIL_VAL.data;
}

pub fn isNumber(self: Value) bool {
    return (self.data & QNAN) != QNAN;
}

pub fn isObject(self: Value) bool {
    return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
}

pub fn asNumber(self: Value) f64 {
    std.debug.assert(self.isNumber());
    return @bitCast(self.data);
}

const NumberCastOptions = packed struct {
    string: bool = true,
    tuple: bool = false,
};
pub fn asNumberCast(self: Value, cast_options: NumberCastOptions) !f64 {
    if (@call(.always_inline, isNumber, .{self}))
        return self.asNumber();

    if (self.isObjectOfType(.Tuple) and cast_options.tuple)
        return try self.asObjectOfType(.Tuple).values[0].asNumberCast(cast_options);

    if (self.isObjectOfType(.String) and cast_options.string)
        return try std.fmt.parseFloat(f64, self.asObjectOfType(.String).value);

    return error.Uncastable;
}

pub fn asBool(self: Value) bool {
    std.debug.assert(self.isBool());
    return self.data == TRUE_VAL.data;
}

pub fn asObject(self: Value) *Object.ObjObject {
    std.debug.assert(self.isObject());
    return @ptrFromInt(self.data & ~(SIGN_BIT | QNAN));
}

pub fn initNumber(x: f64) Value {
    return Value{ .data = @bitCast(x) };
}

pub fn initBool(x: bool) Value {
    return if (x) TRUE_VAL else FALSE_VAL;
}

pub fn initObject(x: *Object.ObjObject) Value {
    return Value{ .data = SIGN_BIT | QNAN | @intFromPtr(x) };
}

pub inline fn initNil() Value {
    return NIL_VAL;
}

pub fn isObjectOfType(self: @This(), object_type: std.meta.Tag(Object.ObjObject)) bool {
    return self.isObject() and std.meta.activeTag(self.asObject().*) == object_type;
}

pub fn asObjectOfType(self: @This(), comptime object_type: std.meta.Tag(Object.ObjObject)) *std.meta.TagPayload(Object.ObjObject, object_type) {
    std.debug.assert(self.isObjectOfType(object_type));
    const object = self.asObject();
    return &@field(object, @tagName(object_type));
}

pub fn asStringCastNum(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
    return if (self.isObjectOfType(.String))
        self.asObjectOfType(.String).value
    else if (self.isNumber())
        try std.fmt.allocPrint(allocator, "{d}", .{self.asNumber()})
    else
        return error.CannotCastToString;
}

pub fn format(
    self: @This(),
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try VM.tostring_internal(self, writer);
}
