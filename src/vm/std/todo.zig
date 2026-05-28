const std = @import("std");
const VM = @import("../vm.zig");

// ret_val is returned if given, like nil or 0. if null it errors out
pub fn TODO(comptime missing_func: []const u8, comptime ret_val: ?VM.Value) VM.Object.ObjNativeFunction.NativeFunc {
    return struct {
        pub fn todofunction (_: *VM, _: *VM.Scope, _: []VM.Value) anyerror!VM.Value {
            std.log.err("Warning: {s} not implemented!\n", .{missing_func});

            if (ret_val) |val|
                return val;

            return error.NotImplemented;
        }
    }.todofunction;
}
