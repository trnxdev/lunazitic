const std = @import("std");
const VM = @import("../vm.zig");

pub fn init(vm: *VM) !VM.Value {
    const io = try VM.Object.ObjTable.create(vm);
    try io.fields.putWithKeyObjectAuto("write", try VM.Object.ObjNativeFunction.create(vm, &write));
    return io.object.asValue();
}

// FIXME: Use some default file in the spec
pub fn write(_: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (args[0].isNumber()) {
        std.debug.print("{d}", .{args[0].asNumber()});
        return VM.Value.initNil();
    }

    if (!args[0].isObjectOfType(.String))
        return error.BadArgument;

    const string = args[0].asObjectOfType(.String).value;
    std.debug.print("{s}", .{string});

    return VM.Value.initNil();
}
