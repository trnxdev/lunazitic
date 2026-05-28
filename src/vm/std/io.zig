const std = @import("std");
const VM = @import("../vm.zig");
const TODO = @import("todo.zig").TODO;

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const io_table = try VM.Object.ObjTable.create(vm);

    try io_table.fields.putWithKeyObjectAuto("close", try NativeFunction.create(vm, TODO("io.close", null)));
    try io_table.fields.putWithKeyObjectAuto("flush", try NativeFunction.create(vm, TODO("io.flush", null)));
    try io_table.fields.putWithKeyObjectAuto("input", try NativeFunction.create(vm, TODO("io.input", null)));
    try io_table.fields.putWithKeyObjectAuto("lines", try NativeFunction.create(vm, TODO("io.lines", null)));
    try io_table.fields.putWithKeyObjectAuto("open", try NativeFunction.create(vm, TODO("io.open", null)));
    try io_table.fields.putWithKeyObjectAuto("output", try NativeFunction.create(vm, TODO("io.output", null)));
    try io_table.fields.putWithKeyObjectAuto("popen", try NativeFunction.create(vm, TODO("io.popen", null)));
    try io_table.fields.putWithKeyObjectAuto("read", try NativeFunction.create(vm, TODO("io.read", null)));
    try io_table.fields.putWithKeyObjectAuto("stderr", try NativeFunction.create(vm, TODO("io.stderr", null)));
    try io_table.fields.putWithKeyObjectAuto("stdin", try NativeFunction.create(vm, TODO("io.stdin", null)));
    try io_table.fields.putWithKeyObjectAuto("stdout", try NativeFunction.create(vm, TODO("io.stdout", null)));
    try io_table.fields.putWithKeyObjectAuto("tmpfile", try NativeFunction.create(vm, TODO("io.tmpfile", null)));
    try io_table.fields.putWithKeyObjectAuto("type", try NativeFunction.create(vm, TODO("io.type", null)));
    try io_table.fields.putWithKeyObjectAuto("write", try NativeFunction.create(vm, &write));

    return io_table.object.asValue();
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

    const text = args[0].asObjectOfType(.String).value;
    std.debug.print("{s}", .{text});

    return VM.Value.initNil();
}
