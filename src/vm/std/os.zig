const std = @import("std");
const VM = @import("../vm.zig");
const Value = @import("../value.zig");

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const os = try VM.Object.ObjTable.create(vm);
    try os.fields.putWithKeyObjectAuto("clock", try NativeFunction.create(vm, &clock));
    try os.fields.putWithKeyObjectAuto("getenv", try NativeFunction.create(vm, &getenv));
    try os.fields.putWithKeyObjectAuto("execute", try NativeFunction.create(vm, &execute));
    return os.object.asValue();
}

pub fn clock(vm: *VM, _: *VM.Scope, _: []VM.Value) anyerror!VM.Value {
    // Returns an approximation of the amount in seconds of CPU time used by the program.
    // TODO: This is not the same as Lua's os.clock, feel free to send rage letters at [redacted white house email]
    // :trollface:
    return VM.Value.initNumber(@as(f64, @floatFromInt(std.time.milliTimestamp() - vm.internals.program_start)) / 1000.0);
}

pub fn getenv(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    // Lua still continues execution if the argument is number.
    // But `export 2=YO` results in an error.
    if (!args[0].isObjectOfType(.String))
        return error.BadArgument;

    // TODO: maybe save envMap in the internals?
    // Tho... What if a env variable is updated while the program is running
    // Let's just keep it simple for now
    const key = args[0].asObjectOfType(.String).value;

    const value = std.process.getEnvVarOwned(vm.allocator, key) catch |e| switch (e) {
        std.process.GetEnvVarOwnedError.EnvironmentVariableNotFound => return VM.Value.initNil(),
        else => return e,
    };

    return (try VM.Object.ObjString.create(vm, value)).object.asValue();
}

// TODO: make sure this is spec compliant
pub fn execute(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    const command = try args[0].asStringCastNum(vm.allocator);
    var child = std.process.Child.init(&.{command}, vm.allocator);
    const term = try child.spawnAndWait();
    return Value.initNumber(@floatFromInt(term.Exited));
}
