const std = @import("std");
const VM = @import("../vm.zig");
const Value = @import("../value.zig");
const TODO = @import("todo.zig").TODO;
const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const os_table = try VM.Object.ObjTable.create(vm);

    try os_table.fields.putWithKeyObjectAuto("clock", try NativeFunction.create(vm, &clock));
    try os_table.fields.putWithKeyObjectAuto("date", try NativeFunction.create(vm, TODO("os.date", null)));
    try os_table.fields.putWithKeyObjectAuto("difftime", try NativeFunction.create(vm, TODO("os.difftime", null)));
    try os_table.fields.putWithKeyObjectAuto("execute", try NativeFunction.create(vm, &execute));
    try os_table.fields.putWithKeyObjectAuto("exit", try NativeFunction.create(vm, TODO("os.exit", null)));
    try os_table.fields.putWithKeyObjectAuto("getenv", try NativeFunction.create(vm, &getenv));
    try os_table.fields.putWithKeyObjectAuto("remove", try NativeFunction.create(vm, TODO("os.remove", null)));
    try os_table.fields.putWithKeyObjectAuto("rename", try NativeFunction.create(vm, TODO("os.rename", null)));
    try os_table.fields.putWithKeyObjectAuto("setlocale", try NativeFunction.create(vm, TODO("os.setlocale", null)));
    try os_table.fields.putWithKeyObjectAuto("time", try NativeFunction.create(vm, TODO("os.time", null)));
    try os_table.fields.putWithKeyObjectAuto("tmpname", try NativeFunction.create(vm, &tmpname));

    return os_table.object.asValue();
}

const tmpchars = std.ascii.letters;
const TMPCHAR_MIN = 0;
const TMPCHAR_MAX = tmpchars.len - 1;

pub fn tmpname(vm: *VM, _: *VM.Scope, _: []VM.Value) anyerror!VM.Value {
    // TODO: don't reinit it every time, for now it's fine.
    var rng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });

    var string = try vm.allocator.dupe(u8, "/tmp/lua_012345");
    const REPLACE_BEGIN: usize = 9;
    const REPLACE_END = string.len;

    for (string[REPLACE_BEGIN .. REPLACE_END]) |*c| {
        const idx = rng.random().intRangeAtMost(usize, TMPCHAR_MIN, TMPCHAR_MAX);
        c.* = tmpchars[idx];
    }

    return (try VM.Object.ObjString.createMoved(vm, string)).object.asValue();
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
    // problem: What if a env variable is updated while the program is running
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
