const std = @import("std");
const VM = @import("../vm.zig");

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const table = try VM.Object.ObjTable.create(vm);
    // Function table.setn was deprecated.
    // Function table.getn corresponds to the new length operator (#);
    // use the operator instead of the function.
    // (See compile-time option LUA_COMPAT_GETN in luaconf.h.)
    try table.fields.putWithKeyObjectAuto("getn", try NativeFunction.create(vm, &getn));
    try table.fields.putWithKeyObjectAuto("concat", try NativeFunction.create(vm, &concat));
    return table.object.asValue();
}

pub fn getn(_: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const fields = args[0].asObjectOfType(.Table).fields;
    return VM.Value.initNumber(@floatFromInt(fields.len()));
}

// concat() args: https://www.codecademy.com/resources/docs/lua/tables/concat
// table: The table containing values to concatenate.
// separator: The character or string used to separate the concatenated values.
// [i] (optional): The starting index in the table for concatenation.
// [j] (optional): The ending index in the table for concatenation.
pub fn concat(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const table: *VM.Object.ObjTable = args[0].asObjectOfType(.Table);

    var res = std.ArrayList(u8).empty;
    defer res.deinit(vm.allocator);

    for (table.fields.array_part.items) |item| {
        if (item.isNil()) continue;
        const value = item.asObjectOfType(.String).value;
        try res.appendSlice(vm.allocator, value);
    }

    return (try VM.Object.ObjString.createMoved(vm, try res.toOwnedSlice(vm.allocator))).object.asValue();
}
