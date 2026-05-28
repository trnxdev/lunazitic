const std = @import("std");
const VM = @import("../vm.zig");
const TODO = @import("./todo.zig").TODO;

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const table_obj = try VM.Object.ObjTable.create(vm);
    // Function table.setn was deprecated.
    // use the operator instead of the function.
    // (See compile-time option LUA_COMPAT_GETN in luaconf.h.)
    try table_obj.fields.putWithKeyObjectAuto("concat", try NativeFunction.create(vm, &concat));
    try table_obj.fields.putWithKeyObjectAuto("insert", try NativeFunction.create(vm, &insert));
    try table_obj.fields.putWithKeyObjectAuto("maxn", try NativeFunction.create(vm, TODO("table.maxn", null)));
    try table_obj.fields.putWithKeyObjectAuto("remove", try NativeFunction.create(vm, TODO("table.remove", null)));
    try table_obj.fields.putWithKeyObjectAuto("sort", try NativeFunction.create(vm, TODO("table.sort", null)));
    // DEPRECATED: Function table.getn corresponds to the new length operator (#);
    try table_obj.fields.putWithKeyObjectAuto("getn", try NativeFunction.create(vm, &getn));

    return table_obj.object.asValue();
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

    const table_obj: *VM.Object.ObjTable = args[0].asObjectOfType(.Table);
    var sep: []const u8 = "";

    if (args.len >= 2) {
        if (!args[1].isObjectOfType(.String))
            return error.BadArgument;
        sep = args[1].asObjectOfType(.String).value;
    }

    if (args.len >= 3) {
        std.log.warn("table.concat with start/end indices is not yet implemented!!!", .{});
    }

    var output = std.ArrayList(u8).empty;
    defer output.deinit(vm.allocator);

    for (table_obj.fields.array_part.items, 0..) |item, idx| {
        if (item.isNil()) continue;
        const value = item.asObjectOfType(.String).value;
        try output.appendSlice(vm.allocator, value);

        if (sep.len >= 1 and idx != table_obj.fields.array_part.items.len - 1)
            try output.appendSlice(vm.allocator, sep);
    }

    return (try VM.Object.ObjString.createMoved(vm, try output.toOwnedSlice(vm.allocator))).object.asValue();
}

// table.insert (table, [pos,] value)
// Inserts element value at position pos in table, shifting up other elements to open space,
// if necessary. The default value for pos is n+1, where n is the length of the table (see §2.5.5),
// so that a call table.insert(t,x) inserts x at the end of table t.

pub fn insert(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const table_obj: *VM.Object.ObjTable = args[0].asObjectOfType(.Table);
    var pos: usize = table_obj.fields.array_part.items.len; // default: n + 1
    var value: VM.Value = args[1];

    switch (args.len) {
        1 => unreachable,
        2 => {},
        else => {
            if (!args[1].isNumber())
                return error.BadArgument;
            pos = @as(usize, @intFromFloat(args[1].asNumber()));
            value = args[2];
        },
    }

    try table_obj.fields.array_part.insert(vm.allocator, pos, value);
    return VM.Value.initNil();
}
