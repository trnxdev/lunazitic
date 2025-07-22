const std = @import("std");
const VM = @import("vm.zig");
const AST = @import("../ast.zig");
const Compiler = @import("compiler.zig");
const Value = @import("value.zig");

obj_union: ObjObject,
marked: bool = false,

pub const ObjObject = union(enum) {
    Table: ObjTable,
    String: ObjString,
    Closure: ObjClosure,
    Function: ObjFunction,
    NativeFunction: ObjNativeFunction,
    Tuple: ObjTuple,

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Table => (&self.Table).deinit(),
            .String => (&self.String).deinit(allocator),
            .Closure => (&self.Closure).deinit(allocator),
            .Function => (&self.Function).deinit(allocator),
            .NativeFunction => {},
            .Tuple => (&self.Tuple).deinit(allocator),
        }

        allocator.destroy(self);
    }

    pub fn asValue(self: *@This()) VM.Value {
        return VM.Value.initObject(self);
    }
};

pub const ObjTable = struct {
    object: *ObjObject,
    fields: CustomMap,
    metatable: ?*ObjTable = null,

    // Implementation of Lua tables
    const CustomMap = struct {
        hash_part: HashPart,
        string_part: StringPart,
        array_part: ArrayPart,

        const StringPart = std.StringHashMap(VM.Value);
        const HashPart = std.AutoHashMap(VM.Value, VM.Value);
        const ArrayPart = std.ArrayList(VM.Value);

        pub fn init(allocator: std.mem.Allocator) CustomMap {
            return .{
                .hash_part = HashPart.init(allocator),
                .array_part = ArrayPart.init(allocator),
                .string_part = StringPart.init(allocator),
            };
        }

        pub fn deinit(self: *CustomMap) void {
            self.array_part.deinit();
            self.hash_part.deinit();
            self.string_part.deinit();
        }

        pub fn putWithValue(self: *CustomMap, key: VM.Value, value: VM.Value) !void {
            try if (key.isObjectOfType(.String))
                self.string_part.put(key.asObjectOfType(.String).value, value)
            else
                self.hash_part.put(key, value);
        }

        pub fn putWithKey(self: *CustomMap, key: []const u8, value: Value) !void {
            try self.string_part.put(key, value);
        }

        pub fn putWithKeyObjectAuto(self: *CustomMap, key: []const u8, value: anytype) !void {
            try self.string_part.put(key, value.object.asValue());
        }

        pub fn putNoKey(self: *CustomMap, value: VM.Value) !void {
            try self.array_part.append(value);
        }

        pub fn getWithStr(self: *CustomMap, str: []const u8) !*VM.Value {
            const value = try self.string_part.getOrPut(str);

            if (!value.found_existing)
                value.value_ptr.* = VM.Value.initNil();

            return value.value_ptr;
        }

        pub fn getWithKey(self: *CustomMap, key: VM.Value) !*VM.Value {
            if (key.isNumber()) o: {
                const index = key.asNumber();

                if (!number_belongs_in_array_part(index))
                    break :o;

                if (@as(usize, @intFromFloat(index)) > self.array_part.items.len) {
                    // make sure to resize to the right size
                    const old_size = self.array_part.items.len;
                    const new_size = @as(usize, @intFromFloat(index));
                    try self.array_part.resize(new_size);

                    for (old_size..new_size) |idx| {
                        self.array_part.items[idx] = VM.Value.initNil();
                    }
                }

                return &self.array_part.items[@intFromFloat(index - 1)]; // Lua, please.
            }

            if (key.isObjectOfType(.String))
                return self.getWithStr(key.asObjectOfType(.String).value);

            const value = try self.hash_part.getOrPut(key);

            if (!value.found_existing)
                value.value_ptr.* = VM.Value.initNil();

            return value.value_ptr;
        }

        pub fn getWithKeyOrNull(self: *CustomMap, key: VM.Value) !?*VM.Value {
            if (key.isNumber()) o: {
                const index = key.asNumber();
                const in_array_part = number_belongs_in_array_part(index);

                if (!in_array_part)
                    break :o;

                if (@as(usize, @intFromFloat(index)) <= self.array_part.items.len)
                    return &self.array_part.items[@intFromFloat(index - 1)]; // Lua, please.

                unreachable;
            }

            if (key.isObjectOfType(.String)) {
                const value = try self.string_part.getOrPut(key.asObjectOfType(.String).value);

                if (!value.found_existing) {
                    value.value_ptr.* = VM.Value.initNil();
                    return null;
                }

                return value.value_ptr;
            }

            const value = try self.hash_part.getOrPut(key);

            if (!value.found_existing) {
                value.value_ptr.* = VM.Value.initNil();
                return null;
            }

            return value.value_ptr;
        }

        pub fn firstEntry(self: *CustomMap, vm: *VM) !?struct { Value, Value } {
            if (self.array_part.items.len > 0) o: {
                return try self.firstArrayEntry() orelse break :o;
            }

            if (self.hash_part.count() > 0) o: {
                return try self.firstObjectEntry() orelse break :o;
            }

            if (self.string_part.count() > 0) {
                return try self.firstStringEntry(vm);
            }

            return null;
        }

        pub fn firstArrayEntry(self: *CustomMap) !?struct { Value, Value } {
            if (self.array_part.items.len == 0)
                return null;

            // find first non-nil value
            for (self.array_part.items, 0..) |value, idx| {
                if (value.isNil())
                    continue;

                return .{ Value.initNumber(@floatFromInt(idx + 1)), value };
            }

            return null;
        }

        pub fn firstStringEntry(self: *CustomMap, vm: *VM) !?struct { Value, Value } {
            var iter = self.string_part.iterator();
            const entry = iter.next() orelse return null;
            return .{ (try VM.Object.ObjString.create(vm, entry.key_ptr.*)).object.asValue(), entry.value_ptr.* };
        }

        pub fn firstObjectEntry(self: *CustomMap) !?struct { Value, Value } {
            var iter = self.hash_part.iterator();
            const entry = iter.next() orelse return null;
            return .{ entry.key_ptr.*, entry.value_ptr.* };
        }

        pub fn len(self: CustomMap) usize {
            return self.array_part.items.len + self.hash_part.count() + self.string_part.count();
        }
    };

    pub fn create(vm: *VM) !*@This() {
        const obj_string = try vm.allocateObject();
        obj_string.* = .{ .Table = .{
            .object = obj_string,
            .fields = CustomMap.init(vm.allocator),
        } };
        return &obj_string.Table;
    }

    pub fn createIndependant(allocator: std.mem.Allocator) !*@This() {
        const obj_table = try allocator.create(ObjObject);
        obj_table.* = .{ .Table = .{
            .object = obj_table,
            .fields = CustomMap.init(allocator),
        } };
        return &obj_table.Table;
    }

    pub fn deinit(self: *@This()) void {
        self.fields.deinit();
    }

    pub inline fn number_belongs_in_array_part(num: f64) bool {
        return num == @floor(num) and num >= 1; // Lua tables are 1-indexed
    }
};

pub const ObjString = struct {
    object: *ObjObject,
    value: []u8,

    pub fn create(vm: *VM, value: []const u8) !*@This() {
        const gop = try vm.string_pool.getOrPut(value);

        if (!gop.found_existing) {
            const obj_string = try vm.allocateObject();
            obj_string.* = .{ .String = .{
                .object = obj_string,
                .value = try vm.allocator.dupe(u8, value),
            } };
            gop.value_ptr.* = &obj_string.String;
        }

        return gop.value_ptr.*;
    }

    pub fn createMoved(vm: *VM, value: []u8) !*@This() {
        const gop = try vm.string_pool.getOrPut(value);

        if (!gop.found_existing) {
            const obj_string = try vm.allocateObject();
            obj_string.* = .{ .String = .{
                .object = obj_string,
                .value = value,
            } };
            gop.value_ptr.* = &obj_string.String;
        } else {
            vm.allocator.free(value);
        }

        return gop.value_ptr.*;
    }

    pub fn createIndependant(allocator: std.mem.Allocator, value: []u8) !*@This() {
        const obj_string = try allocator.create(ObjObject);
        obj_string.* = .{ .String = .{
            .object = obj_string,
            .value = value,
        } };
        return &obj_string.String;
    }

    pub fn createIndependantMoved(allocator: std.mem.Allocator, value: []const u8) !*@This() {
        const obj_string = try allocator.create(ObjObject);
        obj_string.* = .{ .String = .{
            .object = obj_string,
            .value = try allocator.dupe(u8, value),
        } };
        return &obj_string.String;
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.value);
    }
};

// Wrap-Arounds: Closure (VM Runtime), Worker (Compiler)
//               ^ Runtime Data        ^ Compiler Data
pub const ObjFunction = struct {
    object: *ObjObject,
    name: ?[]const u8,
    params: []const []const u8,
    var_arg: bool = false,
    locals: u8 = 0,
    upvalues: []const Compiler.UpvalueData = &.{},
    instructions: []const Compiler.Instruction = &.{},
    constants: []const Value = &.{},

    pub fn getConstant(self: *@This(), index: usize) ?Value {
        const len = self.constants.items.len;

        if (index > len)
            return error.IndexTooLarge;

        if (index < len)
            return error.IndexTooSmall;

        return self.constants.items[index];
    }

    pub fn create(vm: *VM, name: ?[]const u8, params: ?[]const []const u8, var_arg: bool) !*ObjFunction {
        const obj_function = try vm.allocateObject();
        obj_function.* = .{ .Function = .{
            .object = obj_function,
            .name = name,
            .params = if (params) |p| p else &.{},
            .var_arg = var_arg,
        } };
        return &obj_function.Function;
    }

    pub fn createIndependant(allocator: std.mem.Allocator, name: ?[]const u8, params: ?[]const []const u8, var_arg: bool) !*ObjFunction {
        const obj_function = try allocator.create(ObjObject);
        obj_function.* = .{ .Function = .{
            .object = obj_function,
            .name = name,
            .params = if (params) |p| p else &.{},
            .var_arg = var_arg,
        } };
        return &obj_function.Function;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.constants) |c| {
            if (c.isObject())
                c.asObject().deinit(allocator);
        }

        if (self.constants.len > 0)
            allocator.free(self.constants);

        if (self.instructions.len > 0) {
            allocator.free(self.instructions);
        }

        if (self.upvalues.len > 0) {
            allocator.free(self.upvalues);
        }
    }
};

pub const ObjClosure = struct {
    object: *ObjObject,
    func: *ObjFunction,
    upvalues: []*Value,
    defined_in_scope: ?*VM.Scope,

    pub fn create(
        vm: *VM,
        func: *ObjFunction,
        def_in_scope: ?*VM.Scope,
    ) !*@This() {
        const obj_closure = try vm.allocateObject();
        obj_closure.* = .{ .Closure = .{
            .func = func,
            .object = obj_closure,
            .upvalues = try vm.allocator.alloc(*Value, func.upvalues.len),
            .defined_in_scope = def_in_scope,
        } };

        for (obj_closure.Closure.upvalues) |*upvalue| upvalue.* = undefined;

        return &obj_closure.Closure;
    }

    pub fn createIndependant(
        allocator: std.mem.Allocator,
        func: *ObjFunction,
        def_in_scope: ?*VM.Scope,
    ) !*@This() {
        const obj_closure = try allocator.create(ObjObject);
        obj_closure.* = .{ .Closure = .{
            .object = obj_closure,
            .func = func,
            .upvalues = try allocator.alloc(*Value, func.upvalues.len),
            .defined_in_scope = def_in_scope,
        } };

        for (obj_closure.Closure.upvalues) |*upvalue| upvalue.* = undefined;
        for (obj_closure.Closure.upvalues) |upvalue| upvalue.* = Value.initNil();

        return &obj_closure.Closure;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.func.object.deinit(allocator);
        allocator.free(self.upvalues);
    }
};

pub const ObjNativeFunction = struct {
    object: *ObjObject,
    func: NativeFunc,

    pub const NativeFunc = *const fn (vm: *VM, scope: *VM.Scope, args: []VM.Value) anyerror!VM.Value;

    pub fn create(vm: *VM, func: NativeFunc) !*@This() {
        const obj_native_function = try vm.allocateObject();
        obj_native_function.* = .{ .NativeFunction = .{
            .object = obj_native_function,
            .func = func,
        } };
        return &obj_native_function.NativeFunction;
    }
};

// Vararg, Multiple return values
pub const ObjTuple = struct {
    object: *ObjObject,
    values: []VM.Value,

    // If ownership is created here, use this.
    pub fn createOwned(vm: *VM, values: []const VM.Value) !*@This() {
        const obj_tuple = try vm.allocateObject();
        obj_tuple.* = .{ .Tuple = .{
            .object = obj_tuple,
            .values = try vm.allocator.dupe(VM.Value, values),
        } };
        return &obj_tuple.Tuple;
    }

    // If ownership is moved to the Object, use this.
    pub fn createMoved(vm: *VM, values: []VM.Value) !*@This() {
        const obj_tuple = try vm.allocateObject();
        obj_tuple.* = .{ .Tuple = .{
            .object = obj_tuple,
            .values = values,
        } };
        return &obj_tuple.Tuple;
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.values);
    }
};
