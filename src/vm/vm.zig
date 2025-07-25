const std = @import("std");
const AST = @import("../ast.zig");
pub const Value = @import("value.zig");
pub const Object = @import("object.zig");
const std_math = @import("./std/math.zig");
const std_os = @import("./std/os.zig");
const std_io = @import("./std/io.zig");
const std_string = @import("./std/string.zig");
const std_table = @import("./std/table.zig");
const Parser = @import("../parser.zig");
const Compiler = @import("compiler.zig");

allocator: std.mem.Allocator,
global_vars: *Object.ObjTable,
string_pool: std.StringHashMap(*Object.ObjString),
metatables_for_primitives: MpPrimitivesMaps = undefined, // like "string".byte("otherstring"), was implemented before rewrite
internals: struct {
    magic_number_1: usize = 0,
    magic_number_0: usize = 0,
    reference_to_next: *Object.ObjNativeFunction,
    reference_to_inner_ipairs: *Object.ObjNativeFunction,
    program_start: i64, // milliseconds
    rng: std.Random.Xoshiro256,
},
values: std.ArrayListUnmanaged(*Object.ObjObject),
scopes: [std.math.maxInt(u8)]Scope = undefined,
global_symbol_map: []const Compiler.KstringVSymbol = &.{},
save: [255]Value = undefined,
save_max: usize = 0,

const MpType = enum(u4) {
    Number,
    Bool,
    Nil,
    String,
};
// !!! Length is Amount of fields in MpType
const MpPrimitivesMaps = [4]Object.ObjTable;

const MAX_SYMBOL = std.math.maxInt(Compiler.Instruction.Symbol);
const MAX_INSTRUCTIONS = @typeInfo(Compiler.Instruction).@"union".fields.len;

pub const Scope = struct {
    internals: std.StringArrayHashMapUnmanaged(Value) = .{},
    outer: ?*Scope = null,
    return_slot: ?*Object.ObjTuple = null,
    varargs: ?*Object.ObjTuple = null,
    locals: [MAX_SYMBOL]Value = [_]Value{Value.initNil()} ** MAX_SYMBOL,
    exit: Exit = .None,
    pc: usize = 0,
    index: usize = 0,
    registers: [Compiler.Instruction.MaxReg]Value = [_]Value{Value.initNil()} ** Compiler.Instruction.MaxReg,

    const Exit = enum {
        None,
        Return,
        Break,
        Continue,
        NaturalEnd,
    };
};

pub const ValueList = std.ArrayList(Value);
pub const VM = @This();

pub fn init(
    allocator: std.mem.Allocator,
    program_start: i64,
) !*VM {
    const vm = try allocator.create(VM);

    vm.* = VM{
        .internals = .{
            .program_start = program_start,
            .rng = std.Random.Xoshiro256.init(0),
            .reference_to_next = undefined,
            .reference_to_inner_ipairs = undefined,
        },
        .global_vars = try Object.ObjTable.createIndependant(allocator),
        .allocator = allocator,
        .string_pool = std.StringHashMap(*Object.ObjString).init(allocator),
        .values = .{},
    };
    vm.internals.reference_to_next = try Object.ObjNativeFunction.create(vm, &next);
    vm.internals.reference_to_inner_ipairs = try Object.ObjNativeFunction.create(vm, &inner_ipairs);

    try vm.global_vars.fields.putWithKeyObjectAuto("print", try Object.ObjNativeFunction.create(vm, &print));
    try vm.global_vars.fields.putWithKeyObjectAuto("tostring", try Object.ObjNativeFunction.create(vm, &tostring));
    try vm.global_vars.fields.putWithKeyObjectAuto("assert", try Object.ObjNativeFunction.create(vm, &assert));
    try vm.global_vars.fields.putWithKeyObjectAuto("next", try Object.ObjNativeFunction.create(vm, &next));
    try vm.global_vars.fields.putWithKeyObjectAuto("pairs", try Object.ObjNativeFunction.create(vm, &pairs));
    try vm.global_vars.fields.putWithKeyObjectAuto("ipairs", try Object.ObjNativeFunction.create(vm, &ipairs));
    try vm.global_vars.fields.putWithKeyObjectAuto("_G", vm.global_vars);
    try vm.global_vars.fields.putWithKeyObjectAuto("_VERSION", try Object.ObjString.create(vm, "Lua 5.1"));

    // String STD + Primitive Metatable
    const string_table = &vm.metatables_for_primitives[@intFromEnum(MpType.String)];
    string_table.* = ((try std_string.init(vm)).asObjectOfType(.Table)).*;
    try vm.global_vars.fields.putWithKeyObjectAuto("string", string_table.*);

    // Table STD
    const table = try std_table.init(vm);
    try vm.global_vars.fields.putWithKey("table", table);

    // Math STD
    const math = try std_math.init(vm);
    try vm.global_vars.fields.putWithKey("math", math);

    // OS STD
    const os = try std_os.init(vm);
    try vm.global_vars.fields.putWithKey("os", os);

    // IO STD
    const io = try std_io.init(vm);
    try vm.global_vars.fields.putWithKey("io", io);

    return vm;
}

pub fn deinit(self: *@This()) void {
    for (self.values.items) |item| {
        item.deinit(self.allocator);
    }

    self.global_vars.object.deinit(self.allocator);
    self.string_pool.deinit();
    self.values.deinit(self.allocator);
    self.allocator.destroy(self);
}

pub fn errorFmt(_: *@This(), err: anyerror, comptime log_fmt: []const u8, log_args: anytype) anyerror {
    std.log.err("Lunazitic VM: " ++ log_fmt, log_args);
    return err;
}

pub fn allocateObject(self: *@This()) !*Object.ObjObject {
    const ptr = try self.allocator.create(Object.ObjObject);
    try self.values.append(self.allocator, ptr);
    return ptr;
}

// TODO: needs updating
pub fn pairs(vm: *VM, _: *Scope, args: []Value) anyerror!Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const return_vals = try vm.allocator.alloc(Value, 3);

    return_vals[0] = vm.internals.reference_to_next.object.asValue();
    return_vals[1] = args[0];
    return_vals[2] = Value.initNil();

    return (try Object.ObjTuple.createMoved(vm, return_vals)).object.asValue();
}

pub fn ipairs(vm: *VM, _: *Scope, args: []Value) anyerror!Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const return_vals = try vm.allocator.alloc(Value, 3);

    return_vals[0] = vm.internals.reference_to_inner_ipairs.object.asValue();
    return_vals[1] = args[0];
    return_vals[2] = Value.initNumber(0);

    return (try Object.ObjTuple.createMoved(vm, return_vals)).object.asValue();
}

pub fn inner_ipairs(vm: *VM, _: *Scope, args: []Value) anyerror!Value {
    if (args.len < 2)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    if (!args[1].isNumber())
        return error.BadArgument;

    const table: *Object.ObjTable = args[0].asObjectOfType(.Table);
    const index: usize = @intFromFloat(args[1].asNumber());

    const new_index: usize = index + 1;
    std.debug.assert(Object.ObjTable.number_belongs_in_array_part(@floatFromInt(new_index)));

    if (new_index > table.fields.array_part.items.len)
        return Value.initNil();

    const field = table.fields.array_part.items[new_index -| 1];

    if (field.isNil())
        return Value.initNil();

    const return_vals = try vm.allocator.alloc(Value, 2);

    return_vals[0] = Value.initNumber(@floatFromInt(new_index));
    return_vals[1] = field;

    return (try Object.ObjTuple.createMoved(vm, return_vals)).object.asValue();
}

pub fn getMetaTable(self: *@This(), value: Value) ?*Object.ObjTable {
    if (value.isObjectOfType(.Table))
        return value.asObjectOfType(.Table).metatable;

    const mp_type: MpType = if (value.isNumber())
        .Number
    else if (value.isBool())
        .Bool
    else if (value.isNil())
        .Nil
    else if (value.isObjectOfType(.String))
        .String
    else
        return null;

    return &self.metatables_for_primitives[@intFromEnum(mp_type)];
}

// TODO: needs updating
pub fn next(vm: *VM, _: *Scope, args: []Value) anyerror!Value {
    var return_vals = try vm.allocator.alloc(Value, 2);
    return_vals[0] = Value.initNil();
    return_vals[1] = Value.initNil();

    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isObjectOfType(.Table))
        return error.BadArgument;

    const table: *Object.ObjTable = args[0].asObjectOfType(.Table);
    var previous_maxxed: bool = false;

    o: {
        if (args.len < 2 or args[1].isNil()) {
            return_vals[0], return_vals[1] = try table.fields.firstEntry(vm) orelse break :o;
            break :o;
        }

        const given_key = args[1];

        if (given_key.isNumber()) i: {
            const given_key_num = given_key.asNumber();

            if (!Object.ObjTable.number_belongs_in_array_part(given_key_num)) {
                break :i;
            }

            var next_key: usize = @intFromFloat(given_key_num + 1);

            u: {
                z: while (table.fields.array_part.items.len >= next_key) {
                    if (table.fields.array_part.items[next_key -| 1].isNil()) {
                        next_key += 1;
                        continue :z;
                    }

                    break :u;
                }

                previous_maxxed = true;
                break :i;
            }

            return_vals[0] = Value.initNumber(@floatFromInt(next_key));
            return_vals[1] = table.fields.array_part.items[next_key -| 1];

            break :o;
        }

        if (given_key.isObjectOfType(.String) or previous_maxxed) i: {
            if (previous_maxxed) {
                return_vals[0], return_vals[1] = try table.fields.firstStringEntry(vm) orelse break :i;
                break :o;
            }

            const given_key_str = given_key.asObjectOfType(.String).value;
            var iter = table.fields.string_part.iterator();

            y: while (iter.next()) |entry| {
                if (std.mem.eql(u8, entry.key_ptr.*, given_key_str)) {
                    const nnext = iter.next() orelse break :y;
                    return_vals[0] = (try Object.ObjString.create(vm, nnext.key_ptr.*)).object.asValue();
                    return_vals[1] = nnext.value_ptr.*;
                    break :o;
                }
            }

            previous_maxxed = true;
        }

        if (previous_maxxed) {
            return_vals[0], return_vals[1] = try table.fields.firstObjectEntry() orelse break :o;
            break :o;
        }

        var iter = table.fields.hash_part.iterator();

        while (iter.next()) |entry| {
            if (!sameValues(vm, given_key, entry.key_ptr.*))
                continue;

            const nnext = iter.next() orelse break :o;

            return_vals[0] = nnext.key_ptr.*;
            return_vals[1] = nnext.value_ptr.*;

            break :o;
        }
    }

    return (try Object.ObjTuple.createMoved(vm, return_vals)).object.asValue();
}

pub fn print(vm: *VM, scope: *Scope, args: []const Value) anyerror!Value {
    _ = vm;
    _ = scope;
    var buffered = std.io.bufferedWriter(std.io.getStdOut().writer());
    const stdout = buffered.writer().any();

    for (args, 0..) |arg, i| {
        try tostring_internal(arg, stdout);
        if (i + 1 < args.len)
            try stdout.writeByte('\t');
    }

    try stdout.writeByte(std.ascii.control_code.lf);

    try buffered.flush();

    return Value.initNil();
}

// Returns bytes written.
pub fn tostring_internal(value: Value, writer: std.io.AnyWriter) !void {
    if (value.isObject()) {
        const obj = value.asObject();

        if (value.isObjectOfType(.String)) {
            try writer.writeAll(value.asObjectOfType(.String).value);
            return;
        }

        const obj_type_str = switch (std.meta.activeTag(obj.*)) {
            .Table => "table",
            .Function => "function",
            .NativeFunction => "function:n",
            .Closure => "function:c",
            .String => unreachable,
            .Tuple => "THIS_SHOULD_NOT_BE_PRINTED_tuple",
        };
        try writer.print("{s}: 0x:{x}", .{ obj_type_str, @intFromPtr(obj) });
    } else if (value.isNumber()) {
        var buf: [std.fmt.format_float.bufferSize(.decimal, f64)]u8 = undefined;
        const str = try std.fmt.formatFloat(&buf, value.asNumber(), .{ .mode = .decimal });
        try writer.writeAll(str);
    } else if (value.isBool()) {
        try writer.writeAll(if (value.asBool()) "true" else "false");
    } else if (value.isNil()) {
        try writer.writeAll("nil");
    } else {
        try writer.writeAll("THIS_SHOULD_NOT_BE_PRINTED_default");
    }
}

pub fn tostring(vm: *VM, _: *Scope, args: []const Value) anyerror!Value {
    if (args.len < 1)
        return error.ValueExpected;

    const arg = args[0];
    if (arg.isObject() and arg.isObjectOfType(.String)) {
        return (try Object.ObjString.create(vm, arg.asObjectOfType(.String).value)).object.asValue();
    }

    var buf: std.ArrayList(u8) = .init(vm.allocator);
    try tostring_internal(arg, buf.writer().any());

    return (try Object.ObjString.createMoved(vm, try buf.toOwnedSlice())).object.asValue();
}

pub fn assert(vm: *VM, scope: *Scope, args: []Value) anyerror!Value {
    _ = scope;

    if (args.len < 1)
        return error.InvalidArgumentCount;

    if (!args[0].isBool())
        return Value.initNil();

    const message = if (args.len >= 2)
        if (args[1].isObjectOfType(.String))
            args[1].asObjectOfType(.String).value
        else if (args[1].isNumber())
            try std.fmt.allocPrint(vm.allocator, "{d}", .{args[1].asNumber()})
        else
            return error.BadArgument
    else
        "assertion failed";

    if (!args[0].asBool()) {
        std.log.err("{s}", .{message});
        return error.AssertionFailed;
    }

    return Value.initNil();
}

pub fn runClosure(self: *@This(), closure: *Object.ObjClosure, scope: *Scope) anyerror!void {
    o: while (scope.pc < closure.func.instructions.len) {
        try @call(.always_inline, runInstr, .{ self, closure.func.instructions[scope.pc], closure, scope });

        if (scope.exit == .NaturalEnd)
            scope.exit = .None;

        if (scope.exit != .None)
            break :o;
    }
}

// 2.2 – Booleans
//The boolean type has two values, false and true, which represent the traditional boolean values.
//However, they do not hold a monopoly of condition values: In Lua, any value may represent a condition.
//Conditionals (such as the ones in control structures) consider false and nil as false and anything else as true.
//Beware that, unlike some other scripting languages, Lua considers both zero and the empty string as true in conditional tests.
inline fn if_truthy(value: Value) bool {
    if (@call(.always_inline, Value.isBool, .{value}))
        return value.asBool();

    if (value.isNil())
        return false;

    if (value.isNumber() or value.isObject())
        return true;

    @panic("FIXME: Should be unreachable?");
}

pub fn newScope(self: *@This(), scope: *Scope) !*Scope {
    std.debug.assert(scope.index < self.scopes.len -| 1);
    const new_scope: *Scope = &self.scopes[scope.index + 1];
    new_scope.* = .{};
    new_scope.index = scope.index + 1;
    return new_scope;
}

pub fn newScopeInherit(self: *@This(), scope: *Scope) !*Scope {
    std.debug.assert(scope.index < self.scopes.len -| 1);
    const new_scope: *Scope = &self.scopes[scope.index + 1];
    new_scope.index = scope.index + 1;
    new_scope.varargs = scope.varargs;
    return new_scope;
}

// "Memory is infinite, for now."
pub fn destroyScope(self: *@This(), scope: *Scope) void {
    scope.pc = 0;
    scope.exit = .None;
    scope.return_slot = null;

    // if (scope.return_slot) |rs| {
    //    rs.deinit(self.allocator);
    //}

    if (scope.varargs) |va| {
        va.deinit(self.allocator);
    }
}

pub inline fn getOperand(self: *@This(), op: Compiler.Instruction.Operand, closure: *Object.ObjClosure, scope: *Scope) Value {
    return switch (op) {
        .vararg => if (scope.varargs) |va| va.object.asValue() else Value.initNil(),
        .constant => |c| closure.func.constants[c],
        .global => |g| self.global_vars.fields.string_part.get(self.global_symbol_map[g].k) orelse Value.initNil(),
        .local => |l| scope.locals[l],
        .register => |r| scope.registers[r],
        .upvalue => |u| closure.upvalues[u].*,
        .save_or_nil => |s| if (self.save_max > s) self.save[s] else Value.initNil(),
    };
}

pub inline fn getOperandPtr(self: *@This(), op: Compiler.Instruction.Operand, closure: *Object.ObjClosure, scope: *Scope) !*Value {
    return switch (op) {
        .constant => @panic("Cannot get constant ptr!"),
        .vararg => @panic("Cannot get vararg ptr!"),
        .save_or_nil => @panic("Cannot get save ptr! Use instructions that use save directly."),
        .global => |g| (try self.global_vars.fields.getWithStr(self.global_symbol_map[g].k)), // TODO: optimize?
        .local => |l| &scope.locals[l],
        .register => |r| &scope.registers[r],
        .upvalue => |u| closure.upvalues[u],
    };
}

pub inline fn bin_op(
    self: *@This(),
    op: AST.BinaryOp,
    left: Value,
    right: Value,
    dest: *Value,
    scope: *Scope,
) !void {

    // FIXME Both and and or use short-cut evaluation, they come before evaluating rhs
    switch (op) {
        .Or, .And => std.debug.panic("Unreachable at {}", .{scope.pc}),
        .Eql => {
            dest.* = VM.Value.initBool(self.sameValues(left, right));
        },
        .Neql => {
            dest.* = VM.Value.initBool(!self.sameValues(left, right));
        },
        // https://godbolt.org/z/WTMs1bM3W
        inline .Add, .Sub, .Mul, .Div => |a| {
            const lhs_num = try left.asNumberCast(.{ .tuple = true });
            const rhs_num = try right.asNumberCast(.{ .tuple = true });

            dest.* = Value.initNumber(switch (a) {
                .Add => lhs_num + rhs_num,
                .Sub => lhs_num - rhs_num,
                .Mul => lhs_num * rhs_num,
                .Div => lhs_num / rhs_num,
                else => unreachable,
            });
        },
        inline .Lt, .Lte, .Gt, .Gte => |e| {
            const lhs_num = try left.asNumberCast(.{ .string = true });
            const rhs_num = try right.asNumberCast(.{ .string = true });

            dest.* = Value.initBool(switch (e) {
                .Lt => lhs_num < rhs_num,
                .Lte => lhs_num <= rhs_num,
                .Gt => lhs_num > rhs_num,
                .Gte => lhs_num >= rhs_num,
                else => unreachable,
            });
        },
        .Concat => {
            // FIXME: also prolly fix it (use casts)
            const lhs_str = try left.asStringCastNum(self.allocator);
            defer if (left.isNumber()) self.allocator.free(lhs_str);
            const rhs_str = try right.asStringCastNum(self.allocator);
            defer if (right.isNumber()) self.allocator.free(rhs_str);

            const new_string = try Object.ObjString.createMoved(self, try std.mem.concat(self.allocator, u8, &.{
                lhs_str,
                rhs_str,
            }));
            dest.* = new_string.object.asValue();
        },
        .Mod => {
            const lhs_num = try left.asNumberCast(.{ .string = true });
            const rhs_num = try right.asNumberCast(.{ .string = true });

            dest.* = Value.initNumber(@mod(lhs_num, rhs_num));
        },
        .Pow => {
            const lhs_num = try left.asNumberCast(.{ .string = true });
            const rhs_num = try right.asNumberCast(.{ .string = true });

            dest.* = Value.initNumber(std.math.pow(f64, lhs_num, rhs_num));
        },
    }
}

pub fn runInstr(self: *@This(), instr: Compiler.Instruction, closure: *Object.ObjClosure, scope: *Scope) !void {
    var lock_pc: bool = false;
    defer if (!lock_pc) {
        scope.pc += 1;
    };

    switch (instr) {
        .unwrap_tuple_save => |op_list| {
            var o_buf: [255]Value = undefined;
            const values = try self.resolveOperandListToBuf(op_list, &o_buf, closure, scope);
            self.save_max = try self.extractTuples(o_buf[0..values], &self.save);
        },
        .unwrap_single_tuple_save => |op| {
            self.save_max = try self.extractTuples(&.{self.getOperand(op, closure, scope)}, &self.save);
        },
        .set_local_from_constant => |slfc| {
            scope.locals[slfc.local] = closure.func.constants[slfc.constant];
        },
        .panic => @panic("PANIC HIT"),
        .@"return" => |r| {
            scope.exit = .Return;
            scope.return_slot = if (r)
                try Object.ObjTuple.createOwned(self, self.save[0..self.save_max])
            else
                try Object.ObjTuple.createOwned(self, &.{});
        },
        .call_func_args => {},
        .call_func => |cf| {
            const cfa = closure.func.instructions[scope.pc - 1];
            std.debug.assert(cfa == .call_func_args);

            const function = self.getOperand(cf.func, closure, scope);

            var buf: [1024]Value = undefined;
            const reg_values = try self.resolveOperandListToBuf(cfa.call_func_args.args, &buf, closure, scope);
            const result = try self.callFunction(function, scope, buf[0..reg_values], cfa.call_func_args.has_tuple);

            if (cf.dest) |dest|
                (try self.getOperandPtr(dest, closure, scope)).* = result;
        },
        .copy => |c| {
            (try self.getOperandPtr(c.dest, closure, scope)).* = self.getOperand(c.src, closure, scope);
        },
        .copy_first_from_if_tuple => |cffit| {
            const tuple = self.getOperand(cffit.src, closure, scope);

            if (tuple.isObjectOfType(.Tuple)) {
                const tuple_obj: *Object.ObjTuple = tuple.asObjectOfType(.Tuple);
                (try self.getOperandPtr(cffit.dest, closure, scope)).* = if (tuple_obj.values.len == 0) Value.initNil() else tuple_obj.values[0];
            } else {
                (try self.getOperandPtr(cffit.dest, closure, scope)).* = self.getOperand(cffit.src, closure, scope);
            }
        },
        .set_nil_local => |snl| {
            scope.locals[snl] = Value.initNil();
        },
        // FIXME: Out of Bounds Safety
        .numeric_for_start => |nfs| {
            const numeric_for_end = closure.func.instructions[nfs.loop_pos].numeric_for_loop;

            const start_reg = try self.getOperandPtr(nfs.start_reg, closure, scope);
            const step_reg = try self.getOperandPtr(nfs.step_reg, closure, scope);
            const end_reg = try self.getOperandPtr(nfs.end_reg, closure, scope);
            const variable_reg = &scope.locals[nfs.var_symbol];

            while (start_reg.*.asNumber() <= end_reg.*.asNumber()) {
                variable_reg.* = start_reg.*;
                scope.pc = numeric_for_end.start_pos + 1;

                o: while (scope.pc < nfs.loop_pos) {
                    try self.runInstr(closure.func.instructions[scope.pc], closure, scope);

                    if (scope.exit != .None)
                        break :o;
                }

                start_reg.* = Value.initNumber(start_reg.*.asNumber() + step_reg.*.asNumber());
            }

            scope.pc = nfs.loop_pos;
            scope.exit = .NaturalEnd;
            lock_pc = true;
        },
        .numeric_for_loop => {},
        .bin => |b| {
            const left = self.getOperand(b.lhs, closure, scope);
            const right = self.getOperand(b.rhs, closure, scope);
            const dest = try self.getOperandPtr(b.dest, closure, scope);

            try self.bin_op(b.op, left, right, dest, scope);
        },
        .triple_reg_bin => |b| {
            try self.bin_op(b.op, scope.registers[b.lhs], scope.registers[b.rhs], &scope.registers[b.dest], scope);
        },
        .llr_bin => |b| {
            const left = scope.locals[b.lhs_local];
            const right = scope.locals[b.rhs_local];
            const dest = &scope.registers[b.dest];

            try self.bin_op(b.op, left, right, dest, scope);
        },
        .rlr_bin => |b| {
            const left = scope.registers[b.lhs];
            const right = scope.locals[b.rhs_local];
            const dest = &scope.registers[b.dest];

            try self.bin_op(b.op, left, right, dest, scope);
        },
        .unary_op => |u| {
            const value = self.getOperand(u.src, closure, scope);
            const dest = try self.getOperandPtr(u.dest, closure, scope);

            switch (u.op) {
                .Not => dest.* = Value.initBool(!if_truthy(value)),
                .Neg => dest.* = Value.initNumber(-value.asNumber()),
                .Len => {
                    if (value.isObjectOfType(.String)) {
                        dest.* = Value.initNumber(@floatFromInt(value.asObjectOfType(.String).value.len));
                    } else if (value.isObjectOfType(.Table)) {
                        const fields = value.asObjectOfType(.Table).fields;
                        dest.* = Value.initNumber(@floatFromInt(fields.len()));
                    } else {
                        return error.BadArgument;
                    }
                },
            }
        },
        .new_table => |nt| {
            (try self.getOperandPtr(nt, closure, scope)).* = (try Object.ObjTable.create(self)).object.asValue();
        },
        .table_append_save_after => |tsa| {
            const table: *Object.ObjTable = (self.getOperand(tsa.table, closure, scope)).asObjectOfType(.Table);
            for (self.save[tsa.after..self.save_max]) |val|
                try table.fields.putNoKey(val);
        },
        .table_append => |ta| {
            const table: *Object.ObjTable = (self.getOperand(ta.table, closure, scope)).asObjectOfType(.Table);
            try table.fields.putNoKey(self.getOperand(ta.value, closure, scope));
        },
        .table_set_by_str => |ta| {
            const table: *Object.ObjTable = (self.getOperand(ta.table, closure, scope)).asObjectOfType(.Table);
            const field = try table.fields.getWithStr(ta.field);
            field.* = self.getOperand(ta.value, closure, scope);
        },
        .table_set_by_value => |ta| {
            const field_val = self.getOperand(ta.field, closure, scope);
            if (field_val.isNil())
                return error.NilAccess;
            const table: *Object.ObjTable = (self.getOperand(ta.table, closure, scope)).asObjectOfType(.Table);
            const field = try table.fields.getWithKey(field_val);
            field.* = self.getOperand(ta.value, closure, scope);
        },
        .get_field_by_str => |ta| {
            const maybe_table = self.getOperand(ta.table, closure, scope);

            const table: *Object.ObjTable = if (maybe_table.isObjectOfType(.Table))
                maybe_table.asObjectOfType(.Table)
            else if (self.getMetaTable(maybe_table)) |meta_table|
                meta_table
            else
                return error.NilAccess;

            (try self.getOperandPtr(ta.dest, closure, scope)).* = (try table.fields.getWithStr(ta.field)).*;
        },
        .get_field_by_value => |ta| {
            const maybe_table = self.getOperand(ta.table, closure, scope);

            const table: *Object.ObjTable = if (maybe_table.isObjectOfType(.Table))
                maybe_table.asObjectOfType(.Table)
            else if (self.getMetaTable(maybe_table)) |meta_table|
                meta_table
            else
                return error.NilAccess;

            const field = self.getOperand(ta.field, closure, scope);
            (try self.getOperandPtr(ta.dest, closure, scope)).* = (try table.fields.getWithKey(field)).*;
        },
        .jump => |j| {
            lock_pc = true;
            scope.pc = j;
        },
        .jump_if_true => |j| {
            if (if_truthy(self.getOperand(j.state_reg, closure, scope))) {
                lock_pc = true;
                scope.pc = j.target;
            }
        },
        .jump_if_nil => |j| {
            if (self.getOperand(j.value, closure, scope).isNil()) {
                lock_pc = true;
                scope.pc = j.target;
            }
        },
        .jump_if_false => |j| {
            if (!if_truthy(self.getOperand(j.state_reg, closure, scope))) {
                lock_pc = true;
                scope.pc = j.target;
            }
        },
        .jump_if_false_register => |j| {
            if (!if_truthy(scope.registers[j.state_reg])) {
                lock_pc = true;
                scope.pc = j.target;
            }
        },
        .make_closure => |mc| {
            const inner_func: *Object.ObjFunction = closure.func.constants[mc.constant].asObjectOfType(.Function);
            const new_closure = try Object.ObjClosure.create(
                self,
                inner_func,
                scope,
            );

            for (inner_func.upvalues, 0..) |upvalue_data, i| {
                if (upvalue_data.is_local)
                    new_closure.upvalues[i] = &scope.locals[upvalue_data.symbol]
                else
                    new_closure.upvalues[i] = closure.upvalues[upvalue_data.symbol];
            }

            (try self.getOperandPtr(mc.dest, closure, scope)).* = new_closure.object.asValue();
        },
    }
}

// Lua's == TODO: is this implemented correctly?
pub fn sameValues(_: *@This(), lhs: Value, rhs: Value) bool {
    if (lhs.isNumber() and rhs.isNumber())
        return lhs.asNumber() == rhs.asNumber();

    if (lhs.isObjectOfType(.String) and rhs.isObjectOfType(.String))
        return std.mem.eql(u8, lhs.asObjectOfType(.String).value, rhs.asObjectOfType(.String).value);

    if (lhs.isObject() and rhs.isObject())
        return @intFromPtr(lhs.asObject()) == @intFromPtr(rhs.asObject());

    if (lhs.isBool() and rhs.isBool())
        return lhs.asBool() == rhs.asBool();

    if (lhs.isNil() and rhs.isNil())
        return true;

    return false;
}
pub fn extractTuples(_: *@This(), to_extract: []const Value, dest: []Value) !usize {
    var wrote: usize = 0;
    const max = dest.len;

    for (to_extract) |arg| {
        if (wrote >= max) break;

        if (arg.isObjectOfType(.Tuple)) {
            const tuple_vals = arg.asObjectOfType(.Tuple).values;
            const to_copy = @min(tuple_vals.len, max - wrote);
            std.mem.copyForwards(Value, dest[wrote..][0..to_copy], tuple_vals[0..to_copy]);
            wrote += to_copy;
        } else {
            dest[wrote] = arg;
            wrote += 1;
        }
    }

    return wrote;
}

const MAX_ARGS = 255;

pub fn callFunction(self: *@This(), func: Value, scope: *Scope, given_args: []Value, args_has_tuple: Compiler.Instruction.YNU) anyerror!Value {
    const args = if (args_has_tuple == .No) v: {
        break :v given_args;
    } else v: {
        var raw_args: [255]Value = undefined;
        const args_len = try self.extractTuples(given_args, &raw_args);
        break :v raw_args[0..args_len];
    };

    if (func.isObjectOfType(.NativeFunction)) {
        const native_func = func.asObjectOfType(.NativeFunction);
        return native_func.func(self, scope, args);
    }

    if (!func.isObjectOfType(.Closure))
        return self.errorFmt(error.InvalidCaller, "Expected function (closure), got {}", .{func});

    const closure: *Object.ObjClosure = func.asObjectOfType(.Closure);
    const new_scope = try self.newScope(scope);

    for (0..closure.func.params.len) |i| {
        new_scope.locals[i] = args[i];
    }

    if (closure.func.var_arg and args.len > closure.func.params.len) {
        const varargs = try self.allocator.alloc(Value, args.len - closure.func.params.len);

        for (closure.func.params.len..args.len) |i|
            varargs[i] = args[i];

        new_scope.varargs = (try Object.ObjTuple.createMoved(self, varargs));
    }

    try self.runClosure(closure, new_scope);
    defer self.destroyScope(new_scope);

    if (new_scope.return_slot) |rs|
        return rs.object.asValue();

    return Value.initNil();
}

pub fn resolveOperandListToBuf(self: *@This(), op_list: Compiler.Instruction.OperandList, buf: []Value, closure: *Object.ObjClosure, scope: *Scope) !usize {
    for (op_list, 0..) |opr, i|
        buf[i] = self.getOperand(opr, closure, scope);

    return op_list.len;
}
