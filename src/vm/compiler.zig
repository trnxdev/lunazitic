const std = @import("std");
const AST = @import("../ast.zig");
const Value = @import("value.zig");
const Object = @import("object.zig");

pub const Instruction = union(enum) {
    @"return": bool, // true if it returns something, false if it returns nothing
    make_closure: struct {
        constant: usize,
        dest: Operand,
    },
    set_nil_local: Symbol,
    panic: void,
    copy_first_from_if_tuple: SrcDest,
    copy: SrcDest,
    new_table: Operand,
    call_func_args: struct {
        args: OperandList,
        has_tuple: YNU,
    },
    call_func: FuncDest,
    jump: PC,
    jump_if_false: struct { state_reg: Operand, target: PC },
    jump_if_false_register: struct { state_reg: Reg, target: PC },
    jump_if_true: struct { state_reg: Operand, target: PC },
    jump_if_nil: struct { value: Operand, target: PC },
    set_local_from_constant: struct { constant: Instruction.Symbol, local: Instruction.Symbol },
    numeric_for_start: struct {
        loop_pos: PC,
        var_symbol: Symbol,
        start_reg: Operand,
        end_reg: Operand,
        step_reg: Operand,
    },
    numeric_for_loop: struct {
        start_pos: PC,
    },
    bin: struct {
        lhs: Operand,
        rhs: Operand,
        dest: Operand,
        op: AST.BinaryOp,
    },
    triple_reg_bin: struct {
        lhs: Instruction.Reg,
        rhs: Instruction.Reg,
        dest: Instruction.Reg,
        op: AST.BinaryOp,
    },
    llr_bin: struct {
        lhs_local: Instruction.Symbol,
        rhs_local: Instruction.Symbol,
        dest: Instruction.Reg,
        op: AST.BinaryOp,
    },
    rlr_bin: struct {
        lhs: Instruction.Reg,
        rhs_local: Instruction.Symbol,
        dest: Instruction.Reg,
        op: AST.BinaryOp,
    },
    table_append: struct {
        table: Operand,
        value: Operand,
    },
    table_append_save_after: struct {
        table: Operand,
        after: u8,
    },
    table_set_by_str: struct {
        table: Operand,
        field: []const u8,
        value: Operand,
    },
    table_set_by_value: struct {
        table: Operand,
        field: Operand,
        value: Operand,
    },
    get_field_by_str: struct {
        field: []const u8,
        table: Operand,
        dest: Operand,
    },
    get_field_by_value: struct {
        field: Operand,
        table: Operand,
        dest: Operand,
    },
    unary_op: struct {
        src: Operand,
        op: AST.UnaryOp,
        dest: Operand,
    },
    unwrap_tuple_save: OperandList,
    unwrap_single_tuple_save: Operand,

    pub const YNU = enum {
        Yes,
        No,
        Unknown,
    };
    pub const PC = usize;
    pub const Symbol = u12;
    pub const Reg = u8;
    pub const MaxReg = std.math.maxInt(Reg);
    pub const OperandList = []const Operand;

    const LhsRhsDest = struct {
        lhs: Operand,
        rhs: Operand,
        dest: Operand,
    };
    const SrcDest = struct {
        src: Operand,
        dest: Operand,
    };

    // TODO: VarArg
    const FuncDest = struct {
        func: Operand,
        dest: ?Operand,
    };

    pub const Operand = union(enum) {
        vararg: void,
        register: Reg,
        local: Symbol,
        upvalue: Symbol,
        global: Symbol,
        save_or_nil: u8,
        constant: Symbol, //TODO: u12?
    };
};

//comptime {
//    @compileLog("Instruction size: ", @sizeOf(Instruction));
//}

allocator: std.mem.Allocator,
global_symbol_map: std.ArrayListUnmanaged(KstringVSymbol) = .{}, // Reduces Opreand size

pub const KstringVSymbol = struct { k: []const u8, v: Instruction.Symbol };
// TODO: std.BufSet looks promissing
const StringSet = std.StringArrayHashMapUnmanaged(void);
const SymbolIndex = usize;

const LocalData = struct {
    symbol: Instruction.Symbol,
    captured: bool,
};

pub const UpvalueData = struct { symbol: Instruction.Symbol, is_local: bool };

const Worker = struct {
    allocator: std.mem.Allocator,
    // Some of those should be read-only after done working.
    function: *Object.ObjFunction,
    symbol_map: std.StringArrayHashMapUnmanaged(LocalData) = .{},
    previous_worker: ?*Worker = null,
    upvalues: std.ArrayListUnmanaged(UpvalueData) = .{},
    reg_allocated: [Instruction.MaxReg]bool = [_]bool{false} ** Instruction.MaxReg,
    instructions: std.ArrayListUnmanaged(Instruction) = .{},
    constants: std.ArrayListUnmanaged(Value) = .{},

    pub fn lastInstructionProducedTuple(self: *@This()) bool {
        return self.instructions.items.len > 0 and self.instructions.items[self.instructions.items.len - 1] == .call_func;
    }

    // Use with caution!
    pub fn peekInstruction(self: *@This()) ?*Instruction {
        if (self.instructions.items.len == 0)
            return null;

        return &self.instructions.items[self.instructions.items.len -| 1];
    }

    pub fn getLocalData(self: *@This(), symbol: []const u8) !?*LocalData {
        const map = self.symbol_map;
        return map.getPtr(symbol);
    }

    pub fn getOrCreateEmpty(self: *@This()) !*LocalData {
        const map = &self.symbol_map;
        // TODO: free this
        const new_index = map.count();
        const gop = try map.getOrPut(self.allocator, try std.fmt.allocPrint(self.allocator, "\x00{d}", .{map.count()}));

        if (!gop.found_existing) {
            if (new_index > std.math.maxInt(Instruction.Symbol))
                std.debug.panic("Reached maximum amount of allocatable Symbols for Locals!", .{});

            gop.value_ptr.* = .{ .symbol = @intCast(new_index), .captured = false };
        }

        return gop.value_ptr;
    }

    pub fn getOrCreateLocalData(self: *@This(), symbol: []const u8, captured: bool) !*LocalData {
        const map = &self.symbol_map;
        const new_index = map.count();
        const gop = try map.getOrPut(self.allocator, symbol);

        if (!gop.found_existing) {
            if (new_index > std.math.maxInt(Instruction.Symbol))
                std.debug.panic("Reached maximum amount of allocatable Symbols for Locals!", .{});

            gop.value_ptr.* = .{ .symbol = @intCast(new_index), .captured = captured };
        }

        return gop.value_ptr;
    }

    pub fn getConstant(self: *@This(), index: usize) ?Value {
        const len = self.constants.items.len;

        if (index > len)
            return error.IndexTooLarge;

        if (index < len)
            return error.IndexTooSmall;

        return self.constants.items[index];
    }

    // TODO: make it u12?
    pub fn putConstant(self: *@This(), constant: Value) !u12 {
        const index = self.constants.items.len;
        try self.constants.append(self.allocator, constant);

        if (index >= std.math.maxInt(u12))
            return error.MaxConstants;

        return @intCast(index);
    }

    pub fn allocateReg(self: *@This()) !Instruction.Operand {
        for (self.reg_allocated, 0..) |ra, i| {
            if (!ra) {
                self.reg_allocated[i] = true;
                return .{ .register = @intCast(i) };
            }
        }

        return error.RegisterMemoryOverflow;
    }

    pub fn freeReg(self: *@This(), index: Instruction.Operand) void {
        if (index == .register) {
            if (!self.reg_allocated[index.register])
                @panic("Invalid/Double Free");

            self.reg_allocated[index.register] = false;
        }
    }

    pub fn deinit(self: *@This()) void {
        self.symbol_map.deinit(self.allocator);
        self.upvalues.deinit(self.allocator);
        self.instructions.deinit(self.allocator);
        self.constants.deinit(self.allocator);
    }
};

pub fn findUpvalue(self: *@This(), worker: *Worker, name: []const u8) !?Instruction.Symbol {
    if (worker.previous_worker) |cur_worker| {
        if (try cur_worker.getLocalData(name)) |local_data| {
            const index = try self.addUpvalue(worker, local_data.symbol, true);
            return index;
        }

        if (try self.findUpvalue(cur_worker, name)) |outer_upvalue| {
            const index = try self.addUpvalue(worker, outer_upvalue, false);
            return index;
        }
    }

    return null;
}

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.global_symbol_map.deinit(self.allocator);
}

pub fn getGlobalSymbol(self: *@This(), symbol: []const u8) !?Instruction.Symbol {
    for (self.global_symbol_map.items) |item| {
        if (std.mem.eql(u8, item.k, symbol))
            return item.v;
    }

    return null;
}

pub fn getOrCreateGlobalSymbol(self: *@This(), symbol: []const u8) !Instruction.Symbol {
    for (self.global_symbol_map.items) |item| {
        if (std.mem.eql(u8, item.k, symbol))
            return item.v;
    }

    const index = self.global_symbol_map.items.len;

    if (index >= std.math.maxInt(u12))
        return error.indexLarge;

    try self.global_symbol_map.append(self.allocator, .{ .k = symbol, .v = @intCast(index) });
    return @intCast(index);
}

pub fn compileRoot(self: *@This(), root: AST.Block) anyerror!*Object.ObjClosure {
    const root_function = try Object.ObjFunction.createIndependant(
        self.allocator,
        null,
        null,
        false,
    );
    var worker: Worker = .{ .function = root_function, .allocator = self.allocator };
    defer worker.deinit();
    try self.compileBlock(root, &worker);
    root_function.instructions = try worker.instructions.toOwnedSlice(self.allocator);
    root_function.constants = try worker.constants.toOwnedSlice(self.allocator);
    const root_closure = try Object.ObjClosure.createIndependant(self.allocator, root_function, null);
    return root_closure;
}

pub fn compileBlock(self: *@This(), block: AST.Block, worker: *Worker) anyerror!void {
    for (block) |stat| {
        try self.compileStat(stat.*, worker);
    }
}

pub fn setByName(self: *@This(), n: []const u8, worker: *Worker, reg: Instruction.Operand) !void {
    if (try worker.getLocalData(n)) |local| {
        if (reg == .constant)
            try worker.instructions.append(self.allocator, Instruction{ .set_local_from_constant = .{
                .constant = reg.constant,
                .local = local.symbol,
            } })
        else
            try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                .src = reg,
                .dest = .{ .local = local.symbol },
            } });
    } else if (try self.findUpvalue(worker, n)) |upvalue|
        try worker.instructions.append(self.allocator, Instruction{ .copy = .{
            .src = reg,
            .dest = .{ .upvalue = upvalue },
        } })
    else
        try worker.instructions.append(self.allocator, Instruction{ .copy = .{
            .src = reg,
            .dest = .{ .global = try self.getOrCreateGlobalSymbol(n) },
        } });
}
// That is, any function can have at most 200 local variables declared within it, regardless of how many local declarations are used.
// > https://stackoverflow.com/a/66539256
const MAXVARS = 200;

pub fn copyConstToLocal(self: *@This(), op: Instruction.Operand, worker: *Worker) !Instruction.Operand {
    if (op == .constant) {
        const empty = try worker.getOrCreateEmpty();

        try worker.instructions.append(self.allocator, .{
            .set_local_from_constant = .{ .constant = op.constant, .local = empty.symbol },
        });
        return .{ .local = empty.symbol };
    }

    return op;
}

pub fn compileStat(self: *@This(), stat: AST.Stat, worker: *Worker) anyerror!void {
    switch (stat) {
        .FunctionCall => |fc| _ = try self.funcCall(fc.*, worker, false),
        .NumericFor => |nf| {
            const start_b = try self.compileExp(nf.Start.*, worker);
            defer worker.freeReg(start_b);
            const step_b = if (nf.Step) |step| try self.compileExp(step.*, worker) else try self.fetchNumber(1, worker);
            defer worker.freeReg(step_b);
            const end_b = try self.compileExp(nf.End.*, worker);
            defer worker.freeReg(end_b);

            const start = try self.copyConstToLocal(start_b, worker);
            const step = try self.copyConstToLocal(step_b, worker);
            const end = try self.copyConstToLocal(end_b, worker);

            const to_patch = worker.instructions.items.len;
            const name_data = try worker.getOrCreateLocalData(nf.Name, false);

            try worker.instructions.append(self.allocator, Instruction{
                .numeric_for_start = .{
                    .loop_pos = 0,
                    .var_symbol = name_data.symbol,
                    .end_reg = end,
                    .start_reg = start,
                    .step_reg = step,
                },
            });

            // index is not same as the var in lua

            for (nf.Block) |inner_stat| {
                try self.compileStat(inner_stat.*, worker);
            }

            const loop = worker.instructions.items.len;
            try worker.instructions.append(self.allocator, Instruction{
                .numeric_for_loop = .{
                    .start_pos = to_patch,
                },
            });

            worker.instructions.items[to_patch].numeric_for_start.loop_pos = loop;

            //   while (worker.symbol_map.count() != remove_point) {
            //       worker.symbol_map.orderedRemoveAt(remove_point);
            //   }
        },
        .VarDecl => |vd| {
            var operands = std.ArrayList(Instruction.Operand).init(self.allocator);
            defer operands.deinit();

            var has_tuple: bool = false;

            for (vd.Exps) |exp| {
                const operand = try self.compileExp(exp.*, worker);
                try operands.append(operand);

                if (worker.lastInstructionProducedTuple())
                    has_tuple = true;
            }

            const ops = try operands.toOwnedSlice();

            if (has_tuple) {
                try worker.instructions.append(self.allocator, Instruction{
                    .unwrap_tuple_save = ops,
                });
            }

            for (ops) |op| worker.freeReg(op);

            for (vd.Vars, 0..) |variable, i| {
                const operand: Instruction.Operand = if (has_tuple) .{ .save_or_nil = @intCast(i) } else ops[i];

                switch (variable) {
                    .Name => |n| {
                        try self.setByName(n, worker, operand);
                    },
                    .DotAccess => |da| {
                        const table = try self.compilePrefix(da.Prefix.*, worker);
                        defer worker.freeReg(table);

                        try worker.instructions.append(self.allocator, Instruction{ .table_set_by_str = .{
                            .field = da.Key,
                            .table = table,
                            .value = operand,
                        } });
                    },
                    .TableAccess => |ta| {
                        const table = try self.compilePrefix(ta.Prefix.*, worker);
                        defer worker.freeReg(table);

                        const field = try self.compileExp(ta.Key.*, worker);
                        defer worker.freeReg(field);

                        try worker.instructions.append(self.allocator, Instruction{ .table_set_by_value = .{
                            .field = field,
                            .table = table,
                            .value = operand,
                        } });
                    },
                }
            }
        },
        .LocalVarDecl => |lvd| {
            const exps: []*AST.Exp = lvd.Exps orelse {
                for (lvd.Names) |name| {
                    try worker.instructions.append(self.allocator, Instruction{
                        .set_nil_local = (try worker.getOrCreateLocalData(name, false)).symbol,
                    });
                }

                return;
            };

            var reg_exps: std.ArrayListUnmanaged(Instruction.Operand) = .{};
            defer reg_exps.deinit(self.allocator);

            var has_tuple: bool = false;

            for (exps) |exp| {
                const exp_result = try self.compileExp(exp.*, worker);
                try reg_exps.append(self.allocator, exp_result);

                if (worker.lastInstructionProducedTuple()) {
                    has_tuple = true;

                    if (lvd.Names.len == 1) {
                        try worker.instructions.append(self.allocator, Instruction{
                            .copy_first_from_if_tuple = .{ .src = exp_result, .dest = .{ .local = (try worker.getOrCreateLocalData(lvd.Names[0], false)).symbol } },
                        });
                        return;
                    }
                }
            }

            const rgs = try reg_exps.toOwnedSlice(self.allocator);
            if (has_tuple) {
                try worker.instructions.append(self.allocator, Instruction{
                    .unwrap_tuple_save = rgs,
                });
            }
            for (rgs) |rg| worker.freeReg(rg);

            std.debug.assert(lvd.Names.len < 255);

            // local [name] = [function call]

            for (lvd.Names, 0..) |name, i| {
                const operand: Instruction.Operand = if (has_tuple) .{ .save_or_nil = @as(u8, @intCast(i)) } else rgs[i];
                const local_data = try worker.getOrCreateLocalData(name, false);

                try worker.instructions.append(self.allocator, Instruction{
                    .copy = .{ .src = operand, .dest = .{ .local = local_data.symbol } },
                });
            }
        },
        .While => |wh| {
            const begin = worker.instructions.items.len;

            const exp = try self.compileExp(wh.Exp.*, worker);
            defer worker.freeReg(exp);

            const to_patch = worker.instructions.items.len;
            if (exp == .register) {
                try worker.instructions.append(self.allocator, Instruction{
                    .jump_if_false_register = .{
                        .state_reg = exp.register,
                        .target = 0,
                    },
                });
            } else {
                try worker.instructions.append(self.allocator, Instruction{
                    .jump_if_false = .{
                        .state_reg = exp,
                        .target = 0,
                    },
                });
            }

            //   const remove_point = worker.instructions.items.len;
            const temp = worker.symbol_map;
            worker.symbol_map = try worker.symbol_map.clone(self.allocator);

            for (wh.Block) |innerstat| {
                try self.compileStat(innerstat.*, worker);
            }

            worker.symbol_map = temp;

            try worker.instructions.append(self.allocator, Instruction{
                .jump = begin,
            });

            const end = worker.instructions.items.len;
            if (exp == .register) {
                worker.instructions.items[to_patch].jump_if_false_register.target = end;
            } else {
                worker.instructions.items[to_patch].jump_if_false.target = end;
            }
        },
        .If => |i| {
            const exp = try self.compileExp(i.Exp.*, worker);
            defer worker.freeReg(exp);

            const to_patch_if_false = worker.instructions.items.len;
            if (exp == .register) {
                try worker.instructions.append(self.allocator, Instruction{
                    .jump_if_false_register = .{
                        .state_reg = exp.register,
                        .target = 0,
                    },
                });
            } else {
                try worker.instructions.append(self.allocator, Instruction{
                    .jump_if_false = .{
                        .state_reg = exp,
                        .target = 0,
                    },
                });
            }

            for (i.Block) |innerstat| {
                try self.compileStat(innerstat.*, worker);
            }

            const to_patch_end = worker.instructions.items.len;
            try worker.instructions.append(self.allocator, Instruction{ .jump = 0 });

            if (exp == .register) {
                worker.instructions.items[to_patch_if_false].jump_if_false_register.target = worker.instructions.items.len;
            } else {
                worker.instructions.items[to_patch_if_false].jump_if_false.target = worker.instructions.items.len;
            }

            if (i.Else) |else_b| {
                for (else_b) |innerstat| {
                    try self.compileStat(innerstat.*, worker);
                }
            }

            const end = worker.instructions.items.len;
            worker.instructions.items[to_patch_end].jump = end;
        },
        // FIXME
        .Return => |r| {
            var reg_list: std.ArrayListUnmanaged(Instruction.Operand) = .{};
            defer reg_list.deinit(self.allocator);

            for (r) |exp| {
                const exp_result = try self.compileExp(exp.*, worker);
                try reg_list.append(self.allocator, exp_result);
            }

            if (reg_list.items.len >= 1) {
                try worker.instructions.append(self.allocator, Instruction{
                    .unwrap_tuple_save = try reg_list.toOwnedSlice(self.allocator),
                });

                try worker.instructions.append(self.allocator, .{ .@"return" = true });
            } else {
                try worker.instructions.append(self.allocator, .{ .@"return" = false });
            }
        },
        .LocalFunction => |lf| {
            const func_symbol = (try worker.getOrCreateLocalData(lf.Name, false)).symbol;

            var local_function = try Object.ObjFunction.createIndependant(
                self.allocator,
                lf.Name,
                lf.Body.Params.Names,
                lf.Body.Params.HasVararg,
            );

            var new_worker = Worker{
                .function = local_function,
                .allocator = self.allocator,
                .previous_worker = worker,
            };
            defer new_worker.deinit();

            for (lf.Body.Params.Names) |n| {
                _ = try new_worker.getOrCreateLocalData(n, false);
            }

            if (lf.Body.Block.len > 0) {
                try self.compileBlock(lf.Body.Block, &new_worker);
            }

            local_function.instructions = try new_worker.instructions.toOwnedSlice(self.allocator);
            local_function.constants = try new_worker.constants.toOwnedSlice(self.allocator);
            local_function.upvalues = try new_worker.upvalues.toOwnedSlice(self.allocator);

            const function_constant = try worker.putConstant(local_function.object.asValue());

            try worker.instructions.append(self.allocator, Instruction{ .make_closure = .{
                .constant = function_constant,
                .dest = .{ .local = func_symbol },
            } });
            return;
        },
        .GenericFor => |gf| {
            var operands = std.ArrayList(Instruction.Operand).init(self.allocator);
            defer operands.deinit();

            for (gf.Exps) |exp| {
                try operands.append(try self.compileExp(exp.*, worker));
            }

            const ops = try operands.toOwnedSlice();
            try worker.instructions.append(self.allocator, Instruction{
                .unwrap_tuple_save = ops,
            });
            for (ops) |op| worker.freeReg(op);

            // Initter returns (iterator as a function, ?param1, ?param2)
            const iterator = try worker.allocateReg();
            defer worker.freeReg(iterator);
            const param_1 = try worker.allocateReg();
            defer worker.freeReg(param_1);
            const param_2 = try worker.allocateReg();
            defer worker.freeReg(param_2);

            try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                .src = .{ .save_or_nil = 0 },
                .dest = iterator,
            } });
            try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                .src = .{ .save_or_nil = 1 },
                .dest = param_1,
            } });
            try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                .src = .{ .save_or_nil = 2 },
                .dest = param_2,
            } });

            const begin = worker.instructions.items.len;

            const iterator_res = try worker.allocateReg();
            defer worker.freeReg(iterator_res);

            const vals = try self.allocator.alloc(Instruction.Operand, 2);
            vals[0] = param_1;
            vals[1] = param_2;

            try worker.instructions.append(self.allocator, Instruction{
                .call_func_args = .{ .args = vals, .has_tuple = .Unknown },
            });
            try worker.instructions.append(self.allocator, Instruction{ .call_func = .{
                .func = iterator,
                .dest = iterator_res,
            } });

            try worker.instructions.append(self.allocator, Instruction{ .unwrap_single_tuple_save = iterator_res });

            // var_1, ···, var_n => local var_1, ···, var_n = f(s, var)
            for (gf.Names, 0..) |name, i| {
                if (i >= gf.Names.len) break;

                const local_data = try worker.getOrCreateLocalData(name, false);

                try worker.instructions.append(self.allocator, Instruction{
                    .copy = .{ .src = .{ .save_or_nil = @intCast(i) }, .dest = .{ .local = local_data.symbol } },
                });
            }

            //  if var_1 == nil then break end
            const to_patch = worker.instructions.items.len;
            try worker.instructions.append(self.allocator, Instruction{ .jump_if_nil = .{
                .value = .{ .save_or_nil = 0 },
                .target = 0,
            } });

            // var = var_1
            try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                .src = .{ .save_or_nil = 0 },
                .dest = param_2,
            } });

            for (gf.Block) |innerstat| {
                try self.compileStat(innerstat.*, worker);
            }

            try worker.instructions.append(self.allocator, Instruction{ .jump = begin });
            worker.instructions.items[to_patch].jump_if_nil.target = worker.instructions.items.len;
        },
        else => |a| std.debug.panic("TODO {}\n", .{a}),
    }
}

pub fn putOptimizedBin(self: *@This(), lhs: Instruction.Operand, rhs: Instruction.Operand, dest: Instruction.Operand, op: AST.BinaryOp, worker: *Worker) !void {
    if (lhs == .register and rhs == .register and dest == .register) {
        try worker.instructions.append(self.allocator, Instruction{
            .triple_reg_bin = .{
                .lhs = lhs.register,
                .rhs = rhs.register,
                .dest = dest.register,
                .op = op,
            },
        });
    } else if (lhs == .local and rhs == .local and dest == .register) {
        try worker.instructions.append(self.allocator, Instruction{
            .llr_bin = .{
                .lhs_local = lhs.local,
                .rhs_local = rhs.local,
                .dest = dest.register,
                .op = op,
            },
        });
    } else if (lhs == .register and rhs == .local and dest == .register) {
        try worker.instructions.append(self.allocator, Instruction{
            .rlr_bin = .{
                .lhs = lhs.register,
                .rhs_local = rhs.local,
                .dest = dest.register,
                .op = op,
            },
        });
    } else {
        try worker.instructions.append(self.allocator, Instruction{
            .bin = .{
                .lhs = lhs,
                .rhs = rhs,
                .dest = dest,
                .op = op,
            },
        });
    }
}

// Owner owns the register
pub fn compileExp(self: *@This(), exp: AST.Exp, worker: *Worker) anyerror!Instruction.Operand {
    return switch (exp) {
        .Nil => try self.fetchNil(worker),
        .Bool => |b| try self.fetchBool(worker, b),
        // FIXME: If it exists, use existing one. Maybe with sets?
        .Number => |n| try self.fetchNumber(n, worker),
        .Prefix => |p| try self.compilePrefix(p.*, worker),
        .String => |string| try self.fetchString(worker, string),
        .Binary => |b| {
            const lhs = try self.compileExp(b.Left.*, worker); // Also dest

            // And and Or use short-cut evaluation, which
            // may not evaluate the right-hand side and
            // spend less time or not error out
            if (b.Op == .And) {
                const dest = try worker.allocateReg();
                try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                    .src = lhs,
                    .dest = dest,
                } });
                const to_patch = worker.instructions.items.len;
                // TODO: optimize
                try worker.instructions.append(self.allocator, Instruction{
                    .jump_if_false = .{
                        .state_reg = dest,
                        .target = 0,
                    },
                });

                const rhs = try self.compileExp(b.Right.*, worker);
                defer worker.freeReg(rhs);

                try self.putOptimizedBin(lhs, rhs, dest, .Eql, worker);

                worker.instructions.items[to_patch].jump_if_false.target = worker.instructions.items.len;

                return dest;
            }

            if (b.Op == .Or) {
                const dest = try worker.allocateReg();
                const to_patch = worker.instructions.items.len;
                try worker.instructions.append(self.allocator, Instruction{ .jump_if_true = .{
                    .state_reg = lhs,
                    .target = 0,
                } });

                const rhs = try self.compileExp(b.Right.*, worker);
                defer worker.freeReg(rhs);

                try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                    .src = rhs,
                    .dest = dest,
                } });

                const to_patch2 = worker.instructions.items.len;
                try worker.instructions.append(self.allocator, Instruction{ .jump = 0 });

                worker.instructions.items[to_patch].jump_if_true.target = worker.instructions.items.len;
                try worker.instructions.append(self.allocator, Instruction{ .copy = .{
                    .src = lhs,
                    .dest = dest,
                } });

                worker.instructions.items[to_patch2].jump = worker.instructions.items.len;
                return dest;
            }

            const rhs = try self.compileExp(b.Right.*, worker);
            defer worker.freeReg(rhs);

            const dest = try worker.allocateReg();

            try self.putOptimizedBin(lhs, rhs, dest, b.Op, worker);

            return dest;
        },
        .Function => |f| {
            var local_function = try Object.ObjFunction.createIndependant(
                self.allocator,
                null,
                f.Params.Names,
                f.Params.HasVararg,
            );

            var new_worker = Worker{
                .function = local_function,
                .allocator = self.allocator,
                .previous_worker = worker,
            };
            defer new_worker.deinit();

            for (f.Params.Names) |n| {
                _ = try new_worker.getOrCreateLocalData(n, false);
            }

            if (f.Block.len > 0) {
                try self.compileBlock(f.Block, &new_worker);
            }

            local_function.instructions = try new_worker.instructions.toOwnedSlice(self.allocator);
            local_function.constants = try new_worker.constants.toOwnedSlice(self.allocator);
            local_function.upvalues = try new_worker.upvalues.toOwnedSlice(self.allocator);

            const transfer = try worker.allocateReg();
            const function_constant = try worker.putConstant(local_function.object.asValue());

            try worker.instructions.append(self.allocator, Instruction{ .make_closure = .{
                .constant = function_constant,
                .dest = transfer,
            } });

            return transfer;
        },
        .TableConstructor => |tc| {
            const table_reg = try self.startTable(worker);

            var ops = std.ArrayList(Instruction.Operand).init(self.allocator);
            defer ops.deinit();

            for (tc) |iexp| {
                switch (iexp) {
                    .Exp => |e| try ops.append(try self.compileExp(e.*, worker)),
                    .Name => |n| try ops.append(try self.compileExp(n.Exp.*, worker)),
                    .Index => |i| try ops.append(try self.compileExp(i.Exp.*, worker)),
                }
            }

            const final_ops = try ops.toOwnedSlice();
            for (final_ops) |op| worker.freeReg(op);

            try worker.instructions.append(self.allocator, Instruction{
                .unwrap_tuple_save = final_ops,
            });

            for (tc, 0..) |field, idx| {
                switch (field) {
                    .Exp => {
                        if (idx + 1 == tc.len) {
                            try worker.instructions.append(self.allocator, Instruction{
                                .table_append_save_after = .{ .table = table_reg, .after = @intCast(idx) },
                            });
                        } else {
                            try worker.instructions.append(self.allocator, Instruction{
                                .table_append = .{
                                    .table = table_reg,
                                    .value = .{ .save_or_nil = @intCast(idx) },
                                },
                            });
                        }
                    },
                    .Name => |name| {
                        try worker.instructions.append(self.allocator, Instruction{ .table_set_by_str = .{
                            .table = table_reg,
                            .field = name.Name,
                            .value = .{ .save_or_nil = @intCast(idx) },
                        } });
                    },
                    .Index => |index| {
                        const table_index = try self.compileExp(index.Index.*, worker);

                        try worker.instructions.append(self.allocator, Instruction{ .table_set_by_value = .{
                            .table = table_reg,
                            .field = table_index,
                            .value = .{ .save_or_nil = @intCast(idx) },
                        } });
                    },
                }
            }

            return table_reg;
        },
        .Vararg => {
            return .vararg;
        },
        .Unary => |u| {
            const inner = try self.compileExp(u.Right.*, worker);
            defer worker.freeReg(inner);

            const dest = try worker.allocateReg();

            try worker.instructions.append(self.allocator, Instruction{ .unary_op = .{
                .op = u.Op,
                .src = inner,
                .dest = dest,
            } });

            return dest;
        },
    };
}

pub fn compilePrefix(self: *@This(), pref: AST.Prefix, worker: *Worker) anyerror!Instruction.Operand {
    return switch (pref) {
        .FunctionCall => |fc| (try self.funcCall(fc.*, worker, true)) orelse unreachable,
        .Var => |v| switch (v) {
            // Locals => Upvalues => Globals
            .Name => |name| {
                const dest = try worker.allocateReg();

                if (try worker.getLocalData(name)) |local_data| {
                    return .{ .local = local_data.symbol };
                }

                if (try self.findUpvalue(worker, name)) |upval| {
                    return .{ .upvalue = upval };
                }

                worker.freeReg(dest);

                return .{ .global = try self.getOrCreateGlobalSymbol(name) };
            },
            .DotAccess => |da| {
                const prefix_result = try self.compilePrefix(da.Prefix.*, worker);
                defer worker.freeReg(prefix_result);

                const field_result = try worker.allocateReg();

                try worker.instructions.append(self.allocator, Instruction{ .get_field_by_str = .{
                    .field = da.Key,
                    .table = prefix_result,
                    .dest = field_result,
                } });

                return field_result;
            },
            .TableAccess => |da| {
                const prefix_result = try self.compilePrefix(da.Prefix.*, worker);
                defer worker.freeReg(prefix_result);

                const key_result = try self.compileExp(da.Key.*, worker);
                defer worker.freeReg(key_result);

                const field_result = try worker.allocateReg();

                try worker.instructions.append(self.allocator, Instruction{ .get_field_by_value = .{
                    .field = key_result,
                    .table = prefix_result,
                    .dest = field_result,
                } });

                return field_result;
            },
        },
        .Parenthesis => |p| {
            return try self.compileExp(p.*, worker);
        },
    };
}
// TODO: implement search
pub fn fetchNil(_: *@This(), worker: *Worker) !Instruction.Operand {
    const constant_index = try worker.putConstant(Value.initNil());
    return .{ .constant = constant_index };
}

pub fn fetchBool(_: *@This(), worker: *Worker, state: bool) !Instruction.Operand {
    const constant_index = try worker.putConstant(Value.initBool(state));
    return .{ .constant = constant_index };
}

pub fn fetchString(self: *@This(), worker: *Worker, string: []const u8) !Instruction.Operand {
    const constant_index = try worker.putConstant((try Object.ObjString.createIndependantMoved(self.allocator, string)).object.asValue());
    return .{ .constant = constant_index };
}

pub fn fetchNumber(_: *@This(), number: f64, worker: *Worker) !Instruction.Operand {
    const constant_index = try worker.putConstant(Value.initNumber(number));
    return .{ .constant = constant_index };
}

pub fn startTable(self: *@This(), worker: *Worker) !Instruction.Operand {
    const dest = try worker.allocateReg();
    try worker.instructions.append(self.allocator, Instruction{ .new_table = dest });
    return dest;
}

// If need_output is false, then the result is 0
pub fn funcCall(self: *@This(), fc: AST.FunctionCall, worker: *Worker, comptime need_output: bool) anyerror!?Instruction.Operand {
    var reg_args: std.ArrayListUnmanaged(Instruction.Operand) = .{};
    defer reg_args.deinit(self.allocator);

    if (fc.Method) |_| {
        // No need to free, since it's done in final_reg_args
        const inner_prefix = try self.compilePrefix(fc.Prefix.*, worker);
        try reg_args.append(self.allocator, inner_prefix);
    }

    var has_tuple: bool = false;

    switch (fc.Args) {
        .Explist => |exp_list| {
            for (exp_list) |exp| {
                try reg_args.append(self.allocator, try self.compileExp(exp.*, worker));

                if (worker.lastInstructionProducedTuple())
                    has_tuple = true;
            }
        },
        .String => |str| {
            try reg_args.append(self.allocator, try self.fetchString(worker, str));
        },
        else => @panic("TODO"),
    }

    const final_reg_args = try reg_args.toOwnedSlice(self.allocator);
    defer for (final_reg_args) |fra| worker.freeReg(fra);

    const dest = try worker.allocateReg();
    if (!need_output) worker.freeReg(dest);

    const prefix = if (fc.Method) |method|
        try self.compilePrefix(.{ .Var = .{ .DotAccess = .{
            .Key = method,
            .Prefix = fc.Prefix,
        } } }, worker)
    else
        try self.compilePrefix(fc.Prefix.*, worker);

    if (final_reg_args.len > 255)
        return error.TooManyFunctionArguments;

    try worker.instructions.append(self.allocator, Instruction{
        .call_func_args = .{
            .args = final_reg_args,
            .has_tuple = if (has_tuple) .Yes else .No,
        },
    });
    try worker.instructions.append(self.allocator, Instruction{ .call_func = .{
        .func = prefix,
        .dest = dest,
    } });
    worker.freeReg(prefix);

    return if (need_output) dest else null;
}

pub fn addUpvalue(self: *@This(), worker: *Worker, symbol: Instruction.Symbol, is_local: bool) !Instruction.Symbol {
    for (worker.upvalues.items, 0..) |upvalue_data, i| {
        if (is_local == upvalue_data.is_local and upvalue_data.symbol == symbol)
            return @intCast(i);
    }

    if (worker.upvalues.items.len > std.math.maxInt(Instruction.Symbol)) {
        return error.TooManyClosureVariables;
    }

    const index = worker.upvalues.items.len;

    try worker.upvalues.append(self.allocator, .{
        .is_local = is_local,
        .symbol = symbol,
    });

    return @intCast(index);
}
