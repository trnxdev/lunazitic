const std = @import("std");
const Token = @import("token.zig");
const TokenStream = @import("token_stream.zig");
const AST = @import("ast.zig");

arena: std.heap.ArenaAllocator,
token_stream: *TokenStream,
current: Token,

pub const Mentioned = std.ArrayList([]const u8);

// This creates an arena allocator, please use .deinit to free up
// the ast and the arena.
pub fn init(allocator: std.mem.Allocator, token_stream: *TokenStream) @This() {
    const arena = std.heap.ArenaAllocator.init(allocator);

    return .{
        .arena = arena,
        .token_stream = token_stream,
        .current = undefined,
    };
}

pub fn deinit(self: *@This()) void {
    self.arena.deinit();
}

pub fn arena_allocator(self: *@This()) std.mem.Allocator {
    return self.arena.allocator();
}

pub fn allocate(self: *@This(), comptime a_type: type, initial_value: a_type) !*a_type {
    const node = try self.arena_allocator().create(a_type);
    node.* = initial_value;
    return node;
}

pub fn advance(self: *@This()) !Token {
    const previous = self.current;
    self.current = try self.token_stream.read();
    return previous;
}

pub fn advanceIfCurrentKindEql(self: *@This(), kind: Token.Kind) !Token {
    if (self.current.kind != kind) {
        return error.UnexpectedToken;
    }

    return try self.advance();
}

pub fn parseRoot(self: *@This()) !AST.Block {
    _ = try self.advance();
    return try self.parseStmtUntil(&.{.EOF});
}

pub fn parseStmtUntil(self: *@This(), until: []const Token.Kind) anyerror!AST.Block {
    var statements = std.ArrayList(*AST.Stat).empty;
    defer statements.deinit(self.arena_allocator());

    o: while (true) {
        const current_kind = self.current.kind;

        for (until) |u| {
            if (current_kind == u)
                break :o;
        }

        while (self.current.kind == .Semicolon) {
            _ = try self.advanceIfCurrentKindEql(.Semicolon);
        }

        const statement = try self.parseStmt();
        try statements.append(self.arena_allocator(), statement);
    }

    return try statements.toOwnedSlice(self.arena_allocator());
}

pub fn parseStmt(self: *@This()) !*AST.Stat {
    const current_token = self.current;
    const current_location = self.token_stream.location;

    const result = switch (self.current.kind) {
        .Name, .LeftParenthesis => self.parseVarDecl(),
        .do => self.parseDo(),
        .@"while" => self.parseWhile(),
        .repeat => self.parseRepeat(),
        .@"if" => self.parseIf(),
        .@"break" => self.parseBreak(),
        .@"for" => self.parseFor(),
        .function => self.parseFunction(),
        .local => self.parseLocal(),
        .@"return" => self.parseReturn(),
        else => return error.ParsingError,
    };

    if (result == error.NotAValidVar) {
        self.token_stream.location = current_location;
        self.current = current_token;

        return try self.allocate(AST.Stat, .{
            .FunctionCall = try self.parseFuncCall(),
        });
    }

    return result;
}

pub fn parseReturn(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.@"return");

    const saved_token = self.current;
    const saved_location = self.token_stream.location;

    const maybe_expressions = self.parseExplist();

    if (!std.meta.isError(maybe_expressions)) {
        return try self.allocate(AST.Stat, .{
            .Return = maybe_expressions catch unreachable,
        });
    }

    self.token_stream.location = saved_location;
    self.current = saved_token;

    return try self.allocate(AST.Stat, .{
        .Return = &.{},
    });
}

pub fn parseVarDecl(self: *@This()) !*AST.Stat {
    const variables = try self.parseVarlist();
    _ = try self.advance();
    const expressions = try self.parseExplist();
    return try self.allocate(AST.Stat, .{
        .VarDecl = .{
            .Vars = variables,
            .Exps = expressions,
        },
    });
}

pub fn parseDo(self: *@This()) anyerror!*AST.Stat {
    _ = try self.advance();
    const block = try self.parseStmtUntil(&.{.end});
    _ = try self.advance();
    return try self.allocate(AST.Stat, .{
        .Do = block,
    });
}

pub fn parseWhile(self: *@This()) !*AST.Stat {
    _ = try self.advance();
    const exp = try self.parseExp();
    _ = try self.advance();
    const block = try self.parseStmtUntil(&.{.end});
    _ = try self.advance();
    return try self.allocate(AST.Stat, .{
        .While = .{
            .Exp = exp,
            .Block = block,
        },
    });
}

pub fn parseRepeat(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.repeat);
    const block = try self.parseStmtUntil(&.{.until});
    _ = try self.advanceIfCurrentKindEql(.until);
    const exp = try self.parseExp();
    return try self.allocate(AST.Stat, .{
        .Repeat = .{
            .Block = block,
            .Exp = exp,
        },
    });
}

pub fn parseIf(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.@"if");
    const condition = try self.parseExp();
    _ = try self.advanceIfCurrentKindEql(.then);

    const then_block = try self.parseStmtUntil(&.{ .elseif, .@"else", .end });

    var else_ifs = std.ArrayList(AST.Stat.ElseIf).empty;
    defer else_ifs.deinit(self.arena_allocator());

    while (self.current.kind == .elseif) {
        _ = try self.advanceIfCurrentKindEql(.elseif);
        const elseif_condition = try self.parseExp();
        _ = try self.advanceIfCurrentKindEql(.then);
        const elseif_block = try self.parseStmtUntil(&.{ .elseif, .@"else", .end });
        try else_ifs.append(self.arena_allocator(), (.{
            .Exp = elseif_condition,
            .Block = elseif_block,
        }));
    }

    var else_block: ?AST.Block = null;

    if (self.current.kind == .@"else") {
        _ = try self.advanceIfCurrentKindEql(.@"else");
        else_block = try self.parseStmtUntil(&.{.end});
    }

    _ = try self.advanceIfCurrentKindEql(.end);

    return try self.allocate(AST.Stat, .{ .If = .{
        .Exp = condition,
        .Block = then_block,
        .ElseIfs = try else_ifs.toOwnedSlice(self.arena_allocator()),
        .Else = else_block,
    } });
}

pub fn parseFor(self: *@This()) !*AST.Stat {
    const current_token = self.current;
    const current_location = self.token_stream.location;

    _ = try self.advanceIfCurrentKindEql(.@"for");

    const name = try self.parseName();

    if (self.current.kind == .Comma or self.current.kind == .in) {
        self.token_stream.location = current_location;
        self.current = current_token;
        return try self.parseGenericFor();
    }

    _ = try self.advanceIfCurrentKindEql(.Equal);
    const start_value = try self.parseExp();
    _ = try self.advanceIfCurrentKindEql(.Comma);
    const end_value = try self.parseExp();
    var step_value: ?*AST.Exp = null;

    if (self.current.kind == .Comma) {
        _ = try self.advance();
        step_value = try self.parseExp();
    }

    _ = try self.advanceIfCurrentKindEql(.do);
    const block = try self.parseStmtUntil(&.{.end});
    _ = try self.advanceIfCurrentKindEql(.end);

    return try self.allocate(AST.Stat, .{
        .NumericFor = .{
            .Name = name,
            .Start = start_value,
            .End = end_value,
            .Step = step_value,
            .Block = block,
        },
    });
}

pub fn parseGenericFor(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.@"for");

    const names = try self.parseNamelist();

    _ = try self.advanceIfCurrentKindEql(.in);
    const expressions = try self.parseExplist();
    _ = try self.advanceIfCurrentKindEql(.do);
    const block = try self.parseStmtUntil(&.{.end});
    _ = try self.advanceIfCurrentKindEql(.end);

    return try self.allocate(AST.Stat, .{
        .GenericFor = .{
            .Names = names,
            .Exps = expressions,
            .Block = block,
        },
    });
}

pub fn parseFunction(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.function);
    const name = try self.parseFuncName();
    const body = try self.parseFuncBody();
    return try self.allocate(AST.Stat, .{
        .Function = .{
            .Name = name,
            .Body = body,
        },
    });
}

pub fn parseLocal(self: *@This()) !*AST.Stat {
    _ = try self.advance();
    return switch (self.current.kind) {
        .function => {
            _ = try self.advanceIfCurrentKindEql(.function);
            const name = try self.parseName();
            const body = try self.parseFuncBody();
            return try self.allocate(AST.Stat, .{
                .LocalFunction = .{
                    .Name = name,
                    .Body = body,
                },
            });
        },
        else => {
            const names = try self.parseNamelist();
            var expressions: ?AST.Explist = null;

            if (self.current.kind == .Equal) {
                _ = try self.advance();
                expressions = try self.parseExplist();
            }

            return try self.allocate(AST.Stat, .{
                .LocalVarDecl = .{
                    .Names = names,
                    .Exps = expressions,
                },
            });
        },
    };
}

pub fn parseVarlist(self: *@This()) !AST.Varlist {
    var variables = std.ArrayList(AST.Var).empty;
    defer variables.deinit(self.arena_allocator());

    var parse_result = self.parseVar();

    o: while (!std.meta.isError(parse_result)) : (parse_result = self.parseVar()) {
        try variables.append(self.arena_allocator(), (parse_result catch unreachable));

        if (self.current.kind != .Comma) {
            break :o;
        }

        _ = try self.advance();
    }

    if (variables.items.len == 0) {
        return error.NotAValidVar;
    }

    return try variables.toOwnedSlice(self.arena_allocator());
}

pub fn parseVar(self: *@This()) !AST.Var {
    const prefix_expr = try self.parsePrefix();

    if (prefix_expr.* != .Var) {
        return error.NotValidVar;
    }

    return prefix_expr.Var;
}

pub fn parseFuncCall(self: *@This()) !*AST.FunctionCall {
    const prefix_expr = try self.parsePrefix();

    if (prefix_expr.* != .FunctionCall) {
        return error.NotAFunctionCall;
    }

    return prefix_expr.FunctionCall;
}

pub fn parsePrefix(self: *@This()) !*AST.Prefix {
    const prefix_node = try self.allocate(AST.Prefix, undefined);

    if (self.current.kind == .LeftParenthesis) {
        _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
        const exp = try self.parseExp();
        _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
        prefix_node.* = .{ .Parenthesis = exp };
    } else if (self.current.kind == .Name) {
        const name = try self.parseName();
        prefix_node.* = .{ .Var = .{ .Name = name } };
    } else {
        return error.UnexpectedToken;
    }

    const prefix_expr = try self.allocate(AST.Exp, .{
        .Prefix = prefix_node,
    });

    o: while (true) {
        switch (self.current.kind) {
            .Colon => {
                _ = try self.advance();
                const name = try self.parseName();
                const prev_prefix = try prefix_expr.Prefix.clone(self.arena_allocator());
                prefix_expr.Prefix.* = .{
                    .FunctionCall = try self.allocate(AST.FunctionCall, .{
                        .Prefix = prev_prefix,
                        .Args = try self.parseArgs(),
                        .Method = name,
                    }),
                };
            },
            .Dot => {
                _ = try self.advance();
                const name = try self.parseName();
                const prev_prefix = try prefix_expr.Prefix.clone(self.arena_allocator());
                prefix_expr.Prefix.* = .{
                    .Var = .{
                        .DotAccess = .{
                            .Prefix = prev_prefix,
                            .Key = name,
                        },
                    },
                };
            },
            .LeftBracket => {
                _ = try self.advance();
                const exp = try self.parseExp();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);
                const prev_prefix = try prefix_expr.Prefix.clone(self.arena_allocator());
                prefix_expr.Prefix.* = .{
                    .Var = .{
                        .TableAccess = .{
                            .Prefix = prev_prefix,
                            .Key = exp,
                        },
                    },
                };
            },
            .LeftParenthesis, .String, .LeftCurlyBrace => {
                const args = try self.parseArgs();
                const prev_prefix = try prefix_expr.Prefix.clone(self.arena_allocator());

                prefix_expr.Prefix.* = .{
                    .FunctionCall = try self.allocate(AST.FunctionCall, .{
                        .Prefix = prev_prefix,
                        .Args = args,
                        .Method = null,
                    }),
                };
            },
            else => break :o,
        }
    }

    return prefix_expr.Prefix;
}

pub fn parseNamelist(self: *@This()) anyerror!AST.Namelist {
    var names = std.ArrayList([]const u8).empty;
    defer names.deinit(self.arena_allocator());

    o: while (self.current.kind == .Name) {
        const name = try self.parseName();
        try names.append(self.arena_allocator(), name);

        if (self.current.kind != .Comma) {
            break :o;
        }

        _ = try self.advance();
    }

    return try names.toOwnedSlice(self.arena_allocator());
}

pub fn parseExplist(self: *@This()) !AST.Explist {
    var expressions = std.ArrayList(*AST.Exp).empty;
    defer expressions.deinit(self.arena_allocator());

    while (self.current.kind != .Semicolon) {
        const exp = try self.parseExp();
        try expressions.append(self.arena_allocator(), exp);

        if (self.current.kind != .Comma) {
            break;
        }

        _ = try self.advance();
    }

    return try expressions.toOwnedSlice(self.arena_allocator());
}

pub fn parseName(self: *@This()) !AST.Name {
    const name = self.current;
    _ = try self.advanceIfCurrentKindEql(.Name);
    // TODO: Is dupe necessary?
    return try self.arena_allocator().dupe(u8, name.lexeme);
}

pub fn parseFuncName(self: *@This()) !AST.FuncName {
    var name_parts = std.ArrayList(AST.Name).empty;
    defer name_parts.deinit(self.arena_allocator());

    while (self.current.kind == .Name) {
        const name = try self.parseName();
        try name_parts.append(self.arena_allocator(), name);

        if (self.current.kind != .Dot) {
            break;
        }

        _ = try self.advance();
    }

    var method: ?AST.Name = null;

    if (self.current.kind == .Colon) {
        _ = try self.advance();
        method = try self.parseName();
    }

    return AST.FuncName{
        .Names = try name_parts.toOwnedSlice(self.arena_allocator()),
        .Method = method,
    };
}

pub fn parseFuncBody(self: *@This()) !AST.FuncBody {
    _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);
    const parameters = try self.parseParlist();
    _ = try self.advanceIfCurrentKindEql(.RightParenthesis);
    const block = try self.parseStmtUntil(&.{.end});

    _ = try self.advanceIfCurrentKindEql(.end);
    return AST.FuncBody{
        .Block = block,
        .Params = parameters,
    };
}

pub fn parseParlist(self: *@This()) !AST.Parlist {
    const names = try self.parseNamelist();

    if (self.current.kind == .Comma) {
        _ = try self.advance();
    }

    const vararg = self.current.kind == .DotDotDot;
    if (vararg) {
        _ = try self.advance();
    }

    return AST.Parlist{
        .Names = names,
        .HasVararg = vararg,
    };
}

// Make a recursive descent parser for expressions
// or
// and
// <     >     <=    >=    ~=    ==
// ..
// +     -
// *     /     %
// not   #     - (unary)
// ^
pub fn parseExp(self: *@This()) anyerror!*AST.Exp {
    return self.parseOr();
}

pub fn parseOr(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseAnd();

    while (self.current.kind == .@"or") {
        _ = try self.advance();
        const right = try self.parseAnd();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = .Or,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseAnd(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseRelational();

    while (self.current.kind == .@"and") {
        _ = try self.advance();
        const right = try self.parseRelational();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = .And,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseRelational(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseConcat();

    while (true) {
        const op = switch (self.current.kind) {
            .Less => AST.BinaryOp.Lt,
            .Greater => AST.BinaryOp.Gt,
            .LessEqual => AST.BinaryOp.Lte,
            .GreaterEqual => AST.BinaryOp.Gte,
            .TildeEqual => AST.BinaryOp.Neql,
            .EqualEqual => AST.BinaryOp.Eql,
            else => break,
        };

        _ = try self.advance();
        const right = try self.parseConcat();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = op,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseConcat(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseAdditive();

    while (self.current.kind == .DotDot) {
        _ = try self.advance();
        const right = try self.parseAdditive();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = .Concat,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseAdditive(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseMultiplicative();

    while (true) {
        const op = switch (self.current.kind) {
            .Plus => AST.BinaryOp.Add,
            .Minus => AST.BinaryOp.Sub,
            else => break,
        };

        _ = try self.advance();
        const right = try self.parseMultiplicative();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = op,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseMultiplicative(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parseUnary();

    while (true) {
        const op = switch (self.current.kind) {
            .Star => AST.BinaryOp.Mul,
            .Slash => AST.BinaryOp.Div,
            .Percent => AST.BinaryOp.Mod,
            else => break,
        };

        _ = try self.advance();
        const right = try self.parseUnary();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = op,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parseUnary(self: *@This()) anyerror!*AST.Exp {
    var unary_ops = std.ArrayList(AST.UnaryOp).empty;

    o: while (true) {
        switch (self.current.kind) {
            .not, .Minus, .Hash => {
                const op = switch (self.current.kind) {
                    .not => AST.UnaryOp.Not,
                    .Minus => AST.UnaryOp.Neg,
                    .Hash => AST.UnaryOp.Len,
                    else => unreachable,
                };

                try unary_ops.append(self.arena_allocator(), op);
                _ = try self.advance();
            },
            else => break :o,
        }
    }

    if (unary_ops.items.len >= 1) {
        const inner = try self.power();

        while (unary_ops.pop()) |unop| {
            const inner_copy = try self.arena_allocator().create(AST.Exp);
            inner_copy.* = inner.*;
            inner.* = .{
                .Unary = .{
                    .Op = unop,
                    .Right = inner_copy,
                },
            };
        }

        return inner;
    }

    return try self.power();
}

pub fn power(self: *@This()) anyerror!*AST.Exp {
    var left = try self.parsePrimary();

    while (self.current.kind == .Caret) {
        _ = try self.advance();
        const right = try self.parseUnary();
        left = try self.allocate(AST.Exp, .{
            .Binary = .{
                .Op = .Pow,
                .Left = try left.clone(self.arena_allocator()),
                .Right = right,
            },
        });
    }

    return left;
}

pub fn parsePrimary(self: *@This()) !*AST.Exp {
    return switch (self.current.kind) {
        .nil => try self.parseNil(),
        .Number => try self.parseNumber(),
        .true, .false => try self.parseBoolean(),
        .String => try self.parseString(),
        .function => try self.parseFunctionExpr(),
        .LeftCurlyBrace => try self.parseTableConstructor(),
        .DotDotDot => try self.parseVararg(),
        else => try self.allocate(AST.Exp, .{ .Prefix = try self.parsePrefix() }),
    };
}

pub fn parseVararg(self: *@This()) !*AST.Exp {
    _ = try self.advanceIfCurrentKindEql(.DotDotDot);
    return try self.allocate(AST.Exp, .{ .Vararg = void{} });
}

pub fn parseBreak(self: *@This()) !*AST.Stat {
    _ = try self.advanceIfCurrentKindEql(.@"break");
    return try self.allocate(AST.Stat, .{ .Break = void{} });
}

pub fn parseNil(self: *@This()) !*AST.Exp {
    _ = try self.advanceIfCurrentKindEql(.nil);
    return try self.allocate(AST.Exp, .{ .Nil = void{} });
}

pub fn parseBoolean(self: *@This()) !*AST.Exp {
    const boolean = self.current;
    std.debug.assert(boolean.kind == .true or boolean.kind == .false);
    _ = try self.advance();
    return try self.allocate(AST.Exp, .{
        .Bool = boolean.kind == .true,
    });
}

pub fn parseFunctionExpr(self: *@This()) !*AST.Exp {
    _ = try self.advanceIfCurrentKindEql(.function);
    const body = try self.parseFuncBody();
    return try self.allocate(AST.Exp, .{
        .Function = body,
    });
}

pub fn parseArgs(self: *@This()) !AST.Args {
    return switch (self.current.kind) {
        .LeftParenthesis => {
            _ = try self.advanceIfCurrentKindEql(.LeftParenthesis);

            var arguments = std.ArrayList(*AST.Exp).empty;
            defer arguments.deinit(self.arena_allocator());

            while (self.current.kind != .RightParenthesis) {
                const exp = try self.parseExp();
                try arguments.append(self.arena_allocator(), exp);
                if (self.current.kind != .Comma and self.current.kind == .RightParenthesis) {
                    break;
                }

                _ = try self.advance();
            }

            _ = try self.advanceIfCurrentKindEql(.RightParenthesis);

            return AST.Args{ .Explist = try arguments.toOwnedSlice(self.arena_allocator()) };
        },
        .LeftCurlyBrace => {
            const table = try self.parseTableConstructor();
            return AST.Args{ .TableConstructor = table.TableConstructor };
        },
        .String => {
            const string = self.current;
            _ = try self.advance();
            return AST.Args{ .String = string.lexeme };
        },
        else => error.UnexpectedToken,
    };
}

// tableconstructor ::= `{` [fieldlist] `}`
// fieldlist ::= field {fieldsep field} [fieldsep]
// field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp
// fieldsep ::= `,´ | `;´

pub fn parseTableConstructor(self: *@This()) !*AST.Exp {
    _ = try self.advanceIfCurrentKindEql(.LeftCurlyBrace);

    var fields = std.ArrayList(AST.Field).empty;
    defer fields.deinit(self.arena_allocator());

    while (self.current.kind != .RightCurlyBrace) {
        const field: AST.Field = switch (self.current.kind) {
            .LeftBracket => o: {
                _ = try self.advance();
                const index = try self.parseExp();
                _ = try self.advanceIfCurrentKindEql(.RightBracket);
                _ = try self.advanceIfCurrentKindEql(.Equal);
                const exp = try self.parseExp();
                break :o .{
                    .Index = .{
                        .Index = index,
                        .Exp = exp,
                    },
                };
            },
            .Name => o: {
                const saved_token = self.current;
                const saved_location = self.token_stream.location;

                const name = try self.parseName();

                if (self.current.kind != .Equal) {
                    self.token_stream.location = saved_location;
                    self.current = saved_token;
                    const exp = try self.parseExp();
                    break :o .{
                        .Exp = exp,
                    };
                }

                _ = try self.advanceIfCurrentKindEql(.Equal);
                const exp = try self.parseExp();

                break :o .{
                    .Name = .{
                        .Name = name,
                        .Exp = exp,
                    },
                };
            },
            else => o: {
                const exp = try self.parseExp();
                break :o .{ .Exp = exp };
            },
        };

        try fields.append(self.arena_allocator(), (field));

        if (self.current.kind != .Comma and self.current.kind != .Semicolon) {
            break;
        }

        _ = try self.advance();
    }

    _ = try self.advanceIfCurrentKindEql(.RightCurlyBrace);

    return try self.allocate(AST.Exp, .{
        .TableConstructor = try fields.toOwnedSlice(self.arena_allocator()),
    });
}

pub fn parseNumber(self: *@This()) !*AST.Exp {
    const number = self.current;
    _ = try self.advance();
    return try self.allocate(AST.Exp, .{
        .Number = try std.fmt.parseFloat(f64, number.lexeme),
    });
}

pub fn parseString(self: *@This()) !*AST.Exp {
    const string = self.current;
    _ = try self.advance();
    return try self.allocate(AST.Exp, .{
        .String = string.lexeme,
    });
}

pub fn parseNameExp(self: *@This()) !*AST.Exp {
    const name = try self.advanceIfCurrentKindEql(.Name);
    return try self.allocate(AST.Exp, .{
        .Prefix = try self.allocate(AST.Prefix, .{
            .Var = .{
                .Name = name.lexeme,
            },
        }),
    });
}

pub fn parseGrouped(self: *@This()) !*AST.Exp {
    _ = try self.advance();
    const exp = try self.parseExp();
    _ = try self.advance();
    return try self.allocate(AST.Exp, .{ .Prefix = try self.allocate(AST.Prefix, .{
        .Parenthesis = exp,
    }) });
}
