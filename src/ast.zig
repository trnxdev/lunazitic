pub const Chunk = []*Stat;
pub const Block = Chunk;

pub const Stat = union(enum) {
    // stat ::= return [explist]
    Return: Explist,
    // stat ::= break
    Break,
    // stat :: = varlist '=' explist
    VarDecl: struct {
        Vars: Varlist,
        Exps: Explist,
    },
    // stat ::= functioncall
    FunctionCall: *FunctionCall,
    // stat ::= do block end
    Do: Block,
    // stat ::= while exp do block end
    While: struct {
        Exp: *Exp,
        Block: Block,
    },
    // stat ::= repeat block until exp
    Repeat: struct {
        Block: Block,
        Exp: *Exp,
    },
    // stat ::= if exp then block {elseif exp then block} [else block] end
    If: struct {
        Exp: *Exp,
        Block: Block,
        ElseIfs: []ElseIf,
        Else: ?Block,
    },
    // stat ::= for Name '=' exp ',' exp [',' exp] do block end
    NumericFor: struct {
        Name: Name,
        Start: *Exp,
        End: *Exp,
        Step: ?*Exp,
        Block: Block,
    },
    // stat ::= for namelist in explist do block end
    GenericFor: struct {
        Names: Namelist,
        Exps: Explist,
        Block: Block,
    },
    // stat ::= function funcname funcbody
    Function: struct {
        Name: FuncName,
        Body: FuncBody,
    },
    // stat ::= local function Name funcbody
    LocalFunction: struct {
        Name: Name,
        Body: FuncBody,
    },
    // stat ::= local namelist ['=' explist]
    LocalVarDecl: struct {
        Names: Namelist,
        Exps: ?Explist,
    },

    pub const ElseIf = struct {
        Exp: *Exp,
        Block: Block,
    };

    pub fn clone(self: *Stat, alloc: @import("std").mem.Allocator) !*Stat {
        const all_exp = try alloc.create(Stat);
        all_exp.* = self.*;
        return all_exp;
    }
};

// funcname ::= Name {‘.’ Name} [‘:’ Name]
pub const FuncName = struct {
    Names: []Name,
    Method: ?Name,
};

pub const Varlist = []Var;
pub const Var = union(enum) {
    Name: Name,
    TableAccess: struct {
        Prefix: *Prefix,
        Key: *Exp,
    },
    DotAccess: struct {
        Prefix: *Prefix,
        Key: Name,
    },
};

pub const Namelist = []Name;
pub const Name = []const u8;

pub const Explist = []*Exp;
pub const Exp = union(enum) {
    Nil,
    Bool: bool,
    Number: f64,
    String: []const u8,
    Vararg,
    Function: Function,
    Prefix: *Prefix,
    TableConstructor: TableConstructor,
    Binary: struct {
        Op: BinaryOp,
        Left: *Exp,
        Right: *Exp,
    },
    Unary: struct {
        Op: UnaryOp,
        Right: *Exp,
    },

    pub fn clone(self: *Exp, alloc: @import("std").mem.Allocator) !*Exp {
        const all_exp = try alloc.create(Exp);
        all_exp.* = self.*;
        return all_exp;
    }
};

pub const Prefix = union(enum) {
    Var: Var,
    FunctionCall: *FunctionCall,
    Parenthesis: *Exp,

    pub fn clone(self: *Prefix, alloc: @import("std").mem.Allocator) !*Prefix {
        const all_exp = try alloc.create(Prefix);
        all_exp.* = self.*;
        return all_exp;
    }
};

pub const FunctionCall = struct {
    Prefix: *Prefix,
    Method: ?Name,
    Args: Args,
};

pub const Args = union(enum) {
    String: []const u8,
    TableConstructor: TableConstructor,
    Explist: Explist,
};

pub const Function = FuncBody;
pub const FuncBody = struct {
    Params: Parlist,
    Block: Block,
};
pub const Parlist = struct {
    Names: Namelist,
    HasVararg: bool,
};

// tableconstructor ::= '{' [fieldlist] '}'
pub const TableConstructor = Fieldlist;
// fieldlist ::= field {fieldsep field} [fieldsep]
pub const Fieldlist = []Field;
// field ::= '[' exp ']' '=' exp | Name '=' exp | exp
pub const Field = union(enum) {
    Exp: *Exp,
    Name: struct {
        Name: Name,
        Exp: *Exp,
    },
    Index: struct {
        Index: *Exp,
        Exp: *Exp,
    },
};
// fieldsep ::= ',' | ';'
// binop ::= `+´ | `-´ | `*´ | `/´ | `^´
// | `%´ | `..´ | `<´ | `<=´ | `>´ | `>=´
// | `==´ | `~=´ | and | or
pub const BinaryOp = enum {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Concat,
    Lt,
    Lte,
    Gt,
    Gte,
    Eql,
    Neql,
    And,
    Or,
};

// unop ::= `-´ | not | `#´
pub const UnaryOp = enum {
    Neg,
    Not,
    Len,
};
