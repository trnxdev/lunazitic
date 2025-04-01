kind: Kind,
lexeme: []const u8 = "",
location: Location = .{},

// Keywords begin at 120
pub const Kind = enum(u8) {
    EOF,

    Name,
    Number,
    String,

    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Caret, // ^
    Hash, // #
    EqualEqual, // ==
    TildeEqual, // ~=
    LessEqual, // <=
    GreaterEqual, // >=
    Less, // <
    Greater, // >
    Equal, // =
    LeftParenthesis, // (
    RightParenthesis, // )
    LeftCurlyBrace, // {
    RightCurlyBrace, // }
    LeftBracket, // [
    RightBracket, // ]
    Semicolon, // ;
    Colon, // :
    Comma, // ,
    Dot, // .
    DotDot, // ..
    DotDotDot, // ...

    @"and" = 120,
    @"break",
    do,
    @"else",
    elseif,
    end,
    false,
    @"for",
    function,
    @"if",
    in,
    local,
    nil,
    not,
    @"or",
    repeat,
    @"return",
    then,
    true,
    until,
    @"while",

    pub fn is_keyword(self: @This()) bool {
        return @intFromEnum(self) >= 120;
    }
};

pub const Location = struct {
    offset: usize = 0,
    line_offset: usize = 0,
    line: usize = 1,
};
