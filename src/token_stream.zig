const std = @import("std");
const Token = @import("token.zig");

buffer: []const u8,
location: Token.Location,
allocator: std.mem.Allocator, //TODO: get rid of this

pub fn read(self: *@This()) !Token {
    self.skipUnreadable();

    if (self.isEOF())
        return self.createTokenIL(.EOF, "<EOF>");

    const character = self.advance();

    return switch (character) {
        '+' => self.createTokenIL(.Plus, "+"),
        '-' => self.createTokenIL(.Minus, "-"),
        '*' => self.createTokenIL(.Star, "*"),
        '%' => self.createTokenIL(.Percent, "%"),
        '^' => self.createTokenIL(.Caret, "^"),
        ':' => self.createTokenIL(.Colon, ":"),
        '#' => self.createTokenIL(.Hash, "#"),
        '(' => self.createTokenIL(.LeftParenthesis, "("),
        ')' => self.createTokenIL(.RightParenthesis, ")"),
        '{' => self.createTokenIL(.LeftCurlyBrace, "{"),
        '}' => self.createTokenIL(.RightCurlyBrace, "}"),
        ']' => self.createTokenIL(.RightBracket, "]"),
        ';' => self.createTokenIL(.Semicolon, ";"),
        ',' => self.createTokenIL(.Comma, ","),
        '/' => self.createTokenIL(.Slash, "/"),
        // == Two Characters
        '[' => if (self.match("["))
            try self.stringToken(.LongBracket)
        else
            self.createTokenIL(.LeftBracket, "["),
        '=' => if (self.match("="))
            self.createTokenIL(.EqualEqual, "==")
        else
            self.createTokenIL(.Equal, "="),
        '~' => if (self.match("="))
            self.createTokenIL(.TildeEqual, "~=")
        else
            error.UnknownCharacter,
        '<' => if (self.match("="))
            self.createTokenIL(.LessEqual, "<=")
        else
            self.createTokenIL(.Less, "<"),
        '>' => if (self.match("="))
            self.createTokenIL(.GreaterEqual, ">=")
        else
            self.createTokenIL(.Greater, ">"),
        // == Multiple Characters
        // ..., .., .
        '.' => if (self.match(".."))
            self.createTokenIL(.DotDotDot, "...")
        else if (self.match("."))
            self.createTokenIL(.DotDot, "..")
        else if (std.ascii.isDigit(self.peek()))
            self.numberToken()
        else
            self.createTokenIL(.Dot, "."),
        // == Literals
        '0'...'9' => self.numberToken(),
        'a'...'z', 'A'...'Z', '_' => self.nameToken(),
        '"' => try self.stringToken(.DoubleQuoted),
        '\'' => try self.stringToken(.SingleQuoted),
        // Long Bracket above
        else => error.UnknownCharacter,
    };
}

pub fn readToArrayUntilEOF(self: *@This(), dest: *std.ArrayList(Token)) !void {
    var token: Token = Token{ .kind = .EOF };
    var is_first_iteration: bool = true;

    o: while (is_first_iteration or token.kind != .EOF) : (token = try self.read()) {
        if (is_first_iteration) {
            is_first_iteration = false;
            continue :o;
        }

        try dest.append(self.allocator, token);
    }
}

pub fn skipUnreadable(self: *@This()) void {
    o: while (!self.isEOF()) {
        switch (self.peek()) {
            '-' => {
                if (self.match("--")) {
                    if (self.match("[[")) {
                        while (!self.match("]]") and !self.isEOF()) {
                            if (self.peek() == '\n')
                                self.location.line += 1;

                            _ = self.advance();
                        }
                    } else {
                        while (self.peek() != '\n') {
                            _ = self.advance();
                        }
                        self.location.line += 1;
                    }
                } else {
                    break :o;
                }
            },
            ' ', '\t' => _ = self.advance(),
            '\n' => {
                self.location.line += 1;
                self.location.line_offset = 0;
                _ = self.advance();
            },
            '\r' => {
                _ = self.advance();
            },
            else => break :o,
        }
    }
}

pub fn nameToken(self: *@This()) Token {
    self.location.offset -= 1;
    self.location.line_offset -= 1;

    const begin = self.location.offset;

    o: while (true) : (_ = self.advance()) {
        if (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
            continue :o;
        }

        break;
    }

    const lexeme = self.buffer[begin..self.location.offset];

    if (std.meta.stringToEnum(Token.Kind, lexeme)) |kind| {
        if (kind.is_keyword())
            return self.createTokenIL(kind, lexeme);
    }

    return self.createTokenIL(.Name, lexeme);
}

// IL = Inherit Location (from the stream)
fn createTokenIL(self: @This(), kind: Token.Kind, lexeme: []const u8) Token {
    return Token{
        .kind = kind,
        .lexeme = lexeme,
        .location = self.location,
    };
}

fn peek(self: @This()) u8 {
    if (self.isEOF())
        return 0;

    return self.buffer[self.location.offset];
}

fn advance(self: *@This()) u8 {
    if (self.location.offset + 1 > self.buffer.len)
        return 0;

    defer self.location.offset += 1;
    defer self.location.line_offset += 1;
    return self.peek();
}

pub fn isEOF(self: @This()) bool {
    return self.location.offset >= self.buffer.len;
}

fn numberToken(self: *@This()) !Token {
    var number_literal = std.ArrayList(u8).empty;
    defer number_literal.deinit(self.allocator);

    var is_hexadecimal: bool = false;
    var is_float: bool = false;
    var has_exponent: bool = false;

    self.location.offset -= 1;
    self.location.line_offset -= 1;

    if (self.match("0x") or self.match("0X")) {
        is_hexadecimal = true;

        // == Hexadecimal
        try number_literal.appendSlice(self.allocator, "0x");
    }

    while (true) : (_ = self.advance()) {
        if (is_hexadecimal and isHexadecimalLike(self.peek()))
            try number_literal.append(self.allocator, self.peek())
        else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
            try number_literal.append(self.allocator, self.peek())
        else
            break;
    }

    if (self.match(".."))
        return error.Malformed;

    if (self.match(".")) {
        is_float = true;
        try number_literal.append(self.allocator, '.');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecimalLike(self.peek()))
                try number_literal.append(self.allocator, self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try number_literal.append(self.allocator, self.peek())
            else
                break;
        }
    }

    // zig fmt: off
    if ((is_hexadecimal and self.match("P") or self.match("p"))
    or (!is_hexadecimal and self.match("E") or self.match("e"))) {
    // zig fmt: on
        has_exponent = true;
        try number_literal.append(self.allocator, std.ascii.toUpper(self.buffer[self.location.offset - 1]));

        if (self.match("-"))
            try number_literal.append(self.allocator, '-')
        else if (self.match("+"))
            try number_literal.append(self.allocator, '+');

        while (true) : (_ = self.advance()) {
            if (is_hexadecimal and isHexadecimalLike(self.peek()))
                try number_literal.append(self.allocator, self.peek())
            else if (!is_hexadecimal and std.ascii.isDigit(self.peek()))
                try number_literal.append(self.allocator, self.peek())
            else
                break;
        }
    }

    return self.createTokenIL(.Number, try number_literal.toOwnedSlice(self.allocator));
}

fn match(self: *@This(), to_match: []const u8) bool {
    const adjust = to_match.len;
    const end = self.location.offset + adjust;

    if (end > self.buffer.len)
        return false;

    const matches = std.mem.eql(
        u8,
        self.buffer[self.location.offset..end],
        to_match,
    );

    if (matches) {
        self.location.offset += adjust;
        self.location.line_offset += adjust;
    }

    return matches;
}

fn isHexadecimalLike(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        'a'...'f', 'A'...'F' => true,
        else => false,
    };
}

const LuaStringType = enum {
    SingleQuoted,
    DoubleQuoted,
    LongBracket,

    fn isQuoted(self: LuaStringType) bool {
        return self != .LongBracket;
    }
};
const StringLiteralTokenError = error{
    UnterminatedString,
    InvalidEscapeChar,
    NotHexadec,
    Utf8CannotEncodeSurrogateHalf,
    CodepointTooLarge,
    UnicodeBraceMissing,
    UnterminatedUnicodeBrace,
} || std.mem.Allocator.Error || std.fmt.ParseIntError;
fn stringToken(self: *@This(), string_type: LuaStringType) StringLiteralTokenError!Token {
    var output = std.ArrayListUnmanaged(u8).empty;
    defer output.deinit(self.allocator);

    o: while (true) {
        if (self.isEOF())
            return error.UnterminatedString;

        const char = self.advance();

        if (string_type == .SingleQuoted and char == '\'')
            break :o;

        if (string_type == .DoubleQuoted and char == '"')
            break :o;

        // FIXME: Support = in Bracket strings
        if (string_type == .LongBracket and char == ']' and self.match("]"))
            break :o;

        if (string_type.isQuoted()) {
            if (char == '\n')
                return error.UnterminatedString;

            if (char == '\\') {
                const escaped_char = self.advance();

                // 3.1 - Lexical Conventions (https://www.lua.org/manual/5.4/manual.html#3)
                switch (escaped_char) {
                    'a' => try output.append(self.allocator, std.ascii.control_code.bel), // Bell)
                    'b' => try output.append(self.allocator, std.ascii.control_code.bs), // Backspace)
                    'f' => try output.append(self.allocator, std.ascii.control_code.ff), // Form Feed)
                    'n' => try output.append(self.allocator, '\n'), // Newline)
                    'r' => try output.append(self.allocator, '\r'), // Carriage Return)
                    't' => try output.append(self.allocator, '\t'), // Horizontal Tab)
                    'v' => try output.append(self.allocator, std.ascii.control_code.vt), // Vertical Tab)
                    '"' => try output.append(self.allocator, '"'),
                    '[' => try output.append(self.allocator, '['),
                    ']' => try output.append(self.allocator, ']'),
                    '\'' => try output.append(self.allocator, ('\'')),
                    '\\' => try output.append(self.allocator, ('\\')),
                    'z' => _ = self.advance(),
                    '\n' => try output.append(self.allocator, ('\n')),
                    'u' => {
                        if (!self.match("{"))
                            return error.UnicodeBraceMissing;

                        const start = self.location.offset;

                        while (self.peek() != '}' and !self.isEOF()) {
                            _ = self.advance();
                        }

                        if (self.isEOF())
                            return error.UnterminatedUnicodeBrace;

                        _ = self.advance(); // Closing }

                        var utf8_output: [8]u8 = undefined;
                        const unicode_codepoint = self.buffer[start..self.location.offset];

                        _ = try std.unicode.utf8Encode(
                            try std.fmt.parseInt(u21, unicode_codepoint, 16),
                            &utf8_output,
                        );

                        try output.appendSlice(self.allocator, &utf8_output);
                    },
                    'x' => {
                        const hexadecimal_0 = self.advance();
                        const hexadecimal_1 = self.advance();

                        if (!isHexadecimalLike(hexadecimal_0) or !isHexadecimalLike(hexadecimal_1))
                            return error.NotHexadec;

                        const hexadecimal_char = try std.fmt.parseInt(
                            u8,
                            &[_]u8{ hexadecimal_0, hexadecimal_1 },
                            16,
                        );

                        try output.append(self.allocator, hexadecimal_char);
                    },
                    '0'...'9' => {
                        const start = self.location.offset - 1;

                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();
                        if (std.ascii.isDigit(self.peek()))
                            _ = self.advance();

                        const ascii_char = try std.fmt.parseInt(
                            u8,
                            self.buffer[start..self.location.offset],
                            10,
                        );
                        try output.append(self.allocator, ascii_char);
                    },
                    else => return error.InvalidEscapeChar,
                }

                continue :o;
            }
        }

        try output.append(self.allocator, char);
    }

    return self.createTokenIL(.String, try output.toOwnedSlice(self.allocator));
}
