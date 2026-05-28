const std = @import("std");
const Pattern = @This();

const isLower = std.ascii.isLower;
const isUpper = std.ascii.isUpper;

// 5.4.1 - Patterns
// bool defines if its lowercase (not opposite)
// so false == opposite
// %s = all space characters, %S = all non-space characters
pub const CharacterClass = union(enum) {
    // true if negated
    character: u8, // x (where x is not ^$()%.[]*+-?) represents x itself
    all_characters, // .
    all_letters: bool, // %a
    all_control: bool, // %c
    all_digit: bool, // %d
    all_lower: bool, // %l
    all_punct: bool, // %p
    all_space: bool, // %s
    all_upper: bool, // %u
    all_alphanumeric: bool, // %w
    all_hexadecimal: bool, // %x
    null_character: bool, // %z
    escaped_non_alphanumeric: u8, // %x (where x is not alphanumeric) represents x itself. used to escape special characters in patterns.
    set: []const CharacterClass,
    set_negated: []const CharacterClass,
    // [set]
    // [^set]

    pub const Scanner = struct {
        text: []const u8,
        ptr: *usize,

        pub fn scan(self: *@This()) anyerror!?CharacterClass {
            const c = self.advance() orelse return null;

            return switch (c) {
                // ^$()%.[]*+-?
                '^', '$', '(', ')' => return error.TODO,
                '+', '-', '*', '?' => return error.FollowerWithoutCharacterClass,
                '[' => {
                    var char_classes = std.ArrayList(CharacterClass).empty;
                    defer char_classes.deinit(std.heap.page_allocator);

                    var is_negated = false;

                    const next_char = self.advance() orelse return error.UnexpectedEndOfCharacterClass;
                    if (next_char == '^') {
                        is_negated = true;
                    } else {
                        self.ptr.* -= 1; // put back the character
                    }

                    while (true) {
                        const cc = (self.scan()) catch |e| {
                            if (e == error.UnexpectedEndOfCharacterClass)
                                break;
                            return e;
                        } orelse return error.NoEndOfCharacterClass;

                        // check if current character is - indicating a range
                        const lookahead = self.advance() orelse return error.UnexpectedEndOfCharacterClass;
                        if (lookahead == '-') {
                            const range_end_char = self.advance() orelse return error.UnexpectedEndOfCharacterClass;
                            if (cc != .character) {
                                return error.InvalidRangeInCharacterClass;
                            }
                            const start_char = cc.character;
                            if (start_char >= range_end_char) {
                                return error.InvalidRangeInCharacterClass;
                            }

                            var ch: u8 = start_char;
                            while (ch <= range_end_char) : (ch += 1) {
                                try char_classes.append(std.heap.page_allocator, .{ .character = ch });
                                if (ch >= 255) break;
                            }
                            continue;
                        } else {
                            self.ptr.* -= 1; // put back the character
                        }

                        try char_classes.append(std.heap.page_allocator, cc);
                    }

                    const result_set = try char_classes.toOwnedSlice(std.heap.page_allocator);

                    if (is_negated)
                        return .{ .set_negated = result_set };

                    return .{ .set = result_set };
                },
                ']' => return error.UnexpectedEndOfCharacterClass,
                '.' => .all_characters,
                '%' => {
                    const d = self.advance() orelse return error.InvalidOptionToPattern;

                    return switch (d) {
                        'a', 'A' => .{ .all_letters = isUpper(d) },
                        'c', 'C' => .{ .all_control = isUpper(d) },
                        'd', 'D' => .{ .all_digit = isUpper(d) },
                        'l', 'L' => .{ .all_lower = isUpper(d) },
                        'p', 'P' => .{ .all_punct = isUpper(d) },
                        's', 'S' => .{ .all_space = isUpper(d) },
                        'u', 'U' => .{ .all_upper = isUpper(d) },
                        'w', 'W' => .{ .all_alphanumeric = isUpper(d) },
                        'x', 'X' => .{ .all_hexadecimal = isUpper(d) },
                        'z', 'Z' => .{ .null_character = isUpper(d) },
                        else => {
                            if (d == '%')
                                return .{ .character = '%' };

                            if (!std.ascii.isAlphanumeric(d))
                                return .{
                                    .escaped_non_alphanumeric = d,
                                };

                            return error.InvalidOptionToPattern;
                        },
                    };
                },
                // x (which is not ^$()%.[]*+-?)
                else => .{ .character = c },
            };
        }

        pub fn advance(self: *@This()) ?u8 {
            if (self.ptr.* + 1 > self.text.len)
                return null;

            defer self.ptr.* += 1;
            return self.text[self.ptr.*];
        }
    };
};

pub const Item = union(enum) {
    // a single character class,
    // which matches any single character in the class;
    character_class: CharacterClass,
    repetition: union(enum) {
        null_or_more_longest: CharacterClass, // character class followerd by *
        single_or_more_longest: CharacterClass, // character class followed by +
        // TODO character class followed by -
    },
    null_or_one: CharacterClass, // character class followed by ?
    // TODO %n
    // TODO %bxy

    pub const Scanner = struct {
        text: []const u8,
        ptr: *usize,
        character_class_scanner: CharacterClass.Scanner,

        pub fn scan(self: *@This()) anyerror!?Item {
            const character_class = (try self.character_class_scanner.scan()) orelse return null;
            const c = self.advance() orelse return .{ .character_class = character_class };

            // TODO: handle %n, %bxy
            return switch (c) {
                '*' => .{ .repetition = .{ .null_or_more_longest = character_class } },
                '+' => .{ .repetition = .{ .single_or_more_longest = character_class } },
                '?' => .{ .null_or_one = character_class },
                '-' => return error.TODO,
                else => {
                    self.ptr.* -= 1; // put back the character
                    return .{ .character_class = character_class };
                },
            };
        }

        pub fn advance(self: *@This()) ?u8 {
            if (self.ptr.* + 1 > self.text.len)
                return null;

            defer self.ptr.* += 1;
            return self.text[self.ptr.*];
        }
    };
};

// A pattern is a sequence of pattern items.
// A '^' at the beginning of a pattern anchors
// the match at the beginning of the subject string.
// A '$' at the end of a pattern anchors the match at
// the end of the subject string. At other positions,
// '^' and '$' have no special meaning and represent themselves.
items: []const Item,
start_anchor: bool, // true if the pattern starts with ^
end_anchor: bool, // true if the pattern ends with $

pub fn make(allocator: std.mem.Allocator, given_pattern: []const u8) !@This() {
    var pattern_text = given_pattern;

    var pattern_items = std.ArrayList(Item).empty;
    defer pattern_items.deinit(allocator);

    const start_anchor = pattern_text.len > 0 and pattern_text[0] == '^';
    const end_anchor = pattern_text.len > 0 and pattern_text[pattern_text.len - 1] == '$';

    if (start_anchor)
        pattern_text = pattern_text[1..];

    if (end_anchor)
        pattern_text = pattern_text[0 .. pattern_text.len - 1];

    var cursor: usize = 0;

    var item_scanner = Item.Scanner{
        .text = pattern_text,
        .ptr = &cursor,
        .character_class_scanner = .{
            .text = pattern_text,
            .ptr = &cursor,
        },
    };

    while (try item_scanner.scan()) |pattern_item|
        try pattern_items.append(allocator, pattern_item);

    return .{
        .items = try pattern_items.toOwnedSlice(allocator),
        .start_anchor = start_anchor,
        .end_anchor = end_anchor,
    };
}

// TODO captures

pub const Iterator = struct {
    pattern: Pattern,
    subject: []const u8,
    match_start: usize = 0,
    match_end: usize = 0,
    subject_ptr: usize = 0, // current position in the subject string
    pattern_ptr: usize = 0, // current position in the pattern

    pub fn scan(self: *@This()) anyerror!?[]const u8 {
        if (self.subject_ptr >= self.subject.len)
            return null;

        // Implement Lua's pattern matching rules
        // For now, just return the next slice that matches the pattern
        // Implement only the test case %s%L

        // do it by matching s[0..], s[1..], s[2..], etc.
        o: while (true) {
            const start_index = self.subject_ptr;
            self.match_start = start_index;

            if (start_index >= self.subject.len)
                return null;

            for (self.pattern.items) |item| {
                const subject_char = self.advance_subject() orelse return null;

                switch (item) {
                    .character_class => |cc| {
                        if (!char_matches_cc(subject_char, cc)) {
                            self.subject_ptr = start_index + 1;
                            continue :o;
                        }
                    },
                    .repetition => |r| {
                        switch (r) {
                            else => unreachable,
                            .single_or_more_longest => |cc| {
                                var count: usize = 0;

                                // `subject_char` was already consumed by `advance_subject()` above.
                                // Use it as the first character of the repetition.
                                if (char_matches_cc(subject_char, cc)) {
                                    count = 1;
                                } else {
                                    self.subject_ptr = start_index + 1;
                                    continue :o;
                                }

                                while (self.subject_ptr < self.subject.len) {
                                    const c = self.subject[self.subject_ptr];
                                    if (!char_matches_cc(c, cc))
                                        break;
                                    self.subject_ptr += 1;
                                    count += 1;
                                }
                                if (count == 0) {
                                    self.subject_ptr = start_index + 1;
                                    continue :o;
                                }
                            },
                        }
                    },
                    else => unreachable,
                }

                continue;
            }

            self.match_end = self.subject_ptr;
            return self.subject[start_index..self.subject_ptr];
        }

        return null; // no more matches
    }

    fn char_matches_item(subject_char: u8, pattern_item: Item) bool {
        return switch (pattern_item) {
            .character_class => |cc| char_matches_cc(subject_char, cc),
            else => unreachable,
        };
    }

    fn char_matches_cc(subject_char: u8, cc: CharacterClass) bool {
        // TODO: make sure all match Lua's character classes (they use ctype.h functions)
        return switch (cc) {
            .all_digit => |ad| invert_if_negated(ad, std.ascii.isDigit(subject_char)),
            .all_lower => |al| invert_if_negated(al, isLower(subject_char)),
            .all_upper => |au| invert_if_negated(au, isUpper(subject_char)),
            .all_space => |as| invert_if_negated(as, std.ascii.isWhitespace(subject_char)),
            .all_alphanumeric => |aa| invert_if_negated(aa, std.ascii.isAlphanumeric(subject_char)),
            .all_letters => |al| invert_if_negated(al, std.ascii.isAlphabetic(subject_char)),
            .all_control => |ac| invert_if_negated(ac, std.ascii.isControl(subject_char)),
            .all_hexadecimal => |ah| invert_if_negated(ah, std.ascii.isHex(subject_char)),
            .all_punct => |ap| invert_if_negated(ap, isPunct(subject_char)),
            .null_character => |nz| invert_if_negated(nz, subject_char == 0),
            .escaped_non_alphanumeric => |c| subject_char == c,
            .character => |c| subject_char == c,
            .all_characters => |_| true,
            inline .set, .set_negated => |set| {
                const is_negated = cc == .set_negated;
                var any_match: bool = false;
                for (set) |sub_cc| {
                    if (char_matches_cc(subject_char, sub_cc)) {
                        any_match = true;
                        break;
                    }
                }
                return invert_if_negated(is_negated, any_match);
            },
        };
    }

    // Lua uses C ctype.h functions for character classes.
    // https://www.programiz.com/c-programming/library-function/ctype.h/ispunct
    // zig fmt: off
    fn isPunct(c: u8) bool {
        return switch (c) {
            '!', '"', '#', '$', '%',
            '&', '\'', '(', ')', '*',
            '+', ',', '-', '.', '/',
            ':', ';', '<', '=', '>',
            '?', '@', '[', '\\', ']',
            '^', '_', '`', '{', '|',
            '}', '~' => true,
            else => false,
        };
    }
    // zig fmt: on

    pub fn advance_subject(self: *@This()) ?u8 {
        if (self.subject_ptr >= self.subject.len)
            return null;

        defer self.subject_ptr += 1;
        return self.subject[self.subject_ptr];
    }

    pub fn advance_pattern_item(self: *@This()) ?Item {
        if (self.pattern_ptr >= self.pattern.items.len)
            return null;

        defer self.pattern_ptr += 1;
        return self.pattern.items[self.pattern_ptr];
    }
};

fn invert_if_negated(is_negated: bool, val: bool) bool {
    return if (is_negated) !val else val;
}
