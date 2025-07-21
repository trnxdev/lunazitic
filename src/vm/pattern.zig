const std = @import("std");
const Pattern = @This();

const isLower = std.ascii.isLower;
const isUpper = std.ascii.isUpper;

// 5.4.1 - Patterns
// bool defines if its lowercase (not opposite)
// so false == opposite
// %s = all space characters, %S = all non-space characters
pub const CharacterClass = union(enum) {
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
    // [set]
    // [^set]

    pub const Scanner = struct {
        text: []const u8,
        ptr: *usize,

        pub fn scan(self: *@This()) anyerror!?CharacterClass {
            const c = self.advance() orelse return null;

            return switch (c) {
                '[', ']', '^', '(', ')', '+', '-', '*', '$' => return error.TODO,
                '.' => .all_characters,
                '%' => {
                    const d = self.advance() orelse return error.InvalidOptionToPattern;

                    return switch (d) {
                        'a', 'A' => .{ .all_letters = isLower(d) },
                        'c', 'C' => .{ .all_control = isLower(d) },
                        'd', 'D' => .{ .all_digit = isLower(d) },
                        'l', 'L' => .{ .all_lower = isLower(d) },
                        'p', 'P' => .{ .all_punct = isLower(d) },
                        's', 'S' => .{ .all_space = isLower(d) },
                        'u', 'U' => .{ .all_upper = isLower(d) },
                        'w', 'W' => .{ .all_alphanumeric = isLower(d) },
                        'x', 'X' => .{ .all_hexadecimal = isLower(d) },
                        'z', 'Z' => .{ .null_character = isLower(d) },
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
        null_or_more: CharacterClass, // character class followerd by *
        single_or_more: CharacterClass, // character class followed by +
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

            return switch (c) {
                '*' => .{ .repetition = .{ .null_or_more = character_class } },
                '+' => .{ .repetition = .{ .single_or_more = character_class } },
                '?' => .{ .null_or_one = character_class },
                // '%' => return error.TODO,
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
    var pattern = given_pattern;

    var items = std.ArrayList(Item).init(allocator);
    defer items.deinit();

    const start_anchor = pattern.len > 0 and pattern[0] == '^';
    const end_anchor = pattern.len > 0 and pattern[pattern.len - 1] == '$';

    if (start_anchor)
        pattern = pattern[1..];

    if (end_anchor)
        pattern = pattern[0 .. pattern.len - 1];

    var ptr: usize = 0;

    var item_scanner = Item.Scanner{
        .text = pattern,
        .ptr = &ptr,
        .character_class_scanner = .{
            .text = pattern,
            .ptr = &ptr,
        },
    };

    while (try item_scanner.scan()) |pattern_item|
        try items.append(pattern_item);

    return .{
        .items = try items.toOwnedSlice(),
        .start_anchor = start_anchor,
        .end_anchor = end_anchor,
    };
}

// TODO captures

pub const Iterator = struct {
    pattern: Pattern,
    subject: []const u8,
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
            const start = self.subject_ptr;

            if (start >= self.subject.len)
                return null;

            for (self.pattern.items) |item| {
                const subject_char = self.advance_subject() orelse return null;

                switch (item) {
                    .character_class => |_| {
                        if (!char_matches_item(subject_char, item)) {
                            self.subject_ptr = start + 1;
                            continue :o;
                        }
                    },
                    else => unreachable,
                }

                continue;
            }

            return self.subject[start..self.subject_ptr];
        }

        return null; // no more matches
    }

    fn char_matches_item(subject_char: u8, pattern_item: Item) bool {
        return switch (pattern_item) {
            .character_class => |cc| switch (cc) {
                .all_digit => |ad| invert_if_upper(ad, std.ascii.isDigit(subject_char)),
                .all_lower => |al| invert_if_upper(al, isLower(subject_char)),
                .all_upper => |au| invert_if_upper(au, isUpper(subject_char)),
                .all_space => |as| invert_if_upper(as, subject_char == ' '),
                else => unreachable,
            },
            else => unreachable,
        };
    }

    fn isDigitSlice(slice: []const u8) bool {
        for (slice) |c| if (!std.ascii.isDigit(c)) return false;
        return true;
    }

    fn isLowerSlice(slice: []const u8) bool {
        for (slice) |c| if (!isLower(c)) return false;
        return true;
    }

    fn isUpperSlice(slice: []const u8) bool {
        for (slice) |c| if (!isUpper(c)) return false;
        return true;
    }

    fn isSpaceSlice(slice: []const u8) bool {
        for (slice) |c| if (c != ' ') return false;
        return true;
    }

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

fn invert_if_upper(is_lower: bool, val: bool) bool {
    return if (is_lower == false) !val else val;
}
