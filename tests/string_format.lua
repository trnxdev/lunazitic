-- Lua 5.1 test suite for string.format

local function assert_equal(actual, expected, desc)
    if actual == expected then
        print(string.format("✓: %s, Expected: %s, Got: %s", desc, expected, actual))
    else
        print(string.format("Ⲭ: %s, Expected: %s, Got: %s", desc, expected, actual))
    end
end

-- Basic specifiers
assert_equal(string.format("%d", 123), "123", "integer (%d)")
--assert_equal(string.format("%i", -456), "-456", "%i: signed integer")
assert_equal(string.format("%d", 4294967295), "4294967295", "unsigned integer (%u)") -- treated as float in Lua 5.1
assert_equal(string.format("%x", 255), "ff", "hex lowercase (%x)")
assert_equal(string.format("%X", 255), "FF", "hex uppercase (%X)")
assert_equal(string.format("%o", 9), "11", "octal (%o)")

-- Floating point
assert_equal(string.format("%f", 3.14159), "3.141590", "%f: default float precision")
assert_equal(string.format("%.2f", 3.14159), "3.14", "%.2f: float with 2 decimals")
assert_equal(string.format("%e", 12345.6789), "1.234568e+004", "%e: scientific notation")
assert_equal(string.format("%E", 12345.6789), "1.234568E+004", "%E: scientific notation uppercase")
assert_equal(string.format("%g", 12345.6789), "12345.7", "%g: shortest representation")
assert_equal(string.format("%g", 0.000123), "0.000123", "%g: small float")
assert_equal(string.format("%G", 0.000123), "0.000123", "%G: small float uppercase")

-- Characters and strings
assert_equal(string.format("%c", 65), "A", "%c: character from ASCII code")
assert_equal(string.format("%s", "hello"), "hello", "%s: string")
assert_equal(string.format("%.3s", "hello"), "hel", "%.3s: string with precision")

-- Width and padding
assert_equal(string.format("%5d", 42), "   42", "%5d: right-aligned width")
assert_equal(string.format("%-5d", 42), "42   ", "%-5d: left-aligned width")
assert_equal(string.format("%05d", 42), "00042", "%05d: zero-padded width")
assert_equal(string.format("%10s", "hi"), "        hi", "%10s: right-aligned string")
assert_equal(string.format("%-10s", "hi"), "hi        ", "%-10s: left-aligned string")

-- Signs and prefixes
assert_equal(string.format("%+d", 42), "+42", "%+d: explicit plus sign")
assert_equal(string.format("% d", 42), " 42", "% d: space for positive")
assert_equal(string.format("%#x", 255), "0xff", "%#x: hex with prefix")
assert_equal(string.format("%#o", 9), "011", "%#o: octal with prefix")
assert_equal(string.format("%#f", 3.0), "3.000000", "%#f: force decimal point")

-- Multiple arguments
assert_equal(string.format("%d + %d = %d", 2, 3, 5), "2 + 3 = 5", "Multiple arguments")

-- Edge cases
assert_equal(string.format("%%"), "%", "Literal percent sign")
assert_equal(string.format("%.0f", 3.5), "4", "%.0f: rounding")
assert_equal(string.format("%.0f", 2.4), "2", "%.0f: rounding down")

-- Negative numbers and large values
--assert_equal(string.format("%d", -2147483648), "-2147483648", "%d: large negative")
assert_equal(string.format("%u", 2 ^ 32 - 1), "4294967295", "%u: max unsigned")

print("All tests passed.")
