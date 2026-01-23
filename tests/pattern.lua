-- gmatch pattern tests for [set] and character classes

s = "hello World 123 abc_DEF-456"

print("1")
for m in string.gmatch(s, "%a+") do print(m) end

print("2")
for m in string.gmatch(s, "%d+") do print(m) end

print("3")
for m in string.gmatch(s, "[hw]%a+") do print(m) end

print("4")
for m in string.gmatch("a1b2c3", "%a%d") do print(m) end

print("5")
for m in string.gmatch("foo_bar baz", "[%a_]+") do print(m) end

print("6")
for m in string.gmatch("x1 y2 z3", "[%w]+") do print(m) end

print("7")
for m in string.gmatch("a,b;c", "[,;]") do print(m) end

print("8")
for m in string.gmatch("abc 123 !?", "[^%s]+") do print(m) end

-- Ranges
print("9")
for m in string.gmatch("a1b2c3x9y8z7", "[a-c][1-3]") do print(m) end

-- string.format
print("10")
formatted = string.format("%.14g", 3.14159265358979323846)
print(formatted)