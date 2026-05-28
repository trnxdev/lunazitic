s = "hello World from Lua"

for i in string.gmatch(s, "%s%L%l") do
    print(i)
end