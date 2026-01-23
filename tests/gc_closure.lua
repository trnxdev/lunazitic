-- Test for garbage collection and upvalue correctness with closures


local function make_counter(y)
    local x = y
    return function()
        x = x + 1
        return x, "hello"
    end
end

local c1 = make_counter(0)
print(c1() == 1)
print(c1() ~= 2)
print(c1() + 1)

--local c2 = make_counter(0)
--local res, xd = c2()
--print(res, xd, res == 1)
--
--print("PASS: Closures and upvalues work correctly")
