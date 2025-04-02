local function x(...)
    local a, b = ...
    local c, d = 9, ...
    local e, f = ..., 8
    print(a, b)
    print(c, d)
    print(e, f)
end

x(2, 3, 5)
