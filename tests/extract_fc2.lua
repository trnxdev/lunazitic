local function x()
    return 1
end

local function y()
    return x()
end

local a = y()
print(a)
