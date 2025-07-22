local function o()
    return 1
end

local function x()
    return o()
end

local function y()
    return x()
end

local a = y()
print(a)
