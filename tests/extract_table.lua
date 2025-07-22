local function x()
    return 1, 3
end

local bytes = { x() }

for k, v in pairs(bytes) do
    print(k, v)
end
