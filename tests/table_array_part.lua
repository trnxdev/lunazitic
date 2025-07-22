local x = {}
x[1] = 2
x[4] = 5

for k, v in ipairs(x) do
    print(k, v)
end

x[2] = 3
x[3] = 4

for k, v in ipairs(x) do
    print(k, v)
end
