local fruits = { "apple", "banana", "zed", x = 1, ["y"] = 2, z = 3, [3.5] = 4, [next] = 9 }

for k, v in next, fruits do
    print(k, v)
end
