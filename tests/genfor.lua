local function counter(max)
    local i = 0

    print("running function")
    return function()
        print("inner running function", i, max)
        i = i + 1
        if i <= max then
            return i
        end

        return nil
    end
end

for i in counter(5) do
    print("for loop", i)
end
