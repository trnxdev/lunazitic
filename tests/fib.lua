local function fib(n)
    if n <= 1 then
        return n
    end

    return fib(n - 1) + fib(n - 2)
end

val = fib(30)
print(val)
