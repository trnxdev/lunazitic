local function x(...)
    print(..., 7, ...)
end

x(2, 3, 4, 5)
--Output: 2 7	2	3	4	5
