-- lua-users.org/lists/lua-l/2022-02/msg00085.html
local io = io.write
local i, j, k
local z, b = {}, {}
local a = ".,-~:;=!*#$@"
local A = 0.0
local sin = math.sin
local cos = math.cos
local int = math.floor
io("\27[2J")
local B = 0
while true do
    for i = 0, 1759 do
        b[i] = " "
        z[i] = 0.0
    end
    j = 0.0
    while 6.28 > j do
        i = 0
        while 6.28 > i do
            local c = sin(i)
            local d = cos(j)
            local e = sin(A)
            local f = sin(j)
            local g = cos(A)
            local h = d + 2
            local D = 1 / (c * h * e + f * g + 5)
            local l = cos(i)
            local m = cos(B)
            local n = sin(B)
            t = c * h * g - f * e
            local x = int(40 + 30 * D * (l * h * m - t * n))
            local y = int(12 + 15 * D * (l * h * n + t * m))
            o = int(x + 80 * y)
            local N = int(8 * ((f * e - c * d * g) * m - c * d * e - f * g - l * d * n)) + 1
            if 22 > y and y > 0 and x > 0 and 80 > x and D > z[o] then
                z[o] = D
                if N > 1 then
                    b[o] = a:sub(N, N)
                else
                    b[o] = a:sub(1, 1)
                end
            end
            i = i + 0.02
        end
        j = j + 0.07
    end
    buf = ""
    for k = 0, 1760 do
        if k % 80 > 0.0 then
            buf = buf .. b[k]
        else
            buf = buf .. "\n"
        end
    end
    io(buf)
    A = A + 0.04
    B = B + 0.02
end
