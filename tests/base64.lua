-- Modified from the original code
-- https://gist.github.com/mmurdoch/3806239

--[[---------------
LuaBit v0.4
-------------------
a bitwise operation lib for lua.

http://luaforge.net/projects/bit/
--------------------]] --

local function check_int(n)
    if (n - math.floor(n) > 0) then
        assert(false, "trying to use bitwise operation on non-integer!")
    end
end

local function to_bits(n)
    check_int(n)
    if (n < 0) then
        return to_bits(bit_not(math.abs(n)) + 1)
    end
    local tbl = {}
    local cnt = 1
    while (n > 0) do
        local last = math.mod(n, 2)
        if (last == 1) then
            tbl[cnt] = 1
        else
            tbl[cnt] = 0
        end
        n = (n - last) / 2
        cnt = cnt + 1
    end

    return tbl
end

local function tbl_to_number(tbl)
    local n = table.getn(tbl)

    local rslt = 0
    local power = 1
    for i = 1, n do
        rslt = rslt + tbl[i] * power
        power = power * 2
    end

    return rslt
end

local function expand(tbl_m, tbl_n)
    local big = {}
    local small = {}
    if (table.getn(tbl_m) > table.getn(tbl_n)) then
        big = tbl_m
        small = tbl_n
    else
        big = tbl_n
        small = tbl_m
    end
    -- expand small
    for i = table.getn(small) + 1, table.getn(big) do
        small[i] = 0
    end
end

local function bit_or(m, n)
    local tbl_m = to_bits(m)
    local tbl_n = to_bits(n)
    expand(tbl_m, tbl_n)

    local tbl = {}
    local rslt = math.max(table.getn(tbl_m), table.getn(tbl_n))
    for i = 1, rslt do
        if (tbl_m[i] == 0 and tbl_n[i] == 0) then
            tbl[i] = 0
        else
            tbl[i] = 1
        end
    end

    return tbl_to_number(tbl)
end

local function bit_and(m, n)
    local tbl_m = to_bits(m)
    local tbl_n = to_bits(n)
    expand(tbl_m, tbl_n)

    local tbl = {}
    local rslt = math.max(table.getn(tbl_m), table.getn(tbl_n))
    for i = 1, rslt do
        if (tbl_m[i] == 0 or tbl_n[i] == 0) then
            tbl[i] = 0
        else
            tbl[i] = 1
        end
    end

    return tbl_to_number(tbl)
end

local function bit_not(n)
    local tbl = to_bits(n)
    local size = math.max(table.getn(tbl), 32)
    for i = 1, size do
        if (tbl[i] == 1) then
            tbl[i] = 0
        else
            tbl[i] = 1
        end
    end
    return tbl_to_number(tbl)
end

local function bit_xor(m, n)
    local tbl_m = to_bits(m)
    local tbl_n = to_bits(n)
    expand(tbl_m, tbl_n)

    local tbl = {}
    local rslt = math.max(table.getn(tbl_m), table.getn(tbl_n))
    for i = 1, rslt do
        if (tbl_m[i] ~= tbl_n[i]) then
            tbl[i] = 1
        else
            tbl[i] = 0
        end
    end

    return tbl_to_number(tbl)
end

local function bit_rshift(n, bits)
    check_int(n)

    local high_bit = 0
    if (n < 0) then
        n = bit_not(math.abs(n)) + 1
        high_bit = 2147483648 -- 0x80000000
    end

    for i = 1, bits do
        n = n / 2
        n = bit_or(math.floor(n), high_bit)
    end
    return math.floor(n)
end

local function bit_logic_rshift(n, bits)
    check_int(n)
    if (n < 0) then
        n = bit_not(math.abs(n)) + 1
    end
    for i = 1, bits do
        n = n / 2
    end
    return math.floor(n)
end

local function bit_lshift(n, bits)
    check_int(n)

    if (n < 0) then
        n = bit_not(math.abs(n)) + 1
    end
    for i = 1, bits do
        n = n * 2
    end
    return bit_and(n, 4294967295) -- 0xFFFFFFFF
end

local function bit_xor2(m, n)
    local rhs = bit_or(bit_not(m), bit_not(n))
    local lhs = bit_or(m, n)
    local rslt = bit_and(lhs, rhs)
    return rslt
end

--- octet -> char encoding.
local ENCODABET = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '+', '/'
}

local DECODABET = {}
for i, v in ipairs(ENCODABET) do
    DECODABET[v] = i - 1
end

local PAD = "="

local function toChar(octet)
    return ENCODABET[octet + 1]
end

local function toOctet(char)
    return DECODABET[char]
end

local function base64_encode(input)
    local bytes = { input:byte(1, #input) }
    local out = {}

    -- Go through each triplet of 3 bytes, which produce 4 octets.
    local i = 1
    while i <= #bytes - 2 do
        local buffer = 0

        -- Fill the buffer with the bytes, producing a 24-bit integer.
        local b = bit_lshift(bytes[i], 16)
        b = bit_and(b, 0xff0000)
        buffer = bit_or(buffer, b)

        b = bit_lshift(bytes[i + 1], 8)
        b = bit_and(b, 0xff00)
        buffer = bit_or(buffer, b)

        b = bit_and(bytes[i + 2], 0xff)
        buffer = bit_or(buffer, b)

        -- Read out the 4 octets into the output buffer.
        b = bit_logic_rshift(buffer, 18)
        b = bit_and(b, 0x3f)
        out[#out + 1] = toChar(b)

        b = bit_logic_rshift(buffer, 12)
        b = bit_and(b, 0x3f)
        out[#out + 1] = toChar(b)

        b = bit_logic_rshift(buffer, 6)
        b = bit_and(b, 0x3f)
        out[#out + 1] = toChar(b)

        b = bit_and(buffer, 0x3f)
        out[#out + 1] = toChar(b)

        i = i + 3
    end

    -- Special case 1: One byte extra, will produce 2 octets.
    if #bytes % 3 == 1 then
        local buffer = bit_lshift(bytes[i], 16)
        buffer = bit_and(buffer, 0xff0000)

        local b = bit_logic_rshift(buffer, 18)
        b = bit_and(b, 0x3f)
        out[#out + 1] = toChar(b)

        b = bit_logic_rshift(buffer, 12)
        b = bit_and(b, 0x3f)
        out[#out + 1] = toChar(b)

        out[#out + 1] = PAD
        out[#out + 1] = PAD

        -- Special case 2: Two bytes extra, will produce 3 octets.
    else
        if #bytes % 3 == 2 then
            local buffer = 0

            local b = bit_lshift(bytes[i], 16)
            b = bit_and(b, 0xff0000)
            buffer = bit_or(buffer, b)

            b = bit_lshift(bytes[i + 1], 8)
            b = bit_and(b, 0xff00)
            buffer = bit_or(buffer, b)

            b = bit_logic_rshift(buffer, 18)
            b = bit_and(b, 0x3f)
            out[#out + 1] = toChar(b)

            b = bit_logic_rshift(buffer, 12)
            b = bit_and(b, 0x3f)
            out[#out + 1] = toChar(b)

            b = bit_logic_rshift(buffer, 6)
            b = bit_and(b, 0x3f)
            out[#out + 1] = toChar(b)

            out[#out + 1] = PAD
        end
    end

    return table.concat(out)
end

local y = base64_encode "yesterday I was at a burger place with my friend and he ordered fanta. guess what? he got his fanta"
print(y)
