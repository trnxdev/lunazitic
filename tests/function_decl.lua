function hello_world()
	return 1
end

function hello_world()
	return 2
end

print(hello_world()) -- 2

local obj = {}

function obj.test()
    return 3 + hello_world()
end

--function obj:test2()
--    return 4
--end

print(obj.test())  -- 3
