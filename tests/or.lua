local function truthy()
    print("truthy evaluated")
    return true
end

local function falsy()
    print("falsy evaluated")
    return false
end

print("Test 1: false and truthy()")
local a = false and truthy() --> should not evaluate truthy()
print("Result:", a)

print("\nTest 2: true and truthy()")
local b = true and truthy() --> should evaluate truthy()
print("Result:", b)

print("\nTest 3: true or falsy()")
local c = true or falsy() --> should not evaluate falsy()
print("Result:", c)

print("\nTest 4: false or truthy()")
local d = false or truthy() --> should evaluate truthy()
print("Result:", d)

print("\nTest 5: nested short-circuit: false and (truthy() or falsy())")
local e = false and (truthy() or falsy()) --> nothing should be evaluated
print("Result:", e)

print("\nTest 6: true or (falsy() and truthy())")
local f = true or (falsy() and truthy()) --> nothing should be evaluated
print("Result:", f)
