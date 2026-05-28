-- GC stress test for Lunazitic
-- Run: lunazitic run tests/gc_stress.lua

local function alloc_tables(rounds, depth)
  local root = {}
  local current = root
  for i = 1, depth do
    local next = { i = i, payload = string.rep("x", 64) }
    current[i] = next
    current = next
  end

  for r = 1, rounds do
    local t = {}
    for i = 1, 200 do
      t[i] = { n = i, s = string.rep("y", 32) }
    end
    if r % 25 == 0 then
      t = nil
    end
  end

  return root
end

local function alloc_strings(rounds)
  local list = {}
  for i = 1, rounds do
    list[i] = string.rep("a", (i % 128) + 1)
    if i % 50 == 0 then
      list[i] = nil
    end
  end
end

local function alloc_tuples(rounds)
  for i = 1, rounds do
    local a, b, c = i, i + 1, i + 2
    if i % 100 == 0 then
      a, b, c = nil, nil, nil
    end
  end
end

local function stress()
  for pass = 1, 50 do
    local root = alloc_tables(100, 25)
    alloc_strings(1000)
    alloc_tuples(1000)
    if pass % 5 == 0 then
      root = nil
    end
  end
end

stress()
print("gc_stress done")
