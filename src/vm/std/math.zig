const std = @import("std");
const VM = @import("../vm.zig");

pub const huge = std.math.inf(f64);
pub const pi = std.math.pi;

const NativeFunction = VM.Object.ObjNativeFunction;

pub fn init(vm: *VM) !VM.Value {
    const math = try VM.Object.ObjTable.create(vm);
    try math.fields.putWithKeyObjectAuto("abs", try NativeFunction.create(vm, &abs));
    try math.fields.putWithKeyObjectAuto("acos", try NativeFunction.create(vm, &acos));
    try math.fields.putWithKeyObjectAuto("asin", try NativeFunction.create(vm, &asin));
    try math.fields.putWithKeyObjectAuto("atan", try NativeFunction.create(vm, &atan));
    try math.fields.putWithKeyObjectAuto("atan2", try NativeFunction.create(vm, &atan2));
    try math.fields.putWithKeyObjectAuto("ceil", try NativeFunction.create(vm, &ceil));
    try math.fields.putWithKeyObjectAuto("cos", try NativeFunction.create(vm, &cos));
    try math.fields.putWithKeyObjectAuto("cosh", try NativeFunction.create(vm, &cosh));
    try math.fields.putWithKeyObjectAuto("deg", try NativeFunction.create(vm, &deg));
    try math.fields.putWithKeyObjectAuto("exp", try NativeFunction.create(vm, &exp));
    try math.fields.putWithKeyObjectAuto("floor", try NativeFunction.create(vm, &floor));
    try math.fields.putWithKeyObjectAuto("fmod", try NativeFunction.create(vm, &fmod));
    try math.fields.putWithKeyObjectAuto("frexp", try NativeFunction.create(vm, &frexp));
    try math.fields.putWithKeyObjectAuto("ldexp", try NativeFunction.create(vm, &ldexp));
    try math.fields.putWithKeyObjectAuto("log", try NativeFunction.create(vm, &log));
    try math.fields.putWithKeyObjectAuto("log10", try NativeFunction.create(vm, &log10));
    try math.fields.putWithKeyObjectAuto("max", try NativeFunction.create(vm, &max));
    try math.fields.putWithKeyObjectAuto("min", try NativeFunction.create(vm, &min));
    try math.fields.putWithKeyObjectAuto("modf", try NativeFunction.create(vm, &modf));
    try math.fields.putWithKeyObjectAuto("pow", try NativeFunction.create(vm, &pow));
    try math.fields.putWithKeyObjectAuto("rad", try NativeFunction.create(vm, &rad));
    try math.fields.putWithKeyObjectAuto("random", try NativeFunction.create(vm, &random));
    try math.fields.putWithKeyObjectAuto("randomseed", try NativeFunction.create(vm, &randomseed));
    try math.fields.putWithKeyObjectAuto("sin", try NativeFunction.create(vm, &sin));
    try math.fields.putWithKeyObjectAuto("sinh", try NativeFunction.create(vm, &sinh));
    try math.fields.putWithKeyObjectAuto("sqrt", try NativeFunction.create(vm, &sqrt));
    try math.fields.putWithKeyObjectAuto("tan", try NativeFunction.create(vm, &tan));
    try math.fields.putWithKeyObjectAuto("tanh", try NativeFunction.create(vm, &tanh));
    try math.fields.putWithKey("huge", VM.Value.initNumber(huge));
    try math.fields.putWithKey("pi", VM.Value.initNumber(pi));

    // Function math.mod was renamed math.fmod. (See compile-time option LUA_COMPAT_MOD in luaconf.h.)
    try math.fields.putWithKeyObjectAuto("mod", try NativeFunction.create(vm, &fmod));
    return math.object.asValue();
}

// just making @abs doesn't work, and std.math.abs doesn't exist
// zig fmt: off
pub const abs = makeSimpleMathFunc(struct { pub fn abs(num: f64) f64 {
        return @abs(num); // math.abs (x) - Returns the absolute value of x.
} }.abs);
pub const acos = makeSimpleMathFunc(struct { pub fn acos(num: f64) f64 {
        return std.math.acos(num); // math.acos (x) - Returns the arc cosine of x (in radians).
} }.acos);
pub const asin = makeSimpleMathFunc(struct { pub fn asin(num: f64) f64 {
        return std.math.asin(num); // math.asin (x) - Returns the arc sine of x (in radians).
} }.asin);
pub const atan = makeSimpleMathFunc(struct { pub fn atan(num: f64) f64 {
        return std.math.atan(num); // math.atan (x) - Returns the arc tangent of x (in radians).
} }.atan);
pub const atan2 = makeSimpleMathFunc2Args(struct { pub fn atan2(num: f64, num2: f64) f64 {
        return std.math.atan2(num, num2); // math.atan2 (y, x) - Returns the arc tangent of y/x (in radians), but uses the signs of both parameters to find the quadrant of the result.
} }.atan2);
pub const ceil = makeSimpleMathFunc(struct { pub fn ceil(num: f64) f64 {
        return @ceil(num); // math.ceil (x) - Returns the smallest integer larger than or equal to x.
} }.ceil);
pub const cos = makeSimpleMathFunc(struct { pub fn cos(num: f64) f64 {
        return std.math.cos(num); // math.cos (x) - Returns the cosine of x (assumed to be in radians).
} }.cos);
pub const cosh = makeSimpleMathFunc(struct { pub fn cosh(num: f64) f64 {
        return std.math.cosh(num); // math.cosh (x) - Returns the hyperbolic cosine of x.
} }.cosh);
pub const deg = makeSimpleMathFunc(struct { pub fn deg(num: f64) f64 {
        return std.math.radiansToDegrees(num); // math.deg (x) - Returns the angle x (given in radians) in degrees.
} }.deg);
pub const exp = makeSimpleMathFunc(struct { pub fn exp(num: f64) f64 {
        return @exp(num); // math.exp (x) - Returns the value of e^x.
} }.exp);
pub const floor = makeSimpleMathFunc(struct { pub fn floor(num: f64) f64 {
        return @floor(num); // math.floor (x) - Returns the largest integer smaller than or equal to x.
} }.floor);
pub const fmod = makeSimpleMathFunc2Args(struct { pub fn fmod(num: f64, num2: f64) f64 {
        return @mod(num, num2); // math.fmod (x, y) - Returns the remainder of the division of x by y that rounds the quotient towards zero.
} }.fmod);
// zig fmt: on
// Returns m and e such that x = m2e,
// e is an integer and the absolute value of m is in the range [0.5, 1) (or zero when x is zero).
pub fn frexp(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const num = try args[0].asNumberCast(.{ .string = true });
    const frexp_result = std.math.frexp(num);

    const vals = try vm.allocator.alloc(VM.Value, 2);
    vals[0] = VM.Value.initNumber(frexp_result.significand);
    vals[1] = VM.Value.initNumber(@floatFromInt(@max(0, frexp_result.exponent)));

    const ret_array = try VM.Object.ObjTuple.createMoved(
        vm,
        vals,
    );
    return ret_array.object.asValue();
}
// zig fmt: off
pub const ldexp = makeSimpleMathFunc2Args(struct { pub fn ldexp(num: f64, num2: f64) f64 {
        // FIXME: Lua allows to go beyond the limits of i32 (more specifically: c_int), but Zig doesn't
        if (num2 > std.math.maxInt(i32))
            return std.math.inf(f64);

        if (num2 < std.math.minInt(i32))
            return 0.0;

        return std.math.ldexp(num, @intFromFloat(num2)); // math.ldexp (m, e) - Returns m * 2^e (e should be an integer).
} }.ldexp);
pub const log = makeSimpleMathFunc(struct { pub fn log(num: f64) f64 {
        return @log(num); // math.log (x) - Returns the natural logarithm of x.
} }.log);
pub const log10 = makeSimpleMathFunc(struct { pub fn log10(num: f64) f64 {
        return @log10(num); // math.log10 (x) - Returns the base 10 logarithm of x.
} }.log10);
pub const max = makeSimpleMathFuncInfArgs(struct { pub fn max(nums: []const f64) f64 {
        var max_number: f64 = nums[0];

        for (nums[1..]) |num| {
            if (num > max_number)
                max_number = num;
        }

        return max_number;
} }.max);
pub const min = makeSimpleMathFuncInfArgs(struct { pub fn min(nums: []const f64) f64 {
        var min_number: f64 = nums[0];

        for (nums[1..]) |num| {
            if (num < min_number)
                min_number = num;
        }

        return min_number;
} }.min);
// zig fmt: on
// math.modf (x) - Returns two numbers, the integral part of x and the fractional part of x.
pub fn modf(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const num = try args[0].asNumberCast(.{
        .string = true,
    });

    const modf_res = std.math.modf(num);
    return (try VM.Object.ObjTuple.createOwned(vm, &[_]VM.Value{
        VM.Value.initNumber(modf_res.ipart),
        VM.Value.initNumber(modf_res.fpart),
    })).object.asValue();
}
// zig fmt: off
pub const pow = makeSimpleMathFunc2Args(struct { pub fn pow(num: f64, num2: f64) f64 {
        return std.math.pow(f64, num, num2); // math.pow (x, y) - Returns x^y.
} }.pow);
pub const rad = makeSimpleMathFunc(struct { pub fn rad(num: f64) f64 {
        return std.math.degreesToRadians(num); // math.rad (x) - Returns the angle x (given in degrees) in radians.
} }.rad);

// zig fmt: on
// AD-HOC: Lua 5.1 spec encourages to use C's rand. But no, we're not doing that. Frick you C's rand.
pub fn random(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    // When called without arguments, returns a uniform pseudo-random real number in the range [0,1].
    if (args.len == 0)
        return VM.Value.initNumber(vm.internals.rng.random().float(f64));

    // When called with an integer number m, math.random returns a uniform pseudo-random integer in the range [1, m].
    if (args.len == 1) {
        const num: i64 = @intFromFloat(try args[0].asNumberCast(.{ .string = true }));
        return VM.Value.initNumber(@floatFromInt(vm.internals.rng.random().intRangeAtMost(i64, 1, num)));
    }

    // When called with two integer numbers m and n, math.random returns a uniform pseudo-random integer in the range [m, n].
    const num: i64 = @intFromFloat(try args[0].asNumberCast(.{ .string = true }));
    const num2: i64 = @intFromFloat(try args[1].asNumberCast(.{ .string = true }));

    return VM.Value.initNumber(@floatFromInt(vm.internals.rng.random().intRangeAtMost(i64, num, num2)));
}
// zig fmt: off

pub fn randomseed(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
    if (args.len < 1)
        return error.InvalidArgumentCount;

    const num = try args[0].asNumberCast(.{ .string = true });
    vm.internals.rng.seed(@intFromFloat(num));

    return VM.Value.initNil();
}

pub const sin = makeSimpleMathFunc(struct { pub fn sin(num: f64) f64 {
        return @sin(num); // math.sin (x) - Returns the sine of x (assumed to be in radians).
} }.sin);
pub const sinh = makeSimpleMathFunc(struct { pub fn sinh(num: f64) f64 {
        return std.math.sinh(num); // math.sinh (x) - Returns the hyperbolic sine of x.
} }.sinh);
pub const sqrt = makeSimpleMathFunc(struct { pub fn sqrt(num: f64) f64 {
        return @sqrt(num); // math.sqrt (x) - Returns the square root of x.
} }.sqrt);
pub const tan = makeSimpleMathFunc(struct { pub fn tan(num: f64) f64 {
        return @tan(num); // math.tan (x) - Returns the tangent of x (assumed to be in radians).
} }.tan);
pub const tanh = makeSimpleMathFunc(struct { pub fn tanh(num: f64) f64 {
        return std.math.tanh(num); // math.tanh (x) - Returns the hyperbolic tangent of x.
} }.tanh);
// zig fmt: on

pub fn makeSimpleMathFunc(func: fn (f64) f64) fn (*VM, *VM.Scope, []VM.Value) anyerror!VM.Value {
    // No lambdas (anonymous functions) in zig :c
    return struct {
        fn innerfunc(_: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
            if (args.len < 1)
                return error.InvalidArgumentCount;

            const num = try args[0].asNumberCast(.{
                .string = true,
            });

            return VM.Value.initNumber(func(num));
        }
    }.innerfunc;
}

pub fn makeSimpleMathFunc2Args(func: fn (f64, f64) f64) fn (*VM, *VM.Scope, []VM.Value) anyerror!VM.Value {
    return struct {
        fn innerfunc(_: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
            if (args.len < 2)
                return error.InvalidArgumentCount;

            const num = try args[0].asNumberCast(.{
                .string = true,
            });
            const num2 = try args[1].asNumberCast(.{
                .string = true,
            });

            return VM.Value.initNumber(func(num, num2));
        }
    }.innerfunc;
}

pub fn makeSimpleMathFuncInfArgs(func: fn ([]const f64) f64) fn (*VM, *VM.Scope, []VM.Value) anyerror!VM.Value {
    return struct {
        fn innerfunc(vm: *VM, _: *VM.Scope, args: []VM.Value) anyerror!VM.Value {
            // FIXME: Max args is 255? Maybe dont allocate in memory
            var nums = std.ArrayList(f64).init(vm.allocator);
            defer nums.deinit();

            for (args) |arg| {
                const num = try arg.asNumberCast(.{
                    .string = true,
                });
                try nums.append(num);
            }

            return VM.Value.initNumber(func(nums.items));
        }
    }.innerfunc;
}
