const std = @import("std");
const ArrayList = std.ArrayList;

const CompileError = error{
    DifferentOperandTypes,
    InvalidInstructionType,
};
const VMError = error{
    StackOverflow,
    StackUnderflow,
};

const Type = union(enum) {
    none,
    u64,

    const Self = @This();

    fn eql(self: *const Self, b: *const Self) bool {
        return switch (self.*) {
            .none => switch (b.*) {
                .none => true,
                else => false,
            },
            .u64 => |valueA| switch (b.*) {
                .u64 => |valueB| valueA == valueB,
                else => false,
            },
        };
    }
};
const Expr = union(enum) {
    value: u64,
    add: struct { *const Expr, *const Expr },
    sub: struct { *const Expr, *const Expr },
    mul: struct { *const Expr, *const Expr },
    div: struct { *const Expr, *const Expr },
    print: *const Expr,
};
const Compiler = struct {
    data: ArrayList(u8),
    exprs: ArrayList(*const fn (*VM) anyerror!void),

    const Self = @This();

    fn new() Self {
        return Self{
            .data = ArrayList(u8).init(allocator),
            .exprs = ArrayList(*const fn (*VM) anyerror!void).init(allocator),
        };
    }
    inline fn push_data(self: *Self, value: anytype) !void {
        @setRuntimeSafety(false);
        const len = self.data.items.len;
        try self.data.resize(self.data.items.len + @sizeOf(@TypeOf(value)));
        const ptr = &self.data.items.ptr[len];
        @as(*@TypeOf(value), @ptrCast(@alignCast(ptr))).* = value;
    }
    inline fn push_expr(self: *Self, f: *const fn (*VM) anyerror!void) !void {
        try self.exprs.append(f);
    }
};

inline fn ReadData(comptime T: type) !type {
    return struct {
        fn eval(vm: *VM) !void {
            try vm.push_stack(vm.read_data(T));
        }
    };
}
inline fn Add(comptime T: type) !type {
    switch (T) {
        void => return CompileError.InvalidInstructionType,
        else => {},
    }
    return struct {
        fn eval(vm: *VM) !void {
            try vm.push_stack(try vm.pop_stack(T) + try vm.pop_stack(T));
        }
    };
}
inline fn Sub(comptime T: type) !type {
    switch (T) {
        void => return CompileError.InvalidInstructionType,
        else => {},
    }
    return struct {
        fn eval(vm: *VM) !void {
            try vm.push_stack(try vm.pop_stack(T) - try vm.pop_stack(T));
        }
    };
}
inline fn Mul(comptime T: type) !type {
    switch (T) {
        void => return CompileError.InvalidInstructionType,
        else => {},
    }
    return struct {
        fn eval(vm: *VM) !void {
            try vm.push_stack(try vm.pop_stack(T) * try vm.pop_stack(T));
        }
    };
}
inline fn Div(comptime T: type) !type {
    switch (T) {
        void => return CompileError.InvalidInstructionType,
        else => {},
    }
    return struct {
        fn eval(vm: *VM) !void {
            try vm.push_stack(try vm.pop_stack(T) / try vm.pop_stack(T));
        }
    };
}
inline fn Print(comptime T: type) !type {
    return struct {
        fn eval(vm: *VM) !void {
            switch (T) {
                void => std.debug.print("none\n", .{}),
                u64 => std.debug.print("{any}\n", .{vm.pop_stack(u64)}),
                else => unreachable,
            }
            // try vm.push_stack(try vm.pop_stack(T) / try vm.pop_stack(T));
        }
    };
}

fn result(expr: *const Expr) Type {
    return switch (expr.*) {
        .value => Type.u64,
        .add, .sub, .mul, .div => |value| result(value[0]),
        .print => Type.none,
    };
}

inline fn type_switch(t: Type, comptime f: anytype) !(*const fn (*VM) anyerror!void) {
    return switch (t) {
        .none => (try f(void)).eval,
        .u64 => (try f(u64)).eval,
    };
}
inline fn binary_operator(compiler: *Compiler, value: anytype, comptime f: anytype) !void {
    const returns = result(value[0]);
    if (!returns.eql(&result(value[1])))
        return CompileError.DifferentOperandTypes;
    try compile(value[1].*, compiler);
    try compile(value[0].*, compiler);
    try compiler.push_expr(try type_switch(returns, f));
}
fn compile(expr: Expr, compiler: *Compiler) anyerror!void {
    switch (expr) {
        .value => |value| {
            try compiler.push_data(value);
            try compiler.push_expr((try ReadData(u64)).eval);
        },
        .add => |value| try binary_operator(compiler, value, Add),
        .sub => |value| try binary_operator(compiler, value, Sub),
        .mul => |value| try binary_operator(compiler, value, Mul),
        .div => |value| try binary_operator(compiler, value, Div),
        .print => |value| {
            const returns = result(value);
            try compile(value.*, compiler);
            try compiler.push_expr(try type_switch(returns, Print));
        },
    }
}

const allocator = std.heap.page_allocator;
const VM = struct {
    data_ptr: [*]u8,
    stack_ptr: usize = 0,
    stack: *[1048576]u8,

    const Self = @This();

    inline fn read_data(self: *Self, comptime T: type) T {
        // std.debug.print("data {any}\n", .{T});
        @setRuntimeSafety(false);
        const value = @as(*T, @ptrCast(@alignCast(self.data_ptr))).*;
        self.data_ptr += @sizeOf(T);
        return value;
    }
    fn push_stack(self: *Self, value: anytype) !void {
        // std.debug.print("push {any}\n", .{@TypeOf(value)});
        @setRuntimeSafety(false);
        const new_stack_ptr = self.stack_ptr + @sizeOf(@TypeOf(value));
        if (new_stack_ptr > self.stack.len - 1) {
            return VMError.StackOverflow;
        }
        @as(*@TypeOf(value), @ptrCast(@alignCast(&self.stack[self.stack_ptr]))).* = value;
        self.stack_ptr = new_stack_ptr;
    }
    fn pop_stack(self: *Self, comptime T: type) !T {
        // std.debug.print("pop {any}\n", .{T});
        @setRuntimeSafety(false);
        self.stack_ptr -= @sizeOf(T);
        if (self.stack_ptr > self.stack.len - 1) {
            return VMError.StackUnderflow;
        }
        return @as(*T, @ptrCast(@alignCast(&self.stack[self.stack_ptr]))).*;
    }
};

pub fn main() !void {
    const exprs = [_]Expr{Expr{ .print = &Expr{ .add = .{
        &Expr{ .value = 123 },
        &Expr{ .value = 69 },
    } } }};
    var compiler = Compiler.new();
    for (exprs) |expr|
        try compile(expr, &compiler);

    const data = try compiler.data.toOwnedSlice();
    const compiled = try compiler.exprs.toOwnedSlice();
    const stack = try allocator.create([1048576]u8);
    defer allocator.free(data);
    defer allocator.free(compiled);
    defer allocator.free(stack);

    var vm = VM{
        .data_ptr = data.ptr,
        .stack = stack,
    };

    for (compiled) |expr| {
        try expr(&vm);
    }
}
