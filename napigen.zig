const root = @import("root");
const std = @import("std");
const napi = @import("napi.zig");

// export the whole napi
pub usingnamespace napi;

// define error types
pub const NapiError = error{ napi_invalid_arg, napi_object_expected, napi_string_expected, napi_name_expected, napi_function_expected, napi_number_expected, napi_boolean_expected, napi_array_expected, napi_generic_failure, napi_pending_exception, napi_cancelled, napi_escape_called_twice, napi_handle_scope_mismatch, napi_callback_scope_mismatch, napi_queue_full, napi_closing, napi_bigint_expected, napi_date_expected, napi_arraybuffer_expected, napi_detachable_arraybuffer_expected, napi_would_deadlock };
pub const Error = std.mem.Allocator.Error || error{InvalidArgumentCount} || NapiError;

/// translate napi_status > 0 to NapiError with the same name
pub fn check(status: napi.napi_status) Error!void {
    if (status != napi.napi_ok) {
        inline for (comptime std.meta.fieldNames(NapiError)) |f| {
            if (status == @field(napi, f)) return @field(NapiError, f);
        } else @panic("unknown napi err");
    }
}

pub const allocator = std.heap.c_allocator;

/// Convenience helper to define N-API module with a single function
pub fn defineModule(comptime init_fn: fn (*JsContext, napi.napi_value) anyerror!napi.napi_value) void {
    const NapigenNapiModule = struct {
        fn register(env: napi.napi_env, exports: napi.napi_value) callconv(.C) napi.napi_value {
            var cx = JsContext.init(env, exports) catch @panic("could not init JS context");
            return init_fn(cx, exports) catch |e| cx.throw(e);
        }
    };

    @export(NapigenNapiModule.register, .{ .name = "napi_register_module_v1", .linkage = .strong });
}

fn KnownArgsTuple(comptime Function: type) type {
    const info = @typeInfo(Function);
    if (info != .Fn)
        @compileError("ArgsTuple expects a function type");

    const function_info = info.Fn;

    comptime var argument_field_list: [function_info.params.len]type = undefined;
    inline for (function_info.params, 0..) |arg, i| {
        const T = arg.type orelse @compileError("cannot create ArgsTuple for function with an 'anytype' parameter");
        argument_field_list[i] = T;
    }

    return std.meta.Tuple(&argument_field_list);
}

pub fn defineConvertedCModule(comptime CT: type, comptime filter: ?fn ([]const u8) bool) void {
    const initFn = struct {
        fn initModule(js: *JsContext, exports: napi.napi_value) Error!napi.napi_value {
            const sizeMapObj = try js.createObject();
            const structDecoderMapObj = try js.createObject();
            const structEncoderMapObj = try js.createObject();
            const functionMapObj = try js.createObject();
            const asyncFunctionMapObj = try js.createObject();
            const constantMapObj = try js.createObject();
            const functionSignatureMapObj = try js.createObject();
            const wrapBuffFunction = try js.createNamedFunction("wrapBuff", JsContext.wrapBuff);

            @setEvalBranchQuota(100_000);
            inline for (comptime std.meta.declarations(CT)) |d| {
                if (comptime filter != null and filter.?(d.name) == false) continue;
                const thisField = comptime @field(CT, d.name);
                const typeOfField = comptime @TypeOf(thisField);
                const jsUndefined = try js.undefined();
                if (typeOfField == type) {
                    const thisType = @typeInfo(thisField);
                    if (thisType == .Opaque) {
                        continue;
                    }

                    try js.setNamedProperty(sizeMapObj, d.name, try js.createNumber(getSizeOf(thisField)));

                    if (thisType == .Struct or thisType == .Union) {
                        const ct_name = comptime cTypeName(thisField);
                        const c_name = comptime ct_name ++ "";
                        var existedDecoder = try js.getNamedProperty(structDecoderMapObj, c_name);

                        if (try js.equals(existedDecoder, jsUndefined)) {
                            existedDecoder = try js.createStructDecoderFunction(thisField);
                            try js.setNamedProperty(structDecoderMapObj, c_name, existedDecoder);
                        }

                        try js.setNamedProperty(structDecoderMapObj, d.name ++ "", existedDecoder);

                        var existedEncoder = try js.getNamedProperty(structEncoderMapObj, c_name);

                        if (try js.equals(existedEncoder, jsUndefined)) {
                            existedEncoder = try js.createStructEncoderFunction(thisField);
                            try js.setNamedProperty(structEncoderMapObj, c_name, existedEncoder);
                        }

                        try js.setNamedProperty(structEncoderMapObj, d.name ++ "", existedEncoder);
                    }

                    continue;
                }

                const c_name = d.name ++ "";
                if (@typeInfo(typeOfField) == .Fn) {
                    if (comptime !isFunctionAutomaticallyExportable(typeOfField)) continue;
                    const fnArgs = KnownArgsTuple(typeOfField);
                    const fnTyp = @typeInfo(typeOfField).Fn;
                    const fnRes = fnTyp.return_type.?;
                    const args: fnArgs = undefined;

                    const fnArgsArray = try js.createArray();

                    inline for (std.meta.fields(fnArgs), 0..) |f, i| {
                        if (comptime f.type == *JsContext) {
                            continue;
                        }
                        const fld = @field(args, f.name);
                        try js.setElement(fnArgsArray, i, try js.createString(cTypeName(@TypeOf(fld))));
                    }
                    if (fnTyp.is_var_args) {
                        try js.setElement(fnArgsArray, fnTyp.params.len, try js.createString("..."));
                    }

                    const thisFn = try js.createNamedFunction(c_name, thisField);
                    try js.setNamedProperty(functionMapObj, c_name, thisFn);
                    const thisAsyncFn = try js.createNamedAsyncFunction(c_name, thisField);
                    try js.setNamedProperty(asyncFunctionMapObj, c_name, thisAsyncFn);

                    const thisSignatureObj = try js.createObject();

                    try js.setNamedProperty(thisSignatureObj, "fn", thisFn);
                    try js.setNamedProperty(thisSignatureObj, "args", fnArgsArray);
                    try js.setNamedProperty(thisSignatureObj, "ret", try js.createString(cTypeName(fnRes)));

                    try js.setNamedProperty(functionSignatureMapObj, c_name, thisSignatureObj);
                    continue;
                }
                try js.setNamedProperty(constantMapObj, c_name, try js.write(thisField));
            }

            const ptrSize = try js.createNumber(getSizeOf([*c]?*anyopaque));
            try js.setNamedProperty(sizeMapObj, "*void" ++ "", ptrSize);
            try js.setNamedProperty(sizeMapObj, "**void" ++ "", ptrSize);

            try js.exportOne(exports, "sizes", sizeMapObj);
            try js.exportOne(exports, "structDecoders", structDecoderMapObj);
            try js.exportOne(exports, "structEncoders", structEncoderMapObj);
            try js.exportOne(exports, "functions", functionMapObj);
            try js.exportOne(exports, "asyncFunctions", asyncFunctionMapObj);
            try js.exportOne(exports, "constants", constantMapObj);
            try js.exportOne(exports, "functionSignatures", functionSignatureMapObj);

            try js.exportOne(exports, "wrapBuff", wrapBuffFunction);

            return exports;
        }
    }.initModule;

    defineModule(initFn);
}

pub const JsContext = struct {
    env: napi.napi_env,
    arena: GenerationalArena,
    refs: std.AutoHashMapUnmanaged(usize, napi.napi_ref) = .{},
    cTypeSymbolRef: napi.napi_ref,
    threadPool: std.Thread.Pool,

    /// Init the JS context.
    pub fn init(env: napi.napi_env, exports: napi.napi_value) !*JsContext {
        var self = try allocator.create(JsContext);
        try check(napi.napi_set_instance_data(env, self, finalize, null));
        self.* = .{ .env = env, .arena = GenerationalArena.init(allocator), .cTypeSymbolRef = std.mem.zeroes(napi.napi_ref), .threadPool = undefined };
        try self.threadPool.init(.{ .allocator = allocator });
        const cTypeSymbol = try self.createSymbol("__cType");
        try check(napi.napi_create_reference(env, cTypeSymbol, 1, &self.cTypeSymbolRef));
        try self.exportOne(exports, "cTypeSymbol", cTypeSymbol);
        return self;
    }

    /// Deinit the JS context.
    pub fn deinit(self: *JsContext) void {
        var _d: u32 = undefined;
        _ = napi.napi_reference_unref(self.env, self.cTypeSymbolRef, &_d);
        self.threadPool.deinit();
        self.arena.deinit();
        allocator.destroy(self);
    }

    /// Retreive the JS context from the N-API environment.
    fn getInstance(env: napi.napi_env) *JsContext {
        var res: *JsContext = undefined;
        check(napi.napi_get_instance_data(env, @ptrCast(&res))) catch @panic("could not get JS context");
        return res;
    }

    fn finalize(_: napi.napi_env, data: ?*anyopaque, _: ?*anyopaque) callconv(.C) void {
        // instance data might be already destroyed
        const self: *JsContext = @ptrCast(@alignCast(data));
        self.deinit();
    }

    /// Get the type of a JS value.
    pub fn typeOf(self: *JsContext, val: napi.napi_value) Error!napi.napi_valuetype {
        var res: napi.napi_valuetype = undefined;
        try check(napi.napi_typeof(self.env, val, &res));
        return res;
    }

    /// Throw an error.
    pub fn throw(self: *JsContext, err: anyerror) napi.napi_value {
        const msg = @as([*c]const u8, @ptrCast(@errorName(err)));
        check(napi.napi_throw_error(self.env, null, msg)) catch |e| {
            if (e != error.napi_pending_exception) std.debug.panic("throw failed {s} {any}", .{ msg, e });
        };
        return self.undefined() catch @panic("throw return undefined");
    }
    /// Throw an error.
    pub fn toJsError(self: *JsContext, err: anyerror) napi.napi_value {
        const msg = self.createString(@errorName(err)) catch @panic("unable to create js string for error message");
        var jsErr: napi.napi_value = undefined;
        check(napi.napi_create_error(self.env, null, msg, &jsErr)) catch @panic("unable to create js error");

        return jsErr;
    }

    /// Get the JS `undefined` value.
    pub fn @"undefined"(self: *JsContext) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_undefined(self.env, &res));
        return res;
    }

    /// Get the JS `null` value.
    pub fn @"null"(self: *JsContext) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_null(self.env, &res));
        return res;
    }

    pub fn equals(self: *JsContext, lhs: napi.napi_value, rhs: napi.napi_value) Error!bool {
        var res: bool = undefined;
        try check(napi.napi_strict_equals(self.env, lhs, rhs, &res));
        return res;
    }

    pub fn createBuffer(self: *JsContext, length: usize) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_create_buffer(self.env, length, null, &res));
        return res;
    }

    /// Create a JS boolean value.
    pub fn createBoolean(self: *JsContext, val: bool) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_boolean(self.env, val, &res));
        return res;
    }

    /// Read a native boolean from a JS value.
    pub fn readBoolean(self: *JsContext, val: napi.napi_value) Error!bool {
        var res: bool = undefined;
        try check(napi.napi_get_value_bool(self.env, val, &res));
        return res;
    }

    /// Create a JS number value.
    pub fn createNumber(self: *JsContext, val: anytype) Error!napi.napi_value {
        var res: napi.napi_value = undefined;

        switch (@TypeOf(val)) {
            u8, u16, u32, c_uint, c_ushort, c_char => try check(napi.napi_create_uint32(self.env, val, &res)),
            u64, usize, c_ulong, c_ulonglong => try check(napi.napi_create_bigint_uint64(self.env, val, &res)),
            i8, i16, i32, c_int, c_short => try check(napi.napi_create_int32(self.env, val, &res)),
            i64, isize, c_long, c_longlong, @TypeOf(0) => try check(napi.napi_create_bigint_int64(self.env, val, &res)),
            f16, f32, f64, c_longdouble, @TypeOf(0.0) => try check(napi.napi_create_double(self.env, @floatCast(val), &res)),
            else => |T| @compileError(@typeName(T) ++ " is not supported number"),
        }

        return res;
    }

    /// Read a native number from a JS value.
    pub fn readNumber(self: *JsContext, comptime T: type, val: napi.napi_value) Error!T {
        var res: T = undefined;
        var lossless: bool = undefined; // TODO: check overflow?

        switch (T) {
            u8, u16, c_ushort, c_char => res = @as(T, @truncate(try self.read(u32, val))),
            u32, c_uint => try check(napi.napi_get_value_uint32(self.env, val, &res)),
            u64, usize, c_ulong, c_ulonglong => try check(napi.napi_get_value_bigint_uint64(self.env, val, &res, &lossless)),
            i8, i16, c_short => res = @as(T, @truncate(try self.read(i32, val))),
            i32, c_int => try check(napi.napi_get_value_int32(self.env, val, &res)),
            i64, isize, c_long, c_longlong => try check(napi.napi_get_value_bigint_int64(self.env, val, &res, &lossless)),
            f16, f32 => res = @as(T, @floatCast(try self.readNumber(f64, val))),
            f64, c_longdouble => try check(napi.napi_get_value_double(self.env, val, &res)),
            else => @compileError(@typeName(T) ++ " is not supported number"),
        }

        return res;
    }

    pub fn createSymbol(self: *JsContext, description: []const u8) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        if (description.len > 0) {
            try check(napi.napi_create_symbol(self.env, try self.createString(description), &res));
            return res;
        }
        try check(napi.napi_create_symbol(self.env, null, &res));
        return res;
    }

    /// Create a JS string value.
    pub fn createString(self: *JsContext, val: []const u8) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_create_string_utf8(self.env, @as([*c]const u8, @ptrCast(val)), val.len, &res));
        return res;
    }

    /// Get the length of a JS string value.
    pub fn getStringLength(self: *JsContext, val: napi.napi_value) Error!usize {
        var res: usize = undefined;
        try check(napi.napi_get_value_string_utf8(self.env, val, null, 0, &res));
        return res;
    }

    /// Read JS string into a temporary, arena-allocated buffer.
    pub fn readString(self: *JsContext, val: napi.napi_value) Error![]const u8 {
        var len: usize = try self.getStringLength(val);
        var buf = try self.arena.allocator().alloc(u8, len + 1);
        try check(napi.napi_get_value_string_utf8(self.env, val, @as([*c]u8, @ptrCast(buf)), buf.len, &len));
        return buf[0..len];
    }

    pub fn readStringCustomAllocator(self: *JsContext, val: napi.napi_value, customAllocator: std.mem.Allocator) Error![]const u8 {
        var len: usize = try self.getStringLength(val);
        var buf = try customAllocator.alloc(u8, len + 1);
        try check(napi.napi_get_value_string_utf8(self.env, val, @as([*c]u8, @ptrCast(buf)), buf.len, &len));
        return buf[0..len];
    }

    /// Create an empty JS array.
    pub fn createArray(self: *JsContext) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_create_array(self.env, &res));
        return res;
    }

    /// Create a JS array with a given length.
    pub fn createArrayWithLength(self: *JsContext, length: u32) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_create_array_with_length(self.env, length, &res));
        return res;
    }

    pub fn createTypedArray(self: *JsContext, comptime elemType: type, buff: anytype) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        const size = @as(usize, @truncate(buff.len));
        const byteSize = switch (elemType) {
            f16 => @sizeOf([size]f32),
            else => @sizeOf([size]elemType),
        };
        var ptr: *[size]elemType = undefined;
        var arrayBuffer: napi.napi_value = undefined;
        try check(napi.napi_create_arraybuffer(self.env, byteSize, @as([*c]?*anyopaque, @ptrCast(&ptr)), &arrayBuffer));
        if (elemType == f16) {
            fp16ToFp32Array(buff, ptr.*);
        } else {
            @memcpy(ptr, &buff);
        }
        const typedarrayType = typedArrayTypeOf(elemType);
        try check(napi.napi_create_typedarray(self.env, typedarrayType, size, arrayBuffer, 0, &res));
        return res;
    }

    /// Create a JS array from a native array/slice.
    pub fn createArrayFrom(self: *JsContext, val: anytype, comptime elemType: type) Error!napi.napi_value {
        switch (@typeInfo(elemType)) {
            .Int, .Float => {
                return try createTypedArray(self, elemType, val);
            },
            else => {},
        }

        const res = try self.createArrayWithLength(@as(u32, @truncate(val.len)));
        for (val, 0..) |v, i| {
            try self.setElement(res, @as(u32, @truncate(i)), try self.write(v));
        }
        return res;
    }

    /// Get the length of a JS array.
    pub fn getArrayLength(self: *JsContext, array: napi.napi_value) Error!u32 {
        var res: u32 = undefined;
        try check(napi.napi_get_array_length(self.env, array, &res));
        return res;
    }

    /// Read a native slice from a JS array.
    pub fn readArray(self: *JsContext, comptime T: type, array: napi.napi_value) Error![]T {
        var isTypedArray: bool = undefined;
        try check(napi.napi_is_typedarray(self.env, array, &isTypedArray));
        if (isTypedArray) {
            var res: *anyopaque = undefined;
            var len: usize = undefined;
            var itemTyp: usize = undefined;
            var arrayBufferOffset: usize = undefined;
            var arrayBuffer: napi.napi_value = undefined;
            try check(napi.napi_get_typedarray_info(self.env, array, @ptrCast(&itemTyp), &len, @ptrCast(&res), &arrayBuffer, &arrayBufferOffset));
            var slicePtr: []T = undefined;
            slicePtr.len = len;
            slicePtr.ptr = @ptrCast(@alignCast(res));

            return slicePtr;
        }

        const len: u32 = try self.getArrayLength(array);
        const res = try self.arena.allocator().alloc(T, len);
        for (res, 0..) |*v, i| {
            v.* = try self.read(T, try self.getElement(array, @as(u32, @intCast(i))));
        }
        return res;
    }

    /// Read a native slice from a JS array.
    pub fn readArrayCustomAllocator(self: *JsContext, comptime T: type, array: napi.napi_value, customAllocator: std.mem.Allocator) Error![]T {
        var isTypedArray: bool = undefined;
        try check(napi.napi_is_typedarray(self.env, array, &isTypedArray));
        if (isTypedArray) {
            var res: *[]T = undefined;
            var len: usize = undefined;
            var itemTyp: usize = undefined;
            var arrayBufferOffset: usize = undefined;
            var arrayBuffer: napi.napi_value = undefined;
            try check(napi.napi_get_typedarray_info(self.env, array, &itemTyp, &len, @as([*c]?*anyopaque, @ptrCast(&res)), &arrayBuffer, &arrayBufferOffset));

            return @as([len]T, res);
        }

        const len: u32 = try self.getArrayLength(array);
        const res = try customAllocator.alloc(T, len);
        for (res, 0..) |*v, i| {
            v.* = try self.read(T, try self.getElement(array, @as(u32, @intCast(i))));
        }
        return res;
    }

    /// Read a native fixed-size array from a JS array.
    pub fn readArrayFixed(self: *JsContext, comptime T: type, comptime len: usize, array: napi.napi_value) Error![len]T {
        var isTypedArray: bool = undefined;
        try check(napi.napi_is_typedarray(self.env, array, &isTypedArray));
        if (isTypedArray) {
            var res: *[len]T = undefined;
            var len_: usize = undefined;
            var itemTyp: c_uint = undefined;
            var arrayBufferOffset: usize = undefined;
            var arrayBuffer: napi.napi_value = undefined;
            try check(napi.napi_get_typedarray_info(self.env, array, &itemTyp, &len_, @as([*c]?*anyopaque, @ptrCast(&res)), &arrayBuffer, &arrayBufferOffset));

            return res.*;
        }

        var res: [len]T = undefined;
        for (0..len) |i| {
            res[i] = try self.read(T, try self.getElement(array, @as(u32, @intCast(i))));
        }
        return res;
    }

    /// Read a native fixed-size array from a JS array.
    pub fn readArrayFixedCustomAllocator(self: *JsContext, comptime T: type, comptime len: usize, array: napi.napi_value, customAllocator: std.mem.Allocator) Error![len]T {
        var isTypedArray: bool = undefined;
        try check(napi.napi_is_typedarray(self.env, array, &isTypedArray));
        if (isTypedArray) {
            var res: *[len]T = undefined;
            var len_: usize = undefined;
            var itemTyp: c_uint = undefined;
            var arrayBufferOffset: usize = undefined;
            var arrayBuffer: napi.napi_value = undefined;
            try check(napi.napi_get_typedarray_info(self.env, array, &itemTyp, &len_, @as([*c]?*anyopaque, @ptrCast(&res)), &arrayBuffer, &arrayBufferOffset));

            return res.*;
        }

        var res: [len]T = undefined;
        for (0..len) |i| {
            res[i] = try self.readCustomAllocator(T, try self.getElement(array, @as(u32, @intCast(i))), customAllocator);
        }
        return res;
    }

    /// Get a JS value from a JS array by index.
    pub fn getElement(self: *JsContext, array: napi.napi_value, index: u32) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_element(self.env, array, index, &res));
        return res;
    }

    /// Set a JS value to a JS array by index.
    pub fn setElement(self: *JsContext, array: napi.napi_value, index: u32, value: napi.napi_value) Error!void {
        try check(napi.napi_set_element(self.env, array, index, value));
    }

    /// Create a JS array from a tuple.
    pub fn createTuple(self: *JsContext, val: anytype) Error!napi.napi_value {
        const fields = std.meta.fields(@TypeOf(val));
        const res = try self.createArrayWithLength(fields.len);
        var cTypeSymbol: napi.napi_value = undefined;
        try check(napi.napi_get_reference_value(self.env, self.cTypeSymbolRef, &cTypeSymbol));
        try self.setProperty(res, cTypeSymbol, try self.createString(cTypeName(@TypeOf(val))));
        inline for (fields, 0..) |f, i| {
            const v = try self.write(@field(val, f.name));
            try self.setElement(res, @as(u32, @truncate(i)), v);
        }
        return res;
    }

    pub fn isBuffer(self: *JsContext, val: napi.napi_value) !bool {
        var isJsBuffer: bool = undefined;
        try check(napi.napi_is_buffer(self.env, val, &isJsBuffer));
        return isJsBuffer;
    }

    /// Read a JS array into a tuple.
    pub fn readTuple(self: *JsContext, comptime T: type, val: napi.napi_value) Error!T {
        var res: T = undefined;

        const isJsBuffer: bool = try self.isBuffer(val);
        if (isJsBuffer) {
            var buffLength: usize = undefined;
            try check(napi.napi_get_buffer_info(self.env, val, @as([*c]?*anyopaque, @ptrCast(@alignCast(&res))), &buffLength));

            return res;
        }

        const fields = std.meta.fields(T);
        inline for (fields, 0..) |f, i| {
            const v = try self.getElement(val, @as(u32, @truncate(i)));
            @field(res, f.name) = try self.read(f.type, v);
        }
        return res;
    }

    /// Read a JS array into a tuple.
    pub fn readTupleCustomAllocator(self: *JsContext, comptime T: type, val: napi.napi_value, customAllocator: std.mem.Allocator) Error!T {
        var res: T = undefined;

        const isJsBuffer: bool = try self.isBuffer(val);
        if (isJsBuffer) {
            var buffLength: usize = undefined;
            try check(napi.napi_get_buffer_info(self.env, val, @as([*c]?*anyopaque, @ptrCast(@alignCast(&res))), &buffLength));

            return res;
        }

        const fields = std.meta.fields(T);
        inline for (fields, 0..) |f, i| {
            const v = try self.getElement(val, @as(u32, @truncate(i)));
            @field(res, f.name) = try self.readCustomAllocator(f.type, v, customAllocator);
        }
        return res;
    }

    /// Create an empty JS object.
    pub fn createObject(self: *JsContext) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_create_object(self.env, &res));
        return res;
    }

    /// Create a JS object from a native value.
    pub fn createObjectFrom(self: *JsContext, val: anytype) Error!napi.napi_value {
        const res: napi.napi_value = try self.createObject();
        var cTypeSymbol: napi.napi_value = undefined;
        try check(napi.napi_get_reference_value(self.env, self.cTypeSymbolRef, &cTypeSymbol));
        try self.setProperty(res, cTypeSymbol, try self.createString(cTypeName(@TypeOf(val))));
        inline for (std.meta.fields(@TypeOf(val))) |f| {
            const v = try self.write(@field(val, f.name));
            try self.setNamedProperty(res, f.name ++ "", v);
        }
        return res;
    }

    /// Read a struct/tuple from a JS object.
    pub fn readObject(self: *JsContext, comptime T: type, val: napi.napi_value) Error!T {
        var res: T = undefined;

        const isJsBuffer: bool = try self.isBuffer(val);
        if (isJsBuffer) {
            var buffLength: usize = undefined;
            try check(napi.napi_get_buffer_info(self.env, val, @as([*c]?*anyopaque, @ptrCast(@alignCast(&res))), &buffLength));

            return res;
        }

        inline for (std.meta.fields(T)) |f| {
            const v = try self.getNamedProperty(val, f.name ++ "");
            @field(res, f.name) = try self.read(f.type, v);
        }
        return res;
    }

    /// Read a struct/tuple from a JS object.
    pub fn readObjectCustomAllocator(self: *JsContext, comptime T: type, val: napi.napi_value, customAllocator: std.mem.Allocator) Error!T {
        var res: T = undefined;

        const isJsBuffer: bool = try self.isBuffer(val);
        if (isJsBuffer) {
            var buffLength: usize = undefined;
            try check(napi.napi_get_buffer_info(self.env, val, @as([*c]?*anyopaque, @ptrCast(@alignCast(&res))), &buffLength));

            return res;
        }

        inline for (std.meta.fields(T)) |f| {
            const v = try self.getNamedProperty(val, f.name ++ "");
            @field(res, f.name) = try self.readCustomAllocator(f.type, v, customAllocator);
        }
        return res;
    }

    /// Get the JS value of an object property by name.
    pub fn getNamedProperty(self: *JsContext, object: napi.napi_value, prop_name: [*:0]const u8) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_named_property(self.env, object, prop_name, &res));
        return res;
    }

    pub fn getProperty(self: *JsContext, object: napi.napi_value, prop: napi.napi_value) Error!napi.napi_value {
        var res: napi.napi_value = undefined;
        try check(napi.napi_get_property(self.env, object, prop, &res));
        return res;
    }

    /// Set the JS value of an object property by name.
    pub fn setNamedProperty(self: *JsContext, object: napi.napi_value, prop_name: [*:0]const u8, value: napi.napi_value) Error!void {
        try check(napi.napi_set_named_property(self.env, object, prop_name, value));
    }

    pub fn setProperty(self: *JsContext, object: napi.napi_value, prop: napi.napi_value, value: napi.napi_value) Error!void {
        try check(napi.napi_set_property(self.env, object, prop, value));
    }

    pub fn wrapPtr(self: *JsContext, val: anytype) Error!napi.napi_value {
        const info = @typeInfo(@TypeOf(val));
        if (comptime info == .Pointer and @typeInfo(info.Pointer.child) == .Fn) @compileError("use createFunction() to export functions");

        var res: napi.napi_value = undefined;

        if (self.refs.get(@intFromPtr(val))) |ref| {
            if (napi.napi_get_reference_value(self.env, ref, &res) == napi.napi_ok and res != null) {
                return res;
            } else {
                _ = napi.napi_delete_reference(self.env, ref);
            }
        }

        var ref: napi.napi_ref = undefined;
        res = try self.createObject();
        var cTypeSymbol: napi.napi_value = undefined;
        try check(napi.napi_get_reference_value(self.env, self.cTypeSymbolRef, &cTypeSymbol));
        try self.setProperty(res, cTypeSymbol, try self.createString(cTypeName(@TypeOf(val))));
        try check(napi.napi_wrap(self.env, res, @constCast(val), &deleteRef, @as(*anyopaque, @ptrCast(@constCast(val))), &ref));
        try self.refs.put(allocator, @intFromPtr(val), ref);

        return res;
    }

    fn deleteRef(env: napi.napi_env, _: ?*anyopaque, ptr: ?*anyopaque) callconv(.C) void {
        var js = JsContext.getInstance(env);

        if (js.refs.get(@intFromPtr(ptr.?))) |ref| {
            // not sure if this is really needed but if we have a new ref and it's valid, we want to skip this
            var val: napi.napi_value = undefined;
            if (napi.napi_get_reference_value(env, ref, &val) == napi.napi_ok) return;

            _ = napi.napi_delete_reference(env, ref);
            _ = js.refs.remove(@intFromPtr(ptr.?));
        }
    }

    fn simpleDeleteRef(env: napi.napi_env, _: ?*anyopaque, ref: ?*anyopaque) callconv(.C) void {
        _ = napi.napi_delete_reference(env, @ptrCast(@alignCast(ref)));
    }

    /// Unwrap a pointer from a JS object.
    pub fn unwrap(self: *JsContext, comptime T: type, val: napi.napi_value) Error!*T {
        var res: *T = undefined;
        const isJsBuffer: bool = try self.isBuffer(val);
        if (isJsBuffer) {
            var buffLength: usize = undefined;
            try check(napi.napi_get_buffer_info(self.env, val, @as([*c]?*anyopaque, @ptrCast(&res)), &buffLength));
            const l2Res: **T = @ptrCast(@alignCast(res));
            return switch (@typeInfo(T)) {
                .Opaque => l2Res.*,
                else => res,
            };
        }

        try check(napi.napi_unwrap(self.env, val, @as([*c]?*anyopaque, @ptrCast(&res))));
        return res;
    }

    pub fn isWrapped(self: *JsContext, val: napi.napi_value) bool {
        var res: *anyopaque = undefined;
        const r = napi.napi_unwrap(self.env, val, @as([*c]?*anyopaque, @ptrCast(&res)));
        if (r == napi.napi_ok) {
            return true;
        }
        return false;
    }

    pub fn briefNapiValue(self: *JsContext, val: napi.napi_value) !void {
        const jsType = try self.typeOf(val);

        switch (jsType) {
            napi.napi_undefined => std.debug.print("undefined\n", .{}),
            napi.napi_null => std.debug.print("null\n", .{}),
            napi.napi_boolean => std.debug.print("boolean {any}\n", .{self.readBoolean(val)}),
            napi.napi_string => std.debug.print("string {any}\n", .{self.readString(val)}),
            napi.napi_symbol => std.debug.print("symbol {any}\n", .{val}),
            napi.napi_object => {
                var jsPropNames: napi.napi_value = undefined;
                try check(napi.napi_get_property_names(self.env, val, &jsPropNames));
                const propNames = try self.readArray([]const u8, jsPropNames);
                std.debug.print("object {any}\n", .{propNames});
            },
            napi.napi_function => std.debug.print("function {any}\n", .{val}),
            napi.napi_external => std.debug.print("external {any}\n", .{val}),
            napi.napi_bigint => std.debug.print("bigint {any}\n", .{val}),
            else => std.debug.print("others {any}\n", .{val}),
        }
    }

    pub const read = if (@hasDecl(root, "napigenRead")) root.napigenRead else defaultRead;

    pub fn defaultRead(self: *JsContext, comptime T: type, val: napi.napi_value) Error!T {
        if (T == napi.napi_value) return val;
        if (comptime isString(T)) return self.readString(val);

        return switch (@typeInfo(T)) {
            .Void => void{},
            .Null => null,
            .Bool => self.readBoolean(val),
            .Int, .ComptimeInt, .Float, .ComptimeFloat => self.readNumber(T, val),
            .Enum => std.meta.intToEnum(T, self.read(u32, val)),
            .Struct, .Union => if (isTuple(T)) self.readTuple(T, val) else self.readObject(T, val),
            .Optional => |info| if (try self.typeOf(val) == napi.napi_null) null else self.read(info.child, val),
            .Pointer => |info| switch (info.size) {
                .One, .C => self.unwrap(info.child, val),
                .Slice => self.readArray(info.child, val),
                else => @compileError("reading " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
            },
            .Array => |info| try self.readArrayFixed(info.child, info.len, val),
            else => @compileError("reading " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
        };
    }

    pub const readCustomAllocator = if (@hasDecl(root, "napigenReadCustomAllocator")) root.napigenReadCustomAllocator else defaultReadCustomAllocator;

    pub fn defaultReadCustomAllocator(self: *JsContext, comptime T: type, val: napi.napi_value, customAllocator: std.mem.Allocator) Error!T {
        if (T == napi.napi_value) return val;
        if (comptime isString(T)) return self.readString(val);

        return switch (@typeInfo(T)) {
            .Void => void{},
            .Null => null,
            .Bool => self.readBoolean(val),
            .Int, .ComptimeInt, .Float, .ComptimeFloat => self.readNumber(T, val),
            .Enum => std.meta.intToEnum(T, self.readCustomAllocator(u32, val, customAllocator)),
            .Struct, .Union => if (isTuple(T)) self.readTupleCustomAllocator(T, val, customAllocator) else self.readObjectCustomAllocator(T, val, customAllocator),
            .Optional => |info| if (try self.typeOf(val) == napi.napi_null) null else self.readCustomAllocator(info.child, val, customAllocator),
            .Pointer => |info| switch (info.size) {
                .One, .C => self.unwrap(info.child, val),
                .Slice => self.readArrayCustomAllocator(info.child, val, customAllocator),
                else => @compileError("reading " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
            },
            .Array => |info| try self.readArrayFixedCustomAllocator(info.child, info.len, val, customAllocator),
            else => @compileError("reading " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
        };
    }

    pub fn dynamicRead(self: *JsContext, napiSlice: []napi.napi_value, result: *[]?[*c]anyopaque) !void {
        var ptr: ?*u8 = @ptrCast(@alignCast(result.*));
        for (napiSlice) |nv| {
            const napiType = try self.typeOf(nv);
            switch (napiType) {
                napi.napi_null, napi.napi_undefined => {
                    const ptrPtr: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    ptrPtr.* = null;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_boolean => {
                    const boolVal = try self.readBoolean(nv);
                    const tgt: [*c]bool = @ptrCast(@alignCast(ptr));
                    tgt.* = boolVal;
                    ptr += @sizeOf(boolVal);
                },
                napi.napi_number => {
                    const u32Val = try self.readNumber(u32, nv);
                    const tgt: [*c]u32 = @ptrCast(@alignCast(ptr));
                    tgt.* = u32Val;
                    ptr += @sizeOf(u32);
                },
                napi.napi_object => {
                    const unwrapped = try self.unwrap([*c]anyopaque, nv);
                    const tgt: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    tgt.* = unwrapped;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_string => {
                    const u8Slice = try self.readString(nv);
                    const tgt: [*c]const []u8 = @ptrCast(@alignCast(ptr));
                    tgt.* = u8Slice.ptr;
                    ptr += @sizeOf([*c]const []u8);
                },
                napi.napi_function => {
                    const unwrapped = try self.unwrap([*c]anyopaque, nv);
                    if (unwrapped == null) {
                        try self.throw(error.InvalidCallback);
                        return;
                    }
                    const tgt: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    tgt.* = unwrapped;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_bigint => {
                    const u64Val = try self.readNumber(u64, nv);
                    const tgt: [*c]u64 = @ptrCast(@alignCast(ptr));
                    tgt.* = u64Val;
                    ptr += @sizeOf(u64);
                },
                else => {},
            }
        }
    }

    pub fn dynamicReadCustomAllocator(self: *JsContext, napiSlice: []napi.napi_value, result: *[]?[*c]anyopaque, customAllocator: std.mem.Allocator) !void {
        var ptr: ?*u8 = @ptrCast(@alignCast(result.*));
        for (napiSlice) |nv| {
            const napiType = try self.typeOf(nv);
            switch (napiType) {
                napi.napi_null, napi.napi_undefined => {
                    const ptrPtr: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    ptrPtr.* = null;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_boolean => {
                    const boolVal = try self.readBoolean(nv);
                    const tgt: [*c]bool = @ptrCast(@alignCast(ptr));
                    tgt.* = boolVal;
                    ptr += @sizeOf(boolVal);
                },
                napi.napi_number => {
                    const u32Val = try self.readNumber(u32, nv);
                    const tgt: [*c]u32 = @ptrCast(@alignCast(ptr));
                    tgt.* = u32Val;
                    ptr += @sizeOf(u32);
                },
                napi.napi_object => {
                    const unwrapped = try self.unwrap([*c]anyopaque, nv);
                    const tgt: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    tgt.* = unwrapped;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_string => {
                    const u8Slice = try self.readStringCustomAllocator(nv, customAllocator);
                    const tgt: [*c]const []u8 = @ptrCast(@alignCast(ptr));
                    tgt.* = u8Slice.ptr;
                    ptr += @sizeOf([*c]const []u8);
                },
                napi.napi_function => {
                    const unwrapped = try self.unwrap([*c]anyopaque, nv);
                    if (unwrapped == null) {
                        try self.throw(error.InvalidCallback);
                        return;
                    }
                    const tgt: [*c]anyopaque = @ptrCast(@alignCast(ptr));
                    tgt.* = unwrapped;
                    ptr += @sizeOf([*c]anyopaque);
                },
                napi.napi_bigint => {
                    const u64Val = try self.readNumber(u64, nv);
                    const tgt: [*c]u64 = @ptrCast(@alignCast(ptr));
                    tgt.* = u64Val;
                    ptr += @sizeOf(u64);
                },
                else => {},
            }
        }
    }

    pub fn getGlobal(self: *JsContext) !napi.napi_value {
        var globalObj: napi.napi_value = undefined;
        try check(napi.napi_get_global(self.env, &globalObj));

        return globalObj;
    }

    pub const write = if (@hasDecl(root, "napigenWrite")) root.napigenWrite else defaultWrite;

    pub fn defaultWrite(self: *JsContext, val: anytype) Error!napi.napi_value {
        const T = @TypeOf(val);

        if (T == napi.napi_value) return val;
        if (comptime isString(T)) return self.createString(val);

        return switch (@typeInfo(T)) {
            .Void => self.undefined(),
            .Null => self.null(),
            .Bool => self.createBoolean(val),
            .Int, .ComptimeInt, .Float, .ComptimeFloat => self.createNumber(val),
            .Enum => self.createNumber(@as(u32, @intFromEnum(val))),
            .Struct, .Union => if (isTuple(T)) self.createTuple(val) else self.createObjectFrom(val),
            .Optional => if (val) |v| self.write(v) else self.null(),
            .Pointer => |info| switch (info.size) {
                .One, .C => self.wrapPtr(val),
                .Slice => self.createArrayFrom(val, info.child),
                else => @compileError("writing " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
            },
            .Array => |info| self.createArrayFrom(val, info.child),
            else => @compileError("writing " ++ @tagName(@typeInfo(T)) ++ " " ++ @typeName(T) ++ " is not supported"),
        };
    }

    /// Create a JS function.
    pub fn createFunction(self: *JsContext, comptime fun: anytype) Error!napi.napi_value {
        return self.createNamedFunction("anonymous", fun);
    }

    pub fn createNamedFunction(self: *JsContext, comptime name: [*:0]const u8, comptime fun: anytype) Error!napi.napi_value {
        return self.createNamedFunctionWithThisArg(name, fun, null);
    }

    /// Create a named JS function.
    pub fn createNamedFunctionWithThisArg(self: *JsContext, comptime name: [*:0]const u8, comptime fun: anytype, thisPtr: ?*anyopaque) Error!napi.napi_value {
        const F = @TypeOf(fun);
        const Args = KnownArgsTuple(F);
        const Res = @typeInfo(F).Fn.return_type.?;
        const FnInfo = @typeInfo(F).Fn;

        const Helper = struct {
            fn call(env: napi.napi_env, cb_info: napi.napi_callback_info) callconv(.C) napi.napi_value {
                var js = JsContext.getInstance(env);
                js.arena.inc();
                defer js.arena.dec();

                const args = readArgs(js, cb_info) catch |e| return js.throw(e);
                const res = @call(.auto, fun, args);

                if (comptime @typeInfo(Res) == .ErrorUnion) {
                    return if (res) |r| js.write(r) catch |e| js.throw(e) else |e| js.throw(e);
                } else {
                    return js.write(res) catch |e| js.throw(e);
                }
            }

            fn readArgs(js: *JsContext, cb_info: napi.napi_callback_info) Error!Args {
                var args: Args = undefined;
                var argc: usize = args.len;
                var argv: [args.len]napi.napi_value = undefined;

                var jsThis: napi.napi_value = undefined;
                var cThis: ?*anyopaque = undefined;
                try check(napi.napi_get_cb_info(js.env, cb_info, &argc, &argv, &jsThis, &cThis));
                const jsGlobal = try js.getGlobal();

                if (try js.equals(jsGlobal, jsThis)) {
                    jsThis = null;
                }
                if (!js.isWrapped(jsThis)) {
                    jsThis = null;
                }

                var i: usize = 0;
                inline for (std.meta.fields(Args)) |f| {
                    if (comptime f.type == *JsContext) {
                        @field(args, f.name) = js;
                        continue;
                    }
                    if (i == 0) {
                        switch (@typeInfo(f.type)) {
                            .Pointer => |info| switch (info.size) {
                                .One, .C => {
                                    if (cThis != null) {
                                        @field(args, f.name) = @ptrCast(@alignCast(cThis));
                                    } else if (jsThis != null) {
                                        @field(args, f.name) = try js.read(f.type, jsThis);
                                    } else {
                                        @field(args, f.name) = try js.read(f.type, argv[i]);
                                        i += 1;
                                    }
                                },
                                else => {
                                    @field(args, f.name) = try js.read(f.type, argv[i]);
                                    i += 1;
                                },
                            },
                            else => {
                                @field(args, f.name) = try js.read(f.type, argv[i]);
                                i += 1;
                            },
                        }
                    }
                }

                if (FnInfo.is_var_args) {
                    const resetNapiVals: [argc - i]napi.napi_value = argv + i;
                    var resetArgs: [16]?[*c]anyopaque = undefined;
                    try self.dynamicRead(resetNapiVals, &resetArgs);

                    return args ++ resetArgs;
                }

                if (i != argc) {
                    std.debug.print("Expected {d} args\n", .{i});
                    return error.InvalidArgumentCount;
                }

                return args;
            }
        };

        var res: napi.napi_value = undefined;
        try check(napi.napi_create_function(self.env, name, napi.NAPI_AUTO_LENGTH, &Helper.call, thisPtr, &res));
        return res;
    }

    pub fn createAsyncFunction(self: *JsContext, comptime fun: anytype) Error!napi.napi_value {
        return self.createNamedAsyncFunction("anonymous", fun);
    }

    pub fn createNamedAsyncFunction(self: *JsContext, comptime name: [*:0]const u8, comptime fun: anytype) Error!napi.napi_value {
        return self.createNamedAsyncFunctionWithThisArg(name, fun, null);
    }

    pub fn createNamedAsyncFunctionWithThisArg(self: *JsContext, comptime name: [*:0]const u8, comptime fun: anytype, thisPtr: ?*anyopaque) Error!napi.napi_value {
        const F = @TypeOf(fun);
        const Args = KnownArgsTuple(F);
        const Res = @typeInfo(F).Fn.return_type.?;
        const FnInfo = @typeInfo(F).Fn;

        const Helper1 = struct {
            fnArgs: Args,
            fnRet: Res,
            arena: std.heap.ArenaAllocator,
            jsDeferred: napi.napi_deferred,
            jsTSFN: napi.napi_threadsafe_function,

            fn callJsCb(env: napi.napi_env, jsCallback: napi.napi_value, context: ?*anyopaque, data: ?*anyopaque) callconv(.C) void {
                _ = jsCallback;
                _ = data;

                var js = JsContext.getInstance(env);
                const this: *@This() = @ptrCast(@alignCast((context)));
                defer allocator.destroy(this);
                defer this.arena.deinit();
                defer _ = napi.napi_release_threadsafe_function(this.jsTSFN, napi.napi_tsfn_release);
                const res = this.fnRet;

                if (comptime @typeInfo(Res) == .ErrorUnion) {
                    if (res) |r| {
                        check(napi.napi_resolve_deferred(env, this.jsDeferred, js.write(r))) catch |e| {
                            _ = napi.napi_reject_deferred(env, this.jsDeferred, js.toJsError(e));
                            return;
                        };
                    }
                    _ = napi.napi_reject_deferred(env, this.jsDeferred, js.toJsError(res));
                    return;
                }

                const jsRes = js.write(res) catch |e| {
                    _ = napi.napi_reject_deferred(env, this.jsDeferred, js.toJsError(e));
                    return;
                };

                check(napi.napi_resolve_deferred(env, this.jsDeferred, jsRes)) catch |e| {
                    _ = napi.napi_reject_deferred(env, this.jsDeferred, js.toJsError(e));
                    return;
                };
            }

            fn call(this: *@This()) void {
                const res = @call(.auto, fun, this.fnArgs);
                this.fnRet = res;
                _ = napi.napi_call_threadsafe_function(this.jsTSFN, @ptrCast(&this.fnRet), napi.napi_tsfn_blocking);
                defer _ = napi.napi_release_threadsafe_function(this.jsTSFN, napi.napi_tsfn_release);
            }
        };

        const Helper2 = struct {
            fn call(env: napi.napi_env, cb_info: napi.napi_callback_info) callconv(.C) napi.napi_value {
                var js = JsContext.getInstance(env);

                var promise: napi.napi_value = undefined;
                var deferred: napi.napi_deferred = undefined;
                check(napi.napi_create_promise(env, &deferred, &promise)) catch |e| return js.throw(e);
                var helper = allocator.create(Helper1) catch |e| return js.throw(e);
                helper.jsDeferred = deferred;
                helper.arena = std.heap.ArenaAllocator.init(allocator);

                const asyncResourceName = js.createString("native_call_" ++ @typeName(F)) catch |e| return js.throw(e);
                check(napi.napi_create_threadsafe_function(env, null, null, asyncResourceName, 0, 2, null, null, @ptrCast(helper), &Helper1.callJsCb, &helper.jsTSFN)) catch |e| return js.throw(e);
                helper.fnArgs = readArgs(js, cb_info, helper.arena.allocator()) catch |e| return js.throw(e);

                js.threadPool.spawn(Helper1.call, .{helper}) catch |e| return js.throw(e);

                return promise;
            }

            fn readArgs(js: *JsContext, cb_info: napi.napi_callback_info, customAllocator: std.mem.Allocator) Error!Args {
                var args: Args = undefined;
                var argc: usize = args.len;
                var argv: [args.len]napi.napi_value = undefined;

                var jsThis: napi.napi_value = undefined;
                var cThis: ?*anyopaque = undefined;
                try check(napi.napi_get_cb_info(js.env, cb_info, &argc, &argv, &jsThis, &cThis));
                const jsGlobal = try js.getGlobal();

                if (try js.equals(jsGlobal, jsThis)) {
                    jsThis = null;
                }
                if (!js.isWrapped(jsThis)) {
                    jsThis = null;
                }

                var i: usize = 0;
                inline for (std.meta.fields(Args)) |f| {
                    if (comptime f.type == *JsContext) {
                        @field(args, f.name) = js;
                        continue;
                    }
                    if (i == 0) {
                        switch (@typeInfo(f.type)) {
                            .Pointer => |info| switch (info.size) {
                                .One, .C => {
                                    if (cThis != null) {
                                        @field(args, f.name) = @ptrCast(@alignCast(cThis));
                                    } else if (jsThis != null) {
                                        @field(args, f.name) = try js.readCustomAllocator(f.type, jsThis, customAllocator);
                                    } else {
                                        @field(args, f.name) = try js.readCustomAllocator(f.type, argv[i], customAllocator);
                                        i += 1;
                                    }
                                },
                                else => {
                                    @field(args, f.name) = try js.readCustomAllocator(f.type, argv[i], customAllocator);
                                    i += 1;
                                },
                            },
                            else => {
                                @field(args, f.name) = try js.readCustomAllocator(f.type, argv[i], customAllocator);
                                i += 1;
                            },
                        }
                    } else {
                        @field(args, f.name) = try js.readCustomAllocator(f.type, argv[i], customAllocator);
                        i += 1;
                    }
                }

                if (FnInfo.is_var_args) {
                    const resetNapiVals: [argc - i]napi.napi_value = argv + i;
                    var resetArgs: [16]?[*c]anyopaque = undefined;
                    try self.dynamicReadCustomAllocator(resetNapiVals, &resetArgs, customAllocator);

                    return args ++ resetArgs;
                }

                if (i != argc) {
                    std.debug.print("Expected {d} args\n", .{i});
                    return error.InvalidArgumentCount;
                }

                return args;
            }
        };

        var res: napi.napi_value = undefined;
        try check(napi.napi_create_function(self.env, name, napi.NAPI_AUTO_LENGTH, &Helper2.call, thisPtr, &res));
        return res;
    }

    /// Call a JS function.
    pub fn callFunction(self: *JsContext, recv: napi.napi_value, fun: napi.napi_value, args: anytype) Error!napi.napi_value {
        const Args = @TypeOf(args);
        var argv: [std.meta.fields(Args).len]napi.napi_value = undefined;
        inline for (std.meta.fields(Args), 0..) |f, i| {
            argv[i] = try self.write(@field(args, f.name));
        }

        var res: napi.napi_value = undefined;
        try check(napi.napi_call_function(self.env, recv, fun, argv.len, &argv, &res));
        return res;
    }

    /// Export a single declaration.
    pub fn exportOne(self: *JsContext, exports: napi.napi_value, comptime name: []const u8, val: anytype) Error!void {
        const c_name = name ++ "";

        if (comptime @typeInfo(@TypeOf(val)) == .Fn) {
            try self.setNamedProperty(exports, c_name, try self.createNamedFunction(c_name, val));
        } else {
            try self.setNamedProperty(exports, c_name, try self.write(val));
        }
    }

    /// Export all public declarations from a module.
    pub fn exportAll(self: *JsContext, exports: napi.napi_value, comptime mod: anytype) Error!void {
        inline for (comptime std.meta.declarations(mod)) |d| {
            if (@TypeOf(@field(mod, d.name)) == type) continue;

            try self.exportOne(exports, d.name, @field(mod, d.name));
        }
    }

    pub fn createStructDecoderFunction(self: *JsContext, comptime typ: type) Error!napi.napi_value {
        switch (@typeInfo(typ)) {
            .Struct, .Union => {},
            else => @compileError("Struct decoder can only be created for Struct or Union"),
        }

        const tmp = struct {
            fn constructor(ijs: *JsContext, buff: [*c]?*anyopaque) Error!napi.napi_value {
                const typedPtr = @as(*typ, @ptrCast(buff));
                return ijs.createObjectFrom(typedPtr.*);
            }
        }.constructor;

        return try self.createNamedFunction(cTypeName(typ) ++ "_decoder", tmp);
    }

    pub fn createStructEncoderFunction(self: *JsContext, comptime typ: type) !napi.napi_value {
        switch (@typeInfo(typ)) {
            .Struct, .Union => {},
            else => @compileError("Struct encoder can only be created for Struct or Union"),
        }

        const tmp = struct {
            fn constructor(ijs: *JsContext, input: typ) !napi.napi_value {
                const jsBuffer = try ijs.createBuffer(@sizeOf(@TypeOf(input)));
                var buff: *typ = undefined;
                var buffSize: usize = undefined;
                try check(napi.napi_get_buffer_info(ijs.env, jsBuffer, @as([*c]?*anyopaque, @ptrCast(&buff)), &buffSize));
                buff.* = input;
                return jsBuffer;
            }
        }.constructor;

        return try self.createNamedFunction(cTypeName(typ) ++ "_encoder", tmp);
    }

    pub fn wrapBuff(self: *JsContext, target: napi.napi_value, jsBuff: napi.napi_value) !napi.napi_value {
        const targetType = try self.typeOf(target);

        if (targetType != napi.napi_object and targetType != napi.napi_function) {
            return self.throw(error.napi_object_expected);
        }
        const jsBuffValid = try self.isBuffer(jsBuff);
        if (!jsBuffValid) {
            return self.throw(error.napi_buffer_expected);
        }
        var res: *anyopaque = undefined;
        var buffLength: usize = undefined;
        var ref: napi.napi_ref = undefined;

        try check(napi.napi_get_buffer_info(self.env, jsBuff, @ptrCast(@alignCast(&res)), &buffLength));
        try check(napi.napi_create_reference(self.env, target, 1, &ref));
        try check(napi.napi_wrap(self.env, target, res, &simpleDeleteRef, ref, null));

        return target;
    }
};

// To allow reading strings and other slices, we need to allocate memory
// somewhere. Such data is only needed for a short time, so we can use a
// generational arena to free the memory when it is no longer needed
// - count is increased when a function is called and decreased when it returns
// - when count reaches 0, the arena is reset (but not freed)
const GenerationalArena = struct {
    count: u32 = 0,
    arena: std.heap.ArenaAllocator,

    pub fn init(child_allocator: std.mem.Allocator) GenerationalArena {
        return .{
            .arena = std.heap.ArenaAllocator.init(child_allocator),
        };
    }

    pub fn deinit(self: *GenerationalArena) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *GenerationalArena) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn inc(self: *GenerationalArena) void {
        self.count += 1;
    }

    pub fn dec(self: *GenerationalArena) void {
        self.count -= 1;
        if (self.count == 0) {
            _ = self.arena.reset(.retain_capacity);
        }
    }
};

pub fn isString(comptime T: type) bool {
    const info = @typeInfo(T);

    if (info == .Pointer) {
        const ptr = info.Pointer;
        if (ptr.size == .Slice and ptr.child == u8) return true;

        if (ptr.size == .One and ptr.is_const) {
            const child_info = @typeInfo(ptr.child);
            if (child_info == .Array and child_info.Array.child == u8) {
                return true;
            }
        }
    }

    return false;
}

fn isTuple(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Struct => |s| s.is_tuple,
        else => return false,
    };
}

fn _getLastPart(comptime input: []const u8) []const u8 {
    // Find the last occurrence of '.'
    const dot_index = std.mem.lastIndexOf(u8, input, ".");
    if (dot_index) |index| {
        // Slice the string starting from the character after the last '.'
        return input[index + 1 ..];
    }
    // If no '.' is found, return the entire string
    return input;
}

fn _pointerOf(comptime input: []const u8) []const u8 {
    return "*" ++ input;
}
fn _optionalOf(comptime input: []const u8) []const u8 {
    return "?" ++ input;
}

pub fn cTypeName(comptime inputType: type) []const u8 {
    switch (inputType) {
        c_char => return "u8",
        c_ushort => return "u16",
        c_uint => return "u32",
        c_ulong, c_ulonglong, usize => return "u64",

        c_short => return "i16",
        c_int => return "i32",
        c_long, c_longlong => return "i64",

        else => {},
    }

    return switch (@typeInfo(inputType)) {
        .Opaque => "void",
        .Struct, .Union => _getLastPart(@typeName(inputType)),
        .Pointer => |ptr| _pointerOf(cTypeName(ptr.child)),
        .Array => |arr| cTypeName(arr.child) ++ "[" ++ arr.len ++ "]",
        .Optional => |opt| cTypeName(opt.child),
        else => @typeName(inputType),
    };
}

fn typedArrayTypeOf(comptime elementType: type) c_uint {
    return switch (@typeInfo(elementType)) {
        .Int => |info| switch (info.signedness) {
            .signed => {
                if (info.bits > 64) {
                    @compileError("Type " ++ @typeName(elementType) ++ "cannot be put into a typed array.");
                }
                if (info.bits > 32) {
                    return 9;
                }
                if (info.bits > 16) {
                    return 5;
                }
                if (info.bits > 8) {
                    return 3;
                }
                return 0;
            },
            .unsigned => {
                if (info.bits > 64) {
                    @compileError("Type " ++ @typeName(elementType) ++ "cannot be put into a typed array.");
                }
                if (info.bits > 32) {
                    return 10;
                }
                if (info.bits > 16) {
                    return 6;
                }
                if (info.bits > 8) {
                    return 4;
                }
                return 1;
            },
        },
        .Float => |info| {
            if (info.bits > 64) {
                @compileError("Type " ++ @typeName(elementType) ++ "cannot be put into a typed array.");
            }
            if (info.bits > 32) {
                return 8;
            }

            return 7;
        },

        else => @compileError("Type " ++ @typeName(elementType) ++ "cannot be put into a typed array."),
    };
}

pub fn fp16ToFp32Array(fp16Array: []f16, fp32Array: []f32) []f32 {
    // Perform the conversion.
    for (fp16Array, 0..) |fp16Value, i| {
        fp32Array[i] = @floatCast(fp16Value);
    }

    return fp32Array;
}

fn getSizeOf(comptime inputType: type) u32 {
    return switch (@typeInfo(inputType)) {
        .Opaque => 0,
        else => @sizeOf(inputType),
    };
}

fn isFunctionAutomaticallyExportable(comptime Function: type) bool {
    const info = @typeInfo(Function);
    if (info != .Fn) return false;

    const function_info = info.Fn;
    if (function_info.is_var_args) return false;

    inline for (function_info.params) |arg| {
        _ = arg.type orelse return false;
    }

    return true;
}
