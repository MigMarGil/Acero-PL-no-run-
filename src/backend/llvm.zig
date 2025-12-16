const std = @import("std");
const ast = @import("../ast.zig");

// LLVM C API bindings
pub const llvm = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/Transforms/Scalar.h");
    @cInclude("llvm-c/Transforms/Utils.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/ExecutionEngine.h");
    @cInclude("llvm-c/Support.h");
});

pub const LLVMBackend = struct {
    allocator: std.mem.Allocator,
    context: llvm.LLVMContextRef,
    module: llvm.LLVMModuleRef,
    builder: llvm.LLVMBuilderRef,
    target_machine: llvm.LLVMTargetMachineRef,
    
    // Symbol tables
    functions: std.StringHashMap(llvm.LLVMValueRef),
    variables: std.StringHashMap(llvm.LLVMValueRef),
    types: std.StringHashMap(llvm.LLVMTypeRef),
    
    // Current state
    current_function: ?llvm.LLVMValueRef = null,
    current_basic_block: ?llvm.LLVMBasicBlockRef = null,
    
    pub fn init(allocator: std.mem.Allocator, module_name: []const u8) !LLVMBackend {
        // Initialize LLVM
        llvm.LLVMInitializeAllTargetInfos();
        llvm.LLVMInitializeAllTargets();
        llvm.LLVMInitializeAllTargetMCs();
        llvm.LLVMInitializeAllAsmPrinters();
        llvm.LLVMInitializeAllAsmParsers();
        
        // Create context and module
        const context = llvm.LLVMContextCreate();
        const module = llvm.LLVMModuleCreateWithNameInContext(module_name.ptr, context);
        const builder = llvm.LLVMCreateBuilderInContext(context);
        
        // Create target machine
        var target: llvm.LLVMTargetRef = undefined;
        var target_triple: []const u8 = "x86_64-unknown-linux-gnu";
        
        if (llvm.LLVMGetTargetFromTriple(target_triple.ptr, &target, null) != 0) {
            // Try native target
            target_triple = llvm.LLVMGetDefaultTargetTriple();
            defer llvm.LLVMDisposeMessage(@ptrCast([*c]u8, target_triple));
            
            if (llvm.LLVMGetTargetFromTriple(target_triple, &target, null) != 0) {
                return error.TargetNotFound;
            }
        }
        
        const cpu = "generic";
        const features = "";
        const opt_level = llvm.LLVMCodeGenLevelDefault;
        const reloc_mode = llvm.LLVMRelocDefault;
        const code_model = llvm.LLVMCodeModelDefault;
        
        const target_machine = llvm.LLVMCreateTargetMachine(
            target,
            target_triple.ptr,
            cpu.ptr,
            features.ptr,
            opt_level,
            reloc_mode,
            code_model,
        );
        
        // Set module target triple
        llvm.LLVMSetTarget(module, target_triple.ptr);
        
        return LLVMBackend{
            .allocator = allocator,
            .context = context,
            .module = module,
            .builder = builder,
            .target_machine = target_machine,
            .functions = std.StringHashMap(llvm.LLVMValueRef).init(allocator),
            .variables = std.StringHashMap(llvm.LLVMValueRef).init(allocator),
            .types = std.StringHashMap(llvm.LLVMTypeRef).init(allocator),
        };
    }
    
    pub fn deinit(self: *LLVMBackend) void {
        llvm.LLVMDisposeBuilder(self.builder);
        llvm.LLVMDisposeTargetMachine(self.target_machine);
        llvm.LLVMDisposeModule(self.module);
        llvm.LLVMContextDispose(self.context);
        
        self.functions.deinit();
        self.variables.deinit();
        self.types.deinit();
    }
    
    pub fn compile(self: *LLVMBackend, program: *ast.Node) !void {
        // First pass: declare all functions and types
        try self.declareAll(program);
        
        // Second pass: generate code
        try self.generateCode(program);
        
        // Verify module
        var error_msg: [*c]u8 = null;
        if (llvm.LLVMVerifyModule(self.module, llvm.LLVMAbortProcessAction, &error_msg) != 0) {
            defer llvm.LLVMDisposeMessage(error_msg);
            std.debug.print("Module verification failed: {s}\n", .{error_msg});
            return error.ModuleVerificationFailed;
        }
    }
    
    fn declareAll(self: *LLVMBackend, program: *ast.Node) !void {
        switch (program.ty) {
            .program => {
                for (program.data.program.declarations) |decl| {
                    try self.declareNode(decl);
                }
            },
            else => {},
        }
    }
    
    fn declareNode(self: *LLVMBackend, node: *ast.Node) !void {
        switch (node.ty) {
            .function_decl => try self.declareFunction(node),
            .type_decl => try self.declareType(node),
            .struct_decl => try self.declareStruct(node),
            .actor_decl => try self.declareActor(node),
            else => {},
        }
    }
    
    fn declareFunction(self: *LLVMBackend, node: *ast.Node) !void {
        const func = &node.data.function_decl;
        
        // Convert return type
        const return_type = if (func.return_type) |rt|
            try self.convertType(rt)
        else
            llvm.LLVMVoidTypeInContext(self.context);
        
        // Convert parameter types
        var param_types = std.ArrayList(llvm.LLVMTypeRef).init(self.allocator);
        defer param_types.deinit();
        
        for (func.params) |param| {
            // TODO: Get parameter type from param node
            const param_type = llvm.LLVMInt32TypeInContext(self.context); // Placeholder
            try param_types.append(param_type);
        }
        
        // Create function type
        const func_type = llvm.LLVMFunctionType(
            return_type,
            if (param_types.items.len > 0) param_types.items.ptr else null,
            @intCast(c_uint, param_types.items.len),
            0, // Not vararg
        );
        
        // Create function
        const llvm_func = llvm.LLVMAddFunction(
            self.module,
            func.name.ptr,
            func_type,
        );
        
        // Set calling convention based on region
        if (func.region == .stack) {
            llvm.LLVMSetFunctionCallConv(llvm_func, llvm.LLVMCCallConv);
        } else if (func.region == .arena) {
            // TODO: Custom calling convention for arena functions
        }
        
        // Store in symbol table
        try self.functions.put(func.name, llvm_func);
    }
    
    fn declareType(self: *LLVMBackend, node: *ast.Node) !void {
        const type_decl = &node.data.type_decl;
        const llvm_type = try self.convertType(type_decl.underlying);
        // TODO: Store type mapping
    }
    
    fn declareStruct(self: *LLVMBackend, node: *ast.Node) !void {
        const struct_decl = &node.data.struct_decl;
        
        // Create struct type
        var field_types = std.ArrayList(llvm.LLVMTypeRef).init(self.allocator);
        defer field_types.deinit();
        
        for (struct_decl.fields) |field| {
            const field_type = field.data.variable_decl.type orelse {
                // TODO: Error handling
                continue;
            };
            
            const llvm_field_type = try self.convertType(field_type);
            try field_types.append(llvm_field_type);
        }
        
        const struct_type = llvm.LLVMStructCreateNamed(
            self.context,
            struct_decl.name.ptr,
        );
        
        if (field_types.items.len > 0) {
            llvm.LLVMStructSetBody(
                struct_type,
                field_types.items.ptr,
                @intCast(c_uint, field_types.items.len),
                0, // Not packed
            );
        }
        
        // Store in type table
        try self.types.put(struct_decl.name, struct_type);
    }
    
    fn declareActor(self: *LLVMBackend, node: *ast.Node) !void {
        // Actors are represented as opaque structs
        const actor = &node.data.actor_decl;
        const actor_type = llvm.LLVMStructCreateNamed(self.context, actor.name.ptr);
        
        // Actor struct contains:
        // 1. Pointer to vtable
        // 2. Actor state
        // 3. Mailbox
        
        const vtable_type = llvm.LLVMPointerType(llvm.LLVMVoidTypeInContext(self.context), 0);
        const state_type = if (actor.state_type) |st|
            try self.convertType(st)
        else
            llvm.LLVMVoidTypeInContext(self.context);
        
        // TODO: Define mailbox type
        
        llvm.LLVMStructSetBody(
            actor_type,
            &[2]llvm.LLVMTypeRef{ vtable_type, state_type },
            2,
            0,
        );
        
        try self.types.put(actor.name, actor_type);
    }
    
    fn generateCode(self: *LLVMBackend, program: *ast.Node) !void {
        switch (program.ty) {
            .program => {
                for (program.data.program.declarations) |decl| {
                    try self.generateNode(decl);
                }
            },
            else => {},
        }
    }
    
    fn generateNode(self: *LLVMBackend, node: *ast.Node) !void {
        switch (node.ty) {
            .function_decl => try self.generateFunction(node),
            .variable_decl => try self.generateVariable(node),
            else => {},
        }
    }
    
    fn generateFunction(self: *LLVMBackend, node: *ast.Node) !void {
        const func = &node.data.function_decl;
        
        if (func.body == null) {
            return; // External function
        }
        
        const llvm_func = self.functions.get(func.name) orelse {
            // Function should have been declared already
            return error.FunctionNotDeclared;
        };
        
        // Create entry basic block
        const entry_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            llvm_func,
            "entry",
        );
        
        llvm.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        
        // Set current function
        self.current_function = llvm_func;
        self.current_basic_block = entry_block;
        
        // Allocate parameters
        var param_idx: c_uint = 0;
        var param = llvm.LLVMGetFirstParam(llvm_func);
        while (param != null) : (param = llvm.LLVMGetNextParam(param)) {
            const param_name = func.params[param_idx].data.identifier.name;
            const alloca = self.buildAlloca(llvm.LLVMTypeOf(param), param_name);
            
            // Store parameter value
            llvm.LLVMBuildStore(self.builder, param, alloca);
            
            // Store in variable table
            try self.variables.put(param_name, alloca);
            
            param_idx += 1;
        }
        
        // Generate function body
        if (func.body) |body| {
            try self.generateStatement(body);
        }
        
        // If function doesn't end with return, add one
        const last_inst = llvm.LLVMGetLastInstruction(entry_block);
        if (last_inst == null or llvm.LLVMGetInstructionOpcode(last_inst) != llvm.LLVMRet) {
            if (func.return_type == null) {
                llvm.LLVMBuildRetVoid(self.builder);
            } else {
                // TODO: Return default value based on type
                const zero = llvm.LLVMConstNull(try self.convertType(func.return_type.?));
                llvm.LLVMBuildRet(self.builder, zero);
            }
        }
        
        // Clear current function
        self.current_function = null;
        self.current_basic_block = null;
    }
    
    fn generateVariable(self: *LLVMBackend, node: *ast.Node) !void {
        const var_decl = &node.data.variable_decl;
        
        if (self.current_function == null) {
            // Global variable
            const var_type = try self.convertType(var_decl.type.?);
            const global = llvm.LLVMAddGlobal(self.module, var_type, var_decl.name.ptr);
            
            if (var_decl.value) |value| {
                const init_val = try self.generateExpression(value);
                llvm.LLVMSetInitializer(global, init_val);
            } else {
                llvm.LLVMSetInitializer(global, llvm.LLVMConstNull(var_type));
            }
            
            // Set linkage based on visibility
            if (var_decl.is_public) {
                llvm.LLVMSetLinkage(global, llvm.LLVMExternalLinkage);
            } else {
                llvm.LLVMSetLinkage(global, llvm.LLVMInternalLinkage);
            }
            
            try self.variables.put(var_decl.name, global);
        } else {
            // Local variable
            const var_type = try self.convertType(var_decl.type.?);
            const alloca = self.buildAlloca(var_type, var_decl.name);
            
            if (var_decl.value) |value| {
                const init_val = try self.generateExpression(value);
                llvm.LLVMBuildStore(self.builder, init_val, alloca);
            }
            
            try self.variables.put(var_decl.name, alloca);
        }
    }
    
    fn generateStatement(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        return switch (node.ty) {
            .block_stmt => self.generateBlock(node),
            .return_stmt => self.generateReturn(node),
            .if_stmt => self.generateIf(node),
            .while_stmt => self.generateWhile(node),
            .expression_stmt => self.generateExpressionStatement(node),
            else => {
                // Default to generating as expression
                return self.generateExpression(node);
            },
        };
    }
    
    fn generateBlock(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const block = &node.data.block_stmt;
        var last_value: llvm.LLVMValueRef = null;
        
        for (block.statements) |stmt| {
            last_value = try self.generateStatement(stmt);
        }
        
        return last_value;
    }
    
    fn generateReturn(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const return_stmt = &node.data.return_stmt;
        
        if (return_stmt.value) |value| {
            const ret_val = try self.generateExpression(value);
            return llvm.LLVMBuildRet(self.builder, ret_val);
        } else {
            return llvm.LLVMBuildRetVoid(self.builder);
        }
    }
    
    fn generateIf(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const if_stmt = &node.data.if_stmt;
        
        const cond_val = try self.generateExpression(if_stmt.condition);
        
        // Create basic blocks
        const then_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "then",
        );
        const else_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "else",
        );
        const merge_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "merge",
        );
        
        // Create conditional branch
        llvm.LLVMBuildCondBr(self.builder, cond_val, then_block, else_block);
        
        // Generate then block
        llvm.LLVMPositionBuilderAtEnd(self.builder, then_block);
        _ = try self.generateStatement(if_stmt.then_branch);
        llvm.LLVMBuildBr(self.builder, merge_block);
        
        // Generate else block if present
        llvm.LLVMPositionBuilderAtEnd(self.builder, else_block);
        if (if_stmt.else_branch) |else_branch| {
            _ = try self.generateStatement(else_branch);
        }
        llvm.LLVMBuildBr(self.builder, merge_block);
        
        // Continue from merge block
        llvm.LLVMPositionBuilderAtEnd(self.builder, merge_block);
        
        return null;
    }
    
    fn generateWhile(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const while_stmt = &node.data.while_stmt;
        
        // Create basic blocks
        const cond_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "while.cond",
        );
        const body_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "while.body",
        );
        const end_block = llvm.LLVMAppendBasicBlockInContext(
            self.context,
            self.current_function.?,
            "while.end",
        );
        
        // Jump to condition block
        llvm.LLVMBuildBr(self.builder, cond_block);
        
        // Generate condition
        llvm.LLVMPositionBuilderAtEnd(self.builder, cond_block);
        const cond_val = try self.generateExpression(while_stmt.condition);
        llvm.LLVMBuildCondBr(self.builder, cond_val, body_block, end_block);
        
        // Generate body
        llvm.LLVMPositionBuilderAtEnd(self.builder, body_block);
        _ = try self.generateStatement(while_stmt.body);
        llvm.LLVMBuildBr(self.builder, cond_block); // Loop back
        
        // Continue from end block
        llvm.LLVMPositionBuilderAtEnd(self.builder, end_block);
        
        return null;
    }
    
    fn generateExpressionStatement(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        // Just evaluate the expression and discard the result
        return self.generateExpression(node);
    }
    
    fn generateExpression(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        return switch (node.ty) {
            .number_literal => self.generateNumberLiteral(node),
            .string_literal => self.generateStringLiteral(node),
            .identifier => self.generateIdentifier(node),
            .binary_expr => self.generateBinaryExpr(node),
            .call_expr => self.generateCallExpr(node),
            else => {
                std.debug.print("Unsupported expression type: {s}\n", .{@tagName(node.ty)});
                return error.UnsupportedExpression;
            },
        };
    }
    
    fn generateNumberLiteral(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const num = &node.data.number_literal;
        const type_ref = try self.convertType(num.type.?);
        
        return switch (num.type.?.kind) {
            .i8, .i16, .i32, .i64 => llvm.LLVMConstInt(
                type_ref,
                @floatToInt(u64, num.value),
                0,
            ),
            .u8, .u16, .u32, .u64 => llvm.LLVMConstInt(
                type_ref,
                @floatToInt(u64, num.value),
                0,
            ),
            .f32, .f64 => llvm.LLVMConstReal(
                type_ref,
                num.value,
            ),
            else => {
                std.debug.print("Unsupported numeric type: {s}\n", .{@tagName(num.type.?.kind)});
                return error.UnsupportedType;
            },
        };
    }
    
    fn generateStringLiteral(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const str = &node.data.string_literal;
        
        // Create global string constant
        const str_type = llvm.LLVMArrayType(
            llvm.LLVMInt8TypeInContext(self.context),
            @intCast(c_uint, str.value.len + 1),
        );
        
        const global_str = llvm.LLVMAddGlobal(self.module, str_type, ".str");
        llvm.LLVMSetLinkage(global_str, llvm.LLVMPrivateLinkage);
        llvm.LLVMSetGlobalConstant(global_str, 1);
        llvm.LLVMSetUnnamedAddr(global_str, 1);
        
        // Initialize with string data
        var init_data = std.ArrayList(u8).init(self.allocator);
        defer init_data.deinit();
        
        try init_data.appendSlice(str.value);
        try init_data.append(0); // Null terminator
        
        const init = llvm.LLVMConstStringInContext(
            self.context,
            init_data.items.ptr,
            @intCast(c_uint, init_data.items.len - 1), // Exclude null terminator
            1, // Don't null terminate (we already did)
        );
        
        llvm.LLVMSetInitializer(global_str, init);
        
        // Get pointer to first element
        const zero = llvm.LLVMConstInt(
            llvm.LLVMInt32TypeInContext(self.context),
            0,
            0,
        );
        
        const indices = [2]llvm.LLVMValueRef{ zero, zero };
        return llvm.LLVMBuildInBoundsGEP(
            self.builder,
            global_str,
            &indices,
            2,
            "",
        );
    }
    
    fn generateIdentifier(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const ident = &node.data.identifier;
        
        if (self.variables.get(ident.name)) |var_ref| {
            // Load variable value
            return llvm.LLVMBuildLoad2(
                self.builder,
                llvm.LLVMTypeOf(var_ref),
                var_ref,
                ident.name.ptr,
            );
        }
        
        std.debug.print("Undefined variable: {s}\n", .{ident.name});
        return error.UndefinedVariable;
    }
    
    fn generateBinaryExpr(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const binary = &node.data.binary_expr;
        
        const left_val = try self.generateExpression(binary.left);
        const right_val = try self.generateExpression(binary.right);
        
        const left_type = llvm.LLVMTypeOf(left_val);
        const right_type = llvm.LLVMTypeOf(right_val);
        
        // Ensure types match
        if (!llvm.LLVMGetTypeKind(left_type) == llvm.LLVMGetTypeKind(right_type)) {
            // TODO: Type conversion
            return error.TypeMismatch;
        }
        
        return switch (binary.op) {
            .add => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind)
                llvm.LLVMBuildAdd(self.builder, left_val, right_val, "addtmp")
            else
                llvm.LLVMBuildFAdd(self.builder, left_val, right_val, "addtmp"),
            .sub => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind)
                llvm.LLVMBuildSub(self.builder, left_val, right_val, "subtmp")
            else
                llvm.LLVMBuildFSub(self.builder, left_val, right_val, "subtmp"),
            .mul => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind)
                llvm.LLVMBuildMul(self.builder, left_val, right_val, "multmp")
            else
                llvm.LLVMBuildFMul(self.builder, left_val, right_val, "multmp"),
            .div => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind) {
                if (llvm.LLVMIsSigned(left_type) == 1)
                    llvm.LLVMBuildSDiv(self.builder, left_val, right_val, "divtmp")
                else
                    llvm.LLVMBuildUDiv(self.builder, left_val, right_val, "divtmp")
            } else
                llvm.LLVMBuildFDiv(self.builder, left_val, right_val, "divtmp"),
            .eq => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind)
                llvm.LLVMBuildICmp(self.builder, llvm.LLVMIntEQ, left_val, right_val, "eqtmp")
            else
                llvm.LLVMBuildFCmp(self.builder, llvm.LLVMRealOEQ, left_val, right_val, "eqtmp"),
            .lt => if (llvm.LLVMGetTypeKind(left_type) == llvm.LLVMIntegerTypeKind) {
                if (llvm.LLVMIsSigned(left_type) == 1)
                    llvm.LLVMBuildICmp(self.builder, llvm.LLVMIntSLT, left_val, right_val, "lttmp")
                else
                    llvm.LLVMBuildICmp(self.builder, llvm.LLVMIntULT, left_val, right_val, "lttmp")
            } else
                llvm.LLVMBuildFCmp(self.builder, llvm.LLVMRealOLT, left_val, right_val, "lttmp"),
            .assign => {
                // Left must be an identifier (lvalue)
                if (binary.left.ty != .identifier) {
                    return error.NotAssignable;
                }
                
                const ident = &binary.left.data.identifier;
                const var_ref = self.variables.get(ident.name) orelse {
                    return error.UndefinedVariable;
                };
                
                _ = llvm.LLVMBuildStore(self.builder, right_val, var_ref);
                return right_val;
            },
            else => {
                std.debug.print("Unsupported binary operator: {s}\n", .{@tagName(binary.op)});
                return error.UnsupportedOperator;
            },
        };
    }
    
    fn generateCallExpr(self: *LLVMBackend, node: *ast.Node) !llvm.LLVMValueRef {
        const call = &node.data.call_expr;
        
        // Get function
        const func_name = call.callee.data.identifier.name;
        const llvm_func = self.functions.get(func_name) orelse {
            std.debug.print("Undefined function: {s}\n", .{func_name});
            return error.UndefinedFunction;
        };
        
        // Generate arguments
        var args = std.ArrayList(llvm.LLVMValueRef).init(self.allocator);
        defer args.deinit();
        
        for (call.args) |arg| {
            const arg_val = try self.generateExpression(arg);
            try args.append(arg_val);
        }
        
        // Build call
        return llvm.LLVMBuildCall2(
            self.builder,
            llvm.LLVMGetElementType(llvm.LLVMTypeOf(llvm_func)), // Function type
            llvm_func,
            if (args.items.len > 0) args.items.ptr else null,
            @intCast(c_uint, args.items.len),
            "calltmp",
        );
    }
    
    fn buildAlloca(self: *LLVMBackend, ty: llvm.LLVMTypeRef, name: []const u8) llvm.LLVMValueRef {
        // Save current insertion point
        const current_block = llvm.LLVMGetInsertBlock(self.builder);
        const current_func = llvm.LLVMGetBasicBlockParent(current_block);
        
        // Create alloca at function entry
        const entry_block = llvm.LLVMGetEntryBasicBlock(current_func);
        const first_inst = llvm.LLVMGetFirstInstruction(entry_block);
        
        if (first_inst != null) {
            llvm.LLVMPositionBuilderBefore(self.builder, first_inst);
        } else {
            llvm.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }
        
        const alloca = llvm.LLVMBuildAlloca(self.builder, ty, name.ptr);
        
        // Restore insertion point
        llvm.LLVMPositionBuilderAtEnd(self.builder, current_block);
        
        return alloca;
    }
    
    fn convertType(self: *LLVMBackend, t: *ast.Type) !llvm.LLVMTypeRef {
        return switch (t.kind) {
            .void => llvm.LLVMVoidTypeInContext(self.context),
            .bool => llvm.LLVMInt1TypeInContext(self.context),
            .i8 => llvm.LLVMInt8TypeInContext(self.context),
            .i16 => llvm.LLVMInt16TypeInContext(self.context),
            .i32 => llvm.LLVMInt32TypeInContext(self.context),
            .i64 => llvm.LLVMInt64TypeInContext(self.context),
            .u8 => llvm.LLVMInt8TypeInContext(self.context),
            .u16 => llvm.LLVMInt16TypeInContext(self.context),
            .u32 => llvm.LLVMInt32TypeInContext(self.context),
            .u64 => llvm.LLVMInt64TypeInContext(self.context),
            .f32 => llvm.LLVMFloatTypeInContext(self.context),
            .f64 => llvm.LLVMDoubleTypeInContext(self.context),
            .f128 => llvm.LLVMFP128TypeInContext(self.context),
            .pointer => {
                const element_type = t.element_type orelse {
                    return error.InvalidPointerType;
                };
                const pointee_type = try self.convertType(element_type);
                return llvm.LLVMPointerType(pointee_type, 0);
            },
            .array => {
                const element_type = t.element_type orelse {
                    return error.InvalidArrayType;
                };
                const elem_type = try self.convertType(element_type);
                const length = t.length orelse {
                    return error.UnknownArrayLength;
                };
                return llvm.LLVMArrayType(elem_type, @intCast(c_uint, length));
            },
            .struct_type => {
                if (t.name) |name| {
                    if (self.types.get(name)) |struct_type| {
                        return struct_type;
                    }
                }
                
                // Create new struct type
                if (t.fields) |fields| {
                    var field_types = std.ArrayList(llvm.LLVMTypeRef).init(self.allocator);
                    defer field_types.deinit();
                    
                    for (fields) |field| {
                        const field_type = field.type;
                        const llvm_field_type = try self.convertType(field_type);
                        try field_types.append(llvm_field_type);
                    }
                    
                    const struct_type = llvm.LLVMStructCreateNamed(
                        self.context,
                        t.name orelse "",
                    );
                    
                    if (field_types.items.len > 0) {
                        llvm.LLVMStructSetBody(
                            struct_type,
                            field_types.items.ptr,
                            @intCast(c_uint, field_types.items.len),
                            0,
                        );
                    }
                    
                    if (t.name) |name| {
                        try self.types.put(name, struct_type);
                    }
                    
                    return struct_type;
                }
                
                // Empty struct
                return llvm.LLVMStructCreateNamed(self.context, t.name orelse "");
            },
            else => {
                std.debug.print("Unsupported type kind: {s}\n", .{@tagName(t.kind)});
                return error.UnsupportedType;
            },
        };
    }
    
    pub fn optimize(self: *LLVMBackend, level: OptimizationLevel) !void {
        const pass_builder = llvm.LLVMCreatePassBuilderOptions();
        defer llvm.LLVMDisposePassBuilderOptions(pass_builder);
        
        // Set optimization level
        llvm.LLVMPassBuilderOptionsSetOptLevel(pass_builder, @enumToInt(level));
        llvm.LLVMPassBuilderOptionsSetSizeLevel(pass_builder, 0);
        
        // Run optimization passes
        const pass_manager = llvm.LLVMCreateFunctionPassManagerForModule(self.module);
        defer llvm.LLVMDisposePassManager(pass_manager);
        
        // Add standard optimization passes
        llvm.LLVMAddInstructionCombiningPass(pass_manager);
        llvm.LLVMAddReassociatePass(pass_manager);
        llvm.LLVMAddGVNPass(pass_manager);
        llvm.LLVMAddCFGSimplificationPass(pass_manager);
        
        if (level != .none) {
            llvm.LLVMAddLICMPass(pass_manager);
            llvm.LLVMAddLoopUnrollPass(pass_manager);
            
            if (level == .aggressive) {
                llvm.LLVMAddAggressiveInstCombinerPass(pass_manager);
                llvm.LLVMAddDeadStoreEliminationPass(pass_manager);
            }
        }
        
        // Initialize and run passes
        _ = llvm.LLVMInitializeFunctionPassManager(pass_manager);
        
        var func = llvm.LLVMGetFirstFunction(self.module);
        while (func != null) {
            _ = llvm.LLVMRunFunctionPassManager(pass_manager, func);
            func = llvm.LLVMGetNextFunction(func);
        }
        
        _ = llvm.LLVMFinalizeFunctionPassManager(pass_manager);
    }
    
    pub fn writeObjectFile(self: *LLVMBackend, output_path: []const u8) !void {
        var error_msg: [*c]u8 = null;
        
        if (llvm.LLVMTargetMachineEmitToFile(
            self.target_machine,
            self.module,
            output_path.ptr,
            llvm.LLVMObjectFile,
            &error_msg,
        ) != 0) {
            defer llvm.LLVMDisposeMessage(error_msg);
            std.debug.print("Failed to write object file: {s}\n", .{error_msg});
            return error.ObjectFileWriteFailed;
        }
    }
    
    pub fn writeBitcodeFile(self: *LLVMBackend, output_path: []const u8) !void {
        if (llvm.LLVMWriteBitcodeToFile(self.module, output_path.ptr) != 0) {
            return error.BitcodeWriteFailed;
        }
    }
    
    pub fn writeAssemblyFile(self: *LLVMBackend, output_path: []const u8) !void {
        var error_msg: [*c]u8 = null;
        
        if (llvm.LLVMTargetMachineEmitToFile(
            self.target_machine,
            self.module,
            output_path.ptr,
            llvm.LLVMAssemblyFile,
            &error_msg,
        ) != 0) {
            defer llvm.LLVMDisposeMessage(error_msg);
            std.debug.print("Failed to write assembly file: {s}\n", .{error_msg});
            return error.AssemblyWriteFailed;
        }
    }
    
    pub fn getModuleIR(self: *LLVMBackend) ![]const u8 {
        var ir_str: [*c]u8 = null;
        llvm.LLVMPrintModuleToString(self.module, &ir_str);
        defer llvm.LLVMDisposeMessage(ir_str);
        
        return std.mem.span(ir_str);
    }
    
    pub fn dumpModule(self: *LLVMBackend) void {
        var ir_str: [*c]u8 = null;
        llvm.LLVMPrintModuleToString(self.module, &ir_str);
        defer llvm.LLVMDisposeMessage(ir_str);
        
        std.debug.print("{s}\n", .{ir_str});
    }
};

pub const OptimizationLevel = enum(c_uint) {
    none = 0,
    less = 1,
    default = 2,
    aggressive = 3,
};
