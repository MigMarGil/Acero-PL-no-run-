const std = @import("std");
const ast = @import("../ast.zig");

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),
    warnings: std.ArrayList(Warning),
    current_scope: *ast.Symbol.Scope,
    global_scope: *ast.Symbol.Scope,
    current_function: ?*ast.Node = null,
    current_region: ast.Region = .stack,
    
    pub const Error = struct {
        message: []const u8,
        loc: ast.Node.Loc,
        severity: Severity = .error,
        
        pub const Severity = enum {
            error,
            warning,
            info,
        };
    };
    
    pub const Warning = struct {
        message: []const u8,
        loc: ast.Node.Loc,
    };
    
    pub fn init(allocator: std.mem.Allocator) !SemanticAnalyzer {
        const global_scope = try allocator.create(ast.Symbol.Scope);
        global_scope.* = ast.Symbol.Scope.init(allocator, null, .global);
        
        return SemanticAnalyzer{
            .allocator = allocator,
            .errors = std.ArrayList(Error).init(allocator),
            .warnings = std.ArrayList(Warning).init(allocator),
            .current_scope = global_scope,
            .global_scope = global_scope,
        };
    }
    
    pub fn deinit(self: *SemanticAnalyzer) void {
        self.errors.deinit();
        self.warnings.deinit();
        // TODO: Clean up scopes
    }
    
    pub fn analyze(self: *SemanticAnalyzer, program: *ast.Node) !void {
        try self.visitProgram(program);
        
        if (self.errors.items.len > 0) {
            return error.SemanticError;
        }
    }
    
    fn visitProgram(self: *SemanticAnalyzer, node: *ast.Node) !void {
        switch (node.ty) {
            .program => {
                // First pass: collect all declarations
                for (node.data.program.declarations) |decl| {
                    try self.collectDeclaration(decl);
                }
                
                // Second pass: analyze bodies
                for (node.data.program.declarations) |decl| {
                    try self.visitDeclaration(decl);
                }
            },
            else => {
                try self.error(node.loc, "Expected program node");
            },
        }
    }
    
    fn collectDeclaration(self: *SemanticAnalyzer, node: *ast.Node) !void {
        switch (node.ty) {
            .function_decl => try self.collectFunction(node),
            .actor_decl => try self.collectActor(node),
            .type_decl => try self.collectType(node),
            .struct_decl => try self.collectStruct(node),
            .variable_decl => try self.collectVariable(node),
            .import_decl => {}, // Ignored for now
            else => {
                try self.error(node.loc, "Unsupported declaration type");
            },
        }
    }
    
    fn collectFunction(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const func = &node.data.function_decl;
        
        // Create function type
        const func_type = try self.allocator.create(ast.Type);
        func_type.* = .{
            .kind = .function_type,
            .size = 8, // Pointer size
            .align = 8,
            .name = func.name,
            .region = func.region,
            .return_type = func.return_type,
            .params = null, // TODO: Set parameter types
        };
        
        // Create symbol
        const symbol = try self.allocator.create(ast.Symbol);
        symbol.* = .{
            .name = func.name,
            .type = func_type,
            .node = node,
            .scope = self.current_scope,
            .is_public = func.is_public,
            .is_mutable = false,
            .is_extern = func.is_extern,
        };
        
        // Add to current scope
        try self.current_scope.symbols.put(func.name, symbol);
    }
    
    fn collectActor(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const actor = &node.data.actor_decl;
        
        // Create actor type
        const actor_type = try self.allocator.create(ast.Type);
        actor_type.* = .{
            .kind = .actor,
            .size = 16, // Actor handle size
            .align = 8,
            .name = actor.name,
            .region = actor.region,
        };
        
        // Create symbol
        const symbol = try self.allocator.create(ast.Symbol);
        symbol.* = .{
            .name = actor.name,
            .type = actor_type,
            .node = node,
            .scope = self.current_scope,
            .is_public = actor.is_public,
            .is_mutable = false,
            .is_extern = false,
        };
        
        try self.current_scope.symbols.put(actor.name, symbol);
    }
    
    fn collectType(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const type_decl = &node.data.type_decl;
        
        // Create symbol for the type
        const symbol = try self.allocator.create(ast.Symbol);
        symbol.* = .{
            .name = type_decl.name,
            .type = type_decl.underlying,
            .node = node,
            .scope = self.current_scope,
            .is_public = type_decl.is_public,
            .is_mutable = false,
            .is_extern = false,
        };
        
        try self.current_scope.symbols.put(type_decl.name, symbol);
    }
    
    fn collectStruct(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const struct_decl = &node.data.struct_decl;
        
        // Create struct type
        const struct_type = try self.allocator.create(ast.Type);
        struct_type.* = .{
            .kind = .struct_type,
            .size = 0, // Calculated later
            .align = 1,
            .name = struct_decl.name,
            .region = struct_decl.region,
            .fields = null, // Will be set during analysis
        };
        
        // Create symbol
        const symbol = try self.allocator.create(ast.Symbol);
        symbol.* = .{
            .name = struct_decl.name,
            .type = struct_type,
            .node = node,
            .scope = self.current_scope,
            .is_public = struct_decl.is_public,
            .is_mutable = false,
            .is_extern = false,
        };
        
        try self.current_scope.symbols.put(struct_decl.name, symbol);
    }
    
    fn collectVariable(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const var_decl = &node.data.variable_decl;
        
        if (var_decl.type == null and var_decl.value == null) {
            try self.error(node.loc, "Variable declaration must have type or initial value");
            return;
        }
        
        // Create symbol
        const symbol = try self.allocator.create(ast.Symbol);
        symbol.* = .{
            .name = var_decl.name,
            .type = var_decl.type, // May be null initially
            .node = node,
            .scope = self.current_scope,
            .is_public = var_decl.is_public,
            .is_mutable = var_decl.is_mutable,
            .is_extern = false,
        };
        
        try self.current_scope.symbols.put(var_decl.name, symbol);
    }
    
    fn visitDeclaration(self: *SemanticAnalyzer, node: *ast.Node) !void {
        switch (node.ty) {
            .function_decl => try self.visitFunction(node),
            .actor_decl => try self.visitActor(node),
            .type_decl => try self.visitType(node),
            .struct_decl => try self.visitStruct(node),
            .variable_decl => try self.visitVariable(node),
            else => {},
        }
    }
    
    fn visitFunction(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const func = &node.data.function_decl;
        const previous_function = self.current_function;
        const previous_scope = self.current_scope;
        const previous_region = self.current_region;
        
        self.current_function = node;
        self.current_region = func.region;
        
        // Create function scope
        const function_scope = try self.allocator.create(ast.Symbol.Scope);
        function_scope.* = ast.Symbol.Scope.init(self.allocator, previous_scope, func.region);
        self.current_scope = function_scope;
        
        // Add parameters to scope
        for (func.params) |param| {
            try self.visitParameter(param);
        }
        
        // Analyze function body
        if (func.body) |body| {
            try self.visitNode(body);
        }
        
        // Restore previous state
        self.current_scope = previous_scope;
        self.current_function = previous_function;
        self.current_region = previous_region;
    }
    
    fn visitActor(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const actor = &node.data.actor_decl;
        const previous_region = self.current_region;
        
        self.current_region = actor.region;
        
        // Analyze message types
        for (actor.messages) |message| {
            try self.visitDeclaration(message);
        }
        
        // Analyze handler functions
        for (actor.handlers) |handler| {
            try self.visitFunction(handler);
        }
        
        self.current_region = previous_region;
    }
    
    fn visitType(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const type_decl = &node.data.type_decl;
        
        // Analyze methods
        for (type_decl.methods) |method| {
            try self.visitFunction(method);
        }
    }
    
    fn visitStruct(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const struct_decl = &node.data.struct_decl;
        const previous_region = self.current_region;
        
        self.current_region = struct_decl.region;
        
        // Analyze fields
        var total_size: usize = 0;
        var max_align: usize = 1;
        
        for (struct_decl.fields) |field| {
            try self.visitField(field);
            
            if (field.data.variable_decl.type) |field_type| {
                // Update struct alignment
                max_align = @max(max_align, field_type.align);
                
                // Align current offset
                total_size = std.mem.alignForward(total_size, field_type.align);
                
                // TODO: Store field offset
                total_size += field_type.size;
            }
        }
        
        // Final alignment
        total_size = std.mem.alignForward(total_size, max_align);
        
        // Update struct type info
        if (self.current_scope.lookup(struct_decl.name)) |symbol| {
            if (symbol.type.kind == .struct_type) {
                symbol.type.size = total_size;
                symbol.type.align = max_align;
            }
        }
        
        // Analyze methods
        for (struct_decl.methods) |method| {
            try self.visitFunction(method);
        }
        
        self.current_region = previous_region;
    }
    
    fn visitVariable(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const var_decl = &node.data.variable_decl;
        
        // Check region compatibility
        if (!self.isRegionCompatible(self.current_region, var_decl.region)) {
            try self.error(node.loc, "Variable region incompatible with current region");
        }
        
        // Analyze initial value
        if (var_decl.value) |value| {
            try self.visitNode(value);
            
            // Infer type from value if not specified
            if (var_decl.type == null) {
                const value_type = try self.inferType(value);
                var_decl.type = value_type;
                
                // Update symbol
                if (self.current_scope.lookup(var_decl.name)) |symbol| {
                    symbol.type = value_type;
                }
            } else {
                // Check type compatibility
                const value_type = try self.inferType(value);
                if (!self.typesCompatible(var_decl.type.?, value_type)) {
                    try self.error(node.loc, "Type mismatch in variable initialization");
                }
            }
        }
        
        // Check that variable has a type
        if (var_decl.type == null) {
            try self.error(node.loc, "Cannot infer variable type");
        }
    }
    
    fn visitParameter(self: *SemanticAnalyzer, node: *ast.Node) !void {
        // TODO: Implement parameter analysis
        _ = node;
    }
    
    fn visitField(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const var_decl = &node.data.variable_decl;
        
        // Check field type
        if (var_decl.type == null) {
            try self.error(node.loc, "Field must have explicit type");
            return;
        }
        
        // Check region compatibility
        if (!self.isRegionCompatible(self.current_region, var_decl.region)) {
            try self.error(node.loc, "Field region incompatible with struct region");
        }
        
        // Analyze default value
        if (var_decl.value) |value| {
            try self.visitNode(value);
            
            const value_type = try self.inferType(value);
            if (!self.typesCompatible(var_decl.type.?, value_type)) {
                try self.error(node.loc, "Type mismatch in field default value");
            }
        }
    }
    
    fn visitNode(self: *SemanticAnalyzer, node: *ast.Node) !void {
        switch (node.ty) {
            .block_stmt => try self.visitBlock(node),
            .if_stmt => try self.visitIf(node),
            .while_stmt => try self.visitWhile(node),
            .for_stmt => try self.visitFor(node),
            .return_stmt => try self.visitReturn(node),
            .call_expr => try self.visitCall(node),
            .binary_expr => try self.visitBinaryExpr(node),
            .unary_expr => try self.visitUnaryExpr(node),
            .identifier => try self.visitIdentifier(node),
            .number_literal => try self.visitNumberLiteral(node),
            .string_literal => try self.visitStringLiteral(node),
            .spawn_expr => try self.visitSpawn(node),
            .send_expr => try self.visitSend(node),
            else => {
                // Visit children
                if (node.children) |children| {
                    for (children) |child| {
                        try self.visitNode(child);
                    }
                }
            },
        }
    }
    
    fn visitBlock(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const block = &node.data.block_stmt;
        const previous_region = self.current_region;
        const previous_scope = self.current_scope;
        
        self.current_region = block.region;
        
        // Create block scope
        const block_scope = try self.allocator.create(ast.Symbol.Scope);
        block_scope.* = ast.Symbol.Scope.init(self.allocator, previous_scope, block.region);
        self.current_scope = block_scope;
        
        // Visit statements
        for (block.statements) |stmt| {
            try self.visitNode(stmt);
        }
        
        self.current_scope = previous_scope;
        self.current_region = previous_region;
    }
    
    fn visitIf(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const if_stmt = &node.data.if_stmt;
        
        // Check condition type
        try self.visitNode(if_stmt.condition);
        const cond_type = try self.inferType(if_stmt.condition);
        
        if (cond_type.kind != .bool) {
            try self.error(if_stmt.condition.loc, "If condition must be boolean");
        }
        
        // Visit then branch
        try self.visitNode(if_stmt.then_branch);
        
        // Visit else branch if present
        if (if_stmt.else_branch) |else_branch| {
            try self.visitNode(else_branch);
        }
    }
    
    fn visitWhile(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const while_stmt = &node.data.while_stmt;
        
        // Check condition type
        try self.visitNode(while_stmt.condition);
        const cond_type = try self.inferType(while_stmt.condition);
        
        if (cond_type.kind != .bool) {
            try self.error(while_stmt.condition.loc, "While condition must be boolean");
        }
        
        // Visit body
        try self.visitNode(while_stmt.body);
    }
    
    fn visitFor(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const for_stmt = &node.data.for_stmt;
        
        // Visit iterator declaration
        try self.visitNode(for_stmt.iterator);
        
        // Visit iterable
        try self.visitNode(for_stmt.iterable);
        const iterable_type = try self.inferType(for_stmt.iterable);
        
        // Check if iterable is iterable
        if (iterable_type.kind != .array and iterable_type.kind != .slice) {
            try self.warning(for_stmt.iterable.loc, "For loop expects array or slice");
        }
        
        // Visit body
        try self.visitNode(for_stmt.body);
    }
    
    fn visitReturn(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const return_stmt = &node.data.return_stmt;
        
        if (self.current_function == null) {
            try self.error(node.loc, "Return statement outside function");
            return;
        }
        
        const func = &self.current_function.?.data.function_decl;
        
        if (return_stmt.value) |value| {
            try self.visitNode(value);
            const value_type = try self.inferType(value);
            
            if (func.return_type) |return_type| {
                if (!self.typesCompatible(return_type, value_type)) {
                    try self.error(value.loc, "Return type mismatch");
                }
            } else {
                try self.error(node.loc, "Function should not return a value");
            }
        } else {
            if (func.return_type != null) {
                try self.error(node.loc, "Function must return a value");
            }
        }
    }
    
    fn visitCall(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const call = &node.data.call_expr;
        
        // Visit callee
        try self.visitNode(call.callee);
        const callee_type = try self.inferType(call.callee);
        
        if (callee_type.kind != .function_type) {
            try self.error(call.callee.loc, "Can only call functions");
            return;
        }
        
        // Check argument count
        // TODO: Check against function signature
        
        // Visit arguments
        for (call.args) |arg| {
            try self.visitNode(arg);
        }
        
        // Set node type to function return type
        if (node.data.identifier.symbol == null) {
            node.data.identifier.symbol = try self.resolveSymbol(call.callee.data.identifier.name);
        }
    }
    
    fn visitBinaryExpr(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const binary = &node.data.binary_expr;
        
        try self.visitNode(binary.left);
        try self.visitNode(binary.right);
        
        const left_type = try self.inferType(binary.left);
        const right_type = try self.inferType(binary.right);
        
        // Check type compatibility based on operator
        switch (binary.op) {
            .add, .sub, .mul, .div, .mod => {
                if (!self.isNumericType(left_type) or !self.isNumericType(right_type)) {
                    try self.error(node.loc, "Arithmetic operation requires numeric types");
                }
                
                if (!self.typesCompatible(left_type, right_type)) {
                    try self.warning(node.loc, "Implicit type conversion in arithmetic");
                }
            },
            .eq, .ne, .lt, .le, .gt, .ge => {
                if (!self.typesCompatible(left_type, right_type)) {
                    try self.error(node.loc, "Comparison requires compatible types");
                }
            },
            .and, .or => {
                if (left_type.kind != .bool or right_type.kind != .bool) {
                    try self.error(node.loc, "Logical operators require boolean types");
                }
            },
            .assign => {
                if (!self.typesCompatible(left_type, right_type)) {
                    try self.error(node.loc, "Type mismatch in assignment");
                }
                
                // Check if left is mutable
                if (binary.left.ty == .identifier) {
                    if (self.current_scope.lookup(binary.left.data.identifier.name)) |symbol| {
                        if (!symbol.is_mutable) {
                            try self.error(binary.left.loc, "Cannot assign to immutable variable");
                        }
                    }
                }
            },
            else => {
                // TODO: Handle other operators
            },
        }
    }
    
    fn visitUnaryExpr(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const unary = &node.data.unary_expr;
        
        try self.visitNode(unary.operand);
        const operand_type = try self.inferType(unary.operand);
        
        switch (unary.op) {
            .neg => {
                if (!self.isNumericType(operand_type)) {
                    try self.error(node.loc, "Negation requires numeric type");
                }
            },
            .not => {
                if (operand_type.kind != .bool) {
                    try self.error(node.loc, "Logical not requires boolean type");
                }
            },
            .bit_not => {
                if (!self.isIntegerType(operand_type)) {
                    try self.error(node.loc, "Bitwise not requires integer type");
                }
            },
            .deref => {
                if (operand_type.kind != .pointer) {
                    try self.error(node.loc, "Dereference requires pointer type");
                }
            },
            .addr_of, .ref, .mut_ref => {
                // These create pointers/references
            },
        }
    }
    
    fn visitIdentifier(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const ident = &node.data.identifier;
        
        if (self.current_scope.lookup(ident.name)) |symbol| {
            ident.symbol = symbol;
            ident.type = symbol.type;
        } else {
            try self.error(node.loc, std.fmt.allocPrint(
                self.allocator,
                "Undefined identifier '{s}'",
                .{ident.name}
            ) catch "Undefined identifier");
        }
    }
    
    fn visitNumberLiteral(self: *SemanticAnalyzer, node: *ast.Node) !void {
        _ = node;
        // Number literals already have their type set by the parser
    }
    
    fn visitStringLiteral(self: *SemanticAnalyzer, node: *ast.Node) !void {
        _ = node;
        // String literals already have their type set by the parser
    }
    
    fn visitSpawn(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const spawn = &node.data.spawn_expr;
        
        // Check actor type
        if (spawn.actor_type.kind != .actor) {
            try self.error(node.loc, "Spawn requires actor type");
        }
        
        // Check region compatibility
        if (!self.isRegionCompatible(self.current_region, spawn.region)) {
            try self.error(node.loc, "Spawn region incompatible with current region");
        }
        
        // Visit arguments
        for (spawn.args) |arg| {
            try self.visitNode(arg);
        }
    }
    
    fn visitSend(self: *SemanticAnalyzer, node: *ast.Node) !void {
        const send = &node.data.send_expr;
        
        // Visit actor and message
        try self.visitNode(send.actor);
        try self.visitNode(send.message);
        
        const actor_type = try self.inferType(send.actor);
        if (actor_type.kind != .actor) {
            try self.error(send.actor.loc, "Send requires actor handle");
        }
        
        // Check region
        if (send.region != .message) {
            try self.warning(node.loc, "Send should use message region");
        }
    }
    
    // Type inference and checking
    fn inferType(self: *SemanticAnalyzer, node: *ast.Node) !*ast.Type {
        return switch (node.ty) {
            .identifier => node.data.identifier.type orelse {
                try self.error(node.loc, "Cannot infer type of identifier");
                return error.TypeError;
            },
            .number_literal => node.data.number_literal.type orelse {
                try self.error(node.loc, "Number literal has no type");
                return error.TypeError;
            },
            .string_literal => node.data.string_literal.type,
            .call_expr => {
                if (node.data.call_expr.callee.data.identifier.symbol) |symbol| {
                    if (symbol.type.return_type) |return_type| {
                        return return_type;
                    }
                }
                try self.error(node.loc, "Cannot infer return type of function call");
                return error.TypeError;
            },
            .binary_expr => {
                const binary = &node.data.binary_expr;
                const left_type = try self.inferType(binary.left);
                
                return switch (binary.op) {
                    .add, .sub, .mul, .div, .mod => left_type,
                    .eq, .ne, .lt, .le, .gt, .ge,
                    .and, .or => try self.createBasicType(.bool, 1, 1),
                    .assign => try self.inferType(binary.left),
                    else => left_type,
                };
            },
            .unary_expr => {
                const unary = &node.data.unary_expr;
                const operand_type = try self.inferType(unary.operand);
                
                return switch (unary.op) {
                    .neg, .bit_not => operand_type,
                    .not => try self.createBasicType(.bool, 1, 1),
                    .deref => operand_type.element_type orelse {
                        try self.error(node.loc, "Cannot dereference non-pointer type");
                        return error.TypeError;
                    },
                    .addr_of, .ref, .mut_ref => {
                        const ptr_type = try self.createBasicType(.pointer, 8, 8);
                        ptr_type.element_type = operand_type;
                        return ptr_type;
                    },
                };
            },
            else => {
                try self.error(node.loc, "Cannot infer type");
                return error.TypeError;
            },
        };
    }
    
    fn typesCompatible(self: *SemanticAnalyzer, t1: *ast.Type, t2: *ast.Type) bool {
        // Same type
        if (t1.kind == t2.kind and t1.size == t2.size) {
            return true;
        }
        
        // Numeric conversions
        if (self.isNumericType(t1) and self.isNumericType(t2)) {
            return true;
        }
        
        // Pointer conversions
        if (t1.kind == .pointer and t2.kind == .pointer) {
            if (t1.element_type == null or t2.element_type == null) {
                return true; // void pointers
            }
            return self.typesCompatible(t1.element_type.?, t2.element_type.?);
        }
        
        // Array to slice decay
        if (t1.kind == .array and t2.kind == .slice) {
            if (t1.element_type) |elem1| {
                if (t2.element_type) |elem2| {
                    return self.typesCompatible(elem1, elem2);
                }
            }
        }
        
        return false;
    }
    
    fn isNumericType(self: *SemanticAnalyzer, t: *ast.Type) bool {
        return switch (t.kind) {
            .i8, .i16, .i32, .i64,
            .u8, .u16, .u32, .u64,
            .f16, .f32, .f64, .f128 => true,
            else => false,
        };
    }
    
    fn isIntegerType(self: *SemanticAnalyzer, t: *ast.Type) bool {
        return switch (t.kind) {
            .i8, .i16, .i32, .i64,
            .u8, .u16, .u32, .u64 => true,
            else => false,
        };
    }
    
    fn isRegionCompatible(self: *SemanticAnalyzer, source: ast.Region, target: ast.Region) bool {
        // Region compatibility matrix
        const matrix = [6][6]bool{
            // to: stack, arena, heap, global, thread, message
            [_]bool{ true,  true,  true,  true,  true,  true  }, // from: stack
            [_]bool{ false, true,  true,  true,  true,  true  }, // from: arena
            [_]bool{ false, false, true,  true,  false, false }, // from: heap
            [_]bool{ false, false, false, true,  false, false }, // from: global
            [_]bool{ true,  true,  true,  true,  true,  true  }, // from: thread
            [_]bool{ true,  true,  true,  true,  true,  true  }, // from: message
        };
        
        return matrix[@intFromEnum(source)][@intFromEnum(target)];
    }
    
    fn resolveSymbol(self: *SemanticAnalyzer, name: []const u8) !*ast.Symbol {
        if (self.current_scope.lookup(name)) |symbol| {
            return symbol;
        }
        
        // Try global scope
        if (self.global_scope.lookup(name)) |symbol| {
            return symbol;
        }
        
        return error.SymbolNotFound;
    }
    
    fn createBasicType(self: *SemanticAnalyzer, kind: ast.TypeKind, size: usize, align: usize) !*ast.Type {
        const type_node = try self.allocator.create(ast.Type);
        type_node.* = .{
            .kind = kind,
            .size = size,
            .align = align,
            .name = null,
            .region = self.current_region,
            .fields = null,
            .methods = null,
            .params = null,
            .return_type = null,
            .element_type = null,
            .length = null,
        };
        return type_node;
    }
    
    fn error(self: *SemanticAnalyzer, loc: ast.Node.Loc, message: []const u8) !void {
        try self.errors.append(.{
            .message = message,
            .loc = loc,
        });
    }
    
    fn warning(self: *SemanticAnalyzer, loc: ast.Node.Loc, message: []const u8) !void {
        try self.warnings.append(.{
            .message = message,
            .loc = loc,
        });
    }
};
