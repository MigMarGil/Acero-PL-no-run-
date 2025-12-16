const std = @import("std");
const ast = @import("../ast.zig");
const tokens = @import("tokens.zig");
const Token = tokens.Token;
const TokenStream = tokens.TokenStream;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    stream: TokenStream,
    errors: std.ArrayList(Error),
    current_file: []const u8,
    
    pub const Error = struct {
        message: []const u8,
        loc: Token.Loc,
        
        pub fn format(self: Error, writer: anytype) !void {
            try writer.print("{}: {s}", .{ self.loc, self.message });
        }
    };
    
    pub fn init(allocator: std.mem.Allocator, token_stream: TokenStream, file: []const u8) Parser {
        return .{
            .allocator = allocator,
            .stream = token_stream,
            .errors = std.ArrayList(Error).init(allocator),
            .current_file = file,
        };
    }
    
    pub fn parse(self: *Parser) !*ast.Node {
        var imports = std.ArrayList(*ast.Node).init(self.allocator);
        var declarations = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.stream.isAtEnd()) {
            if (self.match(.kw_import)) {
                const import_node = try self.parseImport();
                try imports.append(import_node);
            } else if (self.match(.kw_pub)) {
                const decl = try self.parseDeclaration(true);
                if (decl) |d| try declarations.append(d);
            } else {
                const decl = try self.parseDeclaration(false);
                if (decl) |d| try declarations.append(d);
            }
        }
        
        const program = try ast.Node.create(self.allocator, .program, .{
            .file = self.current_file,
            .start = 0,
            .end = self.stream.peek().loc.end,
            .line = 1,
            .column = 1,
        });
        
        program.data = .{
            .program = .{
                .modules = declarations.toOwnedSlice(),
                .imports = imports.toOwnedSlice(),
                .exports = &.{}, // Se llena durante análisis semántico
            },
        };
        
        return program;
    }
    
    fn parseImport(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        const path = try self.expect(.string);
        var alias: ?[]const u8 = null;
        
        if (self.match(.kw_as)) {
            const alias_token = try self.expect(.identifier);
            alias = alias_token.lexeme;
        }
        
        try self.expect(.semicolon);
        
        const import_node = try ast.Node.create(self.allocator, .import_decl, start);
        import_node.data = .{
            .import_decl = .{
                .path = path.lexeme[1..path.lexeme.len-1], // Remove quotes
                .alias = alias,
            },
        };
        
        return import_node;
    }
    
    fn parseDeclaration(self: *Parser, is_public: bool) !?*ast.Node {
        return if (self.match(.kw_fn))
            try self.parseFunction(is_public)
        else if (self.match(.kw_actor))
            try self.parseActor(is_public)
        else if (self.match(.kw_type))
            try self.parseTypeDeclaration(is_public)
        else if (self.match(.kw_struct))
            try self.parseStruct(is_public)
        else if (self.match(.kw_enum))
            try self.parseEnum(is_public)
        else if (self.match(.kw_union))
            try self.parseUnion(is_public)
        else if (self.match(.kw_let) or self.match(.kw_const) or
                 self.match(.kw_global) or self.match(.kw_static))
            try self.parseVariableDeclaration(is_public)
        else {
            try self.errorAtCurrent("Expected declaration");
            self.synchronize();
            return null;
        };
    }
    
    fn parseFunction(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        // Parse type parameters
        var type_params = std.ArrayList([]const u8).init(self.allocator);
        if (self.match(.lt)) {
            while (!self.check(.gt) and !self.stream.isAtEnd()) {
                const param = try self.expect(.identifier);
                try type_params.append(param.lexeme);
                
                if (!self.match(.comma)) break;
            }
            try self.expect(.gt);
        }
        
        // Parse parameters
        try self.expect(.l_paren);
        var params = std.ArrayList(*ast.Node).init(self.allocator);
        
        if (!self.check(.r_paren)) {
            while (true) {
                const param = try self.parseParameter();
                try params.append(param);
                
                if (!self.match(.comma)) break;
            }
        }
        
        try self.expect(.r_paren);
        
        // Parse return type
        var return_type: ?*ast.Type = null;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }
        
        // Parse region annotation
        var region = ast.Region.stack;
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
        }
        
        // Parse body
        var body: ?*ast.Node = null;
        if (self.match(.l_brace)) {
            body = try self.parseBlock(region);
            try self.expect(.r_brace);
        } else {
            try self.expect(.semicolon);
        }
        
        const func_node = try ast.Node.create(self.allocator, .function_decl, start);
        func_node.data = .{
            .function_decl = .{
                .name = name.lexeme,
                .params = params.toOwnedSlice(),
                .return_type = return_type,
                .body = body,
                .region = region,
                .is_public = is_public,
                .is_extern = body == null,
                .is_export = false,
            },
        };
        
        return func_node;
    }
    
    fn parseActor(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        // Parse type parameters
        if (self.match(.lt)) {
            while (!self.check(.gt) and !self.stream.isAtEnd()) {
                _ = try self.expect(.identifier);
                if (!self.match(.comma)) break;
            }
            try self.expect(.gt);
        }
        
        // Parse state type
        var state_type: ?*ast.Type = null;
        if (self.match(.colon)) {
            state_type = try self.parseType();
        }
        
        // Parse region annotation
        var region = ast.Region.heap; // Los actores por defecto en heap
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
        }
        
        try self.expect(.l_brace);
        
        var messages = std.ArrayList(*ast.Node).init(self.allocator);
        var handlers = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            if (self.match(.kw_type)) {
                const message = try self.parseMessage();
                try messages.append(message);
            } else if (self.match(.kw_fn)) {
                const handler = try self.parseHandler();
                try handlers.append(handler);
            } else {
                try self.errorAtCurrent("Expected message type or handler function");
                self.synchronize();
            }
        }
        
        try self.expect(.r_brace);
        
        const actor_node = try ast.Node.create(self.allocator, .actor_decl, start);
        actor_node.data = .{
            .actor_decl = .{
                .name = name.lexeme,
                .state_type = state_type,
                .messages = messages.toOwnedSlice(),
                .handlers = handlers.toOwnedSlice(),
                .region = region,
                .is_public = is_public,
            },
        };
        
        return actor_node;
    }
    
    fn parseMessage(self: *Parser) !*ast.Node {
        const name = try self.expect(.identifier);
        
        if (self.match(.equal)) {
            // Alias type
            const underlying = try self.parseType();
            try self.expect(.semicolon);
            
            const type_node = try ast.Node.create(self.allocator, .type_decl, name.loc);
            type_node.data = .{
                .type_decl = .{
                    .name = name.lexeme,
                    .underlying = underlying,
                    .methods = &.{},
                    .is_public = false,
                },
            };
            
            return type_node;
        }
        
        // Struct type
        try self.expect(.l_brace);
        var fields = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            const field = try self.parseField();
            try fields.append(field);
            
            if (!self.match(.comma)) break;
        }
        
        try self.expect(.r_brace);
        try self.expect(.semicolon);
        
        const struct_node = try ast.Node.create(self.allocator, .struct_decl, name.loc);
        struct_node.data = .{
            .struct_decl = .{
                .name = name.lexeme,
                .fields = fields.toOwnedSlice(),
                .methods = &.{},
                .region = .message, // Los mensajes siempre en región message
                .is_public = false,
            },
        };
        
        return struct_node;
    }
    
    fn parseHandler(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        try self.expect(.l_paren);
        
        var params = std.ArrayList(*ast.Node).init(self.allocator);
        if (!self.check(.r_paren)) {
            const param = try self.parseParameter();
            try params.append(param);
        }
        
        try self.expect(.r_paren);
        
        var return_type: ?*ast.Type = null;
        if (self.match(.arrow)) {
            return_type = try self.parseType();
        }
        
        var region = ast.Region.message;
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
        }
        
        try self.expect(.l_brace);
        const body = try self.parseBlock(region);
        try self.expect(.r_brace);
        
        const func_node = try ast.Node.create(self.allocator, .function_decl, start);
        func_node.data = .{
            .function_decl = .{
                .name = name.lexeme,
                .params = params.toOwnedSlice(),
                .return_type = return_type,
                .body = body,
                .region = region,
                .is_public = false,
                .is_extern = false,
                .is_export = false,
            },
        };
        
        return func_node;
    }
    
    fn parseTypeDeclaration(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        if (self.match(.equal)) {
            const underlying = try self.parseType();
            try self.expect(.semicolon);
            
            const type_node = try ast.Node.create(self.allocator, .type_decl, start);
            type_node.data = .{
                .type_decl = .{
                    .name = name.lexeme,
                    .underlying = underlying,
                    .methods = &.{},
                    .is_public = is_public,
                },
            };
            
            return type_node;
        }
        
        // Parse type parameters
        if (self.match(.lt)) {
            while (!self.check(.gt) and !self.stream.isAtEnd()) {
                _ = try self.expect(.identifier);
                if (!self.match(.comma)) break;
            }
            try self.expect(.gt);
        }
        
        try self.expect(.l_brace);
        
        var methods = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            if (self.match(.kw_fn)) {
                const method = try self.parseFunction(false);
                try methods.append(method);
            } else {
                try self.errorAtCurrent("Expected method declaration");
                self.synchronize();
            }
        }
        
        try self.expect(.r_brace);
        
        // Create a struct type for now
        const underlying = try self.createBasicType(.struct_type, 0, 8); // Placeholder
        
        const type_node = try ast.Node.create(self.allocator, .type_decl, start);
        type_node.data = .{
            .type_decl = .{
                .name = name.lexeme,
                .underlying = underlying,
                .methods = methods.toOwnedSlice(),
                .is_public = is_public,
            },
        };
        
        return type_node;
    }
    
    fn parseStruct(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        // Parse type parameters
        if (self.match(.lt)) {
            while (!self.check(.gt) and !self.stream.isAtEnd()) {
                _ = try self.expect(.identifier);
                if (!self.match(.comma)) break;
            }
            try self.expect(.gt);
        }
        
        // Parse region annotation
        var region = ast.Region.stack;
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
        }
        
        try self.expect(.l_brace);
        
        var fields = std.ArrayList(*ast.Node).init(self.allocator);
        var methods = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            if (self.match(.kw_fn)) {
                const method = try self.parseFunction(false);
                try methods.append(method);
            } else {
                const field = try self.parseField();
                try fields.append(field);
                
                if (!self.match(.comma)) {
                    if (!self.check(.r_brace)) {
                        try self.errorAtCurrent("Expected ',' or '}'");
                    }
                    break;
                }
            }
        }
        
        try self.expect(.r_brace);
        
        const struct_node = try ast.Node.create(self.allocator, .struct_decl, start);
        struct_node.data = .{
            .struct_decl = .{
                .name = name.lexeme,
                .fields = fields.toOwnedSlice(),
                .methods = methods.toOwnedSlice(),
                .region = region,
                .is_public = is_public,
            },
        };
        
        return struct_node;
    }
    
    fn parseEnum(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        // Parse underlying type
        var underlying: ?*ast.Type = null;
        if (self.match(.colon)) {
            underlying = try self.parseType();
        }
        
        try self.expect(.l_brace);
        
        var variants = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            const variant_name = try self.expect(.identifier);
            
            var variant_data: ?*ast.Node = null;
            if (self.match(.l_paren)) {
                variant_data = try self.parseType();
                try self.expect(.r_paren);
            }
            
            // Create variant node
            const variant_node = try ast.Node.create(self.allocator, .variable_decl, variant_name.loc);
            // TODO: Set variant data
            
            try variants.append(variant_node);
            
            if (!self.match(.comma)) break;
        }
        
        try self.expect(.r_brace);
        
        // For now, create a placeholder type node
        const underlying_type = underlying orelse try self.createBasicType(.i32, 4, 4);
        
        const type_node = try ast.Node.create(self.allocator, .type_decl, start);
        type_node.data = .{
            .type_decl = .{
                .name = name.lexeme,
                .underlying = underlying_type,
                .methods = &.{},
                .is_public = is_public,
            },
        };
        
        return type_node;
    }
    
    fn parseUnion(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const name = try self.expect(.identifier);
        
        try self.expect(.l_brace);
        
        var variants = std.ArrayList(*ast.Type).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            const variant_type = try self.parseType();
            try variants.append(variant_type);
            
            if (!self.match(.comma)) break;
        }
        
        try self.expect(.r_brace);
        
        // Create union type
        const union_type = try self.createBasicType(.union_type, 0, 8); // Placeholder
        union_type.fields = null; // TODO: Set variants
        
        const type_node = try ast.Node.create(self.allocator, .type_decl, start);
        type_node.data = .{
            .type_decl = .{
                .name = name.lexeme,
                .underlying = union_type,
                .methods = &.{},
                .is_public = is_public,
            },
        };
        
        return type_node;
    }
    
    fn parseVariableDeclaration(self: *Parser, is_public: bool) !*ast.Node {
        const start = self.previous().loc;
        const keyword = self.previous().ty;
        
        const is_mutable = keyword == .kw_let;
        const is_global = keyword == .kw_global or keyword == .kw_static;
        const is_const = keyword == .kw_const;
        
        var name_token = self.stream.peek();
        if (self.match(.kw_mut)) {
            name_token = try self.expect(.identifier);
        } else {
            name_token = try self.expect(.identifier);
        }
        
        var type_annotation: ?*ast.Type = null;
        if (self.match(.colon)) {
            type_annotation = try self.parseType();
        }
        
        var value: ?*ast.Node = null;
        if (self.match(.equal)) {
            value = try self.parseExpression();
        }
        
        // Region annotation
        var region = if (is_global) ast.Region.global else ast.Region.stack;
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
        }
        
        try self.expect(.semicolon);
        
        const var_node = try ast.Node.create(self.allocator, .variable_decl, start);
        var_node.data = .{
            .variable_decl = .{
                .name = name_token.lexeme,
                .type = type_annotation,
                .value = value,
                .region = region,
                .is_mutable = is_mutable and !is_const,
                .is_public = is_public,
            },
        };
        
        return var_node;
    }
    
    fn parseParameter(self: *Parser) !*ast.Node {
        const start = self.stream.peek().loc;
        
        var is_mut = false;
        if (self.match(.kw_mut)) {
            is_mut = true;
        }
        
        const name = try self.expect(.identifier);
        try self.expect(.colon);
        const param_type = try self.parseType();
        
        const param_node = try ast.Node.create(self.allocator, .parameter, start);
        // TODO: Set parameter data
        
        return param_node;
    }
    
    fn parseField(self: *Parser) !*ast.Node {
        const start = self.stream.peek().loc;
        
        var is_mut = false;
        if (self.match(.kw_mut)) {
            is_mut = true;
        }
        
        const name = try self.expect(.identifier);
        try self.expect(.colon);
        const field_type = try self.parseType();
        
        var default_value: ?*ast.Node = null;
        if (self.match(.equal)) {
            default_value = try self.parseExpression();
        }
        
        const field_node = try ast.Node.create(self.allocator, .variable_decl, start);
        field_node.data = .{
            .variable_decl = .{
                .name = name.lexeme,
                .type = field_type,
                .value = default_value,
                .region = .stack, // Los campos heredan la región de la estructura
                .is_mutable = is_mut,
                .is_public = false,
            },
        };
        
        return field_node;
    }
    
    fn parseType(self: *Parser) !*ast.Type {
        const token = self.stream.peek();
        
        // Check for region annotation
        if (self.match(.at)) {
            const region_token = try self.expect(.identifier);
            const region = ast.Region.fromString(region_token.lexeme) orelse {
                try self.errorAtPrevious("Invalid region");
                return error.ParseError;
            };
            
            const base_type = try self.parseType();
            base_type.region = region;
            return base_type;
        }
        
        // Parse base type
        const base_type = try self.parseBaseType();
        
        // Parse type modifiers
        return self.parseTypeModifiers(base_type);
    }
    
    fn parseBaseType(self: *Parser) !*ast.Type {
        const token = self.stream.advance();
        
        return switch (token.ty) {
            .kw_void => try self.createBasicType(.void, 0, 1),
            .kw_bool => try self.createBasicType(.bool, 1, 1),
            .kw_char => try self.createBasicType(.char, 1, 1),
            .kw_string => try self.createBasicType(.string, 16, 8),
            
            .kw_i8 => try self.createBasicType(.i8, 1, 1),
            .kw_i16 => try self.createBasicType(.i16, 2, 2),
            .kw_i32 => try self.createBasicType(.i32, 4, 4),
            .kw_i64 => try self.createBasicType(.i64, 8, 8),
            
            .kw_u8 => try self.createBasicType(.u8, 1, 1),
            .kw_u16 => try self.createBasicType(.u16, 2, 2),
            .kw_u32 => try self.createBasicType(.u32, 4, 4),
            .kw_u64 => try self.createBasicType(.u64, 8, 8),
            
            .kw_f16 => try self.createBasicType(.f16, 2, 2),
            .kw_f32 => try self.createBasicType(.f32, 4, 4),
            .kw_f64 => try self.createBasicType(.f64, 8, 8),
            .kw_f128 => try self.createBasicType(.f128, 16, 16),
            
            .kw_any => try self.createBasicType(.any, 8, 8),
            
            .identifier => {
                // User-defined type
                const type_node = try self.createBasicType(.struct_type, 0, 8); // Placeholder
                type_node.name = token.lexeme;
                return type_node;
            },
            
            .l_paren => {
                // Tuple or function type
                var types = std.ArrayList(*ast.Type).init(self.allocator);
                
                if (!self.check(.r_paren)) {
                    while (true) {
                        const elem_type = try self.parseType();
                        try types.append(elem_type);
                        
                        if (!self.match(.comma)) break;
                    }
                }
                
                try self.expect(.r_paren);
                
                if (types.items.len == 1) {
                    // Just parentheses
                    return types.items[0];
                }
                
                // Tuple type
                const tuple_type = try self.createBasicType(.tuple_type, 0, 8);
                tuple_type.params = types.toOwnedSlice();
                return tuple_type;
            },
            
            else => {
                try self.errorAtPrevious("Expected type");
                return error.ParseError;
            },
        };
    }
    
    fn parseTypeModifiers(self: *Parser, base_type: *ast.Type) !*ast.Type {
        var current_type = base_type;
        
        while (true) {
            if (self.match(.question)) {
                // Optional type
                const optional_type = try self.createBasicType(.optional, 8, 8);
                optional_type.element_type = current_type;
                current_type = optional_type;
            } else if (self.match(.exclamation)) {
                // Result type (T!E)
                const result_type = try self.createBasicType(.result, 16, 8);
                result_type.element_type = current_type;
                
                if (self.match(.identifier)) {
                    // TODO: Parse error type
                }
                
                current_type = result_type;
            } else if (self.match(.star)) {
                // Pointer type
                const pointer_type = try self.createBasicType(.pointer, 8, 8);
                pointer_type.element_type = current_type;
                current_type = pointer_type;
            } else if (self.match(.l_bracket)) {
                // Array or slice type
                if (self.match(.r_bracket)) {
                    // Slice type []
                    const slice_type = try self.createBasicType(.slice, 16, 8);
                    slice_type.element_type = current_type;
                    current_type = slice_type;
                } else {
                    // Array type [N]
                    const size_expr = try self.parseExpression();
                    try self.expect(.r_bracket);
                    
                    // TODO: Evaluate constant expression
                    const array_type = try self.createBasicType(.array, 0, 8);
                    array_type.element_type = current_type;
                    // array_type.length = evaluated_size;
                    current_type = array_type;
                }
            } else if (self.match(.arrow)) {
                // Function type
                const return_type = try self.parseType();
                
                const func_type = try self.createBasicType(.function_type, 8, 8);
                func_type.return_type = return_type;
                // TODO: Set params
                current_type = func_type;
            } else {
                break;
            }
        }
        
        return current_type;
    }
    
    fn parseBlock(self: *Parser, region: ast.Region) !*ast.Node {
        const start = self.stream.peek().loc;
        var statements = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            const stmt = try self.parseStatement();
            if (stmt) |s| {
                try statements.append(s);
            }
        }
        
        const block_node = try ast.Node.create(self.allocator, .block_stmt, start);
        block_node.data = .{
            .block_stmt = .{
                .statements = statements.toOwnedSlice(),
                .region = region,
            },
        };
        
        return block_node;
    }
    
    fn parseStatement(self: *Parser) !?*ast.Node {
        if (self.match(.kw_return)) {
            return try self.parseReturn();
        } else if (self.match(.kw_if)) {
            return try self.parseIf();
        } else if (self.match(.kw_while)) {
            return try self.parseWhile();
        } else if (self.match(.kw_for)) {
            return try self.parseFor();
        } else if (self.match(.kw_loop)) {
            return try self.parseLoop();
        } else if (self.match(.kw_match)) {
            return try self.parseMatch();
        } else if (self.match(.kw_break)) {
            return try self.parseBreak();
        } else if (self.match(.kw_continue)) {
            return try self.parseContinue();
        } else if (self.match(.kw_defer)) {
            return try self.parseDefer();
        } else if (self.match(.kw_spawn)) {
            return try self.parseSpawn();
        } else if (self.match(.kw_send)) {
            return try self.parseSend();
        } else if (self.match(.l_brace)) {
            const block = try self.parseBlock(ast.Region.stack);
            try self.expect(.r_brace);
            return block;
        } else {
            return try self.parseExpressionStatement();
        }
    }
    
    fn parseExpressionStatement(self: *Parser) !?*ast.Node {
        const expr = try self.parseExpression();
        
        if (self.match(.semicolon)) {
            return expr;
        }
        
        // Expression without semicolon is allowed in some contexts
        return expr;
    }
    
    fn parseExpression(self: *Parser) !*ast.Node {
        return try self.parseAssignment();
    }
    
    fn parseAssignment(self: *Parser) !*ast.Node {
        var expr = try self.parseOr();
        
        while (true) {
            if (self.match(.eq)) {
                const op = ast.Node.BinaryOp.assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.plus_eq)) {
                const op = ast.Node.BinaryOp.add_assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.minus_eq)) {
                const op = ast.Node.BinaryOp.sub_assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.star_eq)) {
                const op = ast.Node.BinaryOp.mul_assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.slash_eq)) {
                const op = ast.Node.BinaryOp.div_assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.percent_eq)) {
                const op = ast.Node.BinaryOp.mod_assign;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.and_eq)) {
                const op = ast.Node.BinaryOp.bit_and;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.or_eq)) {
                const op = ast.Node.BinaryOp.bit_or;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.xor_eq)) {
                const op = ast.Node.BinaryOp.bit_xor;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.shift_left_eq)) {
                const op = ast.Node.BinaryOp.shift_left;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.shift_right_eq)) {
                const op = ast.Node.BinaryOp.shift_right;
                const right = try self.parseAssignment();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseOr(self: *Parser) !*ast.Node {
        var expr = try self.parseAnd();
        
        while (self.match(.or_or)) {
            const op = ast.Node.BinaryOp.or;
            const right = try self.parseAnd();
            expr = try self.makeBinaryExpr(expr, op, right);
        }
        
        return expr;
    }
    
    fn parseAnd(self: *Parser) !*ast.Node {
        var expr = try self.parseEquality();
        
        while (self.match(.and_and)) {
            const op = ast.Node.BinaryOp.and;
            const right = try self.parseEquality();
            expr = try self.makeBinaryExpr(expr, op, right);
        }
        
        return expr;
    }
    
    fn parseEquality(self: *Parser) !*ast.Node {
        var expr = try self.parseComparison();
        
        while (true) {
            if (self.match(.eq_eq)) {
                const op = ast.Node.BinaryOp.eq;
                const right = try self.parseComparison();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.not_eq)) {
                const op = ast.Node.BinaryOp.ne;
                const right = try self.parseComparison();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseComparison(self: *Parser) !*ast.Node {
        var expr = try self.parseBitwiseOr();
        
        while (true) {
            if (self.match(.lt)) {
                const op = ast.Node.BinaryOp.lt;
                const right = try self.parseBitwiseOr();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.lt_eq)) {
                const op = ast.Node.BinaryOp.le;
                const right = try self.parseBitwiseOr();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.gt)) {
                const op = ast.Node.BinaryOp.gt;
                const right = try self.parseBitwiseOr();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.gt_eq)) {
                const op = ast.Node.BinaryOp.ge;
                const right = try self.parseBitwiseOr();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseBitwiseOr(self: *Parser) !*ast.Node {
        var expr = try self.parseBitwiseXor();
        
        while (self.match(.pipe)) {
            const op = ast.Node.BinaryOp.bit_or;
            const right = try self.parseBitwiseXor();
            expr = try self.makeBinaryExpr(expr, op, right);
        }
        
        return expr;
    }
    
    fn parseBitwiseXor(self: *Parser) !*ast.Node {
        var expr = try self.parseBitwiseAnd();
        
        while (self.match(.caret)) {
            const op = ast.Node.BinaryOp.bit_xor;
            const right = try self.parseBitwiseAnd();
            expr = try self.makeBinaryExpr(expr, op, right);
        }
        
        return expr;
    }
    
    fn parseBitwiseAnd(self: *Parser) !*ast.Node {
        var expr = try self.parseShift();
        
        while (self.match(.ampersand)) {
            const op = ast.Node.BinaryOp.bit_and;
            const right = try self.parseShift();
            expr = try self.makeBinaryExpr(expr, op, right);
        }
        
        return expr;
    }
    
    fn parseShift(self: *Parser) !*ast.Node {
        var expr = try self.parseTerm();
        
        while (true) {
            if (self.match(.shift_left)) {
                const op = ast.Node.BinaryOp.shift_left;
                const right = try self.parseTerm();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.shift_right)) {
                const op = ast.Node.BinaryOp.shift_right;
                const right = try self.parseTerm();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseTerm(self: *Parser) !*ast.Node {
        var expr = try self.parseFactor();
        
        while (true) {
            if (self.match(.plus)) {
                const op = ast.Node.BinaryOp.add;
                const right = try self.parseFactor();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.minus)) {
                const op = ast.Node.BinaryOp.sub;
                const right = try self.parseFactor();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseFactor(self: *Parser) !*ast.Node {
        var expr = try self.parseUnary();
        
        while (true) {
            if (self.match(.star)) {
                const op = ast.Node.BinaryOp.mul;
                const right = try self.parseUnary();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.slash)) {
                const op = ast.Node.BinaryOp.div;
                const right = try self.parseUnary();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else if (self.match(.percent)) {
                const op = ast.Node.BinaryOp.mod;
                const right = try self.parseUnary();
                expr = try self.makeBinaryExpr(expr, op, right);
            } else {
                break;
            }
        }
        
        return expr;
    }
    
    fn parseUnary(self: *Parser) !*ast.Node {
        if (self.match(.minus)) {
            const op = ast.Node.UnaryOp.neg;
            const operand = try self.parseUnary();
            return try self.makeUnaryExpr(op, operand);
        } else if (self.match(.exclamation)) {
            const op = ast.Node.UnaryOp.not;
            const operand = try self.parseUnary();
            return try self.makeUnaryExpr(op, operand);
        } else if (self.match(.tilde)) {
            const op = ast.Node.UnaryOp.bit_not;
            const operand = try self.parseUnary();
            return try self.makeUnaryExpr(op, operand);
        } else if (self.match(.star)) {
            const op = ast.Node.UnaryOp.deref;
            const operand = try self.parseUnary();
            return try self.makeUnaryExpr(op, operand);
        } else if (self.match(.ampersand)) {
            const op = ast.Node.UnaryOp.addr_of;
            const operand = try self.parseUnary();
            return try self.makeUnaryExpr(op, operand);
        } else if (self.match(.kw_mut)) {
            if (self.check(.kw_ref)) {
                _ = self.stream.advance();
                const op = ast.Node.UnaryOp.mut_ref;
                const operand = try self.parseUnary();
                return try self.makeUnaryExpr(op, operand);
            }
            // Fall through to parsePrimary
        }
        
        return try self.parsePrimary();
    }
    
    fn parsePrimary(self: *Parser) !*ast.Node {
        if (self.match(.kw_true)) {
            return try self.makeBoolLiteral(true);
        } else if (self.match(.kw_false)) {
            return try self.makeBoolLiteral(false);
        } else if (self.match(.kw_null)) {
            return try self.makeNullLiteral();
        } else if (self.match(.kw_undefined)) {
            return try self.makeUndefinedLiteral();
        } else if (self.match(.integer) or self.match(.float)) {
            return try self.parseNumberLiteral();
        } else if (self.match(.string)) {
            return try self.parseStringLiteral();
        } else if (self.match(.character)) {
            return try self.parseCharacterLiteral();
        } else if (self.match(.l_paren)) {
            return try self.parseGroupedOrTuple();
        } else if (self.match(.l_brace)) {
            return try self.parseStructLiteral();
        } else if (self.match(.l_bracket)) {
            return try self.parseArrayLiteral();
        } else if (self.match(.kw_typeof)) {
            return try self.parseTypeOf();
        } else if (self.match(.kw_spawn)) {
            return try self.parseSpawn();
        } else if (self.match(.identifier)) {
            return try self.parseIdentifierOrCall();
        } else {
            try self.errorAtCurrent("Expected expression");
            return error.ParseError;
        }
    }
    
    fn parseGroupedOrTuple(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        if (self.match(.r_paren)) {
            // Empty tuple
            const tuple_type = try self.createBasicType(.tuple_type, 0, 1);
            const tuple_node = try ast.Node.create(self.allocator, .struct_literal, start);
            // TODO: Set tuple data
            return tuple_node;
        }
        
        const first_expr = try self.parseExpression();
        
        if (self.match(.comma)) {
            // Tuple literal
            var elements = std.ArrayList(*ast.Node).init(self.allocator);
            try elements.append(first_expr);
            
            while (!self.check(.r_paren) and !self.stream.isAtEnd()) {
                const expr = try self.parseExpression();
                try elements.append(expr);
                
                if (!self.match(.comma)) break;
            }
            
            try self.expect(.r_paren);
            
            const tuple_node = try ast.Node.create(self.allocator, .struct_literal, start);
            // TODO: Set tuple data with elements
            return tuple_node;
        }
        
        // Grouped expression
        try self.expect(.r_paren);
        return first_expr;
    }
    
    fn parseStructLiteral(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        var fields = std.ArrayList(*ast.Node).init(self.allocator);
        
        if (!self.check(.r_brace)) {
            while (true) {
                const name = try self.expect(.identifier);
                try self.expect(.colon);
                const value = try self.parseExpression();
                
                // Create field assignment node
                const field_node = try ast.Node.create(self.allocator, .binary_expr, name.loc);
                field_node.data = .{
                    .binary_expr = .{
                        .op = .assign,
                        .left = try self.makeIdentifier(name.lexeme, name.loc),
                        .right = value,
                        .region = .stack,
                    },
                };
                
                try fields.append(field_node);
                
                if (!self.match(.comma)) break;
            }
        }
        
        try self.expect(.r_brace);
        
        const struct_node = try ast.Node.create(self.allocator, .struct_literal, start);
        // TODO: Set struct literal data
        return struct_node;
    }
    
    fn parseArrayLiteral(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        var elements = std.ArrayList(*ast.Node).init(self.allocator);
        
        if (!self.check(.r_bracket)) {
            while (true) {
                const expr = try self.parseExpression();
                try elements.append(expr);
                
                if (!self.match(.comma)) break;
            }
        }
        
        try self.expect(.r_bracket);
        
        const array_node = try ast.Node.create(self.allocator, .array_literal, start);
        // TODO: Set array literal data
        return array_node;
    }
    
    fn parseTypeOf(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        try self.expect(.l_paren);
        const expr = try self.parseExpression();
        try self.expect(.r_paren);
        
        const type_of_node = try ast.Node.create(self.allocator, .typeof_expr, start);
        // TODO: Set typeof data
        return type_of_node;
    }
    
    fn parseSpawn(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const actor_type = try self.parseType();
        
        try self.expect(.l_paren);
        var args = std.ArrayList(*ast.Node).init(self.allocator);
        
        if (!self.check(.r_paren)) {
            while (true) {
                const arg = try self.parseExpression();
                try args.append(arg);
                
                if (!self.match(.comma)) break;
            }
        }
        
        try self.expect(.r_paren);
        
        const spawn_node = try ast.Node.create(self.allocator, .spawn_expr, start);
        spawn_node.data = .{
            .spawn_expr = .{
                .actor_type = actor_type,
                .args = args.toOwnedSlice(),
                .region = .heap, // Los actores spawn se crean en heap por defecto
            },
        };
        
        return spawn_node;
    }
    
    fn parseSend(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const actor = try self.parseExpression();
        
        if (!self.match(.arrow)) {
            try self.errorAtCurrent("Expected '->' after actor");
            return error.ParseError;
        }
        
        const message = try self.parseExpression();
        
        const send_node = try ast.Node.create(self.allocator, .send_expr, start);
        send_node.data = .{
            .send_expr = .{
                .actor = actor,
                .message = message,
                .region = .message,
            },
        };
        
        return send_node;
    }
    
    fn parseIdentifierOrCall(self: *Parser) !*ast.Node {
        const identifier = self.previous();
        const start = identifier.loc;
        
        // Check for function call
        if (self.match(.l_paren)) {
            var args = std.ArrayList(*ast.Node).init(self.allocator);
            
            if (!self.check(.r_paren)) {
                while (true) {
                    const arg = try self.parseExpression();
                    try args.append(arg);
                    
                    if (!self.match(.comma)) break;
                }
            }
            
            try self.expect(.r_paren);
            
            const call_node = try ast.Node.create(self.allocator, .call_expr, start);
            call_node.data = .{
                .call_expr = .{
                    .callee = try self.makeIdentifier(identifier.lexeme, identifier.loc),
                    .args = args.toOwnedSlice(),
                    .region = .stack,
                },
            };
            
            return call_node;
        }
        
        // Just an identifier
        return try self.makeIdentifier(identifier.lexeme, identifier.loc);
    }
    
    fn parseNumberLiteral(self: *Parser) !*ast.Node {
        const token = self.previous();
        const lexeme = token.lexeme;
        
        // Parse number based on suffix
        if (std.mem.endsWith(u8, lexeme, "i8")) {
            const value = try std.fmt.parseInt(i8, lexeme[0..lexeme.len-2], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .i8);
        } else if (std.mem.endsWith(u8, lexeme, "i16")) {
            const value = try std.fmt.parseInt(i16, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .i16);
        } else if (std.mem.endsWith(u8, lexeme, "i32")) {
            const value = try std.fmt.parseInt(i32, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .i32);
        } else if (std.mem.endsWith(u8, lexeme, "i64")) {
            const value = try std.fmt.parseInt(i64, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .i64);
        } else if (std.mem.endsWith(u8, lexeme, "u8")) {
            const value = try std.fmt.parseInt(u8, lexeme[0..lexeme.len-2], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .u8);
        } else if (std.mem.endsWith(u8, lexeme, "u16")) {
            const value = try std.fmt.parseInt(u16, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .u16);
        } else if (std.mem.endsWith(u8, lexeme, "u32")) {
            const value = try std.fmt.parseInt(u32, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .u32);
        } else if (std.mem.endsWith(u8, lexeme, "u64")) {
            const value = try std.fmt.parseInt(u64, lexeme[0..lexeme.len-3], 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .u64);
        } else if (std.mem.endsWith(u8, lexeme, "f32")) {
            const value = try std.fmt.parseFloat(f32, lexeme[0..lexeme.len-3]);
            return try self.makeNumberLiteral(@floatCast(f64, value), .f32);
        } else if (std.mem.endsWith(u8, lexeme, "f64")) {
            const value = try std.fmt.parseFloat(f64, lexeme[0..lexeme.len-3]);
            return try self.makeNumberLiteral(value, .f64);
        } else if (std.mem.indexOf(u8, lexeme, ".")) |_| {
            // Float literal without suffix
            const value = try std.fmt.parseFloat(f64, lexeme);
            return try self.makeNumberLiteral(value, .f64);
        } else {
            // Integer literal without suffix
            const value = try std.fmt.parseInt(i64, lexeme, 10);
            return try self.makeNumberLiteral(@floatFromInt(f64, value), .i32);
        }
    }
    
    fn parseStringLiteral(self: *Parser) !*ast.Node {
        const token = self.previous();
        const lexeme = token.lexeme;
        
        // Remove quotes
        const value = lexeme[1..lexeme.len-1];
        
        const string_node = try ast.Node.create(self.allocator, .string_literal, token.loc);
        string_node.data = .{
            .string_literal = .{
                .value = value,
                .type = try self.createBasicType(.string, 16, 8),
            },
        };
        
        return string_node;
    }
    
    fn parseCharacterLiteral(self: *Parser) !*ast.Node {
        const token = self.previous();
        const lexeme = token.lexeme;
        
        // Remove quotes and handle escape sequences
        const value = lexeme[1..lexeme.len-1];
        var char_value: u8 = 0;
        
        if (value.len == 1) {
            char_value = value[0];
        } else if (value.len == 2 and value[0] == '\\') {
            char_value = switch (value[1]) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\x00',
                '\'' => '\'',
                '"' => '"',
                '\\' => '\\',
                else => value[1],
            };
        }
        
        const char_node = try ast.Node.create(self.allocator, .char_literal, token.loc);
        // TODO: Set character literal data
        return char_node;
    }
    
    fn parseReturn(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        var value: ?*ast.Node = null;
        if (!self.check(.semicolon) and !self.check(.r_brace)) {
            value = try self.parseExpression();
        }
        
        try self.expect(.semicolon);
        
        const return_node = try ast.Node.create(self.allocator, .return_stmt, start);
        return_node.data = .{
            .return_stmt = .{
                .value = value,
                .region = .stack,
            },
        };
        
        return return_node;
    }
    
    fn parseIf(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const condition = try self.parseExpression();
        
        try self.expect(.l_brace);
        const then_branch = try self.parseBlock(ast.Region.stack);
        try self.expect(.r_brace);
        
        var else_branch: ?*ast.Node = null;
        if (self.match(.kw_else)) {
            if (self.match(.kw_if)) {
                else_branch = try self.parseIf();
            } else {
                try self.expect(.l_brace);
                else_branch = try self.parseBlock(ast.Region.stack);
                try self.expect(.r_brace);
            }
        }
        
        const if_node = try ast.Node.create(self.allocator, .if_stmt, start);
        if_node.data = .{
            .if_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
                .region = .stack,
            },
        };
        
        return if_node;
    }
    
    fn parseWhile(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const condition = try self.parseExpression();
        
        try self.expect(.l_brace);
        const body = try self.parseBlock(ast.Region.stack);
        try self.expect(.r_brace);
        
        const while_node = try ast.Node.create(self.allocator, .while_stmt, start);
        while_node.data = .{
            .while_stmt = .{
                .condition = condition,
                .body = body,
                .region = .stack,
            },
        };
        
        return while_node;
    }
    
    fn parseFor(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        const iterator = try self.parseVariableDeclaration(false) orelse {
            try self.errorAtCurrent("Expected iterator declaration");
            return error.ParseError;
        };
        
        try self.expect(.kw_in);
        const iterable = try self.parseExpression();
        
        try self.expect(.l_brace);
        const body = try self.parseBlock(ast.Region.stack);
        try self.expect(.r_brace);
        
        const for_node = try ast.Node.create(self.allocator, .for_stmt, start);
        for_node.data = .{
            .for_stmt = .{
                .iterator = iterator,
                .iterable = iterable,
                .body = body,
                .region = .stack,
            },
        };
        
        return for_node;
    }
    
    fn parseLoop(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        try self.expect(.l_brace);
        const body = try self.parseBlock(ast.Region.stack);
        try self.expect(.r_brace);
        
        const loop_node = try ast.Node.create(self.allocator, .loop_stmt, start);
        // TODO: Set loop data
        return loop_node;
    }
    
    fn parseMatch(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const value = try self.parseExpression();
        
        try self.expect(.l_brace);
        
        var cases = std.ArrayList(*ast.Node).init(self.allocator);
        
        while (!self.check(.r_brace) and !self.stream.isAtEnd()) {
            if (self.match(.kw_case)) {
                const pattern = try self.parseExpression();
                try self.expect(.arrow);
                
                var body: *ast.Node = undefined;
                if (self.match(.l_brace)) {
                    body = try self.parseBlock(ast.Region.stack);
                    try self.expect(.r_brace);
                } else {
                    body = try self.parseExpression();
                    try self.expect(.semicolon);
                }
                
                const case_node = try ast.Node.create(self.allocator, .case_stmt, pattern.loc);
                // TODO: Set case data
                try cases.append(case_node);
            } else if (self.match(.kw_default)) {
                try self.expect(.arrow);
                
                var body: *ast.Node = undefined;
                if (self.match(.l_brace)) {
                    body = try self.parseBlock(ast.Region.stack);
                    try self.expect(.r_brace);
                } else {
                    body = try self.parseExpression();
                    try self.expect(.semicolon);
                }
                
                const default_node = try ast.Node.create(self.allocator, .case_stmt, start);
                // TODO: Set default case data
                try cases.append(default_node);
            } else {
                try self.errorAtCurrent("Expected 'case' or 'default'");
                self.synchronize();
            }
        }
        
        try self.expect(.r_brace);
        
        const match_node = try ast.Node.create(self.allocator, .match_stmt, start);
        // TODO: Set match data
        return match_node;
    }
    
    fn parseBreak(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        
        var value: ?*ast.Node = null;
        if (!self.check(.semicolon)) {
            value = try self.parseExpression();
        }
        
        try self.expect(.semicolon);
        
        const break_node = try ast.Node.create(self.allocator, .break_stmt, start);
        // TODO: Set break data
        return break_node;
    }
    
    fn parseContinue(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        try self.expect(.semicolon);
        
        const continue_node = try ast.Node.create(self.allocator, .continue_stmt, start);
        // TODO: Set continue data
        return continue_node;
    }
    
    fn parseDefer(self: *Parser) !*ast.Node {
        const start = self.previous().loc;
        const expr = try self.parseExpression();
        try self.expect(.semicolon);
        
        const defer_node = try ast.Node.create(self.allocator, .defer_stmt, start);
        // TODO: Set defer data
        return defer_node;
    }
    
    // Helper functions
    fn makeBinaryExpr(self: *Parser, left: *ast.Node, op: ast.Node.BinaryOp, right: *ast.Node) !*ast.Node {
        const start = left.loc;
        const binary_node = try ast.Node.create(self.allocator, .binary_expr, start);
        binary_node.data = .{
            .binary_expr = .{
                .op = op,
                .left = left,
                .right = right,
                .region = .stack,
            },
        };
        return binary_node;
    }
    
    fn makeUnaryExpr(self: *Parser, op: ast.Node.UnaryOp, operand: *ast.Node) !*ast.Node {
        const start = operand.loc;
        const unary_node = try ast.Node.create(self.allocator, .unary_expr, start);
        unary_node.data = .{
            .unary_expr = .{
                .op = op,
                .operand = operand,
                .region = .stack,
            },
        };
        return unary_node;
    }
    
    fn makeIdentifier(self: *Parser, name: []const u8, loc: Token.Loc) !*ast.Node {
        const ast_loc = .{
            .file = loc.file,
            .start = loc.start,
            .end = loc.end,
            .line = loc.line,
            .column = loc.column,
        };
        
        const ident_node = try ast.Node.create(self.allocator, .identifier, ast_loc);
        ident_node.data = .{
            .identifier = .{
                .name = name,
                .type = null,
                .symbol = null,
            },
        };
        return ident_node;
    }
    
    fn makeNumberLiteral(self: *Parser, value: f64, kind: ast.TypeKind) !*ast.Node {
        const size = switch (kind) {
            .i8, .u8 => 1,
            .i16, .u16 => 2,
            .i32, .u32, .f32 => 4,
            .i64, .u64, .f64 => 8,
            .f128 => 16,
            else => 8,
        };
        
        const align = size;
        const type_node = try self.createBasicType(kind, size, align);
        
        const number_node = try ast.Node.create(self.allocator, .number_literal, self.previous().loc);
        number_node.data = .{
            .number_literal = .{
                .value = value,
                .type = type_node,
            },
        };
        
        return number_node;
    }
    
    fn makeBoolLiteral(self: *Parser, value: bool) !*ast.Node {
        const bool_node = try ast.Node.create(self.allocator, .bool_literal, self.previous().loc);
        // TODO: Set bool literal data
        return bool_node;
    }
    
    fn makeNullLiteral(self: *Parser) !*ast.Node {
        const null_node = try ast.Node.create(self.allocator, .null_literal, self.previous().loc);
        // TODO: Set null literal data
        return null_node;
    }
    
    fn makeUndefinedLiteral(self: *Parser) !*ast.Node {
        const undefined_node = try ast.Node.create(self.allocator, .null_literal, self.previous().loc);
        // TODO: Set undefined literal data
        return undefined_node;
    }
    
    fn createBasicType(self: *Parser, kind: ast.TypeKind, size: usize, align: usize) !*ast.Type {
        const type_node = try self.allocator.create(ast.Type);
        type_node.* = .{
            .kind = kind,
            .size = size,
            .align = align,
            .name = null,
            .region = .stack,
            .fields = null,
            .methods = null,
            .params = null,
            .return_type = null,
            .element_type = null,
            .length = null,
        };
        return type_node;
    }
    
    // Error handling
    fn errorAtCurrent(self: *Parser, message: []const u8) !void {
        self.errorAt(self.stream.peek().loc, message);
    }
    
    fn errorAtPrevious(self: *Parser, message: []const u8) !void {
        self.errorAt(self.previous().loc, message);
    }
    
    fn errorAt(self: *Parser, loc: Token.Loc, message: []const u8) !void {
        try self.errors.append(.{
            .message = message,
            .loc = loc,
        });
    }
    
    fn synchronize(self: *Parser) void {
        _ = self.stream.advance();
        
        while (!self.stream.isAtEnd()) {
            if (self.previous().ty == .semicolon) return;
            
            switch (self.stream.peek().ty) {
                .kw_fn, .kw_actor, .kw_type, .kw_struct, .kw_enum,
                .kw_union, .kw_let, .kw_const, .kw_global, .kw_static,
                .kw_return, .kw_if, .kw_while, .kw_for, .kw_loop,
                .kw_match, .kw_break, .kw_continue, .kw_defer => return,
                else => {},
            }
            
            _ = self.stream.advance();
        }
    }
    
    // Token stream helpers
    fn match(self: *Parser, ty: Token.Type) bool {
        return self.stream.match(ty);
    }
    
    fn check(self: *Parser, ty: Token.Type) bool {
        return self.stream.peek().ty == ty;
    }
    
    fn expect(self: *Parser, ty: Token.Type) !Token {
        return self.stream.expect(ty) catch {
            try self.errorAtCurrent(std.fmt.allocPrint(
                self.allocator,
                "Expected {s}, found {s}",
                .{ @tagName(ty), @tagName(self.stream.peek().ty) }
            ) catch "Expected token");
            return error.ParseError;
        };
    }
    
    fn previous(self: *Parser) Token {
        // Simple implementation - in real parser would track previous token
        return .{
            .ty = .eof,
            .lexeme = "",
            .loc = .{
                .file = self.current_file,
                .start = 0,
                .end = 0,
                .line = 0,
                .column = 0,
            },
        };
    }
};
