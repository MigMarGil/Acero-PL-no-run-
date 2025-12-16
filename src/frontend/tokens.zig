const std = @import("std");

pub const Token = struct {
    ty: Type,
    lexeme: []const u8,
    loc: Loc,
    
    pub const Type = enum {
        // Palabras clave
        kw_fn,
        kw_actor,
        kw_type,
        kw_struct,
        kw_enum,
        kw_union,
        kw_let,
        kw_mut,
        kw_const,
        kw_global,
        kw_static,
        kw_extern,
        kw_export,
        kw_import,
        kw_from,
        kw_as,
        kw_pub,
        kw_priv,
        kw_return,
        kw_if,
        kw_else,
        kw_elif,
        kw_while,
        kw_for,
        kw_in,
        kw_loop,
        kw_match,
        kw_case,
        kw_default,
        kw_break,
        kw_continue,
        kw_defer,
        kw_spawn,
        kw_send,
        kw_recv,
        kw_select,
        kw_with,
        kw_do,
        kw_try,
        kw_catch,
        kw_throw,
        kw_async,
        kw_await,
        kw_yield,
        
        // Tipos
        kw_void,
        kw_bool,
        kw_char,
        kw_string,
        kw_i8, kw_i16, kw_i32, kw_i64,
        kw_u8, kw_u16, kw_u32, kw_u64,
        kw_f16, kw_f32, kw_f64, kw_f128,
        kw_any,
        kw_typeof,
        
        // Valores literales
        kw_true,
        kw_false,
        kw_null,
        kw_undefined,
        
        // Regiones
        kw_stack,
        kw_arena,
        kw_heap,
        kw_global,
        kw_thread,
        kw_message,
        
        // Identificadores
        identifier,
        
        // Literales
        integer,
        float,
        string,
        character,
        
        // Operadores
        plus,           // +
        minus,          // -
        star,           // *
        slash,          // /
        percent,        // %
        
        ampersand,      // &
        pipe,           // |
        caret,          // ^
        tilde,          // ~
        
        exclamation,    // !
        question,       // ?
        at,             // @
        dollar,         // $
        
        // Operadores de comparación
        eq_eq,          // ==
        not_eq,         // !=
        lt,             // <
        lt_eq,          // <=
        gt,             // >
        gt_eq,          // >=
        
        // Operadores lógicos
        and_and,        // &&
        or_or,          // ||
        
        // Operadores de bits
        shift_left,     // <<
        shift_right,    // >>
        
        // Asignación
        eq,             // =
        plus_eq,        // +=
        minus_eq,       // -=
        star_eq,        // *=
        slash_eq,       // /=
        percent_eq,     // %=
        and_eq,         // &=
        or_eq,          // |=
        xor_eq,         // ^=
        shift_left_eq,  // <<=
        shift_right_eq, // >>=
        
        // Incremento/decremento
        plus_plus,      // ++
        minus_minus,    // --
        
        // Puntuación
        l_paren,        // (
        r_paren,        // )
        l_brace,        // {
        r_brace,        // }
        l_bracket,      // [
        r_bracket,      // ]
        
        comma,          // ,
        colon,          // :
        semicolon,      // ;
        dot,            // .
        dot_dot,        // ..
        dot_dot_dot,    // ...
        
        arrow,          // ->
        fat_arrow,      // =>
        
        // Especiales
        eof,
        error,
        comment,
        doc_comment,
        
        // Directivas del preprocesador
        directive,
        
        pub fn isKeyword(ty: Type) bool {
            return switch (ty) {
                .kw_fn, .kw_actor, .kw_type, .kw_struct, .kw_enum, .kw_union,
                .kw_let, .kw_mut, .kw_const, .kw_global, .kw_static,
                .kw_extern, .kw_export, .kw_import, .kw_from, .kw_as,
                .kw_pub, .kw_priv, .kw_return, .kw_if, .kw_else, .kw_elif,
                .kw_while, .kw_for, .kw_in, .kw_loop, .kw_match, .kw_case,
                .kw_default, .kw_break, .kw_continue, .kw_defer, .kw_spawn,
                .kw_send, .kw_recv, .kw_select, .kw_with, .kw_do, .kw_try,
                .kw_catch, .kw_throw, .kw_async, .kw_await, .kw_yield,
                .kw_void, .kw_bool, .kw_char, .kw_string,
                .kw_i8, .kw_i16, .kw_i32, .kw_i64,
                .kw_u8, .kw_u16, .kw_u32, .kw_u64,
                .kw_f16, .kw_f32, .kw_f64, .kw_f128,
                .kw_any, .kw_typeof,
                .kw_true, .kw_false, .kw_null, .kw_undefined,
                .kw_stack, .kw_arena, .kw_heap, .kw_global, .kw_thread, .kw_message => true,
                else => false,
            };
        }
        
        pub fn isTypeKeyword(ty: Type) bool {
            return switch (ty) {
                .kw_void, .kw_bool, .kw_char, .kw_string,
                .kw_i8, .kw_i16, .kw_i32, .kw_i64,
                .kw_u8, .kw_u16, .kw_u32, .kw_u64,
                .kw_f16, .kw_f32, .kw_f64, .kw_f128,
                .kw_any => true,
                else => false,
            };
        }
        
        pub fn isRegionKeyword(ty: Type) bool {
            return switch (ty) {
                .kw_stack, .kw_arena, .kw_heap, .kw_global, .kw_thread, .kw_message => true,
                else => false,
            };
        }
        
        pub fn toString(ty: Type) []const u8 {
            return @tagName(ty);
        }
    };
    
    pub const Loc = struct {
        file: []const u8,
        start: usize,
        end: usize,
        line: u32,
        column: u32,
        
        pub fn span(self: Loc, other: Loc) Loc {
            return .{
                .file = self.file,
                .start = @min(self.start, other.start),
                .end = @max(self.end, other.end),
                .line = self.line,
                .column = self.column,
            };
        }
        
        pub fn format(self: Loc, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try writer.print("{s}:{}:{}", .{ self.file, self.line, self.column });
        }
    };
    
    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        
        try writer.print("{s}", .{self.ty.toString()});
        
        if (self.lexeme.len > 0) {
            try writer.print("('{s}')", .{self.lexeme});
        }
        
        try writer.print(" @{}", .{self.loc});
    }
};

pub const TokenStream = struct {
    tokens: []const Token,
    position: usize = 0,
    
    pub fn init(tokens: []const Token) TokenStream {
        return .{ .tokens = tokens };
    }
    
    pub fn peek(self: *const TokenStream) Token {
        if (self.position < self.tokens.len) {
            return self.tokens[self.position];
        }
        return .{ .ty = .eof, .lexeme = "", .loc = .{
            .file = "",
            .start = 0,
            .end = 0,
            .line = 0,
            .column = 0,
        }};
    }
    
    pub fn advance(self: *TokenStream) Token {
        const token = self.peek();
        if (token.ty != .eof) {
            self.position += 1;
        }
        return token;
    }
    
    pub fn match(self: *TokenStream, ty: Token.Type) bool {
        if (self.peek().ty == ty) {
            _ = self.advance();
            return true;
        }
        return false;
    }
    
    pub fn expect(self: *TokenStream, ty: Token.Type) !Token {
        const token = self.peek();
        if (token.ty == ty) {
            return self.advance();
        }
        
        return error.UnexpectedToken;
    }
    
    pub fn isAtEnd(self: *const TokenStream) bool {
        return self.position >= self.tokens.len or self.peek().ty == .eof;
    }
    
    pub fn reset(self: *TokenStream, position: usize) void {
        self.position = position;
    }
    
    pub fn save(self: *const TokenStream) usize {
        return self.position;
    }
};
