const std = @import("std");
const tokens = @import("tokens.zig");
const Token = tokens.Token;

pub const Lexer = struct {
    source: []const u8,
    file: []const u8,
    position: usize = 0,
    line: u32 = 1,
    column: u32 = 1,
    start: usize = 0,
    
    pub fn init(source: []const u8, file: []const u8) Lexer {
        return .{
            .source = source,
            .file = file,
        };
    }
    
    pub fn tokenize(allocator: std.mem.Allocator, source: []const u8, file: []const u8) ![]const Token {
        var lexer = init(source, file);
        var token_list = std.ArrayList(Token).init(allocator);
        
        while (true) {
            const token = lexer.nextToken();
            try token_list.append(token);
            
            if (token.ty == .eof or token.ty == .error) {
                break;
            }
        }
        
        return token_list.toOwnedSlice();
    }
    
    fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();
        self.start = self.position;
        
        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }
        
        const c = self.advance();
        
        return switch (c) {
            '(' => self.makeToken(.l_paren),
            ')' => self.makeToken(.r_paren),
            '{' => self.makeToken(.l_brace),
            '}' => self.makeToken(.r_brace),
            '[' => self.makeToken(.l_bracket),
            ']' => self.makeToken(.r_bracket),
            ',' => self.makeToken(.comma),
            '.' => self.match('.') ? (self.match('.') ? self.makeToken(.dot_dot_dot) : self.makeToken(.dot_dot)) : self.makeToken(.dot),
            ';' => self.makeToken(.semicolon),
            ':' => self.makeToken(.colon),
            '@' => self.makeToken(.at),
            '$' => self.makeToken(.dollar),
            '?' => self.makeToken(.question),
            
            '+' => self.match('=') ? self.makeToken(.plus_eq) : 
                   self.match('+') ? self.makeToken(.plus_plus) : self.makeToken(.plus),
            '-' => self.match('=') ? self.makeToken(.minus_eq) :
                   self.match('>') ? self.makeToken(.arrow) :
                   self.match('-') ? self.makeToken(.minus_minus) : self.makeToken(.minus),
            '*' => self.match('=') ? self.makeToken(.star_eq) : self.makeToken(.star),
            '/' => if (self.match('/')) {
                return self.comment();
            } else if (self.match('*')) {
                return self.blockComment();
            } else if (self.match('=')) {
                return self.makeToken(.slash_eq);
            } else {
                return self.makeToken(.slash);
            },
            '%' => self.match('=') ? self.makeToken(.percent_eq) : self.makeToken(.percent),
            
            '=' => self.match('=') ? self.makeToken(.eq_eq) :
                   self.match('>') ? self.makeToken(.fat_arrow) : self.makeToken(.eq),
            '!' => self.match('=') ? self.makeToken(.not_eq) : self.makeToken(.exclamation),
            '<' => self.match('=') ? self.makeToken(.lt_eq) :
                   self.match('<') ? (self.match('=') ? self.makeToken(.shift_left_eq) : self.makeToken(.shift_left)) : self.makeToken(.lt),
            '>' => self.match('=') ? self.makeToken(.gt_eq) :
                   self.match('>') ? (self.match('=') ? self.makeToken(.shift_right_eq) : self.makeToken(.shift_right)) : self.makeToken(.gt),
            
            '&' => self.match('=') ? self.makeToken(.and_eq) :
                   self.match('&') ? self.makeToken(.and_and) : self.makeToken(.ampersand),
            '|' => self.match('=') ? self.makeToken(.or_eq) :
                   self.match('|') ? self.makeToken(.or_or) : self.makeToken(.pipe),
            '^' => self.match('=') ? self.makeToken(.xor_eq) : self.makeToken(.caret),
            '~' => self.makeToken(.tilde),
            
            '"' => self.string(),
            '\'' => self.character(),
            
            '0'...'9' => self.number(),
            
            'a'...'z', 'A'...'Z', '_' => self.identifier(),
            
            else => self.makeToken(.error),
        };
    }
    
    fn comment(self: *Lexer) Token {
        // Comentario de línea
        while (self.peek() != '\n' and !self.isAtEnd()) {
            _ = self.advance();
        }
        
        const lexeme = self.source[self.start..self.position];
        return Token{
            .ty = .comment,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn blockComment(self: *Lexer) Token {
        // Comentario de bloque
        var depth: u32 = 1;
        
        while (depth > 0 and !self.isAtEnd()) {
            if (self.peek() == '/' and self.peekNext() == '*') {
                _ = self.advance();
                _ = self.advance();
                depth += 1;
            } else if (self.peek() == '*' and self.peekNext() == '/') {
                _ = self.advance();
                _ = self.advance();
                depth -= 1;
            } else {
                if (self.peek() == '\n') {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                _ = self.advance();
            }
        }
        
        if (depth > 0) {
            return self.makeToken(.error);
        }
        
        const lexeme = self.source[self.start..self.position];
        const ty: Token.Type = if (std.mem.startsWith(u8, lexeme, "/*!")) .doc_comment else .comment;
        
        return Token{
            .ty = ty,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn string(self: *Lexer) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\\') {
                _ = self.advance(); // Skip backslash
            }
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 1;
            }
            _ = self.advance();
        }
        
        if (self.isAtEnd()) {
            return self.makeToken(.error);
        }
        
        _ = self.advance(); // Closing quote
        
        const lexeme = self.source[self.start..self.position];
        return Token{
            .ty = .string,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn character(self: *Lexer) Token {
        if (self.peek() == '\\') {
            _ = self.advance(); // Skip backslash
        }
        _ = self.advance(); // Character
        
        if (self.peek() != '\'') {
            return self.makeToken(.error);
        }
        
        _ = self.advance(); // Closing quote
        
        const lexeme = self.source[self.start..self.position];
        return Token{
            .ty = .character,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn number(self: *Lexer) Token {
        var is_float = false;
        
        // Parte entera
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }
        
        // Parte fraccionaria
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            is_float = true;
            _ = self.advance(); // .
            
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        
        // Exponente
        if (self.peek() == 'e' or self.peek() == 'E') {
            is_float = true;
            _ = self.advance(); // e o E
            
            if (self.peek() == '+' or self.peek() == '-') {
                _ = self.advance();
            }
            
            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        
        // Sufijos de tipo
        if (self.peek() == 'i' or self.peek() == 'u' or self.peek() == 'f') {
            _ = self.advance();
            switch (self.peek()) {
                '8', '1', '3', '6' => {
                    _ = self.advance();
                    if (self.peek() == '2' or self.peek() == '4') {
                        _ = self.advance();
                    }
                },
                else => {},
            }
        }
        
        const lexeme = self.source[self.start..self.position];
        return Token{
            .ty = if (is_float) .float else .integer,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn identifier(self: *Lexer) Token {
        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
            _ = self.advance();
        }
        
        const lexeme = self.source[self.start..self.position];
        const ty = self.identifierType(lexeme);
        
        return Token{
            .ty = ty,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn identifierType(self: *Lexer, lexeme: []const u8) Token.Type {
        const keyword_map = std.ComptimeStringMap(Token.Type, .{
            // Palabras clave principales
            .{ "fn", .kw_fn },
            .{ "actor", .kw_actor },
            .{ "type", .kw_type },
            .{ "struct", .kw_struct },
            .{ "enum", .kw_enum },
            .{ "union", .kw_union },
            .{ "let", .kw_let },
            .{ "mut", .kw_mut },
            .{ "const", .kw_const },
            .{ "global", .kw_global },
            .{ "static", .kw_static },
            .{ "extern", .kw_extern },
            .{ "export", .kw_export },
            .{ "import", .kw_import },
            .{ "from", .kw_from },
            .{ "as", .kw_as },
            .{ "pub", .kw_pub },
            .{ "priv", .kw_priv },
            .{ "return", .kw_return },
            .{ "if", .kw_if },
            .{ "else", .kw_else },
            .{ "elif", .kw_elif },
            .{ "while", .kw_while },
            .{ "for", .kw_for },
            .{ "in", .kw_in },
            .{ "loop", .kw_loop },
            .{ "match", .kw_match },
            .{ "case", .kw_case },
            .{ "default", .kw_default },
            .{ "break", .kw_break },
            .{ "continue", .kw_continue },
            .{ "defer", .kw_defer },
            .{ "spawn", .kw_spawn },
            .{ "send", .kw_send },
            .{ "recv", .kw_recv },
            .{ "select", .kw_select },
            .{ "with", .kw_with },
            .{ "do", .kw_do },
            .{ "try", .kw_try },
            .{ "catch", .kw_catch },
            .{ "throw", .kw_throw },
            .{ "async", .kw_async },
            .{ "await", .kw_await },
            .{ "yield", .kw_yield },
            
            // Tipos
            .{ "void", .kw_void },
            .{ "bool", .kw_bool },
            .{ "char", .kw_char },
            .{ "string", .kw_string },
            .{ "i8", .kw_i8 },
            .{ "i16", .kw_i16 },
            .{ "i32", .kw_i32 },
            .{ "i64", .kw_i64 },
            .{ "u8", .kw_u8 },
            .{ "u16", .kw_u16 },
            .{ "u32", .kw_u32 },
            .{ "u64", .kw_u64 },
            .{ "f16", .kw_f16 },
            .{ "f32", .kw_f32 },
            .{ "f64", .kw_f64 },
            .{ "f128", .kw_f128 },
            .{ "any", .kw_any },
            .{ "typeof", .kw_typeof },
            
            // Valores literales
            .{ "true", .kw_true },
            .{ "false", .kw_false },
            .{ "null", .kw_null },
            .{ "undefined", .kw_undefined },
            
            // Regiones
            .{ "stack", .kw_stack },
            .{ "arena", .kw_arena },
            .{ "heap", .kw_heap },
            .{ "global", .kw_global },
            .{ "thread", .kw_thread },
            .{ "message", .kw_message },
        });
        
        return keyword_map.get(lexeme) orelse .identifier;
    }
    
    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    _ = self.advance();
                },
                else => break,
            }
        }
    }
    
    fn advance(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        
        const c = self.source[self.position];
        self.position += 1;
        self.column += 1;
        
        return c;
    }
    
    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.position] != expected) return false;
        
        self.position += 1;
        self.column += 1;
        return true;
    }
    
    fn peek(self: *const Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }
    
    fn peekNext(self: *const Lexer) u8 {
        if (self.position + 1 >= self.source.len) return 0;
        return self.source[self.position + 1];
    }
    
    fn isAtEnd(self: *const Lexer) bool {
        return self.position >= self.source.len;
    }
    
    fn makeToken(self: *const Lexer, ty: Token.Type) Token {
        const lexeme = self.source[self.start..self.position];
        return Token{
            .ty = ty,
            .lexeme = lexeme,
            .loc = self.currentLoc(),
        };
    }
    
    fn currentLoc(self: *const Lexer) Token.Loc {
        return .{
            .file = self.file,
            .start = self.start,
            .end = self.position,
            .line = self.line,
            .column = self.column -| (self.position - self.start),
        };
    }
};
