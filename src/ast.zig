const std = @import("std");

// Tipos de nodo del AST
pub const NodeType = enum {
    // Declaraciones
    program,
    module_decl,
    import_decl,
    export_decl,
    
    // Tipos
    type_decl,
    struct_decl,
    enum_decl,
    union_decl,
    alias_decl,
    
    // Funciones y métodos
    function_decl,
    method_decl,
    constructor_decl,
    destructor_decl,
    
    // Variables
    variable_decl,
    constant_decl,
    global_decl,
    static_decl,
    
    // Control de flujo
    if_stmt,
    else_stmt,
    elif_stmt,
    while_stmt,
    for_stmt,
    loop_stmt,
    match_stmt,
    case_stmt,
    
    // Expresiones
    call_expr,
    binary_expr,
    unary_expr,
    member_expr,
    index_expr,
    cast_expr,
    sizeof_expr,
    alignof_expr,
    offsetof_expr,
    
    // Literales
    number_literal,
    string_literal,
    char_literal,
    array_literal,
    struct_literal,
    bool_literal,
    null_literal,
    
    // Identificadores
    identifier,
    qualified_name,
    
    // Bloques
    block_stmt,
    return_stmt,
    break_stmt,
    continue_stmt,
    defer_stmt,
    error_stmt,
    
    // Concurrencia
    spawn_expr,
    send_expr,
    receive_expr,
    select_stmt,
    
    // Regiones de memoria
    region_decl,
    alloc_expr,
    dealloc_expr,
    
    // Otros
    attribute,
    comment,
    empty_stmt,
};

// Regiones de memoria
pub const Region = enum {
    stack,      // Vida de la función actual
    arena,      // Vida controlada manualmente
    heap,       // Vida dinámica (liberación automática)
    global,     // Vida del programa completo
    thread,     // Vida del thread actual
    message,    // Vida del mensaje (para actores)
    
    pub fn default() Region {
        return .stack;
    }
    
    pub fn fromString(str: []const u8) ?Region {
        const map = std.ComptimeStringMap(Region, .{
            .{ "stack", .stack },
            .{ "arena", .arena },
            .{ "heap", .heap },
            .{ "global", .global },
            .{ "thread", .thread },
            .{ "message", .message },
        });
        return map.get(str);
    }
    
    pub fn toString(self: Region) []const u8 {
        return switch (self) {
            .stack => "stack",
            .arena => "arena",
            .heap => "heap",
            .global => "global",
            .thread => "thread",
            .message => "message",
        };
    }
};

// Tipos de datos
pub const TypeKind = enum {
    // Primitivos
    void,
    bool,
    char,
    string,
    
    // Enteros
    i8, i16, i32, i64,
    u8, u16, u32, u64,
    
    // Punto flotante
    f16, f32, f64, f128,
    
    // Compuestos
    pointer,
    array,
    slice,
    struct_type,
    enum_type,
    union_type,
    function_type,
    tuple_type,
    
    // Especiales
    any,
    type,
    region,
    actor,
    channel,
    
    // Error
    error_type,
    optional,
    result,
};

pub const Type = struct {
    kind: TypeKind,
    size: usize,
    align: usize,
    name: ?[]const u8 = null,
    region: Region = .stack,
    
    // Para tipos compuestos
    fields: ?[]Field = null,
    methods: ?[]*Node = null,
    params: ?[]Type = null,
    return_type: ?*Type = null,
    element_type: ?*Type = null,
    length: ?usize = null,
    
    pub const Field = struct {
        name: []const u8,
        type: *Type,
        offset: usize,
        default_value: ?*Node = null,
    };
    
    pub fn hash(self: Type) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(@tagName(self.kind));
        hasher.update(std.mem.asBytes(&self.size));
        hasher.update(std.mem.asBytes(&self.align));
        if (self.name) |name| {
            hasher.update(name);
        }
        hasher.update(self.region.toString());
        return hasher.final();
    }
    
    pub fn toString(self: *const Type, allocator: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();
        
        const writer = buffer.writer();
        
        if (self.name) |name| {
            try writer.print("{s}", .{name});
        } else {
            try writer.print("{s}", .{@tagName(self.kind)});
        }
        
        if (self.region != .stack) {
            try writer.print(" @{s}", .{self.region.toString()});
        }
        
        return buffer.toOwnedSlice();
    }
};

// Nodo del AST
pub const Node = struct {
    ty: NodeType,
    loc: Loc,
    parent: ?*Node = null,
    children: ?[]*Node = null,
    data: Data,
    
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
    };
    
    pub const Data = union {
        // Programa
        program: struct {
            modules: []*Node,
            imports: []*Node,
            exports: []*Node,
        },
        
        // Módulo
        module_decl: struct {
            name: []const u8,
            body: *Node,
            is_public: bool = false,
        },
        
        // Función
        function_decl: struct {
            name: []const u8,
            params: []*Node,
            return_type: ?*Type,
            body: ?*Node,
            region: Region,
            is_public: bool = false,
            is_extern: bool = false,
            is_export: bool = false,
        },
        
        // Variable
        variable_decl: struct {
            name: []const u8,
            type: ?*Type,
            value: ?*Node,
            region: Region,
            is_mutable: bool = true,
            is_public: bool = false,
        },
        
        // Tipo
        type_decl: struct {
            name: []const u8,
            underlying: *Type,
            methods: []*Node,
            is_public: bool = false,
        },
        
        // Estructura
        struct_decl: struct {
            name: []const u8,
            fields: []*Node,
            methods: []*Node,
            region: Region,
            is_public: bool = false,
        },
        
        // Actor
        actor_decl: struct {
            name: []const u8,
            state_type: ?*Type,
            messages: []*Node,
            handlers: []*Node,
            region: Region,
            is_public: bool = false,
        },
        
        // Llamada a función
        call_expr: struct {
            callee: *Node,
            args: []*Node,
            region: Region,
        },
        
        // Expresión binaria
        binary_expr: struct {
            op: BinaryOp,
            left: *Node,
            right: *Node,
            region: Region,
        },
        
        // Expresión unaria
        unary_expr: struct {
            op: UnaryOp,
            operand: *Node,
            region: Region,
        },
        
        // Literal numérico
        number_literal: struct {
            value: f64,
            type: ?*Type = null,
        },
        
        // Literal de cadena
        string_literal: struct {
            value: []const u8,
            type: *Type,
        },
        
        // Identificador
        identifier: struct {
            name: []const u8,
            type: ?*Type = null,
            symbol: ?*Symbol = null,
        },
        
        // Bloque
        block_stmt: struct {
            statements: []*Node,
            region: Region,
        },
        
        // If
        if_stmt: struct {
            condition: *Node,
            then_branch: *Node,
            else_branch: ?*Node,
            region: Region,
        },
        
        // While
        while_stmt: struct {
            condition: *Node,
            body: *Node,
            region: Region,
        },
        
        // For
        for_stmt: struct {
            iterator: *Node,
            iterable: *Node,
            body: *Node,
            region: Region,
        },
        
        // Return
        return_stmt: struct {
            value: ?*Node,
            region: Region,
        },
        
        // Spawn actor
        spawn_expr: struct {
            actor_type: *Type,
            args: []*Node,
            region: Region,
        },
        
        // Send message
        send_expr: struct {
            actor: *Node,
            message: *Node,
            region: Region,
        },
        
        // Alloc
        alloc_expr: struct {
            type: *Type,
            count: ?*Node,
            region: Region,
        },
        
        // Atributo
        attribute: struct {
            name: []const u8,
            args: []*Node,
        },
        
        // Comentario
        comment: struct {
            text: []const u8,
            is_doc: bool = false,
        },
    };
    
    pub const BinaryOp = enum {
        // Aritméticos
        add,        // +
        sub,        // -
        mul,        // *
        div,        // /
        mod,        // %
        
        // Bit a bit
        bit_and,    // &
        bit_or,     // |
        bit_xor,    // ^
        shift_left, // <<
        shift_right, // >>
        
        // Comparación
        eq,         // ==
        ne,         // !=
        lt,         // <
        le,         // <=
        gt,         // >
        ge,         // >=
        
        // Lógicos
        and,        // &&
        or,         // ||
        
        // Asignación
        assign,     // =
        add_assign, // +=
        sub_assign, // -=
        mul_assign, // *=
        div_assign, // /=
        mod_assign, // %=
    };
    
    pub const UnaryOp = enum {
        neg,        // -
        not,        // !
        bit_not,    // ~
        deref,      // *
        addr_of,    // &
        ref,        // ref
        mut_ref,    // mut ref
    };
    
    pub fn create(allocator: std.mem.Allocator, ty: NodeType, loc: Loc) !*Node {
        const node = try allocator.create(Node);
        node.ty = ty;
        node.loc = loc;
        node.parent = null;
        node.children = null;
        
        // Inicializar data según tipo
        switch (ty) {
            .program => node.data = .{ .program = .{
                .modules = &.{},
                .imports = &.{},
                .exports = &.{},
            }},
            .block_stmt => node.data = .{ .block_stmt = .{
                .statements = &.{},
                .region = .stack,
            }},
            else => node.data = undefined,
        }
        
        return node;
    }
    
    pub fn addChild(self: *Node, child: *Node) void {
        child.parent = self;
        
        if (self.children == null) {
            self.children = &.{child};
        } else {
            var list = std.ArrayList(*Node).init(self.getAllocator() orelse return);
            list.appendSlice(self.children.?);
            list.append(child);
            self.children = list.toOwnedSlice() catch return;
        }
    }
    
    pub fn getAllocator(self: *const Node) ?std.mem.Allocator {
        if (self.parent) |parent| {
            return parent.getAllocator();
        }
        return null;
    }
};

// Símbolo (para tabla de símbolos)
pub const Symbol = struct {
    name: []const u8,
    type: *Type,
    node: *Node,
    scope: *Scope,
    is_public: bool = false,
    is_mutable: bool = true,
    is_extern: bool = false,
    
    pub const Scope = struct {
        parent: ?*Scope,
        symbols: std.StringHashMap(*Symbol),
        region: Region,
        
        pub fn init(allocator: std.mem.Allocator, parent: ?*Scope, region: Region) Scope {
            return .{
                .parent = parent,
                .symbols = std.StringHashMap(*Symbol).init(allocator),
                .region = region,
            };
        }
        
        pub fn lookup(self: *const Scope, name: []const u8) ?*Symbol {
            if (self.symbols.get(name)) |sym| {
                return sym;
            }
            if (self.parent) |parent| {
                return parent.lookup(name);
            }
            return null;
        }
    };
};
