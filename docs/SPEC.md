# Especificación del Lenguaje ACERO v0.1

## 1. Visión General

ACERO es un lenguaje de sistemas que prioriza:
- **Control predictivo** de memoria sin recolector de basura
- **Compilación incremental** ultra-rápida
- **Concurrencia segura** mediante actores
- **Tipos en tiempo de ejecución** para reflexión nativa

## 2. Sistema de Tipos

### 2.1 Tipos Primitivos
```acero
// Enteros con/sin signo
let a: i8 = 127;
let b: u64 = 1000;

// Punto flotante
let c: f32 = 3.14;
let d: f64 = 2.71828;

// Booleanos y caracteres
let e: bool = true;
let f: char = 'A';

// Cadenas (UTF-8, inmutables)
let g: string = "Hola";

### 2.2 Tipos Compuestos
// Estructuras
type Vector3 {
    x: f32
    y: f32
    z: f32
    
    fn length(self) -> f32 {
        return math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z);
    }
}

// Enumeraciones
type Color {
    Red
    Green
    Blue
    RGB(r: u8, g: u8, b: u8)
}

// Uniones
type Result {
    Ok(value: i32)
    Error(msg: string)
}

### 2.3 Tipos de Referencia
// Punteros
let ptr: *i32 = &x;

// Slices
let slice: []i32 = array[0..10];

// Arrays
let array: [10]i32 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

## 3. Sistema de Memoria por Regiones
3.1 Regiones Disponibles
acero
@stack      // Vida de la función actual (automática)
@arena      // Vida controlada manualmente (liberación masiva)
@heap       // Vida dinámica (liberación automática)
@global     // Vida del programa completo
@thread     // Vida del hilo actual
@message    // Vida del mensaje (para actores)
3.2 Uso de Regiones
acero
fn process_frame(arena: Arena) @arena {
    // Memoria en arena para este frame
    let buffer = arena.alloc(u8, 1024);
    
    // Memoria en stack (default)
    let temp: i32 = 42;
    
    // Liberación automática al final del frame
    arena.reset();
}

// Variables globales
global CONFIG: Config @global = Config{};
3.3 Reglas de Compatibilidad
@stack → @arena:  (arena vive más)

@arena → @stack:  (stack vive menos)

@global → Cualquiera: (global vive siempre)

Cualquiera → @message: (mensaje se procesa rápido)

4. Concurrencia por Actores
4.1 Definición de Actores
acero
actor Counter @heap {
    state: i32 = 0
    
    fn increment(msg: Increment) @message {
        state += msg.amount;
        return state;
    }
    
    fn get(msg: Get) -> i32 @message {
        return state;
    }
}

// Tipos de mensaje
type Increment { amount: i32 }
type Get {}
4.2 Uso de Actores
acero
fn main() {
    // Crear actor
    let counter = spawn Counter();
    
    // Enviar mensajes
    let result1 = counter->Increment{amount: 5};
    let result2 = counter->Get{};
    
    io.println("Counter value: ", result2); // 5
}
4.3 Canales (Alternativa)
acero
fn producer(ch: Channel<i32>) {
    for (let i = 0; i < 10; i += 1) {
        ch.send(i);
    }
    ch.close();
}

fn consumer(ch: Channel<i32>) {
    while (true) {
        let value = ch.receive() catch break;
        io.println("Received: ", value);
    }
}
5. Sistema de Módulos
5.1 Importación
acero
// Importar módulo completo
import std.io;

// Importar con alias
import std.collections as col;

// Importar elementos específicos
from std.math import { sin, cos, tan };

// Importar todo
from std.string import *;
5.2 Exportación
acero
// Exportar función
pub fn public_function() {
    // ...
}

// Exportar tipo
pub type PublicType {
    // ...
}

// Módulo privado por defecto
priv fn private_function() {
    // ...
}
6. Control de Flujo
6.1 Condicionales
acero
if (x > 0) {
    io.println("Positivo");
} elif (x < 0) {
    io.println("Negativo");
} else {
    io.println("Cero");
}
6.2 Bucles
acero
// While
while (condition) {
    // ...
}

// For (rangos)
for (let i in 0..10) {
    io.println(i);
}

// For (colecciones)
for (item in collection) {
    io.println(item);
}

// Loop infinito
loop {
    // ...
    break when condition;
}
6.3 Match (Pattern Matching)
acero
match (value) {
    case 0 => io.println("Cero");
    case 1 | 2 => io.println("Uno o Dos");
    case x if x > 10 => io.println("Mayor que 10");
    case _ => io.println("Otro");
}
7. Manejo de Errores
7.1 Result Types
acero
fn divide(a: i32, b: i32) -> Result<i32, string> {
    if (b == 0) {
        return Error("División por cero");
    }
    return Ok(a / b);
}

fn main() {
    match (divide(10, 0)) {
        case Ok(value) => io.println("Resultado: ", value);
        case Error(msg) => io.println("Error: ", msg);
    }
}
7.2 Defer
acero
fn process_file(path: string) {
    let file = io.open(path);
    defer file.close(); // Se ejecuta al final del scope
    
    // Usar file...
    
    // file.close() se llama automáticamente aquí
}
8. Compilación Incremental
ACERO implementa compilación incremental mediante:

Hashing semántico (ignora comentarios/whitespace)

Cache por módulo con dependencias

Invalidación granular de código afectado

Cache persistente en disco

bash
# Primera compilación (completa)
acero compile programa.acero    # ~500ms

# Cambio menor (solo módulo afectado)
acero compile programa.acero    # ~50ms

# Cambio de interfaz (módulos dependientes)
acero compile programa.acero    # ~150ms
9. Interoperabilidad con C
acero
// Declarar función C
extern fn printf(format: *const u8, ...) -> i32;

// Usar desde ACERO
fn main() {
    printf("Hola desde C\n");
}

// Exportar función a C
export fn acero_function() -> i32 {
    return 42;
}
10. Ejecución en Tiempo de Ejecución
10.1 Reflection
acero
type Person {
    name: string
    age: i32
}

fn inspect(value: any) {
    let type_info = type_of(value);
    
    io.println("Tipo: ", type_info.name);
    io.println("Tamaño: ", type_info.size);
    
    if (type_info.kind == .struct_type) {
        for (type_info.fields) |field| {
            io.println("  Campo: ", field.name, " -> ", field.type.name);
        }
    }
}
10.2 Metaprogramación
acero
// Comptime (tiempo de compilación)
comptime {
    // Generar código en tiempo de compilación
    for ([1, 2, 3, 4, 5]) |n| {
        io.println("Número: ", n);
    }
}

// Plantillas de tipos
type Container(T) {
    items: []T
    length: usize
    
    fn get(self, index: usize) -> ?T {
        if (index < self.length) {
            return self.items[index];
        }
        return null;
    }
}
11. Librería Estándar
11.1 Módulos Principales
std.io - Entrada/salida

std.mem - Gestión de memoria

std.math - Funciones matemáticas

std.thread - Concurrencia

std.collections - Estructuras de datos

std.string - Manipulación de cadenas

std.time - Medición del tiempo

std.test - Utilidades para testing

11.2 Ejemplos
acero
import std;

fn main() {
    // IO
    io.println("Hola, mundo!");
    let input = io.read_line();
    
    // Collections
    let vec = collections.Vector.new(i32);
    vec.push(1);
    vec.push(2);
    vec.push(3);
    
    // Strings
    let str = "   Hola   ";
    let trimmed = string.trim(str);
    
    // Time
    let start = time.now();
    // ... operación costosa ...
    let elapsed = time.now() - start;
    io.println("Tiempo: ", elapsed, "ms");
}
12. Guía de Estilo
12.1 Convenciones
Funciones: snake_case

Tipos: PascalCase

Variables: snake_case

Constantes: SCREAMING_SNAKE_CASE

Módulos: snake_case

12.2 Formato
acero
// Buena
fn calculate_area(width: f32, height: f32) -> f32 {
    return width * height;
}

// Mala
fn CalculateArea(w:f32,h:f32)->f32{return w*h;}
12.3 Comentarios
acero
// Comentario de una línea

/*
 * Comentario de bloque
 * para documentación compleja
 */

/// Documentación de función
/// # Parámetros
/// - `x`: Valor de entrada
/// # Retorna
/// El doble de `x`
fn double(x: i32) -> i32 {
    return x * 2;
}
Apéndice A: Gramática BNF
text
program     = { import | declaration }
import      = "import" identifier [ "as" identifier ] ";"
            | "from" identifier "import" "{" identifier { "," identifier } "}" ";"

declaration = [ "pub" ] ( function | type | variable | actor )
function    = "fn" identifier [ "<" type_params ">" ]
              "(" [ params ] ")" [ "->" type ] [ "@" region ]
              ( block | ";" )
type        = "type" identifier [ type_params ] [ "=" type_expr ] ";"
variable    = ( "let" | "mut" | "const" | "global" | "static" )
              identifier [ ":" type ] [ "=" expression ] [ "@" region ] ";"
actor       = "actor" identifier [ type_params ] [ ":" type ] [ "@" region ]
              "{" { message | handler } "}"

type_expr   = identifier
            | type_expr "[" [ expression ] "]"
            | type_expr "?"
            | type_expr "!"
            | "*" type_expr
            | "(" type_expr { "," type_expr } ")"

expression  = assignment
assignment  = logical_or { ( "=" | "+=" | "-=" | "*=" | "/=" ) assignment }
logical_or  = logical_and { "||" logical_and }
logical_and = equality { "&&" equality }
equality    = comparison { ( "==" | "!=" ) comparison }
comparison  = term { ( "<" | "<=" | ">" | ">=" ) term }
term        = factor { ( "+" | "-" ) factor }
factor      = unary { ( "*" | "/" | "%" ) unary }
unary       = [ "-" | "!" | "~" | "*" | "&" ] primary
primary     = number | string | char | "true" | "false" | "null"
            | identifier [ "(" [ arguments ] ")" ]
            | "(" expression ")"
            | "{" [ field_init { "," field_init } ] "}"
            | "[" [ expression { "," expression } ] "]"

block       = "{" { statement } "}"
statement   = expression ";"
            | declaration
            | "return" [ expression ] ";"
            | "if" expression block [ "else" ( "if" expression block | block ) ]
            | "while" expression block
            | "for" identifier "in" expression block
            | "loop" block
            | "match" expression "{" { case } "}"
            | "defer" expression ";"
            | "spawn" type_expr "(" [ arguments ] ")"
            | expression "->" expression ";"

region      = "stack" | "arena" | "heap" | "global" | "thread" | "message"
Apéndice B: Palabras Clave Reservadas
text
// Declaraciones
fn, actor, type, struct, enum, union
let, mut, const, global, static
import, from, as, pub, priv
extern, export

// Control de flujo
if, else, elif, while, for, in, loop
match, case, default
return, break, continue
defer, try, catch, throw

// Concurrencia
spawn, send, recv, select, async, await, yield

// Valores
true, false, null, undefined

// Tipos
void, bool, char, string
i8, i16, i32, i64
u8, u16, u32, u64
f16, f32, f64, f128
any, typeof

// Regiones
stack, arena, heap, global, thread, message
Apéndice C: Operadores
text
// Aritméticos
+   -   *   /   %   **
// Comparación
==  !=  <   <=  >   >=
// Lógicos
&&  ||  !
// Bit a bit
&   |   ^   ~   <<  >>
// Asignación
=   +=  -=  *=  /=  %=  &=  |=  ^=  <<= >>=
// Otros
.   ..  ... ->  =>  @   ?   !