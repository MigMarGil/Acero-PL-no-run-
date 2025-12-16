const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("frontend/lexer.zig").Lexer;
const Token = @import("frontend/tokens.zig").Token;
const Parser = @import("frontend/parser.zig").Parser;
const SemanticAnalyzer = @import("semantic/analyzer.zig").SemanticAnalyzer;
const LLVMBackend = @import("backend/llvm.zig").LLVMBackend;
const regions = @import("runtime/regions.zig");
const actors = @import("runtime/actors.zig");
const IncrementalCache = @import("cache/incremental.zig").IncrementalCache;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    
    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }
    
    const command = args[1];
    
    if (std.mem.eql(u8, command, "compile")) {
        if (args.len < 3) {
            std.debug.print("Error: Missing input file\n", .{});
            printUsage();
            std.process.exit(1);
        }
        
        const input_file = args[2];
        var output_file: ?[]const u8 = null;
        var optimize_level: LLVMBackend.OptimizationLevel = .default;
        var emit_ir = false;
        var emit_asm = false;
        
        // Parse options
        var i: usize = 3;
        while (i < args.len) : (i += 1) {
            if (std.mem.eql(u8, args[i], "-o")) {
                i += 1;
                if (i >= args.len) {
                    std.debug.print("Error: Missing output file after -o\n", .{});
                    std.process.exit(1);
                }
                output_file = args[i];
            } else if (std.mem.eql(u8, args[i], "-O0")) {
                optimize_level = .none;
            } else if (std.mem.eql(u8, args[i], "-O1")) {
                optimize_level = .less;
            } else if (std.mem.eql(u8, args[i], "-O2")) {
                optimize_level = .default;
            } else if (std.mem.eql(u8, args[i], "-O3")) {
                optimize_level = .aggressive;
            } else if (std.mem.eql(u8, args[i], "--emit-ir")) {
                emit_ir = true;
            } else if (std.mem.eql(u8, args[i], "--emit-asm")) {
                emit_asm = true;
            } else if (std.mem.eql(u8, args[i], "--help")) {
                printUsage();
                return;
            } else {
                std.debug.print("Error: Unknown option '{s}'\n", .{args[i]});
                printUsage();
                std.process.exit(1);
            }
        }
        
        try compileFile(
            allocator,
            input_file,
            output_file,
            optimize_level,
            emit_ir,
            emit_asm,
        );
    } else if (std.mem.eql(u8, command, "run")) {
        if (args.len < 3) {
            std.debug.print("Error: Missing input file\n", .{});
            printUsage();
            std.process.exit(1);
        }
        
        const input_file = args[2];
        try compileAndRun(allocator, input_file);
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            std.debug.print("Error: Missing input file\n", .{});
            printUsage();
            std.process.exit(1);
        }
        
        const input_file = args[2];
        try checkFile(allocator, input_file);
    } else if (std.mem.eql(u8, command, "test")) {
        try runTests(allocator);
    } else if (std.mem.eql(u8, command, "bench")) {
        try runBenchmarks(allocator);
    } else if (std.mem.eql(u8, command, "clean")) {
        try cleanCache();
    } else if (std.mem.eql(u8, command, "version")) {
        printVersion();
    } else {
        std.debug.print("Error: Unknown command '{s}'\n", .{command});
        printUsage();
        std.process.exit(1);
    }
}

fn compileFile(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    output_file: ?[]const u8,
    optimize_level: LLVMBackend.OptimizationLevel,
    emit_ir: bool,
    emit_asm: bool,
) !void {
    std.debug.print("=== Compilando {s} ===\n\n", .{input_file});
    
    const start_time = std.time.microtimestamp();
    
    // Leer archivo
    const source = try std.fs.cwd().readFileAlloc(allocator, input_file, std.math.maxInt(usize));
    defer allocator.free(source);
    
    // 1. Lexer
    std.debug.print("[1/5] Analizando tokens... ", .{});
    const lexer_start = std.time.microtimestamp();
    const tokens = try Lexer.tokenize(allocator, source, input_file);
    const lexer_time = std.time.microtimestamp() - lexer_start;
    std.debug.print("({} tokens, {}μs)\n", .{ tokens.len, lexer_time });
    
    // 2. Parser
    std.debug.print("[2/5] Parseando AST... ", .{});
    const parser_start = std.time.microtimestamp();
    var parser = Parser.init(allocator, @import("frontend/tokens.zig").TokenStream.init(tokens), input_file);
    const ast_node = try parser.parse();
    const parser_time = std.time.microtimestamp() - parser_start;
    std.debug.print("({}μs)\n", .{parser_time});
    
    if (parser.errors.items.len > 0) {
        std.debug.print("\n=== ERRORES DE PARSER ===\n", .{});
        for (parser.errors.items) |err| {
            std.debug.print("  {s}:{}:{}: {s}\n", .{
                err.loc.file, err.loc.line, err.loc.col, err.message
            });
        }
        return error.ParseError;
    }
    
    // 3. Análisis semántico
    std.debug.print("[3/5] Análisis semántico... ", .{});
    const semantic_start = std.time.microtimestamp();
    var analyzer = try SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();
    
    try analyzer.analyze(ast_node);
    const semantic_time = std.time.microtimestamp() - semantic_start;
    std.debug.print("({}μs)\n", .{semantic_time});
    
    if (analyzer.errors.items.len > 0) {
        std.debug.print("\n=== ERRORES SEMÁNTICOS ===\n", .{});
        for (analyzer.errors.items) |err| {
            std.debug.print("  {}:{}: {s}\n", .{
                err.loc.line, err.loc.col, err.message
            });
        }
        return error.SemanticError;
    }
    
    if (analyzer.warnings.items.len > 0) {
        std.debug.print("\n=== ADVERTENCIAS ===\n", .{});
        for (analyzer.warnings.items) |warn| {
            std.debug.print("  {}:{}: {s}\n", .{
                warn.loc.line, warn.loc.col, warn.message
            });
        }
    }
    
    // 4. Generación de código
    std.debug.print("[4/5] Generando código... ", .{});
    const codegen_start = std.time.microtimestamp();
    
    const module_name = std.fs.path.stem(input_file);
    var backend = try LLVMBackend.init(allocator, module_name);
    defer backend.deinit();
    
    try backend.compile(ast_node);
    
    // Optimizar
    try backend.optimize(optimize_level);
    
    const codegen_time = std.time.microtimestamp() - codegen_start;
    std.debug.print("({}μs)\n", .{codegen_time});
    
    // 5. Escribir salida
    std.debug.print("[5/5] Escribiendo salida... ", .{});
    const output_start = std.time.microtimestamp();
    
    const default_output = try std.fmt.allocPrint(allocator, "{s}.o", .{module_name});
    defer allocator.free(default_output);
    
    const out_file = output_file orelse default_output;
    
    if (emit_ir) {
        const ir_file = try std.fmt.allocPrint(allocator, "{s}.ll", .{module_name});
        defer allocator.free(ir_file);
        
        try backend.writeBitcodeFile(ir_file);
        std.debug.print("IR escrito en {s}\n", .{ir_file});
    } else if (emit_asm) {
        const asm_file = try std.fmt.allocPrint(allocator, "{s}.s", .{module_name});
        defer allocator.free(asm_file);
        
        try backend.writeAssemblyFile(asm_file);
        std.debug.print("Ensamblador escrito en {s}\n", .{asm_file});
    } else {
        try backend.writeObjectFile(out_file);
        std.debug.print("Objeto escrito en {s}\n", .{out_file});
    }
    
    const output_time = std.time.microtimestamp() - output_start;
    const total_time = std.time.microtimestamp() - start_time;
    
    // Estadísticas
    std.debug.print("\n=== ESTADÍSTICAS ===\n", .{});
    std.debug.print("  Tiempo total:      {}μs\n", .{total_time});
    std.debug.print("  Tiempo lexer:      {}μs ({:.1}%)\n", .{ lexer_time, @intToFloat(f64, lexer_time) * 100.0 / @intToFloat(f64, total_time) });
    std.debug.print("  Tiempo parser:     {}μs ({:.1}%)\n", .{ parser_time, @intToFloat(f64, parser_time) * 100.0 / @intToFloat(f64, total_time) });
    std.debug.print("  Tiempo semántico:  {}μs ({:.1}%)\n", .{ semantic_time, @intToFloat(f64, semantic_time) * 100.0 / @intToFloat(f64, total_time) });
    std.debug.print("  Tiempo codegen:    {}μs ({:.1}%)\n", .{ codegen_time, @intToFloat(f64, codegen_time) * 100.0 / @intToFloat(f64, total_time) });
    std.debug.print("  Tiempo salida:     {}μs ({:.1}%)\n", .{ output_time, @intToFloat(f64, output_time) * 100.0 / @intToFloat(f64, total_time) });
    std.debug.print("  Tokens:            {}\n", .{tokens.len});
    std.debug.print("  Optimización:      {s}\n", .{@tagName(optimize_level)});
    
    // Mostrar IR si se solicita
    if (emit_ir) {
        std.debug.print("\n=== IR GENERADO ===\n", .{});
        const ir = try backend.getModuleIR();
        std.debug.print("{s}\n", .{ir});
    }
    
    std.debug.print("\n✅ Compilación exitosa!\n", .{});
}

fn compileAndRun(allocator: std.mem.Allocator, input_file: []const u8) !void {
    // Compilar
    const temp_dir = "/tmp/acero";
    try std.fs.cwd().makePath(temp_dir);
    
    const module_name = std.fs.path.stem(input_file);
    const obj_file = try std.fmt.allocPrint(allocator, "{s}/{s}.o", .{ temp_dir, module_name });
    defer allocator.free(obj_file);
    
    const exe_file = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ temp_dir, module_name });
    defer allocator.free(exe_file);
    
    try compileFile(allocator, input_file, obj_file, .default, false, false);
    
    // Enlazar
    std.debug.print("\n=== ENLAZANDO ===\n", .{});
    
    const link_start = std.time.microtimestamp();
    
    var argv = [_][]const u8{
        "cc",
        obj_file,
        "-o",
        exe_file,
        "-lm",
        "-lpthread",
    };
    
    const result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &argv,
        .cwd = ".",
    });
    
    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }
    
    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("Error enlazando: {s}\n", .{result.stderr});
        return error.LinkFailed;
    }
    
    const link_time = std.time.microtimestamp() - link_start;
    std.debug.print("Enlazado en {}μs\n", .{link_time});
    
    // Ejecutar
    std.debug.print("\n=== EJECUTANDO ===\n", .{});
    
    const run_result = try std.ChildProcess.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{exe_file},
        .cwd = ".",
    });
    
    defer {
        allocator.free(run_result.stdout);
        allocator.free(run_result.stderr);
    }
    
    std.debug.print("{s}", .{run_result.stdout});
    
    if (run_result.stderr.len > 0) {
        std.debug.print("stderr: {s}\n", .{run_result.stderr});
    }
    
    std.debug.print("\n✅ Programa ejecutado (exit code: {})\n", .{run_result.term});
}

fn checkFile(allocator: std.mem.Allocator, input_file: []const u8) !void {
    std.debug.print("=== Verificando {s} ===\n\n", .{input_file});
    
    const source = try std.fs.cwd().readFileAlloc(allocator, input_file, std.math.maxInt(usize));
    defer allocator.free(source);
    
    // Lexer
    const tokens = try Lexer.tokenize(allocator, source, input_file);
    std.debug.print("✓ Tokens: {}\n", .{tokens.len});
    
    // Parser
    var parser = Parser.init(allocator, @import("frontend/tokens.zig").TokenStream.init(tokens), input_file);
    const ast_node = try parser.parse();
    
    if (parser.errors.items.len > 0) {
        std.debug.print("\n✗ Errores de parser: {}\n", .{parser.errors.items.len});
        for (parser.errors.items) |err| {
            std.debug.print("  {s}:{}:{}: {s}\n", .{
                err.loc.file, err.loc.line, err.loc.col, err.message
            });
        }
        return error.ParseError;
    }
    std.debug.print("✓ AST generado\n", .{});
    
    // Análisis semántico
    var analyzer = try SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();
    
    try analyzer.analyze(ast_node);
    
    if (analyzer.errors.items.len > 0) {
        std.debug.print("\n✗ Errores semánticos: {}\n", .{analyzer.errors.items.len});
        for (analyzer.errors.items) |err| {
            std.debug.print("  {}:{}: {s}\n", .{
                err.loc.line, err.loc.col, err.message
            });
        }
        return error.SemanticError;
    }
    
    if (analyzer.warnings.items.len > 0) {
        std.debug.print("\n⚠ Advertencias: {}\n", .{analyzer.warnings.items.len});
        for (analyzer.warnings.items) |warn| {
            std.debug.print("  {}:{}: {s}\n", .{
                warn.loc.line, warn.loc.col, warn.message
            });
        }
    }
    
    std.debug.print("\n✅ Archivo válido!\n", .{});
    
    // Estadísticas del AST
    var decl_count: usize = 0;
    var func_count: usize = 0;
    var var_count: usize = 0;
    var type_count: usize = 0;
    
    if (ast_node.ty == .program) {
        const program = ast_node.data.program;
        decl_count = program.declarations.len;
        
        for (program.declarations) |decl| {
            switch (decl.ty) {
                .function_decl => func_count += 1,
                .variable_decl => var_count += 1,
                .type_decl, .struct_decl, .enum_decl, .union_decl => type_count += 1,
                else => {},
            }
        }
    }
    
    std.debug.print("\n=== ESTADÍSTICAS ===\n", .{});
    std.debug.print("  Declaraciones totales: {}\n", .{decl_count});
    std.debug.print("  Funciones:             {}\n", .{func_count});
    std.debug.print("  Variables:             {}\n", .{var_count});
    std.debug.print("  Tipos definidos:       {}\n", .{type_count});
    std.debug.print("  Líneas de código:      {}\n", .{std.mem.count(u8, source, "\n") + 1});
}

fn runTests(allocator: std.mem.Allocator) !void {
    std.debug.print("=== EJECUTANDO TESTS ===\n\n", .{});
    
    const test_dir = "test/unit";
    var dir = try std.fs.cwd().openDir(test_dir, .{});
    defer dir.close();
    
    var walker = try dir.walk(allocator);
    defer walker.deinit();
    
    var passed: usize = 0;
    var failed: usize = 0;
    var skipped: usize = 0;
    
    while (try walker.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.path, ".acero")) continue;
        
        const test_file = try std.fs.path.join(allocator, &.{ test_dir, entry.path });
        defer allocator.free(test_file);
        
        std.debug.print("Test: {s}... ", .{entry.path});
        
        // Verificar sintaxis y semántica
        if (checkFile(allocator, test_file)) {
            std.debug.print("✅\n", .{});
            passed += 1;
        } else |err| {
            std.debug.print("❌ ({})\n", .{err});
            failed += 1;
        }
    }
    
    std.debug.print("\n=== RESULTADOS ===\n", .{});
    std.debug.print("  Pasados:  {}\n", .{passed});
    std.debug.print("  Fallados: {}\n", .{failed});
    std.debug.print("  Saltados: {}\n", .{skipped});
    std.debug.print("  Total:    {}\n", .{passed + failed + skipped});
    
    if (failed > 0) {
        std.process.exit(1);
    }
}

fn runBenchmarks(allocator: std.mem.Allocator) !void {
    std.debug.print("=== EJECUTANDO BENCHMARKS ===\n\n", .{});
    
    // Benchmark de lexer
    std.debug.print("Benchmark Lexer... ", .{});
    const lexer_source = "fn main() { let x = 42; return x + 1; }";
    const lexer_iterations = 10000;
    
    const lexer_start = std.time.microtimestamp();
    for (0..lexer_iterations) |_| {
        _ = try Lexer.tokenize(allocator, lexer_source, "benchmark");
    }
    const lexer_time = std.time.microtimestamp() - lexer_start;
    
    std.debug.print("{} iteraciones en {}μs ({}μs/iter)\n", .{
        lexer_iterations,
        lexer_time,
        lexer_time / lexer_iterations,
    });
    
    // Benchmark de parser
    std.debug.print("Benchmark Parser... ", .{});
    const tokens = try Lexer.tokenize(allocator, lexer_source, "benchmark");
    const parser_iterations = 1000;
    
    const parser_start = std.time.microtimestamp();
    for (0..parser_iterations) |_| {
        var parser = Parser.init(allocator, @import("frontend/tokens.zig").TokenStream.init(tokens), "benchmark");
        _ = try parser.parse();
    }
    const parser_time = std.time.microtimestamp() - parser_start;
    
    std.debug.print("{} iteraciones en {}μs ({}μs/iter)\n", .{
        parser_iterations,
        parser_time,
        parser_time / parser_iterations,
    });
    
    // Benchmark de análisis semántico
    std.debug.print("Benchmark Análisis Semántico... ", .{});
    var parser = Parser.init(allocator, @import("frontend/tokens.zig").TokenStream.init(tokens), "benchmark");
    const ast_node = try parser.parse();
    const semantic_iterations = 100;
    
    const semantic_start = std.time.microtimestamp();
    for (0..semantic_iterations) |_| {
        var analyzer = try SemanticAnalyzer.init(allocator);
        defer analyzer.deinit();
        _ = analyzer.analyze(ast_node) catch {};
    }
    const semantic_time = std.time.microtimestamp() - semantic_start;
    
    std.debug.print("{} iteraciones en {}μs ({}μs/iter)\n", .{
        semantic_iterations,
        semantic_time,
        semantic_time / semantic_iterations,
    });
    
    std.debug.print("\n✅ Benchmarks completados\n", .{});
}

fn cleanCache() !void {
    const cache_dir = ".acero_cache";
    
    if (std.fs.cwd().openDir(cache_dir, .{})) |dir| {
        dir.close();
        try std.fs.cwd().deleteTree(cache_dir);
        std.debug.print("✅ Cache limpiado\n", .{});
    } else |_| {
        std.debug.print("ℹ️  No hay cache que limpiar\n", .{});
    }
}

fn printUsage() void {
    std.debug.print(
        \\Uso: acero <comando> [opciones]
        \\
        \\Comandos:
        \\  compile <archivo>  Compilar archivo ACERO
        \\  run <archivo>      Compilar y ejecutar
        \\  check <archivo>    Verificar sintaxis y semántica
        \\  test               Ejecutar tests unitarios
        \\  bench              Ejecutar benchmarks
        \\  clean              Limpiar cache
        \\  version            Mostrar versión
        \\
        \\Opciones de compile:
        \\  -o <archivo>       Especificar archivo de salida
        \\  -O0, -O1, -O2, -O3 Nivel de optimización
        \\  --emit-ir          Generar IR LLVM
        \\  --emit-asm         Generar ensamblador
        \\  --help             Mostrar esta ayuda
        \\
        \\Ejemplos:
        \\  acero compile programa.acero -o programa.o -O2
        \\  acero run ejemplo.acero
        \\  acero check modulo.acero
        \\
    , .{});
}

fn printVersion() void {
    std.debug.print(
        \\ACERO Compiler v0.1.0
        \\Un lenguaje de sistemas con memoria predictiva
        \\Compilado con Zig {}
        \\
        \\Características:
        \\  ✓ Memoria por regiones (stack, arena, heap, global, thread, message)
        \\  ✓ Tipos en tiempo de ejecución
        \\  ✓ Concurrencia por actores
        \\  ✓ Compilación incremental
        \\  ✓ Backend LLVM
        \\
        \\Repositorio: https://github.com/acero-lang/acero
        \\Licencia: MIT
        \\
    , .{@import("builtin").zig_version});
}
