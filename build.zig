const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Configuración del compilador ACERO
    const acero_exe = b.addExecutable(.{
        .name = "acero",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    // Flags para release
    if (optimize == .ReleaseFast or optimize == .ReleaseSafe) {
        acero_exe.strip = true;
    }

    // Enlazar con librerías del sistema
    acero_exe.linkLibC();
    if (target.result.os.tag == .linux) {
        acero_exe.linkSystemLibrary("pthread");
        acero_exe.linkSystemLibrary("m");
    }

    // Instalar el ejecutable
    b.installArtifact(acero_exe);

    // Comando 'run'
    const run_cmd = b.addRunArtifact(acero_exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Ejecutar el compilador ACERO");
    run_step.dependOn(&run_cmd.step);

    // Tests unitarios
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    unit_tests.linkLibC();

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Ejecutar tests unitarios");
    test_step.dependOn(&run_unit_tests.step);

    // Tests de integración
    const integration_test = b.addTest(.{
        .root_source_file = .{ .path = "test/integration/test_runner.zig" },
        .target = target,
        .optimize = optimize,
    });
    integration_test.linkLibC();

    const run_integration_test = b.addRunArtifact(integration_test);
    const integration_step = b.step("test-integration", "Ejecutar tests de integración");
    integration_step.dependOn(&run_integration_test.step);

    // Benchmarks
    const benchmark_exe = b.addExecutable(.{
        .name = "benchmarks",
        .root_source_file = .{ .path = "test/benchmarks/runner.zig" },
        .target = target,
        .optimize = optimize,
    });
    benchmark_exe.linkLibC();

    const benchmark_cmd = b.addRunArtifact(benchmark_exe);
    const benchmark_step = b.step("bench", "Ejecutar benchmarks");
    benchmark_step.dependOn(&benchmark_cmd.step);

    // Formateador de código
    const fmt_step = b.step("fmt", "Formatear código Zig");
    fmt_step.dependOn(b.addFmt(.{
        .paths = &.{"src", "test", "build.zig"},
    }));
}
