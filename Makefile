.PHONY: all build release debug test clean fmt docs examples bench

# Variables
ACERO := ./zig-out/bin/acero
ZIG := zig
CARGO := cargo

# Targets principales
all: build

build:
	$(ZIG) build

release:
	$(ZIG) build -Doptimize=ReleaseSafe

debug:
	$(ZIG) build -Doptimize=Debug

run:
	$(ZIG) build run -- $(ARGS)

# Testing
test: test-unit test-integration

test-unit:
	$(ZIG) build test

test-integration:
	$(ZIG) build test-integration

# Benchmarks
bench:
	$(ZIG) build bench

# Formateo
fmt:
	$(ZIG) fmt src/ test/ build.zig

# Limpieza
clean:
	rm -rf zig-cache zig-out docs/html

# Documentación
docs:
	$(ZIG) build-lib --docs docs/html src/main.zig

# Ejemplos
examples:
	@echo "=== Compilando ejemplos ==="
	@for example in examples/*/*.acero; do \
		echo "Compilando $$example..."; \
		$(ACERO) $$example 2>&1 | grep -E "(EXITOSA|ERROR)" || true; \
	done

# Herramientas de desarrollo
tools:
	$(ZIG) build -Dtools=true

# Instalación
install: release
	sudo cp $(ACERO) /usr/local/bin/acero
	sudo mkdir -p /usr/local/lib/acero
	sudo cp -r lib/std/* /usr/local/lib/acero/

# Desinstalar
uninstall:
	sudo rm -f /usr/local/bin/acero
	sudo rm -rf /usr/local/lib/acero

# Ayuda
help:
	@echo "Comandos disponibles:"
	@echo "  make build        - Compilar en modo desarrollo"
	@echo "  make release      - Compilar en modo release"
	@echo "  make debug        - Compilar con símbolos de debug"
	@echo "  make run          - Ejecutar: make run ARGS=archivo.acero"
	@echo "  make test         - Ejecutar todos los tests"
	@echo "  make test-unit    - Ejecutar tests unitarios"
	@echo "  make test-integration - Ejecutar tests de integración"
	@echo "  make bench        - Ejecutar benchmarks"
	@echo "  make fmt          - Formatear código"
	@echo "  make clean        - Limpiar builds"
	@echo "  make docs         - Generar documentación"
	@echo "  make examples     - Compilar todos los ejemplos"
	@echo "  make tools        - Construir herramientas"
	@echo "  make install      - Instalar en sistema"
	@echo "  make uninstall    - Desinstalar"
	@echo "  make help         - Mostrar esta ayuda"
