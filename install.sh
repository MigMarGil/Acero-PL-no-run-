#!/bin/bash

# Script de instalación para ACERO

set -e

echo "=== INSTALANDO ACERO v0.1 ==="

# Verificar dependencias
echo "Verificando dependencias..."

if ! command -v zig &> /dev/null; then
    echo "Error: Zig no está instalado"
    echo "Instala Zig desde: https://ziglang.org/download/"
    exit 1
fi

if ! command -v llvm-config &> /dev/null; then
    echo "Advertencia: LLVM no está instalado"
    echo "El backend LLVM no funcionará sin LLVM 15+"
    echo "Puedes instalar con: sudo apt install llvm-15 clang-15"
fi

ZIG_VERSION=$(zig version)
echo "✓ Zig $ZIG_VERSION"

# Construir compilador
echo "Construyendo compilador ACERO..."
zig build -Doptimize=ReleaseSafe

if [ $? -eq 0 ]; then
    echo "✓ Compilador construido"
else
    echo "✗ Error construyendo compilador"
    exit 1
fi

# Instalar en sistema
read -p "¿Instalar en /usr/local/bin? [s/N]: " -n 1 -r
echo

if [[ $REPLY =~ ^[Ss]$ ]]; then
    echo "Instalando..."
    sudo cp zig-out/bin/acero /usr/local/bin/acero
    sudo mkdir -p /usr/local/share/acero
    sudo cp -r examples /usr/local/share/acero/
    sudo cp -r docs /usr/local/share/acero/
    
    echo "✓ ACERO instalado en /usr/local/bin"
    echo ""
    echo "Para usar:"
    echo "  acero compile programa.acero"
    echo "  acero run ejemplo.acero"
    echo ""
    echo "Ejemplos en: /usr/local/share/acero/examples"
else
    echo "Instalación saltada"
    echo ""
    echo "Para usar desde este directorio:"
    echo "  ./zig-out/bin/acero compile programa.acero"
    echo "  ./zig-out/bin/acero run ejemplo.acero"
fi

# Configurar cache
echo "Configurando cache incremental..."
mkdir -p ~/.cache/acero
echo "✓ Cache configurado en ~/.cache/acero"

# Probar instalación
echo ""
echo "=== PRUEBA DE INSTALACIÓN ==="

if ./zig-out/bin/acero version; then
    echo "✓ Instalación exitosa!"
    
    echo ""
    echo "=== EJEMPLOS DISPONIBLES ==="
    echo "1. Hello World:"
    echo "   acero compile examples/hello/main.acero"
    echo ""
    echo "2. Motor de juegos:"
    echo "   acero compile examples/game/engine.acero"
    echo ""
    echo "3. Tests unitarios:"
    echo "   acero test"
    echo ""
    echo "Para más información:"
    echo "   acero --help"
    echo "   ver docs/SPEC.md"
else
    echo "✗ Error en la prueba"
    exit 1
fi

echo ""
echo "¡ACERO está listo para usar! 🚀"
