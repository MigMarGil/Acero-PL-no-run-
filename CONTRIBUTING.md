# Guía de Contribución

## Objetivo del Proyecto

ACERO es un lenguaje de sistemas que resuelve el problema de tener que elegir entre:
- **Control manual** (C/Rust) vs **Desarrollo rápido** (Go/Java)
- **Seguridad de memoria** vs **Simplicidad**
- **Rendimiento** vs **Productividad**

## Estructura del Proyecto
acero/
├── src/
│ ├── frontend/ # Lexer, parser, tokens
│ ├── semantic/ # Type checker, analyzer
│ ├── runtime/ # Regiones, actores, tipos
│ ├── backend/ # LLVM codegen
│ ├── cache/ # Compilación incremental
│ └── modules/ # Sistema de módulos
├── lib/ # Librería estándar
├── examples/ # Ejemplos demostrativos
├── test/ # Tests y benchmarks
└── docs/ # Documentación


## Cómo Empezar

### 1. Configurar Entorno
```bash
# Clonar repositorio
git clone https://github.com/acero-lang/acero.git
cd acero

# Instalar dependencias
sudo apt install zig llvm-15 clang-15

# Construir compilador
zig build

# Ejecutar tests
zig build test
```
### 2. Flujo de Trabajo
Crear una issue para discutir cambios

Hacer fork del repositorio

Crear rama descriptiva: feature/nueva-feature o fix/bug-description

Implementar cambios con tests

Ejecutar pruebas: make test

Crear Pull Request

### 3. Convenciones de Código
Zig
Seguir el estilo oficial de Zig

Usar zig fmt antes de commitear

Documentar funciones públicas con comentarios triple-slash

Nombres descriptivos en inglés

ACERO
Palabras clave en inglés

API consistente con la filosofía del lenguaje

Documentación en docs/

### 4. Áreas de Contribución
Prioridad Alta
Backend LLVM completo

Sistema de módulos

Compilación incremental

Librería estándar básica

Prioridad Media
Optimizaciones

Debug information

Cross-compilation

Package manager

Prioridad Baja
IDE integration

Profiling tools

Documentation website

### 5. Escribir Tests
```acero
// test/unit/nueva_feature.acero
import std.test;

fn test_nueva_funcionalidad() -> bool {
    test.assert(1 + 1 == 2, "Matemáticas básicas");
    return true;
}

// Test runner automático
fn main() -> i32 {
    if (!test_nueva_funcionalidad()) return 1;
    return 0;
}
```
### 6. Escribir Documentación
Especificación
Actualizar docs/SPEC.md para cambios en el lenguaje

Documentar nuevas características

Incluir ejemplos de uso

Comentarios de Código
```zig
/// Función que hace algo importante
/// # Parámetros
/// - `input`: Valor de entrada
/// # Retorna
/// Resultado procesado
/// # Ejemplo
/// ```acero
/// let result = funcion_ejemplo(42);
/// ```
pub fn funcion_ejemplo(input: i32) i32 {
    return input * 2;
}
```
### 7. Proceso de Code Review
Revisión de Código

¿Sigue las convenciones?

¿Tiene tests adecuados?

¿Documentación actualizada?

Pruebas

Todos los tests pasan

No hay regresiones

Benchmark performance

Aprobación

Al menos 1 maintainer debe aprobar

CI debe pasar

Documentación actualizada

8. Reportar Bugs
Usar el template de issue:

text
## Descripción
[Describir el bug]

## Pasos para Reproducir
1. [Primer paso]
2. [Segundo paso]
3. ...

## Comportamiento Esperado
[Lo que debería pasar]

## Comportamiento Actual
[Lo que realmente pasa]

## Ambiente
- ACERO versión: [ej. v0.1.0]
- Zig versión: [ej. 0.12.0]
- Sistema operativo: [ej. Ubuntu 22.04]
- LLVM versión: [ej. 15.0.0]

## Código de Ejemplo
```acero
// Código mínimo que reproduce el bug
text
```

### 9. Solicitar Características

Usar el template de feature request:
Problema
[Descripción del problema que resuelve]

Solución Propuesta
[Descripción de la solución]

Alternativas Consideradas
[Otras soluciones posibles]

Impacto
Compatibilidad hacia atrás: [Sí/No]

Complejidad: [Baja/Media/Alta]

Performance: [Mejora/Degrada/Neutral]

text

### 10. Comunidad

- **Discusiones**: GitHub Discussions
- **Chat**: Discord/Slack (pendiente)
- **Issues**: GitHub Issues
- **RFCs**: Pull Requests con tag "RFC"

## Reconocimiento

- Contributors list en README.md
- Mention en release notes
- Badges de contribución

## Licencia

Al contribuir, aceptas licenciar tu trabajo bajo la MIT License.

---

**¡Gracias por contribuir a ACERO!** 
