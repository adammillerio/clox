// This is a "define guard", which prevents multiple inclusion by wrapping the
// include itself in a preprocessor directive. All header files should have this.
// See: https://en.wikipedia.org/wiki/Include_guard
// See: https://google.github.io/styleguide/cppguide.html#The__define_Guard
#ifndef clox_common_h
#define clox_common_h

// C99 Boolean bool
#include <stdbool.h>
// NULL pointer constant, size_t (unsigned int of machine word size aka sizeof)
#include <stddef.h>
// Explicit sized integer types ie uint8_t (8-bit unsigned int)
#include <stdint.h>

// Allows for pass; instead of just ; on no-op lines
#define pass

// Use NaN boxing for storing Values
#define NAN_BOXING

// Compiler Debug: Print the bytecode for a Chunk after it is compiled
// Uncomment to enable
// #define DEBUG_PRINT_CODE

// VM Debug: Disassemble and print each instruction before executing it
// Uncomment to enable
// #define DEBUG_TRACE_EXECUTION

// GC Debug: Trigger garbage collection on every new allocation, regardless of
// the current threshold
// #define DEBUG_STRESS_GC

// GC Debug: Log all GC cycles and actions taken during them
// #define DEBUG_LOG_GC

// Maximum count of elements that can be stored in a uint8_t indexed array, such
// as the Locals array
#define UINT8_COUNT (UINT8_MAX + 1)

#endif
