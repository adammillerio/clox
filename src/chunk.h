#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/* One-bye operation codes (opcode).
 *
 * Represents the type of instruction currently being implemented.
 */
typedef enum {
    OP_CONSTANT,       // Get a constant from the Chunk's constant pool
    OP_NIL,            // Push nil onto the value stack
    OP_TRUE,           // Push true onto the value stack
    OP_FALSE,          // Push false onto the value stack
    OP_POP,            // Remove the top value from the stack and discard
    OP_GET_LOCAL,      // Get the value from a defined local variable
    OP_SET_LOCAL,      // Set the value of a local variable
    OP_GET_GLOBAL,     // Get the value from a defined global variable
    OP_DEFINE_GLOBAL,  // Define a global variable
    OP_SET_GLOBAL,     // Set the value of a global variable
    OP_GET_UPVALUE,    // Get a captured local variable via it's upvalue
    OP_SET_UPVALUE,    // Set a captured local variable via it's upvalue
    OP_GET_PROPERTY,   // Get a property of an object or its instances
    OP_SET_PROPERTY,   // Set a field (stateful property) on an instance
    OP_GET_SUPER,      // Retrieve the superclass method for a super call
    OP_EQUAL,          // Binary equality (a == b)
    OP_GREATER,        // Binary greater (a > b)
    OP_LESS,           // Binary less (a < b)
    OP_ADD,            // Binary addition (2 + 2)
    OP_SUBTRACT,       // Binary subtraction (2 - 2)
    OP_MULTIPLY,       // Binary multiplication (2 * 2)
    OP_DIVIDE,         // Binary division (2 / 2)
    OP_NOT,            // Unary not (!a)
    OP_NEGATE,         // Unary negation (-a)
    OP_PRINT,          // Print result of expr to stdout
    OP_JUMP,           // "Jump" the instruction pointer forward to an offset
    OP_JUMP_IF_FALSE,  // Jump forward if the top value is false
    OP_LOOP,           // Jump backward unconditionally to an offset
    OP_CALL,           // Call a function with N arguments currently on the stack
    OP_INVOKE,         // Bind and immediately call an instance method on the stack
    OP_SUPER_INVOKE,   // Bind and immediately call a superclass method
    OP_CLOSURE,        // Create a closure to wrap a function on the stack
    OP_CLOSE_UPVALUE,  // Hoist a closed-over value off the stack onto the heap
    OP_RETURN,         // Return from the current function
    OP_CLASS,          // Define a new class
    OP_INHERIT,        // Define an inheritance between a class and it's superclass
    OP_METHOD,         // Define a new method on a class
} OpCode;

/* @struct Chunk
 * @brief A "chunk" of bytecode, which is a dynamic array of bytecode to be
 * executed.
 */
typedef struct {
    int count;             // Current number of bytes in array.
    int capacity;          // Max number of bytes in the array. The array will be
                           // grown if count + 1 > capacity
    uint8_t* code;         // Dynamic array holding the bytecode for this Chunk.
    int* lines;            // Dynamic array holding line numbers, the value in 
                           // each index is the line in which the current byte's 
                           // source code was encountered
    ValueArray constants;  // All constants stored for this Chunk.
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif
