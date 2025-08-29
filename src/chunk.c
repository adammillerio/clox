#include "chunk.h"

#include <stdlib.h>

#include "memory.h"
#include "vm.h"

/*
 * Create a new Chunk of bytecode.
 *
 * This is both a "Constructor" for new chunks and a way to reset after freeing
 * one in memory.
 *
 * @param chunk the chunk of bytecode to (re)initialize
 */
void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    // Init the dynamic array holding the constant pool for this chunk
    initValueArray(&chunk->constants);
}

/* Free a Chunk of bytecode.
 *
 * This frees the underlying array and initializes the Chunk back to default
 * values.
 *
 * @param chunk pointer to the Chunk to free
 */
void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    // Free the line number location array
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    // Free the dynamic array holding the constant pool for this chunk
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

/* Write a new byte to the end of a Chunk of bytecode.
 *
 * This adds the byte to the end of the Chunk's internal dynamic array, growing
 * it if more space is needed.
 *
 * @param chunk pointer to the Chunk to write to
 * @param byte data to write to the Chunk
 * @param line line number in the source where this byte was generated from,
 * this is stored in a parallel array and used during error handling
 */
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    // Dynamic array out of capacity, resize
    // With amortized analysis rather than worst-case analysis, this operation
    // is O(1) and not O(n), as it is only called when the array is out of
    // space, and it is always grown by a multiple of it's current size. This
    // also initializes the code[] array on first access
    // See: https://en.wikipedia.org/wiki/Amortized_analysis
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;

        chunk->capacity = GROW_CAPACITY(oldCapacity);
        // Grow the code and line arrays by the same amount
        chunk->code =
            GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines =
            GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    // Append byte to end of the array and increment
    chunk->code[chunk->count] = byte;
    // Add line location of the byte
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

/* Add a constant to the constant pool.
 *
 * Constants defined in a given Chunk are embedded in a secondary values array
 * called the "constant pool". They are then retieved via the OP_CONSTANT
 * instruction, which takes an index in the pool and returns the stored value.
 *
 * @param chunk pointer to the chunk to add a constant to
 * @param value lox constant value to store in the pool
 *
 * @return index of the inserted value in the pool array, used for retrieval
 */
int addConstant(Chunk* chunk, Value value) {
    // GC stack guard in case array is grown while adding the constant
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}
