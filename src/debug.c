#include "debug.h"

#include <stdio.h>

#include "object.h"
#include "value.h"

/* Disassemble a chunk of bytecode.
 *
 * This will disassemble each individual instruction in the Chunk's bytecode
 * array, displaying a human-readable representation of the encoded operation.
 *
 * @param chunk pointer to bytecode Chunk
 * @param name display name of this Chunk, displayed as a separator between
 * Chunks
 */
void disassembleChunk(Chunk* chunk, const char* name) {
    // == test chunk ==
    // 0000  123 OP_CONSTANT         0 '1.2'
    // 0002    | OP_RETURN
    printf("== %s ==\n", name);

    // Disassemble each instruction in the Chunk. The increment happens inside
    // the for loop since instructions have variable sizes
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

/* Disassemble a constant instruction.
 *
 * This retrieves the operand immediately after the opcode, which is the index
 * of the constant value in the constant pool, and displays the retrieved value.
 *
 * @param name name of the constant instruction
 * @param chunk pointer to the Chunk the instruction is in
 * @param offset byte offset of the OP_CONSTANT opcode in the code array
 *
 * @return byte offset of the next opcode to disassemble
 */
static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    // OP_CONSTANT 0 '1.2'
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");

    // Return next offset, after OP_CONSTANT opcode and index operand
    return offset + 2;
}

static int invokeInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    uint8_t argCount = chunk->code[offset + 2];

    printf("%-16s (%d args) %4d '", name, argCount, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");

    return offset + 3;
}

/* Disassemble a simple instruction like return.
 *
 * @param name name of the simple instruction
 * @param offset current byte offset in the chunk
 *
 * @return the next byte offset after this one
 */
static int simpleInstruction(const char* name, int offset) {
    // OP_RETURN
    printf("%s\n", name);
    return offset + 1;
}

/* Disassemble an instruction with a single operand.
 *
 * This displays the instruction and the one byte operand's value.
 *
 * @param name instruction name
 * @param chunk bytecode chunk being disassembled
 * @param offset current byte offset in the chunk
 *
 * @return the next byte offset, after the instruction and one operand
 */
static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    // 0002    | OP_GET_LOCAL        0
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

/* Disassemble a jump instruction.
 *
 * This displays the instruction as well as the source and destination values
 * for the jump in the bytecode.
 *
 * @param name instruction name
 * @param sign Positive (1) or negative (-1) jump
 * @param chunk bytecode chunk being disassembled
 * @param offset current byte offset in the chunk
 *
 * @return the next byte offset, after the instruction and two operands
 */
static int jumpInstruction(const char* name, int sign, Chunk* chunk,
                           int offset) {
    // 0007    | OP_JUMP_IF_FALSE    7 -> 31
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);

    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);

    return offset + 3;
}

/* Disassemble a bytecode instruction.
 *
 * @param chunk pointer to bytecode Chunk
 * @param offset byte offset in the Chunk's code array of the next instruction
 * to disassemble
 *
 * @return the byte offset of the next instruction
 */
int disassembleInstruction(Chunk* chunk, int offset) {
    // Print the byte offset of this instruction
    // 0000
    printf("%04d ", offset);
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        // Instruction is from same source line as the last, print continuation
        // 0000  123...
        // 0002    |...
        printf("   | ");
    } else {
        // Print line number
        printf("%4d ", chunk->lines[offset]);
    }

    // Read the first byte at the given offset (opcode)
    uint8_t instruction = chunk->code[offset];

    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_NIL:
            return simpleInstruction("OP_NIL", offset);
        case OP_TRUE:
            return simpleInstruction("OP_TRUE", offset);
        case OP_FALSE:
            return simpleInstruction("OP_FALSE", offset);
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        case OP_GET_LOCAL:
            return byteInstruction("OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return byteInstruction("OP_SET_LOCAL", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case OP_DEFINE_GLOBAL:
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case OP_GET_UPVALUE:
            return byteInstruction("OP_GET_UPVALUE", chunk, offset);
        case OP_SET_UPVALUE:
            return byteInstruction("OP_SET_UPVALUE", chunk, offset);
        case OP_GET_PROPERTY:
            return constantInstruction("OP_GET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY:
            return constantInstruction("OP_SET_PROPERTY", chunk, offset);
        case OP_GET_SUPER:
            return constantInstruction("OP_GET_SUPER", chunk, offset);
        case OP_EQUAL:
            return simpleInstruction("OP_EQUAL", offset);
        case OP_GREATER:
            return simpleInstruction("OP_GREATER", offset);
        case OP_LESS:
            return simpleInstruction("OP_LESS", offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NOT:
            return simpleInstruction("OP_NOT", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case OP_JUMP:
            return jumpInstruction("OP_JUMP", 1, chunk, offset);
        case OP_JUMP_IF_FALSE:
            return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
        case OP_LOOP:
            return jumpInstruction("OP_LOOP", -1, chunk, offset);
        case OP_CALL:
            return byteInstruction("OP_CALL", chunk, offset);
        case OP_INVOKE:
            return invokeInstruction("OP_INVOKE", chunk, offset);
        case OP_SUPER_INVOKE:
            return invokeInstruction("OP_SUPER_INVOKE", chunk, offset);
        case OP_CLOSURE: {
            // fun outer() {
            //     var a = 1;
            //     var b = 2;
            //     fun middle() {
            //         var c = 3;
            //         var d = 4;
            //         fun inner() {
            //               print a + c + b + d;
            //         }
            //     }
            // }
            // 0004    9 OP_CLOSURE          2 <fn inner>
            // 0006      |                     upvalue 0
            // 0008      |                     local 1
            // 0010      |                     upvalue 1
            // 0012      |                     local 2
            offset++;
            uint8_t constant = chunk->code[offset++];

            printf("%-16s %4d ", "OP_CLOSURE", constant);
            printValue(chunk->constants.values[constant]);
            printf("\n");

            ObjFunction* function =
                AS_FUNCTION(chunk->constants.values[constant]);
            for (int j = 0; j < function->upvalueCount; j++) {
                int isLocal = chunk->code[offset++];
                int index = chunk->code[offset++];
                printf("%04d      |                     %s %d\n", offset - 2,
                       isLocal ? "local" : "upvalue", index);
            }

            return offset;
        }
        case OP_CLOSE_UPVALUE:
            return simpleInstruction("OP_CLOSE_UPVALUE", offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        case OP_CLASS:
            return constantInstruction("OP_CLASS", chunk, offset);
        case OP_INHERIT:
            return simpleInstruction("OP_INHERIT", offset);
        case OP_METHOD:
            return constantInstruction("OP_METHOD", chunk, offset);
        default:
            // Unknown opcode, print error and move to next byte
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}
