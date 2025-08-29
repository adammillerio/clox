#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/* Lox Parser.
 *
 * This processes Tokens generated while scanning a Lox source and "emits" the
 * corresponding bytecode instructions to be consumed by the Lox Interpreter.
 */
typedef struct {
    Token current;   // The Token currently being processed.
    Token previous;  // The previously processed token, which is referenced for
                     // things like binary operations.
    bool hadError;   // Whether or not an error was encountered while compiling
                     // the current Chunk.
    bool panicMode;  // Whether or not the Parser has encountered an error and
                     // is in "panic mode". In panic mode, the Parser will
                     // continue to process Tokens but will not report
                     // additional errors.
} Parser;

/* Operation Precedence
 *
 * Lox uses a type of Operation-precedence parser called a Pratt Parser
 *
 * This enum has all of the precedence levels in Lox, from lowest to highest,
 * which are used in conjunction with the ParseRule table and parsePrecedence
 * to ensure that for example, -(1 + 1) is parsed differently than -1 + 1
 *
 * See: https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing
 */
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY      // 12345
} Precedence;

// Type alias for a function that takes a bool and returns nothing
// Python: Callable[bool, None]
typedef void (*ParseFn)(bool canAssign);

/* Parse Precedence rule.
 *
 * This represents the parsing rules for a given Token. These are used to define
 * the rules[] table below for the parsePrecedence() function.
 */
typedef struct {
    ParseFn prefix;         // Pointer to the parsing function to use for prefix
                            // expressions containing this token type, if any.
    ParseFn infix;          // Pointer to the parsing function to use for infix
                            // expressions containing this token type, if any.
    Precedence precedence;  // Parsing precedence of this token type, when
                            // parsing the right operand of a prefix/infix
                            // expression, the parsePrecedence() function will
                            // use this rule to ensure that only tokens of equal
                            // or lower precedence are matched.
} ParseRule;

/* A Local variable.
 *
 * This represents a locally defined variable encountered during compilation.
 */
typedef struct {
    Token name;       // Variable name, used during resolution
    int depth;        // Compiler scopeDepth that this Local was declared in
    bool isCaptured;  // Whether or not this local is captured in an Upvalue for
                      // a Closure
} Local;

/* An "upvalue" representing a nonlocal variable captured for use by a function.
 *
 * When a function references variables which are neither global nor local to
 * the function itself, they will be "captured" in an Upvalue structure that is
 * emitted alongside a Closure which will place these values onto the stack
 * again prior to function execution.
 */
typedef struct {
    uint8_t index;  // Index of the local slot in the enclosing function
    bool isLocal;   // Whether or not this captured upvalue is local to the
                    // enclosing (calling) function, or one if it's upvalues
} Upvalue;

/* Function types.
 *
 * This is used to control type specific behavior in call related operations.
 */
typedef enum {
    TYPE_FUNCTION,     // Lox function
    TYPE_INITIALIZER,  // Class initializer
    TYPE_METHOD,       // An instance-bound class method
    TYPE_SCRIPT,       // Top-level code represented as the first call
} FunctionType;

/* Struct to hold information about the current variable scope.
 */
typedef struct Compiler {        // Forward declaration to avoid incomplete type
                                 // errors when working with nested compilers
    struct Compiler* enclosing;  // The "enclosing" compiler, which is the
                                 // function above this one on the call stack
                                 // which called this one
    ObjFunction* function;       // Function being compiled
    FunctionType type;  // Whether this is a FUNCTION or SCRIPT (top-level code)

    Local locals[UINT8_COUNT];  // Array to hold Local variables
    int localCount;             // Current number of locals in the array
    Upvalue
        upvalues[UINT8_COUNT];  // Array to hold "nonlocal" or upvalue variables
    int scopeDepth;             // Number of blocks surrounding the currently
                                // compiled code
} Compiler;

/* Struct to hold information about the current class being defined, if any.
 */
typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;  // Enclosing class definition, if any
    bool hasSuperclass;  // Whether or not the class being compiled has a
                         // superclass
} ClassCompiler;

// Lox Parser "Singleton" global variable.
// All functions in this module will reference this global Parser.
Parser parser;

// Compiler state for the code currently being compiled
// All functions in this module will reference this global Compiler.
Compiler* current = NULL;

// Compiler state for the class currently being defined
// All functions in this module will reference this global ClassCompiler.
ClassCompiler* currentClass = NULL;

/* Retrieve the current bytecode Chunk being compiled.
 *
 * @return pointer to the currently compiling Chunk
 */
static Chunk* currentChunk() { return &current->function->chunk; }

/* Report an error at a specific Token in a Lox source.
 *
 * After the Parser encounters an error, it will enable "panic mode" and will
 * not report further errors until it can sychronize to a known good state.
 *
 * Additionally, the hadError flag will be set, indicating that the provided Lox
 * source has compilation errors and ending execution.
 *
 * @param token the Token where the error was encountered.
 * @param message error message to report to the user
 */
static void errorAt(Token* token, const char* message) {
    // Already in panic mode, don't report further errors
    if (parser.panicMode) return;
    // Enable panic mode
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
        pass
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

/* Report an error at the previously parsed Token.
 *
 * @param message error message to report to the user
 */
static void error(const char* message) { errorAt(&parser.previous, message); }

/* Report an error at the Token currently being parsed.
 *
 * @param message error message to report to the user
 */
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

/* Advance to the next Token.
 *
 * This stores a reference to the previously parsed Token at Parser.previous and
 * calls scanToken() to instruct the Lox Scanner to scan and return the next
 * Token from the provided Lox source, which is stored at Parser.current
 */
static void advance() {
    // Store the previous token, so that it's lexeme can be accessed during
    // parsing
    parser.previous = parser.current;

    for (;;) {
        // Scan the next Token
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        // Scanned token is an Error token, report error and continue to next
        // iteration until a non-error token is scanned
        errorAtCurrent(parser.current.start);
    }
}

/* Consume a specific type of Token, erroring if not found.
 *
 * The parser advances to the next Token, "consuming" the returned Token. This
 * is used for things ie groupings where "( expression" must have a
 * corresponding ")".
 *
 * @param type type of Token to match and consume.
 * @param message error message to report to the user if a Token is not matched
 */
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

/* Check if the current Token is of the provided type.
 *
 * @return whether or not the Token is of the provided type
 */
static bool check(TokenType type) { return parser.current.type == type; }

/* Check for a given TokenType and advance if present.
 *
 * @return whether or not the Token is of the provided type
 */
static bool match(TokenType type) {
    if (!check(type)) return false;

    advance();

    return true;
}

/* Emit a byte of bytecode.
 *
 * This will write the provided byte to the current bytecode Chunk, including
 * the previously parsed Token's line location in the source for error handling.
 *
 * This is used for bytecode instructions with no operands ie OP_NEGATE
 *
 * @param byte the byte of code to write to the current Chunk
 */
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

/* Emit two bytes of bytecode.
 *
 * This is used for bytecode instructions which have operands, such as
 * OP_CONSTANT, where the operand is the index of the constant value in the
 * Chunk's constant pool.
 *
 * @param byte2 first byte of code to write, usually the instruction
 * @param byte2 second byte of code to write, usually the operand
 */
static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

/* Emit a loop instruction
 *
 * This emits an OP_LOOP instruction for an unconditional jump backwards in the
 * current bytecode chunk. Since this is a backwards jump, the offset is emitted
 * with the correct value instead of being patched afterwards via patchJump.
 *
 * Functionally there is no difference between OP_LOOP and OP_JUMP on the VM
 * side, however they are split for easy identification.
 *
 * @param loopStart Offset in the current chunk where the loop starts, this is
 *        used as the jump offset in the instruction
 */
static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    // Emit the backwards jump and the calculated offset
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    // See patchJump for info on offset writing/reading
    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

/* Emit a jump instruction
 *
 * This emits an instruction for a forward jump in the current bytecode chunk.
 * It can either be unconditional (OP_JUMP) or based on the top value of the
 * stack (OP_JUMP_IF_FALSE).
 *
 * Because the instruction is emitted before the jump point is determined, the
 * offset is set to UINT16_MAX, which should be adjusted to the correct offset
 * later in compilation via patchJump()
 *
 * @param instruction Which type of jump to emit, either OP_JUMP or
 * OP_JUMP_IF_FALSE
 *
 * @return Offset in current bytecode chunk of the emitted instruction, minus
 * the two operands, to be used in a call to patchJump()
 */
static int emitJump(uint8_t instruction) {
    emitByte(instruction);

    // Emit two bytes, to be set to the correct offset via patchJump
    emitByte(0xff);
    emitByte(0xff);

    // Return code offset, minus the two operands
    return currentChunk()->count - 2;
}

/* Emit an implicit return operation to the current Chunk.
 *
 * For explicit return statements, see returnStatement()
 */
static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        // Class initializer, (always) implicitly return the new instance
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        // Implicit nil return at end of function
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

/* Add a constant to the current Chunk.
 *
 * @param value Lox value to write to the constant pool
 *
 * @return Index of the stored constant in the Chunk's constant pool.
 */
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);

    // Index is > 255
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

/* Emit a new constant instruction to the current Chunk.
 *
 * This stores the provided Lox value in the Chunk's constant pool and emits an
 * OP_CONSTANT instruction with the index. This is used by the interpreter to
 * put the constant value on the VM's value stack during operations.
 *
 * @param value Lox value to write to the constant pool and emit for
 */
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

/* Patch a previously emitted forward jump instruction.
 *
 * This will update the operands of the jump at the given offset so that it will
 * jump "over" a block of bytecode to the desired offset.
 *
 * @param offset Location of the instruction to patch emitted by emitJump
 */
static void patchJump(int offset) {
    // -2 to adjust for the operands of the jump
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    // Patch the operands to the jump to the correct 16-bit offset
    // The first int is shifted 8 bits right, leaving:
    //
    // Which is then binary ORed with the other int to get the full value:
    //   01001101 10110011
    //   00000000 01001101
    // & 11111111 11111111
    //   00000000 01001101
    // This is read back in the VM via the READ_SHORT macro
    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

/* Initialize the Compiler.
 *
 * This resets the provided Compiler, setting the count and depth to 0. It also
 * stores a link to the Compiler for the function or top-level code which called
 * (and thus triggered compilation of) this function.
 *
 * @param compiler pointer to a Compiler struct
 * @param type Whether this is a function or top level code
 */
static void initCompiler(Compiler* compiler, FunctionType type) {
    // Store the link to the enclosing (calling) function's Compiler
    compiler->enclosing = current;
    // Null and allocate new function object, the null is to avoid GC issues
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();

    current = compiler;

    // Capture the function name unless this is top-level code, which just has
    // the arbitrary name <script>
    if (type != TYPE_SCRIPT) {
        // Copy the previously scanned lexeme (the function name), and store it
        // in the new Compiler. The source is ultimately freed so this must be
        // a copy.
        current->function->name =
            copyString(parser.previous.start, parser.previous.length);
    }

    // Reserve the stack slot 0 for internal use by the VM
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;

    if (type != TYPE_FUNCTION) {
        // Class method call, slot 0 will store a reference to the instance
        // the method is being invoked on. Declare it as a named local variable
        // "this", allowing all compiled references to the instance to be
        // treated like a normal local variable
        local->name.start = "this";
        local->name.length = 4;
    } else {
        // Function call, slot 0 will store a reference to the ObjFunction
        // an ObjClosure represents. No identifier is needed since it will
        // not be referenced by the user
        local->name.start = "";
        local->name.length = 0;
    }
}

/* End compilation of the current Chunk.
 *
 *
 * @return Function object containing the compiled bytecode Chunk.
 */
static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL
                                             ? function->name->chars
                                             : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

/* Begin a new local scope, incrementing the current depth counter by 1. */
static void beginScope() { current->scopeDepth++; }

/* End the current local scope, decrementing the current depth counter by 1. */
static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth >
               current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            // Close upvalue capturing this local, returning it to the heap (if
            // there are no more references)
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            // Remove the local from the stack and discard it
            emitByte(OP_POP);
        }

        current->localCount--;
    }
}

// Forward declarations to avoid issues when referencing these functions in the
// body of parser functions as well as the ParseRule table itself
static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/* Store the identifier of a Lox value on the heap in the constant table.
 *
 * When retrieving or setting a constant such as a global variable, this will
 * store the string identifier of it (ie the variable name) as a constant in
 * the constant table, and return the index, which will be emitted as the
 * argument to a OP_SET_GLOBAL/OP_GET_GLOBAL instruction for retrieval by the
 * VM.
 *
 * @param name Token whose lexeme identifies a variable
 *
 * @return index of the stored identifier constant
 */
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

/* Check if two variable identifiers are equal.
 *
 * @param a Token representing identifier a
 * @param b Token representing identifier b
 * @return result Whether or not both identifiers reference the same variable
 */
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

/* Resolve a local variable referenced during compilation.
 *
 * @param compiler pointer to compiler state
 * @param token the Token identifying the local variable
 *
 * @return index in the Compiler locals of this variable, or -1 if not found
 */
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                // var a = a; <- Error
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    // No local variable defined in any scope with this name
    return -1;
}

/* Add a resolved and "captured" nonlocal/upvalue to a function.
 *
 * This will register the variable in the nonlocals/upvalues  collection for the
 * current scope. This is used to compile the closure which encapsulates the
 * function itself and all of the local/nonlocal variables it uses.
 *
 * @param compiler pointer to compiler state
 * @param index Slot index of the nonlocal in the enclosing function's stack,
 * this is the operand to the GET/SET_UPVALUE instructions for using it
 * @param isLocal Whether or not this captured upvalue is local to the enclosing
 * (calling) function, or one of it's upvalues
 * @return index of the created nonlocal/upvalue in this function's array
 */
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    // Increment this function's nonlocal count
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            // nonlocal referenced earlier, return previously stored index
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;

    return compiler->function->upvalueCount++;
}

/* Recursively resolve a nonlocal/upvalue corresponding to a given name.
 *
 * When a variable is referenced in code and there is no local variable
 * corresponding to it, this will recursively resolve an "upvalue" or nonlocal
 * value with this name in all enclosing scopes. This "flattens" the reference
 * chain for nonlocal values and ensures that variables can still be correctly
 * referenced on the stack in complex closure scenarios.
 *
 * @param compiler pointer to compiler state
 * @param name Token encountered during declaration
 * @return index of the created nonlocal/upvalue in this function's array
 */
static int resolveUpvalue(Compiler* compiler, Token* name) {
    // fun outer() {
    //     var x = "value";
    //     fun middle() {
    //         fun inner() {
    //           print x;
    //         }
    //     print "create inner closure";
    //     return inner;
    //     }
    //     print "return from outer";
    //     return middle;
    // }
    // var mid = outer();
    // var in = mid();
    // in();
    // Top level code, no enclosing scopes for upvalues to be in
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        // There is a "nonlocal" local in the enclosing scope that is captured
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    // Recursively check for a nonlocal with this name
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        // An enclosing scope has a captured nonlocal/upvalue with this name,
        // meaning that there is a local in that scope that the one enclosing
        // this function has captured
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    // No local with this name in any scope, probably* global (*at runtime)
    return -1;
}

/* Add a variable to the local scope.
 *
 * This will register the variable in the locals collection for the current
 * scope.
 *
 * @param name Token encountered during declaration
 */
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    // Initialize the next available Local in the Compiler's array
    Local* local = &current->locals[current->localCount++];

    // Store the name and the sentinel value -1 to indicate it is uninitialized
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

/* Declare a variable in the current scope. */
static void declareVariable() {
    // Globals are stored on the stack and there are no enclosing scopes
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;

    // Search backwards in the locals array until a variable that is outside of
    // the local scope to detect duplicate local variable declarations
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        // Check if a local with this identifier exists in this scope or ones
        // enclosing it
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

/* Parse a variable expression.
 *
 * This parses an identifier Token for the variable name and stores it in the
 * constant table, emitting an instruction for setting/getting the value at
 * runtime by the VM. This is done in order to avoid having to supply the string
 * itself as an argument to the bytecode instruction, which only accepts a
 * uint8_t.
 *
 * For constants, an OP_DEFINE_GLOBAL instruction is emitted which contains the
 * index of the constant in the globals for the current chunk. This is then
 * accessed via an OP_CONSTANT with the same index.
 *
 * For locals, the scope and all enclosing scopes are checked for an existing
 * var with the same name. Then, the value is stored in the locals for the
 * current scope. Local variables are not stored in the chunk during compilation
 * like constants, and are instead loaded at runtime from the Compiler state via
 * the OP_GET_LOCAL instruction.
 *
 * @param errorMessage error message to display if an identifier is not found
 *
 * @return index of the stored identifier constant
 */
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    // Declare variable in the global scope
    declareVariable();
    if (current->scopeDepth > 0) return 0;

    // Store constant identifier on the stack
    return identifierConstant(&parser.previous);
}

/* Initialize the most recently defined local variable.
 *
 * This records the scope depth of the local variable most recently defined with
 * defineVariable().
 */
static void markInitialized() {
    // Top-level function definition bound to a global
    if (current->scopeDepth == 0) return;

    // Initialize the most recently defined local variable, setting it's scope
    // depth to the current scope
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/* Define a global variable.
 *
 * Emit a bytecode instruction for defining a global variable at runtime, with
 * the index of the identifier for the value in the constant table as an
 * argument to the instruction.
 *
 * @param global index of the identifier constant in the constant table
 */
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/* Parse a list of arguments.
 *
 * This will work through arguments enclosed in (), such as for function and
 * constructor calls, evaluating each expression.
 */
static uint8_t argumentList() {
    uint8_t argCount = 0;

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }

            argCount++;
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

/* Compile a logical AND expression.
 *
 * This is compiled after the left side has been evaluated and the result is on
 * the stack:
 *              V Here
 * (2 + 2 == 5) and true
 *
 * A forward jump is emitted that will occur if the left side value is false,
 * also known as "short circuiting".
 */
static void and_(bool canAssign) {
    // Jump to ::end:: if the value on the top of the stack is false
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    // No jump, evaluate right hand
    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    // ::end::
    // Short circuit, right hand skipped
    patchJump(endJump);
}

/* Compile a binary expression. */
static void binary(bool canAssign) {
    // Retrieve the operator type from the previously parsed Token
    TokenType operatorType = parser.previous.type;

    // Get the parse precedence for this operator type and parse the next
    // token at this precedence + 1
    // Since the left operand was parsed and added to the stack prior to the
    // binary operator being encountered, this will place the right operand on
    // the stack as well
    ParseRule* rule = getRule(operatorType);
    // Precedence + 1 is used since binary operations are left associative ie
    // 1 + 2 + 3 + 4 should be parsed as ((1 + 2) + 3) + 4, so this will parse
    // the right operand only, leaving the rest to further parsing, this applies
    // to multiple operations of the same type
    parsePrecedence((Precedence)(rule->precedence + 1));

    // Emit the bytecode for the operator type, which will take the right and
    // left operands off of the stack perform the operation, and put the result
    // back
    switch (operatorType) {
        case TOKEN_BANG_EQUAL:
            // a != b becomes !(a ==b)
            emitBytes(OP_EQUAL, OP_NOT);
            break;
        case TOKEN_EQUAL_EQUAL:
            // a == b
            emitByte(OP_EQUAL);
            break;
        case TOKEN_GREATER:
            // a > b
            emitByte(OP_GREATER);
            break;
        case TOKEN_GREATER_EQUAL:
            // a >= b becomes !(a < b)
            emitBytes(OP_LESS, OP_NOT);
            break;
        case TOKEN_LESS:
            // a < b
            emitByte(OP_LESS);
            break;
        case TOKEN_LESS_EQUAL:
            // a <= b becomes !(a > b)
            emitBytes(OP_GREATER, OP_NOT);
            break;
        case TOKEN_PLUS:
            emitByte(OP_ADD);
            break;
        case TOKEN_MINUS:
            emitByte(OP_SUBTRACT);
            break;
        case TOKEN_STAR:
            emitByte(OP_MULTIPLY);
            break;
        case TOKEN_SLASH:
            emitByte(OP_DIVIDE);
            break;
        default:
            return;  // Unreachable.
    }
}

/* Call a Lox function. */
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        // Set this property on an object
        // canAssign is checked here to ensure that any "dangling" invalid
        // assignment is not evaluated and leads to a compiler error ie:
        // a + b.c = 3 <- Invalid, not evaluated since canAssign is false
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        // Bound instance method call, emit an optimized "superinstruction"
        // which combines the most common case of accessing/binding and
        // immediately calling an instance method
        uint8_t argCount = argumentList();

        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

/* Parse a literal value.
 *
 * Literal values have their own dedicated opcode which pushes the respective
 * value onto the stack. Since there is only ever one instance of each literal,
 * this saves on bytes by avoiding an operand.
 *
 * @param canAssign unused
 */
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE:
            emitByte(OP_FALSE);
            break;
        case TOKEN_NIL:
            emitByte(OP_NIL);
            break;
        case TOKEN_TRUE:
            emitByte(OP_TRUE);
            break;
        default:
            return;  // Unreachable.
    }
}

/* Parse a grouping expression (2 + 2)
 *
 * The grouping itself is only for language semantics and does not generate
 * bytecode itself. Instead the inner expression is evaluated, which will
 * emit it.
 *
 * @param canAssign unused
 */
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/* Parse a number literal.
 *
 * This uses the C strtod function to parse the previous Token's lexeme as a
 * double, and then adds it to the constant pool, emitting the necessary
 * instructions for retrieval at runtime.
 *
 * @param canAssign unused
 */
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/* Compile an or statement.
 *
 * This is compiled after the left side has been evaluated and the result is on
 * the stack:
 *              V Here
 * (2 + 2 == 4) or true
 *
 * A forward jump is emitted that will occur if the left side value is false,
 * also known as "short circuiting".
 */
static void or_(bool canAssign) {
    // (2 + 2 == 4) or false
    // At the point where the or is reached, the result of the left hand
    // is on the stack, so jump over the right hand if true, or "short circuit"

    // Jump to ::else:: if the value on the top of the stack is false
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    // Jump to ::end:: which will not be jumped over if the value is true
    int endJump = emitJump(OP_JUMP);

    // ::else::
    patchJump(elseJump);

    // Remove the left hand and evaluate the right hand
    emitByte(OP_POP);
    parsePrecedence(PREC_OR);

    // ::end::
    // Short circuit
    patchJump(endJump);
}

/* Parse a string literal.
 *
 * This copies the string by value from the Lox source, removing the quotes, and
 * emits an OP_CONSTANT for accessing it at runtime.
 *
 * @param canAssign unused
 */
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(
        copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/* Parse a named variable.
 *
 * Store the name of a variable as a constant and emit an instruction for the
 * value of it to be set at runtime.
 *
 * @param name Token whose lexeme identifies a variable
 * @param canAssign whether or not value assignment is allowed at this point, if
 * this is false and an = is encountered, it will be ignored and trigger an
 * error. See parsePrecedence() for more info
 */
static void namedVariable(Token name, bool canAssign) {
    // Determine variable type and emit the corresponding instruction
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        // The variable is being set
        // foo.bar = "baz";
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        // The variable is being retrieved
        // print foo.bar;
        emitBytes(getOp, (uint8_t)arg);
    }
}

/* Parse a variable expression.
 *
 * @param canAssign whether or not value assignment is allowed at this point,
 * see namedVariable() for more info
 */
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

/* Create a synthetic token.
 *
 * Synthetic tokens are used in places where additional meaning is ascribed to
 * a variable at runtime which does not have a corresponding lexeme. For
 * example, super is a synthetic token since there is no lexeme with the word
 * "super" being compiled, vs something like "this"
 *
 * Since this is a token it is a C string rather than a heap allocated Lox
 * string, and is managed as such.
 *
 * @return Manually created Token instance
 */
static Token syntheticToken(const char* text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

/* Emit an instruction for accessing a superclass.
 *
 * This retrieves the synthetic token for super which points to the superclass
 * of the current class being compiled. In the common case, a special
 * OP_SUPER_INVOKE instruction is emitted which will both retrieve and invoke
 * the superclass method.
 *
 * In the uncommon case of retrieving the super reference without directly
 * using it, an OP_GET_SUPER instruction is emitted instead for retrieval.
 *
 * @param canAssign unused
 */
static void super_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class.");
    } else if (!currentClass->hasSuperclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TOKEN_DOT, "Expect '.' after 'super'.");
    consume(TOKEN_IDENTIFIER, "Expect superclass method name.");

    uint8_t name = identifierConstant(&parser.previous);

    // Get the receiving class instance of this super call
    namedVariable(syntheticToken("this"), false);

    if (match(TOKEN_LEFT_PAREN)) {
        // Optimized path for most common immediate super calls
        // super().foo()
        uint8_t argCount = argumentList();

        // Get the superclass variable for the receiving class and
        // emit an instruction for resolving and invoking it at runtime
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        // Unoptimized and unlikely path
        // var s = super(); s.foo()
        // Get the superclass variable for the receiving class and
        // emit an instruction for resolving it at runtime
        namedVariable(syntheticToken("super"), false);
        emitBytes(OP_GET_SUPER, name);
    }
}

/* Define a this reference as a local "variable".
 *
 * The this keyword, which is used to reference an instance within class
 * methods, is defined like any other local non-assignable variable. Connecting
 * it to the receiver happens in the VM at runtime.
 */
static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
}

/* Parse a unary operation.
 *
 * This parses the operand to the unary operation, adding the result to the
 * value stack. Then it emits the opcode corresponding to the unary
 * operation being performed, which will be applied to the value on the
 * stack.
 *
 * @param canAssign unused
 */
static void unary(bool canAssign) {
    // Retrieve the operator type from the previously parsed Token
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Operate on the output of the parsed operand above.
    switch (operatorType) {
        case TOKEN_BANG:
            // Not the top value on the stack and push it back
            emitByte(OP_NOT);
            break;
        case TOKEN_MINUS:
            // Negate the top value on the stack and push it back
            emitByte(OP_NEGATE);
            break;
        default:
            return;  // Unreachable.
    }
}

/* Parsing precedence table.
 *
 * Table of TokenType -> Precedence(infix, prefix, precedence) which is used
 * in conjunction with parsePrecedence to implement Lox's Pratt Parser.
 *
 * For example:
 * [TOKEN_MINUS] = {unary, binary, PREC_TERM}
 *
 * Means that when parsing the expression "-1 - 1", this rule will result in
 * the unary() function being called to handle "-1", and the binary()
 * function being called to subtract the "1" from it: > -1 - 1
 * == code ==
 * 0000    1 OP_CONSTANT         0 '1'
 * 0002    | OP_NEGATE
 * 0003    | OP_CONSTANT         1 '1'
 * 0005    | OP_SUBTRACT
 * 0006    2 OP_RETURN
 *
 * Which results in the correct evaluation at runtime:
 * 0000    1 OP_CONSTANT         0 '1'
 *           [ 1 ]
 * 0002    | OP_NEGATE
 *           [ -1 ]
 * 0003    | OP_CONSTANT         1 '1'
 *           [ -1 ][ 1 ]
 * 0005    | OP_SUBTRACT
 *           [ -2 ]
 * 0006    2 OP_RETURN
 *
 * When consuming the right operand to these expressions, only Tokens of the
 * PREC_TERM or lower are consumed: + - * / ! - . () and primary exprs
 * (12345), which handles the general PEMDAS precedence order and ensures
 * that an expression like "-(1 - 1)" is handled differently: > -(1 - 1)
 * == code ==
 * 0000    1 OP_CONSTANT         0 '1'
 * 0002    | OP_CONSTANT         1 '1'
 * 0004    | OP_SUBTRACT
 * 0005    | OP_NEGATE
 * 0006    2 OP_RETURN
 *
 * 0000    1 OP_CONSTANT         0 '1'
 *           [ 1 ]
 * 0002    | OP_CONSTANT         1 '1'
 *           [ 1 ][ 1 ]
 * 0004    | OP_SUBTRACT
 *           [ 0 ]
 * 0005    | OP_NEGATE
 *           [ -0 ]
 * 0006    2 OP_RETURN
 *
 * In this case, since groupings () are a lower precedence than MINUS, the
 * entire "(1 - 1)" is consumed as the right operand to the initial "-",
 * which internally evaluates the "1 - 1" expression.
 */
ParseRule rules[] = {
    // C99 "Designated Initializers", which allows for declaration of an
    // array's indices in any order
    // int a[6] = { [4] = 29, [2] = 15 };
    // is the same as
    // int a[6] = { 0, 0, 15, 0, 29, 0 };
    // https://gcc.gnu.org/onlinedocs/gcc/Designated-Inits.html
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

/* Parse a Token of a given Precedence or lower.
 *
 * This function is called when assembling an expression, after consuming a
 * Token that identifies it, such as the "-" in -1, or the "+" in 1 + 1
 *
 * First, the ParseRule is retrieved from the rules[] table for the previous
 * Token that was matched.
 *
 * If a prefix function is defined, it will be called. For example, in "-1",
 * the previous token is "-", which will call the unary() function, which
 * will itself parse the "1", resulting in an OP_CONSTANT and an OP_NEGATE
 * instruction.
 *
 * If an infix function is defined, it will also be called. For example, in
 * "-1 + 1", the previous token would be "+", which will call the binary()
 * function, which will itself parse the "1", resulting in an OP_CONSTANT
 * and OP_ADD after the two instructions yielded for the "-1"
 *
 * Both the prefix and infix rules can themselves call the parsePrecedence
 * function. The supplied Precedence with the rules[] table ensures that
 * expressions such as "-(1 + 1)" and "-1 + 1" yield different instructions.
 *
 * @param precedence only parse Tokens of equal or lesser precedence than
 * this, when parsing infix expressions
 */
static void parsePrecedence(Precedence precedence) {
    // Read the next token
    advance();

    // Look up the prefix parser for the read TokenType
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        // No parser, this TokenType as a prefix is a syntax error
        // For example: -1 is valid, +1 is not
        error("Expect expression.");
        return;
    }

    // Set a flag for whether or not value assignment can occur at this point
    // For example:
    // a * b = c + d
    // a * b is of PREC_FACTOR which is higher than PREC_ASSIGNMENT, meaning
    // that a * b is not a valid assignment target. This flag is passed on to
    // the parser function in order to contextually handle whether or not to
    // consume and interpret an = sign
    bool canAssign = precedence <= PREC_ASSIGNMENT;

    // Call the prefix parser function for this TokenType
    prefixRule(canAssign);

    // While the current token's type is of lesser or equal precedence,
    // continue to advance and consume. This will also stop on any token
    // that is not an infix operator (PREC_NONE).
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // The prefixRule was called with canAssign = false, so there is an
    // unconsumed = leftover, report error
    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

/* Retrieve the parsing rule for a given Token type.
 *
 * See the rules[] table and parsePrecedence for more details.
 *
 * @param type token type being parsed
 * @return the ParseRule for this token type
 */
static ParseRule* getRule(TokenType type) { return &rules[type]; }

/* Parse an expression.
 *
 * This parses at PREC_ASSIGNMENT, the highest defined (valid) precedence.
 */
static void expression() { parsePrecedence(PREC_ASSIGNMENT); }

/* Compile a block.
 *
 * This just compiles all statements within curly braces { }
 */
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/* Compile a Lox function.
 *
 * Each function is compiled using an independent Compiler instance which will
 * contain the bytecode for it. These are then assembled as frames in a call
 * stack to be evaluated at runtime.
 *
 * @param type Whether this is a function or top level code
 */
static void function(FunctionType type) {
    // Initialize compiler for this function's code
    Compiler compiler;
    initCompiler(&compiler, type);

    // Begin scope, no endScope() is needed as that is handled implicitly by the
    // call to endCompiler()
    beginScope();

    // () {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    // Parse and define all function parameters, incrementing the overall arity
    // of the function each time
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }

            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");

    // print 1 + 1; }
    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    // Emit a variable amount of bytes corresponding to the captured nonlocal
    // upvalues for this function and their locations on the stack once they
    // are "hoisted" back on to it from the heap at runtime
    for (int i = 0; i < function->upvalueCount; i++) {
        // Whether this upvalue is local to the enclosing function, or an
        // upvalue that it captured from another one, this determines which
        // collection is accessed with the index emitted
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

/* Compile a Lox Class method.
 *
 * The method body is compiled using function(). If this is the initializer for
 * the class, it will be of TYPE_INITIALIZER. Otherwise, it will be TYPE_METHOD.
 *
 * These are used along with the compiled function to wire up the necessary
 * references at runtime in the VM.
 */
static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");

    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 &&
        memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);

    emitBytes(OP_METHOD, constant);
}

/* Compile a Lox Class declaration.
 *
 * This will compile and create a new class, updating the ClassCompiler
 * reference, declaring and defining the class in the local scope along with
 * its methods.
 */
static void classDeclaration() {
    // Bar < Foo
    // Bar
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);

    // Declare and define the variable for the class from the parsed name, so
    // that it can be used in forward references
    declareVariable();
    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    // Set the current class reference to the newly defined one
    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name.");

        // Foo
        variable(false);

        // Foo < Foo <- Error
        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself.");
        }

        // Create a new scope, since super is unique to each class definition.
        beginScope();
        // Define the superclass currently on the stack as the local "variable"
        // super
        addLocal(syntheticToken("super"));
        defineVariable(0);

        // Load the inheriting class onto the stack, and emit an instruction to
        // set up the relation between them
        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    // Load the defined class variable back onto the stack for methods
    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    // Compile all class method definitions
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");

    // Pop the class back off of the stack
    emitByte(OP_POP);

    if (classCompiler.hasSuperclass) {
        // Close the scope for the synthetic "super" variable representing the
        // superclass of this class declaration, which is now in a scope that
        // is accessible by all methods of this class
        endScope();
    }

    // Return the current class reference to the enclosing class, if any
    currentClass = currentClass->enclosing;
}

/* Compile a Lox function declaration.
 *
 * This handles the variable assignment for the function. For the compilation of
 * function arguments and body, see function()
 */
static void funDeclaration() {
    //     V Here
    // fun foo() { print 1 + 1; }
    // Parse in the function declaration's name
    // foo
    uint8_t global = parseVariable("Expect function name.");

    // Immediately mark the variable representing the function as initialized
    // as functions refer to themselves during initialization in the case of
    // local recursion
    markInitialized();

    function(TYPE_FUNCTION);
    defineVariable(global);
}

/* Parse a variable declaration.
 *
 * This parses a global variable's name and value (defaulting to nil) and emits
 * the OP_CONSTANT and OP_DEFINE_GLOBAL instructions required to define it at
 * runtime.
 */
static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        // Desugar:
        // var a;
        // Into:
        // var a = nil;
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

/* Parse an expression statement.
 *
 * An expression statement is just an expression and a semicolon, which is used
 * in places like function calls and variable assignment:
 *
 * var bar = "baz"
 * foo(bar)
 *
 * The expression is parsed, and the resulting value placed on the stack is
 * immediately discarded via an OP_POP instruction.
 */
static void expressionStatement() {
    expression();

    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");

    emitByte(OP_POP);
}

/* Compile a for loop. */
static void forStatement() {
    // for (var i = 0; i <= 5; i = i + 1)
    // Start scope for the entire statement and body
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer (;
        pass
    } else if (match(TOKEN_VAR)) {
        // (var i = 0;
        varDeclaration();
    } else {
        // (i = 0;
        // expressionStatement() is used instead of expression() in order to
        // remove the result from the stack and to consume the semicolon
        expressionStatement();
    }

    // Get offset in current chunk for start of the loop
    int loopStart = currentChunk()->count;
    // ::start::
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        // i <= 5;
        // Evaluate condition expression, if any, leaving result on the stack
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump to ::exit:: if the condition is false, exiting the loop
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        // Remove condition result from the stack
        emitByte(OP_POP);
    }

    // consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    if (!match(TOKEN_RIGHT_PAREN)) {
        // Jump to ::body:: unconditionally
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        // ::increment::

        // Evaluate increment expression and remove result from the stack
        // i = i + 1;)
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // Jump to ::start:: and end this iteration of the loop
        emitLoop(loopStart);

        // Change loopStart to point to the start of the increment
        loopStart = incrementStart;

        // ::body::
        patchJump(bodyJump);
    }

    // Loop body
    statement();
    // Jump to ::increment:: to handle incrementing before ending this iteration
    emitLoop(loopStart);

    // ::exit:: is only emitted and patched if there is a condition
    if (exitJump != -1) {
        // ::exit::
        patchJump(exitJump);
        // Remove condition result from the stack
        emitByte(OP_POP);
    }

    endScope();
}

/* Compile an if statement. */
static void ifStatement() {
    // **Note:** The two OP_POP instructions are emitted explicitly here rather
    // than in the opcode implementation itself because when evaluating
    // expressions with logical operators, the value is still needed
    // @see //aemisc/clox/compiler:and_
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // Emit the jump instruction over the then block if the condition is false
    int thenJump = emitJump(OP_JUMP_IF_FALSE);

    // Begin then block

    // Remove condition from the stack and run the then block statement
    emitByte(OP_POP);
    statement();

    // Emit the jump instruction over the else block from the then block
    int elseJump = emitJump(OP_JUMP);

    // End then block

    // Begin else block

    // Update the jump offset over the then block and remove the condition
    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();

    // Update the jump offset over the else block
    patchJump(elseJump);

    // End else block
}

/* Parse a print statement.
 *
 * This parses an expresson and then emits an OP_PRINT instruction, which will
 * print the value on the stack to stdout at runtime.
 */
static void printStatement() {
    expression();

    consume(TOKEN_SEMICOLON, "Expect ';' after value.");

    emitByte(OP_PRINT);
}

/* Parse a return statement.
 *
 * This parses an explicit return statement. For implicit nil returns, either
 * as a statement or implicitly at the end of a function, see emitReturn()
 */
static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        // Implicit nil return
        // return;
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            // Implicit return value from a class initializer is always the
            // instance
            error("Can't return a value from an initializer.");
        }

        // Explicit return, compile expression and emit return
        // return 2 + 2;
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

/* Compile a while loop. */
static void whileStatement() {
    // Record the offset of the current instruction, used to set ::start::
    int loopStart = currentChunk()->count;
    // ::start::
    // Evaluate loop expression, leaving the result on the stack
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // Jump to ::exit:: if the expression evaluates to false
    int exitJump = emitJump(OP_JUMP_IF_FALSE);

    // Remove result and run loop body
    emitByte(OP_POP);
    statement();

    // Jump to ::start::
    emitLoop(loopStart);

    // ::exit::
    patchJump(exitJump);
    // Remove result and exit the loop
    emitByte(OP_POP);
}

/* Synchronize the compiler after an error.
 *
 * When an error is encountered and the panic flag is set, this will skip Tokens
 * until it encounters a semicolon or the beginning of another statement in
 * order to avoid cascading error reports to the user.
 */
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                pass;  // Do nothing.
        }

        advance();
    }
}

/* Parse a declaration. */
static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // Error encountered during last parse, attempt to synchronize to a good
    // state
    if (parser.panicMode) synchronize();
}

/* Parse a statement */
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

/* Compile a Lox source.
 *
 * @param source the lox source string
 */
ObjFunction* compile(const char* source) {
    initScanner(source);

    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

/* GC: Mark all roots in use by bytecode Compilers.
 *
 * This traverses the linked list of currently compiling functions and marks
 * them during a GC cycle.
 */
void markCompilerRoots() {
    Compiler* compiler = current;

    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
