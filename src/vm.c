#include "vm.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"  // IWYU pragma: keep
#include "compiler.h"
#include "debug.h"  // IWYU pragma: keep
#include "memory.h"
#include "object.h"

// Lox VM "Singleton" global variable.
// All functions in this module will reference this global VM.
VM vm;

/* Get the total elapsed time of this Lox script.
 *
 * This is a native C function which is made globally available as clock() when
 * the VM is initialized.
 *
 * @return Total execution time in seconds
 */
static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

/* Reset the VM's (up)value stack.
 *
 * This sets the stackTop pointer to the location of the 0th element in the
 * array, which indicates an empty stack. No other deallocation is needed, as
 * previous values will just be overwritten.
 */
static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

/* Report a runtime error.
 *
 * This is a variadic function which calls vfprintf with the provided formatter
 * and arguments, as well as the line where it was encountered.
 *
 * @param format format string with error message
 * @param *args arguments to vfprintf for formatting
 */
static void runtimeError(const char* format, ...) {
    // This is how you handle a variadic function call in C
    // Python:
    // def runtimeError(format: str, *args: Any) -> None:
    //     print(format, *args, file=sys.stderr)

    // "Start" variadic handler, the second argument to va_start is the last
    // named argument before the ..., in this case format. From here args will
    // contain all variadic args.
    va_list args;
    va_start(args, format);

    vfprintf(stderr, format, args);

    // "End" variadic handler
    va_end(args);

    fputs("\n", stderr);

    // Print stack trace from the previous (failed) instruction in each frame
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;

        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);

        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

/* Define a native C function.
 *
 * This stores the pointer to the function in the VM's global's by name, wrapped
 * in an ObjNative.
 *
 * @param name Name to identify this native function in Lox
 * @param function Pointer to implementing C function
 */
static void defineNative(const char* name, NativeFn function) {
    // Push native fun name and pointer on the stack to avoid unintended GC
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));

    // Set global variable for the ObjNative
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);

    // Remove from the stack
    pop();
    pop();
}

/* Initialize the Lox Virtual Machine. */
void initVM() {
    resetStack();

    // Initialize allocated object list
    vm.objects = NULL;

    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    // Initialize the global variable table
    initTable(&vm.globals);

    // Initialize the string interning table
    initTable(&vm.strings);

    // Intern the reserved init method string, initially setting it to null in
    // order to protect it from the GC during string allocation
    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    // Define native clock() C function
    defineNative("clock", clockNative);
}

/* Free the Lox Virtual Machine. */
void freeVM() {
    // Free the global variable table
    freeTable(&vm.globals);

    // Free the string interning table
    freeTable(&vm.strings);

    // Clear the init string
    vm.initString = NULL;

    // Free the allocated object list
    freeObjects();
}

/* Push a value onto the top of the VM's value stack.
 *
 * The value will be stored in the element currently pointed to by stackTop. The
 * stackTop pointer will then be incremented for the next push/pop operation.
 *
 * @param value the value to push onto the stack
 */
void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

/* Pop the value on the top of the VM's value stack.
 *
 * This decrements the stackTop pointer to the previous element and returns the
 * value which is stored there.
 *
 * @return the value popped from the stack
 */
Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

/* Retrieve a value down the stack without removing it.
 *
 * @param distance Distance down the stack to retrieve value
 *
 * @return value Retrieved value from the stack
 */
static Value peek(int distance) { return vm.stackTop[-1 - distance]; }

/* Call a Lox function
 *
 * This creates a new call frame for the function being called which is
 * enclosed by the calling function provided, updating the slots, function and
 * instruction pointers to the function being called.
 *
 * @param function Lox function being called
 * @param argCount Total arguments parsed prior to call, this is compared with
 * the arity of the function
 *
 * @return Execution result, this is false if an incorrect amount of arguments
 * is provided, or a stack overflow
 */
static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.",
                     closure->function->arity, argCount);

        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    // Create new call frame
    CallFrame* frame = &vm.frames[vm.frameCount++];

    // Set function reference and instruction pointer to the called function
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;

    // Update the slots pointer for the frame's window, subtracting 1 to account
    // for the VM's reserved stack slot 0:
    // return 4 + sum(5, 6, 7);
    //                      V stackTop - 3 - 1
    // (0) script 4 sum 5 6 7
    //              V Here
    // (0) script 4 sum 5 6 7
    // Giving a frame window from indexes 2-6 on the stack
    frame->slots = vm.stackTop - argCount - 1;

    return true;
}

/* Call a Lox value.
 *
 * This will dispatch to the correct handler function depending on the type of
 * callable Lox object it is.
 *
 * @param callee Lox value being called
 * @param argCount Total arguments parsed prior to call
 *
 * @return Execution result
 */
static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                // Create a new version of this class method bound to the
                // callee's instance and call it
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);

                // Set the reserved slot 0 for this stack window to the class
                // instance this method is bound to. This is defined as the
                // local pseudo-variable "this" in the compiler, which can
                // be referred to in the method body
                vm.stackTop[-argCount - 1] = bound->receiver;

                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                // Create a new instance of this class and place it back on the
                // stack
                ObjClass* klass = AS_CLASS(callee);

                // Set the reserved slot 0 for this stack window to the new
                // class instance being created. This can't be referred to by
                // the user and is mostly to keep it marked by the GC
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));

                // Call the class' initializer, if any
                Value initializer;
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    // Class has no initializer, but user provided arguments
                    // during construction
                    runtimeError("Expected 0 arguments but got %d.", argCount);

                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                // Call the native C function and push the result
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                // Non-callable object type
                break;
        }
    }

    runtimeError("Can only call functions and classes.");
    return false;
}

/* Invoke a bound method on a class instance.
 *
 * This will retrieve and call a method defined on a class bound to the instance
 * that is calling it.
 *
 * @param klass Lox class reference for method lookup
 * @param name Method name
 * @param argCount Arity of the class method for call
 *
 * @return True if the method was called without any runtime errors
 */
static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    return call(AS_CLOSURE(method), argCount);
}

/* Invoke a method on the class instance currently on the stack.
 *
 * @param name Method name
 * @param argCount Arity of the class method for call
 *
 * @return True if the method was called without any runtime errors
 */
static bool invoke(ObjString* name, int argCount) {
    // Get the instance off of the stack
    Value receiver = peek(argCount);

    // Stack value isn't a class instance
    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    // Cast the Value to an instance and directly call the method for it
    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        // The accessed value is a field and not a method, place it on the
        // stack similar to OP_GET_PROPERTY
        vm.stackTop[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

/* Bind a class method to an instance.
 *
 * @param klass Lox class
 * @param name Method name
 *
 * @return True if the method was called without any runtime errors
 */
static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

    pop();
    push(OBJ_VAL(bound));
    return true;
}

/* Capture a nonlocal Value as an Upvalue.
 *
 * Given a stack slot, this will create an Upvalue for itand add it to the VM's
 * collection. If there is already an ObjUpvalue capturing the provided Value,
 * it will be returned instead.
 *
 * @param local Pointer to the stack slot whose Value is being captured.
 *
 * @return New or existing ObjUpvalue representing this Value.
 */
static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;

    // Seek past all upvalues which point to stack locations below this one
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        // Already an existing ObjUpvalue for this local
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        // Add upvalue to end of list
        vm.openUpvalues = createdUpvalue;
    } else {
        // Insert upvalue into the middle of the list
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

/* Close a captured upvalue(s), moving the underlying captured Value to the heap
 *
 * @param last Pointer to the last stack slot to close, all upvalues that point
 * to this slot or one above it will be closed
 */
static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;

        // Move the variable referenced by this upvalue to the heap, closing it
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;

        vm.openUpvalues = upvalue->next;
    }
}

/* Define a method on a class.
 *
 * This is called after the OP_CLOSURE instruction which is used to define the
 * closure backing this method at runtime.
 *
 * After, this pops the defined method name and the class it is being defined on
 * off of the stack and defines it within the class' method table.
 *
 * @param name Name of the method being defined
 */
static void defineMethod(ObjString* name) {
    Value method = peek(0);

    ObjClass* klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);

    pop();
}

/* Return the "falsiness" of a given value.
 *
 * Falsiness in the case of Lox follows the Ruby pattern, where nil (null)
 * considered false, and all other values (other than boolean True) are false.
 *
 * @param value Lox Value instance to evaluate
 *
 * @return whether or not this value is "falsey", that is, it returns true if
 *         the value can be considered as boolean false.
 */
static bool isFalsey(Value value) {
    // nil is considered false, all other non-bool values are true
    // Python: value is None or (isinstance(value, bool) and !value)
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

/* Concatenate two strings.
 *
 * This pops the previous two string values off of the value stack and adds
 * them together, copying the full set of chars from each, then pushes the new
 * string back onto the stack.
 */
static void concatenate() {
    // Don't remove the strings yet so that the GC does not sweep them while
    // allocating the new string
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));

    // Get new length of string
    int length = a->length + b->length;

    // Allocate space for new length + null char
    char* chars = ALLOCATE(char, length + 1);

    // Copy string a's chars into the string from the start
    memcpy(chars, a->chars, a->length);
    // Copy string b's chars into the string after a's and add null char
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    // Allocate the Lox string instance and push it onto the value stack
    // takeString takes ownership of the chars string, rather than copying it
    // by value. The operands are then removed from the stack to be GC'd
    ObjString* result = takeString(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

/* Run the current Chunk of bytecode.
 *
 * This will run the Chunk currently stored at vm.chunk, executing all
 * bytecode instructions.
 *
 * Bytecode can represent both statements and expressions. Expressions modify
 * the stack, so their stack effect will always add up to 1 (the one result
 * value of the expression). Conversely, the stack effect of a statement will
 * always add up to 0, as the stack is unchanged after execution.
 *
 * For example in:
 * print "foo";
 *
 * The "foo" expression puts the string constant "foo" on the stack (+1), while
 * the print statement removes it and prints the value to stdout (-1),
 * ultimately providing the stack effect of 0.
 *
 * @return result the result of Chunk execution
 */
static InterpretResult run() {
    // Store a reference to the top frame in the call stack. This is stored
    // rather than using the array for clarity, and also to potentially have the
    // reference stored in a register at runtime
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

// Macro: Read the byte at the instruction pointer and increment it
#define READ_BYTE() (*frame->ip++)
// Read and build a 16-bit unsigned int using a two-byte operand from a chunk
// To combine, the first int is shifted 8 bits left, leaving:
// 01001101 00000000
// Which is then binary ORed with the other int to get the full value:
// 01001101 00000000
// |        10110011
// 01001101 10110011
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
// Macro: Read an index operand from the bytecode and retrieve the constant from
// the Chunk's constant pool
#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])
// Macro: Read a constant as a Lox string
#define READ_STRING() AS_STRING(READ_CONSTANT())
// Macro: Perform a binary operation on the last value in the stack. The
// arguments are the *_VAL macro for the value type, and the operator, both
// literally, which is mildly insane. The operation code itself is wrapped in
// a do while loop because it allows for a macro with multiple statements that
// also allows a semicolon at the end. (See: 15.3.1)
// This operation works backwards, removing the right operand from the stack
// before the left.
#define BINARY_OP(valueType, op)                          \
    do {                                                  \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers.");    \
            return INTERPRET_RUNTIME_ERROR;               \
        }                                                 \
        double b = AS_NUMBER(pop());                      \
        double a = AS_NUMBER(pop());                      \
        push(valueType(a op b));                          \
    } while (false)

    for (;;) {
// VM Debug: Disassemble and print instruction
// See common.h for definition of this flag
#ifdef DEBUG_TRACE_EXECUTION
        // Buffer to bring values in line with opcode
        printf("          ");
        // Print all values in the VM's value stack from bottom to top
        // 0000  123 OP_CONSTANT         0 '1.2'
        //           [ 1.2 ]
        // 0002    | OP_NEGATE
        //           [ -1.2 ]
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");

        // The instruction pointer is a direct pointer but
        // disassembleInstruction takes an integer offset, so this does pointer
        // math to convert the pointer to it's offset in the array. Subtracting
        // two pointers of the same data type will yield the distance between
        // them, using the size of the type.
        // See N1570 Section 6.5.6 "Additive Operators" Line 9
        // https://www.iso-9899.info/n1570.html#6.5.6
        disassembleInstruction(
            &frame->closure->function->chunk,
            (int)(frame->ip - frame->closure->function->chunk.code));
#endif

        // Read and dispatch the next instruction based on the opcode
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                // Read a constant value and push it onto the VM's value stack
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL:
                push(NIL_VAL);
                break;
            case OP_TRUE:
                push(BOOL_VAL(true));
                break;
            case OP_FALSE:
                push(BOOL_VAL(false));
                break;
            case OP_POP:
                // Remove the top value from the stack and discard
                pop();
                break;
            case OP_GET_LOCAL: {
                // Read the stack slot index from the opcode
                uint8_t slot = READ_BYTE();

                // Retrieve the local variable from it's slot in the current
                // frame's portion of the stack and push it onto the top, so
                // that it can be used by subsequent operations
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                // Read the stack slot index from the opcode
                uint8_t slot = READ_BYTE();

                // Update the local variable at the stack slot within the
                // current frame's portion of the stack with the value currently
                // on the top of the stack. This does not pop the value from the
                // stack since assignment is an expression that produces itself
                // as the value.
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                // Read the identifier for the global from the constant table
                ObjString* name = READ_STRING();

                // Load the runtime value for this global from the globals table
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Push the runtime value onto the stack
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                // Read the name of the global variable stored in the constant
                // table
                ObjString* name = READ_STRING();

                // Define a global variable with this name using the top value
                // on the stack
                tableSet(&vm.globals, name, peek(0));

                // Pop the value off of the stack and discard. This is done
                // after it is set in the table to ensure that it is not garbage
                // collected while still being set, ie if the table needs to
                // dynamically resize, which takes time
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();

                if (tableSet(&vm.globals, name, peek(0))) {
                    // No variable with this name declared, delete the stored
                    // value and error
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                // Push the Value referenced by this upvalue onto the stack
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                // Capture the location of the Value on the top of the stack
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                // foo.bar
                // var foo = "definitely foo"
                // foo.bar <- Invalid
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // foo
                ObjInstance* instance = AS_INSTANCE(peek(0));
                // "bar"
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    // Valid property, pop the instance currently on the top of
                    // the stack "before" the dot and push the retrieved
                    // property back on the stack
                    pop();
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_PROPERTY: {
                // foo.bar = "baz"
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // foo
                ObjInstance* instance = AS_INSTANCE(peek(1));
                // "baz" <- Removed from top of stack, set as bar on foo
                // No check needed since fields are implicitly set like in other
                // dynamic langs, to the chagrin of your local type checker
                tableSet(&instance->fields, READ_STRING(), peek(0));

                // Pop "baz" back off the stack, then the instance, then put
                // it back so that it is the result of the set:
                // print foo.bar = "baz";
                // "baz"
                Value value = pop();
                pop();
                push(value);
                break;
            }
            case OP_GET_SUPER: {
                // Get the method name and superclass reference on the
                // stack
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop());

                // Bind the method from the superclass with this name
                if (!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_EQUAL: {
                // Binary equality (a == b)
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:
                // Binary greater [a > b]
                BINARY_OP(BOOL_VAL, >);
                break;
            case OP_LESS:
                // Binary less (a < b)
                BINARY_OP(BOOL_VAL, <);
                break;
            case OP_ADD: {
                // Binary addition
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    // "foo" + "bar"
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    // 2 + 2
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());

                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError(
                        "Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                break;
            }
            case OP_SUBTRACT:
                // Binary subtraction
                BINARY_OP(NUMBER_VAL, -);
                break;
            case OP_MULTIPLY:
                // Binary multiplication
                BINARY_OP(NUMBER_VAL, *);
                break;
            case OP_DIVIDE:
                // Binary division
                BINARY_OP(NUMBER_VAL, /);
                break;
            case OP_NOT:
                push(BOOL_VAL(isFalsey(pop())));
                break;
            case OP_NEGATE:
                // Value is not a number
                // -false; <- Runtime error
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Unary negation: Negate the last value on the stack and
                // push it back var a = 1; -a == -1
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            case OP_PRINT: {
                // Take the top value off of the stack (the result of the
                // last expression), and print it to stdout
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                // Jump forwards to instruction offset unconditionally
                uint16_t offset = READ_SHORT();

                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                // Read the two-byte jump offset
                uint16_t offset = READ_SHORT();

                // If the top value on the stack is falsey, jump to the
                // instruction at the offset, which is the end of the then
                // block
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                // Jump backwards to instruction offset unconditionally
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                // Retrieve the Function object from argCount down the
                // stack, past all arguments, and call it
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Update local reference to the current frame, which is now
                // the frame for the called function. The interpreter will
                // now reference the frame's instruction pointer to the code
                // for the function
                frame = &vm.frames[vm.frameCount - 1];

                break;
            }
            case OP_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();

                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Update reference to the current frame
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_SUPER_INVOKE: {
                ObjString* method = READ_STRING();

                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop());

                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                // Wrap the function itself in a closure for this call and
                // push it onto the stack
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);

                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();

                    if (isLocal) {
                        // Capture the local at the index in the calling
                        // function (the one defining this closure) frame's
                        // stack window
                        closure->upvalues[i] =
                            captureUpvalue(frame->slots + index);
                    } else {
                        // Use the already captured value in the frame
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OP_CLOSE_UPVALUE:
                // "Close" a captured upvalue, hoisting it off the stack and
                // onto the heap
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            case OP_RETURN: {
                // Update the slots pointer for the frame's window,
                // Get the result of the called function from the stack and
                // discard the frame for it:
                // return 4 + sum(5, 6, 7);
                //                        V Pop return value (18)
                // (0) script 4 sum 5 6 7 18
                //                      V
                // (0) script 4 sum 5 6 7
                Value result = pop();
                // Close all upvalues in the function parameters
                closeUpvalues(frame->slots);
                vm.frameCount--;

                if (vm.frameCount == 0) {
                    // Top-level code, pop call frame for script and end
                    // execution
                    pop();
                    return INTERPRET_OK;
                }

                // Discard all var slots used by the called function, push
                // the result, and update the local frame reference
                //              V Pointed to by frame->slots
                // (0) script 4 sum 5 6 7
                vm.stackTop = frame->slots;
                // (0) script 4 18
                push(result);
                // Update local frame reference to continue execution of the
                // calling frame, with the result of the call (if any) on
                // the stack
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLASS:
                // Create a new class with the name on the stack
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            case OP_INHERIT: {
                // Get (what should be) class references off of the stack
                Value superclass = peek(1);

                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass* subclass = AS_CLASS(peek(0));

                // Copy all the inherited methods to the subclass. This
                // avoids additional lookups when calling inherited methods
                // and works since classes cannot have methods added to
                // them after definition. This is emitted prior to the
                // method definitions, so there are no existing methods to
                // overwrite
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop();  // Subclass.
                break;
            }
            case OP_METHOD:
                defineMethod(READ_STRING());
                break;
        }
    }

// Friendship ended with macros
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

/* Interpret a Lox source.
 *
 * Given a string with a Lox source, either from a REPL or a file, this will
 * run it in the Lox VM.
 *
 * @param source Lox source to interpret
 *
 * @return result the result of execution
 */
InterpretResult interpret(const char* source) {
    // Compile the source and store the resulting "function" representing
    // the top-level code
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    // Push the function to the reserved stack slot 0 in the VM, which
    // always points to the function being called (held here to prevent GC)
    push(OBJ_VAL(function));

    // Initialize the first call frame for top-level code, pointing to the
    // first instruction in the chunk and the bottom of the VM's value stack
    ObjClosure* closure = newClosure(function);

    // Replace the function in slot 0 with the closure for it
    pop();
    push(OBJ_VAL(closure));

    // Call the closure for the function
    call(closure, 0);

    // Run bytecode in Chunk and return result
    return run();
}
