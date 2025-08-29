#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

// Maximum call depth of the Lox VM. Additional calls beyond this depth will
// result in a call stack overflow.
#define FRAMES_MAX 64

// Max amount of values that can be stored in the VM's stack, exceeding this
// will result in a value stack overflow. This is defined to be proprtional to
// the call depth to ensure enough slots for locals are available.
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// A frame on the call stack.
//
// This encapsulates a closure for a Lox function on the call stack, which
// itself encapsulates the actual ObjFunction definition, along with all captured
// local variables related to this invocation which are on the value stack.
typedef struct {
    ObjClosure* closure;  // Closure for the Lox function being called
    uint8_t* ip;   // Pointer to the caller's frame
    Value* slots;  // First slot in the value stack for this frame, the name is
                   // plural because it will be used as a pointer to a region in
                   // the greater Value stack in the VM. For example:
                   // frame->slots[2]
                   // When compiled will go to the start of whatever is pointed
                   // to by slots, add 2 to the pointer, and access whatever is
                   // there. (https://c-faq.com/aryptr/aryptr2.html)
} CallFrame;

/* Lox Virtual Machine.
 *
 * The Lox VM handles execution of Lox bytecode Chunks.
 */
typedef struct {
    CallFrame frames[FRAMES_MAX];  // Array for the Lox VM's call stack.
    int frameCount;  // Current call depth (call stack height).
    Value stack[STACK_MAX];  // Array for the Lox VM's value stack.
    Value* stackTop;  // Pointer to the array element after the top value on the
                      // stack. This is a byte pointer like ip for the same
                      // reason. Because it points to the next index, this means
                      // address 0 is an empty stack, rather than -1 which would
                      // be undefined. The C standard does allow for pointing to
                      // just past the end of an array, which this will point to
                      // in the case of a full stack.
    Table globals;    // Hash table of all defined global variables at runtime
    Table strings;    // Hash table of all unique string values defined, used
                      // for "string interning" in order to ensure two strings
                      // with the same chars point to the same memory and thus
                      // are equal
    ObjString* initString;  // "init" string, which is used to look up the
                            // initializer method for a class during construction.
                            // This is interned manually during VM creation as
                            // an optimization when creating class instances
    ObjUpvalue* openUpvalues;  // Intrusive linked list of all open upvalues
    size_t bytesAllocated;  // Total memory allocated to VM currently, in bytes
    size_t nextGC;  // The threshold at which the next GC cycle will be
                    // triggered. This should be > bytesAllowed and is adjusted
                    // by the GC at runtime
    Obj* objects;     // Pointer to the top of the linked list of all allocated
                      // objects (the first allocated object, or NULL)
    int grayCount;    // Current number of gray marked objects in the worklist
    int grayCapacity; // Current capacity of the grayStack
    Obj** grayStack;  // The "worklist" of gray marked objects for the garbage
                      // collector
} VM;

/* Bytecode interpretation result.
 * 
 * The result of execution of a given bytecode Chunk in the Lox VM
 */
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

// Export the global reference to the Lox vm instance, so that it can be used
// by other files ie for memory management
extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
