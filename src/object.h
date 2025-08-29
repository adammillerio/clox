#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

/* Macro to retrieve the ObjType of a heap allocated Obj instance.
 *
 * @param value enclosing Lox Value
 *
 * @return the Lox ObjType of the inner Obj instance
 */
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

/* Macro to assert that the ObjType of a heap allocated Obj is a bound method.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a bound Lox method.
 */
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)

/* Macro to assert that the ObjType of a heap allocated Obj is a class.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a Lox class
 */
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)

/* Macro to assert that the ObjType of a heap allocated Obj is a closure.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a Lox function closure
 */
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)

/* Macro to assert that the ObjType of a heap allocated Obj is a function.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a Lox function
 */
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)

/* Macro to assert that the ObjType of a heap allocated Obj is a class instance.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a Lox class instance
 */
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)

/* Macro to assert that the ObjType of a heap allocated Obj instance is a native function.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a native C function
 */
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)

/* Macro to assert that the ObjType of a heap allocated Obj instance is a string.
 *
 * isObjType() is broken out into an inline function due to the fact that the
 * expression argument (value) is placed directly into the function body, this
 * results in double-evaluation related side-effects, such as:
 * IS_STRING(POP())
 *
 * Resulting in:
 * IS_OBJ(POP()) && AS_OBJ(POP())->type == type;
 *
 * Which would cause two values to be popped from the value stack.
 *
 * @param value Lox Value to check
 *
 * @return whether or not the Lox Value's Obj is a string
 */
#define IS_STRING(value) isObjType(value, OBJ_STRING)

/* Macro to retrieve the inner Lox ObjBoundMethod from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjClass representing a bound Lox class method
 */
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))

/* Macro to retrieve the inner Lox ObjClass from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjClass representing a class
 */
#define AS_CLASS(value) ((ObjClass*)AS_OBJ(value))

/* Macro to retrieve the inner Lox ObjClosure from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjClosure representing a function closure
 */
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))

/* Macro to retrieve the inner Lox ObjFunction from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjFunction representing a function definition
 */
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))

/* Macro to retrieve the inner Lox ObjInstance from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjInstance representing a class instance
 */
#define AS_INSTANCE(value) ((ObjInstance*)AS_OBJ(value))

/* Macro to retrieve the inner Lox ObjNative from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjNative representing a native C function
 */
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)

/* Macro to retrieve the inner Lox ObjString from a Value.
 *
 * @param value the Lox Value
 *
 * @return the inner Lox ObjString
 */
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))

/* Macro to retrieve the inner C string from a Lox ObjString in a Value.
 *
 * This is a convienence function as the C string is frequently accessed at
 * runtime.
 *
 * @param value the Lox Value
 *
 * @return the inner chars* C string from the Lox ObjString instance
 */
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

/* Lox Obj type.
 *
 * This is stored as the type field on a Lox Obj and indicates the type of it
 * at runtime.
 */
typedef enum {
    OBJ_BOUND_METHOD,  // A class method bound to an instance of the class
    OBJ_CLASS,  // An object representing a class definition
    OBJ_CLOSURE,  // A closure, wrapping a runtime call of a function along with
                  // it's scope and captured locals
    OBJ_FUNCTION,  // Lox function
    OBJ_INSTANCE,  // An instance of a class
    OBJ_NATIVE,  // Native C function implementing things like syscalls
    OBJ_STRING,  // String value ("foo")
    OBJ_UPVALUE,  // A nonlocal Lox "upvalue" which is captured by a closure
} ObjType;

/* Lox Object.
 *
 * Represents a heap allocated Lox Obj instance. This is the base class, with
 * type specific definitions below.
 */
struct Obj {
    ObjType type;  // The type of Lox Obj this is, ie OBJ_STRING for a ObjString
    bool isMarked;  // Whether or not this object is marked as reachable by the
                    // garbage collector
    struct Obj* next;  // Pointer to the next Obj in the linked list of all
                       // objects, used for memory management.
};

/* Lox Function.
 *
 * This is an Obj that represents a Lox function defined in a program at runtime.
 */
typedef struct {
    Obj obj;  // Base obj instance, with type information
    int arity;  // Number of parameters expected
    int upvalueCount;  // Number of captured local "upvalues" this function has
    Chunk chunk;  // Compiled bytecode chunk
    ObjString* name;  // Function name
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

/* A native C function.
 *
 * This is an Obj that represents a native function, which is just a pointer to
 * a C function that typically implements system level functionality, such as
 * accessing the system clock or working with files.
 */
typedef struct {
    Obj obj;  // Base obj instance, with type information
    NativeFn function;  // Pointer to implementing native C function
} ObjNative;

/* Lox String Object.
 *
 * This is an Obj which represents a string defined in a program at runtime.
 */
struct ObjString {
    Obj obj;        // Base Obj instance, with type information
    int length;     // Length of the string in chars
    char* chars;    // Underlying C string stored in the ObjString
    uint32_t hash;  // Hash of string's chars, see hashString()
};

/* Lox "upvalue" or nonlocal captured variable.
 *
 * When a function uses a variable nonlocal to itself (but also not global), it
 * will be captured in the closure corresponding to it. This is a direct
 * reference to the captured variable on the stack at runtime, which means that
 * interactions with it are the same as they would be for a local variable:
 * fun outer() {
 *     var x = "before";
 *     fun inner() {
 *         x = "assigned";
 *     }
 *     inner();
 *     print x;
 * }
 * outer();
 *
 * In this example, inner captures the nonlocal variable x and stores the ref
 * to it in an ObjUpvalue at runtime, which is then accessed to reassign the
 * underlying value directly by reference.
 */
typedef struct ObjUpvalue {
    Obj obj;  // Base Obj instance, with type information
    Value* location;  // Pointer which is initially to a stack slot to be
                      // captured. Once the Value in the slot is hoisted off
                      // of the stack onto the heap, this will point to the
                      // location of closed, which will make both the GET/SET
                      // opcodes for upvalues work as expected (see 25.4.4)
    Value closed;   // Pointer to the variable on the heap once it is "closed
                    // over"
    struct ObjUpvalue* next;  // Pointer to the next ObjUpvalue in the intrusive
                              // list of all captured upvalues maintained at
                              // runtime for deduplication
} ObjUpvalue;

/* A function closure.
 *
 * A closure is a function along with any nonlocal upvalues that it captures.
 */
typedef struct {
    Obj obj;  // Base Obj instance, with type information
    ObjFunction* function;  // Pointer to the function that this closure wraps
    ObjUpvalue** upvalues;  // Collection of upvalue pointers for this closure
    int upvalueCount;  // Total number of captured upvalues in this closure
} ObjClosure;

/* A class.
 *
 * A class definition has the class name and all methods defined during
 * compilation, which are bound to instances when invoked.
 */
typedef struct {
    Obj obj;  // Base Obj instance, with type information
    ObjString* name;  // Class name
    Table methods;  // Class methods
} ObjClass;

/* A class instance.
 *
 * An instance has a reference to the class definition, as well as a table for
 * all set instance fields (at runtime or otherwise).
 */
typedef struct {
    Obj obj;  // Base Obj instance, with type information
    ObjClass* klass;  // Class that this is an instance of
    Table fields;  // Instance fields and their state
} ObjInstance;

/* A bound class method.
 *
 * These are created when a class instance is invoked and contains a reference
 * to the class method and it's receiving instance.
 */
typedef struct {
    Obj obj;  // Base Obj instance, with type information
    Value receiver;  // Receiver value to bind this method to, currently this is
                     // always an ObjInstance, but uses Value to avoid requiring
                     // casts to pass it to functions which operate generically
                     // on Lox values
    ObjClosure* method;  // Closure representing the generic class method that
                         // is being bound
} ObjBoundMethod;

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);
ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);

void printObject(Value value);

/* Check if the Obj in a Lox Value is of a given type.
 *
 * This should be used via the IS_* macros defined above.
 *
 * @param value Lox Value to check
 * @param type Lox ObjType to check the Value against
 *
 * @return whether or not the Lox Value's Obj is of the given type
 */
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
