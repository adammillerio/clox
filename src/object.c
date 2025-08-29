#include "object.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

/* Macro to allocate a Lox object instance.
 *
 * This exists mostly to handle type casting to the desired Obj type.
 *
 * @param type the C type, for allocation sizing (e.g. ObjString)
 * @param objectType Lox object instance ObjType (e.g. OBJ_STRING)
 *
 * @return the allocated Lox object instance
 */
#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

/* Allocate a Lox object of a given size.
 *
 * The call specifies the size so that there is allocated space for additional
 * fields on the object depending on it's type. For example, an ObjString will
 * allocate additional space for the underlying C string backing it.
 *
 * @param size allocation size of the object in bytes
 * @param type Lox object instance ObjType for the allocation
 *
 * @return pointer to the newly allocated Lox object instance
 */
static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->isMarked = false;

    // Insert the new object into the VM's linked list of all objects at the top
    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

    return object;
}

/* Allocate a new bound class method on the heap.
 *
 * @param receiver Lox object to bind to
 * @param method Class method closure
 *
 * @return Lox object with method bound to the receiver
 */
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);

    bound->receiver = receiver;
    bound->method = method;

    return bound;
}

/* Allocate a new class on the heap.
 *
 * This will allocate the class itself as well as a table for all of the class
 * methods as they are compiled.
 *
 * @param name Class name
 *
 * @return Class object
 */
ObjClass* newClass(ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klass->name = name;
    initTable(&klass->methods);

    return klass;
}

/* Allocate a new Lox function closure object on the heap.
 *
 * A closure is a collection which holds a reference to the function object
 * itself, as well as to any nonlocal "upvalues" that it captures.
 *
 * @param function Lox function object for this closure
 *
 * @return Allocated Lox function closure instance
 */
ObjClosure* newClosure(ObjFunction* function) {
    ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
    for (int i = 0; i < function->upvalueCount; i++) {
        // Zero out all values for the GC
        upvalues[i] = NULL;
    }

    ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalueCount = function->upvalueCount;

    return closure;
}

/* Allocate a new Lox function object on the heap.
 *
 * This will also initialize the underlying Chunk which will hold the compiled
 * bytecode associated with this function.
 *
 * @return allocated Lox function instance
 */
ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

    function->arity = 0;
    function->upvalueCount = 0;
    function->name = NULL;
    initChunk(&function->chunk);

    return function;
}

/* Allocate a new instance of a Lox class on the heap.
 *
 * This stores a reference to the class for method calls and initializes the
 * table for any instance fields that will be set (in the constructor or
 * otherwise).
 *
 * @param klass Lox class object
 *
 * @return Instance of the Lox class provided
 */
ObjInstance* newInstance(ObjClass* klass) {
    ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->klass = klass;
    initTable(&instance->fields);

    return instance;
}

/* Allocate a new native C function object on the heap.
 *
 * This is just a wrapper that holds the pointer to the C function being
 * defined.
 *
 * @param function Native C function pointer
 *
 * @return allocated native C function instance
 */
ObjNative* newNative(NativeFn function) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;

    return native;
}

/* Allocate a new Lox string object on the heap.
 *
 * @param chars C string to be used for allocation
 * @param length string length in chars
 * @param hash calculated hash of the string chars, see hashstring()
 *
 * @return allocated Lox string object instance with the provided chars
 */
static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // Add string to interned table, verification of uniqueness is done before
    // calling allocateString()
    // GC stack guard in case table is grown while adding the new string
    push(OBJ_VAL(string));
    tableSet(&vm.strings, string, NIL_VAL);
    pop();

    return string;
}

/* Generate the hash for a given string.
 *
 * This implements the FNV-1a hashing algorithm, for more info:
 * https://sch.internalae.com/sch?s=arc+http://www.isthe.com/chongo/tech/comp/fnv/
 *
 * @param key string key to hash
 * @param length string length, used for hashing algorithm
 *
 * @return calculated hash for the string
 */
static uint32_t hashString(const char* key, int length) {
    // Initial hash constant for a 32-bit keyspace, this is completely arbitrary
    // and defined in the link above.
    // hash = offset_basis
    uint32_t hash = 2166136261u;

    // Mutate initial hash using all chars in the string
    // for each octet_of_data to be hashed
    for (int i = 0; i < length; i++) {
        // xor the hash against each octet, which is a char since chars are 8bit
        // hash = hash xor octet_of_data
        hash ^= (uint8_t)key[i];
        // FNV_prime for a 32-bit keyspace = 2^24 + 2^8 + 0x93 = 16777619
        // hash = hash * FNV_prime
        hash *= 16777619;
    }

    return hash;
}

/* Take a string, reallocating it within a Lox string object
 *
 * This re-uses the provided chars. For a by-value copy, see copyString()
 *
 * @param chars C string to be used for allocation
 * @param length string length in chars
 *
 * @return allocated Lox string object instance with the provided chars
 */
ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    // Check if this string is "interned". If it is, free the supplied string
    // and instead return a reference to the existing one in memory.
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

/* Copy a string by value.
 *
 * This allocates a new C string on the heap, copies the provided chars into
 * it, and allocates a new Lox string object with it. To reallocate and take
 * ownership of an existing string's memory, see takeString()
 *
 * @param chars C string to be used for allocation
 * @param length string length in chars
 *
 * @return allocated Lox string object instance with a copy of the provided
 * chars
 */
ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);

    // Check if this string is "interned" and return the pointer to the existing
    // string in memory instead
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    // Allocate a new array for the string
    char* heapChars = ALLOCATE(char, length + 1);

    // Copy the string
    memcpy(heapChars, chars, length);
    // Add a null char, ObjString already stores the length, but this allows the
    // string to be passed around to functions expecting a terminated C string
    heapChars[length] = '\0';

    return allocateString(heapChars, length, hash);
}

/* Allocate a new captured Lox upvalue on the heap.
 *
 * An upvalue is any Lox value nonlocal to a function which is "captured" in
 * order to ensure it is preserved on the heap and made available at runtime
 * on the stack when necessary.
 *
 * The Upvalue initially returned by this function will have it's location
 * property set to the local stack slot. After the value is "closed over" and
 * hoisted off the stack and onto the heap, it will instead point to the same
 * Value pointed to by closed.
 *
 * @param slot Pointer to a Lox Value on the stack to be captured
 *
 * @return Allocated Lox upvalue instance
 */
ObjUpvalue* newUpvalue(Value* slot) {
    ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->closed = NIL_VAL;
    upvalue->location = slot;
    upvalue->next = NULL;

    return upvalue;
}

/* Print a function.
 *
 * @param function Lox function object to print
 */
static void printFunction(ObjFunction* function) {
    // Top-level code
    if (function->name == NULL) {
        printf("<script>");
        return;
    }

    printf("<fn %s>", function->name->chars);
}

/* Print a Lox object.
 *
 * A human-readable representation of the object will be printed to stdout.
 *
 * @param value Lox object value to print
 */
void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_BOUND_METHOD:
            printFunction(AS_BOUND_METHOD(value)->method->function);
            break;
        case OBJ_CLASS:
            // Print the class name
            printf("%s", AS_CLASS(value)->name->chars);
            break;
        case OBJ_CLOSURE:
            // Print the underlying ObjFunction
            printFunction(AS_CLOSURE(value)->function);
            break;
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
            break;
        case OBJ_INSTANCE:
            printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
            break;
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_UPVALUE:
            // What's upvalue?
            printf("upvalue");
            break;
    }
}
