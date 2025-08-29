#include "memory.h"

#include <stdlib.h>

#include "compiler.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>

#include "debug.h"  // IWYU pragma: keep
#endif

// Multiple to use when deriving the next GC threshold for memory (in bytes)
// ie 1024^2 * 2
#define GC_HEAP_GROW_FACTOR 2

/* Reallocate a dynamically allocated area in memory such as a dynamic array.
 *
 * This should be called via GROW_ARRAY/GROW_CAPACITY, which handles determining
 * capacity and memory allocation sizes for the array's type as well as casting
 * back to the original type.
 *
 * oldSize 0, newSize non zero -> A new block is allocated
 * oldSize non-zero, newSize 0 -> Allocation is freed
 * oldSize non-zero, newSize < oldSize -> Existing allocation is shrunk
 * oldSize non-zero, newSize > oldSize -> If there is enough free memory after
 *     the existing allocation, it will be grown to the new size and the same
 * ptr is returned. If not, a new block is allocated of the desired size, the
 *     memory up to oldSize is copied to it, and a ptr to the new area is
 * returned.
 *
 * More info: sch cpp realloc
 *
 * If the newly reallocated amount of bytes exceeds the current GC threshold, or
 * if GC stress testing is enabled, a garbage collection cycle will be triggered
 * before execution continues.
 *
 * @param pointer the dynamically allocated object to resize
 * @param oldSize old size of the array, sizeof(type) * oldCount with GROW_ARRAY
 * @param newSize new size of the array, sizeof(type) * newCount with GROW_ARRAY
 *
 * @return the pointer to the resized object
 */
void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
    // Update total allocated bytes
    vm.bytesAllocated += newSize - oldSize;

    // If reallocate is being called to get more memory, run GC. This avoids
    // infinite recursion when calling reallocate to free memory in the GC
    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        // Reallocation triggered GC
        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
    }

    // New size is 0, free the array
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    // oldSize 0 -> this will effectively call malloc(pointer) for a new array
    // newSize < oldSize -> Updates shrinks array and returns same pointer
    // newSize > oldSize ->
    //      If enough free memory after array, update the size and return same
    //      pointer If not, allocate a new block of desired size, copy items,
    //      and
    //          return a pointer to the new array
    void *result = realloc(pointer, newSize);

    // Failed to allocate memory for new array, exit program
    if (result == NULL) exit(1);

    return result;
}

/* GC: Mark an object as reachable and add it to the worklist.
 *
 * Given an object not already marked during a GC cycle, this will add it to
 * the collection of gray objects on the worklist. Next, it will be blackened
 * and have all of it's own references added to the worklist, continuing the
 * wave of reachability tracing.
 *
 * @param object Lox object to mark as reachable.
 */
void markObject(Obj *object) {
    // Null reference, nothing to mark
    if (object == NULL) return;
    // Already marked, return to avoid a cycle
    if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // Grow the array using the native C realloc() since this memory must
    // exist outside of the GC'd space itself
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack =
            (Obj **)realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

        // Can't allocate more memory for the GC worklist, all is lost
        if (vm.grayStack == NULL) exit(1);
    }

    // Add this "gray" marked object to the worklist, so that it's references
    // can be handled in the next pass
    vm.grayStack[vm.grayCount++] = object;
}

/* GC: Mark a Lox Value as reachable.
 *
 * @param value Lox value to mark
 */
void markValue(Value value) {
    // If this value is heap-allocated and not a primitive on the stack, mark it
    if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

/* GC: Mark all values in a Lox ValueArray
 *
 * @param array Lox ValueArray to mark all Values in
 */
static void markArray(ValueArray *array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

/* GC: Blacken a marked object in the gray worklist.
 *
 * Given a gray object previously marked and added to the worklist during the
 * reachability trace, this will "blacken" the object. All of the object's
 * references will themselves be marked and added to the gray worklist,
 * indicating that the object is now both marked as reachable and traced.
 *
 * @param object Lox Object to blacken
 */
static void blackenObject(Obj *object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void *)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type) {
        case OBJ_BOUND_METHOD: {
            // Mark the references to the bound method and it's receiver
            ObjBoundMethod *bound = (ObjBoundMethod *)object;
            markValue(bound->receiver);
            markObject((Obj *)bound->method);
            break;
        }
        case OBJ_CLASS: {
            // Mark this class and it's method definition table
            ObjClass *klass = (ObjClass *)object;
            markObject((Obj *)klass->name);
            markTable(&klass->methods);
            break;
        }
        case OBJ_CLOSURE: {
            // Mark this closure's function
            ObjClosure *closure = (ObjClosure *)object;
            markObject((Obj *)closure->function);

            // Mark all captured upvalues in this closure
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj *)closure->upvalues[i]);
            }

            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction *function = (ObjFunction *)object;

            // Mark a function's name and it's constants
            markObject((Obj *)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_INSTANCE: {
            // Mark the instance field table, as well as the class definition
            // that it is a part of
            ObjInstance *instance = (ObjInstance *)object;
            markObject((Obj *)instance->klass);
            markTable(&instance->fields);
            break;
        }
        case OBJ_UPVALUE:
            // Mark the reference to the closed over value in an upvalue
            markValue(((ObjUpvalue *)object)->closed);
            break;
        // Native type, no references
        case OBJ_NATIVE:
        // No references other than the string itself
        case OBJ_STRING:
            break;
    }
}

/* Free a Lox object instance.
 *
 * This frees the memory backing a Lox object instance, handling any additional
 * memory that needs to be freed depending on the object's type.
 *
 * @param object the Lox object to free
 */
static void freeObject(Obj *object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void *)object, object->type);
#endif

    switch (object->type) {
        case OBJ_BOUND_METHOD:
            // Bound methods do not own their references, nothing to free
            FREE(ObjBoundMethod, object);
            break;
        case OBJ_CLASS: {
            // Free the class and it's method definition table
            ObjClass *klass = (ObjClass *)object;
            freeTable(&klass->methods);

            FREE(ObjClass, object);
            break;
        }
        case OBJ_CLOSURE: {
            // Free this closure's array of upvalue references. The variables
            // pointed to by the upvalues are handled by the GC
            ObjClosure *closure = (ObjClosure *)object;
            FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);

            // Noting extra to free, the ObjFunction itself could be referenced
            // elsewhere so it is managed by GC
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction *function = (ObjFunction *)object;

            // Free the Lox function's bytecode Chunk
            freeChunk(&function->chunk);

            FREE(ObjFunction, object);
            break;
        }
        case OBJ_INSTANCE: {
            // Free the instance's field table, with the values themselves being
            // freed by the GC if they are not referenced elsewhere
            ObjInstance *instance = (ObjInstance *)object;
            freeTable(&instance->fields);
            FREE(ObjInstance, object);
            break;
        }
        case OBJ_NATIVE:
            // Nothing extra in the Lox VM needs to be freed with native C funcs
            FREE(ObjNative, object);
            break;
        case OBJ_STRING: {
            ObjString *string = (ObjString *)object;

            // Free the underlying C string literal
            FREE_ARRAY(char, string->chars, string->length + 1);

            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE:
            // Upvalues are not owned by their closures, no need to free the ref
            FREE(ObjUpvalue, object);
            break;
    }
}

/* GC: Mark all of the VM's root objects.
 *
 * This is the initial phase of the GC cycle which adds all of the root objects
 * to the gray worklist, providing a source for reachability tracing.
 */
static void markRoots() {
    // Walk and mark the roots currently on the VM's stack
    for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    for (int i = 0; i < vm.frameCount; i++) {
        // Mark all closures on the VM's call stack
        markObject((Obj *)vm.frames[i].closure);
    }

    for (ObjUpvalue *upvalue = vm.openUpvalues; upvalue != NULL;
         upvalue = upvalue->next) {
        // Mark all currently open upvalues
        markObject((Obj *)upvalue);
    }

    // Walk and mark the VM's global variables
    markTable(&vm.globals);

    markCompilerRoots();

    // Manually mark the reserved initializer string
    markObject((Obj *)vm.initString);
}

/* GC: Trace the references of all gray objects on the worklist.
 *
 * This traverses the worklist, blackening each gray object and traversing it's
 * own references, marking them gray and adding them to the worklist. The
 * worklist is filled until the entire graph of reachable objects is traversed.
 */
static void traceReferences() {
    while (vm.grayCount > 0) {
        // Process an object on the worklist, blackening the object itself and
        // marking its references as gray to be processed in the next pass
        Obj *object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

/* GC: Sweep all unmarked (unreachable) objects.
 *
 * This is called after the graph of reachable objects from the compiler roots
 * has been traced and all reachable objects has been marked. It will traverse
 * the entire set of objects on the heap, "sweeping" any unmarked objects and
 * freeing the memory backing them.
 */
static void sweep() {
    Obj *previous = NULL;
    Obj *object = vm.objects;

    // Traverse all objects in the heap
    while (object != NULL) {
        if (object->isMarked) {
            // Object is marked (reachable), unmark for next GC cycle and
            // continue
            object->isMarked = false;
            previous = object;
            object = object->next;
        } else {
            // Object is unmarked (unreachable)
            Obj *unreached = object;

            // Unlink it from the list
            object = object->next;

            if (previous != NULL) {
                // Link the previous value to the one after the unlinked value
                previous->next = object;
            } else {
                vm.objects = object;
            }

            // Free the unreachable and unlinked object
            freeObject(unreached);
        }
    }
}

/* GC: Run a complete garbage collection cycle.
 *
 * This is called during reallocation once the GC threshold is triggered, and
 * will run a full mark/sweep of the objects in the VM, freeing any that are
 * no longer reachable.
 *
 * Once the GC is complete, the threashold will be updated to reflect the new
 * size relative to the configured grow factor.
 */
void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    tableRemoveWhite(&vm.strings);
    sweep();

    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
           before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

/* Free all objects in the VM's allocation list.
 *
 * This walks the linked list of all allocated objects within the VM
 * and frees each one. It also frees the garbage collector's
 * worklist of marked objects.
 */
void freeObjects() {
    Obj *object = vm.objects;
    while (object != NULL) {
        Obj *next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
