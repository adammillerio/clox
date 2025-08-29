#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

/* Macro for allocating an array of a given type and length/count.
 *
 * This mostly exists to handle the type* cast after allocation.
 *
 * @param type the type to use for array allocation sizing
 * @param count initial capacity of the array
 *
 * @return the allocated array
 */
#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

/* "Free" a value by reallocating it down to 0 bytes.
 *
 * @param type the type of the value being pointed to, for allocation sizing
 * @param pointer the pointer to the value to free
 */
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

/* Macro for determining the new capacity for a dynamic collection such as an array
 *
 * All initial capacities <8 will be initialized as 8 to avoid churn at the cost
 * of a slight increase in initial memory usage.
 *
 * @param capacity Array capacity, becomes sizeof(type) * capacity allocation
 *
 * @return the new grow capacity
 */
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)

/* Macro for calling reallocate() to grow a dynamic array of a given type.
 * 
 * This uses sizeof to get the size of the type in order to calculate the new
 * required array size in memory. It also casts the returned void* pointer to
 * the newly resized area back to the provided type.
 *
 * @param type the type of items within the array, ie uint8_t for an int array
 * @param pointer pointer to the space in memory to be reallocated
 * @param oldCount old (current) item count, used to calculate allocation size
 * @param newCount new item count, used to calculate allocation size
 *
 * @return the pointer to the resized object
 */
#define GROW_ARRAY(type, pointer, oldCount, newCount)     \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
                      sizeof(type) * (newCount))

/* Macro for freeing a dynamic array by reallocating it to a new size of 0.
 *
 * @param type the type of items within the array, ie uint8_t for an int array
 * @param pointer pointer to the space in memory to be reallocated
 * @param oldCount old (current) item count, used to calculate allocation size
 */
#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);
void markObject(Obj* object);
void markValue(Value value);
void collectGarbage();
void freeObjects();

#endif
