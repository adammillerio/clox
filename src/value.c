#include "value.h"

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"

/*
 * Create a new ValueArray.
 *
 * This is both a "Constructor" for new arrays and a way to reset after freeing
 * one in memory.
 *
 * @param array pointer to the value array to (re)initialize
 */
void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

/* Write a new Lox Value to a ValueArray.
 *
 * This adds the value to the end of the internal dynamic array, growing it if
 * more space is needed.
 *
 * @param array pointer to the ValueArray to write to
 * @param value lox Value to write to the array
 */
void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values =
            GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

/* Free a ValueArray.
 *
 * This frees the underlying array and initializes it back to default values.
 *
 * @param array pointer to the ValueArray to free
 */
void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

/* Print a Lox Value to stdout.
 *
 * This will print a human-readable representation of the Lox Value to stdout.
 *
 * @param value lox Value to print
 */
void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        printf("nil");
    } else if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        printObject(value);
    }
#else
    switch (value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true" : "false");
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_NUMBER:
            printf("%g", AS_NUMBER(value));
            break;
        case VAL_OBJ:
            printObject(value);
            break;
    }
#endif
}

/* Test for equality of two Lox Values.
 *
 * This compares two Lox Values via their inner C type value.
 *
 * Because Value is a union, it contains unused bits and padding to align it
 * with the system's word length. This means memcmp() can't be used for a more
 * efficient comparison, since it compares values bit-for-bit. Even if the bits
 * representing the actual value of each are the same, the rest are unused and
 * can be anything. See section 18.4.2 for more info.
 *
 * @param a left operand
 * @param b right operand
 *
 * @result whether or not a == b
 */
bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        // Explicitly cast two Lox Values in case this is a NaN
        // comparison, which should always evaluate to false
        return AS_NUMBER(a) == AS_NUMBER(b);
    }

    return a == b;
#else
    if (a.type != b.type) return false;

    switch (a.type) {
        case VAL_BOOL:
            return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:
            return true;
        case VAL_NUMBER:
            return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:
            return AS_OBJ(a) == AS_OBJ(b);
        default:
            return false;  // Unreachable.
    }
#endif
}
