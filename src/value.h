#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"

// Obj forward declaration due to cyclical deps between Value <-> Obj, actual
// definition in object.h
typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

// Bit mask for NaNs which contain pointers to Lox Objects, which is just
// the sign bit in the last bit position set to 1. This indicates that
// Mantissa bits 0-49 will contain the Lox Object pointer, though in
// practical use only bits 0-47 contain an address
#define SIGN_BIT ((uint64_t)0x8000000000000000)

// Bit mask for all quiet NaNs which includes the 11 exponent bits, as
// well as the Quiet NaN flag bit QF (51) and the Intel 
// QNaN Floating-Point Indef FP (50) which can't be re-used. These are
// combined with bits in the mantissa to indicate different Lox Value
// types
// Binary representation:
//  Sign  Exp Bits  QF FP    Mantissa  <- IEEE 754 purpose
//  0   11111111111 1  1   000...00000 <- Value
// 63   ........... 51 50  ..........0 <- Bit position
#define QNAN ((uint64_t)0x7ffc000000000000)

// Type tags for nil, false, and true, using the first 2 mantissa bits
// 01111111111111...01
#define TAG_NIL   1
// 01111111111111...10
#define TAG_FALSE 2
// 01111111111111...11
#define TAG_TRUE  3

/* Generic Lox Value type. */
typedef uint64_t Value;

/* Check if a Lox Value is a boolean.
 *
 * This is similar to (but less readable than):
 * ((v) == TRUE_VAL || (v) == FALSE_VAL)
 *
 * In order to avoid any possible side effects from v being evaluated
 * twice since this macro is expanded directly. Instead the actual val
 * is ORed with 1 and compared to the tagged true value. If the value
 * has any bits other than the first in use, it will not match the true
 * value and evaluate to false.
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is a boolean
 */
#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)

/* Check if a Lox Value is nil.
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is nil
 */
#define IS_NIL(value) ((value) == NIL_VAL)

/* Check if a Lox Value is a number.
 *
 * This should be used with AS_NUMBER to ensure the value being used is 
 * actually of the expected type, before using the 64-bits within. This
 * is done by comparing the Value with the QNAN mask, as all doubles
 * which are actually numbers do not have these bits set
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is a Lox number
 */
#define IS_NUMBER(value) (((value) & QNAN) != QNAN)

/* Check if the inner value is a Lox Obj instance
 *
 * This compares the value provided with the sign and QNAN bits in order
 * to determine that the lower bits contain an object pointer. If the
 * sign is 0, it is one of the other type tags, and if the QNAN bits are
 * not set then it is an otherwise normal C double.
 *
 * @param value the Lox Value wrapping the actual value
 *
 * @return whether or not the inner value is an object
 */
#define IS_OBJ(value) \
    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)

/* Macro for "casting" a Lox Value to a C double.
 *
 * @param value Lox Value instance
 *
 * @return C double representation of Lox Value
 */
#define AS_NUMBER(value) valueToNum(value)

/* Cast a Lox Value to a Lox Obj.
 *
 * This is the inverse of OBJ_VAL and does a bitwise NAND on the combined
 * sign and QNAN bits to zero them out, leaving the remaining object
 * address in the Mantissa bits.
 *
 * The AS_* functions defined in object.h can be used to cast to a
 * specific type of Lox object instance.
 *
 * @param value the Lox Value wrapping the Obj
 *
 * @return the inner Lox Obj
 */
#define AS_OBJ(value) \
    ((Obj*)(uintptr_t)((value) & ~ (SIGN_BIT | QNAN)))

/* Macro for initializing a new false Value instance.
 *
 * This converts a C bool to the Lox equivalent by using the C
 * conditional operator
 *
 * @return Value instance with a false type tag
 */
#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)

/* Macro for initializing a new false Value instance.
 *
 * @return Value instance with a false type tag
 */
#define FALSE_VAL ((Value) (uint64_t)(QNAN | TAG_FALSE))

/* Macro for initializing a new true Value instance.
 *
 * @return Value instance with a true type tag
 */
#define TRUE_VAL ((Value) (uint64_t)(QNAN | TAG_TRUE))

/* Macro for initializing a new nil Value instance.
 *
 * @return Value instance with a nil type tag
 */
#define NIL_VAL ((Value) (uint64_t)(QNAN | TAG_NIL))

/* Macro for initializing a new number Value instance.
 *
 * @return Value instance representing a number
 */
#define NUMBER_VAL(num) numToValue(num)

/* Macro for initializing a new object Value instance.
 *
 * This combines the sign and QNAN flag bits 50-63 and stores the pointer
 * address in Mantissa bits 0-49.
 *
 * @return Value instance for a Lox object
 */
#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

/* "Cast" a Lox Value to a C double.
 *
 * This does a memcpy which will (or should) inidicate to the compiler
 * that this Value is actually a C double.
 *
 * @param value Lox Value instance
 *
 * @return C double representation of Lox Value
 */
static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));

    return num;
}

/* "Cast" a C double to a Lox Value.
 *
 * This does a memcpy which will (or should) inidicate to the compiler
 * that this C double is actually a Lox Value for a double.
 *
 * @param C double instance
 *
 * @return Lox Value for a given C double
 */
static inline Value numToValue(double num) {
    // "Convert" a pointer to a double to a Lox Value (type punning) by
    // copying the contents to a new local variable. This is the most
    // common method of implementing type punning so most compilers will
    // optimize the copy operation out
    Value value;
    memcpy(&value, &num, sizeof(double));

    return value;
}

#else

/* Lox Value type.
 *
 * Represents the Lox type of a given Value instance, which is then used to
 * access the corresponding C type data via the union.
 */
typedef enum {
    VAL_BOOL,   // Boolean value, corresponds to C bool (true, false)
    VAL_NIL,    // nil, does not correspond to any C type. Any Value instances
                // with type VAL_NIL will have no value set in the union.
    VAL_NUMBER, // Lox number, corresponds to C double (123.456)
    VAL_OBJ,    // Lox Obj, representing a Lox Object instance of some type
} ValueType;

/* A Lox Value.
 *
 * This is a tagged union which represents a dynamically typed value in Lox.
 *
 * Creation of Values can be handled via the *_VAL methods defined below, and
 * retrieval can be handled with the AS_* macros.
 */
typedef struct {
    ValueType type;  // The type of Lox data contained within this Value. This
                     // is the "tag" in the tagged union, which is what is used
                     // to identify the concrete C type of the value stored in
                     // the as property.
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;            // C union containing the actual data for this Lox Value.
                     // Unions are as large as their largest field, but they
                     // are all laid out on top of one another in memory, so
                     // the type tag is used to determine how many bytes to
                     // retrieve and interpret for the value.
} Value;

/* Check if a Lox Value is a bool.
 *
 * This should be used with AS_BOOL to ensure the value being used is actually
 * of the expected type, before retrieving a value from the union.
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is a Lox bool
 */
#define IS_BOOL(value) ((value).type == VAL_BOOL)

/* Check if a Lox Value is nil.
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is nil
 */
#define IS_NIL(value) ((value).type == VAL_NIL)

/* Check if a Lox Value is a number.
 *
 * This should be used with AS_NUMBER to ensure the value being used is actually
 * of the expected type, before retrieving a value from the union.
 *
 * @param value Lox Value instance
 *
 * @return whether or not the value is a Lox number
 */
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

/* Check if the inner value is a Lox Obj instance
 *
 * @param value the Lox Value wrapping the actual value
 *
 * @return whether or not the inner value is of type VAL_OBJ
 */
#define IS_OBJ(value) ((value).type == VAL_OBJ)

/* Cast a Lox Value to a Lox Obj.
 *
 * This unwraps the inner Lox Obj instance from the Value and returns it. The
 * AS_* functions ddefined in object.h can be used to cast to a specific type
 * of Lox object instance.
 *
 * @param value the lox value wrapping the Obj
 *
 * @return the inner Lox Obj
 */
#define AS_OBJ(value) ((value).as.obj)

// AS_NIL is not needed, since a nil Value just has the VAL_NIL tag and no data
/* Macro for "casting" a Lox Value to a C boolean.
 *
 * @param value Lox Value instance
 *
 * @return C boolean representation of Lox Value
 */
#define AS_BOOL(value) ((value).as.boolean)

/* Macro for "casting" a Lox Value to a C double.
 *
 * @param value Lox Value instance
 *
 * @return C double representation of Lox Value
 */
#define AS_NUMBER(value) ((value).as.number)

/* Macro for initializing a new bool Value instance.
 *
 * BOOL_VAL(true) becomes ((Value) {VAL_BOOL, {.boolean = true}})
 *
 * @param value C boolean literal value
 *
 * @return Value instance of type VAL_BOOL with provided boolean value
 */
#define BOOL_VAL(value) ((Value) {VAL_BOOL, {.boolean = value}})

/* Macro for initializing a new nil Value instance.
 *
 * NIL_VAL becomes ((Value) {VAL_NIL, {.number = 0}})
 *
 * @return Value instance of type VAL_NIL with no data
 */
#define NIL_VAL ((Value) {VAL_NIL, {.number = 0}})

/* Macro for initializing a new Value instance for a Lox number.
 *
 * NUMBER_VAL(123.456) becomes ((Value) {VAL_NUMBER, {.number = 123.456}})
 *
 * @param value numerical value, must be a C double
 *
 * @return Value instance of type VAL_NUMBER with provided value
 */
#define NUMBER_VAL(value) ((Value) {VAL_NUMBER, {.number = value}})

/* Macro for initializing a new Value instance for a Lox Obj.
 *
 * @OBJ_VAL(object) becomes ((Value) {VAL_OBJ, {.obj = obj})
 *
 * @param object Lox Obj instance
 *
 * @return Value instance of type VAL_OBJ with provided object
 */
#define OBJ_VAL(object) ((Value) {VAL_OBJ, {.obj = (Obj*)object}})

#endif

/* @struct ValueArray
 *
 * @brief A dynamic array of Lox runtime Values.
 */
typedef struct {
    int capacity;  // Max number of Values in the array. The array will be grown
                   // if count + 1 > capacity
    int count;     // Current number of Values in the array
    Value* values; // Dynamic array holding the Values in this ValueArray.
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
