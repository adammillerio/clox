#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

/* An Entry in a hash Table. */
typedef struct {
    ObjString* key;  // Pointer to the string key for this Entry or NULL if
                     // empty or a "tombstone"
    Value value;     // The Lox Value instance stored at this Entry. If the Entry
                     // is empty, this will be NULL. If the Entry is a "tombstone"
                     // of a deleted value, it will be true
} Entry;

/* A hash Table */
typedef struct {
    int count;       // Current count of values stored in the Table. This is
                     // used to calculate the load factor of the Table and
                     // includes any "tombstones" of deleted values. These
                     // tombstones will not be copied during a resize.
    int capacity;    // Current capacity of the array
    Entry* entries;  // Dynamic array containing the Entry instances stored in
                     // this Table
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(Table* from, Table* to);
ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash);
void tableRemoveWhite(Table* table);
void markTable(Table* table);

#endif
