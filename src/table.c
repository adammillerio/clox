#include "table.h"

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"  // IWYU pragma: keep
#include "value.h"

// The max "load factor" of a hash table, the underlying dynamic array for a
// given table will be grown if the load factor exceeds this value. The higher
// this value is, the higher likelihood there is of hash collision.
// For example, a table with:
// count = 5
// capacity = 16
// Has a load factor of:
// load_factor = 5 / 16 = 0.3125
#define TABLE_MAX_LOAD 0.75

/* Initialize a new hash table.
 *
 * @param table pointer to hash table to (re)initialize
 */
void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

/* Free a hash table.
 *
 * @param table pointer to hash table to free
 */
void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

/* Find an entry in the hash table.
 *
 * Given an array of entries from a Table, the capacity, and a key, this will
 * calculate the hash index and search the array for a corresponding entry.
 *
 * In the case of adding a new entry, if a tombstone is found, it will be
 * returned instead, so that the Entry underlying it can be re-used with the
 * new value.
 *
 * @param entries pointer to an array of entries from a Table
 * @param capacity current capacity of the array, used for hashing
 * @param key string key to hash and search the array for
 *
 * @return the stored Entry for the key, or NULL if not present
 */
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    // Determine the first index to try for the entry by using the modulus of
    // it's hash against the current total capacity of the array. This will
    // effectively "fold" the maximum numeric range of the key space down onto
    // the current capacity.
    // For example:
    // capacity = 8
    // key = "b"
    // key->hash = 98
    // index = 98 % 8 = 2
    uint32_t index = key->hash & (capacity - 1);
    Entry* tombstone = NULL;

    // Search the array starting at this index for a matching key
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                // If a tombstone was found before this empty bucket, return it
                // instead so that it will be re-used, otherwise, return this
                // empty entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone, see tableDelete()
                // If this is the first tombstone found, store it, so that it
                // will be re-used instead of using an empty bucket
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // We found the key
            return entry;
        }

        // Collision, check the next index, the value is modulo'd for wraparound
        // 2 % 8 = 3
        index = (index + 1) & (capacity - 1);
    }
}

// If this returns true, the Value pointed to by value will be the desired value
/* Get a value from a hash Table.
 *
 * @param table pointer to the Table to search
 * @param key string key to search for in the Table
 * @param value pointer to the Value at this key, if found, NULL otherwise
 *
 * @return whether or not the value was found
 */
bool tableGet(Table* table, ObjString* key, Value* value) {
    // Empty array, return to avoid working on a null entries array
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Update provided pointer to the found value
    *value = entry->value;
    return true;
}

/* Adjust the capacity of the dynamic array backing a given hash Table.
 *
 * @param table pointer to the Table to adjust
 * @param capacity new capacity of the entries array
 */
static void adjustCapacity(Table* table, int capacity) {
    // Allocate a new dynamic array of entries with the updated size
    Entry* entries = ALLOCATE(Entry, capacity);

    // Initialize all buckets in the array to null/nil
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Reset and recalculate the count, since any tombstones in the old array
    // will not be copied over
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        // Check if this bucket in the old array has a value
        Entry* entry = &table->entries[i];
        // Skip if empty bucket or a tombstone
        if (entry->key == NULL) continue;

        // Locate the Entry in the new array and copy it over, this is required
        // since the calculation of a hash index is dependent on the size of the
        // array, so value locations shift after expansion.
        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;

        table->count++;
    }

    // Free the old array
    FREE_ARRAY(Entry, table->entries, table->capacity);

    // Set the new array and capacity
    table->entries = entries;
    table->capacity = capacity;
}

/* Set a value in a hash Table.
 *
 * @param table pointer to the table which will store the value
 * @param key string key to store the value under in the Table
 * @param value Lox Value to store in the Table
 *
 * @return if true, this is a new key, with no previous value in the Table
 */
bool tableSet(Table* table, ObjString* key, Value value) {
    // If the table will be beyond the configured max load factor after adding
    // this value, grow the underlying array
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    // Check if there is an existing entry at this key
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // If this is a new key and the bucket is not a tombstone, increment count,
    // this causes tombstones to be considered in the table's count and load
    // factor in order to avoid a possible infinite loop if the entire array is
    // filled with values or tombstones.
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    // Upsert key
    entry->key = key;
    entry->value = value;

    return isNewKey;
}

/* Delete the value at a given key in the hash Table.
 *
 * @param table pointer to the table to delete the value from
 * @param key string key of the value to delete
 *
 * @return whether or not an entry was found and deleted
 */
bool tableDelete(Table* table, ObjString* key) {
    /* Some housekeeping is required when a value is deleted, due to the fact
     * that the placement of other values in the case of hash collision.
     *
     * For example:
     * capacity = 8
     * hash("a"), hash("b"), hash("c"), hash("d") = 2
     * Because the first three keys have the same hash, they will be placed one
     * after the other:
     * [X, X, "a", "b", "c", X, X, X]
     *
     * If "b" is removed, this leaves a hole:
     * [X, X, "a", X, "c", X, X, X]
     *
     * This will cause findEntry("c") to return NULL, as it will stop on the
     * hole. On deletion, a "Tombstone" sentinel value is placed in the array
     * instead:
     * [X, X, "a", T, "c", X, X, X]
     *
     * findEntry("c") will now skip the tombstone as expected and return the
     * value at the correct location of "c".
     *
     * tableSet("d") will place the value in the first tombstone it finds:
     * [X, X, "a", "d", "c", X, X, X]
     *
     * This re-uses the existing Entry without breaking the chain, since all
     * values still share the same key and start at the same initial index in
     * the case of collision.
     */
    if (table->count == 0) return false;

    // Find the entry
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry
    entry->key = NULL;
    entry->value = BOOL_VAL(true);

    return true;
}

/* Add all elements from one hash Table to another.
 *
 * The two Tables do not need to be the same size, as tableSet will adjust the
 * capacity as necessary while copying values.
 *
 * @param from pointer to the Table to copy from
 * @param to pointer to the Table to copy to
 */
void tableAddAll(Table* from, Table* to) {
    // Copy all non-null buckets to the other table
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

/* Find a C string by value in a hash Table.
 *
 * This differs from findEntry in that it does a direct char-by-char comparison
 * of the provided chars and each key in the Table. This is used for the string
 * "interning" process in the takeString()/copyString() functions which handle
 * string creation.
 *
 * Interning is implemented as a hash Table "Set", which stores the set of all
 * unique strings that have been defined. When a new unique string is defined,
 * it will be stored in this Table. If another string is defined with the same
 * value, a pointer to the interned string will be returned instead. This leads
 * to all strings with the same chars being effectively the same string in
 * memory, allowing for the (much faster) by-pointer comparison in findEntry()
 *
 * @param table pointer to the string interning Table to search
 * @param chars C string to be searched for
 * @param length length of the string
 * @param hash calculated hash of the string
 *
 * @return a pointer to either a newly interned ObjString instance, or the
 * existing one if not unique
 */
ObjString* tableFindString(Table* table, const char* chars, int length,
                           uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash & (table->capacity - 1);
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length && entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            // Compare length/hash first, short circuiting if they do not equal,
            // then compare the strings by value, returning it if they match
            return entry->key;
        }

        // Collision with a different set of chars that have the same hash,
        // continue searching
        index = (index + 1) & (table->capacity - 1);
    }
}

/* Remove any unmarked values from a table which are about to be swept.
 *
 * This is called between mark*() and sweep() in collectGarbage(), and is used
 * to clear out tables with weak references such as the string pool. At this
 * point, any unmarked value here is about to be swept.
 *
 * @param table Table to clear
 */
void tableRemoveWhite(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];

        if (entry->key != NULL && !entry->key->obj.isMarked) {
            // Delete the key string as well, to avoid a dangling reference
            tableDelete(table, entry->key);
        }
    }
}

/* Mark a table's keys and values.
 *
 * This traverses the table, marking each entry value as well as the string key
 * identifying it.
 *
 * @param table Table to mark
 */
void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}
