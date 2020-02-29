#pragma once

#include "allocator.h"
#include "zodiac_string.h"

#include <cassert>
#include <string.h>

template <typename Key_Type>
using Hash_Table_Keys_Equal_FN = bool (*)(const Key_Type& a, const Key_Type& b);

bool hash_table_strings_equal(const String& a, const String& b);

template <typename Key_Type, typename Value_Type>
struct Hash_Table
{
    uint64_t* hashes = nullptr;
    Key_Type* keys = nullptr;
    Value_Type* values = nullptr;

    int64_t capacity = 0;

    Hash_Table_Keys_Equal_FN<Key_Type> keys_equal = nullptr;
    Allocator* allocator = {};
};

uint64_t hash_key(const String& str);
uint64_t hash_key(const char* str);

#define HASH_TABLE_INITIAL_CAPACITY 16

template <typename Key_Type, typename Value_Type>
void hash_table_init(Allocator* allocator, Hash_Table<Key_Type, Value_Type>* hash_table,
                     Hash_Table_Keys_Equal_FN<Key_Type> keys_equal)
{
    assert(allocator);
    assert(hash_table);

    auto hashes_size = sizeof(uint64_t) * HASH_TABLE_INITIAL_CAPACITY;
    auto keys_size = sizeof(Key_Type) * HASH_TABLE_INITIAL_CAPACITY;
    auto values_size = sizeof(Value_Type) * HASH_TABLE_INITIAL_CAPACITY;

    auto total_size = hashes_size + keys_size + values_size;

    uint8_t* mem = alloc_array<uint8_t>(allocator, total_size);
    assert(mem);

    hash_table->hashes = (uint64_t*)mem;
    hash_table->keys = (Key_Type*)(&mem[hashes_size]);
    hash_table->values = (Value_Type*)(&mem[hashes_size + values_size]);

    memset(hash_table->hashes, 0, hashes_size);

    hash_table->capacity = HASH_TABLE_INITIAL_CAPACITY;
    hash_table->keys_equal = keys_equal;
    hash_table->allocator = allocator;
}

template <typename Key_Type, typename Value_Type>
void hash_table_add(Hash_Table<Key_Type, Value_Type>* ht, Key_Type key, Value_Type value)
{
    uint64_t hash = hash_key(key);
    uint64_t hash_index = hash % ht->capacity;
    int64_t iteration_count = 0;

    while (iteration_count < ht->capacity)
    {
        if (ht->hashes[hash_index] == 0)
        {
            ht->hashes[hash_index] = hash;
            ht->keys[hash_index] = key;
            ht->values[hash_index] = value;
            return;
        }
        else
        {
            hash_index++;
        }
        iteration_count++;
    }

    hash_table_grow(ht);
    hash_table_add(ht, key, value);
}

template <typename Key_Type, typename Value_Type>
void hash_table_grow(Hash_Table<Key_Type, Value_Type>* ht)
{
    auto new_cap = ht->capacity * 2;

    auto new_hashes_size = sizeof(uint64_t) * new_cap;
    auto new_keys_size = sizeof(Key_Type) * new_cap;
    auto new_values_size = sizeof(Value_Type) * new_cap;

    auto new_total_size = new_hashes_size + new_keys_size + new_values_size;

    auto old_cap = ht->capacity;
    auto old_hashes = ht->hashes;
    auto old_keys = ht->keys;
    auto old_values = ht->values;

    auto new_mem = alloc_array<uint8_t>(ht->allocator, new_total_size);

    ht->capacity = new_cap;
    ht->hashes = (uint64_t*)(&new_mem[0]);
    ht->keys = (Key_Type*)(&new_mem[new_hashes_size]);
    ht->values = (Value_Type*)(&new_mem[new_hashes_size + new_keys_size]);

    memset(ht->hashes, 0, new_hashes_size);

    for (int64_t i = 0; i < old_cap; i++)
    {
        if (old_hashes[i])
        {
            hash_table_add(ht, old_keys[i], old_values[i]);
        }
    }

    free(ht->allocator, old_hashes);
}

template <typename Key_Type, typename Value_Type>
bool hash_table_find(Hash_Table<Key_Type, Value_Type>* ht, Key_Type key, Value_Type* vptr)
{
    uint64_t hash = hash_key(key);
    uint64_t hash_index = hash % ht->capacity;
    int64_t iteration_count = 0;

    while (iteration_count < ht->capacity)
    {
        if (ht->hashes[hash_index] == hash &&
            ht->keys_equal(ht->keys[hash_index], key))
        {
            *vptr = ht->values[hash_index];
            return true;
        }
        else if (ht->hashes[hash_index] == 0)
        {
            return false;
        }

        hash_index++;
        iteration_count++;
    }

    return false;
}
