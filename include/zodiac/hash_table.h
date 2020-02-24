#pragma once

#include "allocator.h"

#include <cassert>

template <typename Key_Type, typename Value_Type>
struct Hash_Table
{
    uint64_t* hashes = nullptr;
    Key_Type* keys = nullptr;
    Value_Type* values = nullptr;

    int64_t capacity = 0;

    Allocator* allocator = {};
};

uint64_t hash_key(const char* str);

#define HASH_TABLE_INITIAL_CAPACITY 16

template <typename Key_Type, typename Value_Type>
void hash_table_init(Allocator* allocator, Hash_Table<Key_Type, Value_Type>* hash_table)
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

    hash_table->capacity = HASH_TABLE_INITIAL_CAPACITY;
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

    assert(false); // grow
}

template <typename Key_Type, typename Value_Type>
bool hash_table_find(Hash_Table<Key_Type, Value_Type>* ht, Key_Type key, Value_Type* vptr)
{
    uint64_t hash = hash_key(key);
    uint64_t hash_index = hash % ht->capacity;
    int64_t iteration_count = 0;

    while (true)
    {
        if (iteration_count >= ht->capacity)
        {
            return false;
        }

        if (ht->hashes[hash_index] == hash)
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
