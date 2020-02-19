
#pragma once

#include "allocator.h"

#include <cassert>

template <typename Key_Type, typename Value_Type>
struct Hash_Table
{
    
};

template <typename Key_Type, typename Value_Type>
void hash_table_init(Allocator* allocator, Hash_Table<Key_Type, Value_Type>* hash_table)
{
    assert(allocator);
    assert(hash_table);
}
