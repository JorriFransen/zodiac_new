#pragma once

#include "hash_table.h"
#include "zodiac_string.h"

namespace Zodiac
{

struct Atom
{
    String data = nullptr;
    uint64_t length = 0;
};

struct Atom_Table
{
    Hash_Table<String, Atom> atoms = {};
    Allocator* allocator = nullptr;
};

void atom_table_init(Allocator* allocator, Atom_Table* at);

Atom atom_get(Atom_Table* at, const String& str, uint64_t length);

}
