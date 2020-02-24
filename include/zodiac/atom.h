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
};

void atom_table_init(Allocator* allocator, Atom_Table* at);

Atom atom_get(Atom_Table* at, String str, uint64_t length);

uint64_t atom_hash(String string, uint64_t length);

}
