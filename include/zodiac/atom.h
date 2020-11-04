#pragma once

#include "hash_table.h"
#include "zodiac_string.h"

namespace Zodiac
{

struct Atom
{
    const char *data;
    uint64_t length;
};

struct Atom_Table
{
    Hash_Table<String, Atom> atoms = {};
    Allocator *allocator = nullptr;
};

void atom_table_init(Allocator *allocator, Atom_Table *at);

Atom atom_get(Atom_Table *at, const String& str, uint64_t length);
Atom atom_get(Atom_Table *at, const String& str);

Atom atom_get(Atom_Table *at, const char *cstr, uint64_t length);
Atom atom_get(Atom_Table *at, const char *cstr);

bool operator==(const Atom& a, const Atom& b);

int64_t atom_to_s64(const Atom& atom, uint64_t base = 10);
uint64_t atom_to_u64(const Atom& atom, uint64_t base = 10);

float atom_to_float(const Atom &atom);
double atom_to_double(const Atom &atom);

String string_ref(const Atom &atom);

}
