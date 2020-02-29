#include "atom.h"

namespace Zodiac
{

void atom_table_init(Allocator* allocator, Atom_Table* at)
{
    hash_table_init(allocator, &at->atoms, *hash_table_strings_equal);
    at->allocator = allocator;
}

Atom atom_get(Atom_Table* at, const String& str, uint64_t length)
{
    Atom atom;
    bool found = hash_table_find(&at->atoms, str, &atom);

    if (!found)
    {
        auto str_copy = copy_string(at->allocator, str);
        atom = { str_copy.data, length };
        hash_table_add(&at->atoms, str_copy, atom);
    }

    return atom;
}

Atom atom_get(Atom_Table* at, const String& str)
{
    return atom_get(at, str, string_length(str));
}

Atom atom_get(Atom_Table* at, const char* cstr, uint64_t length)
{
    return atom_get(at, { (char*)cstr, (int64_t)length });
}

Atom atom_get(Atom_Table* at, const char* cstr)
{
    return atom_get(at, cstr, strlen(cstr));
}

bool operator==(const Atom& a, const Atom& b)
{
    return a.data == b.data;
}

}
