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
        auto str_copy = copy_string(at->allocator, str, length);
        atom = { str_copy, length };
        hash_table_add(&at->atoms, str_copy, atom);
    }

    return atom;
}

}
