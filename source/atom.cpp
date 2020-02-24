#include "atom.h"

namespace Zodiac
{

void atom_table_init(Allocator* allocator, Atom_Table* at)
{
    hash_table_init(allocator, &at->atoms);
}

Atom atom_get(Atom_Table* at, String str, uint64_t length)
{
    Atom atom;
    bool found = hash_table_find(&at->atoms, str, &atom);

    if (!found)
    {
        atom = { str, length };
        hash_table_add(&at->atoms, str, atom);
    }

    return atom;
}

uint64_t atom_hash(String string, uint64_t length)
{
	// 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

// #ifdef __linux
// #pragma clang diagnostic push
// #pragma clang diagnostic ignored "-Wimplicitly-unsigned-literal"
// #endif
    uint64_t hash = 14695981039346656037u;
// #ifdef __linux
// #pragma clang diagnostic pop
// #endif

    for (uint64_t i = 0; i < length; i++)
    {
        hash = hash ^ (string[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

}
