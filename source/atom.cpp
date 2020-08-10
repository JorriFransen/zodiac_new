#include "atom.h"

#include <stdio.h>

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

static uint64_t _digit_value(char c)
{
    if (c >= '0' && c <= '9')
    {
        return ((uint64_t)c - '0');
    }
    else if (c >= 'a'  && c <= 'f')
    {
        return ((uint64_t)c - 'a') + 10;
    }
    else if (c >= 'A' && c <= 'F')
    {
        return ((uint64_t)c - 'A') + 10;
    }
    else assert(false);

    assert(false);
    return 0;
}

int64_t atom_to_s64(const Atom& atom, uint64_t base /*= 10*/)
{
    uint64_t result = 0;

    uint64_t start_index = 0;
    bool negate = false;
    if (atom.data[0] == '-')
    {
        negate = true;
        start_index = 1;
    }

    for (uint64_t i = start_index; i < atom.length; i++)
    {
        result *= base;
        uint64_t digit_value = _digit_value(atom.data[i]);
        result += digit_value;
    }

    if (negate)
    {
        result = -result;
    }

    return result;
}

uint64_t atom_to_u64(const Atom& atom, uint64_t base /*= 10*/)
{
    uint64_t result = 0;

    for (uint64_t i = 0; i < atom.length; i++)
    {
        result *= base;
        uint64_t digit_value = _digit_value(atom.data[i]);
        result += digit_value;
    }

    return result;
}

}
