#include "hash_table.h"

#include <string.h>

namespace Zodiac
{

uint64_t hash_key(const String& str)
{
    auto length = (uint64_t)str.length;

	// 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

    uint64_t hash = 14695981039346656037u;

    for (uint64_t i = 0; i < length; i++)
    {
        hash = hash ^ (str.data[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

uint64_t hash_key(const char *str)
{
    auto length = strlen(str);

	// 64 bit FNV hash
    if (length == 0)
    {
        return 0;
    }

    uint64_t hash = 14695981039346656037u;

    for (uint64_t i = 0; i < length; i++)
    {
        hash = hash ^ (str[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

bool hash_table_strings_equal(const String& a, const String& b)
{
    if (a.length != b.length) return false;

    for (int64_t i = 0; i < a.length; i++)
    {
        if (a.data[i] != b.data[i]) return false;
    }

    return true;
}

}
