#include "hash_table.h"

#include <string.h>

uint64_t hash_key(const char* str)
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
