
#include "common.h"

namespace Zodiac
{
    uint64_t hash_pointer(void *ptr)
    {
        uint64_t hash = (uint64_t)ptr;
        hash = (~hash) + (hash << 21);
        hash = hash ^ (hash >> 24);
        hash = (hash + (hash << 3)) + (hash << 8);
        hash = hash ^ (hash >> 14);
        hash = (hash + (hash << 2)) + (hash << 4);
        hash = hash ^ (hash >> 28);
        hash = hash + (hash << 31);
        return hash;
    }
}
