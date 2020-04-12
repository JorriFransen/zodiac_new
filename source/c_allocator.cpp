
#include "c_allocator.h"

#include <cstdlib>
#include <cassert>

namespace Zodiac
{

void* _c_allocate(Allocator* allocator, Allocation_Mode mode, int64_t size, void* old_ptr)
{
    assert(allocator);

    switch (mode)
    {
        case ALLOCATE:
        {
            assert(size);
            return malloc(size);
            break;
        }

        case REALLOCATE:
        {
            assert(false);
            break;
        }

        case FREE:
        {
            assert(old_ptr);
            std::free(old_ptr);
            break;
        }

        case FREE_ALL:
        {
            assert(false);
            break;
        }
    }

    return nullptr;
}

static bool instance_initialized = false;

Allocator* c_allocator_get()
{
    if (!instance_initialized) {
        instance = { _c_allocate };
        instance_initialized = true;
    }

    return &instance;
}

}
