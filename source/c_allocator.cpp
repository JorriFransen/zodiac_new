
#include "c_allocator.h"

#include <cassert>

void* _c_allocate(Allocator* allocator, Allocation_Mode mode, int64_t size, void* old_ptr)
{
    assert(false);
}

Allocator* c_allocator_get()
{
    if (!instance_initialized) {
        instance = { _c_allocate };
        instance_initialized = true;
    }

    return &instance;
}
