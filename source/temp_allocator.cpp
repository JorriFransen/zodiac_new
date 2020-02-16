
#include "temp_allocator.h"

#include <cassert>

void* _temp_allocate(Allocator* allocator, Allocation_Mode mode, int64_t size, void* old_ptr)
{
    switch (mode)
    {
        case ALLOCATE:
        {
            assert(false);
            break;
        }

        case REALLOCATE:
        {
            assert(false);
            break;
        }

        case FREE:
        {
            assert(false);
            break;
        }

        case FREE_ALL:
        {
            assert(false); 
            break;
        }
    }

    assert(false);
    return nullptr;
}

Allocator* temp_allocator_get()
{
    if (!__global_instance_initialized__)
    {
        __global_instance__ = { _temp_allocate };
        __global_instance_initialized__ = true;

        assert(false); // Init ta data
    }

    return &__global_instance__;
}
