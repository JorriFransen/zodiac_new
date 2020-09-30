#include "temp_allocator.h"

#include "common.h"

#include "c_allocator.h"

namespace Zodiac
{

void *_temp_allocate(Allocator *allocator, Allocation_Mode mode, int64_t size, void *old_ptr)
{
    auto ta = (Temp_Allocator*)allocator;

    switch (mode)
    {
        case ALLOCATE:
        {
            assert(ta->capacity - ta->next_index >= size);

            uint8_t *result = &ta->data[ta->next_index];
            ta->next_index += size;

            return result;
            break;
        }

        case REALLOCATE:
        {
            assert(false);
            assert(old_ptr);
            break;
        }

        case FREE:
        {
            assert(old_ptr);
            return nullptr;
            break;
        }

        case FREE_ALL:
        {
            assert(false); 
            return nullptr;
            break;
        }
    }

    assert(false);
    return nullptr;
}

static Temp_Allocator __global_instance__ = {};
static bool __global_instance_initialized__ = false;

Allocator *temp_allocator_get()
{
    if (!__global_instance_initialized__)
    {
        auto ca = c_allocator_get();

        const auto TEMP_MEM_SIZE = 8096;
        uint8_t *data = alloc_array<uint8_t>(ca, TEMP_MEM_SIZE);

        __global_instance__ = { { _temp_allocate }, data, 0, TEMP_MEM_SIZE };

        __global_instance_initialized__ = true;

    }

    return &__global_instance__.allocator;
}

void temp_allocator_reset(Allocator *allocator)
{
    auto ta = (Temp_Allocator*)allocator;
    ta->next_index = 0;
}

}
