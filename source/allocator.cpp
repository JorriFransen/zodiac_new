#include "allocator.h"

#include "common.h"

namespace Zodiac
{

void* alloc(Allocator* allocator, int64_t size)
{
    return allocator->alloc(allocator, ALLOCATE, size, nullptr);
}

void* free(Allocator* allocator, void* ptr)
{
    return allocator->alloc(allocator, FREE, 0, ptr);
}

void* align_up(void* ptr, uint64_t align)
{
    assert(is_power_of_two(align));

    uint64_t addr = (uint64_t)ptr;
    addr = (addr + (align - 1)) & -align;
    assert(addr >= (uint64_t)ptr);
    return (void*)addr;
}

}
