#include "allocator.h"

void* alloc(Allocator* allocator, int64_t size)
{
    return allocator->alloc(allocator, ALLOCATE, size, nullptr);
}

void* free(Allocator* allocator, void* ptr)
{
    return allocator->alloc(allocator, FREE, 0, ptr);
}
