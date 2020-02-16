#pragma once

#include <cstdint>

struct Allocator;

enum Allocation_Mode
{
    ALLOCATE,
    REALLOCATE,
    FREE,
    FREE_ALL,
};

typedef void* (*Alloc_Function)(Allocator* allocator,
                                Allocation_Mode mode,
                                int64_t size,
                                void* old_ptr);

struct Allocator
{
    Alloc_Function alloc = nullptr; 
};

#define alloc(allocator, size) (allocator->alloc(allocator, ALLOCATE, size, nullptr))
#define free(allocator, ptr) (allocator->alloc(allocator, FREE, 0, ptr))

#define alloc_array(allocator, type, elem_count) ((type*)alloc(allocator, sizeof(type) * elem_count))
