#pragma once

#include <cstdint>
#include <new>

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


void* alloc(Allocator* allocator, int64_t size);
void* free(Allocator* allocator, void* ptr);

template <typename Type>
Type* alloc_type(Allocator* allocator)
{
    auto mem = (Type*)alloc(allocator, sizeof(Type));
    auto result = new (mem) Type();
    return mem;
}

template <typename Element_Type>
Element_Type* alloc_array(Allocator* allocator, int64_t element_count)
{
    return (Element_Type*)alloc(allocator, sizeof(Element_Type) * element_count);
}

template <typename T>
bool is_power_of_two(T v)
{
    if (v <= 0) return false;
    return (v & (v - 1)) == 0;
}

void* align_up(void* ptr, uint64_t align);
