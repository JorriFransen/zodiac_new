
#pragma once

#include "allocator.h"

#include <cassert>
#include <stdint.h>

const int64_t DEFAULT_ARRAY_CAPACITY = 8;

template <typename Element_Type>
struct Array
{
    Element_Type* data = nullptr;
    int64_t count = 0;
    int64_t capacity = 0;
    Allocator* allocator = nullptr;

    Element_Type& operator[](int index)
    {
        return data[index];
    }
};

template <typename Element_Type>
void array_init(Allocator* allocator, Array<Element_Type>* array)
{
    array->data = alloc_array<Element_Type>(allocator, DEFAULT_ARRAY_CAPACITY);
    array->count = 0;
    array->capacity = DEFAULT_ARRAY_CAPACITY;
    array->allocator = allocator;
}

