
#pragma once

#include "allocator.h"

#include <cassert>
#include <stdint.h>
#include <string.h>

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

    const Element_Type& operator[](int index) const
    {
        return data[index];
    }
};

template <typename Element_Type>
void array_init(Allocator* allocator, Array<Element_Type>* array, int64_t capacity)
{
    array->data = alloc_array<Element_Type>(allocator, capacity);
    array->count = 0;
    array->capacity = DEFAULT_ARRAY_CAPACITY;
    array->allocator = allocator;
}

template <typename Element_Type>
void array_free(Array<Element_Type>* array)
{
    free(array->allocator, array->data);
    array->count = 0;
    array->capacity = 0;
    array->allocator = nullptr;
}

template <typename Element_Type>
void array_init(Allocator* allocator, Array<Element_Type>* array)
{
    array_init(allocator, array, DEFAULT_ARRAY_CAPACITY);
}

template <typename Element_Type>
void array_grow(Array<Element_Type>* array, int64_t new_cap)
{
    assert(new_cap > array->capacity);

    auto new_data = alloc_array<Element_Type>(array->allocator, new_cap);
    memcpy(new_data, array->data, array->count * sizeof(Element_Type));
    free(array->allocator, array->data);

    array->data = new_data;
    array->capacity = new_cap;
}

template <typename Element_Type>
void array_append(Array<Element_Type>* array, const Element_Type& element)
{
    if (array->count >= (array->capacity - 1))
    {
        array_grow(array, array->capacity * 2);
    }

    array->data[array->count] = element;
    array->count += 1;
}
