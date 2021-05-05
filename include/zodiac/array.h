
#pragma once

#include "allocator.h"

#include "common.h"

#include <stdint.h>
#include <string.h>

namespace Zodiac
{

const int64_t DEFAULT_ARRAY_CAPACITY = 8;

template <typename Element_Type>
struct Array
{
    Element_Type *data = nullptr;
    int64_t count = 0;
    int64_t capacity = 0;
    Allocator *allocator = nullptr;

    Element_Type& operator[](int index)
    {
        assert(index < count);
        return data[index];
    }

    const Element_Type& operator[](int index) const
    {
        assert(index < count);
        return data[index];
    }
};

template <typename Element_Type>
void array_init(Allocator *allocator, Array<Element_Type> *array, int64_t capacity)
{
    if (capacity > 0)
    {
        array->data = alloc_array<Element_Type>(allocator, capacity);
        array->count = 0;
        array->capacity = capacity;
        array->allocator = allocator;
    }
    else assert(capacity == 0);
}

template <typename Element_Type>
void array_init(Allocator *allocator, Array<Element_Type> *array)
{
    array_init(allocator, array, DEFAULT_ARRAY_CAPACITY);
}

template <typename Element_Type>
Array<Element_Type> array_create(Allocator *allocator, int64_t capacity)
{
    Array<Element_Type> result = {};
    array_init(allocator, &result, capacity);
    return result;
}

template <typename Element_Type>
void array_free(Array<Element_Type> *array)
{
    if (array->allocator && array->data)
    {
        free(array->allocator, array->data);
    }
    array->count = 0;
    array->capacity = 0;
    array->allocator = nullptr;
}

template <typename Element_Type>
void array_grow(Array<Element_Type> *array, int64_t new_cap)
{
    assert(new_cap > array->capacity);

    auto new_data = alloc_array<Element_Type>(array->allocator, new_cap);
    memcpy(new_data, array->data, array->count  *sizeof(Element_Type));
    free(array->allocator, array->data);

    array->data = new_data;
    array->capacity = new_cap;
}

template <typename Element_Type>
void array_append(Array<Element_Type> *array, const Element_Type& element)
{
    if (array->count >= (array->capacity - 1))
    {
        array_grow(array, array->capacity * 2);
    }

    array->data[array->count] = element;
    array->count += 1;
}

template <typename Element_Type>
void array_append_unique(Array<Element_Type> *array, const Element_Type& element)
{
    if (!array_contains(array, element)) array_append(array, element);
}

template <typename Element_Type>
void array_unordered_remove(Array<Element_Type> *array, int64_t index)
{
    assert(index < array->count);

    auto last_idx = array->count - 1;
    if (index != last_idx)
    {
        array->data[index] = array->data[last_idx];
    }
    array->count -= 1;
}

template <typename Element_Type>
void array_ordered_remove(Array<Element_Type> *array, int64_t index)
{
    assert(index < array->count);

    if (index != array->count - 1)
    {
        memmove(array->data + index, array->data + index + 1,
                (array->count - index) * sizeof(Element_Type));
    }
    array->count -= 1;
}

template <typename Element_Type>
Element_Type array_first(Array<Element_Type> *array)
{
    assert(array->count >= 1);
    return array->data[0];
}

template <typename Element_Type>
Element_Type *array_last_ptr(Array<Element_Type> *array)
{
    return array_last_ptr(*array);
}

template <typename Element_Type>
Element_Type *array_last_ptr(Array<Element_Type> &array)
{
    if (array.count < 1) {
        return nullptr;
    } else {
        return &array.data[array.count - 1];
    }
}

template <typename Element_Type>
Element_Type array_last(Array<Element_Type> *array)
{
    return array_last(*array);
}

template <typename Element_Type>
Element_Type array_last(const Array<Element_Type> &array)
{
    Element_Type result = {};
    if (array.count < 1) {
        assert(array.count == 0);
        // Return the zeroed result;
    } else {
        assert(array.count >= 1);
        result = array.data[array.count - 1];
    }

    return result;
}

template <typename Element_Type>
bool array_contains(const Array<Element_Type> *array, Element_Type element)
{
    for (int64_t i = 0; i < array->count; i++) {
        if (array->data[i] == element) return true;
    }
    return false;
}

template <typename Element_Type>
int64_t array_index_of(const Array<Element_Type> *array, Element_Type element)
{
    for (int64_t i = 0; i < array->count; i++) {
        if (array->data[i] == element) return i;
    }
    return -1;
}

}
