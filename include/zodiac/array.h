
#pragma once

#include "allocator.h"

#include <cassert>
#include <stdint.h>

template <typename Element_Type>
struct Array
{
    Element_Type* data = nullptr;
    int64_t count = 0;
    int64_t capacity = 0;
    Allocator* allocator = nullptr;
};

template <typename Element_Type>
void array_init(Allocator* allocator, Array<Element_Type>* array)
{
    assert(false);
}

