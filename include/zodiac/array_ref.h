
#pragma once

#include "allocator.h"
#include "array.h"

#include "common.h"

namespace Zodiac
{

template <typename Element_Type>
struct Array_Ref
{
    const Element_Type *data = nullptr;
    int64_t count = 0;

    Array_Ref() : data(nullptr), count(0) {}

    template <size_t N>
    constexpr Array_Ref(const Element_Type (&c_arr)[N]) : data(c_arr), count(N) {}

    constexpr Array_Ref(const Array<Element_Type> &arr) : data(arr.data), count(arr.count) {}

    Element_Type& operator[](int64_t index)
    {
        assert(index < count);
        return data[index];
    }

    const Element_Type& operator[](int64_t index) const
    {
        assert(index < count);
        return data[index];
    }
};


}
