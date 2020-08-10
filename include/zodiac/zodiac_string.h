
#pragma once


#include "allocator.h"

#include <cassert>

namespace Zodiac
{

struct String
{
    char* data = nullptr;
    int64_t length = 0;

    char& operator[](int index)
    {
        assert(index < length);
        return data[index];
    }

    const char& operator[](int index) const
    {
        assert(index < length);
        return data[index];
    }
};

int64_t string_length(String string);
const String copy_string(Allocator* allocator, const String& string);

const String string_ref(const char* cstr);
const String string_ref(const char* cstr, int64_t length);

}
