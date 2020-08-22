#pragma once

#include "allocator.h"

#include "common.h"

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

    bool operator==(const String &rhs) const
    {
        return data == rhs.data && length == rhs.length;
    }
};

struct Unicode_String
{
    union
    {
        char *data = nullptr;
        wchar_t *wchars;
    };

    int64_t length = 0;

    const wchar_t *c_str() const
    {
        return (wchar_t*)data;
    }
};

int64_t string_length(String string);
const String string_copy(Allocator *allocator, const String& string, int64_t offset, int64_t length);
const String string_copy(Allocator *allocator, const String& string, int64_t length);
const String string_copy(Allocator* allocator, const String& string);
const String string_copy(Allocator *allocator, const char *cstr);

const String string_ref(const char* cstr);
const String string_ref(const char* cstr, int64_t length);

int64_t string_last_index_of(const String &string, char c);

bool string_contains(const String &str, const char *cstr);
bool string_contains(const String &str, const String &sub_str);

bool string_ends_with(const String &a, const char *b);
bool string_ends_with(const String &a, const String &b);

const Unicode_String unicode_string_ref(const wchar_t *utf16_str, int64_t char_count);
}
