
#include "zodiac_string.h"

#include <string.h>

namespace Zodiac
{

int64_t string_length(String string)
{
    return string.length;
}

const String string_copy(Allocator *allocator, const String& string, int64_t offset, int64_t length)
{
    assert(string.length - offset >= length);

    String new_str = { alloc_array<char>(allocator, length + 1), length };
    memcpy(new_str.data, string.data + offset, length);
    new_str.data[new_str.length] = '\0';

    return new_str;
}

const String string_copy(Allocator *allocator, const String& string, int64_t length)
{
    return string_copy(allocator, string, 0, length);
}

const String string_copy(Allocator* allocator, const String& string)
{
    return string_copy(allocator, string, 0, string.length);
}

const String string_copy(Allocator *allocator, const char *cstr)
{
    String wrapped = { (char*)cstr, (int64_t)strlen(cstr) };
    return string_copy(allocator, wrapped);
}

const String string_ref(const char* cstr)
{
    String result = { (char*)cstr, (int64_t)strlen(cstr) };
    return result;
}

const String string_ref(const char* cstr, int64_t length)
{
    String result = { (char*)cstr, length };
    return result;
}

int64_t string_last_index_of(const String &string, char c)
{
    for (int64_t i = string.length - 1; i >= 0; i--)
    {
        if (string.data[i] == c) return i;
    }

    return -1;
}

bool string_contains(const String &str, const char *cstr)
{
    return string_contains(str, string_ref(cstr));
}

bool string_contains(const String &str, const String &sub_str)
{
    if (sub_str.length > str.length) return false;
    if (str == sub_str) return true;
    
    for (int64_t str_i = 0; str_i < str.length; str_i++)
    {
        auto remaining_length = str.length - str_i;
        if (sub_str.length > remaining_length) return false;

        bool match = true;
        for (int64_t substr_i = 0; substr_i < sub_str.length; substr_i++)
        {
            if (str[str_i + substr_i] != sub_str[substr_i]) 
            {
                match = false;
                break;
            }
        }

        if (match) return true;
    }

    return false;
}

bool string_ends_with(const String &a, const char *b)
{
    return string_ends_with(a, string_ref(b));
}

bool string_ends_with(const String &a, const String &b)
{
    if (b.length > a.length) return false;
    if (a == b) return true;

    auto offset = a.length - b.length;

    for (int64_t i = 0; i < b.length; i++)
    {
        if (a[i + offset] != b[i]) return false;
    }

    return true;
}

const Unicode_String unicode_string_ref(const wchar_t *utf16_str, int64_t char_count)
{
    Unicode_String result = {};
    result.wchars = (wchar_t*)utf16_str;
    result.length = char_count;
    return result;
}

}
