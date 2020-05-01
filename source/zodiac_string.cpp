
#include "zodiac_string.h"

#include <cassert>
#include <string.h>

namespace Zodiac
{

int64_t string_length(String string)
{
    return string.length;
}

const String copy_string(Allocator* allocator, const String& string)
{
    String new_str = { alloc_array<char>(allocator, string.length + 1), string.length };
    memcpy(new_str.data, string.data, string.length);
    new_str.data[string.length] = '\0';

    return new_str;
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

}
