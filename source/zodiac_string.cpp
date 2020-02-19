
#include "zodiac_string.h"

#include <cassert>
#include <string.h>

int64_t string_length(String string)
{
    return strlen(string);
}

String copy_string(Allocator* allocator, String string, int64_t length)
{
    char* new_str = alloc_array<char>(allocator, length + 1);

    memcpy(new_str, string, length);
    new_str[length] = '\0';

    return (String)new_str;
}

String copy_string(Allocator* allocator, String string)
{
    return copy_string(allocator, string, string_length(string));
}
