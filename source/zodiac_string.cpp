
#include "zodiac_string.h"

#include <cwchar>
#include <stdio.h>
#include <string.h>

namespace Zodiac
{

int64_t string_length(String string)
{
    return string.length;
}

const String string_copy(Allocator *allocator, const String& string, int64_t offset,
                         int64_t length)
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

const String string_copy(Allocator *allocator, const String& string)
{
    return string_copy(allocator, string, 0, string.length);
}

const String string_copy(Allocator *allocator, const char *cstr, int64_t length)
{
    return string_copy(allocator, { (char*)cstr, length });
}

const String string_copy(Allocator *allocator, const char *cstr)
{
    return string_copy(allocator, { (char*)cstr, (int64_t)strlen(cstr) });
}

void string_free(Allocator *allocator, String &str)
{
    free(allocator, str.data);
}

const String string_append(Allocator *allocator, const String &lhs, const String &rhs)
{
    auto new_len = lhs.length + rhs.length;
    String new_str = { alloc_array<char>(allocator, new_len + 1), new_len };
    memcpy(new_str.data, lhs.data, lhs.length);
    memcpy(new_str.data + lhs.length, rhs.data, rhs.length);
    new_str.data[new_str.length] = '\0';

    return new_str;
}

const String string_append(Allocator *allocator, const String &lhs, const char *cstr)
{
    return string_append(allocator, lhs, string_ref(cstr));
}

const String string_ref(const char *cstr)
{
    String result = { (char*)cstr, (int64_t)strlen(cstr) };
    return result;
}

const String string_ref(const char *cstr, int64_t length)
{
    String result = { (char*)cstr, length };
    return result;
}

const String string_from_int(Allocator *allocator, int64_t val)
{
    assert(val >= 0);

    const auto buf_size = 32;
    char buf[buf_size];
    buf[buf_size - 1] = '\0';

    int64_t length = 0;

    if (val != 0) {

        while (val > 0) {
            auto d = val % 10;
            buf[buf_size - 2 - length] = (char)d + '0';
            val /= 10;
            length++;
        }

    } else {
        buf[buf_size - 2] = '0';
        length = 1;
    }

    return string_copy(allocator, &buf[buf_size - 1 - length], length);

}

int64_t string_last_index_of(const String &string, char c)
{
    for (int64_t i = string.length - 1; i >= 0; i--) {
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

    for (int64_t str_i = 0; str_i < str.length; str_i++) {

        auto remaining_length = str.length - str_i;
        if (sub_str.length > remaining_length) return false;

        bool match = true;
        for (int64_t substr_i = 0; substr_i < sub_str.length; substr_i++) {

            if (str[str_i + substr_i] != sub_str[substr_i]) {
                match = false;
                break;
            }
        }

        if (match) return true;
    }

    return false;
}

bool string_starts_with(const String &a, const char *b)
{
    return string_starts_with(a, string_ref(b));
}

bool string_starts_with(const String &a, const String &b)
{
    if (b.length > a.length) return false;
    if (a == b) return true;

    for (int64_t i = 0; i < b.length; i++) {
        if (a[i] != b[i]) return false;
    }

    return true;
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

    for (int64_t i = 0; i < b.length; i++) {
        if (a[i + offset] != b[i]) return false;
    }

    return true;
}

bool string_equal(const String &a, const char *b)
{
    return string_equal(a, string_ref(b));
}

bool string_equal(const String &a, const String &b)
{
    if (a == b) return true;
    if (a.length != b.length) return false;

    for (int64_t i = 0; i < a.length; i++) {
        if (a[i] != b[i]) return false;
    }

    return true;
}

const String string_print_format(Allocator *allocator, const char *fmt, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);
    auto size = vsnprintf(nullptr, 0, fmt, args_copy);
    va_end(args_copy);

    char *buf = alloc_array<char>(allocator, size + 1);
    assert(buf);

    auto written_size = vsnprintf(buf, size + 1, fmt, args);
    assert(written_size <= size);

    return string_ref(buf, written_size);
}

const Unicode_String unicode_string_ref(const wchar_t *utf16_str, int64_t char_count)
{
    Unicode_String result = {};
    result.wchars = (wchar_t*)utf16_str;
    result.length = char_count;
    return result;
}

const Unicode_String unicode_string_ref(const wchar_t *utf16_str)
{
    return unicode_string_ref(utf16_str, wcslen(utf16_str));
}

const Unicode_String string_append(Allocator *allocator, const Unicode_String &lhs,
                                   const Unicode_String &rhs)
{
    auto new_len = lhs.length + rhs.length;

    Unicode_String new_str;
    new_str.wchars = alloc_array<wchar_t>(allocator, new_len + 1);
    new_str.length = new_len;

    memcpy(new_str.wchars, lhs.wchars, lhs.length * sizeof(wchar_t));
    memcpy(new_str.wchars + lhs.length, rhs.wchars, rhs.length * sizeof(wchar_t));
    new_str.wchars[new_str.length] = L'\0';

    return new_str;
}

const Unicode_String string_append(Allocator *allocator, const Unicode_String &lhs,
                                   const wchar_t *utf16_str)
{
    return string_append(allocator, lhs, unicode_string_ref(utf16_str));
}

}
