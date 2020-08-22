
#pragma once

#include <stdlib.h>

#include <cassert>

#undef assert
#define assert(expr) \
    ((expr) ? (void)0 :\
     __zodiac_assert_fail((#expr), __FILE__, __LINE__, __func__))

namespace Zodiac
{
    void __zodiac_assert_fail(const char *expr, const char *file, int line, const char *func);

    template <typename T>
    T min(T a, T b)
    {
        if (b < a) return b;
        return a;
    }

    template <typename T>
    T max (T a, T b)
    {
        if (b > a) return b;
        return a;
    }
}
