
#pragma once

#include <cassert>

namespace Zodiac
{
#define STATIC_ARRAY_LENGTH(sa) (sizeof(sa) / sizeof(sa[0]))

    template <typename T>
    T min(T a, T b)
    {
        if (b < a) return b;
        return a;
    }

    template <typename Ta, typename Tb>
    Ta min(Ta a, Tb b)
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
