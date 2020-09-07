
#pragma once

#include <cassert>

namespace Zodiac
{
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
