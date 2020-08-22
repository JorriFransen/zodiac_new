#include "common.h"

namespace Zodiac
{

    void __zodiac_assert_fail(const char *expr, const char *file, int line, const char *func)
    {
        __assert_fail(expr, file, line, func);
    }

}
