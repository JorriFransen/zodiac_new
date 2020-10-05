#include "operator.h"

namespace Zodiac
{
    bool binop_is_cmp(Binary_Operator op)
    {
        return op >= BINOP_FIRST_CMP && op <= BINOP_LAST_CMP;
    }

}
