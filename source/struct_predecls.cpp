
#include "struct_predecls.h"

namespace Zodiac
{
    Const_Value create_const_value(AST_Type *type, Integer_Literal integer)
    {
        Const_Value result = {
            .type = type,
        };

        result.integer = integer;

        return result;
    }
}
