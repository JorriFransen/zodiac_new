
#pragma once

#include "array.h"

namespace Zodiac
{
    struct Allocator;
    struct String;
    struct String_Builder;

    struct PT_Node;
    struct Declaration_PTN;
    struct Statement_PTN;
    struct Expression_PTN;
    struct Parameter_PTN;

    struct Parsed_File
    {
        Array<Declaration_PTN*> declarations = {};
    };

    struct Scope;

    struct AST_Node;
    struct AST_Module;
    struct AST_Identifier;
    struct AST_Declaration;
    struct AST_Statement;
    struct AST_Expression;
    struct AST_Type_Spec;
    struct AST_Type;

    struct Integer_Literal
    {
        int64_t s64 = 0;
    };
}

