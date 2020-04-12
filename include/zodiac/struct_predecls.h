
#pragma once

#include "array.h"

namespace Zodiac
{
    struct Allocator;

    struct PT_Node;
    struct Declaration_PTN;
    struct Statement_PTN;
    struct Expression_PTN;

    struct Parsed_File
    {
        Array<Declaration_PTN*> declarations = {};
    };


    struct AST_Node;
    struct AST_Module;
    struct AST_Declaration;
    struct AST_Statement;
    struct AST_Expression;
}

