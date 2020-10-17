
#pragma once

#include "array.h"

namespace Zodiac
{
    struct Allocator;
    struct String;
    struct String_Builder;

    struct Build_Data;

    struct PT_Node;
    struct Identifier_PTN;
    struct Declaration_PTN;
    struct Statement_PTN;
    struct Expression_PTN;
    struct Parameter_PTN;

    struct Parsed_File
    {
        bool valid = true;
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

    union Integer_Literal
    {
        uint64_t u64;
        int64_t s64;

        uint32_t u32;
        int32_t s32;

        uint16_t u16;
        int32_t s16;

        uint8_t u8;
        int8_t s8;
    };

    struct Const_Value
    {
        AST_Type *type = nullptr;

        Integer_Literal integer = {};
    };

}

