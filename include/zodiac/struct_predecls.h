
#pragma once

#include "array.h"
#include "atom.h"
#include "file_pos.h"

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
        Atom name = {};
        Array<Declaration_PTN*> declarations = {};
        File_Pos begin_file_pos = {};
        File_Pos end_file_pos = {};
        bool valid = true;
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

    typedef unsigned int uint;

    union Integer_Literal
    {
        uint64_t u64;
        int64_t s64;

        uint32_t u32;
        int32_t s32;

        uint16_t u16;
        int16_t s16;

        uint8_t u8;
        int8_t s8;
    };

    typedef float r32;
    typedef double r64;

    struct Float_Literal
    {
        r32 r32;
        r64 r64;
    };

    struct Const_Value
    {
        AST_Type *type = nullptr;

        union
        {
            Integer_Literal integer = {};
            bool boolean;
        };
    };

    Const_Value create_const_value(AST_Type *type, Integer_Literal integer);

}

