#pragma once

#include "allocator.h"
#include "atom.h"
#include "ast.h"
#include "array.h"

namespace Zodiac
{
    struct Build_Data
    {
        Atom_Table atom_table = {};
        Array<Atom> kw_atoms = {};

        Array<AST_Type*> type_table = {};
    };

    void build_data_init(Allocator* allocator, Build_Data* build_data);

    AST_Type *build_data_find_or_create_pointer_type(Allocator *allocator, Build_Data *build_data,
                                                     AST_Type *base_type);
    AST_Type *build_data_find_array_type(Allocator *allocator, Build_Data *build_data,
                                                   AST_Type *elem_type, int64_t element_count);
    AST_Type *build_data_create_array_type(Allocator *allocator, Build_Data *build_data,
                                                   AST_Type *elem_type, int64_t element_count);

    AST_Type* build_data_find_function_type(Build_Data *build_data, Array<AST_Type*> param_types,
                                            AST_Type *return_type);
}
