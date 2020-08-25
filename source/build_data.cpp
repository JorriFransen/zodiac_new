
#include "build_data.h"
#include "token.h"

namespace Zodiac
{
    void build_data_init(Allocator* allocator, Build_Data* build_data)
    {
        atom_table_init(allocator, &build_data->atom_table);

        const auto kw_token_count = sizeof(KW_Tokens) / sizeof(KW_Token);
        array_init(allocator, &build_data->kw_atoms, kw_token_count);

        for (uint64_t i = 0; i < kw_token_count; i++)
        {
            auto atom = atom_get(&build_data->atom_table, KW_Tokens[i].string);
            array_append(&build_data->kw_atoms, atom);
        }

        array_init(allocator, &build_data->type_table);
    }

    AST_Type *build_data_find_or_create_pointer_type(Allocator *allocator,
                                                     Build_Data *build_data,
                                                     AST_Type *base_type)
    {
        assert(build_data);

        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {

            auto r_type = build_data->type_table[i];
            if (r_type->kind == AST_Type_Kind::POINTER &&
                r_type->pointer.base == base_type)
            {
                return r_type; 
            } 
        }

        //AST_Type *ptr_type = ast_pointer_type_new(allocator, base_type);
        AST_Type *ptr_type = _ast_find_or_create_pointer_type(allocator, base_type);
        assert(ptr_type);
        array_append(&build_data->type_table, ptr_type);
        return ptr_type;
    }

    AST_Type *build_data_find_or_create_array_type(Allocator *allocator, Build_Data *build_data,
                                                   AST_Type *elem_type, int64_t elem_count)
    {
        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {
            auto r_type = build_data->type_table[i];
            if (r_type->kind == AST_Type_Kind::ARRAY &&
                r_type->array.element_type == elem_type &&
                r_type->array.element_count == elem_count)
            {
                return r_type; 
            }
        }

        AST_Type *arr_type = _ast_create_array_type(allocator, elem_type, elem_count);
        assert(arr_type);
        array_append(&build_data->type_table, arr_type);
        return arr_type;
    }

    AST_Type* build_data_find_function_type(Build_Data *build_data, Array<AST_Type*> param_types,
                                            AST_Type *return_type)
    {
        assert(build_data);
        assert(param_types.count >= 0);
        assert(return_type);

        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {
            auto r_type = build_data->type_table[i];

            if (r_type->kind == AST_Type_Kind::FUNCTION)
            {
                if (r_type->function.return_type == return_type &&
                    r_type->function.param_types.count == param_types.count)
                {
                    bool param_match = true;
                    for (int64_t j = 0; j < param_types.count; j++)
                    {
                        if (r_type->function.param_types[j] != param_types[j])
                        {
                            param_match = false;
                            break;
                        }
                    }

                    if (param_match) return r_type;
                }
            }
        }

        return nullptr;
    }
}
