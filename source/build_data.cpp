
#include "build_data.h"
#include "token.h"

#ifdef WIN32
#define MICROSOFT_CRAZINESS_IMPLEMENTATION

	#ifdef _MSC_VER
		zodiac_disable_msvc_mc_warnings()
	#endif // _MSC_VER

	#include "microsoft_craziness.h"

	#ifdef _MSC_VER
		#pragma warning(disable:4189)
	#endif // _MSC_VER
#endif // WIN32

namespace Zodiac

{
    void build_data_init(Allocator *allocator, Build_Data *build_data,
                         Allocator *err_allocator, Options *options)
    {

#ifdef WIN32
        build_data->sdk_info = find_visual_studio_and_windows_sdk();
#endif // WIN32
        build_data->options = options;

        atom_table_init(allocator, &build_data->atom_table);

        const auto kw_token_count = sizeof(KW_Tokens) / sizeof(KW_Token);
        array_init(allocator, &build_data->kw_atoms, kw_token_count);

        for (uint64_t i = 0; i < kw_token_count; i++)
        {
            auto atom = atom_get(&build_data->atom_table, KW_Tokens[i].string);
            array_append(&build_data->kw_atoms, atom);
        }

        array_init(allocator, &build_data->type_table);
        build_data->entry_module = nullptr;

        build_data->bc_entry_function = nullptr;
        build_data->bc_bytecode_entry_function = nullptr;
        build_data->pre_main_func = nullptr;

        build_data->err_allocator = err_allocator;
        array_init(err_allocator, &build_data->errors);

        build_data->redeclaration_error = false;
        build_data->link_error = false;

        build_data->bytecode_instruction_count = 0;
    }

    AST_Type *build_data_find_or_create_pointer_type(Allocator *allocator,
                                                     Build_Data *build_data,
                                                     AST_Type *base_type)
    {
        assert(build_data);

        if (base_type->pointer_to) {
            assert(base_type->pointer_to->pointer.base == base_type);
            return base_type->pointer_to;
        }

        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {

            auto r_type = build_data->type_table[i];
            if (r_type->kind == AST_Type_Kind::POINTER &&
                r_type->pointer.base == base_type)
            {
                assert(base_type->pointer_to);
                assert(base_type->pointer_to == r_type);
                return r_type;
            }
        }

        AST_Type *ptr_type = _ast_find_or_create_pointer_type(allocator, base_type);
        assert(ptr_type);
        array_append(&build_data->type_table, ptr_type);
        return ptr_type;
    }

    AST_Type *build_data_find_array_type(Build_Data *build_data, AST_Type *elem_type,
                                         int64_t element_count)
    {
        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {
            auto r_type = build_data->type_table[i];
            if (r_type->kind == AST_Type_Kind::ARRAY &&
                r_type->array.element_type == elem_type &&
                r_type->array.element_count == element_count)
            {
                return r_type;
            }
        }

        return nullptr;
    }

    AST_Type *build_data_create_array_type(Allocator *allocator, Build_Data *build_data,
                                                   AST_Type *elem_type, int64_t element_count)
    {
        AST_Type *arr_type = _ast_create_array_type(allocator, elem_type, element_count);
        assert(arr_type);
        array_append(&build_data->type_table, arr_type);
        arr_type->flags |= AST_NODE_FLAG_RESOLVED_ID;
        arr_type->flags |= AST_NODE_FLAG_TYPED;
        return arr_type;
    }

    AST_Type *build_data_find_function_type(Build_Data *build_data, Array<AST_Type*> param_types,
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

    AST_Type *build_data_find_or_create_function_type(Allocator *allocator,
                                                      Build_Data *build_data,
                                                      Array<AST_Type *> param_types,
                                                      AST_Type *return_type)
    {
        AST_Type *result = build_data_find_function_type(build_data, param_types, return_type);

        if (!result) {
            result = ast_function_type_new(allocator, param_types, return_type);
            array_append(&build_data->type_table, result);
        }

        assert(result->kind == AST_Type_Kind::FUNCTION);
        return result;
    }

    AST_Type  *build_data_find_enum_type(Build_Data *build_data, AST_Declaration *enum_decl)
    {
        for (int64_t i = 0; i < build_data->type_table.count; i++)
        {
            auto r_type = build_data->type_table[i];

            if (r_type->kind == AST_Type_Kind::ENUM &&
                r_type->enum_type.declaration == enum_decl)
            {
                return r_type;
            }
        }

        return nullptr;
    }
}
