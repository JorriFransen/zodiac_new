#pragma once

#include "allocator.h"
#include "atom.h"
#include "ast.h"
#include "array.h"
#include "command_line_parser.h"
#include "zodiac_error.h"

#define zodiac_disable_msvc_mc_warnings() \
__pragma(warning(push)) \
__pragma(warning(disable:4189)) \
__pragma(warning(disable:4244)) \
__pragma(warning(disable:4267)) \
__pragma(warning(disable:4456))


#ifdef WIN32
	#ifdef _MSC_VER
		zodiac_disable_msvc_mc_warnings()
	#endif // _MSC_VER

	#include "microsoft_craziness.h"

	#ifdef _MSC_VER
		#pragma warning(pop)
	#endif // _MSC_VER
#endif // WIN32

namespace Zodiac
{
    struct BC_Function;

    struct Build_Data
    {
        Options *options = nullptr;
        Atom_Table atom_table = {};
        Array<Atom> kw_atoms = {};

        Array<AST_Type*> type_table = {};
        AST_Module *entry_module = nullptr;

        BC_Function *bc_entry_function = nullptr;
        BC_Function *bc_bytecode_entry_function = nullptr;
        BC_Function *pre_main_func = nullptr;

        Allocator *err_allocator = nullptr;

        Array<Zodiac_Error> errors = {};

        bool redeclaration_error = true;
        bool link_error = false;

        uint64_t bytecode_instruction_count = 0;

#ifdef WIN32
        Windows_SDK_Info sdk_info = {};
#endif
    };

    void build_data_init(Allocator *allocator, Build_Data *build_data,
                         Allocator *err_allocator, Options *options);

    AST_Type *build_data_find_or_create_pointer_type(Allocator *allocator, Build_Data *build_data,
                                                     AST_Type *base_type);
    AST_Type *build_data_find_array_type(Build_Data *build_data, AST_Type *elem_type,
                                         int64_t element_count);
    AST_Type *build_data_create_array_type(Allocator *allocator, Build_Data *build_data,
                                                   AST_Type *elem_type, int64_t element_count);

    AST_Type *build_data_find_function_type(Build_Data *build_data, Array<AST_Type*> param_types,
                                            AST_Type *return_type);
    AST_Type *build_data_find_or_create_function_type(Allocator *allocator,
                                                      Build_Data *build_data,
                                                      Array<AST_Type *> param_types,
                                                      AST_Type *return_type);

    AST_Type  *build_data_find_enum_type(Build_Data *build_data, AST_Declaration *enum_decl);
}
