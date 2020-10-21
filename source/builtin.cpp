#include "builtin.h"

#include "ast.h"
#include "scope.h"

namespace Zodiac
{

uint64_t Builtin::pointer_size = 64;

#define DEFINE_BUILTIN_ATOM(name) Atom Builtin::atom_ ##name = {};
#define DEFINE_BUILTIN_TYPE(name, kind, size, signed) Atom Builtin::atom_ ##name = {};
BUILTIN_TYPE_LIST
BUILTIN_ATOM_LIST
#undef DEFINE_BUILTIN_ATOM
#undef DEFINE_BUILTIN_TYPE

#define DEFINE_BUILTIN_TYPE(name, kind, size, signed) AST_Type *Builtin::type_ ##name = {};
BUILTIN_TYPE_LIST
#undef DEFINE_BUILTIN_TYPE

AST_Type *Builtin::type_ptr_u8 = {};

#include <stdio.h>

void builtin_initialize_atoms(Atom_Table *at)
{
#define DEFINE_BUILTIN_ATOM(name) Builtin::atom_ ##name = atom_get(at, #name);
#define DEFINE_BUILTIN_TYPE(name, kind, size, sign) Builtin::atom_ ##name = atom_get(at, #name);
    BUILTIN_TYPE_LIST
    BUILTIN_ATOM_LIST
#undef DEFINE_BUILTIN_ATOM
#undef DEFINE_BUILTIN_TYPE
}

void builtin_initialize_types(Allocator *allocator, Build_Data *build_data)
{
#define DEFINE_BUILTIN_TYPE(name, kind, size, sign) \
    Builtin::type_ ##name = builtin_initialize_type(allocator, Builtin_Type_Kind::kind, size, sign);

    BUILTIN_TYPE_LIST

#undef DEFINE_BUILTIN_TYPE

    Builtin::type_ptr_u8 = build_data_find_or_create_pointer_type(allocator, build_data,
                                                                  Builtin::type_u8);
}

AST_Type *builtin_initialize_type(Allocator *allocator, Builtin_Type_Kind kind, uint64_t size,
                                  bool sign)
{
    AST_Type *result = nullptr;

    switch (kind)
    {
        case Builtin_Type_Kind::INVALID: assert(false);

        case Builtin_Type_Kind::VOID:
        {
            result = ast_type_new(allocator, AST_Type_Kind::VOID, size);
            break;
        }

        case Builtin_Type_Kind::INTEGER:
        {
            result = ast_integer_type_new(allocator, size, sign);
            break;
        }

        case Builtin_Type_Kind::FLOAT:
        {
            result = ast_float_type_new(allocator, size);
            break;
        }

        case Builtin_Type_Kind::BOOL:
        {
            assert(!sign);
            result = ast_bool_type_new(allocator, size);
            break;
        }
    }

    assert(result);

    if (kind != Builtin_Type_Kind::VOID) assert(result->bit_size);
    result->flags |= AST_NODE_FLAG_RESOLVED_ID;
    result->flags |= AST_NODE_FLAG_TYPED;
    result->flags |= AST_NODE_FLAG_SIZED;

    return result;
}

Array<AST_Declaration *> builtin_populate_scope(Allocator *allocator, Scope *global_scope)
{
    assert(global_scope);

    File_Pos fp = { 0, 0, 0, string_ref("<builtin type>") };

    #define DEFINE_BUILTIN_TYPE(name, kind, size, signed) { \
        auto ident = ast_identifier_new(allocator, Builtin::atom_##name, fp, fp); \
        auto decl = ast_type_declaration_new(allocator, Builtin::type_##name, ident); \
        decl->flags |= AST_NODE_FLAG_TYPED; \
        scope_add_declaration(global_scope, decl); \
    } 

    BUILTIN_TYPE_LIST

    #undef DEFINE_BUILTIN_TYPE

    auto decls_to_resolve = array_create<AST_Declaration *>(allocator, 2);

    // @TODO: @CLEANUP: Use the targetplatform enum here
    bool platform_linux = false;
    bool platform_windows = false;

#if linux
    platform_linux = true;
#elif _WIN32
    platform_windows = true
#else
        assert(false);
#endif

    fp.file_name = string_ref("<builtin PLATFORM_LINUX>");
    auto ident_PLATFORM_LINUX = ast_identifier_new(allocator, Builtin::atom_PLATFORM_LINUX, fp, fp);
    auto expr_PLATFORM_LINUX = ast_boolean_literal_expression_new(allocator, platform_linux, fp, fp);
    auto decl_PLATFORM_LINUX = ast_constant_declaration_new(allocator, ident_PLATFORM_LINUX,
                                                            nullptr, expr_PLATFORM_LINUX, fp, fp);
    decl_PLATFORM_LINUX->decl_flags |= AST_DECL_FLAG_GLOBAL;
    scope_add_declaration(global_scope, decl_PLATFORM_LINUX);
    array_append(&decls_to_resolve, decl_PLATFORM_LINUX);

    auto ident_PLATFORM_WINDOWS = ast_identifier_new(allocator, Builtin::atom_PLATFORM_WINDOWS,
                                                     fp, fp);
    auto expr_PLATFORM_WINDOWS = ast_boolean_literal_expression_new(allocator, platform_windows,
                                                                    fp, fp);
    auto decl_PLATFORM_WINDOWS = ast_constant_declaration_new(allocator, ident_PLATFORM_WINDOWS,
                                                              nullptr, expr_PLATFORM_WINDOWS, fp, fp);
    decl_PLATFORM_WINDOWS->decl_flags |= AST_DECL_FLAG_GLOBAL;
    scope_add_declaration(global_scope, decl_PLATFORM_WINDOWS);
    array_append(&decls_to_resolve, decl_PLATFORM_WINDOWS);

    return decls_to_resolve;
}


}
