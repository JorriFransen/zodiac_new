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

#include <stdio.h>

void builtin_initialize_atoms(Atom_Table* at)
{
#define DEFINE_BUILTIN_ATOM(name) Builtin::atom_ ##name = atom_get(at, #name);
#define DEFINE_BUILTIN_TYPE(name, kind, size, sign) Builtin::atom_ ##name = atom_get(at, #name);
    BUILTIN_TYPE_LIST
    BUILTIN_ATOM_LIST
#undef DEFINE_BUILTIN_ATOM
#undef DEFINE_BUILTIN_TYPE
}

void builtin_initialize_types(Allocator *allocator)
{
#define DEFINE_BUILTIN_TYPE(name, kind, size, sign) \
    Builtin::type_ ##name = builtin_initialize_type(allocator, Builtin_Type_Kind::kind, size, sign);

    BUILTIN_TYPE_LIST

#undef DEFINE_BUILTIN_TYPE
}

AST_Type *builtin_initialize_type(Allocator *allocator, Builtin_Type_Kind kind, uint64_t size,
                                  bool sign)
{
    switch (kind)
    {
        case Builtin_Type_Kind::INVALID: assert(false);

        case Builtin_Type_Kind::VOID: 
            return ast_type_new(allocator, AST_Type_Kind::VOID, size);

        case Builtin_Type_Kind::INTEGER: 
            return ast_integer_type_new(allocator, size, sign);
    }
}

void builtin_populate_scope(Allocator *allocator, Scope *global_scope)
{
    assert(global_scope);

    File_Pos fp = { 0, 0, 0, string_ref("builtin") };

    #define DEFINE_BUILTIN_TYPE(name, kind, size, signed) { \
        auto ident = ast_identifier_new(allocator, Builtin::atom_##name, fp, fp); \
        auto decl = ast_type_declaration_new(allocator, Builtin::type_##name, ident); \
        scope_add_declaration(allocator, global_scope, decl); \
    } 

    BUILTIN_TYPE_LIST

    #undef DEFINE_BUILTIN_TYPE
}


}
