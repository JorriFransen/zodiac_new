#include "builtin.h"

#include "ast.h"
#include "scope.h"

namespace Zodiac
{

uint64_t Builtin::pointer_size = 64;

Atom Builtin::atom_exit = {};
Atom Builtin::atom_naked = {};
Atom Builtin::atom_s64 = {};

AST_Type *Builtin::type_void = nullptr;
AST_Type *Builtin::type_s64 = nullptr;
AST_Type *Builtin::type_u64 = nullptr;

void builtin_initialize_atoms(Atom_Table* at)
{
    Builtin::atom_exit = atom_get(at, "exit");
    Builtin::atom_naked = atom_get(at, "naked");
    Builtin::atom_s64 = atom_get(at, "s64");
}

void builtin_initialize_types(Allocator *allocator)
{
    Builtin::type_void = ast_type_new(allocator, AST_Type_Kind::VOID, 0);;

    Builtin::type_s64 = ast_integer_type_new(allocator, 64, true);
    Builtin::type_u64 = ast_integer_type_new(allocator, 64, false);
}

void builtin_populate_scope(Allocator *allocator, Scope *global_scope)
{
    assert(global_scope);

    File_Pos fp = { 0, 0, 0, string_ref("builtin") };

    auto s64_ident = ast_identifier_new(allocator, Builtin::atom_s64, fp, fp);
    auto s64_decl = ast_type_declaration_new(allocator, Builtin::type_s64, s64_ident);
    scope_add_declaration(allocator, global_scope, s64_decl);
}

}
