#include "builtin.h"

#include "ast.h"

namespace Zodiac
{

uint64_t Builtin::pointer_size = 64;

Atom Builtin::atom_exit = {};
Atom Builtin::atom_naked = {};

AST_Type *Builtin::type_void = nullptr;
AST_Type *Builtin::type_s64 = nullptr;
AST_Type *Builtin::type_u64 = nullptr;

void builtin_initialize_atoms(Atom_Table* at)
{
    Builtin::atom_exit = atom_get(at, "exit");
    Builtin::atom_naked = atom_get(at, "naked");
}

void builtin_initialize_types(Allocator *allocator)
{
    Builtin::type_void = ast_type_new(allocator, AST_Type_Kind::VOID, 0);;

    Builtin::type_s64 = ast_integer_type_new(allocator, 64, true);
    Builtin::type_u64 = ast_integer_type_new(allocator, 64, false);
}

}
