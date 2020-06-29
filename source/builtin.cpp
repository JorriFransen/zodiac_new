#include "builtin.h"

#include "ast.h"

namespace Zodiac
{

Atom Builtin::atom_exit = {};
Atom Builtin::atom_naked = {};

AST_Type *Builtin::type_s64 = nullptr;
AST_Type *Builtin::type_u64 = nullptr;

void builtin_initialize_atoms(Atom_Table* at)
{
    Builtin::atom_exit = atom_get(at, "exit");
    Builtin::atom_naked = atom_get(at, "naked");
}

void builtin_initialize_types(Allocator *allocator)
{
    Builtin::type_s64 = ast_integer_type_new(allocator, 64, true);
    Builtin::type_u64 = ast_integer_type_new(allocator, 64, false);
}

}
