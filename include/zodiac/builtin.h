#pragma once

#include "struct_predecls.h"
#include "atom.h"

namespace Zodiac
{
    struct Builtin
    {
        static uint64_t pointer_size;

        static Atom atom_exit;
        static Atom atom_naked;
        static Atom atom_s64;

        static AST_Type *type_void;
        static AST_Type *type_s64;
        static AST_Type *type_u64;
    };

    void builtin_initialize_atoms(Atom_Table* at);
    void builtin_initialize_types(Allocator *allocator);

    void builtin_populate_scope(Allocator *allocator, Scope *global_scope);
}
