#pragma once

#include "struct_predecls.h"

namespace Zodiac
{
    enum class Scope_Kind
    {
        INVALID,
        MODULE,
    };

    struct Scope
    {
        Scope_Kind kind = Scope_Kind::INVALID;

        AST_Declaration **declarations = nullptr;
        int64_t decl_count = 0;
        int64_t decl_cap = 0;

        Allocator *allocator = nullptr;
    };

    void scope_populate_ast(Allocator *allocator, AST_Node *anode);
    void scope_add_declaration(Scope *scope, AST_Declaration *adecl);
    void scope_grow(Scope *scope);

    Scope *scope_new(Allocator *allocator, Scope_Kind kind, int64_t initial_cap = 4);

    void scope_print(String_Builder *sb, Scope* scope);
}
