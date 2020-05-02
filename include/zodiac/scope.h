#pragma once

#include "struct_predecls.h"

namespace Zodiac
{
    enum class Scope_Kind
    {
        INVALID,
        MODULE,
        PARAMETER,
        BLOCK,
    };

    struct Scope
    {
        Scope_Kind kind = Scope_Kind::INVALID;

        AST_Declaration **declarations = nullptr;
        int64_t decl_count = 0;
        int64_t decl_cap = 0;

        Scope *parent = nullptr;

        Allocator *allocator = nullptr;
    };

    void scope_populate_ast(Allocator *allocator, AST_Node *anode, Scope *parent_scope);
    void scope_populate_declaration_ast(Allocator *allocator, AST_Declaration *ast_decl, Scope *parent_scope);
    void scope_populate_statement_ast(Allocator *allocator, AST_Statement *ast_stmt, Scope *parent_scope);
    void scope_populate_expression_ast(Allocator* allocator, AST_Expression *ast_expr);
    void scope_add_declaration(Scope *scope, AST_Declaration *adecl);
    void scope_grow(Scope *scope);

    Scope *scope_new(Allocator *allocator, Scope_Kind kind, Scope *parent, int64_t initial_cap = 4);

    void scope_print(String_Builder *sb, Scope* scope, int64_t indent = 0);
}
