#pragma once

#include "struct_predecls.h"

namespace Zodiac
{

    struct Resolve_Result
    {
        int64_t error_count = 0;
    };

    Resolve_Result resolver_resolve_ast_node(AST_Node* ast_node);

    bool resolver_resolve_declaration(Resolve_Result* rr, AST_Declaration* ast_decl);
}
