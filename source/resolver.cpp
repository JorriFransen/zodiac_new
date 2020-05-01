#include "resolver.h"

#include "ast.h"

namespace Zodiac
{

    Resolve_Result resolver_resolve_ast_node(AST_Node* ast_node)
    {
        Resolve_Result result = {};

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto ast_module = (AST_Module*)ast_node;
                uint64_t error_count = 0;
                for (int64_t i = 0; i < ast_module->declarations.count; i++)
                {
                    auto decl = ast_module->declarations[0];
                    bool decl_res = resolver_resolve_declaration(&result, decl);

                    if (!decl_res)
                    {
                        error_count++;
                    }
                }
                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);
            case AST_Node_Kind::DECLARATION: assert(false);
            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
        }

        return result;
    }

    bool resolver_resolve_declaration(Resolve_Result* rr, AST_Declaration* ast_decl)
    {
        assert(rr);

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::VARIABLE: assert(false);
            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                assert(false);
                break;
            }

            case AST_Declaration_Kind::STRUCTURE: assert(false);
        }

    }
}
