
#include "ast.h"

#include "parse_tree_node.h"

#include <cassert>

namespace Zodiac
{

AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind)
    {
        assert(ast_node);
        ast_node->kind = kind;
    }

    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls)
    {
        auto result = ast_node_new<AST_Module>(allocator);

        result->declarations = decls;
        
        return result;
    }

    AST_Node* ast_create_from_parsed_file(Allocator* allocator, Parsed_File* parsed_file)
    {
        assert(allocator);
        assert(parsed_file);

        Array<AST_Declaration*> global_decls = {};
        array_init(allocator, &global_decls);

        for (int64_t i = 0; i < parsed_file->declarations.count; i++)
        {
            AST_Declaration* ast_decl =
                ast_create_declaration_from_ptn(allocator, parsed_file->declarations[i]);
            assert(ast_decl);
            array_append(&global_decls, ast_decl);
        }

        assert(global_decls.count);

        AST_Module* ast_module = ast_module_new(allocator, global_decls);
        assert(ast_module);
        return ast_module;
    }

    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn)
    {
        assert(allocator);
        assert(ptn);
        
        assert(false);

        return nullptr;
    }

    void ast_print(AST_Node* ast_node)
    {
        assert(ast_node);
        assert(false);
    }
}
