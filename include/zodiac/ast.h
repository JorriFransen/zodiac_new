#pragma once

#include "array.h"
#include "struct_predecls.h"

namespace Zodiac
{

    enum class AST_Node_Kind
    {
        INVALID,

        MODULE,
        DECLARATION,
        STATEMENT,
        EXPRESSION,
    };

    struct AST_Node
    {
        AST_Node_Kind kind = AST_Node_Kind::INVALID;
    };

    struct AST_Module : public AST_Node
    {
        static AST_Node_Kind _kind;
        Array<AST_Declaration*> declarations = {};
    };

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind);

    template <typename T>
    T* ast_node_new(Allocator* allocator)
    {
        T* result = alloc_type<T>(allocator);
        ast_node_init(result, T::_kind);
        return result;
    }

    AST_Node* ast_create_from_parsed_file(Allocator* allocator, Parsed_File* parsed_file);
    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn);

    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls);

    void ast_print(AST_Node* ast_node);
}
