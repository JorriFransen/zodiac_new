
#include "ast.h"

#include "builtin.h"
#include "parse_tree_node.h"
#include "string_builder.h"
#include "scope.h"

#include <inttypes.h>

namespace Zodiac
{

    AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;
    AST_Node_Kind AST_Identifier::_kind = AST_Node_Kind::IDENTIFIER;
    AST_Node_Kind AST_Declaration::_kind = AST_Node_Kind::DECLARATION;
    AST_Node_Kind AST_Statement::_kind = AST_Node_Kind::STATEMENT;
    AST_Node_Kind AST_Expression::_kind = AST_Node_Kind::EXPRESSION;
    AST_Node_Kind AST_Type_Spec::_kind = AST_Node_Kind::TYPE_SPEC;
    AST_Node_Kind AST_Type::_kind = AST_Node_Kind::TYPE;

    void ast_node_init(AST_Node* ast_node, AST_Node_Kind kind, const File_Pos &begin_fp,
                       const File_Pos &end_fp)
    {
        assert(ast_node);
        ast_node->kind = kind;
        ast_node->begin_file_pos = begin_fp;
        ast_node->end_file_pos = end_fp;
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
                ast_create_declaration_from_ptn(allocator, parsed_file->declarations[i], nullptr);
            assert(ast_decl);
            array_append(&global_decls, ast_decl);
        }

        assert(global_decls.count);

        auto begin_fp = array_first(&global_decls)->begin_file_pos;
        auto end_fp = array_last(&global_decls)->end_file_pos;

        AST_Module* ast_module = ast_module_new(allocator, global_decls, begin_fp, end_fp);
        assert(ast_module);
        return ast_module;
    }

    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Declaration_PTN* ptn,
                                                     Array<AST_Declaration*> *var_decls)
    {
        assert(allocator);

        auto id_begin_fp = ptn->identifier->self.begin_file_pos;
        auto id_end_fp = ptn->identifier->self.end_file_pos;

        assert(ptn->identifier);
        AST_Identifier* ast_ident = ast_identifier_new(allocator, ptn->identifier->atom,
                                                       id_begin_fp, id_end_fp);
        assert(ast_ident);

        auto begin_fp = ast_ident->begin_file_pos;

        switch (ptn->kind)
        {
            case Declaration_PTN_Kind::INVALID: assert(false);

            case Declaration_PTN_Kind::IMPORT:
            {
                auto ast_ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->import.module_ident_expr);
                assert(ast_ident_expr);

                auto end_fp = ast_ident_expr->end_file_pos;

                return ast_import_declaration_new(allocator, ast_ident, ast_ident_expr, begin_fp,
                                                  end_fp);
                break;
            }

            case Declaration_PTN_Kind::VARIABLE:
            {
                AST_Type_Spec* type_expr = nullptr;
                AST_Expression* init_expr = nullptr;

                if (ptn->variable.type_expression)
                {
                    type_expr =
                        ast_create_type_spec_from_ptn(allocator,
                                                      &ptn->variable.type_expression->self);
                    assert(type_expr);
                }

                if (ptn->variable.init_expression)
                {
                    init_expr = ast_create_expression_from_ptn(allocator,
                                                               ptn->variable.init_expression);
                    assert(init_expr);
                }

                assert(type_expr || init_expr);

                File_Pos end_fp;
                if (init_expr) end_fp = init_expr->end_file_pos;
                else end_fp = type_expr->end_file_pos;
                
                auto result = ast_variable_declaration_new(allocator, ast_ident, type_expr,
                                                           init_expr, begin_fp, end_fp);
                if (var_decls)
                {
                    array_append(var_decls, result);
                }

                return result;
                break; 
            }

            case Declaration_PTN_Kind::CONSTANT:
            {
                AST_Type_Spec* ast_type = nullptr;
                if (ptn->constant.type_expression)
                {
                    ast_type =
                        ast_create_type_spec_from_expression_ptn(allocator,
                                                                 ptn->constant.type_expression);
                    assert(ast_type);
                }

                AST_Expression* ast_init_expr = nullptr;
                if (ptn->constant.init_expression)
                {
                    ast_init_expr = ast_create_expression_from_ptn(allocator,
                                                                   ptn->constant.init_expression);
                    assert(ast_init_expr);
                }

                assert(ast_type || ast_init_expr);

                File_Pos end_fp;
                if (ast_init_expr) end_fp = ast_init_expr->end_file_pos;
                else end_fp = ast_type->end_file_pos;

                return ast_constant_declaration_new(allocator, ast_ident, ast_type, ast_init_expr,
                                                    begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::FUNCTION:
            {
                AST_Type_Spec* ast_type =
                    ast_create_type_spec_from_ptn(allocator, &ptn->function.prototype->self);
                assert(ast_type);

                assert(ast_type->kind == AST_Type_Spec_Kind::FUNCTION);
                assert(ptn->function.prototype->parameters.count ==
                       ast_type->function.parameter_type_specs.count);

                Array<AST_Declaration*> ast_param_decls = {};
                if (ast_type->function.parameter_type_specs.count)
                {
                    array_init(allocator, &ast_param_decls,
                               ast_type->function.parameter_type_specs.count);
                    
                    auto& ptn_params = ptn->function.prototype->parameters;

                    for (int64_t i = 0; i < ptn_params.count; i++)
                    {
                        auto param_ts = ast_type->function.parameter_type_specs[i];
                        auto param_decl = ast_create_declaration_from_ptn(allocator,
                                                                          ptn_params[i],
                                                                          param_ts);
                        assert(param_decl);

                        array_append(&ast_param_decls, param_decl);
                    }

                }

                Array<AST_Declaration*> ast_var_decls = {};
                array_init(allocator, &ast_var_decls);

                AST_Statement *ast_body = nullptr;
                if (ptn->function.body)
                {
                    ast_body = ast_create_statement_from_ptn(allocator, ptn->function.body,
                                                             &ast_var_decls);
                    assert(ast_body);
                }

                bool is_naked = ptn->flags & DPTN_FLAG_IS_NAKED;
                bool is_noreturn = ptn->flags & DPTN_FLAG_NORETURN;
                bool is_foreign = ptn->flags & DPTN_FLAG_FOREIGN;

                auto end_fp = ptn->self.end_file_pos;

                return ast_function_declaration_new(allocator, ast_ident, ast_type,
                                                    ast_param_decls, ast_var_decls, ast_body,
                                                    is_naked, is_noreturn, is_foreign,
                                                    begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::STRUCT:
            {
                Array<AST_Declaration*> ast_member_decls = {};
                if (ptn->structure.member_declarations.count)
                {
                    array_init(allocator, &ast_member_decls,
                               ptn->structure.member_declarations.count);

                    for (int64_t i = 0; i < ptn->structure.member_declarations.count; i++)
                    {
                        auto ast_mem_decl =
                            ast_create_declaration_from_ptn(allocator,
                                                            ptn->structure.member_declarations[i],
                                                            nullptr);
                        assert(ast_mem_decl);

                        array_append(&ast_member_decls, ast_mem_decl);
                    }
                }

                Array<AST_Declaration*> ast_parameters = {};
                if (ptn->structure.parameters.count)
                {
                    array_init(allocator, &ast_parameters, ptn->structure.parameters.count);

                    for (int64_t i = 0; i < ptn->structure.parameters.count; i++)
                    {
                        auto ptn_param = ptn->structure.parameters[i];
                        AST_Type_Spec *ast_param_ts = nullptr;

                        if (ptn_param->type_expression)
                        {
                            ast_param_ts =
                                ast_create_type_spec_from_expression_ptn(allocator,
                                                                         ptn_param->type_expression);
                        }


                        assert(ast_param_ts);
                        auto ast_param_decl = ast_create_declaration_from_ptn(allocator,
                                                                              ptn_param,
                                                                              ast_param_ts);
                        assert(ast_param_decl);

                        array_append(&ast_parameters, ast_param_decl);
                    }
                }

                assert(ast_parameters.count >= 0);
                auto end_fp = ptn->self.end_file_pos;

                return ast_structure_declaration_new(allocator, ast_ident, ast_member_decls,
                                                     ast_parameters, begin_fp, end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Declaration* ast_create_declaration_from_ptn(Allocator* allocator, Parameter_PTN* ptn, 
                                                     AST_Type_Spec *type_spec)
    {
        auto id_begin_fp = ptn->identifier->self.begin_file_pos;
        auto id_end_fp = ptn->identifier->self.end_file_pos;

        auto ast_ident = ast_identifier_new(allocator, ptn->identifier->atom, id_begin_fp,
                                            id_end_fp);
        assert(ast_ident);
        auto begin_fp = ast_ident->begin_file_pos;
        auto end_fp = ast_ident->end_file_pos;

        auto result = ast_parameter_declaration_new(allocator, ast_ident, type_spec, begin_fp,
                                                    end_fp);
        assert(result);

        return result;
    }

    AST_Statement* ast_create_statement_from_ptn(Allocator* allocator, Statement_PTN* ptn, 
                                                 Array<AST_Declaration*> *var_decls)
    {
        assert(ptn);
        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;

        switch (ptn->kind)
        {
            case Statement_PTN_Kind::INVALID: assert(false); 

            case Statement_PTN_Kind::BLOCK:
            {
                Array<AST_Statement*> ast_block_stmts = {};
                if (ptn->block.statements.count)
                {
                    array_init(allocator, &ast_block_stmts);

                    for (int64_t i = 0; i < ptn->block.statements.count; i++)
                    {
                        AST_Statement* ast_block_stmt =
                            ast_create_statement_from_ptn(allocator,
                                                          ptn->block.statements[i], var_decls);
                        assert(ast_block_stmt);

                        array_append(&ast_block_stmts, ast_block_stmt);
                    }
                }

                return ast_block_statement_new(allocator, ast_block_stmts, begin_fp, end_fp);
                break; 
            }

            case Statement_PTN_Kind::DECLARATION:
            {
                auto ast_declaration = ast_create_declaration_from_ptn(allocator,
                                                                       ptn->declaration,
                                                                       var_decls);
                assert(ast_declaration);
                return ast_declaration_statement_new(allocator, ast_declaration, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::EXPRESSION:
            {
                auto ast_expression = ast_create_expression_from_ptn(allocator, ptn->expression);
                return ast_expression_statement_new(allocator, ast_expression, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::RETURN:
            {
                AST_Expression* return_val_expr = nullptr;
                if (ptn->return_stmt.expression)
                {
                    return_val_expr = ast_create_expression_from_ptn(allocator,
                                                                     ptn->return_stmt.expression);
                    assert(return_val_expr);
                }

                return ast_return_statement_new(allocator, return_val_expr, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::ASSIGNMENT:
            {
                auto ast_ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->assignment.ident_expression);
                assert(ast_ident_expr);

                auto ast_rhs_expr = 
                    ast_create_expression_from_ptn(allocator, ptn->assignment.rhs_expression);
                assert(ast_rhs_expr);

                return ast_assignment_statement_new(allocator, ast_ident_expr, ast_rhs_expr,
                                                    begin_fp, end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Expression* ast_create_expression_from_ptn(Allocator* allocator, Expression_PTN* ptn)
    {
        assert(allocator);

        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;
       
        switch (ptn->kind)
        {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL:
            {
                AST_Expression* ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->call.ident_expression);
                assert(ident_expr);

                Array<AST_Expression*> arg_exprs = {};
                if (ptn->call.arg_list)
                {
                    array_init(allocator, &arg_exprs);

                    for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++)
                    {
                        AST_Expression* arg_expr =
                            ast_create_expression_from_ptn(allocator,
                                    ptn->call.arg_list->expressions[i]);
                        assert(arg_expr);

                        array_append(&arg_exprs, arg_expr);
                    }
                }

                return ast_call_expression_new(allocator, ident_expr, arg_exprs,
                                               ptn->call.is_builtin, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::IDENTIFIER:
            {
                auto id_begin_fp = ptn->identifier->self.begin_file_pos;
                auto id_end_fp = ptn->identifier->self.end_file_pos;

                AST_Identifier* identifier = ast_identifier_new(allocator, ptn->identifier->atom,
                                                                id_begin_fp, id_end_fp);
                return ast_identifier_expression_new(allocator, identifier, begin_fp, end_fp);

                break;
            }

            case Expression_PTN_Kind::BINARY:
            {
                auto ast_lhs_expr = ast_create_expression_from_ptn(allocator, ptn->binary.lhs);
                assert(ast_lhs_expr);

                auto ast_rhs_expr = ast_create_expression_from_ptn(allocator, ptn->binary.rhs);
                assert(ast_rhs_expr);

                return ast_binary_expression_new(allocator, ptn->binary.op, ast_lhs_expr,
                                                 ast_rhs_expr, begin_fp, end_fp);
                break;
            };

            case Expression_PTN_Kind::UNARY:
            {
                auto ast_operand_expr =
                    ast_create_expression_from_ptn(allocator, ptn->unary.operand_expression);
                assert(ast_operand_expr);

                return ast_unary_expression_new(allocator, ptn->unary.op, ast_operand_expr,
                                                begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::DOT:
            {
                auto ast_parent_expr = ast_create_expression_from_ptn(allocator,
                                                                      ptn->dot.parent_expression);
                assert(ast_parent_expr);

                auto id_begin_fp = ptn->dot.child_identifier->self.begin_file_pos;
                auto id_end_fp = ptn->dot.child_identifier->self.end_file_pos;

                auto ast_child_ident = ast_identifier_new(allocator,
                                                          ptn->dot.child_identifier->atom,
                                                          id_begin_fp, id_end_fp);
                assert(ast_child_ident);

                return ast_dot_expression_new(allocator, ast_parent_expr, ast_child_ident,
                                              begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::COMPOUND:
            {
                Array<AST_Expression*> ast_compound_exprs = {};
                if (ptn->compound.list->expressions.count)
                {
                    array_init(allocator, &ast_compound_exprs);

                    for (int64_t i = 0; i < ptn->compound.list->expressions.count; i++)
                    {
                        auto ast_compound_expr =
                            ast_create_expression_from_ptn(allocator,
                                                           ptn->compound.list->expressions[i]);
                        assert(ast_compound_expr);

                        array_append(&ast_compound_exprs, ast_compound_expr);
                    }
                }

                AST_Type_Spec* ast_ts = nullptr;
                if (ptn->compound.type_expression)
                {
                    ast_ts =
                        ast_create_type_spec_from_expression_ptn(allocator,
                                                                 ptn->compound.type_expression);
                    assert(ast_ts);
                }

                return ast_compound_expression_new(allocator, ast_compound_exprs, ast_ts, begin_fp,
                                                   end_fp);
                break;
            }

            case Expression_PTN_Kind::NUMBER_LITERAL:
            {
                assert(((int64_t)ptn->number_literal.value.u64) == ptn->number_literal.value.s64);
                return ast_number_literal_expression_new(allocator, ptn->number_literal.value.s64,
                                                         begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::STRING_LITERAL:
            {
                return ast_string_literal_expression_new(allocator, ptn->string_literal.atom,
                                                         begin_fp, end_fp);
                break;
            };

            case Expression_PTN_Kind::ARRAY_TYPE: assert(false);
            case Expression_PTN_Kind::POINTER_TYPE: assert(false);

            case Expression_PTN_Kind::POLY_TYPE:
            {
                assert(ptn->poly_type.identifier);

                auto id_begin_file_pos = ptn->poly_type.identifier->self.begin_file_pos;
                auto id_end_file_pos = ptn->poly_type.identifier->self.end_file_pos;

                auto ast_ident = ast_identifier_new(allocator, ptn->poly_type.identifier->atom,
                                                    id_begin_file_pos, id_end_file_pos);
                assert(ast_ident);

                AST_Identifier* ast_spec_ident = nullptr;
                if (ptn->poly_type.specification_identifier)
                {
                    auto spec_id_begin_fp =
                        ptn->poly_type.specification_identifier->self.begin_file_pos;
                    auto spec_id_end_fp =
                        ptn->poly_type.specification_identifier->self.end_file_pos;

                    ast_spec_ident =
                        ast_identifier_new(allocator,
                                           ptn->poly_type.specification_identifier->atom,
                                           spec_id_begin_fp, spec_id_end_fp);
                    assert(ast_spec_ident);
                }

                AST_Declaration *poly_type_decl = ast_poly_type_declaration_new(allocator,
                                                                                ast_ident,
                                                                                ast_spec_ident,
                                                                                begin_fp, end_fp);
                assert(poly_type_decl);
                return ast_poly_identifier_expression_new(allocator, poly_type_decl, begin_fp,
                                                          end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Type_Spec* ast_create_type_spec_from_ptn(Allocator* allocator, PT_Node* ptn)
    {
        auto begin_fp = ptn->begin_file_pos;
        auto end_fp = ptn->end_file_pos;

        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);
            case PT_Node_Kind::IDENTIFIER: assert(false);

            case PT_Node_Kind::FUNCTION_PROTO:
            {
                auto function_ptn = (Function_Proto_PTN*)ptn;

                Array<AST_Type_Spec*> ast_param_types = {};
                if (function_ptn->parameters.count)
                {
                    array_init(allocator, &ast_param_types);
                    for (int64_t i = 0; i < function_ptn->parameters.count; i++)
                    {
                        AST_Type_Spec* ast_param_type = 
                            ast_create_type_spec_from_ptn(allocator,
                                                          &function_ptn->parameters[i]->self);
                        assert(ast_param_type);
                        
                        array_append(&ast_param_types, ast_param_type);
                    }

                }

                AST_Type_Spec* ast_return_type = nullptr;
                if (function_ptn->return_type_expression)
                {
                    ast_return_type =
                        ast_create_type_spec_from_ptn(allocator,
                                                      &function_ptn->return_type_expression->self);
                    assert(ast_return_type);
                }

                return ast_function_type_spec_new(allocator, ast_param_types, ast_return_type,
                                                  begin_fp, end_fp);
                break;
            }

            case PT_Node_Kind::PARAMETER:
            {
                auto param_ptn = (Parameter_PTN*)ptn;
                return ast_create_type_spec_from_expression_ptn(allocator,
                                                                param_ptn->type_expression);
                break;
            }

            case PT_Node_Kind::EXPRESSION_LIST: assert(false);
            case PT_Node_Kind::DECLARATION: assert(false);
            case PT_Node_Kind::STATEMENT: assert(false);

            case PT_Node_Kind::EXPRESSION:
            {
                return ast_create_type_spec_from_expression_ptn(allocator, (Expression_PTN*)ptn);
                break; 
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Type_Spec* ast_create_type_spec_from_expression_ptn(Allocator* allocator,
                                                            Expression_PTN* ptn)
    {
        assert(allocator);

        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;

        switch (ptn->kind)
        {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL:
            {
                assert(!ptn->call.is_builtin);

                auto ast_ident_expr = ast_create_expression_from_ptn(allocator,
                                                                     ptn->call.ident_expression);
                assert(ast_ident_expr);

                Array<AST_Expression*> ast_arg_exprs = {};
                array_init(allocator, &ast_arg_exprs);

                for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++)
                {
                    auto ast_arg_expr =
                        ast_create_expression_from_ptn(allocator,
                                                       ptn->call.arg_list->expressions[i]);
                    assert(ast_arg_expr);

                    array_append(&ast_arg_exprs, ast_arg_expr);
                }

                return ast_templated_type_spec_new(allocator, ast_ident_expr, ast_arg_exprs,
                                                   begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::IDENTIFIER:
            {
                auto id_begin_fp = ptn->identifier->self.begin_file_pos;
                auto id_end_fp = ptn->identifier->self.end_file_pos;

                AST_Identifier* ident = ast_identifier_new(allocator, ptn->identifier->atom,
                                                           id_begin_fp, id_end_fp);
                assert(ident);

                return ast_identifier_type_spec_new(allocator, ident, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::BINARY: assert(false);
            case Expression_PTN_Kind::UNARY: assert(false);

            case Expression_PTN_Kind::DOT:
            {
                auto ast_dot_expr = ast_create_expression_from_ptn(allocator, ptn);
                assert(ast_dot_expr);

                return ast_dot_type_spec_new(allocator, ast_dot_expr, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::COMPOUND: assert(false);
            case Expression_PTN_Kind::NUMBER_LITERAL: assert(false);
            case Expression_PTN_Kind::STRING_LITERAL: assert(false);
                                                      
            case Expression_PTN_Kind::ARRAY_TYPE:
            {
                auto elem_type_ptn = ptn->array_type.element_type_expression;
                auto ast_elem_ts = ast_create_type_spec_from_expression_ptn(allocator,
                                                                            elem_type_ptn);
                assert(ast_elem_ts);

                return ast_array_type_spec_new(allocator, ast_elem_ts, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POINTER_TYPE:
            {
                auto ptn_base_expr = ptn->pointer_type.pointee_type_expression;
                auto ast_base_ts = ast_create_type_spec_from_expression_ptn(allocator,
                                                                            ptn_base_expr);
                assert(ast_base_ts);

                return ast_pointer_type_spec_new(allocator, ast_base_ts, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POLY_TYPE:
            {
                auto id_begin_fp = ptn->poly_type.identifier->self.begin_file_pos;
                auto id_end_fp = ptn->poly_type.identifier->self.end_file_pos;

                auto ast_ident = ast_identifier_new(allocator, ptn->poly_type.identifier->atom,
                                                    id_begin_fp, id_end_fp);
                assert(ast_ident);

                AST_Identifier* ast_spec_ident = nullptr;
                if (ptn->poly_type.specification_identifier)
                {
                    auto spec_id_begin_fp = 
                        ptn->poly_type.specification_identifier->self.begin_file_pos;
                    auto spec_id_end_fp = 
                        ptn->poly_type.specification_identifier->self.end_file_pos;

                    ast_spec_ident =
                        ast_identifier_new(allocator,
                                           ptn->poly_type.specification_identifier->atom,
                                           spec_id_begin_fp, spec_id_end_fp);
                    assert(ast_spec_ident);
                }

                AST_Declaration *ast_decl = ast_poly_type_declaration_new(allocator, ast_ident,
                                                                          ast_spec_ident, begin_fp,
                                                                          end_fp);
                assert(ast_decl);

                return ast_poly_identifier_type_spec_new(allocator, ast_decl, ast_spec_ident,
                                                         begin_fp, end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Identifier* ast_identifier_new(Allocator* allocator, Atom& atom, const File_Pos &begin_fp,
                                       const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Identifier>(allocator, begin_fp, end_fp);

        result->atom = atom;
        result->declaration = nullptr;

        return result;
    }

    AST_Module* ast_module_new(Allocator* allocator, Array<AST_Declaration*> decls,
                               const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Module>(allocator, begin_fp, end_fp);

        result->declarations = decls;
        
        return result;
    }

    AST_Declaration* ast_declaration_new(Allocator* allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier* identifier, const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Declaration>(allocator, begin_fp, end_fp);

        result->kind = kind;
        result->identifier = identifier;

        return result;
    }

    AST_Declaration* ast_import_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                AST_Expression* ident_expr,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::IMPORT, identifier,
                                          begin_fp, end_fp);

        result->import.ident_expression = ident_expr;

        return result;
    }

    AST_Declaration* ast_variable_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr,
                                                  const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::VARIABLE, identifier,
                                          begin_fp, end_fp);

        result->variable.type_spec = type_spec;
        result->variable.init_expression = init_expr;

        return result;
    }

    AST_Declaration* ast_constant_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec,
                                                  AST_Expression* init_expr,
                                                  const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::CONSTANT, identifier,
                                          begin_fp, end_fp);
        result->constant.type_spec = type_spec;
        result->constant.init_expression = init_expr;

        return result;
    }

    AST_Declaration* ast_parameter_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                   AST_Type_Spec* type_spec,
                                                   const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::PARAMETER, identifier,
                                          begin_fp, end_fp);

        result->parameter.type_spec = type_spec;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Allocator* allocator, AST_Identifier* identifier,
                                                  AST_Type_Spec* type_spec, 
                                                  Array<AST_Declaration*> parameter_declarations,
                                                  Array<AST_Declaration*> variable_declarations,
                                                  AST_Statement* body,
                                                  bool is_naked, bool is_noreturn, bool is_foreign,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        if (body) assert(body->kind == AST_Statement_Kind::BLOCK);

        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::FUNCTION, identifier,
                                          begin_fp, end_fp);

        result->function.type_spec = type_spec;
        result->function.parameter_declarations = parameter_declarations;
        result->function.variable_declarations = variable_declarations;
        result->function.body = body;
        result->function.parameter_scope = nullptr;

        if (is_naked)
        {
            result->decl_flags |= AST_DECL_FLAG_IS_NAKED;
        }

        if (is_noreturn)
        {
            result->decl_flags |= AST_DECL_FLAG_NORETURN;
        }

        if (is_foreign)
        {
            result->decl_flags |= AST_DECL_FLAG_FOREIGN;
        }

        return result;
    }

    AST_Declaration* ast_type_declaration_new(Allocator *allocator, AST_Type *type,
                                              AST_Identifier *identifier)
    {
        File_Pos fp = { 0, 0, 0, string_ref("<builtin>") };
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::TYPE, identifier,
                                          fp, fp);

        result->type = type;

        return result;
    }

    AST_Declaration* ast_structure_declaration_new(Allocator* allocator,
                                                   AST_Identifier* identifier,
                                                   Array<AST_Declaration*> member_decls,
                                                   Array<AST_Declaration*> parameters,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::STRUCTURE, identifier,
                                          begin_fp, end_fp);

        result->structure.member_declarations = member_decls;
        result->structure.parameters = parameters;

        result->structure.parameter_scope = nullptr;
        result->structure.member_scope = nullptr;

        return result;
    }

    AST_Declaration* ast_poly_type_declaration_new(Allocator* allocator,
                                                   AST_Identifier* identifier,
                                                   AST_Identifier* spec_ident,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::POLY_TYPE, identifier,
                                          begin_fp, end_fp);

        result->poly_type.specification_identifier = spec_ident;

        return result;
    }

    AST_Statement* ast_statement_new(Allocator* allocator, AST_Statement_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Statement>(allocator, begin_fp, end_fp);

        result->kind = kind;

        return result;
    }

    AST_Statement* ast_block_statement_new(Allocator* allocator, Array<AST_Statement*> statements,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::BLOCK, begin_fp, end_fp);

        result->block.statements = statements;
        result->block.scope = nullptr;

        return result;
    }

    AST_Statement* ast_assignment_statement_new(Allocator* allocator, AST_Expression* ident_expr,
                                                AST_Expression* rhs_expr, const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::ASSIGNMENT, begin_fp,
                                        end_fp);

        result->assignment.identifier_expression = ident_expr;
        result->assignment.rhs_expression = rhs_expr;

        return result;
    }

    AST_Statement* ast_return_statement_new(Allocator* allocator, AST_Expression* return_expr,
                                            const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::RETURN, begin_fp, end_fp);

        result->expression = return_expr;

        return result;
    }

    AST_Statement* ast_declaration_statement_new(Allocator* allocator,
                                                 AST_Declaration* declaration,
                                                 const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::DECLARATION, begin_fp,
                                        end_fp);

        result->declaration = declaration;

        return result;
    }

    AST_Statement* ast_expression_statement_new(Allocator* allocator, AST_Expression* expression,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::EXPRESSION, begin_fp,
                                        end_fp);

        result->expression = expression;

        return result;
    }

    AST_Expression* ast_expression_new(Allocator* allocator, AST_Expression_Kind kind,
                                       const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Expression>(allocator, begin_fp, end_fp); 

        result->kind = kind;

        return result;
    }

    AST_Expression* ast_identifier_expression_new(Allocator* allocator, AST_Identifier* identifier,
                                                  const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::IDENTIFIER, begin_fp,
                                         end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Expression* ast_poly_identifier_expression_new(Allocator* allocator,
                                                       AST_Declaration *poly_type_decl,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::POLY_IDENTIFIER,
                                         begin_fp, end_fp);

        result->poly_identifier.poly_type_decl = poly_type_decl;

        return result;
    }

    AST_Expression* ast_dot_expression_new(Allocator* allocator, AST_Expression* parent_expr,
                                           AST_Identifier* child_ident, const File_Pos &begin_fp,
                                           const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::DOT, begin_fp, end_fp);

        result->dot.parent_expression = parent_expr;
        result->dot.child_identifier = child_ident;
        result->dot.child_index = -1;

        return result;
    }

    AST_Expression* ast_binary_expression_new(Allocator* allocator, Binary_Operator op,
                                              AST_Expression* lhs, AST_Expression* rhs,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BINARY, begin_fp, end_fp);

        result->binary.op = op;
        result->binary.lhs = lhs;
        result->binary.rhs = rhs;

        return result;
    }

    AST_Expression* ast_unary_expression_new(Allocator* allocator, Unary_Operator op,
                                             AST_Expression* operand_expr, const File_Pos &begin_fp,
                                             const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::UNARY, begin_fp, end_fp);

        result->unary.op = op;
        result->unary.operand_expression = operand_expr;

        return result;
    }

    AST_Expression* ast_call_expression_new(Allocator* allocator, AST_Expression* ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            bool is_builtin, const File_Pos &begin_fp,
                                            const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CALL, begin_fp, end_fp);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_expressions;
        result->call.is_builtin = is_builtin;
        result->call.callee_declaration = nullptr;

        return result;
    }

    AST_Expression* ast_compound_expression_new(Allocator* allocator, Array<AST_Expression*> exprs,
                                                AST_Type_Spec* type_spec,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::COMPOUND, begin_fp,
                                         end_fp);

        result->compound.expressions = exprs;
        result->compound.type_spec = type_spec;

        return result;
    }

    AST_Expression* ast_number_literal_expression_new(Allocator* allocator, int64_t value,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::NUMBER_LITERAL, begin_fp,
                                         end_fp);
        
        result->number_literal.s64 = value;

        return result;
    }

    AST_Expression* ast_string_literal_expression_new(Allocator* allocator, Atom& atom,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::STRING_LITERAL, begin_fp,
                                         end_fp);

        result->string_literal.atom = atom;

        return result;
    }

    AST_Type_Spec* ast_type_spec_new(Allocator* allocator, AST_Type_Spec_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        AST_Type_Spec* result = ast_node_new<AST_Type_Spec>(allocator, begin_fp, end_fp);

        result->kind = kind;
        result->type = nullptr;

        return result;
    }

    AST_Type_Spec* ast_identifier_type_spec_new(Allocator* allocator, AST_Identifier* identifier,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::IDENTIFIER, begin_fp,
                                        end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec* ast_pointer_type_spec_new(Allocator* allocator, AST_Type_Spec* base_ts,
                                             const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(base_ts); 

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POINTER, begin_fp, end_fp);

        result->base_type_spec = base_ts;

        return result;
    }

    AST_Type_Spec* ast_dot_type_spec_new(Allocator* allocator, AST_Expression* dot_expr,
                                         const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(dot_expr->kind == AST_Expression_Kind::DOT);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::DOT, begin_fp, end_fp);

        result->dot_expression = dot_expr;

        return result;
    }

    AST_Type_Spec* ast_function_type_spec_new(Allocator* allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec* return_type_spec,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(allocator);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FUNCTION, begin_fp, end_fp);

        result->function.parameter_type_specs = param_type_specs;
        result->function.return_type_spec = return_type_spec;

        return result;
    }

    AST_Type_Spec* ast_array_type_spec_new(Allocator* allocator, AST_Type_Spec* element_ts,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::ARRAY, begin_fp, end_fp);

        result->array.element_type_spec = element_ts;

        return result;
    }

    AST_Type_Spec* ast_templated_type_spec_new(Allocator* allocator, AST_Expression* ident_expr,
                                               Array<AST_Expression*> arg_exprs,
                                               const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER ||
               ident_expr->kind == AST_Expression_Kind::DOT);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::TEMPLATED, begin_fp,
                                        end_fp);

        result->templated.ident_expression = ident_expr;
        result->templated.argument_expressions = arg_exprs;

        return result;
    }

    AST_Type_Spec* ast_poly_identifier_type_spec_new(Allocator* allocator, AST_Declaration *decl,
                                                     AST_Identifier* spec_ident,
                                                     const File_Pos &begin_fp,
                                                     const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POLY_IDENTIFIER, begin_fp,
                                        end_fp);

        result->poly_identifier.declaration = decl;
        result->poly_identifier.specification_identifier = spec_ident;

        return result;
    }

    AST_Type* ast_type_new(Allocator *allocator, AST_Type_Kind kind, uint64_t bit_size)
    {
        auto result = alloc_type<AST_Type>(allocator);
        static_cast<AST_Node*>(result)->kind = AST_Type::_kind;
        assert(result);
        result->kind = kind;
        result->bit_size = bit_size;
        result->pointer_to = nullptr;
        return result;
    }

    AST_Type* ast_integer_type_new(Allocator *allocator, uint64_t bit_size, bool sign)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::INTEGER, bit_size);
        result->integer.sign = sign;
        return result;
    }

    AST_Type* ast_pointer_type_new(Allocator *allocator, AST_Type *base_type)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::POINTER, Builtin::pointer_size);
        result->pointer.base = base_type;
        return result;
    }

    AST_Type* ast_function_type_new(Allocator *allocator, Array<AST_Type*> param_types,
                                    AST_Type *return_type)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::FUNCTION, Builtin::pointer_size);

        result->function.param_types = param_types;
        result->function.return_type = return_type;

        return result;
    }

    AST_Type* ast_structure_type_new(Allocator *allocator, AST_Declaration *declaration,
                                     Array<AST_Type*> member_types,
                                     Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::STRUCTURE, 0);
        result->structure.member_types = member_types;
        result->structure.member_scope = member_scope;
        result->structure.declaration = declaration;

        return result;
    }

    AST_Type* ast_find_or_create_pointer_type(Allocator *allocator, AST_Type *base_type)
    {
        if (base_type->pointer_to) return base_type->pointer_to;

        auto result = ast_pointer_type_new(allocator, base_type);
        base_type->pointer_to = result;
        return result;
    }

    void ast_print_indent(uint64_t indent)
    {
        for (uint64_t i = 0; i < indent; i++)
        {
            printf("    ");
        }
    }

    void ast_print(AST_Node* ast_node)
    {
        assert(ast_node);

        switch (ast_node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto module = (AST_Module*)ast_node;
                for (int64_t i = 0; i < module->declarations.count; i++)
                {
                    ast_print_declaration(module->declarations[i], 0);
                }
                break;
            }

            case AST_Node_Kind::IDENTIFIER:
            {
                auto ident = (AST_Identifier*)ast_node;
                printf("%s", ident->atom.data);
                break;
            }

            case AST_Node_Kind::DECLARATION:
            {
                auto ast_decl = static_cast<AST_Declaration*>(ast_node);
                ast_print_declaration(ast_decl, 0);
                break;
            }

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void ast_print_declaration(AST_Declaration* ast_decl, uint64_t indent)
    {
        if (ast_decl->kind == AST_Declaration_Kind::FUNCTION ||
            ast_decl->kind == AST_Declaration_Kind::STRUCTURE)
        {
            printf("\n");
        }

        ast_print_indent(indent);

        if (ast_decl->decl_flags & AST_DECL_FLAG_IS_NAKED)
        {
            printf("#naked ");
        }

        if (ast_decl->kind == AST_Declaration_Kind::POLY_TYPE)
        {
            printf("$");
        }

        printf("%s", ast_decl->identifier->atom.data);

        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT:
            {
                printf(" :: import ");
                ast_print_expression(ast_decl->import.ident_expression, 0);
                printf(";\n");
                break;
            }

            case AST_Declaration_Kind::VARIABLE:
            case AST_Declaration_Kind::CONSTANT:
            {
                printf(" :");
                if (ast_decl->variable.type_spec)
                {
                    printf(" ");
                    ast_print_type_spec(ast_decl->variable.type_spec);
                    if (ast_decl->variable.init_expression)
                    {
                        printf(" ");
                    }
                }
                
                if (ast_decl->variable.init_expression)
                {
                    if (ast_decl->kind == AST_Declaration_Kind::VARIABLE) printf("= ");
                    else printf(": ");

                    ast_print_expression(ast_decl->variable.init_expression, 0);
                }
                printf(";\n");
                break;
            }

            case AST_Declaration_Kind::PARAMETER:
            {
                if (ast_decl->parameter.type_spec)
                {
                    printf(": ");
                    ast_print_type_spec(ast_decl->parameter.type_spec);
                }
                break;
            }

            case AST_Declaration_Kind::FUNCTION:
            {
                printf(" :: func (");
                for (int64_t i = 0; i < ast_decl->function.parameter_declarations.count; i++)
                {
                    if (i > 0) printf(", ");
                    auto param_decl = ast_decl->function.parameter_declarations[i];
                    ast_print_declaration(param_decl, 0);
                }
                printf(")");
                auto function_ts = ast_decl->function.type_spec;
                if (function_ts->function.return_type_spec)
                {
                    printf(" -> ");
                    ast_print_type_spec(function_ts->function.return_type_spec);
                }
                   
                //ast_print_type_spec(ast_decl->function.type_spec);
                if (ast_decl->function.body)
                    ast_print_statement(ast_decl->function.body, indent);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::STRUCTURE:
            {
                printf(" :: struct");
                if (ast_decl->structure.parameters.count)
                {
                    printf("(");
                    for (int64_t i = 0; i < ast_decl->structure.parameters.count; i++)
                    {
                        if (i > 0) printf(", ");
                        auto param_decl = ast_decl->structure.parameters[i];
                        ast_print_declaration(param_decl, 0);
                    }
                    printf(")");
                }
                printf("\n");
                ast_print_indent(indent);
                printf("{\n");
                for (int64_t i = 0; i < ast_decl->structure.member_declarations.count; i++)
                {
                    auto mem_decl = ast_decl->structure.member_declarations[i];
                    ast_print_declaration(mem_decl, indent + 1);
                }
                ast_print_indent(indent);
                printf("}\n");
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE:
            {
                if (ast_decl->poly_type.specification_identifier)
                {
                    printf("/");
                    ast_print(ast_decl->poly_type.specification_identifier);
                }
                break;
            }
        }
    }

    void ast_print_statement(AST_Statement* ast_stmt, uint64_t indent)
    {
        ast_print_indent(indent);

        switch (ast_stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                printf("\n");
                ast_print_indent(indent);
                printf("{\n");
                for (int64_t i = 0; i < ast_stmt->block.statements.count; i++)
                {
                    ast_print_statement(ast_stmt->block.statements[i], indent + 1);
                }
                ast_print_indent(indent);
                printf("}\n");
                break;  
            }

            case AST_Statement_Kind::ASSIGNMENT:
            {
                ast_print_expression(ast_stmt->assignment.identifier_expression, 0);
                printf(" = ");
                ast_print_expression(ast_stmt->assignment.rhs_expression, 0);
                printf("\n");
                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                printf("return");
                if (ast_stmt->expression)
                {
                    printf(" ");
                    ast_print_expression(ast_stmt->expression, 0);
                    printf(";\n");
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                ast_print_declaration(ast_stmt->declaration, 0);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                ast_print_expression(ast_stmt->expression, 0);
                printf(";\n");
            }
        } 
    }

    void ast_print_type_spec(AST_Type_Spec* type_spec)
    {
        switch (type_spec->kind)
        {
            case AST_Type_Spec_Kind::INVALID: assert(false);

            case AST_Type_Spec_Kind::IDENTIFIER:
            {
                printf("%s", type_spec->identifier->atom.data);
                break; 
            }

            case AST_Type_Spec_Kind::POINTER:
            {
                printf("*");
                ast_print_type_spec(type_spec->base_type_spec);
                break; 
            }

            case AST_Type_Spec_Kind::DOT:
            {
                ast_print_expression(type_spec->dot_expression, 0);
                break; 
            }

            case AST_Type_Spec_Kind::FUNCTION:
            {
                printf("func (");
                for (int64_t i = 0; i < type_spec->function.parameter_type_specs.count; i++)
                {
                    if (i > 0) printf(", ");
                    ast_print_type_spec(type_spec->function.parameter_type_specs[i]);
                }
                printf(")");
                if (type_spec->function.return_type_spec)
                {
                    printf(" -> ");
                    ast_print_type_spec(type_spec->function.return_type_spec);
                }
                break;
            }

            case AST_Type_Spec_Kind::ARRAY:
            {
                printf("[]");
                ast_print_type_spec(type_spec->array.element_type_spec);
                break;
            }

            case AST_Type_Spec_Kind::TEMPLATED:
            {
                ast_print_expression(type_spec->templated.ident_expression, 0);
                printf("(");
                for (int64_t i = 0; i < type_spec->templated.argument_expressions.count; i++)
                {
                    if (i > 0) printf(", ");
                    ast_print_expression(type_spec->templated.argument_expressions[i], 0);
                }
                printf(")");
                break;
            }

            case AST_Type_Spec_Kind::POLY_IDENTIFIER:
            {
                printf("$");
                ast_print(type_spec->poly_identifier.declaration);
                if (type_spec->poly_identifier.specification_identifier)
                {
                    printf("/");
                    ast_print(type_spec->poly_identifier.specification_identifier);
                }
                break;
            }
        }
    }

    void ast_print_expression(AST_Expression* ast_expr, uint64_t indent)
    {
        ast_print_indent(indent);

        switch (ast_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                printf("%s", ast_expr->identifier->atom.data);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER:
            {
                ast_print(ast_expr->poly_identifier.poly_type_decl);
                break;
            }

            case AST_Expression_Kind::DOT:
            {
                ast_print_expression(ast_expr->dot.parent_expression, 0);
                printf(".%s", ast_expr->dot.child_identifier->atom.data);
                break;
            }

            case AST_Expression_Kind::BINARY:
            {
                ast_print_expression(ast_expr->binary.lhs, 0);
                switch (ast_expr->binary.op)
                {
                    case BINOP_INVALID: assert(false);
                    case BINOP_ADD: printf(" + "); break;
                    case BINOP_SUB: printf(" - "); break;
                }
                ast_print_expression(ast_expr->binary.rhs, 0);
                break;
            }

            case AST_Expression_Kind::UNARY:
            {
                switch(ast_expr->unary.op)
                {
                    case UNOP_INVALID: assert(false); break;
                    case UNOP_DEREF: printf("<"); break;
                }

                ast_print_expression(ast_expr->unary.operand_expression, 0);
                break;
            }

            case AST_Expression_Kind::CALL: 
            {
                if (ast_expr->call.is_builtin)
                {
                    printf("@");
                }
                ast_print_expression(ast_expr->call.ident_expression, 0);
                printf("(");
                for (int64_t i = 0; i < ast_expr->call.arg_expressions.count; i++)
                {
                    if (i > 0) printf(", ");
                    ast_print_expression(ast_expr->call.arg_expressions[i], 0);
                }
                printf(")");
                break;
            }

            case AST_Expression_Kind::COMPOUND:
            {
                if (ast_expr->compound.type_spec)
                {
                    ast_print_type_spec(ast_expr->compound.type_spec);
                    printf(" ");
                }
                printf("{");
                for (int64_t i = 0; i < ast_expr->compound.expressions.count; i++)
                {
                    if (i > 0) printf(", ");
                    else printf(" ");

                    ast_print_expression(ast_expr->compound.expressions[i], 0);
                }
                printf(" }");
                break;
            }

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                printf("%" PRId64, ast_expr->number_literal.s64);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL:
            {
                printf("\"%s\"", ast_expr->string_literal.atom.data);
                break;
            }
        }
    }

    void ast_print_scope(Allocator* allocator, AST_Node* anode)
    {
        assert(anode);

        String_Builder sb = {};
        string_builder_init(allocator, &sb);

        ast_print_scope(&sb, anode);

        auto str = string_builder_to_string(allocator, &sb);
        printf("%s\n", str.data);
        free(allocator, str.data);

        string_builder_free(&sb);
    }

    void ast_print_scope(String_Builder *sb, AST_Node *anode, int64_t indent/*=0*/)
    {
        switch (anode->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);

            case AST_Node_Kind::MODULE:
            {
                auto ast_module = static_cast<AST_Module*>(anode);
                scope_print(sb, ast_module->module_scope, indent);
                break;
            }

            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                auto ast_decl = static_cast<AST_Declaration*>(anode);
                ast_print_declaration_scopes(sb, ast_decl, indent);
                break;
            }

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void ast_print_scope_indent(String_Builder *sb, int64_t indent)
    {
        for (int64_t i = 0; i < indent; i ++) string_builder_append(sb, "    ");
    }

    void ast_print_declaration_scopes(String_Builder *sb, AST_Declaration *ast_decl, int64_t indent)
    {
        assert(sb);
        switch (ast_decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);

            case AST_Declaration_Kind::IMPORT:
            {
                string_builder_append(sb, " (import)");
                break;
            }

            case AST_Declaration_Kind::VARIABLE:
            {
                string_builder_append(sb, " (variable)");
                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                string_builder_append(sb, " (constant)");
                break;
            }

            case AST_Declaration_Kind::PARAMETER:
            {
                string_builder_append(sb, " (param)");
                break;
            }

            case AST_Declaration_Kind::FUNCTION:
            {
                string_builder_append(sb, " (func)\n");
                scope_print(sb, ast_decl->function.parameter_scope, indent);
                scope_print(sb, ast_decl->function.body->block.scope, indent);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);

            case AST_Declaration_Kind::STRUCTURE:
            {
                string_builder_append(sb, " (struct)\n");
                if (ast_decl->structure.parameters.count > 0)
                {
                    scope_print(sb, ast_decl->structure.parameter_scope, indent);
                }
                scope_print(sb, ast_decl->structure.member_scope, indent);
                break;
            }

            case AST_Declaration_Kind::POLY_TYPE:
            {
                string_builder_append(sb, " (poly_param)");
                break;
            }
        }
    }

    void ast_print_type(String_Builder *sb, AST_Type *type)
    {
        switch (type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                if (type->integer.sign) string_builder_append(sb, "s");
                else string_builder_append(sb, "u");

                string_builder_appendf(sb, "%" PRIu64, type->bit_size);
                break;
            }

            case AST_Type_Kind::POINTER: assert(false);

            case AST_Type_Kind::VOID:
            {
                string_builder_append(sb, "void");
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: 
            {
                auto decl = type->structure.declaration;
                string_builder_appendf(sb, "struct(%s)", decl->identifier->atom.data);
                break;
            }
        }
    }

}
