
#include "ast.h"

#include "builtin.h"
#include "parse_tree_node.h"
#include "string_builder.h"
#include "scope.h"
#include "parser.h"
#include "temp_allocator.h"

#include <inttypes.h>

namespace Zodiac
{

    AST_Node_Kind AST_Module::_kind = AST_Node_Kind::MODULE;
    AST_Node_Kind AST_Identifier::_kind = AST_Node_Kind::IDENTIFIER;
    AST_Node_Kind AST_Declaration::_kind = AST_Node_Kind::DECLARATION;
    AST_Node_Kind AST_Switch_Case::_kind = AST_Node_Kind::SWITCH_CASE;
    AST_Node_Kind AST_Statement::_kind = AST_Node_Kind::STATEMENT;
    AST_Node_Kind AST_Expression::_kind = AST_Node_Kind::EXPRESSION;
    AST_Node_Kind AST_Type_Spec::_kind = AST_Node_Kind::TYPE_SPEC;
    AST_Node_Kind AST_Type::_kind = AST_Node_Kind::TYPE;

    void ast_node_init(AST_Node *ast_node, AST_Node_Kind kind, const File_Pos &begin_fp,
                       const File_Pos &end_fp)
    {
        assert(ast_node);
        ast_node->kind = kind;
        ast_node->begin_file_pos = begin_fp;
        ast_node->end_file_pos = end_fp;
    }

    AST_Module *ast_create_from_parsed_file(Allocator *allocator, Parsed_File *parsed_file)
    {
        assert(allocator);
        assert(parsed_file);

        Array<AST_Declaration*> global_decls = {};
        array_init(allocator, &global_decls);

        for (int64_t i = 0; i < parsed_file->declarations.count; i++)
        {
            AST_Declaration *ast_decl =
                ast_create_declaration_from_ptn(allocator, parsed_file->declarations[i],
                                                nullptr);
            ast_decl->decl_flags |= AST_DECL_FLAG_GLOBAL;
            assert(ast_decl);
            array_append(&global_decls, ast_decl);
        }

        assert(global_decls.count);

        auto begin_fp = array_first(&global_decls)->begin_file_pos;
        auto end_fp = array_last(&global_decls)->end_file_pos;

        AST_Module *ast_module = ast_module_new(allocator, global_decls, begin_fp, end_fp);
        assert(ast_module);
        return ast_module;
    }

    AST_Declaration *ast_create_declaration_from_ptn(Allocator *allocator, Declaration_PTN *ptn,
                                                     Array<AST_Declaration*> *var_decls)
    {
        assert(allocator);

        AST_Identifier *ast_ident = nullptr;
        File_Pos begin_fp;

        if (ptn->identifier)
        {
            auto id_begin_fp = ptn->identifier->self.begin_file_pos;

            auto id_end_fp = ptn->identifier->self.end_file_pos;

            assert(ptn->identifier);
            ast_ident = ast_identifier_new(allocator, ptn->identifier->atom, id_begin_fp,
                                           id_end_fp);
            assert(ast_ident);
            begin_fp = ast_ident->begin_file_pos;
        }
        else
        {
            begin_fp = ptn->self.begin_file_pos; 
        }


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

            case Declaration_PTN_Kind::USING:
            {
                auto import_ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->using_decl.ident_expr); 
                assert(import_ident_expr);

                auto end_fp = ptn->self.end_file_pos;

                return ast_using_declaration_new(allocator, import_ident_expr, begin_fp, end_fp);
            }

            case Declaration_PTN_Kind::VARIABLE:
            {
                AST_Type_Spec *type_expr = nullptr;
                AST_Expression *init_expr = nullptr;

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
                AST_Type_Spec *ast_type = nullptr;
                if (ptn->constant.type_expression)
                {
                    ast_type =
                        ast_create_type_spec_from_expression_ptn(allocator,
                                                                 ptn->constant.type_expression);
                    assert(ast_type);
                }

                AST_Expression *ast_init_expr = nullptr;
                if (ptn->constant.init_expression)
                {
                    ast_init_expr =
                        ast_create_expression_from_ptn(allocator, ptn->constant.init_expression);
                    assert(ast_init_expr);
                }

                assert(ast_type || ast_init_expr);

                File_Pos end_fp;
                if (ast_init_expr) end_fp = ast_init_expr->end_file_pos;
                else end_fp = ast_type->end_file_pos;

                return ast_constant_declaration_new(allocator, ast_ident, ast_type,
                                                    ast_init_expr, begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::FUNCTION:
            {
                AST_Type_Spec *ast_type =
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
                        auto ptn_mem_decl = ptn->structure.member_declarations[i];
                        auto ast_mem_decl = ast_create_declaration_from_ptn(allocator,
                                                                            ptn_mem_decl,
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
                            auto type_expr = ptn_param->type_expression;
                            ast_param_ts =
                                ast_create_type_spec_from_expression_ptn(allocator,
                                                                         type_expr);
                        }


                        auto ast_param_decl =
                            ast_create_declaration_from_ptn(allocator, ptn_param,
                                                            ast_param_ts);

                        assert(ast_param_decl);

                        array_append(&ast_parameters, ast_param_decl);
                    }
                }

                assert(ast_parameters.count >= 0);
                auto end_fp = ptn->self.end_file_pos;

                return ast_structure_declaration_new(allocator, ast_ident,
                                                     ast_member_decls, ast_parameters,
                                                     begin_fp, end_fp);
                break;
            }

            case Declaration_PTN_Kind::ENUM:
            {
                AST_Type_Spec *ast_ts = nullptr;

                auto ptn_ts = (PTN*)ptn->enum_decl.type_spec;
                if (ptn_ts)
                {
                    ast_ts = ast_create_type_spec_from_ptn(allocator, ptn_ts);
                }

                Array<AST_Declaration*> ast_members;
                array_init(allocator, &ast_members, ptn->enum_decl.members.count);

                for (int64_t i = 0; i < ptn->enum_decl.members.count; i++)
                {
                    auto ptn_mem = ptn->enum_decl.members[i];
                    auto ast_member = ast_create_enum_member_from_ptn(allocator, ptn_mem); 
                    assert(ast_member->kind == AST_Declaration_Kind::CONSTANT);
                    assert(ast_member->constant.type_spec == nullptr);

                    array_append(&ast_members, ast_member);

                    auto init_expr = ast_member->constant.init_expression;
                    if (init_expr)
                    {
                        if (init_expr->kind == AST_Expression_Kind::INTEGER_LITERAL)
                        {
                            ast_member->decl_flags |= AST_DECL_FLAG_ENUM_MEMBER_INTINIT;
                        }
                        else if (init_expr->kind == AST_Expression_Kind::IDENTIFIER)
                        {
                            ast_member->decl_flags |= AST_DECL_FLAG_ENUM_MEMBER_IDENTINIT;
                        }
                        else assert(false);
                    }

                }

                return ast_enum_declaration_new(allocator, ast_ident, ast_ts, ast_members,
                                                begin_fp, ptn->self.end_file_pos);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Declaration *ast_create_declaration_from_ptn(Allocator *allocator, Parameter_PTN *ptn, 
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

    AST_Declaration *ast_create_enum_member_from_ptn(Allocator *allocator, PTN *ptn)
    {

        AST_Identifier *identifier = nullptr;
        AST_Expression *init_expr = nullptr;
        
        switch (ptn->kind)
        {
            case PT_Node_Kind::INVALID: assert(false);

            case PT_Node_Kind::IDENTIFIER:
            {
                Identifier_PTN *ident = (Identifier_PTN*)ptn;
                identifier = ast_identifier_new(allocator, ident->atom,
                                                ptn->begin_file_pos,
                                                ptn->end_file_pos);
                break;
            }

            case PT_Node_Kind::FUNCTION_PROTO: assert(false);
            case PT_Node_Kind::PARAMETER: assert(false);
            case PT_Node_Kind::EXPRESSION_LIST: assert(false);

            case PT_Node_Kind::DECLARATION:
            {
                Declaration_PTN *declaration = (Declaration_PTN*)ptn;
                Identifier_PTN *ident = declaration->identifier;

                assert(declaration->kind == Declaration_PTN_Kind::CONSTANT);
                assert(declaration->constant.type_expression == nullptr);

                identifier = ast_identifier_new(allocator, ident->atom,
                                                ident->self.begin_file_pos,
                                                ident->self.end_file_pos);

                auto init_expression = declaration->constant.init_expression;
                if (init_expression)
                {
                    init_expr = ast_create_expression_from_ptn(allocator, init_expression);
                }
                break;
            }

            case PT_Node_Kind::STATEMENT: assert(false);
            case PT_Node_Kind::EXPRESSION: assert(false);
        }

        assert(identifier);

        return ast_constant_declaration_new(allocator, identifier, nullptr, init_expr,
                                            ptn->begin_file_pos, ptn->end_file_pos);
    }

    AST_Statement *ast_create_statement_from_ptn(Allocator *allocator, Statement_PTN *ptn, 
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
                        AST_Statement *ast_block_stmt =
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
                AST_Expression *return_val_expr = nullptr;
                if (ptn->return_stmt.expression)
                {
                    return_val_expr =
                        ast_create_expression_from_ptn(allocator, ptn->return_stmt.expression);
                    assert(return_val_expr);
                }

                return ast_return_statement_new(allocator, return_val_expr, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::BREAK:
            {
                return ast_break_statement_new(allocator, begin_fp, end_fp);
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
            
            case Statement_PTN_Kind::WHILE:
            {
                auto cond_expr = ast_create_expression_from_ptn(allocator,
                                                                ptn->while_stmt.cond_expr);
                auto body = ast_create_statement_from_ptn(allocator, ptn->while_stmt.body,
                                                          var_decls);
                return ast_while_statement_new(allocator, cond_expr, body, begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::IF:
            {
                auto cond_expr = ast_create_expression_from_ptn(allocator,
                                                                ptn->if_stmt.cond_expr);
                auto then_stmt = ast_create_statement_from_ptn(allocator,
                                                               ptn->if_stmt.then_stmt,
                                                               var_decls);
                AST_Statement *else_stmt = nullptr;
                if (ptn->if_stmt.else_stmt)
                {
                    else_stmt = ast_create_statement_from_ptn(allocator,
                                                              ptn->if_stmt.else_stmt,
                                                              var_decls); 
                }

                return ast_if_statement_new(allocator, cond_expr, then_stmt, else_stmt,
                                            begin_fp, end_fp);
                break;
            }

            case Statement_PTN_Kind::SWITCH:
            {
                Array<AST_Switch_Case*> cases = {};
                array_init(allocator, &cases, ptn->switch_stmt.cases.count);

                AST_Expression *expression =
                    ast_create_expression_from_ptn(allocator, ptn->switch_stmt.expression);

                AST_Switch_Case *default_case = nullptr;
                uint32_t case_expr_count = 0;

                for (int64_t i = 0; i < ptn->switch_stmt.cases.count; i++)
                {
                    auto ptn_case = ptn->switch_stmt.cases[i];

                    bool is_default = ptn_case.is_default;

                    Array<AST_Expression *> case_expressions = {};

                    if (is_default)
                    {
                        assert(!ptn_case.expressions.count);
                    }
                    else
                    {
                        auto ptn_exprs = ptn_case.expressions;
                        array_init(allocator, &case_expressions, ptn_exprs.count);

                        assert(ptn_case.expressions.count);
                        for (int64_t expr_i = 0; expr_i < ptn_exprs.count; expr_i++)
                        {
                            auto switch_case_expr = ptn_exprs[expr_i];
                            AST_Expression *case_expr = nullptr;
                            if (!switch_case_expr.range_end_expr)
                            {
                                case_expr =
                                    ast_create_expression_from_ptn(allocator,
                                                                   switch_case_expr.expression);
                            }
                            else
                            {
                                auto r_begin_expr = ast_create_expression_from_ptn(
                                        allocator, switch_case_expr.range_begin_expr);

                                auto r_end_expr = ast_create_expression_from_ptn(
                                        allocator, switch_case_expr.range_end_expr);

                                case_expr = ast_range_expression_new(allocator,
                                                                     r_begin_expr,
                                                                     r_end_expr);
                            }

                            assert(case_expr);
                            array_append(&case_expressions, case_expr);
                        }
            
                        case_expr_count += ptn_exprs.count;

                    }

                    AST_Statement *body = ast_create_statement_from_ptn(allocator,
                                                                        ptn_case.body,
                                                                        var_decls);

                    AST_Switch_Case *ast_case = ast_switch_case_new(allocator,
                                                                    case_expressions,
                                                                    is_default, body,
                                                                    ptn_case.begin_fp, 
                                                                    ptn_case.end_fp);

                    if (is_default)
                    {
                        assert(default_case == nullptr);
                        default_case = ast_case;
                    }

                    array_append(&cases, ast_case);
                }

                return ast_switch_statement_new(allocator, expression, default_case,
                                                cases, case_expr_count, begin_fp, end_fp);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Expression *ast_create_expression_from_ptn(Allocator *allocator, Expression_PTN *ptn)
    {
        assert(allocator);

        auto begin_fp = ptn->self.begin_file_pos;
        auto end_fp = ptn->self.end_file_pos;
       
        switch (ptn->kind)
        {
            case Expression_PTN_Kind::INVALID: assert(false);

            case Expression_PTN_Kind::CALL:
            {
                AST_Expression *ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->call.ident_expression);
                assert(ident_expr);

                Array<AST_Expression*> arg_exprs = {};
                if (ptn->call.arg_list)
                {
                    array_init(allocator, &arg_exprs);

                    for (int64_t i = 0; i < ptn->call.arg_list->expressions.count; i++)
                    {
                        AST_Expression *arg_expr =
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

                AST_Identifier *identifier = ast_identifier_new(allocator, ptn->identifier->atom,
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

                AST_Type_Spec *ast_ts = nullptr;
                if (ptn->compound.type_expression)
                {
                    ast_ts =
                        ast_create_type_spec_from_expression_ptn(allocator,
                                                                 ptn->compound.type_expression);
                    assert(ast_ts);
                }

                return ast_compound_expression_new(allocator, ast_compound_exprs, ast_ts,
                                                   begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::SUBSCRIPT:
            {
                auto pointer_expr =
                    ast_create_expression_from_ptn(allocator, ptn->subscript.pointer_expression);
                auto index_expr =
                    ast_create_expression_from_ptn(allocator, ptn->subscript.index_expression);

                auto bfp = ptn->self.begin_file_pos;
                auto efp = ptn->self.end_file_pos;
                return ast_subscript_expression_new(allocator, pointer_expr, index_expr,
                                                    bfp, efp);
                break;
            }

            case Expression_PTN_Kind::INTEGER_LITERAL:
            {
                assert(((int64_t)ptn->integer_literal.value.u64) ==
                       ptn->integer_literal.value.s64);
                return ast_integer_literal_expression_new(allocator,
                                                          ptn->integer_literal.value.s64,
                                                          begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::FLOAT_LITERAL:
            {
                return ast_float_literal_expression_new(allocator, ptn->float_literal.r32,
                                                        begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::STRING_LITERAL:
            {
                return ast_string_literal_expression_new(allocator, ptn->string_literal.atom,
                                                         begin_fp, end_fp);
                break;
            };

            case Expression_PTN_Kind::CHAR_LITERAL:
            {
                return ast_char_literal_expression_new(allocator, ptn->char_literal.c,
                                                       begin_fp, end_fp);
            }

            case Expression_PTN_Kind::BOOL_LITERAL:
            {
                return ast_boolean_literal_expression_new(allocator, ptn->bool_literal.value,
                                                          begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::ARRAY_TYPE: assert(false);

            case Expression_PTN_Kind::POINTER_TYPE:
            {
                auto operand_expr =
                    ast_create_expression_from_ptn(allocator,
                                                   ptn->pointer_type.pointee_type_expression);
                return ast_addrof_expression_new(allocator, operand_expr, begin_fp, end_fp);
                break;
            }

            case Expression_PTN_Kind::POLY_TYPE:
            {
                assert(ptn->poly_type.identifier);

                auto id_begin_file_pos = ptn->poly_type.identifier->self.begin_file_pos;
                auto id_end_file_pos = ptn->poly_type.identifier->self.end_file_pos;

                auto ast_ident = ast_identifier_new(allocator, ptn->poly_type.identifier->atom,
                                                    id_begin_file_pos, id_end_file_pos);
                assert(ast_ident);

                AST_Identifier *ast_spec_ident = nullptr;
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

                AST_Declaration *poly_type_decl =
                    ast_poly_type_declaration_new(allocator, ast_ident, ast_spec_ident,
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

    AST_Type_Spec *ast_create_type_spec_from_ptn(Allocator *allocator, PT_Node *ptn)
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
                        AST_Type_Spec *ast_param_type = 
                            ast_create_type_spec_from_ptn(allocator,
                                                          &function_ptn->parameters[i]->self);
                        assert(ast_param_type);
                        
                        array_append(&ast_param_types, ast_param_type);
                    }

                }

                AST_Type_Spec *ast_return_type = nullptr;
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
                return ast_create_type_spec_from_expression_ptn(allocator,
                                                               (Expression_PTN*)ptn);
                break; 
            }
        }

        assert(false);
        return nullptr;
    }

    AST_Type_Spec *ast_create_type_spec_from_expression_ptn(Allocator *allocator,
                                                            Expression_PTN *ptn)
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

                auto ast_ident_expr =
                    ast_create_expression_from_ptn(allocator, ptn->call.ident_expression);
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

                AST_Identifier *ident = ast_identifier_new(allocator, ptn->identifier->atom,
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
            case Expression_PTN_Kind::SUBSCRIPT: assert(false);
            case Expression_PTN_Kind::INTEGER_LITERAL: assert(false);
            case Expression_PTN_Kind::FLOAT_LITERAL: assert(false);
            case Expression_PTN_Kind::STRING_LITERAL: assert(false);
            case Expression_PTN_Kind::CHAR_LITERAL: assert(false);
            case Expression_PTN_Kind::BOOL_LITERAL: assert(false);

            case Expression_PTN_Kind::ARRAY_TYPE:
            {
                auto length_ptn = ptn->array_type.length_expression;
                auto elem_type_ptn = ptn->array_type.element_type_expression;
                AST_Expression *length_expr = nullptr;
                if (length_ptn)
                {
                    length_expr = ast_create_expression_from_ptn(allocator, length_ptn);
                }
                auto ast_elem_ts = ast_create_type_spec_from_expression_ptn(allocator,
                                                                            elem_type_ptn);
                assert(ast_elem_ts);

                return ast_array_type_spec_new(allocator, length_expr, ast_elem_ts, begin_fp,
                                               end_fp);
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

                AST_Identifier *ast_spec_ident = nullptr;
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

    AST_Identifier *ast_identifier_new(Allocator *allocator, Atom& atom, const File_Pos &begin_fp,
                                       const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Identifier>(allocator, begin_fp, end_fp);

        result->atom = atom;
        result->declaration = nullptr;

        return result;
    }

    AST_Module *ast_module_new(Allocator *allocator, Array<AST_Declaration*> decls,
                               const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Module>(allocator, begin_fp, end_fp);

        result->declarations = decls;
        
        return result;
    }

    AST_Declaration *ast_declaration_new(Allocator *allocator, AST_Declaration_Kind kind, 
                                         AST_Identifier *identifier, const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Declaration>(allocator, begin_fp, end_fp);

        result->kind = kind;
        result->identifier = identifier;

        return result;
    }

    AST_Declaration *ast_import_declaration_new(Allocator *allocator, AST_Identifier *identifier,
                                                AST_Expression *ident_expr,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::IMPORT, identifier,
                                          begin_fp, end_fp);

        result->import.ident_expression = ident_expr;
        result->import.ast_module = nullptr;
        result->import.parse_queued = false;

        return result;
    }

    AST_Declaration *ast_using_declaration_new(Allocator *allocator,
                                               AST_Expression *import_ident_expr,
                                               const File_Pos &begin_fp,
                                               const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::USING, nullptr,
                                          begin_fp, end_fp);

        result->using_decl.ident_expr = import_ident_expr;
        result->using_decl.import_scope = nullptr;

        return result;
    }

    AST_Declaration *ast_variable_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::VARIABLE, identifier,
                                          begin_fp, end_fp);

        result->variable.type_spec = type_spec;
        result->variable.init_expression = init_expr;

        return result;
    }

    AST_Declaration *ast_constant_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec,
                                                  AST_Expression *init_expr,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::CONSTANT, identifier,
                                          begin_fp, end_fp);
        result->constant.type_spec = type_spec;
        result->constant.init_expression = init_expr;

        return result;
    }

    AST_Declaration *ast_parameter_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   AST_Type_Spec *type_spec,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::PARAMETER, identifier,
                                          begin_fp, end_fp);

        result->parameter.type_spec = type_spec;

        return result;
    }

    AST_Declaration *ast_function_declaration_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  AST_Type_Spec *type_spec, 
                                                  Array<AST_Declaration*> parameter_declarations,
                                                  Array<AST_Declaration*> variable_declarations,
                                                  AST_Statement *body,
                                                  bool is_naked, bool is_noreturn,
                                                  bool is_foreign,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        if (body) assert(body->kind == AST_Statement_Kind::BLOCK);

        assert(identifier);
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

    AST_Declaration *ast_type_declaration_new(Allocator *allocator, AST_Type *type,
                                              AST_Identifier *identifier)
    {
        File_Pos fp = { 0, 0, 0, string_ref("<builtin>") };
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::TYPE, identifier,
                                          fp, fp);

        result->type = type;

        return result;
    }

    AST_Declaration *ast_structure_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   Array<AST_Declaration*> member_decls,
                                                   Array<AST_Declaration*> parameters,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::STRUCTURE,
                                          identifier, begin_fp, end_fp);

        result->structure.member_declarations = member_decls;
        result->structure.parameters = parameters;

        result->structure.parameter_scope = nullptr;
        result->structure.member_scope = nullptr;

        return result;
    }

    AST_Declaration *ast_enum_declaration_new(Allocator *allocator,
                                              AST_Identifier *identifier,
                                              AST_Type_Spec *ast_ts,
                                              Array<AST_Declaration*> member_decls,
                                              const File_Pos &begin_fp,
                                              const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::ENUM,
                                          identifier, begin_fp, end_fp);
        result->enum_decl.type_spec = ast_ts;
        result->enum_decl.member_declarations = member_decls;
        result->enum_decl.member_scope = nullptr;

        return result;
    }

    AST_Declaration *ast_poly_type_declaration_new(Allocator *allocator,
                                                   AST_Identifier *identifier,
                                                   AST_Identifier *spec_ident,
                                                   const File_Pos &begin_fp,
                                                   const File_Pos &end_fp)
    {
        auto result = ast_declaration_new(allocator, AST_Declaration_Kind::POLY_TYPE,
                                          identifier,
                                          begin_fp, end_fp);

        result->poly_type.specification_identifier = spec_ident;

        return result;
    }

    AST_Switch_Case *ast_switch_case_new(Allocator *allocator,
                                         Array<AST_Expression *> case_exprs,
                                         bool is_default, AST_Statement *body, 
                                         const File_Pos &begin_fp,
                                         const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Switch_Case>(allocator, begin_fp, end_fp);

        result->expressions = case_exprs;
        result->is_default = is_default;
        result->body = body;

        return result;
    }

    AST_Statement *ast_statement_new(Allocator *allocator, AST_Statement_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Statement>(allocator, begin_fp, end_fp);

        result->kind = kind;

        return result;
    }

    AST_Statement *ast_block_statement_new(Allocator *allocator, Array<AST_Statement*> statements,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::BLOCK, begin_fp, end_fp);

        result->block.statements = statements;
        result->block.scope = nullptr;

        return result;
    }

    AST_Statement *ast_assignment_statement_new(Allocator *allocator, AST_Expression *ident_expr,
                                                AST_Expression *rhs_expr, const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::ASSIGNMENT, begin_fp,
                                        end_fp);

        result->assignment.identifier_expression = ident_expr;
        result->assignment.rhs_expression = rhs_expr;

        return result;
    }

    AST_Statement *ast_return_statement_new(Allocator *allocator, AST_Expression *return_expr,
                                            const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::RETURN, begin_fp, end_fp);

        result->expression = return_expr;

        return result;
    }

    AST_Statement *ast_break_statement_new(Allocator *allocator, const File_Pos & begin_fp,
                                           const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::BREAK, begin_fp, end_fp);
        return result;
    }

    AST_Statement *ast_declaration_statement_new(Allocator *allocator,
                                                 AST_Declaration *declaration,
                                                 const File_Pos &begin_fp,
                                                 const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::DECLARATION, begin_fp,
                                        end_fp);

        result->declaration = declaration;

        return result;
    }

    AST_Statement *ast_expression_statement_new(Allocator *allocator, AST_Expression *expression,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::EXPRESSION, begin_fp,
                                        end_fp);

        result->expression = expression;

        return result;
    }

    AST_Statement *ast_while_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *body, const File_Pos & begin_fp,
                                           const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::WHILE, begin_fp, end_fp);

        result->while_stmt.cond_expr = cond_expr;
        result->while_stmt.body = body;
        result->while_stmt.body_scope = nullptr;

        return result;
    }

    AST_Statement *ast_if_statement_new(Allocator *allocator, AST_Expression *cond_expr,
                                           AST_Statement *then_stmt, AST_Statement *else_stmt,
                                           const File_Pos & begin_fp, const File_Pos &end_fp)
    {
        assert(cond_expr);
        assert(then_stmt);

        auto result = ast_statement_new(allocator, AST_Statement_Kind::IF, begin_fp, end_fp);

        result->if_stmt.cond_expr = cond_expr;
        result->if_stmt.then_stmt = then_stmt;
        result->if_stmt.else_stmt = else_stmt;

        return result;
    }

    AST_Statement *ast_switch_statement_new(Allocator *allocator, AST_Expression *expression,
                                            AST_Switch_Case *default_case,
                                            Array<AST_Switch_Case*> cases,
                                            uint32_t case_expr_count,
                                            const File_Pos &begin_fp,
                                            const File_Pos &end_fp)
    {
        auto result = ast_statement_new(allocator, AST_Statement_Kind::SWITCH,
                                        begin_fp, end_fp);

        result->switch_stmt.expression = expression;
        result->switch_stmt.default_case = default_case;
        result->switch_stmt.cases = cases;
        result->switch_stmt.case_expr_count = case_expr_count;

        return result;
    }

    AST_Expression *ast_expression_new(Allocator *allocator, AST_Expression_Kind kind,
                                       const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_node_new<AST_Expression>(allocator, begin_fp, end_fp); 

        result->kind = kind;
        result->expr_flags = AST_EXPR_FLAG_NONE;

        return result;
    }

    AST_Expression *ast_identifier_expression_new(Allocator *allocator,
                                                  AST_Identifier *identifier,
                                                  const File_Pos &begin_fp,
                                                  const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::IDENTIFIER, begin_fp,
                                         end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Expression *ast_poly_identifier_expression_new(Allocator *allocator,
                                                       AST_Declaration *poly_type_decl,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::POLY_IDENTIFIER,
                                         begin_fp, end_fp);

        result->poly_identifier.poly_type_decl = poly_type_decl;

        return result;
    }

    AST_Expression *ast_dot_expression_new(Allocator *allocator, AST_Expression *parent_expr,
                                           AST_Identifier *child_ident,
                                           const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::DOT, begin_fp, end_fp);

        result->dot.parent_expression = parent_expr;
        result->dot.child_identifier = child_ident;
        result->dot.child_index = -1;

        return result;
    }

    AST_Expression *ast_binary_expression_new(Allocator *allocator, Binary_Operator op,
                                              AST_Expression *lhs, AST_Expression *rhs,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BINARY, begin_fp,
                                         end_fp);

        result->binary.op = op;
        result->binary.lhs = lhs;
        result->binary.rhs = rhs;

        return result;
    }

    AST_Expression *ast_unary_expression_new(Allocator *allocator, Unary_Operator op,
                                             AST_Expression *operand_expr,
                                             const File_Pos &begin_fp,
                                             const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::UNARY, begin_fp,
                                         end_fp);

        result->unary.op = op;
        result->unary.operand_expression = operand_expr;

        return result;
    }

    AST_Expression *ast_call_expression_new(Allocator *allocator, AST_Expression *ident_expr,
                                            Array<AST_Expression*> arg_expressions,
                                            bool is_builtin, const File_Pos &begin_fp,
                                            const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CALL, begin_fp,
                                         end_fp);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_expressions;
        result->call.is_builtin = is_builtin;
        result->call.callee_declaration = nullptr;

        return result;
    }

    AST_Expression *ast_addrof_expression_new(Allocator *allocator,
                                              AST_Expression *operand_expr,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::ADDROF,
                                         begin_fp, end_fp);

        result->addrof.operand_expr = operand_expr;

        return result;
    }

    AST_Expression *ast_compound_expression_new(Allocator *allocator,
                                                Array<AST_Expression*> exprs,
                                                AST_Type_Spec *type_spec,
                                                const File_Pos &begin_fp,
                                                const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::COMPOUND, begin_fp,
                                         end_fp);

        result->compound.expressions = exprs;
        result->compound.type_spec = type_spec;

        return result;
    }

    AST_Expression *ast_subscript_expression_new(Allocator *allocator,
                                                 AST_Expression *pointer_expr,
                                                 AST_Expression *index_expr,
                                                 const File_Pos &bfp, const File_Pos &efp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::SUBSCRIPT, bfp, efp);
        result->subscript.pointer_expression = pointer_expr;
        result->subscript.index_expression = index_expr;
        return result;
    }

    AST_Expression *ast_cast_expression_new(Allocator *allocator, AST_Expression *operand_expr,
                                            AST_Type *target_type, const File_Pos &bfp,
                                            const File_Pos &efp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CAST, bfp, efp);
        result->cast.operand_expression = operand_expr;
        result->cast.target_type = target_type;
        return result;
    }

    AST_Expression *ast_integer_literal_expression_new(Allocator *allocator, int64_t value,
                                                       const File_Pos &begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::INTEGER_LITERAL,
                                         begin_fp, end_fp);
        
        result->integer_literal.s64 = value;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_float_literal_expression_new(Allocator *allocator, float value,
                                                     const File_Pos & begin_fp,
                                                     const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::FLOAT_LITERAL,
                                         begin_fp, end_fp);
        result->float_literal.r32 = value;
        result->expr_flags |= AST_EXPR_FLAG_CONST;
        return result;
    }

    AST_Expression *ast_string_literal_expression_new(Allocator *allocator, Atom& atom,
                                                      const File_Pos &begin_fp,
                                                      const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::STRING_LITERAL,
                                         begin_fp, end_fp);

        result->string_literal.atom = atom;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_char_literal_expression_new(Allocator *allocator, char c,
                                                      const File_Pos & begin_fp,
                                                      const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::CHAR_LITERAL,
                                         begin_fp, end_fp);

        result->char_literal.c = c;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_boolean_literal_expression_new(Allocator *allocator, bool value,
                                                       const File_Pos & begin_fp,
                                                       const File_Pos &end_fp)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::BOOL_LITERAL,
                                         begin_fp, end_fp);

        result->bool_literal.value = value;
        result->expr_flags |= AST_EXPR_FLAG_CONST;

        return result;
    }

    AST_Expression *ast_range_expression_new(Allocator *allocator, 
                                              AST_Expression *begin_expr,
                                              AST_Expression *end_expr)
    {
        auto result = ast_expression_new(allocator, AST_Expression_Kind::RANGE,
                                         begin_expr->begin_file_pos,
                                         end_expr->end_file_pos);

        result->range.begin = begin_expr;
        result->range.end = end_expr;

        return result;
    }

    AST_Type_Spec *ast_type_spec_new(Allocator *allocator, AST_Type_Spec_Kind kind,
                                     const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        AST_Type_Spec *result = ast_node_new<AST_Type_Spec>(allocator, begin_fp, end_fp);

        result->kind = kind;
        result->type = nullptr;

        return result;
    }

    AST_Type_Spec *ast_identifier_type_spec_new(Allocator *allocator, AST_Identifier *identifier,
                                                const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::IDENTIFIER, begin_fp,
                                        end_fp);

        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec *ast_pointer_type_spec_new(Allocator *allocator, AST_Type_Spec *base_ts,
                                             const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(base_ts); 

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POINTER, begin_fp, end_fp);

        result->base_type_spec = base_ts;

        return result;
    }

    AST_Type_Spec *ast_dot_type_spec_new(Allocator *allocator, AST_Expression *dot_expr,
                                         const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(dot_expr->kind == AST_Expression_Kind::DOT);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::DOT, begin_fp, end_fp);

        result->dot_expression = dot_expr;

        return result;
    }

    AST_Type_Spec *ast_function_type_spec_new(Allocator *allocator,
                                              Array<AST_Type_Spec*> param_type_specs,
                                              AST_Type_Spec *return_type_spec,
                                              const File_Pos &begin_fp, const File_Pos &end_fp)
    {
        assert(allocator);

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FUNCTION, begin_fp, end_fp);

        result->function.parameter_type_specs = param_type_specs;
        result->function.return_type_spec = return_type_spec;

        return result;
    }

    AST_Type_Spec *ast_array_type_spec_new(Allocator *allocator, AST_Expression *length_expr,
                                           AST_Type_Spec *element_ts,
                                           const File_Pos & begin_fp, const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::ARRAY, begin_fp, end_fp);

        result->array.length_expression = length_expr;
        result->array.element_type_spec = element_ts;

        return result;
    }

    AST_Type_Spec *ast_templated_type_spec_new(Allocator *allocator, AST_Expression *ident_expr,
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

    AST_Type_Spec *ast_poly_identifier_type_spec_new(Allocator *allocator, AST_Declaration *decl,
                                                     AST_Identifier *spec_ident,
                                                     const File_Pos &begin_fp,
                                                     const File_Pos &end_fp)
    {
        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::POLY_IDENTIFIER, begin_fp,
                                        end_fp);

        result->poly_identifier.declaration = decl;
        result->poly_identifier.specification_identifier = spec_ident;

        return result;
    }

    AST_Type_Spec *ast_type_spec_from_type_new(Allocator *allocator, AST_Type *type)
    {
        File_Pos fp = {};
        fp.file_name = string_ref("<from_type>");

        auto result = ast_type_spec_new(allocator, AST_Type_Spec_Kind::FROM_TYPE,
                                        fp, fp);
        result->type = type;
        return result;
    }

    AST_Type *ast_type_new(Allocator *allocator, AST_Type_Kind kind, uint64_t bit_size)
    {
        auto result = alloc_type<AST_Type>(allocator);
        static_cast<AST_Node*>(result)->kind = AST_Type::_kind;
        assert(result);
        result->kind = kind;
        result->bit_size = bit_size;
        result->pointer_to = nullptr;
        return result;
    }

    AST_Type *ast_integer_type_new(Allocator *allocator, uint64_t bit_size, bool sign)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::INTEGER, bit_size);
        result->integer.sign = sign;
        return result;
    }

    AST_Type *ast_float_type_new(Allocator *allocator, uint64_t bit_size)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::FLOAT, bit_size);
        return result;
    }

    AST_Type *ast_bool_type_new(Allocator *allocator, uint64_t bit_size)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::BOOL, bit_size);
        return result;
    }

    AST_Type *ast_pointer_type_new(Allocator *allocator, AST_Type *base_type)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::POINTER, Builtin::pointer_size);
        result->pointer.base = base_type;
        result->bit_size = Builtin::pointer_size;
        result->flags |= AST_NODE_FLAG_RESOLVED_ID;
        result->flags |= AST_NODE_FLAG_TYPED;
        result->flags |= AST_NODE_FLAG_SIZED;
        return result;
    }

    AST_Type *ast_function_type_new(Allocator *allocator, Array<AST_Type*> param_types,
                                    AST_Type *return_type)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::FUNCTION, Builtin::pointer_size);

        result->function.param_types = param_types;
        result->function.return_type = return_type;

        return result;
    }

    AST_Type *ast_structure_type_new(Allocator *allocator, AST_Declaration *declaration,
                                     Array<AST_Type*> member_types,
                                     Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::STRUCTURE, 0);
        result->structure.member_types = member_types;
        result->structure.member_scope = member_scope;
        result->structure.declaration = declaration;

        return result;
    }

    AST_Type *ast_enum_type_new(Allocator *allocator, AST_Declaration *declaration,
                                AST_Type *base_type, Scope *member_scope)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::ENUM, 0);
        result->enum_type.base_type = base_type;
        result->enum_type.member_scope = member_scope;
        result->enum_type.declaration = declaration;

        return result;
    }

    AST_Type *_ast_find_or_create_pointer_type(Allocator *allocator, AST_Type *base_type)
    {
        if (base_type->pointer_to) return base_type->pointer_to;

        auto result = ast_pointer_type_new(allocator, base_type);
        base_type->pointer_to = result;

        return result;
    }

    AST_Type *_ast_create_array_type(Allocator *allocator, AST_Type *elem_type,
                                     int64_t elem_count)
    {
        auto result = ast_type_new(allocator, AST_Type_Kind::ARRAY, 0);

        result->array.element_type = elem_type;
        result->array.element_count = elem_count;

        return result;
    }

    AST_Declaration *ast_find_enum_member(AST_Type *enum_type,
                                          Const_Value member_value)
    {
        assert(enum_type->kind == AST_Type_Kind::ENUM);

        auto decl = enum_type->enum_type.declaration;

        assert(member_value.type == enum_type);

        for (int64_t i = 0; i < decl->enum_decl.member_declarations.count; i++)
        {
            auto mem_decl = decl->enum_decl.member_declarations[i];
            auto init_expr = mem_decl->constant.init_expression;

            if (init_expr->integer_literal.u64 == member_value.integer.u64)
            {
                return mem_decl;
            }
        }

        return nullptr;
    }

    void ast_print_indent(uint64_t indent)
    {
        for (uint64_t i = 0; i < indent; i++)
        {
            printf("    ");
        }
    }

    void ast_print(AST_Node *ast_node)
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

            case AST_Node_Kind::SWITCH_CASE: assert(false);

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void ast_print_declaration(AST_Declaration *ast_decl, uint64_t indent)
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

        if (ast_decl->identifier) printf("%s", ast_decl->identifier->atom.data);

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

            case AST_Declaration_Kind::USING:
            {
                printf("using ");
                ast_print_expression(ast_decl->using_decl.ident_expr, 0);
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

            case AST_Declaration_Kind::ENUM:
            {
                printf(" :: enum\n");
                ast_print_indent(indent);
                printf("{\n");

                for (int64_t i = 0; i < ast_decl->enum_decl.member_declarations.count; i++)
                {
                    auto mem_decl = ast_decl->enum_decl.member_declarations[i];
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

    void ast_print_statement(AST_Statement *ast_stmt, uint64_t indent)
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
                }
                printf(";\n");
                break;
            }

            case AST_Statement_Kind::BREAK:
            {
                printf("break;\n");
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
                break;
            }

            case AST_Statement_Kind::WHILE:
            {
                printf("while (");
                ast_print_expression(ast_stmt->while_stmt.cond_expr, 0);
                printf(")");
                ast_print_statement(ast_stmt->while_stmt.body, indent);
                break;
            }
                                            
            case AST_Statement_Kind::IF:
            {
                printf("if (");
                ast_print_expression(ast_stmt->if_stmt.cond_expr, 0);
                printf(")");

                auto then_stmt = ast_stmt->if_stmt.then_stmt;
                auto then_indent = indent;
                if (then_stmt->kind != AST_Statement_Kind::BLOCK)
                {
                    printf("\n");
                    then_indent += 1;
                }

                ast_print_statement(then_stmt, then_indent);

                auto else_stmt = ast_stmt->if_stmt.else_stmt;
                if (else_stmt)
                {
                    ast_print_indent(indent);
                    printf("else");

                    auto else_indent = indent;
                    if (else_stmt->kind == AST_Statement_Kind::IF)
                    {
                        printf("\n");
                    }
                    else if (else_stmt->kind != AST_Statement_Kind::BLOCK)
                    {
                        printf("\n");
                        else_indent += 1;
                    }

                    ast_print_statement(else_stmt, else_indent);
                }
                break;
            }

            case AST_Statement_Kind::SWITCH:
            {
                printf("switch (");
                ast_print_expression(ast_stmt->switch_stmt.expression, 0);
                printf(")\n");
                ast_print_indent(indent);
                printf("{\n");

                indent += 1;

                for (int i = 0; i < ast_stmt->switch_stmt.cases.count; i++)
                {
                    ast_print_indent(indent);

                    AST_Switch_Case *switch_case = ast_stmt->switch_stmt.cases[i];

                    if (switch_case->is_default)
                    {
                        printf("default:");
                    }
                    else
                    {
                        printf("case ");

                        for (int64_t j = 0; j < switch_case->expressions.count; j++)
                        {
                            if (j > 0) printf(", ");

                            AST_Expression *case_expr = switch_case->expressions[j];

                            ast_print_expression(case_expr, 0);
                        }

                        printf(":");
                    }

                    ast_print_statement(switch_case->body, indent);

                    printf("\n");
                }

                indent -= 1;

                ast_print_indent(indent);
                printf("}\n");
                break;
            }
        } 
    }

    void ast_print_type_spec(AST_Type_Spec *type_spec)
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
            
            case AST_Type_Spec_Kind::FROM_TYPE:
            {
                printf("type_spec_from_type(");
                ast_print_type(type_spec->type);
                printf(")");
                break;
            }
        }
    }

    void ast_print_expression(AST_Expression *ast_expr, uint64_t indent)
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
                    case BINOP_EQ: printf(" == "); break;
                    case BINOP_NEQ: printf(" != "); break;
                    case BINOP_LT: printf(" < "); break;
                    case BINOP_LTEQ: printf(" <= "); break;
                    case BINOP_GT: printf(" > "); break;
                    case BINOP_GTEQ: printf(" >= "); break;
                    case BINOP_ADD: printf(" + "); break;
                    case BINOP_SUB: printf(" - "); break;
                    case BINOP_REMAINDER: printf(" %% "); break;
                    case BINOP_MUL: printf("  *"); break;
                    case BINOP_DIV: printf(" / "); break;
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

            case AST_Expression_Kind::ADDROF:
            {
                printf("*");
                ast_print_expression(ast_expr->addrof.operand_expr, 0);
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

            case AST_Expression_Kind::SUBSCRIPT:
            {
                ast_print_expression(ast_expr->subscript.pointer_expression, indent);
                printf("[");
                ast_print_expression(ast_expr->subscript.index_expression, 0);
                printf("]");
                break;
            }

            case AST_Expression_Kind::CAST:
            {
                printf("@compiler_cast(");
                ast_print_type(ast_expr->cast.target_type);
                printf(", ");
                ast_print_expression(ast_expr->cast.operand_expression, 0);
                printf(")");
                break;
            };

            case AST_Expression_Kind::INTEGER_LITERAL:
            {
                printf("%" PRId64, ast_expr->integer_literal.s64);
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL:
            {
                printf("%f", ast_expr->float_literal.r32);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL:
            {
                printf("\"");
                for (uint64_t i = 0; i < ast_expr->string_literal.atom.length; i++)
                {
                    char c;
                    if (parser_make_escape_char(ast_expr->string_literal.atom.data[i], &c))
                    {
                        printf("\\%c", c); 
                    }
                    else
                    {
                        printf("%c", c);
                    }
                }
                printf("\"");
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL:
            {
                const char *prefix = "";
                char c;
                if (parser_make_escape_char(ast_expr->char_literal.c, &c))
                {
                    prefix = "\\";
                }
                printf("'%s%c'", prefix, c);
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL:
            {
                printf("%s", ast_expr->bool_literal.value ? "true" : "false");
                break;
            }

            case AST_Expression_Kind::RANGE:
            {
                ast_print_expression(ast_expr->range.begin, 0);
                printf(" .. ");
                ast_print_expression(ast_expr->range.end, 0);
                break;
            }
        }
    }

    void ast_print_scope(Allocator *allocator, AST_Node *anode)
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

            case AST_Node_Kind::SWITCH_CASE: assert(false);

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

    void ast_print_declaration_scopes(String_Builder *sb, AST_Declaration *ast_decl,
                                      int64_t indent)
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

            case AST_Declaration_Kind::USING: assert(false);

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
                if (ast_decl->function.body)
                    scope_print(sb, ast_decl->function.body->block.scope, indent);
                break;
            }

            case AST_Declaration_Kind::TYPE:
            {
                string_builder_append(sb, " (type)");
                break;
            }

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

            case AST_Declaration_Kind::ENUM: assert(false);

            case AST_Declaration_Kind::POLY_TYPE:
            {
                string_builder_append(sb, " (poly_param)");
                break;
            }
        }
    }

    void ast_print_type(AST_Type *type)
    {
        auto ta = temp_allocator_get();

        String_Builder sb = {};
        string_builder_init(ta, &sb, 16);

        ast_print_type(&sb, type);

        String result = string_builder_to_string(ta, &sb);

        printf("%.*s", (int)result.length, result.data);
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

            case AST_Type_Kind::FLOAT:
            {
                if (type->bit_size == 32) string_builder_append(sb, "float");
                else if (type->bit_size == 64) string_builder_append(sb, "double");
                else assert(false);
                break; 
            }

            case AST_Type_Kind::BOOL:
            {
                string_builder_appendf(sb, "b%" PRIu64, type->bit_size);
                break;
            }

            case AST_Type_Kind::POINTER:
            {
                string_builder_append(sb, "*");
                ast_print_type(sb, type->pointer.base);
                break;
            }

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

            case AST_Type_Kind::ENUM:
            {
                auto decl = type->enum_type.declaration;
                string_builder_appendf(sb, "enum(%s)", decl->identifier->atom.data);
                break;
            }

            case AST_Type_Kind::ARRAY:
            {
                string_builder_appendf(sb, "[%" PRId64 "]", type->array.element_count);
                ast_print_type(sb, type->array.element_type);
                break;
            }
        }
    }

    String ast_type_to_string(Allocator *allocator, AST_Type *type)
    {
        String_Builder sb;
        string_builder_init(allocator, &sb, 32);

        ast_print_type(&sb, type);

        auto result = string_builder_to_string(allocator, &sb);

        string_builder_free(&sb);

        return result;
    }
}
