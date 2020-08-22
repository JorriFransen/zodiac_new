#include "parser.h"

#include "allocator.h"
#include "temp_allocator.h"
#include "builtin.h"

#include <stdio.h>
#include <cstdarg>
#include <inttypes.h>

namespace Zodiac
{

Parser parser_create(Allocator* allocator, Build_Data *build_data)
{
    Parser result = {};
    parser_init(allocator, &result, build_data);
    return result;
}

void parser_init(Allocator* allocator, Parser* parser, Build_Data *build_data)
{
    parser->allocator = allocator;
    parser->build_data = build_data;
}

Parsed_File parser_parse_file(Parser* parser, Token_Stream* ts)
{
    Parsed_File result = {};
    array_init(parser->allocator, &result.declarations);

    while (ts->current_token().kind != TOK_EOF)
    {
        auto ptn = parser_parse_declaration(parser, ts);
        array_append(&result.declarations, ptn);
    }

    return result;
}

Declaration_PTN* parser_parse_declaration(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    bool is_naked = false;
    bool is_noreturn = false;
    bool is_foreign = false;

    while (ts->current_token().kind == TOK_POUND)
    {
        auto directive_tok = ts->next_token();
        if (directive_tok.atom == Builtin::atom_naked)
        {
            is_naked = true;
        }
        else if (directive_tok.atom == Builtin::atom_noreturn)
        {
            is_noreturn = true;
        }
        else if (directive_tok.atom == Builtin::atom_foreign)
        {
            is_foreign = true;
        }
        else assert(false);
        ts->next_token();
    }

    auto identifier = parser_parse_identifier(parser, ts);
    if (!identifier) return nullptr;

    return parser_parse_declaration(parser, ts, identifier, is_naked, is_noreturn, is_foreign);
}

Declaration_PTN* parser_parse_declaration(Parser* parser, Token_Stream* ts,
                                          Identifier_PTN* identifier,
                                          bool is_naked /*= false*/,
                                          bool is_noreturn /*= false*/,
                                          bool is_foreign /*=false*/)
{
    if (!parser_expect_token(parser, ts, TOK_COLON)) return nullptr;

    Declaration_PTN* result = nullptr;
    Expression_PTN* specified_type = nullptr;

    File_Pos end_fp = {};

    if (!parser_is_token(ts, TOK_COLON) && !parser_is_token(ts, TOK_EQ))
    {
        specified_type = parser_parse_expression(parser, ts);
        end_fp = ts->current_token().end_file_pos;
        if (parser_is_token(ts, TOK_SEMICOLON))
        {
            return new_variable_declaration_ptn(parser->allocator, identifier, specified_type,
                                                nullptr, identifier->self.begin_file_pos,
                                                end_fp);
        }
    }

    if (parser_match_token(ts, TOK_COLON))
    {
        if (parser_is_token(ts, TOK_KW_FUNC))
        {
            assert(!specified_type);
            auto function_proto = parser_parse_function_prototype(parser, ts);
            assert(function_proto);

            end_fp = function_proto->self.end_file_pos;

            Statement_PTN* function_body = nullptr;
            auto lbrace_efp = ts->current_token().end_file_pos;
            if (parser_is_token(ts, TOK_LBRACE))
            {
                function_body = parser_parse_statement(parser, ts);
                assert(function_body);
                assert(function_body->kind == Statement_PTN_Kind::BLOCK);
                end_fp = lbrace_efp;
            }

            result = new_function_declaration_ptn(parser->allocator, identifier, function_proto,
                                                  function_body, identifier->self.begin_file_pos,
                                                  end_fp);
        }
        else if (parser_is_token(ts, TOK_KW_STRUCT))
        {
            assert(!specified_type);
            result = parser_parse_struct_declaration(parser, ts, identifier);
        }
        else if (parser_is_token(ts, TOK_KW_IMPORT))
        {
            assert(!specified_type);
            result = parser_parse_import_declaration(parser, ts, identifier);
        }
        else
        {
            Expression_PTN* const_expr = parser_parse_expression(parser, ts);
            assert(const_expr);
            auto sc_efp = ts->current_token().end_file_pos;
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                assert(false);
            }
            else
            {
                end_fp = sc_efp;
            }
            result = new_constant_declaration_ptn(parser->allocator, identifier, nullptr,
                                                  const_expr, identifier->self.begin_file_pos,
                                                  end_fp);
        }
    }
    else if (parser_match_token(ts, TOK_EQ))
    {
        auto expression = parser_parse_expression(parser, ts);

        result = new_variable_declaration_ptn(parser->allocator, identifier, specified_type,
                                              expression, identifier->self.begin_file_pos,
                                              expression->self.end_file_pos);
    }
    else
    {
        assert(false);
    }

    assert(result);
    if (is_naked)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->flags |= DPTN_FLAG_IS_NAKED;
    }
    if (is_noreturn)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->flags |= DPTN_FLAG_NORETURN;
    }
    if (is_foreign)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->flags |= DPTN_FLAG_FOREIGN;
    }
    return result;
}

Declaration_PTN* parser_parse_struct_declaration(Parser* parser, Token_Stream* ts,
                                                 Identifier_PTN* identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_STRUCT)) assert(false);

    Array<Parameter_PTN*> parameters = {};
    if (parser_match_token(ts, TOK_LPAREN))
    {
        parameters = parser_parse_parameter_list(parser, ts);
        assert(parameters.count);

        if (!parser_expect_token(parser, ts, TOK_RPAREN)) assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) assert(false);

    Array<Declaration_PTN*> member_decls = {};
    array_init(parser->allocator, &member_decls);

    File_Pos end_fp = {};

    while (!parser_match_token(ts, TOK_RBRACE))
    {
        assert(parser_is_token(ts, TOK_IDENTIFIER));
        if (ts->peek_token(1).kind == TOK_COMMA)
        {
            auto ta = temp_allocator_get();
            Array<Identifier_PTN*> identifiers = {};
            array_init(ta, &identifiers, 4);

            while (true)
            {
                auto identifier = parser_parse_identifier(parser, ts);
                assert(identifier);
                array_append(&identifiers, identifier);

                if (!parser_match_token(ts, TOK_COMMA))
                {
                    break;
                }
            }

            assert(identifiers.count);

            auto first_mem_decl = parser_parse_declaration(parser, ts, identifiers[0]);
            assert(first_mem_decl);
            assert(first_mem_decl->kind == Declaration_PTN_Kind::VARIABLE ||
                   first_mem_decl->kind == Declaration_PTN_Kind::CONSTANT);
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON)) assert(false);
            array_append(&member_decls, first_mem_decl);

            for (int64_t i = 1; i < identifiers.count; i++)
            {
                auto mem_decl = copy_declaration_ptn(parser->allocator, first_mem_decl); 
                assert(mem_decl);
                assert(mem_decl->kind == Declaration_PTN_Kind::VARIABLE ||
                       mem_decl->kind == Declaration_PTN_Kind::CONSTANT);
                mem_decl->identifier = identifiers[i];
                array_append(&member_decls, mem_decl);

                end_fp = ts->current_token().end_file_pos;
            }

            array_free(&identifiers);
        }
        else
        {
            auto decl = parser_parse_declaration(parser, ts);
            array_append(&member_decls, decl);

            if (decl->kind != Declaration_PTN_Kind::FUNCTION)
            {
                if (!parser_expect_token(parser, ts, TOK_SEMICOLON)) assert(false);
            }

            end_fp = ts->current_token().end_file_pos;
        }
    }

    if (!member_decls.count)
    {
        array_free(&member_decls);
    }

    return new_struct_declaration_ptn(parser->allocator, identifier, member_decls, parameters,
                                      identifier->self.begin_file_pos, end_fp);
}

Declaration_PTN* parser_parse_import_declaration(Parser* parser, Token_Stream* ts,
                                                 Identifier_PTN* identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_IMPORT))
    {
        assert(false);
    }

    Expression_PTN* ident_expr = parser_parse_expression(parser, ts);
    assert(ident_expr);

    auto end_fp = ts->current_token().end_file_pos;

    if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
    {
        assert(false);
    }

    return new_import_declaration_ptn(parser->allocator, identifier, ident_expr,
                                      identifier->self.begin_file_pos, end_fp);
}

Identifier_PTN* parser_parse_identifier(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    auto ident_token = ts->current_token();
    if (!parser_expect_token(parser, ts, TOK_IDENTIFIER)) return nullptr;

    auto begin_fp = ident_token.begin_file_pos;
    auto end_fp = ident_token.end_file_pos;
    return new_identifier_ptn(parser->allocator, ident_token.atom, begin_fp, end_fp);
}

Function_Proto_PTN* parser_parse_function_prototype(Parser* parser, Token_Stream* ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;
    File_Pos end_fp = {};

    if (!parser_expect_token(parser, ts, TOK_KW_FUNC))
    {
        assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        assert(false);
    }

    auto param_list = parser_parse_parameter_list(parser, ts);

    end_fp = ts->current_token().end_file_pos;

    if (!parser_expect_token(parser, ts, TOK_RPAREN))
    {
        assert(false);
    }

    Expression_PTN* return_type_expr = nullptr;
    if (parser_match_token(ts, TOK_RARROW))
    {
        return_type_expr = parser_parse_expression(parser, ts, true);
        end_fp = return_type_expr->self.end_file_pos;
    }

    return new_function_prototype_parse_tree_node(parser->allocator, param_list, return_type_expr,
                                                  begin_fp, end_fp);
}

Array<Parameter_PTN*> parser_parse_parameter_list(Parser* parser, Token_Stream* ts)
{
    Array<Parameter_PTN*> parameters = {};
    array_init(parser->allocator, &parameters, 2);

    while (!parser_is_token(ts, TOK_RPAREN))
    {
        if (parameters.count)
        {
            if (!parser_expect_token(parser, ts, TOK_COMMA))
            {
                assert(false);
            }
        }

        Parameter_PTN* parameter = parser_parse_parameter(parser, ts);
        assert(parameter);
        array_append(&parameters, parameter);
    }

    if (!parameters.count)
    {
        array_free(&parameters);
    }

    return parameters;
}

Parameter_PTN* parser_parse_parameter(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    Identifier_PTN* identifier = parser_parse_identifier(parser, ts);

    if (parser_match_token(ts, TOK_COLON))
    {
        Expression_PTN* type_expr = parser_parse_expression(parser, ts);

        assert(identifier);
        assert(type_expr);

        return new_parameter_ptn(parser->allocator, identifier, type_expr,
                                 identifier->self.begin_file_pos, type_expr->self.end_file_pos);
    }
    else
    {
        return new_parameter_ptn(parser->allocator, identifier, nullptr,
                                 identifier->self.begin_file_pos, identifier->self.end_file_pos);
    }
}

Statement_PTN* parser_parse_statement(Parser* parser, Token_Stream* ts)
{
    auto ct = ts->current_token();

    auto begin_fp = ct.begin_file_pos;

    switch (ct.kind)
    {
        case TOK_LBRACE:
        {
            ts->next_token();
            Array<Statement_PTN*> block_statements = {};
            array_init(parser->allocator, &block_statements);

            File_Pos end_fp = {};

            while (!parser_match_token(ts, TOK_RBRACE))
            {
                auto statement = parser_parse_statement(parser, ts);
                array_append(&block_statements, statement);

                end_fp = ts->current_token().end_file_pos; 
            }

            if (!block_statements.count) array_free(&block_statements);

            return new_block_statement_ptn(parser->allocator, block_statements, begin_fp, end_fp);
            break;
        }

        case TOK_IDENTIFIER:
        {
            Statement_PTN* result = nullptr;
            if (ts->peek_token(1).kind == TOK_COLON)
            {
                auto decl = parser_parse_declaration(parser, ts);
                assert(decl);
                result = new_declaration_statement_ptn(parser->allocator, decl, begin_fp,
                                                       decl->self.end_file_pos);
            }
            else
            {
                auto expr = parser_parse_expression(parser, ts);
                assert(expr);

                if (parser_is_token(ts, TOK_EQ))
                {
                    assert(expr->kind == Expression_PTN_Kind::IDENTIFIER ||
                           expr->kind == Expression_PTN_Kind::DOT);

                    result = parser_parse_assignment_statement(parser, ts, expr);
                }
                else if (parser_is_add_op(ts) && ts->peek_token(1).kind == TOK_EQ)
                {
                    assert(expr->kind == Expression_PTN_Kind::IDENTIFIER ||
                           expr->kind == Expression_PTN_Kind::DOT);

                    result = parser_parse_self_assignment_statement(parser, ts, expr);
                }
                else
                {
                    result = new_expression_statement_ptn(parser->allocator, expr, begin_fp,
                                                          expr->self.end_file_pos);
                }
            }
            assert(result);
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                assert(false);
            }
            return result;
            break;
        }

        case TOK_KW_RETURN:
        {
            ts->next_token();
            Expression_PTN* expr = nullptr;
            bool found_semicolon = false;

            File_Pos end_fp = ts->current_token().end_file_pos;

            if (!parser_match_token(ts, TOK_SEMICOLON))
            {
                expr = parser_parse_expression(parser, ts);
                assert(expr);
            }
            else
            {
                found_semicolon = true;
            }

            if (!found_semicolon)
            {
                end_fp = ts->current_token().end_file_pos;
                if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
                {
                    assert(false);
                }
            }

            auto result = new_return_statement_ptn(parser->allocator, expr, begin_fp, end_fp);
            assert(result);

            return result;
            break;
        }

        default:
        {
            auto expr = parser_parse_expression(parser, ts);
            assert(expr);
            auto end_fp = ts->current_token().end_file_pos;
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                assert(false);
            }
            return new_expression_statement_ptn(parser->allocator, expr, begin_fp, end_fp);
        }
    }
}

Statement_PTN* parser_parse_assignment_statement(Parser* parser, Token_Stream* ts,
                                                 Expression_PTN* ident_expression)
{
    assert(ident_expression->kind == Expression_PTN_Kind::IDENTIFIER ||
           ident_expression->kind == Expression_PTN_Kind::DOT);

    if (!parser_expect_token(parser, ts, TOK_EQ))
    {
        assert(false);
    }

    Expression_PTN* rhs_value = parser_parse_expression(parser, ts);
    assert(rhs_value);

    return new_assignment_statement_ptn(parser->allocator, ident_expression, rhs_value,
                                        ident_expression->self.begin_file_pos,
                                        rhs_value->self.end_file_pos);
}

Statement_PTN* parser_parse_self_assignment_statement(Parser* parser, Token_Stream* ts,
                                                      Expression_PTN* ident_expression)
{
    assert(ident_expression->kind == Expression_PTN_Kind::IDENTIFIER ||
           ident_expression->kind == Expression_PTN_Kind::DOT);

    Binary_Operator op = BINOP_INVALID;

    if (parser_is_add_op(ts))
    {
        op = parser_parse_add_op(ts);
    }
    else assert(false);

    if (!parser_expect_token(parser, ts, TOK_EQ))
    {
        assert(false);
    }


    Expression_PTN* rhs_expr = parser_parse_expression(parser, ts);
    assert(rhs_expr);

    auto begin_fp = ident_expression->self.begin_file_pos;
    auto end_fp = rhs_expr->self.end_file_pos;

    rhs_expr = new_binary_expression_ptn(parser->allocator, op, ident_expression, rhs_expr,
                                         begin_fp, end_fp);

    return new_assignment_statement_ptn(parser->allocator, ident_expression, rhs_expr,
                                        ident_expression->self.begin_file_pos,
                                        rhs_expr->self.end_file_pos);
}

Expression_PTN* parser_parse_expression(Parser* parser, Token_Stream* ts, bool is_type/*=false*/)
{
    return parser_parse_add_expression(parser, ts, is_type);
}

Expression_PTN* parser_parse_add_expression(Parser* parser, Token_Stream* ts, bool is_type/*=false*/)
{
    auto lhs = parser_parse_unary_expression(parser, ts, is_type);
    assert(lhs);

    while (parser_is_add_op(ts) && !(ts->peek_token(1).kind == TOK_EQ))
    {
        auto op = parser_parse_add_op(ts);
        auto rhs = parser_parse_unary_expression(parser, ts, is_type);
        assert(rhs);
        
        auto begin_fp = lhs->self.begin_file_pos;
        auto end_fp = rhs->self.end_file_pos;
        lhs = new_binary_expression_ptn(parser->allocator, op, lhs, rhs, begin_fp, end_fp);
    }

    return lhs;
}

Expression_PTN* parser_parse_unary_expression(Parser* parser, Token_Stream* ts,
                                              bool is_type/*=false*/)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    auto op = parser_parse_unary_op(ts); if (op != UNOP_INVALID)
    {
        auto operand_expr = parser_parse_unary_expression(parser, ts, is_type);
        auto end_fp = operand_expr->self.end_file_pos;
        return new_unary_expression_ptn(parser->allocator, op, operand_expr, begin_fp, end_fp);
    }
    else
    {
        return parser_parse_base_expression(parser, ts, is_type);
    }
}

Expression_PTN* parser_parse_base_expression(Parser* parser, Token_Stream* ts,
                                             bool is_type/*=false*/)
{
    auto ct = ts->current_token();
    auto begin_fp = ct.begin_file_pos;

    Expression_PTN* result = nullptr;

    switch (ct.kind)
    {
        case TOK_IDENTIFIER:
        {
            if (ts->peek_token(1).kind == TOK_LPAREN)
            {
                auto call_expr = parser_parse_call_expression(parser, ts);
                assert(call_expr);

                if (!is_type && parser_match_token(ts, TOK_LBRACE))
                {
                    auto expr_list = parser_parse_expression_list(parser, ts);
                    assert(expr_list);
                    auto end_fp = ts->current_token().end_file_pos;
                    if (!parser_expect_token(parser, ts, TOK_RBRACE)) assert(false);

                    result = new_compound_expression_ptn(parser->allocator, expr_list, call_expr,
                                                         begin_fp, end_fp);
                }
                else
                {
                    result = call_expr;
                }
            }
            else if (!is_type && ts->peek_token(1).kind == TOK_LBRACE)
            {
                auto type_ident = parser_parse_identifier(parser, ts);
                auto end_fp = type_ident->self.end_file_pos;
                auto type_expr = new_identifier_expression_ptn(parser->allocator, type_ident,
                                                               begin_fp, end_fp);

                assert(type_expr);

                if (!parser_expect_token(parser, ts, TOK_LBRACE)) assert(false);
                auto expr_list = parser_parse_expression_list(parser, ts);
                assert(expr_list);
                end_fp = ts->current_token().end_file_pos;
                if (!parser_expect_token(parser, ts, TOK_RBRACE)) assert(false);

                result = new_compound_expression_ptn(parser->allocator, expr_list, type_expr,
                                                     begin_fp, end_fp);
            }
            else
            {
                auto identifier = parser_parse_identifier(parser, ts);
                auto end_fp = identifier->self.end_file_pos;
                result = new_identifier_expression_ptn(parser->allocator, identifier, begin_fp,
                                                       end_fp);
            }
            break;
        }

        case TOK_AT:
        {
            ts->next_token();
            result = parser_parse_call_expression(parser, ts, true);
            break;
        }

        case TOK_NUMBER_LITERAL:
        {
            result = parser_parse_number_literal_expression(parser, ts);
            break;
        }

        case TOK_STRING_LITERAL:
        {
            result = parser_parse_string_literal_expression(parser, ts);
            break;
        }

        case TOK_LBRACK:
        {
            result = parser_parse_array_type_expression(parser, ts);
            break;
        }

        case TOK_LBRACE:
        {
            ts->next_token();
            auto expr_list= parser_parse_expression_list(parser, ts);
            assert(expr_list);
            auto end_fp = ts->current_token().end_file_pos;
            if (!parser_expect_token(parser, ts, TOK_RBRACE)) 
            {
                assert(false);
            }
            result = new_compound_expression_ptn(parser->allocator, expr_list, nullptr,
                                                 begin_fp, end_fp);
            break;
        }

        case TOK_STAR:
        {
            result = parser_parse_pointer_type_expression(parser, ts);
            break;
        }

        case TOK_DOLLAR:
        {
            result = parser_parse_poly_type_expression(parser, ts);
            break;
        }

        default:
        {
            auto ct = ts->current_token();
            parser_report_error(parser, ts, "Unexpected token when parsing expression: '%s', '%s'",
                                token_kind_name(ct.kind), ct.atom.data);
            break;
        }
    }

    assert(result);

    while (true)
    {
        if (parser_match_token(ts, TOK_DOT))
        {
            auto parent = result;
            Identifier_PTN* child_ident = parser_parse_identifier(parser, ts);
            auto end_fp = child_ident->self.end_file_pos;
            result = new_dot_expression_ptn(parser->allocator, parent, child_ident, begin_fp,
                                            end_fp);
        }
        else if (parser_is_token(ts, TOK_LPAREN))
        {
            result = parser_parse_call_expression(parser, ts, result, false);
        }
        else
        {
            break;
        }
    }

    return result;
}

Expression_PTN* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                             Expression_PTN* ident_expr, bool is_builtin/*=false*/)
{
    auto begin_fp = ident_expr->self.begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        assert(false);
    }

    Expression_List_PTN* arg_list = nullptr;
    if (!parser_is_token(ts, TOK_RPAREN))
    {
        arg_list = parser_parse_expression_list(parser, ts);
    }

    auto end_fp = ts->current_token().end_file_pos;

    if (!parser_expect_token(parser, ts, TOK_RPAREN))
    {
        assert(false);
    }

    return new_call_expression_ptn(parser->allocator, is_builtin, ident_expr, arg_list, begin_fp,
                                   end_fp);
}

Expression_PTN* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                             bool is_builtin/*=false*/)
{
    auto identifier = parser_parse_identifier(parser, ts);
    assert(identifier);

    auto begin_fp = identifier->self.begin_file_pos;
    auto end_fp = identifier->self.end_file_pos;

    auto ident_expr = new_identifier_expression_ptn(parser->allocator, identifier, begin_fp,
                                                    end_fp);

    return parser_parse_call_expression(parser, ts, ident_expr, is_builtin);
}

Expression_PTN* parser_parse_number_literal_expression(Parser* parser, Token_Stream* ts)
{
    auto num_tok = ts->current_token();
    auto begin_fp = num_tok.begin_file_pos;
    auto end_fp = num_tok.end_file_pos;
    ts->next_token();

    return new_number_literal_expression_ptn(parser->allocator, num_tok.atom, begin_fp, end_fp);
}

Expression_PTN* parser_parse_string_literal_expression(Parser* parser, Token_Stream* ts)
{
    auto string_tok = ts->current_token();
    auto begin_fp = string_tok.begin_file_pos;
    auto end_fp = string_tok.end_file_pos;
    ts->next_token();

    return new_string_literal_expression_ptn(parser->allocator, string_tok.atom, begin_fp, end_fp);
}

Expression_PTN* parser_parse_array_type_expression(Parser* parser, Token_Stream* ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_LBRACK))
    {
        assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_RBRACK))
    {
        assert(false);
    }

    Expression_PTN* element_type = parser_parse_expression(parser, ts);
    assert(element_type);

    auto end_fp = element_type->self.end_file_pos;

    return new_array_type_expression_ptn(parser->allocator, element_type, begin_fp, end_fp);
}

Expression_PTN* parser_parse_pointer_type_expression(Parser* parser, Token_Stream* ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_STAR))
    {
        assert(false);
    }

    Expression_PTN* pointee_type_expression = parser_parse_expression(parser, ts);
    assert(pointee_type_expression);

    auto end_fp = pointee_type_expression->self.end_file_pos;

    return new_pointer_type_expression_ptn(parser->allocator, pointee_type_expression, begin_fp,
                                           end_fp);
}

Expression_PTN* parser_parse_poly_type_expression(Parser* parser, Token_Stream* ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_DOLLAR)) assert(false);

    auto identifier = parser_parse_identifier(parser, ts);
    assert(identifier);

    auto end_fp = identifier->self.end_file_pos;

    Identifier_PTN* spec_ident = nullptr;
    if (parser_match_token(ts, TOK_FORWARD_SLASH))
    {
        spec_ident = parser_parse_identifier(parser, ts);
        end_fp = spec_ident->self.end_file_pos;
    }

    return new_poly_type_expression_ptn(parser->allocator, identifier, spec_ident, begin_fp,
                                        end_fp);
}

Expression_List_PTN* parser_parse_expression_list(Parser* parser, Token_Stream* ts)
{
    Array<Expression_PTN*> expressions = {};
    array_init(parser->allocator, &expressions, 4);

    auto first = parser_parse_expression(parser, ts);
    assert(first);
    array_append(&expressions, first);

    auto begin_fp = first->self.begin_file_pos;

    while (parser_match_token(ts, TOK_COMMA))
    {
        auto expr = parser_parse_expression(parser, ts);
        assert(expr);
        array_append(&expressions, expr);
    }

    auto end_fp = array_last(&expressions)->self.end_file_pos;

    if (!expressions.count)
    {
        assert(false);
        array_free(&expressions);
    }

    return new_expression_list_ptn(parser->allocator, expressions, begin_fp, end_fp);
}

Binary_Operator parser_parse_add_op(Token_Stream* ts)
{
    auto ct = ts->current_token();
    Binary_Operator result = BINOP_INVALID;
    switch (ct.kind)
    {
        case TOK_PLUS:
        {
            result = BINOP_ADD;
            break;
        }

        case TOK_MINUS:
        {
            result = BINOP_SUB;
            break;
        }

        default: assert(false);
    }

    ts->next_token();
    return result;
}

bool parser_expect_token(Parser* parser, Token_Stream* ts, Token_Kind kind)
{
    assert(parser);

    if (!parser_match_token(ts, kind))
    {
        auto ct = ts->current_token();
        auto fp = ct.begin_file_pos;
        fprintf(stderr, "%s:%" PRIu64 ":%" PRIu64 ": ", fp.file_name.data, fp.line, fp.column);
        fprintf(stderr, "Error: Expected token: \"%s\", got: \"%s\"\n",
                token_kind_name(kind), token_kind_name(ct.kind));
        assert(false); // report error
        return false;
    }

    return true;
}

bool parser_match_token(Token_Stream* ts, Token_Kind kind)
{
    if (parser_is_token(ts, kind))
    {
        ts->next_token();
        return true;
    }

    return false;
}

bool parser_is_token(Token_Stream* ts, Token_Kind kind)
{
    auto ct = ts->current_token();

    if (ct.kind != kind)
    {
        return false;
    }

    return true;
}

bool parser_is_add_op(Token_Stream* ts)
{
    auto ct = ts->current_token();
    return ct.kind == TOK_PLUS || ct.kind == TOK_MINUS;
}

Unary_Operator parser_parse_unary_op(Token_Stream* ts)
{
    auto ct = ts->current_token();

    Unary_Operator result = UNOP_INVALID;

    switch (ct.kind)
    {
        case TOK_LT:
        {
            result = UNOP_DEREF;
            break;
        }

        default: result = UNOP_INVALID;
    }

    if (result != UNOP_INVALID)
    {
        ts->next_token();
    }

    return result;
}

void parser_report_error(Parser* parser, Token_Stream* ts, const char* format, ...)
{
    assert(parser);
    assert(ts);
    assert(format);

    va_list args;
    va_start(args, format);
    parser_report_error(parser, ts, format, args);
    va_end(args);
}

void parser_report_error(Parser* parser, Token_Stream* ts, const char* format, va_list args)
{
    assert(parser);
    assert(ts);
    assert(format);

    auto ct = ts->current_token();
    auto bfp = ct.begin_file_pos;

    fprintf(stderr, "%s:%" PRIu64 ":%" PRIu64 ": Error: ",
            bfp.file_name.data, bfp.line, bfp.column);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
}

void parsed_file_print(Parsed_File* parsed_file)
{
    for (int64_t i = 0; i < parsed_file->declarations.count; i++)
    {
        print_declaration_ptn(parsed_file->declarations[i], 0);
    }
}

} 
