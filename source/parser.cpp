#include "parser.h"

#include "allocator.h"
#include "temp_allocator.h"
#include "builtin.h"

#include <stdio.h>
#include <cstdarg>
#include <inttypes.h>

#include <tracy/Tracy.hpp>

namespace Zodiac
{

Parser parser_create(Allocator *allocator, Build_Data *build_data)
{
    Parser result = {};
    parser_init(allocator, &result, build_data);
    return result;
}

void parser_init(Allocator *allocator, Parser *parser, Build_Data *build_data)
{
    parser->allocator = allocator;
    parser->build_data = build_data;
}

void parsed_file_init(Parser *parser, Parsed_File *pf)
{
    array_init(parser->allocator, &pf->declarations);
}

Parsed_File parser_parse_file(Parser *parser, Token_Stream *ts)
{
    Parsed_File result = {};
    
    parsed_file_init(parser, &result);
    parser_parse_file(parser, ts, &result);

    return result;
}

void parser_parse_file(Parser *parser, Token_Stream *ts, Parsed_File *pf)
{
    ZoneScoped

    while (ts->current_token().kind != TOK_EOF)
    {
        auto ptn = parser_parse_declaration(parser, ts);

        if (!ptn)
        {
            pf->valid = false;
            break;
        }

        if (!(ptn->self.flags & PTN_FLAG_SEMICOLON) &&
            ptn->kind != Declaration_PTN_Kind::FUNCTION &&
            ptn->kind != Declaration_PTN_Kind::STRUCT &&
            ptn->kind != Declaration_PTN_Kind::ENUM)
        {
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                pf->valid = false;
                break;
            }
            else
            {
                ptn->self.flags |= PTN_FLAG_SEMICOLON;
            }
        }
        array_append(&pf->declarations, ptn);
    }
}

void parser_free_parsed_file(Parser *parser, Parsed_File *parsed_file)
{
    for (int64_t i = 0; i < parsed_file->declarations.count; i++)
    {
        free_ptn(parser->allocator, parsed_file->declarations[i]);
    }

    array_free(&parsed_file->declarations);
}

Declaration_PTN *parser_parse_declaration(Parser *parser, Token_Stream *ts)
{
    assert(parser);
    assert(ts);

    bool is_naked = false;
    bool is_noreturn = false;
    bool is_foreign = false;

    if (parser_is_token(ts, TOK_POUND) && ts->peek_token(1).kind == TOK_KW_IF)
    {
        return parser_parse_static_if_declaration(parser, ts);
    }
    else if (parser_is_token(ts, TOK_AT) && ts->peek_token(1).kind == TOK_IDENTIFIER)
    {
        // Static assert on declaration level

        auto bfp = ts->current_token().begin_file_pos;

        auto id_tok = ts->next_token();
        assert(id_tok.atom == Builtin::atom_static_assert);
        ts->next_token();

        if (!parser_expect_token(parser, ts, TOK_LPAREN)) return nullptr;

        auto cond_expr = parser_parse_expression(parser, ts);
        if (!cond_expr) return nullptr;

        auto efp = ts->current_token().end_file_pos;
        if (!parser_expect_token(parser, ts, TOK_RPAREN)) return nullptr;


        auto result = new_static_assert_declaration_ptn(parser->allocator, cond_expr, bfp, efp);

        if (parser_match_token(ts, TOK_SEMICOLON))
        {
            result->self.flags |= PTN_FLAG_SEMICOLON;
        }

        return result;
    }

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
        else 
        {
            zodiac_report_error(parser->build_data, Zodiac_Error_Kind::INVALID_DIRECTIVE, 
                                directive_tok.begin_file_pos, directive_tok.end_file_pos,
                                "Invalid directive: '#%s'", directive_tok.atom.data);
            return nullptr;
        }
        ts->next_token();
    }

    if (ts->current_token().kind == TOK_KW_USING)
    {
        auto begin_fp = ts->current_token().begin_file_pos;
        ts->next_token();
        auto import_expr = parser_parse_expression(parser, ts);
        assert(import_expr);

        auto end_fp = ts->current_token().end_file_pos;

        if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
        {
            return nullptr;
        }

        auto result = new_using_declaration_ptn(parser->allocator, import_expr, begin_fp,
                                                end_fp);
        result->self.flags |= PTN_FLAG_SEMICOLON;
        return result;
    }

    auto identifier = parser_parse_identifier(parser, ts);
    if (!identifier) return nullptr;

    return parser_parse_declaration(parser, ts, identifier, is_naked, is_noreturn,
                                    is_foreign);
}

Declaration_PTN *parser_parse_declaration(Parser *parser, Token_Stream *ts,
                                          Identifier_PTN *identifier,
                                          bool is_naked /*= false*/,
                                          bool is_noreturn /*= false*/,
                                          bool is_foreign /*=false*/)
{
    if (!parser_expect_token(parser, ts, TOK_COLON)) return nullptr;

    Declaration_PTN *result = nullptr;
    Expression_PTN *specified_type = nullptr;

    File_Pos end_fp = {};

    if (!parser_is_token(ts, TOK_COLON) && !parser_is_token(ts, TOK_EQ))
    {
        specified_type = parser_parse_expression(parser, ts);
        end_fp = ts->current_token().end_file_pos;
        if (parser_match_token(ts, TOK_SEMICOLON))
        {
            auto result = new_variable_declaration_ptn(parser->allocator, identifier,
                                                       specified_type, nullptr,
                                                       identifier->self.begin_file_pos,
                                                       end_fp);
            result->self.flags |= PTN_FLAG_SEMICOLON;
            return result;
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

            Statement_PTN *function_body = nullptr;
            auto lbrace_efp = ts->current_token().end_file_pos;
            if (parser_is_token(ts, TOK_LBRACE))
            {
                function_body = parser_parse_statement(parser, ts);

                if (!function_body) return nullptr;

                assert(function_body->kind == Statement_PTN_Kind::BLOCK);
                end_fp = lbrace_efp;
            }

            result = new_function_declaration_ptn(parser->allocator, identifier,
                                                  function_proto,
                                                  function_body,
                                                  identifier->self.begin_file_pos,
                                                  end_fp);

            parser_match_token(ts, TOK_SEMICOLON);
            result->self.flags |= PTN_FLAG_SEMICOLON;
        }
        else if (parser_is_token(ts, TOK_KW_TYPEDEF))
        {
            ts->next_token();
            auto type_expr = parser_parse_expression(parser, ts); 
            assert(type_expr);

            result = new_typedef_declaration_ptn(parser->allocator, identifier,
                                                 type_expr,
                                                 identifier->self.begin_file_pos,
                                                 type_expr->self.end_file_pos);
        }
        else if (parser_is_token(ts, TOK_KW_STRUCT))
        {
            assert(!specified_type);
            result = parser_parse_struct_declaration(parser, ts, identifier);
        }
        else if (parser_is_token(ts, TOK_KW_UNION))
        {
            assert(!specified_type);
            result = parser_parse_union_declaration(parser, ts, identifier);
        }
        else if (parser_is_token(ts, TOK_KW_ENUM))
        {
            result = parser_parse_enum_declaration(parser, ts, identifier,
                                                   specified_type);
        }
        else if (parser_is_token(ts, TOK_KW_IMPORT))
        {
            assert(!specified_type);
            result = parser_parse_import_declaration(parser, ts, identifier);
        }
        else
        {
            Expression_PTN *const_expr = parser_parse_expression(parser, ts);
            assert(const_expr);
            end_fp = ts->current_token().end_file_pos;
            result = new_constant_declaration_ptn(parser->allocator, identifier, nullptr,
                                                  const_expr,
                                                  identifier->self.begin_file_pos,
                                                  end_fp);
        }
    }
    else if (parser_match_token(ts, TOK_EQ))
    {
        auto expression = parser_parse_expression(parser, ts);
        if (!expression) return nullptr;

        auto sc_efp = ts->current_token().end_file_pos;
        auto end_fp = expression->self.end_file_pos;
        if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
        {
            assert(false);
        }
        else
        {
            end_fp = sc_efp;
        }

        result = new_variable_declaration_ptn(parser->allocator, identifier,
                                              specified_type, expression,
                                              identifier->self.begin_file_pos, end_fp);
        result->self.flags |= PTN_FLAG_SEMICOLON;
    }
    else
    {
        assert(false);
    }

    assert(result);
    if (is_naked)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->self.flags |= PTN_FLAG_DECL_IS_NAKED;
    }
    if (is_noreturn)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->self.flags |= PTN_FLAG_FUNC_NORETURN;
    }
    if (is_foreign)
    {
        assert(result->kind == Declaration_PTN_Kind::FUNCTION);
        result->self.flags |= PTN_FLAG_FUNC_FOREIGN;
    }
    return result;
}

Declaration_PTN *parser_parse_struct_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_STRUCT)) assert(false);

    Array<Parameter_PTN*> parameters = {};
    if (parser_match_token(ts, TOK_LPAREN))
    {
        parameters = parser_parse_parameter_list(parser, ts);
        assert(parameters.count);

        if (!parser_expect_token(parser, ts, TOK_RPAREN)) assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) return nullptr;

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

            if (decl->kind != Declaration_PTN_Kind::FUNCTION &&
                !(decl->self.flags  & PTN_FLAG_SEMICOLON))
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

    return new_struct_declaration_ptn(parser->allocator, identifier, member_decls,
                                      parameters, identifier->self.begin_file_pos,
                                      end_fp);
}

Declaration_PTN *parser_parse_union_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_UNION)) assert(false);

    Array<Parameter_PTN *> parameters = {};
    if (parser_match_token(ts, TOK_LPAREN))
    {
        parameters = parser_parse_parameter_list(parser, ts);
        assert(parameters.count);

        if (!parser_expect_token(parser, ts, TOK_RPAREN)) return nullptr;
    }

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) return nullptr;

    Array<Declaration_PTN *>member_decls = {};
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

            if (decl->kind != Declaration_PTN_Kind::FUNCTION &&
                !(decl->self.flags  & PTN_FLAG_SEMICOLON))
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

    return new_union_declaration_ptn(parser->allocator, identifier, member_decls,
                                     parameters,
                                     identifier->self.begin_file_pos, end_fp);
}

Declaration_PTN *parser_parse_enum_declaration(Parser *parser, Token_Stream *ts,
                                               Identifier_PTN *identifier,
                                               Expression_PTN *type_spec_expr)
{
    if (!parser_expect_token(parser, ts, TOK_KW_ENUM))
    {
        assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) assert(false);

    Array<PTN*> members = {};
    array_init(parser->allocator, &members, 8);

    while (!parser_is_token(ts, TOK_RBRACE))
    {
        PTN *member = nullptr;

        auto ident = parser_parse_identifier(parser, ts);
        assert(ident);
        member = (PTN*)ident;

        Declaration_PTN  *decl = nullptr;
        if (parser_is_token(ts, TOK_COLON))
        {
            decl = parser_parse_declaration(parser, ts, ident);
            assert(decl->kind == Declaration_PTN_Kind::CONSTANT);
            assert(decl->constant.type_expression == nullptr);

            member = (PTN*)decl;
        }

        assert(member);
        if (parser_match_token(ts, TOK_COMMA) ||
            parser_match_token(ts, TOK_SEMICOLON))
        {
            array_append(&members, member);
        }
        else if (parser_is_token(ts, TOK_RBRACE))
        {
            array_append(&members, member);
            break;
        }
    }

    auto end_fp = ts->current_token().end_file_pos;
    if (!parser_expect_token(parser, ts, TOK_RBRACE)) assert(false);

    return new_enum_declaration_ptn(parser->allocator, identifier, type_spec_expr,
                                    members, identifier->self.begin_file_pos, end_fp);
}

Declaration_PTN *parser_parse_import_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_IMPORT))
    {
        assert(false);
    }

    Expression_PTN *ident_expr = parser_parse_expression(parser, ts);
    assert(ident_expr);

    auto end_fp = ts->current_token().end_file_pos;

    if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
    {
        assert(false);
    }

    auto result = new_import_declaration_ptn(parser->allocator, identifier, ident_expr,
                                             identifier->self.begin_file_pos, end_fp);
    result->self.flags |= PTN_FLAG_SEMICOLON;
    return result;
}

Declaration_PTN *parser_parse_static_if_declaration(Parser *parser, Token_Stream *ts,
                                                    bool elseif/*= false*/)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_POUND)) return nullptr;

    if (elseif)
    {
        auto ft = ts->current_token();
        assert(ft.kind == TOK_IDENTIFIER);
        assert(ft.atom == Builtin::atom_elseif);
        ts->next_token();
    }
    else
    {
        if (!parser_expect_token(parser, ts, TOK_KW_IF)) return nullptr;
    }

    if (!parser_expect_token(parser, ts, TOK_LPAREN)) return nullptr;

    auto cond_expr = parser_parse_expression(parser, ts);
    if (!cond_expr) return nullptr;

    if (!parser_expect_token(parser, ts, TOK_RPAREN)) return nullptr;

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) return nullptr;

    Array<Declaration_PTN *> then_decls = {};
    array_init(parser->allocator, &then_decls);

    while (!parser_is_token(ts, TOK_RBRACE))
    {
        auto decl = parser_parse_declaration(parser, ts);
        if (!decl) return nullptr;
        array_append(&then_decls, decl);
    }

    auto end_fp = ts->current_token().end_file_pos;
    ts->next_token();

    Array<Declaration_PTN *> else_decls = {};

    if (parser_is_token(ts, TOK_POUND) &&
        ts->peek_token(1).kind == TOK_IDENTIFIER &&
        ts->peek_token(1).atom == Builtin::atom_elseif)
    {
        auto else_if_decl = parser_parse_static_if_declaration(parser, ts, true);
        if (!else_if_decl) return nullptr;

        array_init(parser->allocator, &else_decls, 1);
        array_append(&else_decls, else_if_decl);

        auto result = new_static_if_declaration_ptn(parser->allocator, cond_expr, then_decls,
                                                    else_decls, begin_fp, 
                                                    else_if_decl->self.end_file_pos); 
        result->self.flags |= PTN_FLAG_SEMICOLON;
        return result;

    }
    else if (parser_is_token(ts, TOK_POUND) && ts->peek_token(1).kind == TOK_KW_ELSE)
    {
        array_init(parser->allocator, &else_decls);

        ts->next_token();
        ts->next_token();

        if (!parser_expect_token(parser, ts, TOK_LBRACE)) return nullptr;

        while (!parser_is_token(ts, TOK_RBRACE))
        {
            auto decl = parser_parse_declaration(parser, ts);
            if (!decl) return nullptr;
            array_append(&else_decls, decl);
        }

        end_fp = ts->current_token().end_file_pos;
        ts->next_token();
    }

    auto result = new_static_if_declaration_ptn(parser->allocator, cond_expr,
                                                then_decls, else_decls,
                                                begin_fp, end_fp);
    result->self.flags |= PTN_FLAG_SEMICOLON;
    return result;

}

Identifier_PTN *parser_parse_identifier(Parser *parser, Token_Stream *ts)
{
    assert(parser);
    assert(ts);

    auto ident_token = ts->current_token();
    if (!parser_expect_token(parser, ts, TOK_IDENTIFIER)) return nullptr;

    auto begin_fp = ident_token.begin_file_pos;
    auto end_fp = ident_token.end_file_pos;
    return new_identifier_ptn(parser->allocator, ident_token.atom, begin_fp, end_fp);
}

Function_Proto_PTN *parser_parse_function_prototype(Parser *parser, Token_Stream *ts)
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

    Expression_PTN *return_type_expr = nullptr;
    if (parser_match_token(ts, TOK_RARROW))
    {
        return_type_expr = parser_parse_expression(parser, ts, true);
        end_fp = return_type_expr->self.end_file_pos;
    }

    return new_function_prototype_parse_tree_node(parser->allocator, param_list, return_type_expr,
                                                  begin_fp, end_fp);
}

Array<Parameter_PTN*> parser_parse_parameter_list(Parser *parser, Token_Stream *ts)
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

        Parameter_PTN *parameter = parser_parse_parameter(parser, ts);
        assert(parameter);
        array_append(&parameters, parameter);
    }

    if (!parameters.count)
    {
        array_free(&parameters);
    }

    return parameters;
}

Parameter_PTN *parser_parse_parameter(Parser *parser, Token_Stream *ts)
{
    assert(parser);
    assert(ts);

    Identifier_PTN *identifier = parser_parse_identifier(parser, ts);

    if (parser_match_token(ts, TOK_COLON))
    {
        Expression_PTN *type_expr = parser_parse_expression(parser, ts);

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

Statement_PTN *parser_parse_statement(Parser *parser, Token_Stream *ts)
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
                if (!statement) return nullptr;

                array_append(&block_statements, statement);

                if (!(statement->self.flags & PTN_FLAG_SEMICOLON))
                {
                    if (parser_expect_token(parser, ts, TOK_SEMICOLON))
                    {
                        statement->self.flags |= PTN_FLAG_SEMICOLON;
                    }
                    else
                    {
                        return nullptr;
                    }
                }

                end_fp = ts->current_token().end_file_pos; 
            }

            if (!block_statements.count) array_free(&block_statements);

            auto result = new_block_statement_ptn(parser->allocator, block_statements,
                                                  begin_fp, end_fp);
            result->self.flags |= PTN_FLAG_SEMICOLON;
            return result;
            break;
        }

        case TOK_IDENTIFIER:
        {
            Statement_PTN *result = nullptr;
            if (ts->peek_token(1).kind == TOK_COLON)
            {
                auto decl = parser_parse_declaration(parser, ts);
                if (!decl) return nullptr;

                result = new_declaration_statement_ptn(parser->allocator, decl, begin_fp,
                                                       decl->self.end_file_pos);
                if (decl->self.flags & PTN_FLAG_SEMICOLON)
                {
                    result->self.flags |= PTN_FLAG_SEMICOLON;
                }
            }
            else
            {
                auto expr = parser_parse_expression(parser, ts);
                assert(expr);

                if (parser_is_token(ts, TOK_EQ))
                {
                    assert(expr->kind == Expression_PTN_Kind::IDENTIFIER ||
                           expr->kind == Expression_PTN_Kind::DOT ||
                           expr->kind == Expression_PTN_Kind::SUBSCRIPT);

                    result = parser_parse_assignment_statement(parser, ts, expr);
                }
                else if ((parser_is_add_op(ts) || parser_is_mul_op(ts)) &&
                         ts->peek_token(1).kind == TOK_EQ)
                {
                    assert(expr->kind == Expression_PTN_Kind::IDENTIFIER ||
                           expr->kind == Expression_PTN_Kind::DOT ||
                           expr->kind == Expression_PTN_Kind::SUBSCRIPT);

                    result = parser_parse_self_assignment_statement(parser, ts, expr);
                }
                else
                {
                    result = new_expression_statement_ptn(parser->allocator, expr, begin_fp,
                                                          expr->self.end_file_pos);
                }
            }
            assert(result);
            return result;
            break;
        }

        case TOK_KW_RETURN:
        {
            ts->next_token();
            Expression_PTN *expr = nullptr;
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
            result->self.flags |= PTN_FLAG_SEMICOLON;
            assert(result);

            return result;
            break;
        }

        case TOK_KW_BREAK:
        {
            auto break_tok = ts->current_token();
            ts->next_token();

            auto end_fp = break_tok.end_file_pos;

            auto result = new_break_statement_ptn(parser->allocator, begin_fp, end_fp);

            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                return nullptr;
            }
            else
            {
                result->self.flags |= PTN_FLAG_SEMICOLON;
            }

            return result;
            break;
        }

        case TOK_KW_WHILE:
        {
            ts->next_token();

            if (!parser_expect_token(parser, ts, TOK_LPAREN))
            {
                assert(false);
            }

            auto while_expr = parser_parse_expression(parser, ts);
            assert(while_expr);

            if (!parser_expect_token(parser, ts, TOK_RPAREN))
            {
                assert(false);
            }

            auto while_body = parser_parse_statement(parser, ts);
            assert(while_body);

            auto end_fp = while_body->self.end_file_pos;

            auto result = new_while_statement_ptn(parser->allocator, while_expr,
                                                  while_body, begin_fp, end_fp);
            result->self.flags |= PTN_FLAG_SEMICOLON;
            return result;
            break;
        }

        case TOK_KW_FOR:
        {
            ts->next_token();

            if (!parser_expect_token(parser, ts, TOK_LPAREN))
            {
                return nullptr;
            }

            bool it_is_pointer = false;

            if (parser_match_token(ts, TOK_STAR))
            {
                it_is_pointer = true;
            }

            Identifier_PTN *it_ident = parser_parse_identifier(parser, ts);
            if (!it_ident) return nullptr;

            Identifier_PTN *it_idx_ident = nullptr;
            Expression_PTN *array_expr = nullptr;

            Statement_PTN *result = nullptr;

            if (parser_match_token(ts, TOK_COMMA))
            {
                it_idx_ident = parser_parse_identifier(parser, ts);
                assert(it_idx_ident);

                if (!parser_expect_token(parser, ts, TOK_COLON))
                {
                    return nullptr;
                }

                array_expr = parser_parse_expression(parser, ts);
                assert(array_expr);

                if (!parser_expect_token(parser, ts, TOK_RPAREN))
                {
                    return nullptr;
                }

                Statement_PTN *body_stmt = parser_parse_statement(parser, ts);
                assert(body_stmt);

                result = new_foreach_statement_ptn(parser->allocator, it_ident, it_is_pointer,
                                                   it_idx_ident, array_expr, body_stmt,
                                                   begin_fp,
                                                   body_stmt->self.end_file_pos);
            }
            else if (parser_match_token(ts, TOK_RPAREN))
            {
                assert(!it_is_pointer);

                auto array_expr =
                    new_identifier_expression_ptn(parser->allocator, it_ident,
                                                  it_ident->self.begin_file_pos,
                                                  it_ident->self.end_file_pos);

                auto body_stmt = parser_parse_statement(parser, ts);
                result = new_foreach_statement_ptn(parser->allocator, nullptr, false, nullptr,
                                                   array_expr, body_stmt, begin_fp,
                                                   body_stmt->self.end_file_pos);
            }
            else
            {
                if (!parser_expect_token(parser, ts, TOK_COLON))
                {
                    return nullptr;
                }

                Expression_PTN *type_spec_expr = nullptr;

                if (!parser_is_token(ts, TOK_EQ))
                {
                    type_spec_expr = parser_parse_expression(parser, ts);
                }

                if (parser_match_token(ts, TOK_RPAREN))
                {
                    auto body_stmt = parser_parse_statement(parser, ts);
                    result = new_foreach_statement_ptn(parser->allocator, it_ident, it_is_pointer,
                                                       nullptr, type_spec_expr,
                                                       body_stmt, begin_fp,
                                                       body_stmt->self.end_file_pos);
                }
                else
                {
                    assert(!it_is_pointer);

                    if (!parser_expect_token(parser, ts, TOK_EQ))
                    {
                        return nullptr;
                    }

                    Expression_PTN *init_expr = parser_parse_expression(parser, ts);
                    if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
                    {
                        return nullptr;
                    }

                    Declaration_PTN *init_decl =
                        new_variable_declaration_ptn(parser->allocator, it_ident,
                                                     type_spec_expr, init_expr,
                                                     it_ident->self.begin_file_pos,
                                                     init_expr->self.end_file_pos);

                    Statement_PTN *init_stmt =
                        new_declaration_statement_ptn(parser->allocator, init_decl,
                                                      init_decl->self.begin_file_pos,
                                                      init_decl->self.end_file_pos); 

                    Expression_PTN *cond_expr = parser_parse_expression(parser, ts);
                    assert(cond_expr);

                    if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
                    {
                        return nullptr;
                    }

                    Statement_PTN *step_stmt = parser_parse_statement(parser, ts);
                    assert(step_stmt);

                    if (!parser_expect_token(parser, ts, TOK_RPAREN))
                    {
                        return nullptr;
                    }

                    Statement_PTN *body_stmt = parser_parse_statement(parser, ts);
                    assert(body_stmt);

                    auto end_fp = body_stmt->self.end_file_pos;

                    result = new_for_statement_ptn(parser->allocator, init_stmt,
                                                   cond_expr, step_stmt, body_stmt,
                                                   begin_fp, end_fp);
                }
            }

            assert(result);

            result->self.flags |= PTN_FLAG_SEMICOLON;
            return result;
            break;
        }

        case TOK_KW_IF:
        {
            ts->next_token();

            if (!parser_expect_token(parser, ts, TOK_LPAREN))
            {
                assert(false);
            }

            auto cond_expr = parser_parse_expression(parser, ts);
            assert(cond_expr);

            if (!parser_expect_token(parser, ts, TOK_RPAREN))
            {
                assert(false);
            }

            auto then_stmt = parser_parse_statement(parser, ts);
            if (!then_stmt) return nullptr;

            Statement_PTN *else_stmt = nullptr;

            auto end_fp = then_stmt->self.end_file_pos;

            if (parser_match_token(ts, TOK_KW_ELSE))
            {
                else_stmt = parser_parse_statement(parser, ts);
                end_fp = then_stmt->self.end_file_pos;
            }

            auto result = new_if_statement_ptn(parser->allocator, cond_expr, then_stmt,
                                               else_stmt, begin_fp, end_fp);
            result->self.flags |= PTN_FLAG_SEMICOLON;
            return result;
            break;
        }

        case TOK_KW_SWITCH:
        {
            return parser_parse_switch_statement(parser, ts);
            break;
        }

        case TOK_AT:
        {
            auto bfp = ts->current_token().begin_file_pos;

            auto id_tok = ts->peek_token(1);
            assert(id_tok.kind == TOK_IDENTIFIER);

            if (id_tok.atom == Builtin::atom_static_assert)
            {
                ts->next_token();
                ts->next_token();

                if (!parser_expect_token(parser, ts, TOK_LPAREN)) return nullptr;

                auto cond_expr = parser_parse_expression(parser, ts);
                if (!cond_expr) return nullptr;

                auto efp = ts->current_token().begin_file_pos;
                if (!parser_expect_token(parser, ts, TOK_RPAREN)) return nullptr;

                auto sa_decl = new_static_assert_declaration_ptn(parser->allocator, cond_expr,
                                                                 bfp, efp);
                return new_declaration_statement_ptn(parser->allocator, sa_decl, bfp, efp);
            }
            else
            {
                // Builtin call
                auto expr = parser_parse_expression(parser, ts);
                return new_expression_statement_ptn(parser->allocator, expr,
                                                    bfp, expr->self.end_file_pos);
            }
            break;
        }

        default:
        {
            Statement_PTN *result = nullptr;

            auto expr = parser_parse_expression(parser, ts);
            assert(expr);
            if (parser_is_token(ts, TOK_EQ))
            {
                result = parser_parse_assignment_statement(parser, ts, expr);
            }
            else if ((parser_is_add_op(ts) || parser_is_mul_op(ts)) &&
                      ts->peek_token(1).kind == TOK_EQ)
            {
                result = parser_parse_self_assignment_statement(parser, ts, expr);
            }
            else
            {
                result =  new_expression_statement_ptn(parser->allocator, expr,
                                                       begin_fp,
                                                       expr->self.end_file_pos);
            }

            if (parser_match_token(ts, TOK_SEMICOLON))
            {
                result->self.flags |= PTN_FLAG_SEMICOLON;
            }

            assert(result);
            return result;
        }
    }

    assert(false);
    return nullptr;
}

Statement_PTN *parser_parse_assignment_statement(Parser *parser, Token_Stream *ts,
                                                 Expression_PTN *ident_expression)
{
    assert(ident_expression->kind == Expression_PTN_Kind::IDENTIFIER ||
           ident_expression->kind == Expression_PTN_Kind::DOT ||
           ident_expression->kind == Expression_PTN_Kind::SUBSCRIPT ||
           (ident_expression->kind == Expression_PTN_Kind::UNARY &&
            ident_expression->unary.op == UNOP_DEREF));

    if (!parser_expect_token(parser, ts, TOK_EQ))
    {
        assert(false);
    }

    Expression_PTN *rhs_value = parser_parse_expression(parser, ts);
    assert(rhs_value);

    return new_assignment_statement_ptn(parser->allocator, ident_expression, rhs_value,
                                        ident_expression->self.begin_file_pos,
                                        rhs_value->self.end_file_pos);
}

Statement_PTN *parser_parse_self_assignment_statement(Parser *parser, Token_Stream *ts,
                                                      Expression_PTN *ident_expression)
{
    assert(ident_expression->kind == Expression_PTN_Kind::IDENTIFIER ||
           ident_expression->kind == Expression_PTN_Kind::DOT ||
           ident_expression->kind == Expression_PTN_Kind::SUBSCRIPT ||
           (ident_expression->kind == Expression_PTN_Kind::UNARY &&
            ident_expression->unary.op == UNOP_DEREF));

    Binary_Operator op = BINOP_INVALID;

    if (parser_is_add_op(ts))
    {
        op = parser_parse_add_op(ts);
    }
    else if (parser_is_mul_op(ts))
    {
        op = parser_parse_mul_op(ts);
    }
    else assert(false);

    assert(op == BINOP_ADD ||
           op == BINOP_SUB ||
           op == BINOP_MUL ||
           op == BINOP_DIV ||
           op == BINOP_REMAINDER);

    if (!parser_expect_token(parser, ts, TOK_EQ))
    {
        assert(false);
    }


    Expression_PTN *rhs_expr = parser_parse_expression(parser, ts);
    assert(rhs_expr);

    auto begin_fp = ident_expression->self.begin_file_pos;
    auto end_fp = rhs_expr->self.end_file_pos;

    rhs_expr = new_binary_expression_ptn(parser->allocator, op, ident_expression, rhs_expr,
                                         begin_fp, end_fp);

    return new_assignment_statement_ptn(parser->allocator, ident_expression, rhs_expr,
                                        ident_expression->self.begin_file_pos,
                                        rhs_expr->self.end_file_pos);
}

Statement_PTN *parser_parse_switch_statement(Parser *parser, Token_Stream *ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_KW_SWITCH))
    {
        return nullptr;
    }

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        return nullptr;
    }

    Expression_PTN *expr = parser_parse_expression(parser, ts);
    assert(expr);

    if (!parser_expect_token(parser, ts, TOK_RPAREN))
    {
        return nullptr;
    }

    bool allow_incomplete = false;

    if (parser_match_token(ts, TOK_POUND))
    {
        auto identifier = parser_parse_identifier(parser, ts);
        assert(identifier);

        if (identifier->atom == Builtin::atom_allow_incomplete)
        {
            allow_incomplete = true;
        }
        else
        {
            assert(false);
        }
    }

    if (!parser_expect_token(parser, ts, TOK_LBRACE))
    {
        return  nullptr;
    }

    Array<Switch_Case_PTN> cases = {};
    array_init(parser->allocator, &cases);

    bool has_default_case = false;

    while (parser_is_token(ts, TOK_KW_CASE) ||
           parser_is_token(ts, TOK_KW_DEFAULT))
    {
        auto case_ptn = parser_parse_switch_case(parser, ts);

        if (case_ptn.parse_error)
        {
            return nullptr;
        }

        if (case_ptn.is_default)
        {
            assert(!has_default_case);
            has_default_case = true;
        }
        array_append(&cases, case_ptn);
    }

    auto end_fp = ts->current_token().end_file_pos;

    if (!parser_expect_token(parser, ts, TOK_RBRACE))
    {
        return  nullptr;
    }

    if (!cases.count) array_free(&cases);

    auto result = new_switch_statement_ptn(parser->allocator, expr, cases,
                                           has_default_case, allow_incomplete,
                                           begin_fp, end_fp);
    result->self.flags |= PTN_FLAG_SEMICOLON;
    return result;
}

Switch_Case_PTN parser_parse_switch_case(Parser *parser, Token_Stream *ts)
{
    Switch_Case_PTN result = {};

    result.begin_fp = ts->current_token().begin_file_pos;

    if (parser_match_token(ts, TOK_KW_CASE))
    {
        result.expressions = parser_parse_case_expressions(parser, ts);
        assert(result.expressions.count);

    }
    else if (parser_match_token(ts, TOK_KW_DEFAULT))
    {
        result.is_default = true;
    }

    if (!parser_expect_token(parser, ts, TOK_COLON))
    {
        result.parse_error = true;
        return result;
    }

    Array<Statement_PTN*> body_stmts = {};
    array_init(parser->allocator, &body_stmts);

    while (!parser_is_token(ts, TOK_KW_CASE) &&
           !parser_is_token(ts, TOK_KW_DEFAULT) &&
           !parser_is_token(ts, TOK_RBRACE))
    {
        Statement_PTN *stmt = parser_parse_statement(parser, ts);
        assert(stmt);
        if (!(stmt->self.flags & PTN_FLAG_SEMICOLON))
        {
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                result.parse_error = true;
                return result;
            }
        }
        array_append(&body_stmts, stmt);
    }

    assert(body_stmts.count);

    Statement_PTN *body_stmt = nullptr;

    if (body_stmts.count == 1 && body_stmts[0]->kind == Statement_PTN_Kind::BLOCK)
    {
        body_stmt = body_stmts[0];
        array_free(&body_stmts);
    }
    else
    {
        auto begin_fp = body_stmts[0]->self.begin_file_pos;
        auto end_fp = body_stmts[body_stmts.count - 1]->self.end_file_pos;
        body_stmt = new_block_statement_ptn(parser->allocator, body_stmts, begin_fp,
                                            end_fp);
    }

    assert(body_stmt);
    result.body = body_stmt;

    result.end_fp = body_stmt->self.end_file_pos;

    return result;
}

Array<Switch_Case_Expression_PTN> parser_parse_case_expressions(Parser *parser, Token_Stream *ts)
{
    Array<Switch_Case_Expression_PTN> result = {};
    array_init(parser->allocator, &result, 1);

    while (!parser_is_token(ts, TOK_COLON))
    {
        Expression_PTN *expr = parser_parse_expression(parser, ts); 

        Expression_PTN *range_end_expr = nullptr;
        if (parser_match_token(ts, TOK_DOT_DOT))
        {
            range_end_expr = parser_parse_expression(parser, ts);
        }

        Switch_Case_Expression_PTN sce = { .expression = expr,
                                           .range_end_expr = range_end_expr};
        array_append(&result, sce);

        if (!parser_match_token(ts, TOK_COMMA))
        {
            break;
        }
    }

    return result;
}

Expression_PTN *parser_parse_expression(Parser *parser, Token_Stream *ts, bool is_type/*=false*/)
{
    return parser_parse_cmp_expression(parser, ts, is_type);
}

Expression_PTN *parser_parse_cmp_expression(Parser *parser, Token_Stream *ts, bool is_type/*=false*/)
{
    auto lhs = parser_parse_add_expression(parser, ts, is_type);
    if (!lhs) return nullptr;

    while (parser_is_cmp_op(ts))
    {
        auto op = parser_parse_cmp_op(ts);
        auto rhs = parser_parse_add_expression(parser, ts, is_type);
        assert(rhs);
        
        auto begin_fp = lhs->self.begin_file_pos;
        auto end_fp = rhs->self.end_file_pos;
        lhs = new_binary_expression_ptn(parser->allocator, op, lhs, rhs, begin_fp, end_fp);
    }

    return lhs;

}

Expression_PTN *parser_parse_add_expression(Parser *parser, Token_Stream *ts, bool is_type/*=false*/)
{
    auto lhs = parser_parse_mul_expression(parser, ts, is_type);
    if (!lhs) return nullptr;

    while (parser_is_add_op(ts) && !(ts->peek_token(1).kind == TOK_EQ))
    {
        auto op = parser_parse_add_op(ts);
        auto rhs = parser_parse_mul_expression(parser, ts, is_type);
        assert(rhs);
        
        auto begin_fp = lhs->self.begin_file_pos;
        auto end_fp = rhs->self.end_file_pos;
        lhs = new_binary_expression_ptn(parser->allocator, op, lhs, rhs, begin_fp, end_fp);
    }

    return lhs;
}

Expression_PTN *parser_parse_mul_expression(Parser *parser, Token_Stream *ts, bool is_type/*=false*/)
{
    auto lhs = parser_parse_unary_expression(parser, ts, is_type);
    if (!lhs) return nullptr;

    while (parser_is_mul_op(ts) && !(ts->peek_token(1).kind == TOK_EQ))
    {
        auto op = parser_parse_mul_op(ts);
        auto rhs = parser_parse_unary_expression(parser, ts, is_type);
        assert(rhs);

        auto begin_fp = lhs->self.begin_file_pos;
        auto end_fp = rhs->self.end_file_pos;
        lhs = new_binary_expression_ptn(parser->allocator, op, lhs, rhs, begin_fp, end_fp);
    }

    return lhs;
}

Expression_PTN *parser_parse_unary_expression(Parser *parser, Token_Stream *ts,
                                              bool is_type/*=false*/)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    auto op = parser_parse_unary_op(ts);
    if (op != UNOP_INVALID)
    {
        auto operand_expr = parser_parse_unary_expression(parser, ts, is_type);
        auto end_fp = operand_expr->self.end_file_pos;

        return new_unary_expression_ptn(parser->allocator, op, operand_expr,
                                        begin_fp, end_fp);
    }
    else
    {
        return parser_parse_base_expression(parser, ts, is_type);
    }
}

Expression_PTN *parser_parse_base_expression(Parser *parser, Token_Stream *ts,
                                             bool is_type/*=false*/)
{
    auto ct = ts->current_token();
    auto begin_fp = ct.begin_file_pos;
    Expression_PTN *result = nullptr;

    switch (ct.kind)
    {
        case TOK_IDENTIFIER:
        {
            if (ts->peek_token(1).kind == TOK_LPAREN)
            {
                auto call_expr = parser_parse_call_expression(parser, ts);
                assert(call_expr);
                result = call_expr;

                // ::TypedStructLiterals
                // I think this was meant to parse this:
                // p2 := Vec3(float) { 4, 5, 6 };
                // This should probably be written as this anyways:
                // p2 := cast(Vec3(float), {4, 5, 6 });
                // or whatever the cast syntax will be
                
                //if (!is_type && parser_match_token(ts, TOK_LBRACE))
                //{
                    //auto expr_list = parser_parse_expression_list(parser, ts);
                    //assert(expr_list);
                    //auto end_fp = ts->current_token().end_file_pos;
                    //if (!parser_expect_token(parser, ts, TOK_RBRACE)) assert(false);

                    //result = new_compound_expression_ptn(parser->allocator, expr_list, call_expr,
                                                         //begin_fp, end_fp);
                //}
                //else
                //{
                    //result = call_expr;
                //}
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

        case TOK_CHAR_LITERAL:
        {
            result = parser_parse_character_literal_expression(parser, ts);
            break;
        }

        case TOK_KW_NULL:
        {
            auto null_tok = ts->current_token();
            ts->next_token();

            auto begin_fp = null_tok.begin_file_pos;
            auto end_fp = null_tok.end_file_pos;
            
            result = new_null_literal_expression_ptn(parser->allocator, begin_fp, end_fp);
            break;
        };

        case TOK_KW_TRUE:
        {
            result = parser_parse_boolean_literal_expression(parser, ts);
            break;
        };

        case TOK_KW_FALSE:
        {
            result = parser_parse_boolean_literal_expression(parser, ts);
            break;
        };

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

        case TOK_MINUS:
        {
            assert(false);
            break;
        }

        case TOK_LPAREN:
        {
            ts->next_token();
            result = parser_parse_expression(parser, ts);
            if (!parser_expect_token(parser, ts, TOK_RPAREN))
            {
                return nullptr;
            }
            break;
        }

        default:
        {
            auto ct = ts->current_token();
            parser_report_unexpected_token(parser, ts, ct);
            return nullptr;
            break;
        }
    }

    assert(result);

    while (true)
    {
        if (parser_match_token(ts, TOK_DOT))
        {
            auto parent = result;
            Identifier_PTN *child_ident = parser_parse_identifier(parser, ts);
            auto end_fp = child_ident->self.end_file_pos;
            result = new_dot_expression_ptn(parser->allocator, parent, child_ident, begin_fp,
                                            end_fp);
        }
        else if (parser_is_token(ts, TOK_LPAREN))
        {
            result = parser_parse_call_expression(parser, ts, result, false);
        }
        else if (parser_match_token(ts, TOK_LBRACK))
        {
            Expression_PTN *index_expr = parser_parse_expression(parser, ts);
            auto end_fp = ts->current_token().end_file_pos;
            if (parser_expect_token(parser, ts, TOK_RBRACK))
            {
                result = new_subscript_expression_ptn(parser->allocator, result, index_expr,
                                                      begin_fp, end_fp);
            }
            else assert(false);
        }
        else
        {
            break;
        }
    }

    return result;
}

Expression_PTN *parser_parse_call_expression(Parser *parser, Token_Stream *ts,
                                             Expression_PTN *ident_expr, bool is_builtin/*=false*/)
{
    auto begin_fp = ident_expr->self.begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        assert(false);
    }

    Expression_List_PTN *arg_list = nullptr;
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

Expression_PTN *parser_parse_call_expression(Parser *parser, Token_Stream *ts,
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

Expression_PTN *parser_parse_number_literal_expression(Parser *parser, Token_Stream *ts)
{
    auto num_tok = ts->current_token();
    auto begin_fp = num_tok.begin_file_pos;
    auto end_fp = num_tok.end_file_pos;
    ts->next_token();

    return new_number_literal_expression_ptn(parser->allocator, num_tok.atom, begin_fp, end_fp);
}

Expression_PTN *parser_parse_string_literal_expression(Parser *parser, Token_Stream *ts)
{
    auto string_tok = ts->current_token();
    auto begin_fp = string_tok.begin_file_pos;
    auto end_fp = string_tok.end_file_pos;
    ts->next_token();

    return new_string_literal_expression_ptn(parser->allocator, string_tok.atom,
                                             begin_fp, end_fp);
}

Expression_PTN *parser_parse_character_literal_expression(Parser *parser,
                                                          Token_Stream *ts)
{
    auto char_tok = ts->current_token();
    auto begin_fp = char_tok.begin_file_pos;
    auto end_fp = char_tok.end_file_pos;
    ts->next_token();

    return new_char_literal_expression_ptn(parser->allocator, char_tok.c,
                                           begin_fp, end_fp);
}

Expression_PTN *parser_parse_boolean_literal_expression(Parser *parser, Token_Stream *ts)
{
    auto bool_tok = ts->current_token();
    auto begin_fp = bool_tok.begin_file_pos;
    auto end_fp = bool_tok.end_file_pos;
    ts->next_token();

    bool bool_val = bool_tok.kind == TOK_KW_TRUE ? true : false;
    if (!bool_val) { assert(bool_tok.kind == TOK_KW_FALSE); }

    return new_boolean_literal_expression_ptn(parser->allocator, bool_val, begin_fp, end_fp);
}

Expression_PTN *parser_parse_array_type_expression(Parser *parser, Token_Stream *ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_LBRACK))
    {
        assert(false);
    }

    Expression_PTN *length_expr = nullptr;
    if (!parser_is_token(ts, TOK_RBRACK))
    {
        length_expr = parser_parse_expression(parser, ts);
    }

    if (!parser_expect_token(parser, ts, TOK_RBRACK))
    {
        assert(false);
    }

    Expression_PTN *element_type = parser_parse_expression(parser, ts);
    assert(element_type);

    auto end_fp = element_type->self.end_file_pos;

    return new_array_type_expression_ptn(parser->allocator, length_expr, element_type, begin_fp,
                                         end_fp);
}

Expression_PTN *parser_parse_pointer_type_expression(Parser *parser, Token_Stream *ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_STAR))
    {
        assert(false);
    }

    Expression_PTN *pointee_type_expression = parser_parse_expression(parser, ts);
    assert(pointee_type_expression);

    auto end_fp = pointee_type_expression->self.end_file_pos;

    return new_pointer_type_expression_ptn(parser->allocator, pointee_type_expression, begin_fp,
                                           end_fp);
}

Expression_PTN *parser_parse_poly_type_expression(Parser *parser, Token_Stream *ts)
{
    auto begin_fp = ts->current_token().begin_file_pos;

    if (!parser_expect_token(parser, ts, TOK_DOLLAR)) assert(false);

    auto identifier = parser_parse_identifier(parser, ts);
    assert(identifier);

    auto end_fp = identifier->self.end_file_pos;

    Identifier_PTN *spec_ident = nullptr;
    if (parser_match_token(ts, TOK_FORWARD_SLASH))
    {
        spec_ident = parser_parse_identifier(parser, ts);
        end_fp = spec_ident->self.end_file_pos;
    }

    return new_poly_type_expression_ptn(parser->allocator, identifier, spec_ident, begin_fp,
                                        end_fp);
}

Expression_List_PTN *parser_parse_expression_list(Parser *parser, Token_Stream *ts)
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

Binary_Operator parser_parse_cmp_op(Token_Stream *ts)
{
    auto ct = ts->current_token();
    Binary_Operator result = BINOP_INVALID;

    switch (ct.kind)
    {
        case TOK_EQ_EQ:
        {
            result = BINOP_EQ;
            break;
        }

        case TOK_NEQ:
        {
            result = BINOP_NEQ;
            break;
        }

        case TOK_LT:
        {
            result = BINOP_LT;
            break;
        }

        case TOK_LTEQ:
        {
            result = BINOP_LTEQ;
            break;
        }

        case TOK_GT:
        {
            result = BINOP_GT;
            break;
        }

        case TOK_GTEQ:
        {
            result = BINOP_GT;
            break;
        }

        default: assert(false);
    }

    ts->next_token();
    return result;
}

Binary_Operator parser_parse_add_op(Token_Stream *ts)
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

Binary_Operator parser_parse_mul_op(Token_Stream *ts)
{
    auto ct = ts->current_token();
    Binary_Operator result = BINOP_INVALID;
    switch (ct.kind)
    {
        case TOK_PERCENT:
        {
            result = BINOP_REMAINDER;
            break;
        }

        case TOK_STAR:
        {
            result = BINOP_MUL;
            break;
        }

        case TOK_FORWARD_SLASH:
        {
            result = BINOP_DIV;
            break;
        }

        default: assert(false);
    }

    ts->next_token();
    return result;
}

bool parser_expect_token(Parser *parser, Token_Stream *ts, Token_Kind kind)
{
    assert(parser);

    if (!parser_match_token(ts, kind))
    {
        auto ct = ts->current_token();
        zodiac_report_error(parser->build_data, Zodiac_Error_Kind::UNEXPECTED_TOKEN,
                            ct.begin_file_pos, ct.end_file_pos,
                            "Expected token: \"%s\", got \"%s\"",
                            token_kind_name(kind), token_kind_name(ct.kind));
        return false;
    }

    return true;
}

bool parser_match_token(Token_Stream *ts, Token_Kind kind)
{
    if (parser_is_token(ts, kind))
    {
        ts->next_token();
        return true;
    }

    return false;
}

bool parser_is_token(Token_Stream *ts, Token_Kind kind)
{
    auto ct = ts->current_token();

    if (ct.kind != kind)
    {
        return false;
    }

    return true;
}

bool parser_is_cmp_op(Token_Stream *ts)
{
    auto ct = ts->current_token();
    return ct.kind == TOK_EQ_EQ ||
           ct.kind == TOK_NEQ   ||
           ct.kind == TOK_LT    ||
           ct.kind == TOK_LTEQ  ||
           ct.kind == TOK_GT    ||
           ct.kind == TOK_GTEQ;

}

bool parser_is_add_op(const Token &token)
{
    return token.kind == TOK_PLUS || token.kind == TOK_MINUS;
}

bool parser_is_add_op(Token_Stream *ts)
{
    auto ct = ts->current_token();
    return parser_is_add_op(ct);
}

bool parser_is_mul_op(Token_Stream *ts)
{
    auto ct = ts->current_token();
    return ct.kind == TOK_PERCENT ||
           ct.kind == TOK_STAR    ||
           ct.kind == TOK_FORWARD_SLASH;
}

Unary_Operator parser_parse_unary_op(Token_Stream *ts)
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

        case TOK_MINUS:
        {
            result = UNOP_MINUS;
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


void parser_report_unexpected_token(Parser *parser, Token_Stream *ts, const Token &tok)
{
    auto tok_kind_name = token_kind_name(tok.kind);

    if (tok.kind == TOK_CHAR_LITERAL)
    {
        int c_prefix_len = 0;
        auto c_prefix = "\\";
        char c = tok.c;
        if (parser_make_escape_char(tok.c, &c))
        {
            c_prefix_len = 1;
        }

        zodiac_report_error(parser->build_data, Zodiac_Error_Kind::UNEXPECTED_TOKEN,
                            tok.begin_file_pos, tok.end_file_pos,
                            "Unexpected token when parsing expression: '%s', (%d), '%.*s%c'",
                            tok_kind_name, (int)tok.c, (int)c_prefix_len, c_prefix, c);

    }
    else
    {
        zodiac_report_error(parser->build_data, Zodiac_Error_Kind::UNEXPECTED_TOKEN,
                            tok.begin_file_pos, tok.end_file_pos,
                            "Unexpected token when parsing expression: '%s', '%.*s'",
                            tok_kind_name, tok.atom.length, tok.atom.data);
    }
}

bool parser_make_escape_char(char c, char *dest)
{
    switch (c) {

        case '\n': {
            *dest = 'n';
            return true;
        }

        case '\0': {
            *dest = '0';
            return true;
        }
    }

    *dest = c;

    return false;
}

void parsed_file_print(Parsed_File *parsed_file)
{
    for (int64_t i = 0; i < parsed_file->declarations.count; i++)
    {
        print_declaration_ptn(parsed_file->declarations[i], 0);
    }
}

} 
