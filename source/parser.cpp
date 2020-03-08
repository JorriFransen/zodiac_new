#include "parser.h"

#include "allocator.h"
#include "builtin.h"

#include <stdio.h>
#include <cassert>

namespace Zodiac
#include <stdio.h>
{

Parser parser_create(Allocator* allocator)
{
    Parser result = {};
    parser_init(allocator, &result);
    return result;
}

void parser_init(Allocator* allocator, Parser* parser)
{
    parser->allocator = allocator;
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

    while (ts->current_token().kind == TOK_POUND)
    {
        auto directive_tok = ts->next_token();
        if (directive_tok.atom == Builtin::atom_naked)
        {
            is_naked = true;
        }
        else assert(false);
        ts->next_token();
    }

    auto identifier = parser_parse_identifier(parser, ts);
    if (!identifier) return nullptr;

    return parser_parse_declaration(parser, ts, identifier);
}

Declaration_PTN* parser_parse_declaration(Parser* parser, Token_Stream* ts,
                                                      Identifier_PTN* identifier)
{
    if (!parser_expect_token(parser, ts, TOK_COLON)) return nullptr;

    Expression_Parse_Tree_Node* specified_type = nullptr;

    if (!parser_is_token(ts, TOK_COLON) && !parser_is_token(ts, TOK_EQ))
    {
        specified_type = parser_parse_expression(parser, ts);
        if (parser_is_token(ts, TOK_SEMICOLON))
        {
            return new_variable_declaration_ptn(parser->allocator, identifier, nullptr);
        }
    }

    if (parser_match_token(ts, TOK_COLON))
    {
        if (parser_is_token(ts, TOK_KW_FUNC))
        {
            assert(!specified_type);
            auto function_proto = parser_parse_function_prototype(parser, ts);
            assert(function_proto);

            Statement_PTN* function_body = nullptr;
            if (parser_is_token(ts, TOK_LBRACE))
            {
                function_body = _parser_parse_statement(parser, ts);
                assert(function_body);
                assert(function_body->kind == Statement_PTN_Kind::BLOCK);
            }

            return new_function_declaration_ptn(parser->allocator, identifier, function_proto,
                                                function_body);
        }
        else if (parser_is_token(ts, TOK_KW_STRUCT))
        {
            assert(!specified_type);
            return parser_parse_struct_declaration(parser, ts, identifier);
        }
        else
        {
            assert(false);
        }
    }
    else if (parser_match_token(ts, TOK_EQ))
    {
        auto expression = parser_parse_expression(parser, ts);

        return new_variable_declaration_ptn(parser->allocator, identifier, expression);
    }
    else
    {
        assert(false);
    }

    assert(false);
}

Declaration_PTN*
parser_parse_struct_declaration(Parser* parser, Token_Stream* ts,
                                Identifier_PTN* identifier)
{
    if (!parser_expect_token(parser, ts, TOK_KW_STRUCT)) assert(false);

    if (!parser_expect_token(parser, ts, TOK_LBRACE)) assert(false);

    Array<Declaration_PTN*> member_decls = {};
    array_init(parser->allocator, &member_decls);

    while (!parser_match_token(ts, TOK_RBRACE))
    {
        auto decl = parser_parse_declaration(parser, ts);
        array_append(&member_decls, decl);
        if (!parser_expect_token(parser, ts, TOK_SEMICOLON)) assert(false);
    }

    if (!member_decls.count)
    {
        array_free(&member_decls);
    }

    return new_struct_declaration_ptn(parser->allocator, identifier, member_decls);
}

Identifier_PTN* parser_parse_identifier(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    auto ident_token = ts->current_token();
    if (!parser_expect_token(parser, ts, TOK_IDENTIFIER)) return nullptr;

    return new_identifier_ptn(parser->allocator, ident_token.atom);
}

Function_Proto_PTN* parser_parse_function_prototype(Parser* parser, Token_Stream* ts)
{
    if (!parser_expect_token(parser, ts, TOK_KW_FUNC))
    {
        assert(false);
    }

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        assert(false);
    }

    auto param_list = parser_parse_parameter_list(parser, ts);

    if (!parser_expect_token(parser, ts, TOK_RPAREN))
    {
        assert(false);
    }

    Expression_Parse_Tree_Node* return_type_expr = nullptr;
    if (parser_match_token(ts, TOK_RARROW))
    {
        return_type_expr = parser_parse_expression(parser, ts);
    }

    return new_function_prototype_parse_tree_node(parser->allocator, param_list, return_type_expr);
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
    assert(false);
}

Statement_Parse_Tree_Node* parser_parse_statement(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    // assert(ts);
    // assert(false);

    if (parser_is_token(ts, TOK_IDENTIFIER))
    {
        if (ts->peek_token(1).kind == TOK_COLON)
        {
            auto decl = parser_parse_declaration(parser, ts);
            assert(decl);
            return new_declaration_statement_parse_tree_node(parser->allocator, decl);
        }
        else
        {
            auto expr = parser_parse_expression(parser, ts);
            assert(expr);
            return new_expression_statement_parse_tree_node(parser->allocator, expr);
        }
    }
    else if (parser_match_token(ts, TOK_KW_RETURN))
    {
        Expression_Parse_Tree_Node* return_expr = nullptr;
        if (!parser_is_token(ts, TOK_SEMICOLON))
        {
            return_expr = parser_parse_expression(parser, ts);
            assert(return_expr);
        }

        auto result = new_return_statement_parse_tree_node(parser->allocator, return_expr);
        if (!parser_is_token(ts, TOK_SEMICOLON))
        {
            assert(false);
        }
        return result;
    }
    else
    {
        auto expr = parser_parse_expression(parser, ts);
        return new_expression_statement_parse_tree_node(parser->allocator, expr);
    }
}

Statement_PTN* _parser_parse_statement(Parser* parser, Token_Stream* ts)
{
    auto ct = ts->current_token();

    switch (ct.kind)
    {
        case TOK_LBRACE:
        {
            ts->next_token();
            Array<Statement_PTN*> block_statements = {};
            array_init(parser->allocator, &block_statements);

            while (!parser_match_token(ts, TOK_RBRACE))
            {
                // if (block_statements.count)
                // {
                //     // if (!parser_expect_token(parser, ts, TOK_COMMA)) assert(false);
                //     parser_match_token(ts, TOK_SEMICOLON);
                // }

                auto statement = _parser_parse_statement(parser, ts);
                array_append(&block_statements, statement);
            }

            if (!block_statements.count) array_free(&block_statements);

            return new_block_statement_ptn(parser->allocator, block_statements);
            break;
        }

        case TOK_IDENTIFIER:
        {
            Statement_PTN* result = nullptr;
            if (ts->peek_token(1).kind == TOK_COLON)
            {
                auto decl = parser_parse_declaration(parser, ts);
                assert(decl);
                result = new_declaration_statement_ptn(parser->allocator, decl);
            }
            else
            {
                auto expr = parser_parse_expression(parser, ts);
                assert(expr);
                result = new_expression_statement_ptn(parser->allocator, expr);
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
            Expression_Parse_Tree_Node* expr = nullptr;
            bool found_semicolon = false;
            if (!parser_match_token(ts, TOK_SEMICOLON))
            {
                expr = parser_parse_expression(parser, ts);
                assert(expr);
            }
            else
            {
                found_semicolon = true;
            }

            auto result = new_return_statement_ptn(parser->allocator, expr);
            assert(result);

            if (!found_semicolon)
            {
                if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
                {
                    assert(false);
                }
            }
            return result;
            break;
        }

        default:
        {
            auto expr = parser_parse_expression(parser, ts);
            assert(expr);
            if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
            {
                assert(false);
            }
            return new_expression_statement_ptn(parser->allocator, expr);
        }
    }
}

Expression_Parse_Tree_Node* parser_parse_expression(Parser* parser, Token_Stream* ts)
{
    return parser_parse_add_expression(parser, ts);
}

Expression_Parse_Tree_Node* parser_parse_add_expression(Parser* parser, Token_Stream* ts)
{
    auto lhs = parser_parse_base_expression(parser, ts);
    assert(lhs);

    while (parser_is_add_op(ts))
    {
        auto op = parser_parse_add_op(ts);
        auto rhs = parser_parse_base_expression(parser, ts);
        assert(rhs);
        lhs = new_binary_expression_parse_tree_node(parser->allocator, op, lhs, rhs);
    }

    return lhs;
}

Expression_Parse_Tree_Node* parser_parse_base_expression(Parser* parser, Token_Stream* ts)
{
    auto ct = ts->current_token();

    switch (ct.kind)
    {
        case TOK_IDENTIFIER:
        {
            if (ts->peek_token(1).kind == TOK_LPAREN)
            {
                return parser_parse_call_expression(parser, ts);
            }
            else
            {
                auto identifier = parser_parse_identifier(parser, ts);
                return new_identifier_expression_parse_tree_node(parser->allocator, identifier);
            }
            break;
        }

        case TOK_AT:
        {
            ts->next_token();
            return parser_parse_call_expression(parser, ts, true);
            break;
        }

        case TOK_NUMBER_LITERAL:
        {
            return parser_parse_number_literal_expression(parser, ts);
            break;
        }

        default: assert(false);
    }
}

Call_Expression_Parse_Tree_Node* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                                              bool is_builtin/*=false*/)
{
    auto identifier = parser_parse_identifier(parser, ts);
    assert(identifier);

    if (!parser_expect_token(parser, ts, TOK_LPAREN))
    {
        assert(false);
    }

    Expression_List_Parse_Tree_Node* arg_list = nullptr;
    if (!parser_is_token(ts, TOK_RPAREN))
    {
        arg_list = parser_parse_expression_list(parser, ts);
    }

    if (!parser_expect_token(parser, ts, TOK_RPAREN))
    {
        assert(false);
    }

    return new_call_expression_parse_tree_node(parser->allocator, is_builtin, identifier,
                                                arg_list);
}

Number_Literal_Expression_Parse_Tree_Node*
parser_parse_number_literal_expression(Parser* parser, Token_Stream* ts)
{
    auto num_tok = ts->current_token();
    ts->next_token();

    return new_number_literal_expression_parse_tree_node(parser->allocator, num_tok.atom);
}

Expression_List_Parse_Tree_Node* parser_parse_expression_list(Parser* parser, Token_Stream* ts)
{
    Array<Expression_Parse_Tree_Node*> expressions = {};
    array_init(parser->allocator, &expressions, 4);

    auto first = parser_parse_expression(parser, ts);
    assert(first);
    array_append(&expressions, first);

    while (parser_match_token(ts, TOK_COMMA))
    {
        auto expr = parser_parse_expression(parser, ts);
        assert(expr);
        array_append(&expressions, expr);
    }

    if (!expressions.count)
    {
        array_free(&expressions);
    }

    return new_expression_list_parse_tree_node(parser->allocator, expressions);
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

void parsed_file_print(Parsed_File* parsed_file)
{
    for (int64_t i = 0; i < parsed_file->declarations.count; i++)
    {
        print_declaration_ptn(parsed_file->declarations[i], 0);
    }
}

}
