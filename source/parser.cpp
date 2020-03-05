#include "parser.h"

#include "allocator.h"
#include "builtin.h"

#include <stdio.h>
#include <cassert>

namespace Zodiac
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

Declaration_Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts)
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

Declaration_Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts,
                                                      Identifier_Parse_Tree_Node* identifier)
{
    if (!parser_expect_token(parser, ts, TOK_COLON)) return nullptr;

    if (!parser_is_token(ts, TOK_COLON) && !parser_is_token(ts, TOK_EQ))
    {
        assert(false);
    }

    if (parser_match_token(ts, TOK_COLON))
    {
        if (parser_is_token(ts, TOK_KW_FUNC))
        {
            auto function_proto = parser_parse_function_prototype(parser, ts);
            assert(function_proto);

            Function_Body_Parse_Tree_Node* function_defn = nullptr;
            if (parser_is_token(ts, TOK_LBRACE))
            {
                function_defn = parser_parse_function_body(parser, ts);
                assert(function_defn);
            }

            return new_function_declaration_parse_tree_node(parser->allocator, identifier,
                                                            function_proto, function_defn);
        }
    }
    else if (parser_match_token(ts, TOK_EQ))
    {
        auto expression = parser_parse_expression(parser, ts);

        return new_mutable_declaration_parse_tree_node(parser->allocator, identifier,
                                                       expression);
    }
    else
    {
        assert(false);
    }

    assert(false);
}

Identifier_Parse_Tree_Node* parser_parse_identifier(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    auto ident_token = ts->current_token();
    if (!parser_expect_token(parser, ts, TOK_IDENTIFIER)) return nullptr;

    return new_identifier_parse_tree_node(parser->allocator, ident_token.atom);
}

Function_Prototype_Parse_Tree_Node* parser_parse_function_prototype(Parser* parser,
                                                                    Token_Stream* ts)
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

Function_Body_Parse_Tree_Node* parser_parse_function_body(Parser* parser, Token_Stream* ts)
{
    if (!parser_expect_token(parser, ts, TOK_LBRACE))
    {
        assert(false);
    }

    Array<Statement_Parse_Tree_Node*> statements = {};
    array_init(parser->allocator, &statements);

    while (!parser_match_token(ts, TOK_RBRACE))
    {
        auto statement = parser_parse_statement(parser, ts);
        assert(statement);

        if (!parser_expect_token(parser, ts, TOK_SEMICOLON))
        {
            assert(false);
        }

        array_append(&statements, statement);
    }

    if (!statements.count)
    {
        array_free(&statements);
    }

    return new_function_body_parse_tree_node(parser->allocator, statements);
}

Array<Parameter_Parse_Tree_Node*> parser_parse_parameter_list(Parser* parser, Token_Stream* ts)
{
    Array<Parameter_Parse_Tree_Node*> parameters = {};
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

        Parameter_Parse_Tree_Node* parameter = parser_parse_parameter(parser, ts);
        assert(parameter);
        array_append(&parameters, parameter);
    }

    if (!parameters.count)
    {
        array_free(&parameters);
    }

    return parameters;
}

Parameter_Parse_Tree_Node* parser_parse_parameter(Parser* parser, Token_Stream* ts)
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

        return new_return_statement_parse_tree_node(parser->allocator, return_expr);
    }
    else
    {
        auto expr = parser_parse_expression(parser, ts);
        return new_expression_statement_parse_tree_node(parser->allocator, expr);
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
        parsed_file->declarations[i]->print();
    }
}

}
