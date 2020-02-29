#include "parser.h"

#include "allocator.h"

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

    while (ts->current_token().kind != TOK_EOF)
    {
        auto ptn = parser_parse_declaration(parser, ts);
        assert(ptn);
    }

    return result;
}

Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    while (ts->current_token().kind == TOK_POUND)
    {
        assert(false);
    }

    auto identifier = parser_parse_identifier(parser, ts);
    if (!identifier) return nullptr;

    if (!parser_expect_token(parser, ts, TOK_COLON)) return nullptr;

    auto ct = ts->current_token();
    if (ct.kind == TOK_COLON)
    {
        assert(false);
    }
    else if (ct.kind == TOK_EQ)
    {
        assert(false);
    }
    else
    {
        assert(false);
    }
}

Parse_Tree_Node* parser_parse_identifier(Parser* parser, Token_Stream* ts)
{
    assert(parser);
    assert(ts);

    auto ident_token = ts->current_token();
    if (!parser_expect_token(parser, ts, TOK_IDENTIFIER)) return nullptr;

    return new_identifier_parse_tree_node(parser->allocator, ident_token.atom);
}

bool parser_expect_token(Parser* parser, Token_Stream* ts, Token_Kind kind)
{
    assert(parser);
    assert(ts);
    assert(kind);

    auto ct = ts->current_token();

    if (ct.kind != kind)
    {
        fprintf(stderr, "Error: Expected token: \"%s\", got: \"%s\"\n",
                token_kind_name(kind), token_kind_name(ct.kind));
        assert(false); // report error
    }

    return true;
}

void parsed_file_print(Parsed_File* parsed_file)
{
    assert(false);
    assert(parsed_file);
}

}
