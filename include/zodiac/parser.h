#pragma once

#include "allocator.h"
#include "parse_tree_node.h"
#include "token_stream.h"

namespace Zodiac
{

struct Parser
{
    Allocator* allocator = nullptr;
};

struct Parsed_File
{
    
};

Parser parser_create(Allocator* allocator);
void parser_init(Allocator* allocator, Parser* parser);

Parsed_File parser_parse_file(Parser* parser, Token_Stream* ts);

Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts);
Parse_Tree_Node* parser_parse_identifier(Parser* parser, Token_Stream* ts);

bool parser_expect_token(Parser* parser, Token_Stream* ts, Token_Kind kind);

void parsed_file_print(Parsed_File* parsed_file);
}
