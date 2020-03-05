#pragma once

#include "allocator.h"
#include "parse_tree_node.h"
#include "token_stream.h"
#include "operator.h"

namespace Zodiac
{

struct Parser
{
    Allocator* allocator = nullptr;
};

struct Parsed_File
{
    Array<Declaration_Parse_Tree_Node*> declarations = {}; 
};

Parser parser_create(Allocator* allocator);
void parser_init(Allocator* allocator, Parser* parser);

Parsed_File parser_parse_file(Parser* parser, Token_Stream* ts);

Declaration_Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts);
Declaration_Parse_Tree_Node* parser_parse_declaration(Parser* parser, Token_Stream* ts,
                                                      Identifier_Parse_Tree_Node* identifier);
Struct_Declaration_Parse_Tree_Node*
parser_parse_struct_declaration(Parser* parser, Token_Stream* ts,
                                Identifier_Parse_Tree_Node* identifier);

Identifier_Parse_Tree_Node* parser_parse_identifier(Parser* parser, Token_Stream* ts);

Function_Prototype_Parse_Tree_Node* parser_parse_function_prototype(Parser* parser,
                                                                    Token_Stream* ts);
Function_Body_Parse_Tree_Node* parser_parse_function_body(Parser* parser, Token_Stream* ts);
Array<Parameter_Parse_Tree_Node*> parser_parse_parameter_list(Parser* parser, Token_Stream* ts);
Parameter_Parse_Tree_Node* parser_parse_parameter(Parser* parser, Token_Stream* ts);

Statement_Parse_Tree_Node* parser_parse_statement(Parser* parser, Token_Stream* ts);

Expression_Parse_Tree_Node* parser_parse_expression(Parser* parser, Token_Stream* ts);
Expression_Parse_Tree_Node* parser_parse_add_expression(Parser* parser, Token_Stream* ts);
Expression_Parse_Tree_Node* parser_parse_base_expression(Parser* parser, Token_Stream* ts);
Call_Expression_Parse_Tree_Node* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                                              bool is_builtin = false);
Number_Literal_Expression_Parse_Tree_Node*
parser_parse_number_literal_expression(Parser* parser, Token_Stream* ts);

Expression_List_Parse_Tree_Node* parser_parse_expression_list(Parser* parser, Token_Stream* ts);

Binary_Operator parser_parse_add_op(Token_Stream* ts);

bool parser_expect_token(Parser* parser, Token_Stream* ts, Token_Kind kind);
bool parser_match_token(Token_Stream* ts, Token_Kind kind);
bool parser_is_token(Token_Stream* ts, Token_Kind kind);

bool parser_is_add_op(Token_Stream* ts);

void parsed_file_print(Parsed_File* parsed_file);
}
