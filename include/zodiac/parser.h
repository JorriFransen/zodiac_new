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
    Array<Declaration_PTN*> declarations = {};
};

Parser parser_create(Allocator* allocator);
void parser_init(Allocator* allocator, Parser* parser);

Parsed_File parser_parse_file(Parser* parser, Token_Stream* ts);

Declaration_PTN* parser_parse_declaration(Parser* parser, Token_Stream* ts);
Declaration_PTN* parser_parse_declaration(Parser* parser, Token_Stream* ts,
                                          Identifier_PTN* identifier);

Declaration_PTN* parser_parse_struct_declaration(Parser* parser, Token_Stream* ts,
                                                 Identifier_PTN* identifier);
Declaration_PTN* parser_parse_import_declaration(Parser* parser, Token_Stream* ts,
                                                 Identifier_PTN* identifier);

Identifier_PTN* parser_parse_identifier(Parser* parser, Token_Stream* ts);

Function_Proto_PTN* parser_parse_function_prototype(Parser* parser, Token_Stream* ts);
Array<Parameter_PTN*> parser_parse_parameter_list(Parser* parser, Token_Stream* ts);
Parameter_PTN* parser_parse_parameter(Parser* parser, Token_Stream* ts);

Statement_PTN* parser_parse_statement(Parser* parser, Token_Stream* ts);
Statement_PTN* parser_parse_assignment_statement(Parser* parser, Token_Stream* ts,
                                                 Expression_PTN* ident_expression);
Statement_PTN* parser_parse_self_assignment_statement(Parser* parser, Token_Stream* ts,
                                                      Expression_PTN* ident_expression);

Expression_PTN* parser_parse_expression(Parser* parser, Token_Stream* ts);
Expression_PTN* parser_parse_add_expression(Parser* parser, Token_Stream* ts);
Expression_PTN* parser_parse_base_expression(Parser* parser, Token_Stream* ts);
Expression_PTN* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                             Expression_PTN* ident_expr, bool is_builtin = false);
Expression_PTN* parser_parse_call_expression(Parser* parser, Token_Stream* ts,
                                                              bool is_builtin = false);
Expression_PTN* parser_parse_number_literal_expression(Parser* parser, Token_Stream* ts);
Expression_PTN* parser_parse_string_literal_expression(Parser* parser, Token_Stream* ts);

Expression_PTN* parser_parse_array_type_expression(Parser* parser, Token_Stream* ts);
Expression_PTN* parser_parse_pointer_type_expression(Parser* parser, Token_Stream* ts);

Expression_List_PTN* parser_parse_expression_list(Parser* parser, Token_Stream* ts);

Binary_Operator parser_parse_add_op(Token_Stream* ts);

bool parser_expect_token(Parser* parser, Token_Stream* ts, Token_Kind kind);
bool parser_match_token(Token_Stream* ts, Token_Kind kind);
bool parser_is_token(Token_Stream* ts, Token_Kind kind);

bool parser_is_add_op(Token_Stream* ts);

void parser_report_error(Parser* parser, Token_Stream* ts, const char* format, ...);
void parser_report_error(Parser* parser, Token_Stream* ts, const char* format, va_list args);

void parsed_file_print(Parsed_File* parsed_file);
}
