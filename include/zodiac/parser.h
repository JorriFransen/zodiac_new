#pragma once

#include "allocator.h"
#include "build_data.h"
#include "parse_tree_node.h"
#include "token_stream.h"
#include "operator.h"
#include "struct_predecls.h"

namespace Zodiac
{

struct Parser
{
    Allocator *allocator = nullptr;
    Build_Data *build_data = nullptr;
};

Parser parser_create(Allocator *allocator, Build_Data *build_data);
void parser_init(Allocator *allocator, Parser *parser, Build_Data *build_data);

void parsed_file_init(Parser *parser, Parsed_File *pf);
Parsed_File parser_parse_file(Parser *parser, Token_Stream *ts);
void parser_parse_file(Parser *parser, Token_Stream *ts, Parsed_File *pf);
void parser_free_parsed_file(Parser *parser, Parsed_File *parsed_file);

Declaration_PTN *parser_parse_declaration(Parser *parser, Token_Stream *ts);
Declaration_PTN *parser_parse_declaration(Parser *parser, Token_Stream *ts,
                                          Identifier_PTN *identifier,
                                          bool is_naked = false,
                                          bool is_noreturn = false,
                                          bool is_foreign = false);

Declaration_PTN *parser_parse_struct_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier);
Declaration_PTN *parser_parse_union_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier);
Declaration_PTN *parser_parse_enum_declaration(Parser *parser, Token_Stream *ts,
                                               Identifier_PTN *identifier,
                                               Expression_PTN *type_spec_expr);
Declaration_PTN *parser_parse_import_declaration(Parser *parser, Token_Stream *ts,
                                                 Identifier_PTN *identifier);

Declaration_PTN *parser_parse_static_if_declaration(Parser *parser, Token_Stream *ts,
                                                    bool elseif = false);

Identifier_PTN *parser_parse_identifier(Parser *parser, Token_Stream *ts);

Function_Proto_PTN *parser_parse_function_prototype(Parser *parser, Token_Stream *ts);
Array<Parameter_PTN*> parser_parse_parameter_list(Parser *parser, Token_Stream *ts);
Parameter_PTN *parser_parse_parameter(Parser *parser, Token_Stream *ts);

Statement_PTN *parser_parse_statement(Parser *parser, Token_Stream *ts);
Statement_PTN *parser_parse_assignment_statement(Parser *parser, Token_Stream *ts,
                                                 Expression_PTN *ident_expression);
Statement_PTN *parser_parse_self_assignment_statement(Parser *parser, Token_Stream *ts,
                                                      Expression_PTN *ident_expression);
Statement_PTN *parser_parse_switch_statement(Parser *parser, Token_Stream *ts);
Switch_Case_PTN parser_parse_switch_case(Parser *parser, Token_Stream *ts);
Array<Switch_Case_Expression_PTN> parser_parse_case_expressions(Parser *parser, Token_Stream *ts);

Expression_PTN *parser_parse_expression(Parser *parser, Token_Stream *ts, bool is_type = false);
Expression_PTN *parser_parse_cmp_expression(Parser *parser, Token_Stream *ts, bool is_type = false);
Expression_PTN *parser_parse_add_expression(Parser *parser, Token_Stream *ts, bool is_type = false);
Expression_PTN *parser_parse_mul_expression(Parser *parser, Token_Stream *ts, bool is_type = false);
Expression_PTN *parser_parse_unary_expression(Parser *parser, Token_Stream *ts,
                                              bool is_type = false);
Expression_PTN *parser_parse_base_expression(Parser *parser, Token_Stream *ts, bool is_type = false);
Expression_PTN *parser_parse_call_expression(Parser *parser, Token_Stream *ts,
                                             Expression_PTN *ident_expr, bool is_builtin = false);
Expression_PTN *parser_parse_call_expression(Parser *parser, Token_Stream *ts,
                                                              bool is_builtin = false);
Expression_PTN *parser_parse_number_literal_expression(Parser *parser, Token_Stream *ts);
Expression_PTN *parser_parse_string_literal_expression(Parser *parser, Token_Stream *ts);
Expression_PTN *parser_parse_character_literal_expression(Parser *parser, Token_Stream *ts);
Expression_PTN *parser_parse_boolean_literal_expression(Parser *parser, Token_Stream *ts);

Expression_PTN *parser_parse_array_type_expression(Parser *parser, Token_Stream *ts);
Expression_PTN *parser_parse_pointer_type_expression(Parser *parser, Token_Stream *ts);
Expression_PTN *parser_parse_poly_type_expression(Parser *parser, Token_Stream *ts);

Expression_List_PTN *parser_parse_expression_list(Parser *parser, Token_Stream *ts);

Binary_Operator parser_parse_cmp_op(Token_Stream *ts);
Binary_Operator parser_parse_add_op(Token_Stream *ts);
Binary_Operator parser_parse_mul_op(Token_Stream *ts);

bool parser_expect_token(Parser *parser, Token_Stream *ts, Token_Kind kind);
bool parser_match_token(Token_Stream *ts, Token_Kind kind);
bool parser_is_token(Token_Stream *ts, Token_Kind kind);

bool parser_is_cmp_op(Token_Stream *ts);

bool parser_is_add_op(const Token &token);
bool parser_is_add_op(Token_Stream *ts);

bool parser_is_mul_op(Token_Stream *ts);

Unary_Operator parser_parse_unary_op(Token_Stream *ts);

void parser_report_unexpected_token(Parser *parser, Token_Stream *ts, const Token &tok);

bool parser_make_escape_char(char c, char *dest);

void parsed_file_print(Parsed_File *parsed_file);
}
