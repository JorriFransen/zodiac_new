#include "parse_tree_node.h"

namespace Zodiac
{

PTN_Kind Identifier_PTN::_kind = PTN_Kind::IDENTIFIER;
PTN_Kind Function_Proto_PTN::_kind = PTN_Kind::FUNCTION_PROTO;
PTN_Kind Statement_PTN::_kind = PTN_Kind::STATEMENT;
PTN_Kind Declaration_PTN::_kind = PTN_Kind::DECLARATION;
PTN_Kind Expression_List_PTN::_kind = PTN_Kind::EXPRESSION_LIST;
PTN_Kind Expression_PTN::_kind = PTN_Kind::EXPRESSION;
PTN_Kind Parameter_PTN::_kind = PTN_Kind::PARAMETER;

void init_ptn(PTN* ptn, PTN_Kind kind)
{
    assert(ptn);
    ptn->kind = kind;
}

Statement_PTN* new_statement(Allocator* allocator, Statement_PTN_Kind kind)
{
    auto result = new_ptn<Statement_PTN>(allocator);
    result->kind = kind;
    return result;
}

Identifier_PTN* new_identifier_ptn(Allocator* allocator, const Atom& atom)
{
    Identifier_PTN* result = new_ptn<Identifier_PTN>(allocator);
    result->atom = atom;
    return result;
}

Statement_PTN* new_block_statement_ptn(Allocator* allocator, Array<Statement_PTN*> statements)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::BLOCK);
    result->block.statements = statements;
    return result;
}

Statement_PTN* new_expression_statement_ptn(Allocator* allocator,
                                            Expression_PTN* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::EXPRESSION);
    result->expression = expr;
    return result;
}

Statement_PTN* new_declaration_statement_ptn(Allocator* allocator, Declaration_PTN* decl)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::DECLARATION);
    result->declaration = decl;
    return result;
}

Statement_PTN* new_return_statement_ptn(Allocator* allocator, Expression_PTN* expr)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::RETURN);
    result->return_stmt.expression = expr;
    return result;
}

Statement_PTN* new_assignment_statement_ptn(Allocator* allocator,
                                            Expression_PTN* ident_expression,
                                            Expression_PTN* rhs_expression)
{
    auto result = new_statement(allocator, Statement_PTN_Kind::ASSIGNMENT);
    result->assignment.ident_expression = ident_expression;
    result->assignment.rhs_expression = rhs_expression;
    return result;
}

Function_Proto_PTN* new_function_prototype_parse_tree_node(
    Allocator* allocator,
    Array<Parameter_PTN*> parameters,
    Expression_PTN* return_type_expr
)
{
    Function_Proto_PTN* result = new_ptn<Function_Proto_PTN>(allocator);
    result->parameters = parameters;
    result->return_type_expression = return_type_expr;
    return result;
}

Declaration_PTN* new_import_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                            Expression_PTN* module_ident_expr)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    assert(result);

    result->kind = Declaration_PTN_Kind::IMPORT;
    result->identifier = identifier;
    result->import.module_ident_expr = module_ident_expr;

    return result;
}

Declaration_PTN* new_function_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Function_Proto_PTN* prototype, Statement_PTN* body)
{
    assert(prototype);
    if (body) assert(body->kind == Statement_PTN_Kind::BLOCK);

    auto result = new_ptn<Declaration_PTN>(allocator);
    assert(result);

    result->kind = Declaration_PTN_Kind::FUNCTION;
    result->identifier = identifier;
    result->function.prototype = prototype;
    result->function.body = body;

    return result;
}

Declaration_PTN* new_variable_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Expression_PTN* type_expression,
                                              Expression_PTN* init_expression)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::VARIABLE;
    result->identifier = identifier;
    result->variable.type_expression = type_expression;
    result->variable.init_expression = init_expression;
    return result;
}

Declaration_PTN* new_struct_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                            Array<Declaration_PTN*> members,
                                            Array<Parameter_PTN*> parameters)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::STRUCT;
    result->identifier = identifier;
    result->structure.member_declarations = members;
    result->structure.parameters = parameters;
    return result;
}

Declaration_PTN* new_constant_declaration_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                              Expression_PTN* type_expr, Expression_PTN* init_expr)
{
    auto result = new_ptn<Declaration_PTN>(allocator);
    result->kind = Declaration_PTN_Kind::CONSTANT;
    result->identifier = identifier;
    result->constant.type_expression = type_expr;
    result->constant.init_expression = init_expr;

    return result;
}

Expression_List_PTN* new_expression_list_ptn(Allocator* allocator,
                                             Array<Expression_PTN*> expressions
)
{
    auto result = new_ptn<Expression_List_PTN>(allocator);
    result->expressions = expressions;
    return result;
}

Expression_PTN* new_call_expression_ptn(Allocator* allocator, bool is_builtin,
                                        Expression_PTN* ident_expr, Expression_List_PTN* arg_list)
{
    assert(ident_expr->kind == Expression_PTN_Kind::IDENTIFIER ||
           ident_expr->kind == Expression_PTN_Kind::DOT);

    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::CALL;
    result->call.is_builtin = is_builtin;
    result->call.ident_expression = ident_expr;
    result->call.arg_list = arg_list;
    return result;
}

Expression_PTN* new_identifier_expression_ptn(Allocator* allocator, Identifier_PTN* identifier)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::IDENTIFIER;
    result->identifier = identifier;
    return result;
}

Expression_PTN* new_binary_expression_ptn(Allocator* allocator, Binary_Operator op,
                                          Expression_PTN* lhs, Expression_PTN* rhs)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::BINARY;
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;

    return result;
}

Expression_PTN* new_unary_expression_ptn(Allocator* allocator, Unary_Operator op,
                                         Expression_PTN* operand_expression)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::UNARY;
    result->unary.op = op;
    result->unary.operand_expression = operand_expression;

    return result;
}

Expression_PTN* new_number_literal_expression_ptn(Allocator* allocator, Atom atom)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::NUMBER_LITERAL;
    if (atom.data[0] == '-')
    {
        result->number_literal.value.s64 = atom_to_s64(atom);
    }
    else
    {
        result->number_literal.value.u64 = atom_to_u64(atom);
    }

    return result;
}

Expression_PTN* new_string_literal_expression_ptn(Allocator* allocator, Atom atom)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::STRING_LITERAL;

    result->string_literal.atom = atom;

    return result;
}

Expression_PTN* new_dot_expression_ptn(Allocator* allocator, Expression_PTN* parent,
                                       Identifier_PTN* child_ident)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::DOT;
    result->dot.parent_expression = parent;
    result->dot.child_identifier = child_ident;
    return result;
}

Expression_PTN* new_compound_expression_ptn(Allocator* allocator, Expression_List_PTN* expr_list,
                                            Expression_PTN* type_expression)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::COMPOUND;
    result->compound.list = expr_list;
    result->compound.type_expression = type_expression;
    return result;
}

Expression_PTN* new_array_type_expression_ptn(Allocator* allocator,
                                              Expression_PTN* element_type_expression)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::ARRAY_TYPE;
    result->array_type.element_type_expression = element_type_expression;
    return result;
}

Expression_PTN* new_pointer_type_expression_ptn(Allocator* allocator,
                                                Expression_PTN* pointee_type_expression)
{
    auto result = new_ptn<Expression_PTN>(allocator);
    result->kind = Expression_PTN_Kind::POINTER_TYPE;
    result->pointer_type.pointee_type_expression = pointee_type_expression;
    return result;
}

Parameter_PTN* new_parameter_ptn(Allocator* allocator, Identifier_PTN* identifier,
                                 Expression_PTN* type_expression)
{
    auto result = new_ptn<Parameter_PTN>(allocator);

    result->identifier = identifier;
    result->type_expression = type_expression;

    return result;
}

void print_indent(uint64_t indent)
{
    for (uint64_t i = 0; i < indent; i++)
    {
        printf(" ");
    }
}

Declaration_PTN* copy_declaration_ptn(Allocator* allocator, Declaration_PTN* decl,
                                      PTN_Copy_Flags flags/*= PTNC_FLAG_NONE*/)
{
    Identifier_PTN* ident_copy = nullptr;
    if (flags & PTNC_FLAG_DONT_COPY_IDENTIFIERS)
    {
        ident_copy = decl->identifier;
    }
    else
    {
        ident_copy = copy_identifier_ptn(allocator, decl->identifier);
    }

    assert(ident_copy);

    switch (decl->kind)
    {
        case Declaration_PTN_Kind::INVALID: assert(false);
        case Declaration_PTN_Kind::IMPORT: assert(false);

        case Declaration_PTN_Kind::VARIABLE:
        {
            Expression_PTN* type_expr_copy = nullptr;
            Expression_PTN* init_expr_copy = nullptr;
            if (flags & PTNC_FLAG_DONT_COPY_EXPRESSIONS)
            {
                type_expr_copy = decl->variable.type_expression;
                init_expr_copy = decl->variable.init_expression;
            }
            else
            {
                type_expr_copy = copy_expression_ptn(allocator, decl->variable.type_expression);
                init_expr_copy = copy_expression_ptn(allocator, decl->variable.init_expression);
            }

            assert(type_expr_copy);
            if (decl->variable.init_expression) assert(init_expr_copy);
            
            return new_variable_declaration_ptn(allocator,
                                                ident_copy,
                                                type_expr_copy,
                                                init_expr_copy);
        }

        case Declaration_PTN_Kind::CONSTANT: assert(false);
        case Declaration_PTN_Kind::FUNCTION: assert(false);
        case Declaration_PTN_Kind::STRUCT: assert(false);
    }

    assert(false);
    return nullptr;
}

Expression_PTN* copy_expression_ptn(Allocator* allocator, Expression_PTN* expr,
                                    PTN_Copy_Flags flags/*= PTNC_FLAG_NONE*/)
{
    if (!expr) return nullptr;

    assert(allocator);

    switch (expr->kind)
    {
        case Expression_PTN_Kind::INVALID: assert(false);
        case Expression_PTN_Kind::CALL: assert(false);

        case Expression_PTN_Kind::IDENTIFIER:
        {
            Identifier_PTN* new_identifier = nullptr;
            if (flags & PTNC_FLAG_DONT_COPY_IDENTIFIERS)
                new_identifier = expr->identifier;
            else
                new_identifier = copy_identifier_ptn(allocator, expr->identifier);

            assert(new_identifier);

            return new_identifier_expression_ptn(allocator, new_identifier);
        }

        case Expression_PTN_Kind::BINARY: assert(false);
        case Expression_PTN_Kind::UNARY: assert(false);
        case Expression_PTN_Kind::DOT: assert(false);
        case Expression_PTN_Kind::COMPOUND: assert(false);
        case Expression_PTN_Kind::NUMBER_LITERAL: assert(false);
        case Expression_PTN_Kind::STRING_LITERAL: assert(false);
        case Expression_PTN_Kind::ARRAY_TYPE: assert(false);
        case Expression_PTN_Kind::POINTER_TYPE: assert(false);
    }

    assert(false);
    return nullptr;
}

Identifier_PTN* copy_identifier_ptn(Allocator* allocator, Identifier_PTN* identifier)
{
    return new_identifier_ptn(allocator, identifier->atom);
}

void print_ptn(PTN* ptn, uint64_t indent)
{
    switch (ptn->kind)
    {
        case PTN_Kind::INVALID:
        {
            assert(false);
            break;
        }

        case PTN_Kind::IDENTIFIER:
        {
            auto _this = (Identifier_PTN*)ptn;
            print_indent(indent);
            printf("%s", _this->atom.data);
            break;
        }

        case PTN_Kind::FUNCTION_PROTO:
        {
            auto _this = (Function_Proto_PTN*)ptn;

            print_indent(indent);
            printf("func (");
            if (_this->parameters.count)
            {
                for (int64_t i = 0; i < _this->parameters.count; i++)
                {
                    if (i > 0) printf(", ");
                    print_ptn(&_this->parameters[i]->self, 0);
                }
            }
            printf(")");
            if (_this->return_type_expression)
            {
                printf(" -> ");
                print_expression_ptn(_this->return_type_expression, 0);
            }
            printf("\n");
            // print_indent(indent);
            // printf("FUNC_PROTO:\n");
            break;
        }

        case PTN_Kind::PARAMETER:
        {
            print_indent(indent);
            auto _this = (Parameter_PTN*)ptn;
            print_ptn(&_this->identifier->self, 0);
            printf(": ");
            print_expression_ptn(_this->type_expression, 0);
            break;
        }

        case PTN_Kind::EXPRESSION_LIST:
        {
            auto _this = (Expression_List_PTN*)ptn;
            for (int64_t i = 0 ; i < _this->expressions.count; i++)
            {
                if (i > 0) printf(", ");
                print_expression_ptn(_this->expressions[i], indent);
            }
            break;
        }

        case PTN_Kind::DECLARATION:
        {
            print_declaration_ptn((Declaration_PTN*)ptn, indent);
            break;
        }

        case PTN_Kind::STATEMENT:
        {
            print_statement_ptn((Statement_PTN*)ptn, indent);
            break;
        }

        case PTN_Kind::EXPRESSION:
        {
            assert(false);
            break;
        }
    }
}

void print_statement_ptn(Statement_PTN* statement, uint64_t indent, bool newline/*=true*/)
{
    switch (statement->kind)
    {
        case Statement_PTN_Kind::INVALID: assert(false);

        case Statement_PTN_Kind::BLOCK:
        {
            print_indent(indent);
            printf("{\n");

            for (int64_t i = 0; i < statement->block.statements.count; i++)
            {
                print_statement_ptn(statement->block.statements[i], indent + 4, false);
            }

            print_indent(indent);
            printf("}");
            if (newline) printf("\n");
            break;
        }

        case Statement_PTN_Kind::DECLARATION:
        {
            print_declaration_ptn(statement->declaration, indent, false);
            if (newline) printf("\n");
            break;
        }

        case Statement_PTN_Kind::EXPRESSION:
        {
            print_expression_ptn(statement->expression, indent);
            printf(";\n");
            if (newline) printf("\n");
            break;
        }

        case Statement_PTN_Kind::RETURN:
        {
            print_indent(indent);
            printf("return");
            if (statement->return_stmt.expression)
            {
                printf(" ");
                print_expression_ptn(statement->return_stmt.expression, 0);
            }
            printf(";\n");
            break;
        }

        case Statement_PTN_Kind::ASSIGNMENT:
        {
            print_expression_ptn(statement->assignment.ident_expression, indent);
            printf(" = ");
            print_expression_ptn(statement->assignment.rhs_expression, 0);
            printf(";\n");
            break;
        }
    }
}

void print_declaration_ptn(Declaration_PTN* decl, uint64_t indent, bool newline/*=true*/)
{
    switch (decl->kind)
    {
        case Declaration_PTN_Kind::INVALID: assert(false);

        case Declaration_PTN_Kind::IMPORT:
        {
            print_indent(indent);
            printf("%s :: import ", decl->identifier->atom.data);
            print_expression_ptn(decl->import.module_ident_expr, 0);
            printf(";\n");
            if (newline) printf("\n");
            break;
        }

        case Declaration_PTN_Kind::FUNCTION:
        {
            print_indent(indent);
            printf("%s :: ", decl->identifier->atom.data);
            print_ptn(&decl->function.prototype->self, 0);
            if (decl->function.body)
            {
                print_statement_ptn(decl->function.body, indent);
            }

            if (newline) printf("\n");
            break;
        }

        case Declaration_PTN_Kind::VARIABLE:
        {
            print_indent(indent);
            printf("%s :", decl->identifier->atom.data);
            bool has_type = false;
            if (decl->variable.type_expression)
            {
                printf(" ");
                print_expression_ptn(decl->variable.type_expression, 0);
                has_type = true;
            }
            if (decl->variable.init_expression)
            {
                if (has_type) printf(" ");
                printf("= ");
                print_expression_ptn(decl->variable.init_expression, 0);
            }
            printf(";\n");
            if (newline) printf("\n");
            break;
        }

        case Declaration_PTN_Kind::CONSTANT:
        {
            print_indent(indent);
            printf("%s :", decl->identifier->atom.data);
            bool has_type = false;
            if (decl->constant.type_expression)
            {
                printf(" ");
                print_expression_ptn(decl->constant.type_expression, 0);
                has_type = true;
            }
            if (decl->constant.init_expression)
            {
                if (has_type) printf(" ");
                printf(": ");
                print_expression_ptn(decl->constant.init_expression, 0);
            }
            printf(";\n");
            if (newline) printf("\n");
            break;
        };

        case Declaration_PTN_Kind::STRUCT:
        {
            print_indent(indent);
            printf("%s :: struct", decl->identifier->atom.data);
            if (decl->structure.parameters.count)
            {
                printf("(");
                for (int64_t i = 0; i < decl->structure.parameters.count; i++)
                {
                    print_ptn(&decl->structure.parameters[i]->self, 0);
                }
                printf(")");
            }
            printf("\n");
            print_indent(indent);
            printf("{\n");
            for (int64_t i = 0; i < decl->structure.member_declarations.count; i++)
            {
                auto mem_decl = decl->structure.member_declarations[i];
                if (mem_decl->kind == Declaration_PTN_Kind::FUNCTION)
                {
                    printf("\n");
                }
                print_declaration_ptn(mem_decl, indent + 4, false);
            }
            print_indent(indent);
            printf("}\n");
            if (newline) printf("\n");
            break;
        }
    }
}

void print_expression_ptn(Expression_PTN* expression, uint64_t indent)
{
    switch (expression->kind)
    {
        case Expression_PTN_Kind::INVALID: assert(false);

        case Expression_PTN_Kind::CALL:
        {
            print_expression_ptn(expression->call.ident_expression, indent);
            printf("(");
            if (expression->call.arg_list)
            {
                print_ptn(&expression->call.arg_list->self, 0);
            }
             printf(")");

            // printf("CALL: \"%s\"\n", expression->call.identifier->atom.data);
            break;
        }

        case Expression_PTN_Kind::IDENTIFIER:
        {
            print_indent(indent);
            printf("%s", expression->identifier->atom.data);
            break;
        }

        case Expression_PTN_Kind::BINARY:
        {
            print_indent(indent);
            printf("(");
            print_expression_ptn(expression->binary.lhs, 0);
            switch (expression->binary.op)
            {
                case BINOP_INVALID: assert(false);

                case BINOP_ADD:
                {
                    printf(" + ");
                    break;
                }

                case BINOP_SUB:
                {
                    printf(" - ");
                    break;
                }
            }
            print_expression_ptn(expression->binary.rhs, 0);
            printf(")");
            break;
        }

        case Expression_PTN_Kind::UNARY:
        {
            print_indent(indent);
            printf("(");
            switch (expression->unary.op)
            {
                case UNOP_INVALID: assert(false); break;
                case UNOP_DEREF:   printf("<");   break;
            }
            print_expression_ptn(expression->unary.operand_expression, 0);
            printf(")");
            break;
        };

        case Expression_PTN_Kind::NUMBER_LITERAL:
        {
            print_indent(indent);
            printf("%ld", expression->number_literal.value.s64);
            break;
        }

        case Expression_PTN_Kind::STRING_LITERAL:
        {
            print_indent(indent);
            printf("\"%s\"", expression->string_literal.atom.data);
            break;
        }

        case Expression_PTN_Kind::DOT:
        {
            print_indent(indent);
            print_expression_ptn(expression->dot.parent_expression, 0);
            printf(".");
            print_ptn(&expression->dot.child_identifier->self, 0);
            break;
        }

        case Expression_PTN_Kind::COMPOUND:
        {
            print_indent(indent);
            if (expression->compound.type_expression) 
            {
                print_expression_ptn(expression->compound.type_expression, 0);
                printf(" ");
            }
            printf("{ ");
            print_ptn(&expression->compound.list->self, 0);
            if (expression->compound.list->expressions.count) printf(" ");
            printf("}");
            break;
        }

        case Expression_PTN_Kind::ARRAY_TYPE:
        {
            print_indent(indent);
            printf("[]");
            print_expression_ptn(expression->array_type.element_type_expression, 0);
            break;
        }

        case Expression_PTN_Kind::POINTER_TYPE:
        {
            print_indent(indent);
            printf("*");
            print_expression_ptn(expression->pointer_type.pointee_type_expression, 0);
            break;
        }

    }
}

}
