#include "bytecode.h"

#include "builtin.h"
#include "parser.h"
#include "string_builder.h"

#include <stdio.h>

namespace Zodiac
{
    Bytecode_Builder bytecode_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        Bytecode_Builder result = {};

        result.allocator = allocator;
        result.build_data = build_data;
        result.insert_block = nullptr;

        result.current_function = nullptr;

        array_init(allocator, &result.functions);
        array_init(allocator, &result.parameters);
        array_init(allocator, &result.locals);

        result.next_temp_index = 0;

        return result;
    }

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);
        assert(decl->type);

        auto ex_func = bytecode_find_function(builder, decl);

        if (ex_func)
        {
            assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);
            assert(false);
        }

        assert(!(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE));

        assert(decl->identifier);
        auto name = decl->identifier->atom;

        Bytecode_Function *result = bytecode_new_function(builder, decl->type, name);

        auto index = builder->functions.count;
        array_append(&builder->functions, { decl, result, index });

        decl->decl_flags |= AST_DECL_FLAG_REGISTERED_BYTECODE;

        return result;
    }

    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bytecode_find_function(builder, decl);
        assert(func);

        auto bd = builder->build_data;

        if (decl->decl_flags & AST_DECL_FLAG_IS_ENTRY)
        {
            assert(!bd->bc_entry_function);
            bd->bc_entry_function = func;
            func->flags |= BC_FUNC_FLAG_CRT_ENTRY;
        }
        if (decl->decl_flags & AST_DECL_FLAG_IS_BYTECODE_ENTRY)
        {
            assert(!bd->bc_bytecode_entry_function);
            bd->bc_bytecode_entry_function = func;
        }

        builder->current_function = func;
        builder->parameters.count = 0;
        builder->locals.count = 0;
        builder->next_temp_index = 0;

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
        {
            auto param_decl = decl->function.parameter_declarations[i];
            assert(param_decl->kind == AST_Declaration_Kind::PARAMETER);

            auto param_val = bytecode_parameter_new(builder, param_decl->type,
                                                    param_decl->identifier->atom);
            array_append(&builder->parameters, { param_decl, param_val });
        }

        Bytecode_Block *entry_block = bytecode_new_block(builder, "entry");
        bytecode_append_block(func, entry_block);

        bytecode_set_insert_point(builder, entry_block);

        for (int64_t i = 0; i < decl->function.variable_declarations.count; i++)
        {
            auto var_decl = decl->function.variable_declarations[i];
            assert(var_decl->kind == AST_Declaration_Kind::VARIABLE);

            auto name = var_decl->identifier->atom;

            auto local_val = bytecode_local_alloc_new(builder, var_decl->type, name);

            bytecode_emit_instruction(builder, ALLOCL, nullptr, nullptr, local_val);

            array_append(&builder->locals, { var_decl, local_val });
        }

        bytecode_emit_statement(builder, decl->function.body);

        return func;
    }

    Bytecode_Value *bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(false);
    }

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name)
    {
        Bytecode_Block *result = alloc_type<Bytecode_Block>(builder->allocator);
        result->name = name;
        result->function = nullptr;
        array_init(builder->allocator, &result->instructions);
        return result;
    }

    void bytecode_append_block(Bytecode_Function *function, Bytecode_Block *block)
    {
        assert(!block->function);

        array_append(&function->blocks, block);
        block->function = function;
    }

    void bytecode_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block)
    {
        builder->insert_block = block;
    }

    Bytecode_Function *bytecode_find_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            if (builder->functions[i].declaration == decl) return builder->functions[i].bc_func;
        }

        return nullptr;
    }

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        assert(type->kind == AST_Type_Kind::FUNCTION);

        Bytecode_Function *result = alloc_type<Bytecode_Function>(builder->allocator);

        result->type = type;
        result->name = name;

        array_init(builder->allocator, &result->parameters);
        array_init(builder->allocator, &result->locals, 4);
        array_init(builder->allocator, &result->temps);
        array_init(builder->allocator, &result->blocks, 4);

        return result;
    }

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        switch (decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::USING: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::VARIABLE:
            {
                auto allocl_val = bytecode_find_variable(builder, decl);
                assert(allocl_val);

                if (decl->variable.init_expression)
                {
                    auto init_val = bytecode_emit_expression(builder,
                                                             decl->variable.init_expression);
                    bytecode_emit_store(builder, allocl_val, init_val);
                }

                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::PARAMETER: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::FUNCTION: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::TYPE: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::TYPEDEF: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::STRUCTURE: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::ENUM: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::POLY_TYPE: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::STATIC_IF: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false); //@@TODO: Implement!
        }
    }

    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(builder->insert_block);

        switch (stmt->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                for (int64_t i = 0; i < stmt->block.statements.count; i++)
                {
                    bytecode_emit_statement(builder, stmt->block.statements[i]);
                }
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false); //@@TODO: Implement!

            case AST_Statement_Kind::RETURN:
            {
                if (stmt->expression)
                {
                    auto ret_val = bytecode_emit_expression(builder, stmt->expression);
                    bytecode_emit_instruction(builder, RETURN, ret_val, nullptr, nullptr);
                }
                else
                {
                    assert(false); //@@TODO: Implement!
                }
                break;
            }

            case AST_Statement_Kind::BREAK: assert(false); //@@TODO: Implement!

            case AST_Statement_Kind::DECLARATION:
            {
                bytecode_emit_declaration(builder, stmt->declaration);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                bytecode_emit_expression(builder, stmt->expression);
                break;
            }

            case AST_Statement_Kind::WHILE: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::FOR: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::IF: assert(false); //@@TODO: Implement!
            case AST_Statement_Kind::SWITCH: assert(false); //@@TODO: Implement!
        }
    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr)
    {
        Bytecode_Value *result = nullptr;

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto decl = expr->identifier->declaration;
                assert(decl);

                Bytecode_Value *source_val = nullptr;

                if (decl->kind == AST_Declaration_Kind::VARIABLE)
                {
                    source_val = bytecode_find_variable(builder, decl);
                }
                else if (decl->kind == AST_Declaration_Kind::PARAMETER)
                {
                    source_val = bytecode_find_parameter(builder, decl);
                }
                else
                {
                    assert(false);
                }

                assert(source_val);
                result = bytecode_emit_load(builder, source_val);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::DOT: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::BINARY:
            {

#define _binop_arithmetic(int_sign_op) { \
    auto lhs_expr = expr->binary.lhs; \
    auto rhs_expr = expr->binary.rhs; \
    assert(lhs_expr->type == rhs_expr->type); \
    auto type = lhs_expr->type; \
    if (type->kind == AST_Type_Kind::INTEGER) { \
        assert(lhs_expr->type->integer.sign == rhs_expr->type->integer.sign); \
    } else { \
        assert(type->kind == AST_Type_Kind::FLOAT); \
    } \
    Bytecode_Value *lhs = bytecode_emit_expression(builder, lhs_expr); \
    Bytecode_Value *rhs = bytecode_emit_expression(builder, rhs_expr); \
    result = bytecode_temporary_new(builder, type); \
    if (type->kind == AST_Type_Kind::INTEGER) { \
        if (type->integer.sign) { \
            bytecode_emit_instruction(builder, int_sign_op, lhs, rhs, result); \
        } else { \
            assert(false); \
        } \
    } else { \
        assert(false); \
    } \
    break; \
}
                switch (expr->binary.op)
                {
                    case BINOP_INVALID: assert(false);
                    case BINOP_EQ: assert(false); //@TODO: Implement!
                    case BINOP_NEQ: assert(false); //@TODO: Implement!
                    case BINOP_LT: assert(false); //@TODO: Implement!
                    case BINOP_LTEQ: assert(false); //@TODO: Implement!
                    case BINOP_GT: assert(false); //@TODO: Implement!
                    case BINOP_GTEQ: assert(false); //@TODO: Implement!


                    case BINOP_ADD: _binop_arithmetic(ADD_S);
                    case BINOP_SUB: _binop_arithmetic(SUB_S);

                    case BINOP_REMAINDER: assert(false); //@TODO: Implement!
                    case BINOP_MUL: assert(false); //@TODO: Implement!
                    case BINOP_DIV: assert(false); //@TODO: Implement!

                }

#undef _binop_arithmetic
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::POST_FIX: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::PRE_FIX: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::CALL:
            {
                result = bytecode_emit_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL:
            {
                result = bytecode_emit_builtin_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::ADDROF: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::COMPOUND: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::SUBSCRIPT: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::CAST: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::INTEGER_LITERAL:
            {
                result = bytecode_integer_literal_new(builder, expr->type, expr->integer_literal);
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::STRING_LITERAL:
            {
                result = bytecode_string_literal_new(builder, expr->string_literal.atom);
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::BOOL_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::NULL_LITERAL: assert(false); //@TODO: Implement!
            case AST_Expression_Kind::RANGE: assert(false); //@TODO: Implement!
        }

        assert(result || expr->type->kind == AST_Type_Kind::VOID);
        if (result) assert(result->type = expr->type);

        return result;
    }

    Bytecode_Value *bytecode_emit_call(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::CALL);

        auto decl = expr->call.callee_declaration;
        assert(decl);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bytecode_find_function(builder, decl);
        assert(func);

        auto arg_exprs = expr->call.arg_expressions;
        assert(arg_exprs.count == func->parameters.count);

        for (int64_t i = 0; i < arg_exprs.count; i++)
        {
            Bytecode_Value *arg_val = bytecode_emit_expression(builder, arg_exprs[i]);
            bytecode_emit_instruction(builder, PUSH_ARG, arg_val, nullptr, nullptr);
        }

        Bytecode_Value *return_value = nullptr;
        if (func->type->function.return_type->kind != AST_Type_Kind::VOID)
        {
            return_value = bytecode_temporary_new(builder, func->type->function.return_type);
        }

        auto func_val = bytecode_function_value_new(builder, func);
        auto arg_count_val = bytecode_integer_literal_new(builder, Builtin::type_u64,
                                                          { .u64 = (uint64_t)arg_exprs.count });

        bytecode_emit_instruction(builder, CALL, func_val, arg_count_val, return_value);

        return return_value;
    }

    Bytecode_Value *bytecode_emit_builtin_call(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::BUILTIN_CALL);

        auto args = expr->builtin_call.arg_expressions;

        auto name = expr->builtin_call.identifier->atom;

        if (name == Builtin::atom_static_assert) assert(false);
        else if (name == Builtin::atom_exit)
        {
            assert(args.count == 1);

            auto exit_code_val = bytecode_emit_expression(builder, args[0]);
            bytecode_emit_instruction(builder, EXIT, exit_code_val, nullptr, nullptr);
            return nullptr;

        }
        else if (name == Builtin::atom_syscall)
        {
            int64_t total_arg_size = 0;
            for (int64_t i = 0; i < args.count; i++)
            {
                Bytecode_Value *arg_val = bytecode_emit_expression(builder, args[i]);
                bytecode_emit_instruction(builder, PUSH_ARG, arg_val, nullptr, nullptr);

                assert(arg_val->type->bit_size % 8 == 0);
                total_arg_size += arg_val->type->bit_size / 8;
            }

            auto arg_count_val =
                bytecode_integer_literal_new(builder, Builtin::type_u64,
                                             { .u64 = (uint64_t)args.count });

            auto result = bytecode_temporary_new(builder, expr->type);

            auto arg_size_val = bytecode_integer_literal_new(builder, Builtin::type_s64, 
                                                             { .s64 = total_arg_size });

            bytecode_emit_instruction(builder, SYSCALL, arg_count_val, arg_size_val, result);
            return result;
        }
        else if (name == Builtin::atom_cast) assert(false);
        else if (name == Builtin::atom_sizeof) assert(false);
        else if (name == Builtin::atom_offsetof) assert(false);
        else assert(false);

        assert(false);
    }

    void bytecode_emit_store(Bytecode_Builder *builder, Bytecode_Value *dest, Bytecode_Value *source)
    {
        switch (dest->kind)
        {
            case Bytecode_Value_Kind::INVALID: assert(false);
            case Bytecode_Value_Kind::TEMP: assert(false);
            case Bytecode_Value_Kind::INTEGER_LITERAL: assert(false);
            case Bytecode_Value_Kind::STRING_LITERAL: assert(false);

            case Bytecode_Value_Kind::ALLOCL:
            {
                assert(dest->type == source->type);

                bytecode_emit_instruction(builder, STOREL, dest, source, nullptr);
                break;
            }

            case Bytecode_Value_Kind::PARAM: assert(false);
            case Bytecode_Value_Kind::FUNCTION: assert(false);
        }
    }

    Bytecode_Value *bytecode_emit_load(Bytecode_Builder *builder, Bytecode_Value *source)
    {

        Bytecode_Value *result = bytecode_temporary_new(builder, source->type);

        switch (source->kind)
        {
            case Bytecode_Value_Kind::INVALID: assert(false);
            case Bytecode_Value_Kind::TEMP: assert(false);
            case Bytecode_Value_Kind::INTEGER_LITERAL: assert(false);
            case Bytecode_Value_Kind::STRING_LITERAL: assert(false);

            case Bytecode_Value_Kind::ALLOCL:
            {
                bytecode_emit_instruction(builder, LOADL, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::PARAM:
            {
                bytecode_emit_instruction(builder, LOAD_PARAM, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::FUNCTION: assert(false);

        }

        return result;
    }

    Bytecode_Instruction *bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Opcode op,
                                                    Bytecode_Value *a, Bytecode_Value *b,
                                                    Bytecode_Value *result)
    {
        auto inst = alloc_type<Bytecode_Instruction>(builder->allocator);

        inst->op = op;
        inst->a = a;
        inst->b = b;
        inst->result = result;

        assert(builder->insert_block);
        array_append(&builder->insert_block->instructions, inst);
        return inst;
    }

    Bytecode_Value *bytecode_find_parameter(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::PARAMETER);

        for (int64_t i = 0; i < builder->parameters.count; i++)
        {
            auto info = builder->parameters[i];
            if (info.declaration == decl)
            {
                assert(info.allocl_value->kind == Bytecode_Value_Kind::PARAM);
                return info.allocl_value;
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_find_variable(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);

        for (int64_t i = 0; i < builder->locals.count; i++)
        {
            auto info = builder->locals[i];
            if (info.declaration == decl)
            {
                assert(info.allocl_value->kind == Bytecode_Value_Kind::ALLOCL);
                return info.allocl_value;
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_value_new(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type)
    {
        assert(kind != Bytecode_Value_Kind::INVALID);

        auto result = alloc_type<Bytecode_Value>(builder->allocator);
        result->kind = kind;
        result->type = type;

        return result;
    }

    Bytecode_Value *bytecode_integer_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal)
    {
        assert(type->kind == AST_Type_Kind::INTEGER);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::INTEGER_LITERAL, type);
        result->integer_literal = integer_literal;
        return result;
    }

    Bytecode_Value *bytecode_string_literal_new(Bytecode_Builder *builder, Atom string_literal)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::STRING_LITERAL, 
                                         Builtin::type_ptr_u8);
        result->string_literal = string_literal;
        return result;
    }

    Bytecode_Value *bytecode_local_alloc_new(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::ALLOCL, type);
        result->allocl.name = name;

        array_append(&builder->current_function->locals, result);
        return result;
    }

    Bytecode_Value *bytecode_parameter_new(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::PARAM, type);
        result->allocl.name = name;

        array_append(&builder->current_function->parameters, result);

        return result;
    }

    Bytecode_Value *bytecode_temporary_new(Bytecode_Builder *builder, AST_Type *type)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::TEMP, type);

        result->temp.index = builder->next_temp_index;
        builder->next_temp_index += 1;

        array_append(&builder->current_function->temps, result);

        return result;
    }

    Bytecode_Value *bytecode_function_value_new(Bytecode_Builder *builder, Bytecode_Function *func)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::FUNCTION, func->type);
        result->function = func;
        return result;
    }

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder)
    {
        String_Builder sb = {};
        string_builder_init(allocator, &sb);

        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            bytecode_print_function(&sb, builder->functions[i].bc_func);
        }

        String str = string_builder_to_string(allocator, &sb);
        printf("%s\n", str.data);

        string_builder_free(&sb);
    }

    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func)
    {
        string_builder_appendf(sb, "%s(", func->name.data);

        for (int64_t i = 0; i < func->parameters.count; i++)
        {
            if (i > 0) string_builder_append(sb, ", ");
            auto param = func->parameters[i];
            bytecode_print_value(sb, param);
            string_builder_append(sb, " : ");
            ast_print_type(sb, param->type);
        }

        string_builder_append(sb, ")\n");

        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            bytecode_print_block(sb, func->blocks[i]);
        }

        string_builder_append(sb, "\n");
    }

    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block)
    {
        string_builder_appendf(sb, "  %s:\n", block->name);

        for (int64_t i = 0; i < block->instructions.count; i++)
        {
            bytecode_print_instruction(sb, block->instructions[i]);
        }
    }

    void bytecode_print_instruction(String_Builder *sb, Bytecode_Instruction *inst)
    {
        string_builder_append(sb, "    ");

        if (inst->result)
        {
            bytecode_print_value(sb, inst->result);
            string_builder_append(sb, " = ");
        }

        bool print_args = true;

        switch (inst->op)
        {
            case NOP: assert(false);
            case ALLOCL:
            {
                string_builder_append(sb, "ALLOCL ");
                ast_print_type(sb, inst->result->type);
                break;
            }

            case STOREL:
            {
                string_builder_append(sb, "STOREL ");
                break;
            }

            case LOADL:
            {
                string_builder_append(sb, "LOADL ");
                break;
            }

            case LOAD_PARAM:
            {
                string_builder_append(sb, "LOAD_PARAM ");
                break;
            }

            case ADD_S:
            {
                string_builder_append(sb, "ADD_S ");
                break;
            }

            case SUB_S:
            {
                string_builder_append(sb, "SUB_S ");
                break;
            }

            case PUSH_ARG:
            {
                string_builder_append(sb, "PUSH_ARG ");
                break;
            }

            case CALL:
            {
                print_args = false;
                string_builder_append(sb, "CALL ");
                bytecode_print_value(sb, inst->a);
                string_builder_append(sb, "()");
                string_builder_append(sb, ", ");
                bytecode_print_value(sb, inst->b);
                break;
            }

            case RETURN:
            {
                string_builder_append(sb, "RETURN ");
                break;
            }

            case EXIT:
            {
                string_builder_append(sb, "EXIT ");
                break;
            }

            case SYSCALL:
            {
                string_builder_append(sb, "SYSCALL ");
                break;
            }
        }

        if (print_args && inst->a)
        {
            bytecode_print_value(sb, inst->a);

            if (inst->b)
            {
                string_builder_append(sb, ", ");
                bytecode_print_value(sb, inst->b);
            }
        }
        else if (!inst->a)
        {
            assert(!inst->b);
        }

        string_builder_append(sb, "\n");
    }

    void bytecode_print_value(String_Builder *sb, Bytecode_Value *value)
    {
        switch (value->kind)
        {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::TEMP:
            {
                string_builder_appendf(sb, "%%%" PRId64, value->temp.index);
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL:
            {
                if (value->type->integer.sign)
                {
                    string_builder_appendf(sb, "%" PRId64, value->integer_literal.s64);
                }
                else
                {
                    string_builder_appendf(sb, "%" PRIu64, value->integer_literal.u64);
                }
                break;
            }

            case Bytecode_Value_Kind::STRING_LITERAL:
            {
                string_builder_append(sb, "\"");

                for (uint64_t i = 0; i < value->string_literal.length; i++)
                {
                    char c;
                    if (parser_make_escape_char(value->string_literal.data[i], &c))
                    {
                        string_builder_appendf(sb, "\\%c", c);
                    }
                    else
                    {
                        string_builder_appendf(sb, "%c", c);
                    }
                }

                string_builder_append(sb, "\"");

                break;
            }

            case Bytecode_Value_Kind::ALLOCL:
            case Bytecode_Value_Kind::PARAM:
            {
                string_builder_appendf(sb, "%%%s", value->allocl.name.data);
                break;
            }

            case Bytecode_Value_Kind::FUNCTION:
            {
                string_builder_appendf(sb, "%s", value->function->name.data);
                break;
            }
        }
    }
}
