#include "bytecode.h"

#include "builtin.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{

    void bytecode_builder_init(Allocator *allocator, Bytecode_Builder *builder)
    {
        assert(allocator);
        assert(builder);

        builder->allocator = allocator;

        array_init(allocator, &builder->functions);

        builder->insert_block = nullptr;
    }

    void bytecode_emit_node(Bytecode_Builder *builder, AST_Node *node)
    {
        switch (node->kind)
        {
            case AST_Node_Kind::INVALID: assert(false);
            case AST_Node_Kind::MODULE: assert(false);
            case AST_Node_Kind::IDENTIFIER: assert(false);

            case AST_Node_Kind::DECLARATION:
            {
                bytecode_emit_declaration(builder, static_cast<AST_Declaration*>(node));
                break;
            }

            case AST_Node_Kind::STATEMENT: assert(false);
            case AST_Node_Kind::EXPRESSION: assert(false);
            case AST_Node_Kind::TYPE_SPEC: assert(false);
            case AST_Node_Kind::TYPE: assert(false);
        }
    }

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        switch (decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);
            case AST_Declaration_Kind::VARIABLE: assert(false);
            case AST_Declaration_Kind::CONSTANT: assert(false);
            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                bytecode_emit_function_declaration(builder, decl);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }
    }

    void bytecode_emit_function_declaration(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(builder);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            if (builder->functions[i]->ast_decl == decl) assert(false);
        }

        Bytecode_Function *func = bytecode_new_function(builder, decl);        

        auto func_index = builder->functions.count;
        array_append(&builder->functions, func);
        func->index = func_index;
        builder->current_function = func;
        bytecode_builder_set_insert_point(builder, func);

        assert(decl->function.body);
        bytecode_emit_statement(builder, decl->function.body);
    }

    void bytecode_emit_statement(Bytecode_Builder *builder, AST_Statement *statement)
    {
        switch (statement->kind)
        {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK:
            {
                for (int64_t i = 0; i < statement->block.statements.count; i++)
                {
                    bytecode_emit_statement(builder, statement->block.statements[i]);
                }
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: assert(false);
            case AST_Statement_Kind::RETURN: assert(false);
            case AST_Statement_Kind::DECLARATION: assert(false);

            case AST_Statement_Kind::EXPRESSION:
            {
                bytecode_emit_expression(builder, statement->expression);
                break;
            }
        }
    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expression)
    {
        assert(builder);
        
        switch (expression->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);
            case AST_Expression_Kind::IDENTIFIER: assert(false);
            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);
            case AST_Expression_Kind::DOT: assert(false);
            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                return bytecode_emit_call_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::NUMBER_LITERAL:
            {
                return bytecode_emit_number_literal(builder, expression);
                break;     
            }

            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }
    }

    Bytecode_Value *bytecode_emit_call_expression(Bytecode_Builder *builder,
                                                  AST_Expression *expression)
    {
        assert(builder);
        assert(expression);
        assert(expression->kind == AST_Expression_Kind::CALL); 

        Bytecode_Value *return_value = nullptr;
        
        if (expression->call.is_builtin)
        {
            return_value = bytecode_emit_builtin_call_expression(builder, expression);
        }
        else
        {
            assert(expression->call.arg_expressions.count == 0);

            assert(expression->call.callee_declaration);
            Bytecode_Function *func =
                bytecode_find_function_for_decl(builder, expression->call.callee_declaration);

            assert(func);

            bytecode_emit_instruction(builder, Bytecode_Instruction::CALL);
            bytecode_emit_32(builder, func->index);
        }

        return return_value;
    }

    Bytecode_Value *bytecode_emit_builtin_call_expression(Bytecode_Builder *builder,
                                                          AST_Expression *expression)
    {
        assert(builder);
        assert(expression);
        assert(expression->kind == AST_Expression_Kind::CALL);
        assert(expression->call.is_builtin == true);

        auto ident_expr = expression->call.ident_expression;
        assert(ident_expr->kind == AST_Expression_Kind::IDENTIFIER);

        Bytecode_Value *return_value = nullptr;

        auto atom = ident_expr->identifier->atom;
        if (atom == Builtin::atom_exit)
        {
            assert(expression->call.arg_expressions.count == 1);
            auto arg_expr = expression->call.arg_expressions[0];

            assert(arg_expr->type->kind == AST_Type_Kind::INTEGER);
            auto exit_code_val = bytecode_emit_expression(builder, arg_expr);
            assert(exit_code_val);

            bytecode_emit_instruction(builder, Bytecode_Instruction::EXIT);
            bytecode_emit_local_temp(builder, exit_code_val->local_index);

            return_value = nullptr;
        }
        else assert(false);

        return return_value;
    }

    void bytecode_push_local_temporary(Bytecode_Builder *builder, Bytecode_Value *value)
    {
        assert(builder);
        assert(value);

        assert(builder->current_function);

        int64_t local_index = builder->current_function->local_values.count;

        assert(value->local_index == 0);

        array_append(&builder->current_function->local_values, value);
        value->local_index = local_index;
    }

    void bytecode_emit_local_temp(Bytecode_Builder *builder, uint32_t index)
    {
        bytecode_emit_32(builder, index);
    }

    void bytecode_emit_load_im(Bytecode_Builder *builder, bool sign, uint8_t size)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_IM);

        Bytecode_Size_Specifier size_spec = Bytecode_Size_Specifier::INVALID;

        switch (size)
        {
            case 8: size_spec = Bytecode_Size_Specifier::U8; break;
            case 16: size_spec = Bytecode_Size_Specifier::U16; break;
            case 32: size_spec = Bytecode_Size_Specifier::U32; break;
            case 64: size_spec = Bytecode_Size_Specifier::U64; break;
            default: assert(false);
        }

        if (sign)
        {
            size_spec = (Bytecode_Size_Specifier)(((uint8_t)size_spec) |
                                                  ((uint8_t)Bytecode_Size_Specifier::SIGN_FLAG));
        }

        bytecode_emit_byte(builder, (uint8_t)size_spec);
    }

    Bytecode_Value *bytecode_emit_number_literal(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::NUMBER_LITERAL);
        assert(expr->type->kind == AST_Type_Kind::INTEGER);

        bytecode_emit_load_im(builder, expr->type->integer.sign, expr->type->bit_size);
        switch (expr->type->bit_size)
        {
            case 64:
            {
                bytecode_emit_64(builder, expr->number_literal.s64);
                break;
            }

            default: assert(false);
        }

        auto result = bytecode_new_integer_literal(builder, expr->type);
        bytecode_push_local_temporary(builder, result);

        return result;
    }

    void bytecode_emit_instruction(Bytecode_Builder *builder, Bytecode_Instruction op)
    {
        bytecode_emit_byte(builder, (uint8_t)op);
    }

    uint64_t bytecode_emit_16(Bytecode_Builder *builder, uint16_t val)
    {
        auto offset = bytecode_emit_byte(builder, (val & 0x00ff));
        bytecode_emit_byte(builder,               (val & 0xFF00) >> 8);
        return offset;
    }

    uint64_t bytecode_emit_32(Bytecode_Builder *builder, uint32_t val)
    {
        auto offset = bytecode_emit_16(builder, (val & 0x0000FFFF));
        bytecode_emit_16(builder,               (val & 0xFFFF0000) >> 16);
        return offset;
    }

    uint64_t bytecode_emit_64(Bytecode_Builder *builder, uint64_t val)
    {
        auto offset = bytecode_emit_32(builder, (val & 0x00000000FFFFFFFF));
        bytecode_emit_32(builder,               (val & 0xFFFFFFFF00000000) >> 32);
        return offset;
    }

    uint64_t bytecode_emit_byte(Bytecode_Builder *builder, uint8_t byte)
    {
        assert(builder->insert_block);
        auto offset = builder->insert_block->instructions.count;
        array_append(&builder->insert_block->instructions, byte);
        return offset;
    }

    void bytecode_builder_set_insert_point(Bytecode_Builder *builder, Bytecode_Function *func)
    {
        assert(func->last_block);

        bytecode_builder_set_insert_point(builder, func->last_block);
    }

    void bytecode_builder_set_insert_point(Bytecode_Builder *builder, Bytecode_Block *block)
    {
        builder->insert_block = block;
    }

    void bytecode_builder_append_block(Bytecode_Builder *builder, Bytecode_Function *func,
                                       const char *name)
    {
        auto block = bytecode_new_block(builder->allocator, name);
        array_append(&func->blocks, block);

        func->last_block = block;
    }

    Bytecode_Function *bytecode_find_function_for_decl(Bytecode_Builder *builder,
                                                       AST_Declaration *decl)
    {
        assert(builder);

        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            if (builder->functions[i]->ast_decl == decl)
            {
                return builder->functions[i];
            }
        }

        assert(false);
    }

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto allocator = builder->allocator;

        auto result = alloc_type<Bytecode_Function>(allocator);
        array_init(allocator, &result->local_values);
        array_init(allocator, &result->blocks);
        
        bytecode_builder_append_block(builder, result, "entry");

        result->ast_decl = decl;        

        return result;
    }

    Bytecode_Block *bytecode_new_block(Allocator *allocator, const char *name)
    {
        Bytecode_Block *result = alloc_type<Bytecode_Block>(allocator);

        result->name = string_ref(name);
        array_init(allocator, &result->instructions);

        return result;
    }

    Bytecode_Value *bytecode_new_value(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type)
    {
        auto result = alloc_type<Bytecode_Value>(builder->allocator);
        result->kind = kind;
        result->type = type;
        result->local_index = 0;

        return result;
    }

    Bytecode_Value *bytecode_new_integer_literal(Bytecode_Builder *builder, AST_Type *type)
    {
        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::NUMBER_LITERAL, type);

        return result;
    }

    void bytecode_print(Allocator *allocator, Bytecode_Builder *builder)
    {
        assert(allocator);
        assert(builder);

        String_Builder sb = {};
        string_builder_init(allocator, &sb);

        bytecode_print(&sb, builder);

        auto str = string_builder_to_string(allocator, &sb);
        printf("\n%s\n", str.data);
        free(allocator, str.data);

        string_builder_free(&sb);
    }

    void bytecode_print(String_Builder *sb, Bytecode_Builder *builder)
    {
        assert(sb);
        assert(builder);

        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            auto func = builder->functions[i];
            bytecode_print_function(sb, func);
            string_builder_append(sb, "\n");
        }
    }

    void bytecode_print_function(String_Builder *sb, Bytecode_Function *func)
    {
        assert(sb);
        assert(func);

        string_builder_append(sb, func->ast_decl->identifier->atom.data);
        string_builder_append(sb, ":\n");

        int64_t local_index = 0;

        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            bytecode_print_block(sb, func->blocks[i], &local_index);
        } 
    }

    void bytecode_print_block(String_Builder *sb, Bytecode_Block *block, int64_t *local_indexp)
    {
        string_builder_append(sb, "  ");
        string_builder_append(sb, block->name.data);
        string_builder_append(sb, ":\n");

        int64_t ip = 0;
        while (ip < block->instructions.count)
        {
            Bytecode_Instruction inst = (Bytecode_Instruction)block->instructions[ip];
            ip++;
            bytecode_print_instruction(sb, block, inst, &ip, local_indexp);
        }        
    }

    void bytecode_print_instruction(String_Builder *sb, Bytecode_Block *block, 
                                    Bytecode_Instruction inst, int64_t *ipp, int64_t *local_indexp)
    {
        string_builder_append(sb, "    ");

        switch (inst)
        {
            case Bytecode_Instruction::NOP:
            {
                string_builder_append(sb, "NOP");
                break;
            }
            case Bytecode_Instruction::EXIT:
            {
                uint32_t temp_index = *((uint32_t*)(&block->instructions[*ipp]));
                *ipp += 4;
                string_builder_appendf(sb, "EXIT %%%" PRIu32, temp_index);
                break;
            }
            case Bytecode_Instruction::CALL:
            {

                uint32_t func_index = *((uint32_t*)(&block->instructions[*ipp]));
                *ipp += 4;
                assert(false); // Use index to get function name
                string_builder_appendf(sb, "CALL %" PRIu32, func_index);
                break;
            }
            case Bytecode_Instruction::LOAD_IM:
            {
                string_builder_appendf(sb, "%%%" PRId64, *local_indexp);
                *local_indexp += 1;


                string_builder_append(sb, " = ");
                bytecode_print_im(sb, block, ipp);
                break;
            }
        }

        string_builder_append(sb, "\n");
    }

    void bytecode_print_im(String_Builder *sb, Bytecode_Block *block, int64_t *ipp)
    {
        assert(sb);

        auto size_spec = (Bytecode_Size_Specifier)block->instructions[*ipp];
        *ipp += 1;

        switch (size_spec)
        {
            case Bytecode_Size_Specifier::INVALID: assert(false);
            case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
            case Bytecode_Size_Specifier::U8: assert(false);
            case Bytecode_Size_Specifier::S8: assert(false);
            case Bytecode_Size_Specifier::U16: assert(false);
            case Bytecode_Size_Specifier::S16: assert(false);
            case Bytecode_Size_Specifier::U32: assert(false);
            case Bytecode_Size_Specifier::S32: assert(false);
            case Bytecode_Size_Specifier::U64: assert(false);
            case Bytecode_Size_Specifier::S64:
            {
                int64_t val = *((int64_t*)(&block->instructions[*ipp]));
                string_builder_appendf(sb, "%" PRId64, val);
                *ipp += 8;
                break;
            }
            default: assert(false);
        }
    }
}
