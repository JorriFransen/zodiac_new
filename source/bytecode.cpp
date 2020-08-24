#include "bytecode.h"

#include "builtin.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{

    void bytecode_builder_init(Allocator *allocator, Bytecode_Builder *builder, Build_Data *bd)
    {
        assert(allocator);
        assert(builder);
        assert(bd);

        builder->allocator = allocator;
        builder->build_data = bd;

        array_init(allocator, &builder->program.functions);
        builder->program.entry_function = nullptr;
        builder->program.bytecode_entry_function = nullptr;

        array_init(allocator, &builder->emitted_types);

        builder->insert_block = nullptr;
        builder->current_function = nullptr;
    }

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        switch (decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                if (decl->variable.init_expression)
                {
                    auto init_val = bytecode_emit_expression(builder,
                                                             decl->variable.init_expression);
                    assert(init_val);

                    auto allocl_val = bytecode_find_value_for_variable(builder, decl);
                    assert(allocl_val);
                    bytecode_emit_storel(builder, allocl_val, init_val);
                }
                break;
            }

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

    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl)
    {
        assert(builder);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        for (int64_t i = 0; i < builder->program.functions.count; i++)
        {
            if (builder->program.functions[i]->ast_decl == decl) assert(false);
        }

        Bytecode_Function *func = bytecode_new_function(builder, decl);        

        if (decl->decl_flags & AST_DECL_FLAG_IS_ENTRY)
        {
            //assert(decl->decl_flags & AST_DECL_FLAG_IS_NAKED);
            assert(!builder->program.entry_function);
            builder->program.entry_function = func;
        }
        if (decl->decl_flags & AST_DECL_FLAG_IS_BYTECODE_ENTRY)
        {
            assert(!builder->program.bytecode_entry_function);
            builder->program.bytecode_entry_function = func;
        }

        auto func_index = builder->program.functions.count;
        array_append(&builder->program.functions, func);
        func->index = func_index;

        if (decl->decl_flags & AST_DECL_FLAG_FOREIGN)
        {
            func->flags |= BYTECODE_FUNC_FLAG_FOREIGN;
            //assert(false);
        }
        else
        {
            builder->current_function = func;
            bytecode_builder_append_block(builder, func, "entry");
            bytecode_builder_set_insert_point(builder, func);

            for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
            {
                auto param = decl->function.parameter_declarations[i];
                auto param_val = bytecode_new_value(builder, Bytecode_Value_Kind::PARAMETER, 
                                                    param->type);
                array_append(&func->parameters, { param_val, param });
                param_val->param_index = i;
                param_val->name = param->identifier->atom;
            }

            for (int64_t i = 0; i < decl->function.variable_declarations.count; i++)
            {
                auto var = decl->function.variable_declarations[i];
                bytecode_emit_allocl(builder, var, var->identifier->atom);
            }

            assert(decl->function.body);
            bytecode_emit_statement(builder, decl->function.body);
        }

        return func;
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

            case AST_Statement_Kind::ASSIGNMENT:
            {
                auto rhs_value = bytecode_emit_expression(builder,
                                                          statement->assignment.rhs_expression);
                assert(rhs_value);
                assert(rhs_value->kind == Bytecode_Value_Kind::TEMPORARY);

                auto lvalue = bytecode_emit_lvalue(builder,
                                                   statement->assignment.identifier_expression);
                assert(lvalue);
                if (lvalue->kind == Bytecode_Value_Kind::ALLOCL)
                {
                    assert(lvalue->type == rhs_value->type);
                    bytecode_emit_storel(builder, lvalue, rhs_value);
                }
                else if (lvalue->kind == Bytecode_Value_Kind::TEMPORARY &&
                         lvalue->type->kind == AST_Type_Kind::POINTER &&
                         lvalue->type->pointer.base == rhs_value->type)
                {
                    bytecode_emit_storep(builder, lvalue, rhs_value);
                }
                else assert(false);

                break;
            }

            case AST_Statement_Kind::RETURN:
            {
                if (statement->expression)
                {
                    auto ret_val = bytecode_emit_expression(builder, statement->expression);
                    bytecode_emit_return_statement(builder, ret_val);
                }
                else
                {
                    bytecode_emit_return_statement(builder, nullptr);
                }
                break;
            }

            case AST_Statement_Kind::DECLARATION:
            {
                bytecode_emit_declaration(builder, statement->declaration);
                break;
            }

            case AST_Statement_Kind::EXPRESSION:
            {
                bytecode_emit_expression(builder, statement->expression);
                break;
            }
        }
    }

    void bytecode_emit_return_statement(Bytecode_Builder *builder, Bytecode_Value *ret_val)
    {

        if (ret_val)
        {
            assert(ret_val->kind == Bytecode_Value_Kind::TEMPORARY);
            bytecode_emit_instruction(builder, Bytecode_Instruction::RET);
            bytecode_emit_32(builder, ret_val->local_index);
        } 
        else
        {
            bytecode_emit_instruction(builder, Bytecode_Instruction::RET_VOID);
        }
    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expression)
    {
        assert(builder);
        
        switch (expression->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                return bytecode_emit_identifier(builder, expression->identifier);
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                auto lvalue = bytecode_emit_lvalue(builder, expression->dot.parent_expression);
                assert(lvalue);
                assert(lvalue->kind == Bytecode_Value_Kind::ALLOCL ||
                       lvalue->kind == Bytecode_Value_Kind::PARAMETER);

                auto index = expression->dot.child_index;
                assert(index >= 0);

                Bytecode_Value *ptr = nullptr;

                if (lvalue->type->kind == AST_Type_Kind::STRUCTURE)
                {
                    ptr = bytecode_emit_offset_pointer(builder, lvalue, index);
                }
                else
                {
                    assert(lvalue->type->kind == AST_Type_Kind::POINTER);
                    assert(lvalue->type->pointer.base->kind == AST_Type_Kind::STRUCTURE);

                    lvalue = bytecode_emit_load(builder, lvalue);
                    ptr = bytecode_emit_offset_pointer(builder, lvalue, index);
                }

                assert(ptr);

                auto value = bytecode_emit_loadp(builder, ptr);
                assert(value);
                return value;

                break;
            }

            case AST_Expression_Kind::BINARY:
            {
                return bytecode_emit_binary_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::UNARY: assert(false);

            case AST_Expression_Kind::CALL:
            {
                return bytecode_emit_call_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::ADDROF:
            {
                auto result = bytecode_emit_lvalue(builder, expression->addrof.operand_expr);
                switch (result->kind)
                {
                    case Bytecode_Value_Kind::ALLOCL:
                    {
                        return bytecode_emit_addrof(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                return result;
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

        assert(false);
        return nullptr;
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
            assert(expression->call.callee_declaration);
            Bytecode_Function *func =
                bytecode_find_function_for_decl(builder, expression->call.callee_declaration);

            assert(func);

            for (int64_t i = 0; i < expression->call.arg_expressions.count; i++)
            {
                bytecode_emit_call_arg(builder, expression->call.arg_expressions[i]);
            }

            bytecode_emit_instruction(builder, Bytecode_Instruction::CALL);
            bytecode_emit_32(builder, func->index);
            bytecode_emit_32(builder, expression->call.arg_expressions.count);

            auto ret_type = func->ast_decl->type->function.return_type;
            if (ret_type != Builtin::type_void)
            {
                return_value = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                                  ret_type);
                bytecode_push_local_temporary(builder, return_value);
            }
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
            bytecode_emit_32(builder, exit_code_val->local_index);

            return_value = nullptr;
        }
        else assert(false);

        return return_value;
    }

    Bytecode_Value *bytecode_emit_binary_expression(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(builder);
        assert(expr);
        assert(expr->kind == AST_Expression_Kind::BINARY);

        auto lhs = expr->binary.lhs;
        auto rhs = expr->binary.rhs;

        assert(lhs->type == rhs->type);
        assert(lhs->type->kind == AST_Type_Kind::INTEGER);

        auto lhs_val = bytecode_emit_expression(builder, lhs);
        assert(lhs_val);
        auto rhs_val = bytecode_emit_expression(builder, rhs);
        assert(rhs_val);

        switch (expr->binary.op)
        {
            case BINOP_INVALID: assert(false);

            case BINOP_ADD: 
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::ADD);
                break;
            }

            case BINOP_SUB: assert(false);
        }    

        auto type = lhs->type;
        bytecode_emit_size_spec(builder, type->integer.sign, type->bit_size);

        bytecode_emit_32(builder, lhs_val->local_index);
        bytecode_emit_32(builder, rhs_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_identifier(Bytecode_Builder *builder, AST_Identifier *ident)
    {
        assert(builder);
        assert(ident);
        assert(ident->declaration);

        auto decl = ident->declaration;
        Bytecode_Value *decl_val = nullptr;
        
        switch (ident->declaration->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::VARIABLE: 
            {
                decl_val = bytecode_find_value_for_variable(builder, decl);
                break;
            }

            case AST_Declaration_Kind::CONSTANT: assert(false);

            case AST_Declaration_Kind::PARAMETER:
            {
                decl_val = bytecode_find_value_for_parameter(builder, decl);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::FUNCTION: assert(false);
            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);
        }

        assert(decl_val);

        switch (decl_val->kind)
        {
            case Bytecode_Value_Kind::INVALID: assert(false);
            case Bytecode_Value_Kind::NUMBER_LITERAL: assert(false);
            case Bytecode_Value_Kind::TEMPORARY: assert(false);

            case Bytecode_Value_Kind::ALLOCL:
            {
                auto result = bytecode_emit_loadl(builder, decl_val);
                assert(result);
                return result;
                break;
            }

            case Bytecode_Value_Kind::PARAMETER:
            {
                return bytecode_emit_load_param(builder, decl_val);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_lvalue(Bytecode_Builder *builder, AST_Expression *lvalue_expr)
    {
        assert(builder);

        switch (lvalue_expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER:
            {
                auto ident = lvalue_expr->identifier;
                assert(ident->declaration);
                auto idecl = ident->declaration;

                switch (idecl->kind)
                {
                    case AST_Declaration_Kind::VARIABLE:
                    {
                        auto result = bytecode_find_value_for_variable(builder, idecl);
                        assert(result);
                        assert(result->kind == Bytecode_Value_Kind::ALLOCL);
                        return result;
                        break;
                    }

                    case AST_Declaration_Kind::PARAMETER:
                    {
                        auto result = bytecode_find_value_for_parameter(builder, idecl);
                        assert(result);
                        assert(result->kind == Bytecode_Value_Kind::PARAMETER);
                        return result;
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT:
            {
                auto parent_lvalue = bytecode_emit_lvalue(builder,
                                                          lvalue_expr->dot.parent_expression);
                assert(parent_lvalue);
                assert(parent_lvalue->kind == Bytecode_Value_Kind::ALLOCL ||
                       parent_lvalue->kind == Bytecode_Value_Kind::PARAMETER);

                assert(lvalue_expr->dot.child_index >= 0);

                auto pointer_val = bytecode_emit_offset_pointer(builder, parent_lvalue,
                                                                lvalue_expr->dot.child_index);
                return pointer_val;
                break;
            }

            case AST_Expression_Kind::BINARY: assert(false);
            case AST_Expression_Kind::UNARY: assert(false);
            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);
            case AST_Expression_Kind::NUMBER_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_addrof(Bytecode_Builder *builder, Bytecode_Value *lvalue)
    {
        assert(lvalue->kind == Bytecode_Value_Kind::ALLOCL);

        bytecode_emit_instruction(builder, Bytecode_Instruction::ADDROF);
        bytecode_emit_32(builder, lvalue->alloc_index);

        auto pointer_type = ast_find_or_create_pointer_type(builder->allocator, lvalue->type);
        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, pointer_type);
        bytecode_push_local_temporary(builder, result);

        return result;

    }

    Bytecode_Value *bytecode_emit_allocl(Bytecode_Builder *builder, AST_Declaration *decl,
                                         Atom name)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::ALLOCL);
        bytecode_emit_type_index(builder, decl->type);

        Bytecode_Value *result = bytecode_new_value(builder, Bytecode_Value_Kind::ALLOCL,
                                                    decl->type);
        bytecode_push_local_alloc(builder, result, decl);
        result->name = name;

        return result;
    }

    void bytecode_emit_call_arg(Bytecode_Builder *builder, AST_Expression *arg_expr)
    {
        auto arg_val = bytecode_emit_expression(builder, arg_expr);
        bytecode_emit_instruction(builder, Bytecode_Instruction::PUSH_ARG);
        bytecode_emit_32(builder, arg_val->local_index);
    }

    void bytecode_push_local_temporary(Bytecode_Builder *builder, Bytecode_Value *value)
    {
        assert(builder);
        assert(value);

        assert(builder->current_function);

        int64_t local_index = builder->current_function->local_temps.count;

        assert(value->local_index == 0);

        array_append(&builder->current_function->local_temps, value);
        value->local_index = local_index;
    }

    void bytecode_push_local_alloc(Bytecode_Builder *builder, Bytecode_Value *value,
                                   AST_Declaration *decl)
    {
        assert(value->kind == Bytecode_Value_Kind::ALLOCL);

        assert(builder->current_function);

        int64_t alloc_index = builder->current_function->local_allocs.count;

        assert(value->alloc_index == 0);

        array_append(&builder->current_function->local_allocs, { value, decl });
        value->alloc_index = alloc_index;
    }

    void bytecode_emit_size_spec(Bytecode_Builder *builder, bool sign, uint8_t size)
    {
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

    Bytecode_Value *bytecode_emit_load(Bytecode_Builder *builder, Bytecode_Value *lvalue)
    {
        switch (lvalue->kind)
        {
            case Bytecode_Value_Kind::ALLOCL:
            {
                return bytecode_emit_loadl(builder, lvalue);
            }

            case Bytecode_Value_Kind::PARAMETER:
            {
                return bytecode_emit_load_param(builder, lvalue);
                break;
            }

            case Bytecode_Value_Kind::TEMPORARY:
            {
                assert(false);
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void bytecode_emit_load_im(Bytecode_Builder *builder, bool sign, uint8_t size)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_IM);
        bytecode_emit_size_spec(builder, sign, size);
    }

    Bytecode_Value *bytecode_emit_loadl(Bytecode_Builder *builder, Bytecode_Value *allocl)
    {
        assert(allocl->kind == Bytecode_Value_Kind::ALLOCL);
        assert(allocl->type);
        assert(allocl->type->kind == AST_Type_Kind::INTEGER ||
               allocl->type->kind == AST_Type_Kind::STRUCTURE);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOADL);
        bytecode_emit_32(builder, allocl->alloc_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         allocl->type);
        bytecode_push_local_temporary(builder, result);

        return result;
    }

    Bytecode_Value *bytecode_emit_loadp(Bytecode_Builder *builder, Bytecode_Value *ptr)
    {
        assert(ptr->kind == Bytecode_Value_Kind::TEMPORARY);
        assert(ptr->type->kind == AST_Type_Kind::POINTER);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOADP);
        bytecode_emit_32(builder, ptr->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         ptr->type->pointer.base);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_load_param(Bytecode_Builder *builder, Bytecode_Value *param)
    {
        assert(param->kind == Bytecode_Value_Kind::PARAMETER);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_PARAM);
        bytecode_emit_32(builder, param->param_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         param->type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    void bytecode_emit_storel(Bytecode_Builder *builder, Bytecode_Value *dest,
                              Bytecode_Value *value)
    {
        assert(dest->kind == Bytecode_Value_Kind::ALLOCL);
        assert(value->kind == Bytecode_Value_Kind::TEMPORARY);

        bytecode_emit_instruction(builder, Bytecode_Instruction::STOREL);
        bytecode_emit_32(builder, dest->alloc_index);
        bytecode_emit_32(builder, value->local_index);
    }

    void bytecode_emit_storep(Bytecode_Builder *builder, Bytecode_Value *dest,
                              Bytecode_Value *value)
    {
        assert(dest->kind == Bytecode_Value_Kind::TEMPORARY);
        assert(dest->type->kind == AST_Type_Kind::POINTER);
        assert(value->kind == Bytecode_Value_Kind::TEMPORARY);
        assert(value->type == dest->type->pointer.base);

        bytecode_emit_instruction(builder, Bytecode_Instruction::STOREP);
        bytecode_emit_32(builder, dest->local_index);
        bytecode_emit_32(builder, value->local_index);

    }

    Bytecode_Value *bytecode_emit_offset_pointer(Bytecode_Builder *builder,
                                                 Bytecode_Value *lvalue, int64_t index)
    {
        assert(builder);
        assert(lvalue);
        assert(lvalue->kind == Bytecode_Value_Kind::ALLOCL ||
               lvalue->kind == Bytecode_Value_Kind::PARAMETER ||
               (lvalue->kind == Bytecode_Value_Kind::TEMPORARY &&
                lvalue->type->kind == AST_Type_Kind::POINTER &&
                lvalue->type->pointer.base->kind == AST_Type_Kind::STRUCTURE));
        assert(index >= 0);

        auto lval_type = lvalue->type;
        AST_Type *struct_type = nullptr;
        if (lval_type->kind == AST_Type_Kind::STRUCTURE)
        {
            struct_type = lval_type;
        }
        else if (lval_type->kind == AST_Type_Kind::POINTER)
        {
            assert(lval_type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
            struct_type = lval_type->pointer.base;
        }

        assert(struct_type);

        assert(struct_type->structure.member_types.count > index);
        auto mem_type = struct_type->structure.member_types[index];

        bytecode_emit_instruction(builder, Bytecode_Instruction::OFFSET_PTR);
        switch (lvalue->kind)
        {
            default: assert(false);

            case Bytecode_Value_Kind::ALLOCL:
            {
                bytecode_emit_byte(builder, (uint8_t)Bytecode_Value_Type_Specifier::ALLOCL);
                bytecode_emit_32(builder, lvalue->alloc_index);
                break;
            }

            case Bytecode_Value_Kind::PARAMETER:
            {
                bytecode_emit_byte(builder, (uint8_t)Bytecode_Value_Type_Specifier::PARAMETER);
                bytecode_emit_32(builder, lvalue->param_index);
                break;
            }

            case Bytecode_Value_Kind::TEMPORARY:
            {
                bytecode_emit_byte(builder, (uint8_t)Bytecode_Value_Type_Specifier::TEMPORARY);
                bytecode_emit_32(builder, lvalue->local_index);
                break;
            }
        }

        bytecode_emit_32(builder, index);

        auto mem_ptr_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                   builder->build_data,
                                                                   mem_type);
        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, mem_ptr_type);
        bytecode_push_local_temporary(builder, result);
        return result;
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

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, expr->type);
        bytecode_push_local_temporary(builder, result);

        return result;
    }

    void bytecode_emit_type_index(Bytecode_Builder *builder, AST_Type *type)
    {
        assert(builder);
        assert(type);

        int64_t index;
        bool found = false;

        for (int64_t i = 0; i < builder->emitted_types.count; i++)
        {
            auto emitted_type = builder->emitted_types[i];
            if (emitted_type == type)
            {
                index = i;
                found = true;
                break;
            }
        }

        if (!found)
        {
            index = builder->emitted_types.count;
            array_append(&builder->emitted_types, type);
        }

        assert(index >= 0);
        bytecode_emit_32(builder, (uint32_t)index);
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

        for (int64_t i = 0; i < builder->program.functions.count; i++)
        {
            if (builder->program.functions[i]->ast_decl == decl)
            {
                return builder->program.functions[i];
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_find_value_for_parameter(Bytecode_Builder *builder,
                                                      AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::PARAMETER);

        assert(builder->current_function);
        auto func = builder->current_function;

        for (int64_t i = 0; i < func->parameters.count; i++)
        {
            if (func->parameters[i].ast_decl == decl)
            {
                return func->parameters[i].value;
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_find_value_for_variable(Bytecode_Builder *builder,
                                                     AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);

        assert(builder->current_function);
        auto func = builder->current_function;

        for (int64_t i = 0; i < func->local_allocs.count; i++)
        {
            if (func->local_allocs[i].ast_decl == decl)
            {
                return func->local_allocs[i].value;
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Function *bytecode_new_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto allocator = builder->allocator;

        auto result = alloc_type<Bytecode_Function>(allocator);
        array_init(allocator, &result->parameters);
        array_init(allocator, &result->local_temps);
        array_init(allocator, &result->local_allocs);
        array_init(allocator, &result->blocks);
        
        result->ast_decl = decl;        

        if (decl->decl_flags & AST_DECL_FLAG_IS_NAKED)
            result->flags |= BYTECODE_FUNC_FLAG_NAKED;

        if (decl->decl_flags & AST_DECL_FLAG_NORETURN)
            result->flags |= BYTECODE_FUNC_FLAG_NORETURN;

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

    Bytecode_Iterator bytecode_iterator_create(Bytecode_Builder *builder)
    {
        assert(builder);

        Bytecode_Iterator result = {};
        result.builder = builder;

        stack_init(builder->allocator, &result.arg_stack);
        
        assert(builder->program.functions.count >= 1);
        result.function_index = 0;

        assert(builder->program.functions[0]->blocks.count >= 0);
        result.block_index = 0;

        //assert(builder->program.functions[0]->blocks[0]->instructions.count >= 0);
        result.instruction_index = 0;

        result.local_temp_index = 0;
        result.local_alloc_index = 0;

        return result;
    }

    void bytecode_iterator_free(Bytecode_Iterator *bci)
    {
        stack_free(&bci->arg_stack);
    }

    void bytecode_iterator_advance_function(Bytecode_Iterator *bci)
    {
        assert(bci);
        assert(bci->builder);

        bci->arg_stack.sp = 0;

        bci->function_index++;
        bci->block_index = 0;
        bci->instruction_index = 0;

        bci->local_temp_index = 0;
        bci->local_alloc_index = 0;
        //assert(bci->function_index < bci->builder->functions.count);
    }

    Bytecode_Function *bytecode_iterator_get_function(Bytecode_Iterator *bci)
    {
        assert(bci->function_index < bci->builder->program.functions.count);
        return bci->builder->program.functions[bci->function_index];
    }

    void bytecode_iterator_advance_block(Bytecode_Iterator *bci)
    {
        bci->block_index++;
        bci->instruction_index = 0;
    }

    Bytecode_Block *bytecode_iterator_get_block(Bytecode_Iterator *bci)
    {
        auto func = bytecode_iterator_get_function(bci);
        assert(func);

        assert(bci->block_index < func->blocks.count);
        return func->blocks[bci->block_index];
    }

    void bytecode_iterator_advance_ip(Bytecode_Iterator *bci, int64_t adv /*=1*/)
    {
        assert(adv >= 1);

        bci->instruction_index += adv;

#if DEBUG
        auto block = bytecode_iterator_get_block(bci);
        assert(bci->instruction_index <= block->instructions.count);
#endif
    }

    Bytecode_Instruction bytecode_iterator_get_ip(Bytecode_Iterator *bci)
    {
        return (Bytecode_Instruction)bytecode_iterator_fetch_byte(bci);
    }

    uint8_t bytecode_iterator_fetch_byte(Bytecode_Iterator *bci)
    {
        auto block = bytecode_iterator_get_block(bci);

        assert(bci->instruction_index < block->instructions.count);

        return block->instructions[bci->instruction_index];
    }

    uint32_t bytecode_iterator_fetch_32(Bytecode_Iterator *bci)
    {
        auto block = bytecode_iterator_get_block(bci);
        uint32_t result = *((uint32_t*)(&block->instructions[bci->instruction_index]));

        bytecode_iterator_advance_ip(bci, 4);

        return result;
    }

    uint64_t bytecode_iterator_fetch_64(Bytecode_Iterator *bci)
    {
        auto block = bytecode_iterator_get_block(bci);
        uint64_t result = *((uint64_t*)(&block->instructions[bci->instruction_index]));

        bytecode_iterator_advance_ip(bci, 8);

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

        auto bci = bytecode_iterator_create(builder);

        while (bci.function_index < builder->program.functions.count)
        {
            bytecode_print_function(sb, &bci);
            string_builder_append(sb, "\n");

            bytecode_iterator_advance_function(&bci);
        }

        bytecode_iterator_free(&bci);
    }

    void bytecode_print_function(String_Builder *sb, Bytecode_Iterator *bci)
    {
        auto func = bytecode_iterator_get_function(bci);

        bool is_foreign = func->flags & BYTECODE_FUNC_FLAG_FOREIGN;
        if (is_foreign)
        {
            string_builder_append(sb, "#foreign ");
        }

        string_builder_appendf(sb, "%s(", func->ast_decl->identifier->atom.data);

        for (int64_t i = 0; i < func->ast_decl->function.parameter_declarations.count; i++)
        {
            if (i) string_builder_append(sb, ", ");
            auto param_decl =  func->ast_decl->function.parameter_declarations[i];
            string_builder_appendf(sb, "%%%s", param_decl->identifier->atom.data);
        }

        string_builder_append(sb, ")");

        if (!is_foreign)
        {
            string_builder_append(sb, ":\n");
            while (bci->block_index < func->blocks.count)
            {
                bytecode_print_block(sb, bci);
                bytecode_iterator_advance_block(bci);
            }
        }
        else string_builder_append(sb, "\n");
    }

    void bytecode_print_block(String_Builder *sb, Bytecode_Iterator *bci)
    {
        auto block = bytecode_iterator_get_block(bci);

        string_builder_append(sb, "  ");
        string_builder_append(sb, block->name.data);
        string_builder_append(sb, ":\n");

        while (bci->instruction_index < block->instructions.count)
        {
            bytecode_print_instruction(sb, bci);
        }
    }

    void bytecode_print_instruction(String_Builder *sb, Bytecode_Iterator *bci)
    {
        auto inst = bytecode_iterator_get_ip(bci);
        bytecode_iterator_advance_ip(bci);
        
        if (inst != Bytecode_Instruction::PUSH_ARG) string_builder_append(sb, "    ");

        bool newline = true;

        switch (inst)
        {
            case Bytecode_Instruction::NOP:
            {
                string_builder_append(sb, "NOP");
                break;
            }

            case Bytecode_Instruction::EXIT:
            {
                uint32_t temp_index = bytecode_iterator_fetch_32(bci);
                string_builder_appendf(sb, "EXIT %%%" PRIu32, temp_index);
                break;
            }

            case Bytecode_Instruction::CALL:
            {

                uint32_t func_index = bytecode_iterator_fetch_32(bci);
                uint32_t arg_count = bytecode_iterator_fetch_32(bci);
                auto func = bci->builder->program.functions[func_index];

                auto func_decl = func->ast_decl;
                auto func_name = func_decl->identifier->atom.data;
                auto ret_type = func_decl->type->function.return_type;

                if (ret_type != Builtin::type_void)
                {
                    string_builder_appendf(sb, "%%%" PRId64 " = ", bci->local_temp_index);
                    bci->local_temp_index += 1;
                }

                string_builder_appendf(sb, "CALL %s(", func_name);
                for (uint64_t i = 0; i < arg_count; i++)
                {
                    if (i) string_builder_append(sb, ", ");
                    auto arg_val = stack_peek(&bci->arg_stack, (arg_count - 1) - i);

                    assert(arg_val->kind == Bytecode_Value_Kind::TEMPORARY);
                    string_builder_appendf(sb, "%%%" PRIu32, arg_val->local_index);
                }
                string_builder_append(sb, ")");

                for (uint64_t i = 0; i < arg_count; i++)
                {
                    stack_pop(&bci->arg_stack);
                }

                break;
            }

            case Bytecode_Instruction::RET:
            {
                uint32_t temp_index = bytecode_iterator_fetch_32(bci);
                string_builder_appendf(sb, "RETURN %%%" PRIu32, temp_index);
                break;
            }

            case Bytecode_Instruction::RET_VOID:
            {
                string_builder_append(sb, "RETURN void");
                break;
            }

            case Bytecode_Instruction::ALLOCL:
            {
                uint32_t type_index = bytecode_iterator_fetch_32(bci);
                auto type = bci->builder->emitted_types[type_index];
                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->local_allocs[bci->local_alloc_index++].value->name;
                string_builder_appendf(sb, "%%%s = ALLOCL ", name.data);
                ast_print_type(sb, type);
                break;
            }

            case Bytecode_Instruction::LOAD_IM:
            {
                string_builder_appendf(sb, "%%%" PRId64, bci->local_temp_index);
                bci->local_temp_index += 1;


                string_builder_append(sb, " = LOAD_IM ");
                bytecode_print_im(sb, bci);
                break;
            }

            case Bytecode_Instruction::LOADL:
            {
                uint32_t allocl_index = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->local_allocs[allocl_index].value->name;

                string_builder_appendf(sb, "%%%" PRId64 " = LOADL %%%s", bci->local_temp_index,
                                       name.data);
                bci->local_temp_index += 1;
                break; 
            }

            case Bytecode_Instruction::LOADP:
            {
                uint32_t val_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = LOADP %%%" PRIu32,
                                       bci->local_temp_index, val_idx);
                bci->local_temp_index += 1;
                break;
            }

            case Bytecode_Instruction::LOAD_PARAM:
            {
                uint32_t param_index = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->parameters[param_index].value->name;

                string_builder_appendf(sb, "%%%" PRId64 " = LOAD_PARAM %%%s",
                                       bci->local_temp_index, name.data);
                bci->local_temp_index += 1;
                break; 
            }

            case Bytecode_Instruction::STOREL:
            {
                uint32_t dest_index = bytecode_iterator_fetch_32(bci);
                uint32_t val_index = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->local_allocs[dest_index].value->name;

                string_builder_appendf(sb, "STOREL %%%s %%%" PRIu32, name.data, val_index);
                break;
            }

            case Bytecode_Instruction::STOREP:
            {
                uint32_t dest_idx = bytecode_iterator_fetch_32(bci);
                uint32_t value_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "STOREP %%%" PRIu32 " %%% " PRIu32,
                                       dest_idx, value_idx);
                break;
            }

            case Bytecode_Instruction::ADDROF:
            {
                auto alloc_idx = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->local_allocs[alloc_idx].value->name;

                string_builder_appendf(sb, "%%%" PRIu64 " = ADDROF %%%s",
                                       bci->local_temp_index++, name.data);
                break;
            }

            case Bytecode_Instruction::PUSH_ARG:
            {
                uint32_t val_index = bytecode_iterator_fetch_32(bci);
                auto func = bci->builder->program.functions[bci->function_index];
                stack_push(&bci->arg_stack, func->local_temps[val_index]);
                newline = false;
                break;
            }

            case Bytecode_Instruction::ADD:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);

                uint32_t lhs_idx = bytecode_iterator_fetch_32(bci);
                uint32_t rhs_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRIu64 " = ADD ", bci->local_temp_index);
                bytecode_print_size_spec(sb, size_spec);
                string_builder_appendf(sb, " %%%" PRIu32 " %%%" PRIu32, lhs_idx, rhs_idx);
                bci->local_temp_index += 1;
                break;
            }

            case Bytecode_Instruction::OFFSET_PTR:
            {
                auto kind = (Bytecode_Value_Type_Specifier)bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);

                auto store_idx = bytecode_iterator_fetch_32(bci);
                auto offset = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];

                string_builder_appendf(sb, "%%%" PRIu64 " = OFFSET_PTR ",
                                       bci->local_temp_index++);

                switch (kind)
                {
                    case Bytecode_Value_Type_Specifier::INVALID: assert(false);

                    case Bytecode_Value_Type_Specifier::ALLOCL: 
                    {
                        auto name = func->local_allocs[store_idx].value->name;
                        string_builder_appendf(sb, "%%%s", name.data);
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::PARAMETER:
                    {
                        auto name = func->parameters[store_idx].value->name;
                        string_builder_appendf(sb, "%%%s", name.data);
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::TEMPORARY:
                    {
                        string_builder_appendf(sb, "%%%" PRIu32, store_idx);
                        break;
                    }

                }

                string_builder_appendf(sb, ", %" PRIu32, offset);
                break;
            }
        }

        if (newline) string_builder_append(sb, "\n");
    }

    void bytecode_print_size_spec(String_Builder *sb, Bytecode_Size_Specifier size_spec)
    {
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
                string_builder_append(sb, "S64");
                break;
            }
            default: assert(false);
        }
    }

    void bytecode_print_im(String_Builder *sb, Bytecode_Iterator *bci)
    {
        assert(sb);

        auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_byte(bci);
        bytecode_iterator_advance_ip(bci);

        bytecode_print_size_spec(sb, size_spec); 

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
                int64_t val = bytecode_iterator_fetch_64(bci);
                string_builder_appendf(sb, " %" PRId64, val);
                break;
            }
            default: assert(false);
        }
    }
}
