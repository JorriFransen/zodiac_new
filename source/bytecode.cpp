#include "bytecode.h"

#include "builtin.h"
#include "temp_allocator.h"
#include "parser.h"
#include "const_interpreter.h"

#include "resolver.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{

    void bytecode_builder_init(Allocator *allocator, Bytecode_Builder *builder,
                               Build_Data *bd)
    {
        assert(allocator);
        assert(builder);
        assert(bd);

        builder->allocator = allocator;
        builder->build_data = bd;

        array_init(allocator, &builder->program.functions);
        array_init(allocator, &builder->program.globals);
        array_init(allocator, &builder->program.strings);
        builder->program.entry_function = nullptr;
        builder->program.bytecode_entry_function = nullptr;

        array_init(allocator, &builder->emitted_types);
        builder->program.types = &builder->emitted_types;

        stack_init(allocator, &builder->break_block_stack);
        array_init(allocator, &builder->jump_records);

        builder->insert_block = nullptr;
        builder->current_function = nullptr;
    }

    void bytecode_emit_declaration(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        switch (decl->kind)
        {
            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false);

            case AST_Declaration_Kind::USING: assert(false);

            case AST_Declaration_Kind::VARIABLE:
            {
                if (decl->variable.init_expression)
                {
                    auto init_val =
                        bytecode_emit_expression(builder, decl->variable.init_expression);
                    assert(init_val);

                    auto allocl_val = bytecode_find_value_for_variable(builder, decl);
                    assert(allocl_val);
                    bytecode_emit_storel(builder, allocl_val, init_val);
                }
                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                break;
            }

            case AST_Declaration_Kind::PARAMETER: assert(false);

            case AST_Declaration_Kind::FUNCTION:
            {
                bytecode_emit_function_declaration(builder, decl);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::TYPEDEF: assert(false);

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);

            case AST_Declaration_Kind::ENUM: assert(false); assert(false);
                                             
            case AST_Declaration_Kind::STATIC_IF: assert(false);

            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }
    }

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        if (bytecode_find_function_for_decl(builder, decl))
        {
            assert(false);
            return nullptr;
        }

        Bytecode_Function *func = bytecode_new_function(builder, decl);        

        if (decl->decl_flags & AST_DECL_FLAG_IS_ENTRY)
        {
            //assert(decl->decl_flags & AST_DECL_FLAG_IS_NAKED);
            assert(!builder->program.entry_function);
            builder->program.entry_function = func;
            func->flags |= BYTECODE_FUNC_FLAG_CRT_ENTRY;
        }
        if (decl->decl_flags & AST_DECL_FLAG_IS_BYTECODE_ENTRY)
        {
            assert(!builder->program.bytecode_entry_function);
            builder->program.bytecode_entry_function = func;
        }

        auto func_index = builder->program.functions.count;
        array_append(&builder->program.functions, func);
        func->index = func_index;

        return func;
    }

    Bytecode_Function *bytecode_emit_function_declaration(Bytecode_Builder *builder,
                                                          AST_Declaration *decl)
    {
        assert(builder);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bytecode_find_function_for_decl(builder, decl);
        assert(func);

        if (decl->decl_flags & AST_DECL_FLAG_FOREIGN)
        {
            func->flags |= BYTECODE_FUNC_FLAG_FOREIGN;
            //assert(false);
        }
        else
        {
            builder->jump_records.count = 0;
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

        bytecode_fix_jump_records(builder, func);

        int64_t temp_count = 0;
        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            auto block = func->blocks[i];
            block->preceding_temp_count = temp_count;
            temp_count += block->local_temp_count;
        }

        return func;
    }

    Bytecode_Global bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);
        assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);

        auto init_expr = decl->variable.init_expression;

        Bytecode_Global bg = {
            .decl = decl,
            .type = decl->type,
        };

        if (init_expr)
        {
            assert(init_expr->expr_flags & AST_EXPR_FLAG_CONST);

            auto const_val = const_interpret_expression(init_expr);
            Bytecode_Value *value = bytecode_new_value_from_const_value(builder, const_val);
            value->kind = Bytecode_Value_Kind::GLOBAL;
            value->name = decl->identifier->atom;

            auto index = builder->program.globals.count;
            value->glob_index = index;

            bg.value = value;
        }
        else
        {
            bg.value = bytecode_new_zero_value(builder, Bytecode_Value_Kind::GLOBAL, decl->type);
        }

        array_append(&builder->program.globals, bg);

        return bg;
    }

    struct _Block_Replacement
    {
        int64_t old_index = 0;
        Bytecode_Block *new_target = nullptr;
    };

    void bytecode_fix_jump_records(Bytecode_Builder *builder, Bytecode_Function *func)
    {
        // Emit indices for all jumps
        for (int64_t i = 0; i < builder->jump_records.count; i++)
        {
            auto &jr = builder->jump_records[i];
            uint32_t *ptr = (uint32_t*)&jr.from_block->instructions[jr.index_offset];

            uint32_t index = jr.target_block->index;
            assert(index);
            *ptr = index;
        }

        Array<_Block_Replacement> obsolete_blocks = {};
        array_init(builder->allocator, &obsolete_blocks);

        // Collect obsolete blocks
        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            auto block = func->blocks[i];
            if ((Bytecode_Instruction)block->instructions[0] == Bytecode_Instruction::JUMP)
            {
                assert(block->instructions.count == 5); // 1 byte for inst, 4 for index

                uint32_t target_index = *((uint32_t*)(&block->instructions[1]));
                auto target_block = func->blocks[target_index];

                array_append(&obsolete_blocks, { i, target_block });
            }
        }

        bool obsolete_jump_records_found = false;
        do
        {
            obsolete_jump_records_found = false;
            // Update jump records to the new target blocks
            for (int64_t i = 0; i < builder->jump_records.count; i++)
            {
                auto &jr = builder->jump_records[i];
                for (int64_t j = 0; j < obsolete_blocks.count; j++)
                {
                    auto &br = obsolete_blocks[j];
                    Bytecode_Block *obsolete_block = func->blocks[br.old_index];
                    
                    if (jr.from_block == obsolete_block)
                    {
                        jr.index_offset = -1;
                    }

                    if (jr.target_block == obsolete_block)
                    {
                        jr.target_block = br.new_target;
                        obsolete_jump_records_found = true;
                        break;
                    }
                }
            }
        }
        while (obsolete_jump_records_found);

        for (int64_t i = builder->jump_records.count - 1; i >= 0; i--)
        {
            auto &jr = builder->jump_records[i];
            if (jr.index_offset == -1)
            {
                array_unordered_remove(&builder->jump_records, i);
            }
        }

        // Remove obsolete blocks from function
        int64_t remove_count = 0;
        for (int64_t i = 0; i < obsolete_blocks.count; i++)
        {
            auto index = obsolete_blocks[i].old_index - remove_count;
            remove_count += 1;
            array_ordered_remove(&func->blocks, index);
        }

        // Update indices of remaining blocks
        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            func->blocks[i]->index = i; 
        }
        
        // Emit indices for all remaining jumps again
        for (int64_t i = 0; i < builder->jump_records.count; i++)
        {
            auto &jr = builder->jump_records[i];
            uint32_t index = jr.target_block->index;

            assert(jr.index_offset != -1);
            assert(jr.target_block->index < func->blocks.count);

            uint32_t *ptr = (uint32_t*)&jr.from_block->instructions[jr.index_offset];

            assert(index);
            *ptr = index;
        }

        array_free(&obsolete_blocks);
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
                else if (lvalue->kind == Bytecode_Value_Kind::GLOBAL)
                {
                    assert(lvalue->type == rhs_value->type);
                    bytecode_emit_storeg(builder, lvalue, rhs_value);
                }
                else if (lvalue->kind == Bytecode_Value_Kind::PARAMETER)
                {
                    assert(lvalue->type == rhs_value->type);
                    bytecode_emit_store_param(builder, lvalue, rhs_value);
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

            case AST_Statement_Kind::BREAK:
            {
                assert(stack_count(&builder->break_block_stack));
                bytecode_emit_jump(builder, stack_top(&builder->break_block_stack));
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

            case AST_Statement_Kind::WHILE:
            {
                bytecode_emit_while_statement(builder, statement);
                break;
            }
            
            case AST_Statement_Kind::FOR:
            {
                bytecode_emit_for_statement(builder, statement);
                break;
            }

            case AST_Statement_Kind::IF:
            {
                bytecode_emit_if_statement(builder, statement);
                break;
            }

            case AST_Statement_Kind::SWITCH:
            {
                bytecode_emit_switch_statement(builder, statement);
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

    void bytecode_emit_while_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::WHILE);

        auto current_func = builder->current_function;
        auto cond_block = bytecode_builder_append_block(builder, current_func, "while_cond");
        auto body_block = bytecode_new_block(builder, "while_body");
        auto post_while_block = bytecode_new_block(builder, "post_while");

        bytecode_emit_jump(builder, cond_block);
        bytecode_builder_set_insert_point(builder, cond_block);

        auto cond_val = bytecode_emit_expression(builder, stmt->while_stmt.cond_expr);
        assert(cond_val);

        bytecode_emit_jump_if(builder, body_block, cond_val);
        bytecode_emit_jump(builder, post_while_block);

        bytecode_builder_append_block(builder, current_func, body_block);
        bytecode_builder_set_insert_point(builder, body_block);

        bytecode_push_break_block(builder, post_while_block);
        bytecode_emit_statement(builder, stmt->while_stmt.body);
        bytecode_pop_break_block(builder);

        bytecode_emit_jump(builder, cond_block);

        bytecode_builder_append_block(builder, current_func, post_while_block);
        bytecode_builder_set_insert_point(builder, post_while_block);
    }

    void bytecode_emit_for_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::FOR);

        auto current_func = builder->current_function;

        Bytecode_Block *cond_block = bytecode_new_block(builder, "for_cond");
        Bytecode_Block *body_block = bytecode_new_block(builder, "for_body");
        Bytecode_Block *post_for_block = bytecode_new_block(builder, "post_for");

        for (int64_t i = 0; i < stmt->for_stmt.init_statements.count; i++)
        {
            auto init_stmt = stmt->for_stmt.init_statements[i];
            bytecode_emit_statement(builder, init_stmt);
        }
        bytecode_emit_jump(builder, cond_block);

        bytecode_builder_append_block(builder, current_func, cond_block);
        bytecode_builder_set_insert_point(builder, cond_block);
        auto cond_val = bytecode_emit_expression(builder, stmt->for_stmt.cond_expr);
        bytecode_emit_jump_if(builder, body_block, cond_val);
        bytecode_emit_jump(builder, post_for_block);

        bytecode_builder_append_block(builder, current_func, body_block);
        bytecode_builder_set_insert_point(builder, body_block);


        if (stmt->for_stmt.it_decl)
        {
            auto it_allocl = bytecode_find_value_for_variable(builder,
                                                              stmt->for_stmt.it_decl);
            auto it_init_expr = stmt->for_stmt.it_decl->variable.init_expression;
            auto it_val = bytecode_emit_expression(builder, it_init_expr);
            bytecode_emit_storel(builder, it_allocl, it_val);
        }
        
        bytecode_emit_statement(builder, stmt->for_stmt.body_stmt);
        for (int64_t i = 0; i < stmt->for_stmt.step_statements.count; i++)
        {
            auto step_stmt = stmt->for_stmt.step_statements[i];
            bytecode_emit_statement(builder, step_stmt);
        }
        bytecode_emit_jump(builder, cond_block);

        bytecode_builder_append_block(builder, current_func, post_for_block);
        bytecode_builder_set_insert_point(builder, post_for_block);

    }

    void bytecode_emit_if_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::IF);

        auto cond_val = bytecode_emit_expression(builder, stmt->if_stmt.cond_expr);

        auto current_func = builder->current_function;
        auto then_block = bytecode_builder_append_block(builder, current_func, "then");
        Bytecode_Block  *else_block = nullptr;
        if (stmt->if_stmt.else_stmt)
        {
            else_block = bytecode_new_block(builder, "else");
        }
        auto post_if_block = bytecode_new_block(builder, "post_if");

        bytecode_emit_jump_if(builder, then_block, cond_val);
        bytecode_emit_jump(builder, else_block ? else_block : post_if_block);

        bytecode_builder_set_insert_point(builder, then_block);
        bytecode_emit_statement(builder, stmt->if_stmt.then_stmt);

        then_block = builder->insert_block;
        if (!bytecode_block_ends_with_terminator(then_block))
            bytecode_emit_jump(builder, post_if_block);

        if (stmt->if_stmt.else_stmt)
        {
            bytecode_builder_append_block(builder, current_func, else_block);
            bytecode_builder_set_insert_point(builder, else_block);
            bytecode_emit_statement(builder, stmt->if_stmt.else_stmt);

            else_block = builder->insert_block;
            if (!bytecode_block_ends_with_terminator(else_block))
                bytecode_emit_jump(builder, post_if_block);
        }

        bytecode_builder_append_block(builder, current_func, post_if_block);
        bytecode_builder_set_insert_point(builder, post_if_block);
    }

    void bytecode_emit_switch_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::SWITCH);

        auto ta = temp_allocator_get();

        auto switch_val = bytecode_emit_expression(builder, stmt->switch_stmt.expression);

        bytecode_emit_instruction(builder, Bytecode_Instruction::SWITCH);
        bytecode_emit_type_index(builder, switch_val->type);
        bytecode_emit_32(builder, switch_val->local_index);

        auto case_count = stmt->switch_stmt.case_expr_count;
        bytecode_emit_32(builder, case_count);

        Bytecode_Block *pre_switch_block = builder->insert_block;
        Bytecode_Block *default_block = nullptr;
        Bytecode_Block *post_switch_block = bytecode_new_block(builder, "post_switch");

        Array<Bytecode_Block*> case_blocks = {};
        array_init(ta, &case_blocks, stmt->switch_stmt.cases.count);

        for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++)
        {
            AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];

            const char *block_name = (switch_case->is_default ? "default" : "case");

            Bytecode_Block *case_block = bytecode_new_block(builder, block_name);

            bytecode_builder_set_insert_point(builder, case_block);
            bytecode_push_break_block(builder, post_switch_block);
            bytecode_emit_statement(builder, switch_case->body);
            bytecode_pop_break_block(builder);

            if (!bytecode_block_ends_with_terminator(case_block))
            {
                bytecode_emit_jump(builder, post_switch_block);
            }

            // case_block = builder->insert_block;
            array_append(&case_blocks, case_block);

            if (switch_case->is_default)
            {
                default_block = case_block;
            }
        }

        bytecode_builder_set_insert_point(builder, pre_switch_block);

        if (!stmt->switch_stmt.default_case)
        {
            assert(!default_block);
            default_block = post_switch_block;
        }

        auto offset = bytecode_emit_32(builder, 0);
        bytecode_record_jump(builder, default_block, offset);

        int emitted_case_exprs = 0;

        for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++)
        {
            AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];
            Bytecode_Block *case_block = case_blocks[i];

            if (switch_case->is_default) continue; 

            for (int64_t expr_i = 0; expr_i < switch_case->expressions.count; expr_i++)
            {
                bytecode_emit_integer_literal(builder, switch_case->expressions[expr_i],
                                              true);
                auto offset = bytecode_emit_32(builder, 0);
                bytecode_record_jump(builder, case_block, offset);

                emitted_case_exprs += 1;
            }
        }

        assert(emitted_case_exprs == (int)stmt->switch_stmt.case_expr_count);

        auto func = builder->current_function;

        for (int64_t i = 0; i < case_blocks.count; i++)
        {
            Bytecode_Block *case_block = case_blocks[i];
            bytecode_builder_append_block(builder, func, case_block);
        }

        bytecode_builder_append_block(builder, func, post_switch_block);
        bytecode_builder_set_insert_point(builder, post_switch_block);
    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder,
                                             AST_Expression *expression)
    {
        assert(builder);

        assert(expression->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(expression->flags & AST_NODE_FLAG_TYPED);
        
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
                if (expression->dot.child_decl &&
                    (expression->dot.child_decl->kind == AST_Declaration_Kind::CONSTANT))
                {
                    auto decl = expression->dot.child_decl;
                    auto init_expr = decl->constant.init_expression;
                    return bytecode_emit_expression(builder, init_expr);

                }
                else if (expression->expr_flags & AST_EXPR_FLAG_DOT_COUNT)
                {
                    auto parent_expr = expression->dot.parent_expression;
                    AST_Declaration *parent_decl = nullptr;

                    if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER)
                    {
                        parent_decl = parent_expr->identifier->declaration;
                    }
                    else
                    {
                        assert(false);
                    }
                    assert(parent_decl);

                    auto parent_type = parent_decl->type;
                    assert(parent_type);
                    assert(parent_type->kind == AST_Type_Kind::ARRAY);

                    auto element_count = parent_type->array.element_count;

                    return bytecode_emit_integer_literal(builder, expression->type,
                                                         element_count);
                }
                else
                {
                    auto lvalue = bytecode_emit_lvalue(builder,
                                                       expression->dot.parent_expression);
                    assert(lvalue);
                    assert(lvalue->kind == Bytecode_Value_Kind::ALLOCL ||
                           lvalue->kind == Bytecode_Value_Kind::PARAMETER);

                    auto index = expression->dot.child_index;
                    assert(index >= 0);

                    Bytecode_Value *ptr = nullptr;

                    Bytecode_Value *index_val = bytecode_emit_integer_literal(builder,
                                                                             Builtin::type_u32,
                                                                             index);

                    if (lvalue->type->kind == AST_Type_Kind::STRUCTURE)
                    {
                        ptr = bytecode_emit_aggregate_offset_pointer(builder, lvalue,
                                                                     index_val);
                    }
                    else
                    {
                        // Pointer to struct
                        assert(lvalue->type->kind == AST_Type_Kind::POINTER);
                        assert(lvalue->type->pointer.base->kind == AST_Type_Kind::STRUCTURE);

                        lvalue = bytecode_emit_load(builder, lvalue);
                        ptr = bytecode_emit_aggregate_offset_pointer(builder, lvalue,
                                                                     index_val);
                    }

                    assert(ptr);

                    auto value = bytecode_emit_loadp(builder, ptr);
                    assert(value);
                    return value;

                }
                break;
            }

            case AST_Expression_Kind::BINARY:
            {
                return bytecode_emit_binary_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::UNARY:
            {
                return bytecode_emit_unary_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::POST_FIX:
            {
                auto op_expr = expression->post_fix.operand_expression;
                Bytecode_Value *operand_val = bytecode_emit_expression(builder, op_expr);

                auto one_val = bytecode_emit_integer_literal(builder, op_expr->type, 1);

                if (expression->post_fix.op == BINOP_ADD)
                {
                    bytecode_emit_instruction(builder, Bytecode_Instruction::ADD);
                }
                else if (expression->post_fix.op == BINOP_SUB)
                {
                    bytecode_emit_instruction(builder, Bytecode_Instruction::SUB);
                }
                else assert(false);

                bytecode_emit_size_spec(builder, op_expr->type);
                bytecode_emit_32(builder, operand_val->local_index);
                bytecode_emit_32(builder, one_val->local_index);

                auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                                 op_expr->type);
                bytecode_push_local_temporary(builder, result);

                auto lval = bytecode_emit_lvalue(builder, op_expr);

                bytecode_emit_store(builder, lval, result);

                return operand_val;
                break; 
            }

            case AST_Expression_Kind::PRE_FIX:
            {
                auto op_expr = expression->post_fix.operand_expression;
                Bytecode_Value *operand_val = bytecode_emit_expression(builder, op_expr);

                auto one_val = bytecode_emit_integer_literal(builder, op_expr->type, 1);

                if (expression->post_fix.op == BINOP_ADD)
                {
                    bytecode_emit_instruction(builder, Bytecode_Instruction::ADD);
                }
                else if (expression->post_fix.op == BINOP_SUB)
                {
                    bytecode_emit_instruction(builder, Bytecode_Instruction::SUB);
                }
                else assert(false);

                bytecode_emit_size_spec(builder, op_expr->type);
                bytecode_emit_32(builder, operand_val->local_index);
                bytecode_emit_32(builder, one_val->local_index);

                auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                                 op_expr->type);
                bytecode_push_local_temporary(builder, result);

                auto lval = bytecode_emit_lvalue(builder, op_expr);

                bytecode_emit_store(builder, lval, result);

                return result;
                break; 
            }
            case AST_Expression_Kind::CALL:
            {
                return bytecode_emit_call_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL:
            {
                return bytecode_emit_builtin_call_expression(builder, expression);
                break;
            }

            case AST_Expression_Kind::ADDROF:
            {
                auto result = bytecode_emit_lvalue(builder, expression->addrof.operand_expr);
                switch (result->kind)
                {
                    case Bytecode_Value_Kind::ALLOCL:
                    case Bytecode_Value_Kind::TEMPORARY:
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

            case AST_Expression_Kind::SUBSCRIPT:
            {
                auto ptr_val = bytecode_emit_lvalue(builder,
                                                    expression->subscript.pointer_expression);

                assert(ptr_val);
                assert(ptr_val->type->kind == AST_Type_Kind::POINTER ||
                       ptr_val->type->kind == AST_Type_Kind::ARRAY);

                auto index_val =
                    bytecode_emit_expression(builder, expression->subscript.index_expression);

                assert(index_val->type->kind == AST_Type_Kind::INTEGER);
                if (index_val->type != Builtin::type_u32)
                {
                    index_val = bytecode_emit_cast_int_int(builder, index_val, Builtin::type_u32); 
                }

                auto result = bytecode_emit_array_offset_pointer(builder, ptr_val, index_val);
                return bytecode_emit_load(builder, result);
                break;
            }

            case AST_Expression_Kind::CAST:
            {
                return bytecode_emit_cast(builder, expression->cast.operand_expression,
                                          expression->cast.target_type);
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL:
            {
                return bytecode_emit_float_literal(builder, expression); 
                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL:
            case AST_Expression_Kind::CHAR_LITERAL:
            {
                if (expression->type->kind == AST_Type_Kind::INTEGER ||
                    expression->type->kind == AST_Type_Kind::ENUM)
                {
                    return bytecode_emit_integer_literal(builder, expression);
                }
                else if (expression->type->kind == AST_Type_Kind::FLOAT)
                {
                    return bytecode_emit_float_literal(builder, expression);
                }
                break;
            }

            case AST_Expression_Kind::BOOL_LITERAL:
            {
                return bytecode_emit_bool_literal(builder, expression); 
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL:
            {
                return bytecode_emit_load_str(builder, expression->string_literal.atom);
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL:
            {
                return bytecode_emit_load_null(builder, expression->type);
                break;
            }

            case AST_Expression_Kind::RANGE: assert(false);
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

        auto ret_type = expression->type; 
        if (ret_type != Builtin::type_void)
        {
            return_value = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                              ret_type);
            bytecode_push_local_temporary(builder, return_value);
        }

        return return_value;
    }

    Bytecode_Value *bytecode_emit_builtin_call_expression(Bytecode_Builder *builder,
                                                          AST_Expression *expression)
    {
        assert(builder);
        assert(expression);
        assert(expression->kind == AST_Expression_Kind::BUILTIN_CALL);

        Bytecode_Value *return_value = nullptr;

        auto atom = expression->builtin_call.identifier->atom;
        auto arg_exprs = expression->builtin_call.arg_expressions;
        if (atom == Builtin::atom_exit)
        {
            assert(arg_exprs.count == 1);

            assert(arg_exprs[0]->type->kind == AST_Type_Kind::INTEGER);
            auto exit_code_val = bytecode_emit_expression(builder, arg_exprs[0]);
            assert(exit_code_val);

            bytecode_emit_instruction(builder, Bytecode_Instruction::EXIT);
            bytecode_emit_32(builder, exit_code_val->local_index);

            return_value = nullptr;
        }
        else if (atom == Builtin::atom_syscall)
        {
#ifndef linux
            assert(false);
#endif
            assert(arg_exprs.count >= 1);

            for (int64_t i = 0; i < arg_exprs.count; i++)
            {
                bytecode_emit_call_arg(builder, arg_exprs[i]);
            }

            bytecode_emit_instruction(builder, Bytecode_Instruction::SYSCALL);
            bytecode_emit_32(builder, arg_exprs.count);
        }
        else if (atom == Builtin::atom_cast)
        {
            auto target_type = arg_exprs[0]->type;
            auto operand = arg_exprs[1];

            return bytecode_emit_cast(builder, operand, target_type);
        }
        else if (atom == Builtin::atom_sizeof)
        {
            assert(arg_exprs.count == 1);

            auto type = arg_exprs[0]->type;
            return bytecode_emit_integer_literal(builder, Builtin::type_s64, 
                                                 type->bit_size);
        }
        else if (atom == Builtin::atom_offsetof)
        {
            assert(arg_exprs.count == 2);

            auto mem_name = arg_exprs[0];
            auto struct_name = arg_exprs[1];

            auto struct_decl = struct_name->identifier->declaration;
            assert(struct_decl);

            // auto struct_type = struct_name->type;
            // assert(struct_type);

            auto mem_decl = mem_name->identifier->declaration;
            assert(mem_decl);

            int64_t offset = 0;
            bool found = false;

            for (int64_t i = 0; i < struct_decl->structure.member_declarations.count; i++)
            {
                auto struct_mem = struct_decl->structure.member_declarations[i];
                if (struct_mem == mem_decl)
                {
                    found = true;
                    break;
                }

                auto mem_type = struct_mem->type;
                assert(mem_type);

                auto mem_size = mem_type->bit_size;
                assert(mem_size % 8 == 0);
                
                offset += mem_size / 8;
            }

            assert(found);
            return bytecode_emit_integer_literal(builder, Builtin::type_s64, offset);
        }
        else 
        {
            assert(false);
        }

        return return_value;
    }

    Bytecode_Value *bytecode_emit_binary_expression(Bytecode_Builder *builder,
                                                    AST_Expression *expr)
    {
        assert(builder);
        assert(expr);
        assert(expr->kind == AST_Expression_Kind::BINARY);
       
        assert(expr->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(expr->flags & AST_NODE_FLAG_TYPED);

        auto lhs = expr->binary.lhs;
        auto rhs = expr->binary.rhs;

        assert(lhs->type == rhs->type);
        assert(lhs->type->kind == AST_Type_Kind::INTEGER ||
               lhs->type->kind == AST_Type_Kind::FLOAT ||
               lhs->type->kind == AST_Type_Kind::ENUM);

        auto lhs_val = bytecode_emit_expression(builder, lhs);
        assert(lhs_val);
        auto rhs_val = bytecode_emit_expression(builder, rhs);
        assert(rhs_val);

        switch (expr->binary.op)
        {
            case BINOP_INVALID: assert(false);

            case BINOP_EQ:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::EQ);
                break;
            }

            case BINOP_NEQ:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::NEQ);
                break;
            }

            case BINOP_LT:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::LT);
                break;
            }

            case BINOP_LTEQ:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::LTEQ);
                break;
            }

            case BINOP_GT: 
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::GT);
                break;
            }

            case BINOP_GTEQ:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::GTEQ);
                break;
            }

            case BINOP_ADD: 
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::ADD);
                break;
            }

            case BINOP_SUB:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::SUB);
                break;
            }

            case BINOP_REMAINDER:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::REM);
                break;
            }

            case BINOP_MUL:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::MUL);
                break;
            }

            case BINOP_DIV:
            {
                bytecode_emit_instruction(builder, Bytecode_Instruction::DIV);
                break;
            }
        }    

        auto type = lhs->type;
        bytecode_emit_size_spec(builder, type);

        bytecode_emit_32(builder, lhs_val->local_index);
        bytecode_emit_32(builder, rhs_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_unary_expression(Bytecode_Builder *builder,
                                                   AST_Expression *expr)
    {
        auto operand_val = bytecode_emit_expression(builder,
                                                    expr->unary.operand_expression);

        AST_Type *result_type = nullptr;

        switch (expr->unary.op)
        {
            case UNOP_INVALID: assert(false);

            case UNOP_DEREF:
            {
                result_type = expr->type;
                
                bytecode_emit_instruction(builder, Bytecode_Instruction::DEREF);
                bytecode_emit_32(builder, operand_val->local_index);
                break;
            }

            case UNOP_MINUS:
            {
                result_type = expr->type;

                bytecode_emit_instruction(builder, Bytecode_Instruction::NEG);
                bytecode_emit_size_spec(builder, expr->type);
                bytecode_emit_32(builder, operand_val->local_index);

                break;
            }

            case UNOP_PRE_INC: assert(false);
            case UNOP_PRE_DEC: assert(false);
        }

        assert(result_type);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         result_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_identifier(Bytecode_Builder *builder,
                                             AST_Identifier *ident)
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

            case AST_Declaration_Kind::USING:  assert(false);

            case AST_Declaration_Kind::VARIABLE: 
            {
                decl_val = bytecode_find_value_for_variable(builder, decl);
                break;
            }

            case AST_Declaration_Kind::CONSTANT:
            {
                decl_val = bytecode_emit_constant(builder, decl);
                break;
            }

            case AST_Declaration_Kind::PARAMETER:
            {
                decl_val = bytecode_find_value_for_parameter(builder, decl);
                break;
            }

            case AST_Declaration_Kind::TYPE: assert(false);
            case AST_Declaration_Kind::FUNCTION: assert(false);
            case AST_Declaration_Kind::TYPEDEF: assert(false);

            case AST_Declaration_Kind::STRUCTURE: assert(false);
            case AST_Declaration_Kind::POLY_TYPE: assert(false);

            case AST_Declaration_Kind::ENUM: assert(false); assert(false);
                                             
            case AST_Declaration_Kind::STATIC_IF: assert(false);

            case AST_Declaration_Kind::STATIC_ASSERT: assert(false);
        }

        assert(decl_val);

        switch (decl_val->kind)
        {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::GLOBAL:
            {
                auto result = bytecode_emit_loadg(builder, decl_val);
                assert(result);
                return result;
                break;
            }

            case Bytecode_Value_Kind::TEMPORARY:
            {
                return decl_val;
                break;
            }

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

                        if (idecl->decl_flags & AST_DECL_FLAG_GLOBAL)
                            assert(result->kind == Bytecode_Value_Kind::GLOBAL);
                        else
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

                Bytecode_Value *index_val =
                    bytecode_emit_integer_literal(builder, Builtin::type_u32,
                                                 lvalue_expr->dot.child_index);
                auto pointer_val = bytecode_emit_aggregate_offset_pointer(builder,
                                                                          parent_lvalue,
                                                                          index_val);
                return pointer_val;
                break;
            }

            case AST_Expression_Kind::BINARY: assert(false);

            case AST_Expression_Kind::UNARY:
            {
                assert(lvalue_expr->unary.op == UNOP_DEREF);

                auto op_lvalue =
                    bytecode_emit_expression(builder,
                                             lvalue_expr->unary.operand_expression);

                assert(op_lvalue->type->kind == AST_Type_Kind::POINTER);

                return op_lvalue;

                break;
            }

            case AST_Expression_Kind::POST_FIX: assert(false);
            case AST_Expression_Kind::PRE_FIX: assert(false);
            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::BUILTIN_CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT:
            {
                auto parent_lvalue =
                    bytecode_emit_lvalue(builder, lvalue_expr->subscript.pointer_expression);
                assert(parent_lvalue);

                auto index_value =
                    bytecode_emit_expression(builder, lvalue_expr->subscript.index_expression);
                assert(index_value);

                assert(index_value->type->kind == AST_Type_Kind::INTEGER);
                if (index_value->type != Builtin::type_u32)
                {
                    index_value = bytecode_emit_cast_int_int(builder, index_value,
                                                             Builtin::type_u32); 
                }

                if (parent_lvalue->type->kind == AST_Type_Kind::ARRAY)
                {
                    return bytecode_emit_array_offset_pointer(builder, parent_lvalue,
                                                              index_value); 
                }
                else assert(false);

                break;
            }

            case AST_Expression_Kind::CAST: assert(false);
            case AST_Expression_Kind::INTEGER_LITERAL: assert(false);
            case AST_Expression_Kind::FLOAT_LITERAL: assert(false);
            case AST_Expression_Kind::STRING_LITERAL: assert(false);
            case AST_Expression_Kind::CHAR_LITERAL: assert(false);
            case AST_Expression_Kind::BOOL_LITERAL: assert(false);
            case AST_Expression_Kind::NULL_LITERAL: assert(false);

            case AST_Expression_Kind::RANGE: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_addrof(Bytecode_Builder *builder,
                                         Bytecode_Value *lvalue)
    {
        if (lvalue->kind == Bytecode_Value_Kind::ALLOCL)
        {
            bytecode_emit_instruction(builder, Bytecode_Instruction::ADDROF);
            bytecode_emit_32(builder, lvalue->alloc_index);

            auto pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                      builder->build_data,
                                                                      lvalue->type);
            auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                             pointer_type);
            bytecode_push_local_temporary(builder, result);

            return result;
        }
        else if (lvalue->kind == Bytecode_Value_Kind::TEMPORARY)
        {
            assert(lvalue->type->kind == AST_Type_Kind::POINTER);
            return lvalue;
        }
        else assert(false);
        return nullptr;
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

    Bytecode_Value *bytecode_emit_constant(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::CONSTANT);

        auto result = bytecode_emit_expression(builder, decl->constant.init_expression);
        assert(result->type == decl->type);

        return result;
    }

    Bytecode_Value *bytecode_emit_cast(Bytecode_Builder *builder, AST_Expression *operand_expr,
                                       AST_Type *target_type)
    {
        switch (operand_expr->type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                return bytecode_emit_cast_int(builder, operand_expr, target_type);
                break;
            }

            case AST_Type_Kind::FLOAT:
            {
                return bytecode_emit_cast_float(builder, operand_expr, target_type);
                break;
            }

            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER:
            {
                return bytecode_emit_cast_pointer(builder, operand_expr, target_type);
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);

            case AST_Type_Kind::ENUM:
            {
                return bytecode_emit_cast_enum(builder, operand_expr, target_type);
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_cast_int(Bytecode_Builder *builder,
                                           AST_Expression *operand_expr,
                                           AST_Type *target_type)
    {
        assert(operand_expr->type->kind == AST_Type_Kind::INTEGER);

        auto operand_val = bytecode_emit_expression(builder, operand_expr);

        switch (target_type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            { 
                return bytecode_emit_cast_int_int(builder, operand_val, target_type);
            }

            case AST_Type_Kind::FLOAT:
            {
                return bytecode_emit_cast_int_float(builder, operand_val, target_type);
            }

            case AST_Type_Kind::BOOL:
            {
                return bytecode_emit_cast_int_bool(builder, operand_val, target_type);
            }

            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_cast_enum(Bytecode_Builder *builder, AST_Expression *operand_expr,
                                           AST_Type *target_type)
    {
        assert(operand_expr->type->kind == AST_Type_Kind::ENUM);

        auto operand_val = bytecode_emit_expression(builder, operand_expr);
        assert(operand_val);

        assert(target_type->kind == AST_Type_Kind::INTEGER);

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_ENUM);
        bytecode_emit_size_spec(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, target_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_cast_float(Bytecode_Builder *builder,
                                             AST_Expression *operand_expr,
                                             AST_Type *target_type)
    {
         assert(operand_expr->type->kind == AST_Type_Kind::FLOAT);

        auto operand_val = bytecode_emit_expression(builder, operand_expr);
        assert(operand_val);

        switch (target_type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                return bytecode_emit_cast_float_int(builder, operand_val, target_type);
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: assert(false);
        }
       
        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_emit_cast_pointer(Bytecode_Builder *builder,
                                               AST_Expression *operand_expr,
                                               AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::POINTER);
        assert(operand_expr->type->kind == AST_Type_Kind::POINTER);

        auto operand_val = bytecode_emit_expression(builder, operand_expr);
        assert(operand_val);

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_POINTER);
        bytecode_emit_type_index(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         target_type);
        bytecode_push_local_temporary(builder, result);
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

        builder->insert_block->local_temp_count += 1;
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

    void bytecode_emit_size_spec(Bytecode_Builder *builder, AST_Type *type)
    {
        bool sign = true;
        bool real = false;

        if (type->kind == AST_Type_Kind::INTEGER)
        {
            sign = type->integer.sign;
        }
        else if (type->kind == AST_Type_Kind::BOOL)
        {
            sign = false;
        }
        else if (type->kind == AST_Type_Kind::ENUM)
        {
            auto base_type = type->enum_type.base_type;
            sign = base_type->integer.sign;
        }
        else if (type->kind == AST_Type_Kind::FLOAT)
        {
            real = true;
        }
        else
        {
            assert(false);
        }

        bytecode_emit_size_spec(builder, sign, real, type->bit_size);
    }

    void bytecode_emit_size_spec(Bytecode_Builder *builder, bool sign, bool real, uint8_t size)
    {
        if (real) assert(sign);

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
            size_spec =
                (Bytecode_Size_Specifier)(((uint16_t)size_spec) |
                                          ((uint16_t)Bytecode_Size_Specifier::SIGN_FLAG));
        }

        if (real)
        {
            size_spec =
                (Bytecode_Size_Specifier)(((uint16_t)size_spec) | 
                                          ((uint16_t)Bytecode_Size_Specifier::FLOAT_FLAG));
        }

        bytecode_emit_16(builder, (uint16_t)size_spec);
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
                assert(lvalue->type->kind == AST_Type_Kind::POINTER);
                return bytecode_emit_loadp(builder, lvalue);
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void bytecode_emit_load_float(Bytecode_Builder *builder, uint8_t size)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_FLOAT);
        bytecode_emit_size_spec(builder, true, true, size);
    }

    void bytecode_emit_load_int(Bytecode_Builder *builder, bool sign, uint8_t size)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_INT);
        bytecode_emit_size_spec(builder, sign, false, size);
    }

    Bytecode_Value *bytecode_emit_loadg(Bytecode_Builder *builder, Bytecode_Value *glob)
    {
        assert(glob->kind == Bytecode_Value_Kind::GLOBAL);
        assert(glob->type);
        assert(glob->type->kind == AST_Type_Kind::INTEGER ||
               glob->type->kind == AST_Type_Kind::POINTER ||
               glob->type->kind == AST_Type_Kind::FLOAT ||
               glob->type->kind == AST_Type_Kind::BOOL);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOADG);
        bytecode_emit_32(builder, glob->glob_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         glob->type);
        bytecode_push_local_temporary(builder, result);

        return result;
    }

    Bytecode_Value *bytecode_emit_loadl(Bytecode_Builder *builder, Bytecode_Value *allocl)
    {
        assert(allocl->kind == Bytecode_Value_Kind::ALLOCL);
        assert(allocl->type);
        assert(allocl->type->kind == AST_Type_Kind::INTEGER ||
               allocl->type->kind == AST_Type_Kind::FLOAT ||
               allocl->type->kind == AST_Type_Kind::BOOL ||
               allocl->type->kind == AST_Type_Kind::POINTER ||
               allocl->type->kind == AST_Type_Kind::ENUM ||
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

    Bytecode_Value *bytecode_emit_load_str(Bytecode_Builder *builder, const Atom &atom)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_STR);

        int64_t string_index = -1;

        for (int64_t i = 0; i < builder->program.strings.count; i++)
        {
            if (atom == builder->program.strings[i])
            {
                string_index = i;
                break;
            }
        }

        if (string_index == -1)
        {
            string_index = builder->program.strings.count;
            array_append(&builder->program.strings, atom);
        }

        bytecode_emit_32(builder, string_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         Builtin::type_ptr_u8);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_load_null(Bytecode_Builder *builder, AST_Type *type)
    {
        assert(type->kind == AST_Type_Kind::POINTER);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_NULL);
        bytecode_emit_type_index(builder, type);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                         type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    void bytecode_emit_store(Bytecode_Builder *builder, Bytecode_Value *dest,
                             Bytecode_Value *value)
    {
        switch (dest->kind)
        {
            case Bytecode_Value_Kind::ALLOCL:
            {
                bytecode_emit_storel(builder, dest, value);
                break;
            }
            
            default: assert(false);
        }
    }

    void bytecode_emit_storeg(Bytecode_Builder *builder, Bytecode_Value *dest,
                              Bytecode_Value *value)
    {
        assert(dest->kind == Bytecode_Value_Kind::GLOBAL);
        assert(value->kind == Bytecode_Value_Kind::TEMPORARY);

        bytecode_emit_instruction(builder, Bytecode_Instruction::STOREG);
        bytecode_emit_32(builder, dest->glob_index);
        bytecode_emit_32(builder, value->local_index);
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

    void bytecode_emit_store_param(Bytecode_Builder *builder, Bytecode_Value *dest,
                                   Bytecode_Value *value)
    {
        assert(dest->kind == Bytecode_Value_Kind::PARAMETER);
        assert(value->kind == Bytecode_Value_Kind::TEMPORARY);
        assert(value->type == dest->type);

        bytecode_emit_instruction(builder, Bytecode_Instruction::STORE_PARAM);
        bytecode_emit_32(builder, dest->param_index);
        bytecode_emit_32(builder, value->local_index);
    }

    void bytecode_emit_jump(Bytecode_Builder *builder, Bytecode_Block *block)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::JUMP);
        auto offset = bytecode_emit_32(builder, 0);
        bytecode_record_jump(builder, block, offset);
    }

    void bytecode_emit_jump_if(Bytecode_Builder *builder, Bytecode_Block *block,
                               Bytecode_Value *cond)
    {
        bytecode_emit_instruction(builder, Bytecode_Instruction::JUMP_IF);
        assert(cond->kind == Bytecode_Value_Kind::TEMPORARY);
        bytecode_emit_32(builder, cond->local_index);
        auto offset = bytecode_emit_32(builder, 0);
        bytecode_record_jump(builder, block, offset);
    }

    Bytecode_Value *bytecode_emit_cast_int_int(Bytecode_Builder *builder,
                                               Bytecode_Value *operand_val,
                                               AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::INTEGER);

        assert(operand_val->kind == Bytecode_Value_Kind::TEMPORARY);

#ifndef NDEBUG
        auto op_type = operand_val->type;
        assert(op_type->kind == AST_Type_Kind::INTEGER);
#endif

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_INT);
        bytecode_emit_size_spec(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, target_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_cast_int_float(Bytecode_Builder *builder,
                                                 Bytecode_Value *operand_val,
                                                 AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::FLOAT);
        assert(operand_val->kind == Bytecode_Value_Kind::TEMPORARY);

#ifndef NDEBUG
        auto op_type = operand_val->type;
        assert(op_type->kind == AST_Type_Kind::INTEGER);
#endif

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_INT);
        bytecode_emit_size_spec(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, target_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_cast_int_bool(Bytecode_Builder *builder,
                                                Bytecode_Value *operand_val,
                                                AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::BOOL);
        assert(operand_val->kind == Bytecode_Value_Kind::TEMPORARY);

#ifndef NDEBUG
        auto op_type = operand_val->type;
        assert(op_type->kind == AST_Type_Kind::INTEGER);
#endif

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_INT);
        bytecode_emit_size_spec(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, target_type);
        bytecode_push_local_temporary(builder, result);
        return result;

    }

    Bytecode_Value *bytecode_emit_cast_float_int(Bytecode_Builder *builder,
                                                 Bytecode_Value *operand_val,
                                                 AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::INTEGER);
        assert(operand_val->kind == Bytecode_Value_Kind::TEMPORARY);

#ifndef NDEBUG
        auto op_type = operand_val->type;
        assert(op_type->kind == AST_Type_Kind::FLOAT);
#endif

        bytecode_emit_instruction(builder, Bytecode_Instruction::CAST_FLOAT);
        bytecode_emit_size_spec(builder, target_type);
        bytecode_emit_32(builder, operand_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, target_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_aggregate_offset_pointer(Bytecode_Builder *builder,
                                                           Bytecode_Value *lvalue,
                                                           Bytecode_Value *offset_val)
    {
        assert(builder);
        assert(lvalue);
        assert(lvalue->kind == Bytecode_Value_Kind::ALLOCL ||
               lvalue->kind == Bytecode_Value_Kind::PARAMETER ||
               (lvalue->kind == Bytecode_Value_Kind::TEMPORARY &&
                lvalue->type->kind == AST_Type_Kind::POINTER &&
                lvalue->type->pointer.base->kind == AST_Type_Kind::STRUCTURE));

        assert(offset_val->kind == Bytecode_Value_Kind::TEMPORARY);
        assert(offset_val->type == Builtin::type_u32);

        auto index = offset_val->value.integer.u32;

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

        bytecode_emit_instruction(builder, Bytecode_Instruction::AGG_OFFSET_PTR);
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

        bytecode_emit_32(builder, offset_val->local_index);
        //bytecode_emit_32(builder, index);

        auto mem_ptr_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                   builder->build_data,
                                                                   mem_type);
        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, mem_ptr_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_array_offset_pointer(Bytecode_Builder *builder,
                                                       Bytecode_Value *lvalue,
                                                       Bytecode_Value *offset_val)
    {
        assert(offset_val->type == Builtin::type_u32);

        AST_Type *result_type = nullptr;
        if (lvalue->type->kind == AST_Type_Kind::POINTER)
        {
            result_type = lvalue->type; 
        }
        else if (lvalue->type->kind == AST_Type_Kind::ARRAY)
        {
            result_type =
                build_data_find_or_create_pointer_type(builder->allocator, builder->build_data,
                                                       lvalue->type->array.element_type);
        }
        else assert(false);

        assert(result_type);

        bytecode_emit_instruction(builder, Bytecode_Instruction::ARR_OFFSET_PTR);
        switch (lvalue->kind)
        {
            default: assert(false);

            case Bytecode_Value_Kind::TEMPORARY:
            {
                bytecode_emit_byte(builder, (uint8_t)Bytecode_Value_Type_Specifier::TEMPORARY);
                bytecode_emit_32(builder, lvalue->local_index);
                break;
            }

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
        }

        bytecode_emit_32(builder, offset_val->local_index);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, result_type);
        bytecode_push_local_temporary(builder, result);
        return result;
    }

    Bytecode_Value *bytecode_emit_float_literal(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->type->kind == AST_Type_Kind::FLOAT);

        Bytecode_Value *result = nullptr;

        if (expr->kind == AST_Expression_Kind::FLOAT_LITERAL)
        {
            if (expr->type->bit_size == 32)
            {
                float val = expr->float_literal.r32;
                result = bytecode_emit_float_literal(builder, expr->type, val, (double)val);
            }
            else if (expr->type->bit_size == 64)
            {
                assert(false);
                // double val = expr->float_literal.r64;
                //result = bytecode_emit_float_literal(builder, expr->type, (float)val, val);

            }
            else assert(false);
        }
        if (expr->kind == AST_Expression_Kind::INTEGER_LITERAL)
        {
            switch (expr->type->bit_size)
            {
                case 8: assert(false);
                case 16: assert(false);
                case 32: 
                {
                    float val = expr->integer_literal.s64;
                    result = bytecode_emit_float_literal(builder, expr->type, val, (double)val);
                    break;
                }
                case 64: assert(false);
                default: assert(false);
            }
        }

        assert(result);
        return result;
    }

    Bytecode_Value *bytecode_emit_float_literal(Bytecode_Builder *builder, AST_Type *type,
                                                float f, double d)
    {
        assert(type->kind == AST_Type_Kind::FLOAT);

        bytecode_emit_load_float(builder, type->bit_size);
        
        if (type->bit_size == 32)
        {
            uint32_t val;
            float *val_p = (float*)&val;
            *val_p = f;
            bytecode_emit_32(builder, val);
        }
        else if (type->bit_size == 64)
        {
            uint64_t val;
            double *val_p = (double*)&val;
            *val_p = d;
            bytecode_emit_64(builder, val);
        }
        else assert(false);

        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY, type);
        bytecode_push_local_temporary(builder, result);
        return result;

    }

    Bytecode_Value *bytecode_emit_integer_literal(Bytecode_Builder *builder,
                                                 AST_Expression *expr,
                                                 bool noload /*=false*/)
    {
        assert(expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
               expr->kind == AST_Expression_Kind::BOOL_LITERAL ||
               expr->kind == AST_Expression_Kind::CHAR_LITERAL ||
               expr->kind == AST_Expression_Kind::DOT);
        assert(expr->type->kind == AST_Type_Kind::INTEGER ||
               expr->type->kind == AST_Type_Kind::ENUM);

        if (expr->kind == AST_Expression_Kind::DOT)
        {
            auto decl = expr->dot.child_decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::CONSTANT);
            expr = decl->constant.init_expression;
            assert(expr);
        }

        auto type = expr->type;
        if (type->kind == AST_Type_Kind::ENUM)
        {
            type = type->enum_type.base_type;
        }

        Bytecode_Value *result = nullptr;
        switch (type->bit_size)
        {
            case 8: 
            {
                result = bytecode_emit_integer_literal(builder, expr->type,
                                                      (int8_t)expr->integer_literal.s64,
                                                      noload);
                break;
            }

            case 16:
            {
                result = bytecode_emit_integer_literal(builder, expr->type,
                                                      expr->integer_literal.s16,
                                                      noload);
                break;
            }

            case 32:
            {
                result = bytecode_emit_integer_literal(builder, expr->type,
                                                      expr->integer_literal.s32,
                                                      noload);
                break;
            }

            case 64:
            {
                result = bytecode_emit_integer_literal(builder, expr->type,
                                                      expr->integer_literal.s64,
                                                      noload);
                break;
            }

            default: assert(false);
        }

        assert(result || noload);
        return result;

    }

    Bytecode_Value *bytecode_emit_integer_literal(Bytecode_Builder *builder, AST_Type *type,
                                                 int64_t val, bool noload /*=false*/)
    {
        assert(type->kind == AST_Type_Kind::INTEGER ||
               type->kind == AST_Type_Kind::BOOL ||
               type->kind == AST_Type_Kind::ENUM);

        AST_Type *value_type = type;
        if (type->kind == AST_Type_Kind::ENUM)
        {
            value_type = type;
            type = type->enum_type.base_type; 
        }

        assert(type->kind == AST_Type_Kind::INTEGER ||
               type->kind == AST_Type_Kind::BOOL);

        if (!noload)
        {
            bytecode_emit_load_int(builder, type->integer.sign, type->bit_size);
        }
        else
        {
            bytecode_emit_size_spec(builder, type->integer.sign, false, type->bit_size);
        }

        switch (type->bit_size)
        {
            case 8:
            {
                bytecode_emit_byte(builder, val);
                break; 
            }

            case 32:
            {
                bytecode_emit_32(builder, val);
                break; 
            }

            case 64:
            {
                bytecode_emit_64(builder, val);
                break;
            }

            default: assert(false);
        }

        if (!noload)
        {
            auto result = bytecode_new_value(builder, Bytecode_Value_Kind::TEMPORARY,
                                             value_type);
            bytecode_push_local_temporary(builder, result);
            return result;
        }
        else
        {
            return nullptr;
        }
    }

    Bytecode_Value *bytecode_emit_bool_literal(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::BOOL_LITERAL);
        assert(expr->type->kind == AST_Type_Kind::BOOL);
        assert(expr->type == Builtin::type_bool);

        assert(expr->type->bit_size == 8);

        bytecode_emit_instruction(builder, Bytecode_Instruction::LOAD_BOOL);
        bytecode_emit_byte(builder, expr->bool_literal.value);

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
        assert(builder->insert_block);

        bytecode_emit_byte(builder, (uint8_t)op);

        builder->insert_block->last_instruction = op;
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

    Bytecode_Block *bytecode_builder_append_block(Bytecode_Builder *builder,
                                                  Bytecode_Function *func,
                                                  Bytecode_Block *block)
    {
        block->index = func->blocks.count;
        block->name = bytecode_get_unique_block_name(builder, func, block);

        array_append(&func->blocks, block);

        func->last_block = block;

        return block;
    }

    Bytecode_Block *bytecode_builder_append_block(Bytecode_Builder *builder,
                                                  Bytecode_Function *func, const char *name)
    {
        auto block = bytecode_new_block(builder, name);
        return bytecode_builder_append_block(builder, func, block);
    }

    Atom bytecode_get_unique_block_name(Bytecode_Builder *builder, Bytecode_Function *func,
                                        Bytecode_Block *block)
    {
        auto ta = temp_allocator_get();

        auto result = block->name;

        int name_count = 0;
        for (int64_t i = 0; i < func->blocks.count; i++)
        {
            if (func->blocks[i]->name == result)
            {
                name_count++;
                auto name_sr = string_ref(block->name.data, block->name.length);
                auto num_str = string_from_int(ta, name_count);
                auto str = string_append(ta, name_sr, num_str);
                result = atom_get(&builder->build_data->atom_table, str.data, str.length);
            }
        }

        return result;
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

        if (decl->decl_flags & AST_DECL_FLAG_GLOBAL)
        {
            for (int64_t i = 0; i < builder->program.globals.count; i++)
            {
                if (builder->program.globals[i].decl == decl)
                {
                    return builder->program.globals[i].value;
                }
            }
        }
        else
        {
            assert(builder->current_function);
            auto func = builder->current_function;

            for (int64_t i = 0; i < func->local_allocs.count; i++)
            {
                if (func->local_allocs[i].ast_decl == decl)
                {
                    return func->local_allocs[i].value;
                }
            }
        }

        assert(false);
        return nullptr;
    }

    void bytecode_push_break_block(Bytecode_Builder *builder, Bytecode_Block *break_block)
    {
        assert(builder);
        assert(break_block);
        stack_push(&builder->break_block_stack, break_block);
    }

    void bytecode_pop_break_block(Bytecode_Builder *builder)
    {
        assert(builder);
        assert(stack_count(&builder->break_block_stack));
        stack_pop(&builder->break_block_stack);
    }

    void bytecode_record_jump(Bytecode_Builder *builder, Bytecode_Block *target_block,
                              int64_t offset)
    {
        auto current_block = builder->insert_block;
        array_append(&builder->jump_records, { current_block, target_block, offset });
    }

    bool bytecode_block_ends_with_terminator(Bytecode_Block *block)
    {
        assert(block);

        if (!block->instructions.count)
            return false;

        auto op = block->last_instruction;

        switch (op)
        {
            case Bytecode_Instruction::EXIT:
            case Bytecode_Instruction::RET:
            case Bytecode_Instruction::RET_VOID:
            case Bytecode_Instruction::JUMP:
            case Bytecode_Instruction::JUMP_IF: return true;

            default: return false;
        }
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

        result->pointer = nullptr;

        return result;
    }

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name)
    {
        Bytecode_Block *result = alloc_type<Bytecode_Block>(builder->allocator);

        result->name = atom_get(&builder->build_data->atom_table, name);
        result->index = -1;
        result->local_temp_count = 0;
        result->preceding_temp_count = 0;
        array_init(builder->allocator, &result->instructions);

        return result;
    }

    Bytecode_Value *bytecode_new_value(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type)
    {
        auto result = alloc_type<Bytecode_Value>(builder->allocator);
        result->kind = kind;
        result->is_const = false;
        result->type = type;
        result->local_index = 0;

        return result;
    }

    Bytecode_Value *bytecode_new_zero_value(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                            AST_Type *type)
    {
        auto result = bytecode_new_value(builder, kind, type);

        switch (type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                result->value.integer = {};
                result->is_const = true;
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER:
            {
                result->value.pointer = nullptr;
                result->is_const = true;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: assert(false);
        }

        return result;
    }

    Bytecode_Value *bytecode_new_value_from_const_value(Bytecode_Builder *builder,
                                                        const Const_Value &const_val)
    {
        auto result = bytecode_new_value(builder, Bytecode_Value_Kind::INVALID, const_val.type);

        switch (const_val.type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                result->value.integer.s64 = const_val.integer.s64;
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: assert(false);
        }
        
        result->is_const = true;
        return result;
    }

    AST_Type *bytecode_type_from_size_spec(Bytecode_Size_Specifier sp)
    {
        switch (sp)
        {
            case Bytecode_Size_Specifier::INVALID: assert(false);
            case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
            case Bytecode_Size_Specifier::FLOAT_FLAG: assert(false);
            case Bytecode_Size_Specifier::BIT_SIZE_MASK: assert(false);
            case Bytecode_Size_Specifier::U8: return Builtin::type_u8;
            case Bytecode_Size_Specifier::S8: return Builtin::type_s8;
            case Bytecode_Size_Specifier::U16: return Builtin::type_u16;
            case Bytecode_Size_Specifier::S16: return Builtin::type_s16;
            case Bytecode_Size_Specifier::U32: return Builtin::type_u32;
            case Bytecode_Size_Specifier::S32: return Builtin::type_s32;
            case Bytecode_Size_Specifier::R32: return Builtin::type_float;
            case Bytecode_Size_Specifier::U64: return Builtin::type_u64;
            case Bytecode_Size_Specifier::S64: return Builtin::type_s64; 
            case Bytecode_Size_Specifier::R64: return Builtin::type_double;
        }

        return nullptr;
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

        result.instruction_index = 0;

        result.local_temp_index = 0;
        result.local_alloc_index = 0;

        return result;
    }

    Bytecode_Iterator bytecode_iterator_create(Bytecode_Builder *builder, Bytecode_Function *func)
    {
        assert(builder);

        Bytecode_Iterator result = {};
        result.builder = builder;

        stack_init(builder->allocator, &result.arg_stack);

        assert(builder->program.functions.count > func->index);
        result.function_index = func->index;

        assert(builder->program.functions[0]->blocks.count >= 0);
        result.block_index = 0;

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

#ifndef NDEBUG
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

    uint16_t bytecode_iterator_fetch_16(Bytecode_Iterator *bci)
    {
        auto block = bytecode_iterator_get_block(bci);
        uint16_t result = *((uint16_t*)(&block->instructions[bci->instruction_index]));

        bytecode_iterator_advance_ip(bci, 2);

        return result;
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


        for (int64_t i = 0; i < builder->program.globals.count; i++)
        {
            auto &glob = builder->program.globals[i];
            auto name = glob.decl->identifier->atom;
            string_builder_appendf(sb, "%%%s: ", name.data);
            ast_print_type(sb, glob.value->type);
            string_builder_append(sb, " = ");
            bytecode_print_const_val(sb, glob.value);
            string_builder_append(sb, "\n");
        }

        string_builder_append(sb, "\n");

        auto bci = bytecode_iterator_create(builder);
        while (bci.function_index < builder->program.functions.count)
        {
            bytecode_print_function(sb, &bci);
            string_builder_append(sb, "\n");

            bytecode_iterator_advance_function(&bci);
        }

        bytecode_iterator_free(&bci);
    }

    void bytecode_print_const_val(String_Builder *sb, Bytecode_Value *val)
    {
        assert(val->is_const);

        auto type = val->type;
        switch (type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);
            case AST_Type_Kind::INTEGER:
            {
                bool sign = type->integer.sign;
                switch (type->bit_size)
                {
                    case 64:
                    {
                        if (sign)
                            string_builder_appendf(sb, "%" PRId64, val->value.integer.s64);
                        else
                            string_builder_appendf(sb, "%" PRIu64, val->value.integer.u64);
                        break;
                    }
                    default: assert(false); 
                }
                break;
            }

            case AST_Type_Kind::FLOAT: assert(false);
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::ENUM: assert(false);

            case AST_Type_Kind::ARRAY: assert(false);
        }
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

            case Bytecode_Instruction::LOAD_FLOAT:
            {
                string_builder_appendf(sb, "%%%" PRId64, bci->local_temp_index);
                bci->local_temp_index += 1;

                string_builder_append(sb, " = LOAD_FLOAT ");
                bytecode_print_im_float(sb, bci);
                break;
            }

            case Bytecode_Instruction::LOAD_INT:
            {
                string_builder_appendf(sb, "%%%" PRId64, bci->local_temp_index);
                bci->local_temp_index += 1;


                string_builder_append(sb, " = LOAD_INT ");
                bytecode_print_im_int(sb, bci);
                break;
            }

            case Bytecode_Instruction::LOADG: 
            {
                uint32_t glob_idx = bytecode_iterator_fetch_32(bci);

                auto value = bci->builder->program.globals[glob_idx].value;
                auto name = value->name;

                string_builder_appendf(sb, "%%%" PRId64 " = LOADG %%%s", bci->local_temp_index,
                                       name.data);
                bci->local_temp_index += 1;
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

            case Bytecode_Instruction::LOAD_BOOL:
            {
                bool value = bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);
                string_builder_appendf(sb, "%%%" PRId64 " = LOAD_BOOL %s",
                                       bci->local_temp_index++,
                                       value ? "true" : "false");
                break;
            }

            case Bytecode_Instruction::LOAD_STR:
            {
                uint32_t str_index = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = LOAD_STR %" PRIu32 " (\"",
                                       bci->local_temp_index++, str_index);
                auto str = bci->builder->program.strings[str_index]; 
                for (uint64_t i = 0; i < str.length; i++)
                {
                    char c;
                    if (parser_make_escape_char(str.data[i], &c))
                    {
                        string_builder_appendf(sb, "\\%c", c);
                    }
                    else
                    {
                        string_builder_appendf(sb, "%c", c);
                    }
                }
                string_builder_append(sb, "\")");
                break;
            }

            case Bytecode_Instruction::LOAD_NULL:
            {
                /*uint32_t type_idx =*/ bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = LOAD_NULL",
                                       bci->local_temp_index++);
                break;
            };
            
            case Bytecode_Instruction::STOREG:
            {
                uint32_t glob_index = bytecode_iterator_fetch_32(bci);
                uint32_t val_index = bytecode_iterator_fetch_32(bci);

                auto name = bci->builder->program.globals[glob_index].value->name;

                string_builder_appendf(sb, "STOREG %%%s %%%" PRIu32, name.data,
                                       val_index);
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

            case Bytecode_Instruction::STORE_PARAM:
            {
                uint32_t param_idx = bytecode_iterator_fetch_32(bci);
                uint32_t val_idx = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];
                auto name = func->parameters[param_idx].value->name;

                string_builder_appendf(sb, "STORE_PARAM %%%s %%%" PRIu32,
                                       name.data, val_idx);
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

            case Bytecode_Instruction::DEREF:
            {
                auto tmp_idx = bytecode_iterator_fetch_32(bci);
                string_builder_appendf(sb, "%%%" PRIu64 " = DEREF %%%" PRIu32,
                                       bci->local_temp_index++, tmp_idx);
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

            case Bytecode_Instruction::EQ:
            case Bytecode_Instruction::NEQ:
            case Bytecode_Instruction::GT:
            case Bytecode_Instruction::GTEQ:
            case Bytecode_Instruction::LT:
            case Bytecode_Instruction::LTEQ:
            case Bytecode_Instruction::ADD:
            case Bytecode_Instruction::SUB:
            case Bytecode_Instruction::REM: 
            case Bytecode_Instruction::MUL:
            case Bytecode_Instruction::DIV:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);

                uint32_t lhs_idx = bytecode_iterator_fetch_32(bci);
                uint32_t rhs_idx = bytecode_iterator_fetch_32(bci);

                const char *op_name;
                if      (inst == Bytecode_Instruction::EQ)   op_name = "EQ";
                else if (inst == Bytecode_Instruction::NEQ)  op_name = "NEQ";
                else if (inst == Bytecode_Instruction::GT)   op_name = "GT";
                else if (inst == Bytecode_Instruction::GTEQ) op_name = "GTEQ";
                else if (inst == Bytecode_Instruction::LT)   op_name = "LT";
                else if (inst == Bytecode_Instruction::LTEQ) op_name = "LT";
                else if (inst == Bytecode_Instruction::ADD)  op_name = "ADD";
                else if (inst == Bytecode_Instruction::SUB)  op_name = "SUB";
                else if (inst == Bytecode_Instruction::REM)  op_name = "REM";
                else if (inst == Bytecode_Instruction::MUL)  op_name = "MUL";
                else if (inst == Bytecode_Instruction::DIV)  op_name = "DIV";
                else assert(false);

                string_builder_appendf(sb, "%%%" PRIu64 " = %s ", bci->local_temp_index,
                                       op_name);
                bytecode_print_size_spec(sb, size_spec);
                string_builder_appendf(sb, " %%%" PRIu32 " %%%" PRIu32, lhs_idx, rhs_idx);
                bci->local_temp_index += 1;
                break;
            }

            case Bytecode_Instruction::NEG:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);

                auto op_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRIu64 " = NEG ", bci->local_temp_index++);
                bytecode_print_size_spec(sb, size_spec);
                string_builder_appendf(sb, " %%%" PRIu32, op_idx);
                break;
            }

            case Bytecode_Instruction::JUMP:
            {
                auto block_idx = bytecode_iterator_fetch_32(bci);
                auto func = bci->builder->program.functions[bci->function_index];
                auto block = func->blocks[block_idx];
                string_builder_appendf(sb, "JUMP %s", block->name);
                break;
            }

            case Bytecode_Instruction::JUMP_IF:
            {
                auto tmp_idx = bytecode_iterator_fetch_32(bci);
                auto block_idx = bytecode_iterator_fetch_32(bci);
                auto func = bci->builder->program.functions[bci->function_index];
                auto block = func->blocks[block_idx];
                string_builder_appendf(sb, "JUMP_IF %%% " PRIu32 ", %s",
                                       tmp_idx, block->name);
                break;
            }

            case Bytecode_Instruction::SWITCH:
            {
                auto type_index = bytecode_iterator_fetch_32(bci);
                auto type = bci->builder->emitted_types[type_index];

                auto tmp_idx = bytecode_iterator_fetch_32(bci);
                string_builder_appendf(sb, "SWITCH %%%" PRIu32, tmp_idx);

                auto case_count = bytecode_iterator_fetch_32(bci);
                assert(case_count);

                auto func = bci->builder->program.functions[bci->function_index];

                auto block_idx = bytecode_iterator_fetch_32(bci);
                auto block = func->blocks[block_idx];
                string_builder_appendf(sb, "\n      default -> %s", block->name);

                for (int64_t i = 0; i < case_count; i++)
                {
                    string_builder_append(sb, "\n      ");
                    if (type->kind == AST_Type_Kind::INTEGER)
                    {
                        bytecode_print_im_int(sb, bci);
                    }
                    else 
                    {
                        assert(type->kind == AST_Type_Kind::ENUM);
                        bytecode_print_im_enum(sb, bci, type);
                    }

                    auto block_idx = bytecode_iterator_fetch_32(bci);
                    auto block = func->blocks[block_idx];
                    string_builder_appendf(sb, "   -> %s", block->name);
                }


                break;
            }

            case Bytecode_Instruction::CAST_INT:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);
                auto val_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = CAST_INT ",
                                       bci->local_temp_index++);
                bytecode_print_size_spec(sb, size_spec);
                string_builder_appendf(sb, " %%%" PRIu32, val_idx);
                break;
            }

            case Bytecode_Instruction::CAST_ENUM:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);
                auto val_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = CAST_ENUM ",
                                       bci->local_temp_index++);
                bytecode_print_size_spec(sb, size_spec);
                string_builder_appendf(sb, " %%%" PRIu32, val_idx);
                break;
            }

            case Bytecode_Instruction::CAST_FLOAT:
            {
                auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);
                auto val_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = CAST_FLOAT ",
                                       bci->local_temp_index++);
                bytecode_print_size_spec(sb, size_spec); 
                string_builder_appendf(sb, " %%%" PRIu32, val_idx);
                break;
            }

            case Bytecode_Instruction::CAST_POINTER:
            {
                /*auto type_idx =*/ bytecode_iterator_fetch_32(bci);
                auto val_idx = bytecode_iterator_fetch_32(bci);

                string_builder_appendf(sb, "%%%" PRId64 " = CAST_POINTER %%%",
                                       bci->local_temp_index, val_idx);
                break;
            }

            case Bytecode_Instruction::SYSCALL:
            {
                auto arg_count = bytecode_iterator_fetch_32(bci);
                assert(arg_count);
                string_builder_appendf(sb, "SYSCALL(");
                for (int64_t i = 0; i < arg_count; i++)
                {
                    if (i) string_builder_append(sb, ", ");
                    auto arg_val = stack_peek(&bci->arg_stack, (arg_count - 1) - i);

                    assert(arg_val->kind == Bytecode_Value_Kind::TEMPORARY);
                    string_builder_appendf(sb, "%%%" PRIu32, arg_val->local_index);
                }
                string_builder_append(sb, ")");
                for (int64_t i = 0; i < arg_count; i++) stack_pop(&bci->arg_stack);
                break;
            }

            case Bytecode_Instruction::AGG_OFFSET_PTR:
            {
                auto kind = (Bytecode_Value_Type_Specifier)bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);

                auto store_idx = bytecode_iterator_fetch_32(bci);
                auto offset_val_idx = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];

                string_builder_appendf(sb, "%%%" PRIu64 " = AGG_OFFSET_PTR ",
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

                string_builder_appendf(sb, ", %%%" PRIu32, offset_val_idx);
                break;
            }

            case Bytecode_Instruction::ARR_OFFSET_PTR:
            {
                auto kind = (Bytecode_Value_Type_Specifier)bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);

                auto store_idx = bytecode_iterator_fetch_32(bci);
                auto offset_val_idx = bytecode_iterator_fetch_32(bci);

                auto func = bci->builder->program.functions[bci->function_index];

                string_builder_appendf(sb, "%%%" PRIu64 " = ARR_OFFSET_PTR ",
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

                string_builder_appendf(sb, ", %%%" PRIu32, offset_val_idx);
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

            case Bytecode_Size_Specifier::U8:
            {
                string_builder_append(sb, "U8");
                break;
            }

            case Bytecode_Size_Specifier::S8:
            {
                string_builder_append(sb, "S8");
                break;
            }

            case Bytecode_Size_Specifier::U16: assert(false);
            case Bytecode_Size_Specifier::S16: assert(false);

            case Bytecode_Size_Specifier::U32:
            {
                string_builder_append(sb, "U32");
                break;
            }

            case Bytecode_Size_Specifier::S32:
            {
                string_builder_append(sb, "S32");
                break;
            }

            case Bytecode_Size_Specifier::R32:
            {
                string_builder_append(sb, "R32");
                break;
            }

            case Bytecode_Size_Specifier::U64:
            {
                string_builder_append(sb, "U64");
                break;
            }

            case Bytecode_Size_Specifier::S64:
            {
                string_builder_append(sb, "S64");
                break;
            }

            case Bytecode_Size_Specifier::R64:
            {
                string_builder_append(sb, "R64");
                break;
            }

            default: assert(false);
        }
    }

    void bytecode_print_im_int(String_Builder *sb, Bytecode_Iterator *bci)
    {
        assert(sb);

        auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);

        bytecode_print_size_spec(sb, size_spec); 

        switch (size_spec)
        {
            case Bytecode_Size_Specifier::INVALID: assert(false);
            case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

            case Bytecode_Size_Specifier::U8: 
            {
                uint8_t val = bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);
                string_builder_appendf(sb, " %" PRIu8, val);

                if (val > 9)
                {
                    string_builder_append(sb, " ('");
                    char c;
                    if (parser_make_escape_char(val, &c))
                    {
                        string_builder_appendf(sb, "\\%c", c);
                    }
                    else
                    {
                        string_builder_appendf(sb, "%c", c);
                    }
                    string_builder_append(sb, "')");
                }
                break;
            }

            case Bytecode_Size_Specifier::S8: assert(false);
            case Bytecode_Size_Specifier::U16: assert(false);
            case Bytecode_Size_Specifier::S16: assert(false);

            case Bytecode_Size_Specifier::U32:
            {
                uint32_t val = bytecode_iterator_fetch_32(bci);
                string_builder_appendf(sb, " %" PRIu32, val);
                break;
            }

            case Bytecode_Size_Specifier::S32: assert(false);

            case Bytecode_Size_Specifier::U64:
            {
                uint64_t val = bytecode_iterator_fetch_64(bci);
                string_builder_appendf(sb, " %" PRIu64, val);
                break;
            }

            case Bytecode_Size_Specifier::S64:
            {
                int64_t val = bytecode_iterator_fetch_64(bci);
                string_builder_appendf(sb, " %" PRId64, val);
                break;
            }
            default: assert(false);
        }
    }

    void bytecode_print_im_enum(String_Builder *sb, Bytecode_Iterator *bci,
                                AST_Type *type)
    {
#ifndef NDEBUG
        auto size_spec = (Bytecode_Size_Specifier)
#endif
            bytecode_iterator_fetch_16(bci);



        assert(((uint16_t)size_spec &
                (uint16_t)Bytecode_Size_Specifier::BIT_SIZE_MASK) == type->bit_size);

        uint64_t val = 0;
        switch (type->bit_size)
        {
            case 8:
            {
                val = bytecode_iterator_fetch_byte(bci);
                bytecode_iterator_advance_ip(bci);
                break;
            }

            case 16: val = bytecode_iterator_fetch_16(bci); break;
            case 32: val = bytecode_iterator_fetch_32(bci); break;
            case 64: val = bytecode_iterator_fetch_64(bci); break;
            default: assert(false);
        }

        auto enum_decl = type->enum_type.declaration;

        const char* mem_name = "<member_name_not_found>";

        for (int64_t i = 0; i < enum_decl->enum_decl.member_declarations.count; i++)
        {
            auto mem_decl = enum_decl->enum_decl.member_declarations[i];
            auto mem_expr = mem_decl->constant.init_expression;
            Const_Value mem_val = const_interpret_expression(mem_expr);

            if (mem_val.integer.u64 == val)
            {
                mem_name = mem_decl->identifier->atom.data;
            }
        }

        string_builder_appendf(sb, "%s.%s", enum_decl->identifier->atom.data, mem_name);
    }

    void bytecode_print_im_float(String_Builder *sb, Bytecode_Iterator *bci)
    {
        auto size_spec = (Bytecode_Size_Specifier)bytecode_iterator_fetch_16(bci);

        if (size_spec == Bytecode_Size_Specifier::R32)
        {
            uint32_t raw_val = bytecode_iterator_fetch_32(bci);
            float *val_ptr = (float *)&raw_val;
            float val = *val_ptr;

            string_builder_appendf(sb, "float %f", val);
        }
        else if (size_spec == Bytecode_Size_Specifier::R64)
        {
            uint64_t raw_val = bytecode_iterator_fetch_64(bci);
            double *val_ptr = (double *)&raw_val;
            double val = *val_ptr;
            string_builder_append(sb, "double %f", val);
        }
        else assert(false);
    }
}
