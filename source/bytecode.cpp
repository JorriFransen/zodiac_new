#include "bytecode.h"

#include "ast.h"
#include "builtin.h"
#include "const_interpreter.h"
#include "parser.h"
#include "string_builder.h"
#include "temp_allocator.h"

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
        array_init(allocator, &result.globals);

        result.global_data_size = 0;

        array_init(allocator, &result.parameters);
        array_init(allocator, &result.locals);

        stack_init(allocator, &result.break_block_stack);

        result.next_temp_index = 0;

        return result;
    }

    Bytecode_Function *bytecode_register_function(Bytecode_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func_type = decl->type;
        if (!func_type) {
            assert(decl->function.type_spec && decl->function.type_spec->type);
            func_type = decl->function.type_spec->type;
        }
        assert(func_type);

        auto ex_func = bytecode_find_function(builder, decl);

        if (ex_func)
        {
            assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);
            assert(false);
        }

        assert(!(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE));

        assert(decl->identifier);
        auto name = decl->identifier->atom;

        Bytecode_Function *result = bytecode_new_function(builder, func_type, name);
        if (decl->decl_flags & AST_DECL_FLAG_NORETURN)
        {
            result->flags |= BC_FUNC_FLAG_NORETURN;
        }

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
        {
            auto param_decl = decl->function.parameter_declarations[i];
            assert(param_decl->kind == AST_Declaration_Kind::PARAMETER);

            bytecode_parameter_new(builder, result, param_decl->type, param_decl->identifier->atom);
        }


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
        if (decl->decl_flags & AST_DECL_FLAG_FOREIGN)
        {
            func->flags |= BC_FUNC_FLAG_FOREIGN;
        }

        builder->current_function = func;
        builder->parameters.count = 0;
        builder->locals.count = 0;
        builder->next_temp_index = 0;

        if (func->flags & BC_FUNC_FLAG_FOREIGN) {
            assert(false && "register the func here, so we can load it on startup...");
            return func;
        }

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++)
        {
            auto param_decl = decl->function.parameter_declarations[i];
            auto param_val = func->parameters[i];
            array_append(&builder->parameters, { param_decl, param_val });
        }


        Bytecode_Block *entry_block = bytecode_new_block(builder, "entry");
        bytecode_append_block(builder, func, entry_block);

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

    Bytecode_Global_Info bytecode_emit_global_variable(Bytecode_Builder *builder,
                                                  AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);
        assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);

        Bytecode_Value *global_value = bytecode_global_new(builder, decl->type,
                                                           decl->identifier->atom);

        Const_Value init_const_val = {};
        bool has_initializer = false;

        if (decl->variable.init_expression) {
            auto init_expr = decl->variable.init_expression;
            assert(init_expr->expr_flags & AST_EXPR_FLAG_CONST);

            init_const_val = const_interpret_expression(init_expr);
            has_initializer = true;
        }

        Bytecode_Global_Info global_info = {
            .declaration = decl,
            .global_value = global_value,
            .init_const_val = init_const_val,
            .has_initializer = has_initializer,
        };

        array_append(&builder->globals, global_info);

        auto type = decl->type;
        assert(type->bit_size % 8 == 0);
        auto byte_size = type->bit_size / 8;

        global_value->global.byte_offset = builder->global_data_size;
        builder->global_data_size += byte_size;

        return global_info;
    }

    Bytecode_Block *bytecode_new_block(Bytecode_Builder *builder, const char *name)
    {
        Bytecode_Block *result = alloc_type<Bytecode_Block>(builder->allocator);
        result->name = atom_get(&builder->build_data->atom_table, name);
        result->function = nullptr;
        array_init(builder->allocator, &result->instructions);
        return result;
    }

    void bytecode_append_block(Bytecode_Builder *builder, Bytecode_Function *function,
                               Bytecode_Block *block)
    {
        assert(builder->current_function == function);
        assert(!block->function);

        block->name = bytecode_get_unique_block_name(builder, block->name);

        array_append(&function->blocks, block);
        block->function = function;
    }

    Atom bytecode_get_unique_block_name(Bytecode_Builder *builder, Atom name)
    {
        auto function = builder->current_function;
        auto ta = temp_allocator_get();

        auto result = name;

        int name_count = 0;
        for (int64_t i = 0; i < function->blocks.count; i++) {
            if (function->blocks[i]->name == result) {
                name_count++;
                auto name_sr = string_ref(name.data, name.length);
                auto num_str = string_from_int(ta, name_count);
                auto str = string_append(ta, name_sr, num_str);
                result = atom_get(&builder->build_data->atom_table, str.data, str.length);
            }
        }

        return result;
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

            case AST_Declaration_Kind::CONSTANT: break;

            case AST_Declaration_Kind::PARAMETER: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::FUNCTION: assert(false);

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

            case AST_Statement_Kind::ASSIGNMENT: {
                auto ident_expr = stmt->assignment.identifier_expression;
                auto rhs_expr = stmt->assignment.rhs_expression;

                Bytecode_Value *dest = bytecode_emit_lvalue(builder, ident_expr);
                Bytecode_Value *source = bytecode_emit_expression(builder, rhs_expr);

                bytecode_emit_store(builder, dest, source);
                break;
            }

            case AST_Statement_Kind::RETURN: {
                if (stmt->expression) {
                    auto ret_val = bytecode_emit_expression(builder, stmt->expression);
                    bytecode_emit_instruction(builder, RETURN, ret_val, nullptr, nullptr);
                } else {
                    bytecode_emit_instruction(builder, RETURN_VOID, nullptr, nullptr, nullptr);
                }
                break;
            }

            case AST_Statement_Kind::BREAK: {
                assert(stack_count(&builder->break_block_stack));
                Bytecode_Block *break_block = stack_top(&builder->break_block_stack);
                bytecode_emit_jump(builder, break_block);
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
                if (stmt->declaration->kind != AST_Declaration_Kind::FUNCTION) {
                    bytecode_emit_declaration(builder, stmt->declaration);
                }
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                bytecode_emit_expression(builder, stmt->expression);
                break;
            }

            case AST_Statement_Kind::WHILE: {
                auto cond_block = bytecode_new_block(builder, "while_cond");
                auto body_block = bytecode_new_block(builder, "while_body");
                auto post_while_block = bytecode_new_block(builder, "post_while");

                bytecode_append_block(builder, builder->current_function, cond_block);

                bytecode_emit_jump(builder, cond_block);
                bytecode_set_insert_point(builder, cond_block);

                auto cond_val = bytecode_emit_expression(builder, stmt->while_stmt.cond_expr);

                bytecode_emit_jump_if(builder, cond_val, body_block, post_while_block);

                bytecode_append_block(builder, builder->current_function, body_block);
                bytecode_set_insert_point(builder, body_block);

                bytecode_push_break_block(builder, post_while_block);
                bytecode_emit_statement(builder, stmt->while_stmt.body);
                bytecode_pop_break_block(builder);

                bytecode_emit_jump(builder, cond_block);

                bytecode_append_block(builder, builder->current_function, post_while_block);
                bytecode_set_insert_point(builder, post_while_block);

                break;
            }

            case AST_Statement_Kind::FOR: {
                bytecode_emit_for_statement(builder, stmt);
                break;
            }

            case AST_Statement_Kind::IF: {
                bytecode_emit_if_statement(builder, stmt);
                break;
            }

            case AST_Statement_Kind::SWITCH:
            {
                auto switch_expr = stmt->switch_stmt.expression;
                Bytecode_Value *switch_val = bytecode_emit_expression(builder, switch_expr);

                Bytecode_Block *default_block = nullptr;
                Bytecode_Block *post_switch_block = bytecode_new_block(builder, "post_switch");

                auto func = builder->current_function;

                Bytecode_Instruction *switch_inst = bytecode_emit_instruction(builder, SWITCH,
                                                                              switch_val,
                                                                              nullptr,
                                                                              nullptr);
                Bytecode_Value *switch_data =
                    bytecode_value_new(builder, Bytecode_Value_Kind::SWITCH_DATA, nullptr);
                switch_inst->b = switch_data;
                array_init(builder->allocator, &switch_data->switch_data.cases,
                           stmt->switch_stmt.cases.count);

                auto ta = temp_allocator_get();

                Array<Bytecode_Block *> case_blocks = {};
                array_init(ta, &case_blocks, stmt->switch_stmt.cases.count);

                for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++) {
                    AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];

                    const char *block_name = (switch_case->is_default ? "default" : "case");
                    Bytecode_Block *case_block = bytecode_new_block(builder, block_name);

                    bytecode_append_block(builder, func, case_block);
                    if (switch_case->is_default) {
                        assert(!default_block);
                        default_block = case_block;
                    }

                    bytecode_set_insert_point(builder, case_block);
                    bytecode_push_break_block(builder, post_switch_block);

                    bytecode_emit_statement(builder, switch_case->body);
                    case_block = builder->insert_block;

                    bytecode_pop_break_block(builder);

                    if (!bytecode_block_ends_with_terminator(case_block)) {
                        bytecode_emit_jump(builder, post_switch_block);
                    }

                    array_append(&case_blocks, case_block);
                }

                if (!default_block) {
                    default_block = post_switch_block;
                }

                bool added_default = false;

                for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++)
                {
                    AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];
                    Bytecode_Block *case_block = case_blocks[i];

                    if (switch_case->is_default) {
                        bytecode_add_default_switch_case(switch_inst, case_block);
                        added_default = true;
                        continue;
                    }

                    for (int64_t expr_i = 0; expr_i < switch_case->expressions.count; expr_i++)
                    {
                        AST_Expression *expr = switch_case->expressions[expr_i];
                        assert(expr->expr_flags & AST_EXPR_FLAG_CONST);

                        Bytecode_Value *expr_val = bytecode_emit_expression(builder, expr);
                        bytecode_add_switch_case(switch_inst, expr_val, case_block);
                    }
                }

                if (!added_default) {
                    bytecode_add_default_switch_case(switch_inst, post_switch_block);
                }

                bytecode_append_block(builder, func, post_switch_block);
                bytecode_set_insert_point(builder, post_switch_block);

                break;
            }
        }
    }

    void bytecode_emit_for_statement(Bytecode_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::FOR);

        auto func = builder->current_function;

        Bytecode_Block *cond_block = bytecode_new_block(builder, "for_cond");
        Bytecode_Block *body_block = bytecode_new_block(builder, "for_body");
        Bytecode_Block *post_for_block = bytecode_new_block(builder, "post_for");

        for (int64_t i = 0; i < stmt->for_stmt.init_statements.count; i++) {
            auto init_stmt = stmt->for_stmt.init_statements[i];
            bytecode_emit_statement(builder, init_stmt);
        }
        bytecode_emit_jump(builder, cond_block);

        bytecode_append_block(builder, func, cond_block);
        bytecode_set_insert_point(builder, cond_block);

        Bytecode_Value *cond_val = bytecode_emit_expression(builder, stmt->for_stmt.cond_expr);
        bytecode_emit_jump_if(builder, cond_val, body_block, post_for_block);

        bytecode_append_block(builder, func, body_block);
        bytecode_set_insert_point(builder, body_block);

        if (stmt->for_stmt.it_decl) {
            auto it_allocl = bytecode_find_variable(builder, stmt->for_stmt.it_decl);
            auto it_init_expr = stmt->for_stmt.it_decl->variable.init_expression;
            auto it_val = bytecode_emit_expression(builder, it_init_expr);
            bytecode_emit_store(builder, it_allocl, it_val);
        }

        bytecode_emit_statement(builder, stmt->for_stmt.body_stmt);
        for (int64_t i = 0; i < stmt->for_stmt.step_statements.count; i++) {
            auto step_stmt = stmt->for_stmt.step_statements[i];
            bytecode_emit_statement(builder, step_stmt);
        }
        bytecode_emit_jump(builder, cond_block);

        bytecode_append_block(builder, func, post_for_block);
        bytecode_set_insert_point(builder, post_for_block);
    }

    void bytecode_emit_if_statement(Bytecode_Builder *builder, AST_Statement *stmt) {

        assert(stmt->kind == AST_Statement_Kind::IF);

        auto cond_expr = stmt->if_stmt.cond_expr;
        auto then_stmt = stmt->if_stmt.then_stmt;
        auto else_stmt = stmt->if_stmt.else_stmt;

        auto cond_val = bytecode_emit_expression(builder, cond_expr);
        assert(cond_val->type->kind == AST_Type_Kind::BOOL);

        Bytecode_Block *then_block = bytecode_new_block(builder, "then");
        bytecode_append_block(builder, builder->current_function, then_block);

        Bytecode_Block *else_block = nullptr;
        if (else_stmt) {
            else_block = bytecode_new_block(builder, "else");
        }
        Bytecode_Block *post_if_block = bytecode_new_block(builder, "post_if");

        bytecode_emit_jump_if(builder, cond_val, then_block,
                              else_block ? else_block : post_if_block);

        bytecode_set_insert_point(builder, then_block);
        bytecode_emit_statement(builder, then_stmt);
        then_block = builder->insert_block;

        if (!bytecode_block_ends_with_terminator(then_block)) {
            bytecode_emit_jump(builder, post_if_block);
        }

        if (else_stmt) {
            bytecode_append_block(builder, builder->current_function, else_block);
            bytecode_set_insert_point(builder, else_block);

            bytecode_emit_statement(builder, else_stmt);
            else_block = builder->insert_block;

            if (!bytecode_block_ends_with_terminator(else_block)) {
                bytecode_emit_jump(builder, post_if_block);
            }
        }

        bytecode_append_block(builder, builder->current_function, post_if_block);
        bytecode_set_insert_point(builder, post_if_block);

    }

    Bytecode_Value *bytecode_emit_expression(Bytecode_Builder *builder, AST_Expression *expr)
    {
        Bytecode_Value *result = nullptr;

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = expr->identifier->declaration;

                if (decl->kind == AST_Declaration_Kind::CONSTANT) {
                    assert(decl->constant.init_expression);
                    result = bytecode_emit_expression(builder, decl->constant.init_expression);
                } else {
                    Bytecode_Value *source_val = bytecode_emit_lvalue(builder, expr);
                    result = bytecode_emit_load(builder, source_val);
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::DOT: {

                if (expr->dot.child_decl &&
                    (expr->dot.child_decl->kind == AST_Declaration_Kind::CONSTANT)) {
                    auto decl = expr->dot.child_decl;
                    auto init_expr = decl->constant.init_expression;
                    result = bytecode_emit_expression(builder, init_expr);
                } else if (expr->expr_flags & AST_EXPR_FLAG_DOT_COUNT) {

                    auto parent_expr = expr->dot.parent_expression;
                    AST_Declaration *parent_decl = nullptr;

                    if (parent_expr->kind == AST_Expression_Kind::IDENTIFIER) {
                        parent_decl = parent_expr->identifier->declaration;
                    } else {
                        assert(false);
                    }

                    auto parent_type = parent_decl->type;
                    assert(parent_type);
                    assert(parent_type->kind == AST_Type_Kind::ARRAY);

                    auto element_count = parent_type->array.element_count;
                    Integer_Literal il = { .s64 = element_count };
                    result = bytecode_integer_literal_new(builder, expr->type, il);
                } else {
                    Bytecode_Value *lvalue = bytecode_emit_lvalue(builder, expr);
                    result = bytecode_emit_load(builder, lvalue);
                }

                assert(result);
                break;
            }

            case AST_Expression_Kind::BINARY: {

#define _binop_arithmetic(int_signed_op, int_unsigned_op, float_op) { \
    auto lhs_expr = expr->binary.lhs; \
    auto rhs_expr = expr->binary.rhs; \
    assert(lhs_expr->type == rhs_expr->type); \
    auto type = lhs_expr->type; \
    if (type->kind == AST_Type_Kind::INTEGER) { \
        assert(lhs_expr->type->integer.sign == rhs_expr->type->integer.sign); \
    } else { \
        assert(type->kind == AST_Type_Kind::FLOAT || type->kind == AST_Type_Kind::ENUM); \
    } \
    Bytecode_Value *lhs = bytecode_emit_expression(builder, lhs_expr); \
    Bytecode_Value *rhs = bytecode_emit_expression(builder, rhs_expr); \
    result = bytecode_temporary_new(builder, type); \
    if (type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::ENUM) { \
        if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
        if (type->integer.sign) { \
            bytecode_emit_instruction(builder, int_signed_op, lhs, rhs, result); \
        } else { \
            bytecode_emit_instruction(builder, int_unsigned_op, lhs, rhs, result); \
        } \
    } else if (type->kind == AST_Type_Kind::FLOAT) { \
        assert(float_op != NOP); \
        bytecode_emit_instruction(builder, float_op, lhs, rhs, result); \
    } else { \
        assert(false && "Non integer/float arithmetic not supported!!!"); \
    } \
    break; \
}

#define _binop_compare(int_sign_op, int_unsigned_op, float_op) { \
    auto lhs_expr = expr->binary.lhs; \
    auto rhs_expr = expr->binary.rhs; \
    assert(lhs_expr->type == rhs_expr->type); \
    auto type = lhs_expr->type; \
    auto lhs = bytecode_emit_expression(builder, lhs_expr); \
    auto rhs = bytecode_emit_expression(builder, rhs_expr); \
    result = bytecode_temporary_new(builder, Builtin::type_bool); \
    if (type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::ENUM) { \
        if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
        if (type->integer.sign) { \
            bytecode_emit_instruction(builder, int_sign_op, lhs, rhs, result); \
        } else { \
            bytecode_emit_instruction(builder, int_unsigned_op, lhs, rhs, result); \
        } \
    } else if (type->kind == AST_Type_Kind::FLOAT) { \
        bytecode_emit_instruction(builder, float_op, lhs, rhs, result); \
    } else {\
        assert(false && "Unsupported binop compare"); \
    } \
    break; \
}

                switch (expr->binary.op) {
                    case BINOP_INVALID: assert(false);

                    case BINOP_EQ:  _binop_compare(EQ_S, EQ_U, EQ_F);
                    case BINOP_NEQ:  _binop_compare(NEQ_S, NEQ_U, NEQ_F);
                    case BINOP_LT:   _binop_compare(LT_S, LT_U, LT_F);
                    case BINOP_LTEQ: _binop_compare(LTEQ_S, LTEQ_U, LTEQ_F);
                    case BINOP_GT:   _binop_compare(GT_S, GT_U, GT_F);
                    case BINOP_GTEQ: _binop_compare(GTEQ_S, GTEQ_U, GTEQ_F);

                    case BINOP_ADD: _binop_arithmetic(ADD_S, ADD_U, ADD_F);
                    case BINOP_SUB: _binop_arithmetic(SUB_S, SUB_U, SUB_F);
                    case BINOP_REMAINDER: _binop_arithmetic(REM_S, REM_U, NOP);
                    case BINOP_MUL: _binop_arithmetic(MUL_S, MUL_U, MUL_F);
                    case BINOP_DIV: _binop_arithmetic(DIV_S, DIV_U, DIV_F);

                }

#undef _binop_compare
#undef _binop_arithmetic
                break;
            }

            case AST_Expression_Kind::UNARY: {

                auto operand_expr = expr->unary.operand_expression;
                auto operand_type = operand_expr->type;

                switch (expr->unary.op)
                {
                    case UNOP_INVALID: assert(false);

                    case UNOP_DEREF:
                    {
                        assert(operand_type->kind == AST_Type_Kind::POINTER);
                        assert(expr->type == operand_type->pointer.base);

                        Bytecode_Value *ptr_val = bytecode_emit_expression(builder, operand_expr);
                        result = bytecode_emit_load(builder, ptr_val);
                        break;
                    }

                    case UNOP_MINUS: {
                        assert(operand_type->kind == AST_Type_Kind::INTEGER);
                        assert(operand_type->integer.sign);

                        Bytecode_Value *zero = bytecode_emit_zero_value(builder, operand_type);
                        Bytecode_Value *operand_value = bytecode_emit_expression(builder, operand_expr);

                        result = bytecode_temporary_new(builder, operand_type);
                        bytecode_emit_instruction(builder, SUB_S, zero, operand_value, result);

                        break;
                    }
                }
                break;
            }

            case AST_Expression_Kind::CALL: {
                result = bytecode_emit_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {
                result = bytecode_emit_builtin_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::ADDROF: {
                result = bytecode_emit_lvalue(builder, expr->addrof.operand_expr);
                if (result->kind == Bytecode_Value_Kind::ALLOCL) {
                    // assert(false);
                } else {
                    assert(result->kind == Bytecode_Value_Kind::TEMP);
                }
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::SUBSCRIPT: {
                auto lvalue = bytecode_emit_lvalue(builder, expr);
                result = bytecode_emit_load(builder, lvalue);
                break;
            }

            case AST_Expression_Kind::CAST: {
                result = bytecode_emit_cast(builder, expr);
                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL: {
                if (expr->type->kind == AST_Type_Kind::INTEGER) {
                    result = bytecode_integer_literal_new(builder, expr->type,
                                                          expr->integer_literal);
                } else if (expr->type->kind == AST_Type_Kind::ENUM) {
                    result = bytecode_integer_literal_new(builder, expr->type,
                                                          expr->integer_literal);
                } else if (expr->type->kind == AST_Type_Kind::FLOAT) {
                    result = bytecode_emit_float_literal(builder, expr);
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: {
                assert(expr->type->kind == AST_Type_Kind::FLOAT);
                result = bytecode_emit_float_literal(builder, expr);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: {
                result = bytecode_string_literal_new(builder, expr->string_literal.atom);
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: {
                Integer_Literal il = { .u8 = (uint8_t)expr->char_literal.c };
                result = bytecode_integer_literal_new(builder, Builtin::type_u8, il);
                break;
            }
            case AST_Expression_Kind::BOOL_LITERAL: {
                result = bytecode_bool_literal_new(builder, expr->type, expr->bool_literal.value);
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL:
            {
                result = bytecode_null_literal_new(builder, expr->type);
                break;
            }

            case AST_Expression_Kind::RANGE: assert(false); //@TODO: Implement!
        }

        assert(result || expr->type->kind == AST_Type_Kind::VOID);
        if (result) assert(result->type == expr->type);

        return result;
    }

    Bytecode_Value *bytecode_emit_lvalue(Bytecode_Builder *builder, AST_Expression *expr) {

        Bytecode_Value *result = nullptr;

        switch (expr->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = expr->identifier->declaration;
                assert(decl);

                Bytecode_Value *source_val = nullptr;
                if (decl->kind == AST_Declaration_Kind::VARIABLE) {
                    source_val = bytecode_find_variable(builder, decl);
                }
                else if (decl->kind == AST_Declaration_Kind::PARAMETER) {
                    source_val = bytecode_find_parameter(builder, decl);
                } else {
                    assert(false);
                }

                assert(source_val);
                result = source_val;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                auto parent_expr = expr->dot.parent_expression;

                Bytecode_Value *parent_lvalue = bytecode_emit_lvalue(builder, parent_expr);

                AST_Type *struct_pointer_type = parent_lvalue->type;
                assert(struct_pointer_type->kind == AST_Type_Kind::POINTER);

                AST_Type *struct_type = struct_pointer_type->pointer.base;
                if (struct_type->kind != AST_Type_Kind::STRUCTURE) {

                    assert(struct_type->kind == AST_Type_Kind::POINTER);
                    assert(struct_type->pointer.base->kind == AST_Type_Kind::STRUCTURE);

                    struct_pointer_type = struct_type;
                    struct_type = struct_type->pointer.base;

                    parent_lvalue = bytecode_emit_load(builder, parent_lvalue);
                }

                Integer_Literal il = { .u32 = (uint32_t)expr->dot.child_index };
                Bytecode_Value *index_value = bytecode_integer_literal_new(builder,
                                                                           Builtin::type_u32,
                                                                           il);

                AST_Type *result_type = expr->type->pointer_to;
                assert(result_type);

                result = bytecode_temporary_new(builder, result_type);
                bytecode_emit_instruction(builder, AGG_OFFSET, parent_lvalue, index_value, result);
                break;
            }

            case AST_Expression_Kind::BINARY: assert(false);

            case AST_Expression_Kind::UNARY: {
                assert(expr->unary.op == UNOP_DEREF);

                auto op_lvalue = bytecode_emit_expression(builder, expr->unary.operand_expression);
                assert(op_lvalue->type->kind == AST_Type_Kind::POINTER);

                result = op_lvalue;
                break;
            }

            case AST_Expression_Kind::CALL: assert(false);
            case AST_Expression_Kind::BUILTIN_CALL: assert(false);
            case AST_Expression_Kind::ADDROF: assert(false);
            case AST_Expression_Kind::COMPOUND: assert(false);

            case AST_Expression_Kind::SUBSCRIPT: {
                auto ptr_expr = expr->subscript.pointer_expression;
                auto index_expr = expr->subscript.index_expression;

                auto ptr_val = bytecode_emit_lvalue(builder, ptr_expr);
                if (ptr_val->kind != Bytecode_Value_Kind::ALLOCL) {
                    ptr_val = bytecode_emit_load(builder, ptr_val);
                }

                auto offset_val = bytecode_emit_expression(builder, index_expr);

                AST_Type *result_type = nullptr;
                if (ptr_expr->type->kind == AST_Type_Kind::POINTER) {
                    result_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                         builder->build_data,
                                                                         expr->type);
                } else if (ptr_expr->type->kind == AST_Type_Kind::ARRAY) {
                    auto elem_type = ptr_expr->type->array.element_type;
                    result_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                         builder->build_data,
                                                                         elem_type);
                } else {
                    assert(false);
                }

                assert(result_type);

                result = bytecode_temporary_new(builder, result_type);
                bytecode_emit_instruction(builder, PTR_OFFSET, ptr_val, offset_val, result);
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

        assert(result);
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
        auto arg_count_val = bytecode_integer_literal_new(builder, Builtin::type_s64,
                                                          { .s64 = arg_exprs.count });

        bytecode_emit_instruction(builder, CALL, func_val, arg_count_val, return_value);

        return return_value;
    }

    Bytecode_Value *bytecode_emit_builtin_call(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::BUILTIN_CALL);

        auto args = expr->builtin_call.arg_expressions;

        auto name = expr->builtin_call.identifier->atom;

        if (name == Builtin::atom_static_assert) { assert(false);
        } else if (name == Builtin::atom_exit) {
            assert(args.count == 1);

            auto exit_code_val = bytecode_emit_expression(builder, args[0]);
            bytecode_emit_instruction(builder, EXIT, exit_code_val, nullptr, nullptr);
            return nullptr;

        } else if (name == Builtin::atom_syscall) {
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

        } else if (name == Builtin::atom_cast) {
            assert(args.count == 2);
            auto target_type = args[0]->type;
            auto operand_expr = args[1];
            return bytecode_emit_cast(builder, operand_expr, target_type);

        } else if (name == Builtin::atom_sizeof) {
            assert(args.count == 1);

            auto type = args[0]->type;
            assert(type->bit_size % 8 == 0);

            Integer_Literal il = { .s64 = (int64_t)(type->bit_size / 8) };
            return bytecode_integer_literal_new(builder, Builtin::type_s64, il);

        } else if (name == Builtin::atom_offsetof) {
            assert(args.count == 2);

            auto mem_name = args[0];
            auto struct_name = args[1];

            auto struct_decl = struct_name->identifier->declaration;
            assert(struct_decl);

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

            Integer_Literal il = { .s64 = offset };
            return bytecode_integer_literal_new(builder, Builtin::type_s64, il);
        }
        else assert(false);

        assert(false);
    }

    Bytecode_Value *bytecode_emit_cast(Bytecode_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::CAST);
        return bytecode_emit_cast(builder, expr->cast.operand_expression, expr->cast.target_type);
    }

    Bytecode_Value *bytecode_emit_cast(Bytecode_Builder *builder, AST_Expression *operand,
                                       AST_Type *target_type)
    {
        switch (target_type->kind)
        {
            case  AST_Type_Kind::INVALID: assert(false);
            case  AST_Type_Kind::VOID: assert(false);

            case  AST_Type_Kind::INTEGER: {
                return bytecode_emit_cast_to_int(builder, operand, target_type);
                break;
            }

            case  AST_Type_Kind::FLOAT: {
                return bytecode_emit_cast_to_float(builder, operand, target_type);
                break;
            }
            case  AST_Type_Kind::BOOL: assert(false);
            case  AST_Type_Kind::POINTER: assert(false);
            case  AST_Type_Kind::FUNCTION: assert(false);
            case  AST_Type_Kind::STRUCTURE: assert(false);
            case  AST_Type_Kind::ENUM: assert(false);
            case  AST_Type_Kind::ARRAY: assert(false);
        }

    }

    Bytecode_Value *bytecode_emit_cast_to_int(Bytecode_Builder *builder,
                                              AST_Expression *operand_expr, AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::INTEGER);

        auto operand_type = operand_expr->type;
        if (operand_type->kind == AST_Type_Kind::ENUM) {
            operand_type = operand_type->enum_type.base_type;
        }

        Bytecode_Value *operand_value = bytecode_emit_expression(builder, operand_expr);
        Bytecode_Value *result = bytecode_temporary_new(builder, target_type);

        switch (operand_type->kind) {
            case  AST_Type_Kind::INVALID: assert(false);
            case  AST_Type_Kind::VOID: assert(false);

            case  AST_Type_Kind::INTEGER: {
                bool extend = target_type->bit_size > operand_type->bit_size;

                if (operand_type->integer.sign == target_type->integer.sign) {
                    if (extend) {
                        if (operand_type->integer.sign) {
                            bytecode_emit_instruction(builder, SEXT, operand_value, nullptr,
                                                      result);
                        } else {
                            assert(false);
                        }
                    } else {
                        assert(false);
                    }
                } else if (operand_type->integer.sign) {
                    // Signed --> Unsigned
                    if (extend) {
                        bytecode_emit_instruction(builder, SEXT, operand_value, nullptr, result);
                    } else {
                        bytecode_emit_instruction(builder, TRUNC, operand_value, nullptr, result);
                    }
                } else {
                    // Unsigned --> signed
                    assert(target_type->integer.sign);
                    if (extend) {
                        bytecode_emit_instruction(builder, ZEXT, operand_value, nullptr, result);
                    } else {
                        assert(false);
                    }
                }
                break;
            }

            case  AST_Type_Kind::FLOAT: {
                bytecode_emit_instruction(builder, F_TO_S, operand_value, nullptr, result);
                break;
            }

            case  AST_Type_Kind::BOOL: assert(false);
            case  AST_Type_Kind::POINTER: assert(false);
            case  AST_Type_Kind::FUNCTION: assert(false);
            case  AST_Type_Kind::STRUCTURE: assert(false);
            case  AST_Type_Kind::ENUM: assert(false);
            case  AST_Type_Kind::ARRAY: assert(false);
        }

        assert(result);

        return result;
    }

    Bytecode_Value *bytecode_emit_cast_to_float(Bytecode_Builder *builder,
                                                AST_Expression *operand_expr,
                                                AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::FLOAT);

        auto operand_type = operand_expr->type;

        Bytecode_Value *operand_value = bytecode_emit_expression(builder, operand_expr);
        Bytecode_Value *result = bytecode_temporary_new(builder, target_type);

        switch (operand_type->kind) {
            case  AST_Type_Kind::INVALID: assert(false);
            case  AST_Type_Kind::VOID: assert(false);

            case  AST_Type_Kind::INTEGER: {
                if (operand_type->integer.sign) {
                    bytecode_emit_instruction(builder, S_TO_F, operand_value, nullptr, result);
                } else {
                    bytecode_emit_instruction(builder, U_TO_F, operand_value, nullptr, result);
                }
                break;
            }

            case  AST_Type_Kind::FLOAT:
            {
                assert(operand_expr->type != target_type);
                bytecode_emit_instruction(builder, F_TO_F, operand_value, nullptr, result);
                break;
            }
            case  AST_Type_Kind::BOOL: assert(false);

            case  AST_Type_Kind::POINTER: assert(false);
            case  AST_Type_Kind::FUNCTION: assert(false);
            case  AST_Type_Kind::STRUCTURE: assert(false);
            case  AST_Type_Kind::ENUM: assert(false);
            case  AST_Type_Kind::ARRAY: assert(false);
        }

        return result;
    }

    void bytecode_emit_jump(Bytecode_Builder *builder, Bytecode_Block *dest)
    {
        auto block_value = bytecode_block_value_new(builder, dest);
        bytecode_emit_instruction(builder, JUMP, block_value, nullptr, nullptr);
    }

    void bytecode_emit_jump_if(Bytecode_Builder *builder, Bytecode_Value *cond_val,
                               Bytecode_Block *then_dest, Bytecode_Block *else_dest)
    {
        auto then_block_val = bytecode_block_value_new(builder, then_dest);
        auto else_block_val = bytecode_block_value_new(builder, else_dest);
        bytecode_emit_instruction(builder, JUMP_IF, cond_val, then_block_val, else_block_val);
    }

    void bytecode_emit_store(Bytecode_Builder *builder, Bytecode_Value *dest,
                             Bytecode_Value *source)
    {
        switch (dest->kind) {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::TEMP: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bytecode_emit_instruction(builder, STORE_PTR, dest, source, nullptr);
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL: assert(false);
            case Bytecode_Value_Kind::FLOAT_LITERAL: assert(false);
            case Bytecode_Value_Kind::STRING_LITERAL: assert(false);
            case Bytecode_Value_Kind::BOOL_LITERAL: assert(false);
            case Bytecode_Value_Kind::NULL_LITERAL: assert(false);

            case Bytecode_Value_Kind::ALLOCL: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bytecode_emit_instruction(builder, STOREL, dest, source, nullptr);
                break;
            }

            case Bytecode_Value_Kind::PARAM: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bytecode_emit_instruction(builder, STORE_ARG, dest, source, nullptr);
                break;
            }

            case Bytecode_Value_Kind::GLOBAL:
            {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bytecode_emit_instruction(builder, STORE_GLOBAL, dest, source, nullptr);
                break;
            }

            case Bytecode_Value_Kind::FUNCTION: assert(false);
            case Bytecode_Value_Kind::BLOCK: assert(false);
            case Bytecode_Value_Kind::SWITCH_DATA: assert(false);
        }
    }

    Bytecode_Value *bytecode_emit_load(Bytecode_Builder *builder, Bytecode_Value *source)
    {

        assert(source->type->kind == AST_Type_Kind::POINTER);

        Bytecode_Value *result = bytecode_temporary_new(builder, source->type->pointer.base);

        switch (source->kind) {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::TEMP: {
                assert(source->type->kind == AST_Type_Kind::POINTER);
                bytecode_emit_instruction(builder, LOAD_PTR, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL: assert(false);
            case Bytecode_Value_Kind::FLOAT_LITERAL: assert(false);
            case Bytecode_Value_Kind::STRING_LITERAL: assert(false);
            case Bytecode_Value_Kind::BOOL_LITERAL: assert(false);
            case Bytecode_Value_Kind::NULL_LITERAL: assert(false);

            case Bytecode_Value_Kind::ALLOCL: {
                bytecode_emit_instruction(builder, LOADL, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::PARAM: {
                bytecode_emit_instruction(builder, LOAD_PARAM, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::GLOBAL: {
                bytecode_emit_instruction(builder, LOAD_GLOBAL, source, nullptr, result);
                break;
            }

            case Bytecode_Value_Kind::FUNCTION: assert(false);
            case Bytecode_Value_Kind::BLOCK: assert(false);
            case Bytecode_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    Bytecode_Value *bytecode_emit_float_literal(Bytecode_Builder *builder,
                                                AST_Expression *literal_expr)
    {
        auto type = literal_expr->type;
        assert(type->kind == AST_Type_Kind::FLOAT);

        float f;
        double d;

        if (literal_expr->kind == AST_Expression_Kind::FLOAT_LITERAL) {
            f = literal_expr->float_literal.r32;
            d = literal_expr->float_literal.r64;
        } else if (literal_expr->kind == AST_Expression_Kind::INTEGER_LITERAL) {
            if (type == Builtin::type_float) {
                f = literal_expr->integer_literal.s32;
                d = literal_expr->integer_literal.s32;
            } else if (type == Builtin::type_double) {
                f = literal_expr->integer_literal.s64;
                d = literal_expr->integer_literal.s64;
            } else {
                assert(false);
            }
        } else {
            assert(false);
        }

        return bytecode_float_literal_new(builder, literal_expr->type, f, d);
    }

    Bytecode_Value *bytecode_emit_zero_value(Bytecode_Builder *builder, AST_Type *type) {

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                return bytecode_integer_literal_new(builder, type, { });
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

    void bytecode_add_default_switch_case(Bytecode_Instruction *inst, Bytecode_Block *block)
    {
        assert(inst->op == SWITCH);

        Bytecode_Switch_Data *switch_data = &inst->b->switch_data;

        assert(!switch_data->default_block);
        switch_data->default_block = block;

        Bytecode_Switch_Case case_data = { nullptr, block };
        array_append(&switch_data->cases, case_data);
    }

    void bytecode_add_switch_case(Bytecode_Instruction *inst, Bytecode_Value *case_value,
                                  Bytecode_Block *case_block)
    {
        assert(inst->op == SWITCH);

        Bytecode_Switch_Data *switch_data = &inst->b->switch_data;

        Bytecode_Switch_Case case_data = { case_value, case_block };
        array_append(&switch_data->cases, case_data);
    }

    void bytecode_push_break_block(Bytecode_Builder *builder, Bytecode_Block *block) {
        stack_push(&builder->break_block_stack, block);
    }

    void bytecode_pop_break_block(Bytecode_Builder *builder) {
        stack_pop(&builder->break_block_stack);
    }

    bool bytecode_block_ends_with_terminator(Bytecode_Block *block) {

        if (!block->instructions.count) return false;

        Bytecode_Instruction *last_instruction = array_last(&block->instructions);

        return last_instruction->op == RETURN ||
               last_instruction->op == RETURN_VOID ||
               last_instruction->op == JUMP ||
               last_instruction->op == JUMP_IF ||
               last_instruction->op == EXIT;
    }

    Bytecode_Value *bytecode_find_parameter(Bytecode_Builder *builder, AST_Declaration *decl) {

        assert(decl->kind == AST_Declaration_Kind::PARAMETER);

        for (int64_t i = 0; i < builder->parameters.count; i++) {
            auto info = builder->parameters[i];

            if (info.declaration == decl) {
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

        if (decl->decl_flags & AST_DECL_FLAG_GLOBAL) {
            for (int64_t i = 0; i < builder->globals.count; i++) {
                auto info = builder->globals[i];
                if (info.declaration == decl) {
                    assert(info.global_value->kind == Bytecode_Value_Kind::GLOBAL);
                    return info.global_value;
                }
            }
        } else {
            for (int64_t i = 0; i < builder->locals.count; i++) {
                auto info = builder->locals[i];
                if (info.declaration == decl) {
                    assert(info.allocl_value->kind == Bytecode_Value_Kind::ALLOCL);
                    return info.allocl_value;
                }
            }
        }

        assert(false);
        return nullptr;
    }

    Bytecode_Value *bytecode_value_new(Bytecode_Builder *builder, Bytecode_Value_Kind kind,
                                       AST_Type *type)
    {
        assert(kind != Bytecode_Value_Kind::INVALID);

        assert(type ||
               (kind == Bytecode_Value_Kind::SWITCH_DATA ||
                kind == Bytecode_Value_Kind::BLOCK));

        auto result = alloc_type<Bytecode_Value>(builder->allocator);
        result->kind = kind;
        result->type = type;

        return result;
    }

    Bytecode_Value *bytecode_integer_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal)
    {
        assert(type->kind == AST_Type_Kind::INTEGER ||  type->kind == AST_Type_Kind::ENUM);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::INTEGER_LITERAL, type);
        result->integer_literal = integer_literal;
        return result;
    }

    Bytecode_Value *bytecode_float_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                               float r32, double r64)
    {
        assert(type->kind == AST_Type_Kind::FLOAT);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::FLOAT_LITERAL, type);
        result->float_literal.r32 = r32;
        result->float_literal.r64 = r64;
        return result;
    }

    Bytecode_Value *bytecode_string_literal_new(Bytecode_Builder *builder, Atom string_literal)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::STRING_LITERAL,
                                         Builtin::type_ptr_u8);
        result->string_literal = string_literal;
        return result;
    }

    Bytecode_Value *bytecode_bool_literal_new(Bytecode_Builder *builder, AST_Type *type,
                                              bool value)
    {
        assert(type == Builtin::type_bool);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::BOOL_LITERAL,
                                         Builtin::type_bool);
        result->bool_literal = value;
        return result;
    }

    Bytecode_Value *bytecode_null_literal_new(Bytecode_Builder *builder, AST_Type *type)
    {
        assert(type->kind == AST_Type_Kind::POINTER);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::NULL_LITERAL,
                                         type);
        return result;
    }

    Bytecode_Value *bytecode_local_alloc_new(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }
        assert(pointer_type);

        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::ALLOCL, pointer_type);
        result->allocl.name = name;

        auto index = builder->current_function->locals.count;
        array_append(&builder->current_function->locals, result);
        result->allocl.index = index;
        return result;
    }

    Bytecode_Value *bytecode_parameter_new(Bytecode_Builder *builder, Bytecode_Function *func,
                                           AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }

        assert(pointer_type);
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::PARAM, pointer_type);
        result->parameter.name = name;

        auto index = func->parameters.count;
        array_append(&func->parameters, result);
        result->parameter.index = index;

        return result;
    }

    Bytecode_Value *bytecode_global_new(Bytecode_Builder *builder, AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }

        assert(pointer_type);
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::GLOBAL, pointer_type);
        result->global.name = name;

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

    Bytecode_Value *bytecode_block_value_new(Bytecode_Builder *builder, Bytecode_Block *block)
    {
        auto result = bytecode_value_new(builder, Bytecode_Value_Kind::BLOCK, nullptr);
        result->block = block;
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
            assert(param->type->kind == AST_Type_Kind::POINTER);
            ast_print_type(sb, param->type->pointer.base);
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

        if (inst->result &&
            inst->op != JUMP_IF &&
            inst->op != SWITCH)
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
                assert(inst->result->type->kind == AST_Type_Kind::POINTER);
                ast_print_type(sb, inst->result->type->pointer.base);
                break;
            }

            case STOREL:       string_builder_append(sb, "STOREL "); break;
            case STORE_ARG:    string_builder_append(sb, "STORE_ARG "); break;
            case STORE_GLOBAL: string_builder_append(sb, "STORE_GLOBAL "); break;
            case STORE_PTR:    string_builder_append(sb, "STORE_PTR "); break;

            case LOADL:       string_builder_append(sb, "LOADL "); break;
            case LOAD_PARAM:  string_builder_append(sb, "LOAD_PARAM "); break;
            case LOAD_GLOBAL: string_builder_append(sb, "LOAD_GLOBAL "); break;
            case LOAD_PTR:    string_builder_append(sb, "LOAD_PTR "); break;

            case ADD_S: string_builder_append(sb, "ADD_S "); break;
            case SUB_S: string_builder_append(sb, "SUB_S "); break;
            case REM_S: string_builder_append(sb, "REM_S "); break;
            case MUL_S: string_builder_append(sb, "MUL_S "); break;
            case DIV_S: string_builder_append(sb, "DIV_S "); break;

            case EQ_S:   string_builder_append(sb, "EQ_S "); break;
            case NEQ_S:  string_builder_append(sb, "NEQ_S "); break;
            case LT_S:   string_builder_append(sb, "LT_S "); break;
            case LTEQ_S: string_builder_append(sb, "LTEQ_S "); break;
            case GT_S:   string_builder_append(sb, "GT_S "); break;
            case GTEQ_S: string_builder_append(sb, "GTEQ_S "); break;

            case ADD_U: string_builder_append(sb, "ADD_U "); break;
            case SUB_U: string_builder_append(sb, "SUB_U "); break;
            case REM_U: string_builder_append(sb, "REM_U "); break;
            case MUL_U: string_builder_append(sb, "MUL_U "); break;
            case DIV_U: string_builder_append(sb, "SUB_U "); break;

            case EQ_U:   string_builder_append(sb, "EQ_U "); break;
            case NEQ_U:  string_builder_append(sb, "NEQ_U "); break;
            case LT_U:   string_builder_append(sb, "LT_U "); break;
            case LTEQ_U: string_builder_append(sb, "LTEQ_U "); break;
            case GT_U:   string_builder_append(sb, "GT_U "); break;
            case GTEQ_U: string_builder_append(sb, "GTEQ_U "); break;


            case ADD_F: string_builder_append(sb, "ADD_F "); break;
            case SUB_F: string_builder_append(sb, "SUB_F "); break;
            case MUL_F: string_builder_append(sb, "MUL_F "); break;
            case DIV_F: string_builder_append(sb, "SUB_F "); break;

            case EQ_F:   string_builder_append(sb, "EQ_F "); break;
            case NEQ_F:  string_builder_append(sb, "NEQ_F "); break;
            case LT_F:   string_builder_append(sb, "LT_F "); break;
            case LTEQ_F: string_builder_append(sb, "LTEQ_F "); break;
            case GT_F:   string_builder_append(sb, "GT_F "); break;
            case GTEQ_F: string_builder_append(sb, "GTEQ_F "); break;

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

            case RETURN:      string_builder_append(sb, "RETURN "); break;
            case RETURN_VOID: string_builder_append(sb, "RETURN_VOID "); break;
            case JUMP:        string_builder_append(sb, "JUMP "); break;

            case JUMP_IF: {
                print_args = false;
                string_builder_append(sb, "JUMP_IF ");
                bytecode_print_value(sb, inst->a);
                string_builder_append(sb, ", ");
                bytecode_print_value(sb, inst->b);
                string_builder_append(sb, ", ");
                bytecode_print_value(sb, inst->result);
                break;
            }

            case SWITCH: {
                print_args = false;
                string_builder_append(sb, "SWITCH ");
                bytecode_print_value(sb, inst->a);

                auto switch_data = &inst->b->switch_data;

                for (int64_t i = 0; i < switch_data->cases.count; i++) {
                    auto case_info = switch_data->cases[i];

                    string_builder_append(sb, "\n      ");

                    if (case_info.case_value) {
                        bytecode_print_value(sb, case_info.case_value);
                    } else {
                        string_builder_append(sb, "default");
                    }

                    string_builder_appendf(sb, " -> %s", case_info.target_block->name);

                }
                break;
            }

            case PTR_OFFSET: string_builder_append(sb, "PTR_OFFSET "); break;
            case AGG_OFFSET: string_builder_append(sb, "AGG_OFFSET "); break;

            case ZEXT:   string_builder_append(sb, "ZEXT "); break;
            case SEXT:   string_builder_append(sb, "SEXT "); break;
            case TRUNC:  string_builder_append(sb, "TRUNC "); break;
            case F_TO_S: string_builder_append(sb, "F_TO_S "); break;
            case S_TO_F: string_builder_append(sb, "S_TO_F "); break;
            case U_TO_F: string_builder_append(sb, "U_TO_F "); break;
            case F_TO_F: string_builder_append(sb, "F_TO_F "); break;

            case EXIT:    string_builder_append(sb, "EXIT "); break;
            case SYSCALL: string_builder_append(sb, "SYSCALL "); break;
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
        switch (value->kind) {
            case Bytecode_Value_Kind::INVALID: assert(false);

            case Bytecode_Value_Kind::TEMP: {
                string_builder_appendf(sb, "%%%" PRId64, value->temp.index);
                break;
            }

            case Bytecode_Value_Kind::FLOAT_LITERAL: {
                if (value->type == Builtin::type_float) {
                    string_builder_appendf(sb, "%f", value->float_literal.r32);
                } else if (value->type == Builtin::type_double) {
                    string_builder_appendf(sb, "%f", value->float_literal.r64);
                } else {
                    assert(false);
                }
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL:
            {
                if (value->type->integer.sign)
                {
                    switch (value->type->bit_size) {
                        default: assert(false);
                        case 8:
                                 string_builder_appendf(sb, "%" PRId64 " ('%c')",
                                                        value->integer_literal.s8,
                                                        value->integer_literal.s8);
                                 break;
                        case 16:
                                 string_builder_appendf(sb, "%" PRId64,
                                                        value->integer_literal.s16);
                                 break;
                        case 32:
                                 string_builder_appendf(sb, "%" PRId64,
                                                        value->integer_literal.s32);
                                 break;
                        case 64:
                                 string_builder_appendf(sb, "%" PRId64,
                                                        value->integer_literal.s64);
                                 break;
                    }
                }
                else
                {
                    switch (value->type->bit_size) {
                        default: assert(false);
                        case 8:
                                 string_builder_appendf(sb, "%" PRIu64 " ('%c')",
                                                        value->integer_literal.u8,
                                                        value->integer_literal.u8);
                                 break;
                        case 16:
                                 string_builder_appendf(sb, "%" PRIu64,
                                                        value->integer_literal.u16);
                                 break;
                        case 32:
                                 string_builder_appendf(sb, "%" PRIu64,
                                                        value->integer_literal.u32);
                                 break;
                        case 64:
                                 string_builder_appendf(sb, "%" PRIu64,
                                                        value->integer_literal.u64);
                                 break;
                    }
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

            case Bytecode_Value_Kind::BOOL_LITERAL: assert(false);

            case Bytecode_Value_Kind::NULL_LITERAL: {
                string_builder_append(sb, "null");
                break;
            }

            case Bytecode_Value_Kind::ALLOCL:
            case Bytecode_Value_Kind::PARAM:
            {
                string_builder_appendf(sb, "%%%s", value->allocl.name.data);
                break;
            }

            case Bytecode_Value_Kind::GLOBAL: assert(false);

            case Bytecode_Value_Kind::FUNCTION:
            {
                string_builder_appendf(sb, "%s", value->function->name.data);
                break;
            }

            case Bytecode_Value_Kind::BLOCK:
            {
                string_builder_appendf(sb, "%s", value->block->name.data);
                break;
            }
            case Bytecode_Value_Kind::SWITCH_DATA: assert(false);
        }
    }
}
