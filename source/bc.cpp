#include "bc.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "resolver.h"
#include "string_builder.h"
#include "temp_allocator.h"

namespace Zodiac
{
    BC_Builder bc_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        BC_Builder result = {};

        result.allocator = allocator;
        result.build_data = build_data;

        array_init(allocator, &result.functions);
        array_init(allocator, &result.parameters);
        array_init(allocator, &result.locals);
        array_init(allocator, &result.globals);
        array_init(allocator, &result.string_literals);
        stack_init(allocator, &result.break_block_stack);

        result.next_temp_index = 0;
        result.run_wrapper_count = 0;

        result.insert_block = nullptr;

        return result;
    }

    BC_Function *bc_register_function(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func_type = decl->type;
        if (!func_type) {
            assert(decl->function.type_spec && decl->function.type_spec->type);
            func_type = decl->function.type_spec->type;
        }
        assert(func_type);

        assert(func_type->kind == AST_Type_Kind::FUNCTION);

        auto ex_func = bc_find_function(builder, decl);
        if (ex_func) {
            assert(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE);
            return ex_func;
        }

        assert(!(decl->decl_flags & AST_DECL_FLAG_REGISTERED_BYTECODE));
        assert(decl->identifier);

        auto ta = temp_allocator_get();

        Atom name = decl->identifier->atom;
        Atom prefix_name = {};

        if ((decl->decl_flags & AST_DECL_FLAG_IS_ENTRY) ||
            (is_bytecode_entry_decl(decl)) ||
            (decl->decl_flags & AST_DECL_FLAG_FOREIGN)) {
        } else {
            auto _prefix_name = string_append(ta, string_ref(decl->function.module_name), ".");
            prefix_name = atom_get(&builder->build_data->atom_table, _prefix_name);
        }

        BC_Function *result = bc_new_function(builder, func_type, prefix_name, name);

        if (decl->decl_flags & AST_DECL_FLAG_NORETURN) {
            result->flags |= BC_FUNC_FLAG_NORETURN;
        }

        if (decl->decl_flags & AST_DECL_FLAG_COMPILER_FUNC) {
            result->flags |= BC_FUNC_FLAG_COMPILER_FUNC;
        }

        if (decl->decl_flags & AST_DECL_FLAG_IS_TEST_FUNC) {
            result->flags |= BC_FUNC_FLAG_IS_TEST;
        }

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++) {
            auto param_decl = decl->function.parameter_declarations[i];
            assert(param_decl->kind == AST_Declaration_Kind::PARAMETER);

            bc_parameter_new(builder, result, param_decl->type, param_decl->identifier->atom);
        }

        auto index = builder->functions.count;
        array_append(&builder->functions, { decl, result, index });

        decl->decl_flags |= AST_DECL_FLAG_REGISTERED_BYTECODE;

        return result;
    }

    BC_Function *bc_emit_function_declaration(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bc_find_function(builder, decl);
        assert(func);
        auto bd = builder->build_data;

        if (decl->decl_flags & AST_DECL_FLAG_IS_ENTRY) {
            assert(!bd->bc_entry_function);
            bd->bc_entry_function = func;
            func->flags |= BC_FUNC_FLAG_CRT_ENTRY;
        }

        if (is_bytecode_entry_decl(decl)) {
            assert(!bd->bc_bytecode_entry_function);
            bd->bc_bytecode_entry_function = func;
            func->flags |= BC_FUNC_FLAG_BYTECODE_ENTRY;
        }

        if (decl->decl_flags & AST_DECL_FLAG_FOREIGN) {
            func->flags |= BC_FUNC_FLAG_FOREIGN;
        }

        if (func->flags & BC_FUNC_FLAG_EMITTED) {
            return func;
        }

        // if (builder->build_data->options->verbose)
        //     printf("[BYTECODE] emitting function: %s\n", decl->identifier->atom.data);


        builder->current_function = func;
        builder->parameters.count = 0;
        builder->locals.count = 0;
        builder->next_temp_index = 0;

        if (func->flags & BC_FUNC_FLAG_FOREIGN) {
            for (int64_t i = 0; i < builder->foreign_functions.count; i++) {
                assert(builder->foreign_functions[i] != func);
            }
            array_append(&builder->foreign_functions, func);
            return func;
        }

        for (int64_t i = 0; i < decl->function.parameter_declarations.count; i++) {
            auto param_decl = decl->function.parameter_declarations[i];
            auto param_val = func->parameters[i];
            array_append(&builder->parameters, { param_decl, param_val });
        }

        BC_Block *entry_block = bc_new_block(builder, "entry");
        bc_append_block(builder, func, entry_block);

        bc_set_insert_point(builder, entry_block);

        for (int64_t i = 0; i < decl->function.variable_declarations.count; i++) {
            auto var_decl = decl->function.variable_declarations[i];
            assert(var_decl->kind == AST_Declaration_Kind::VARIABLE);

            auto name = var_decl->identifier->atom;

            auto local_val = bc_local_alloc_new(builder, var_decl->type, name);

            bc_emit_instruction(builder, ALLOCL, nullptr, nullptr, local_val);

            array_append(&builder->locals, { var_decl, local_val });
        }

        bc_emit_statement(builder, decl->function.body);

        BC_Block *last_block = array_last(&func->blocks);
        assert(last_block == builder->insert_block);

        if (!bc_block_ends_with_terminator(last_block) &&
            !(func->flags & BC_FUNC_FLAG_NORETURN)) {

            if (func->type->function.return_type == Builtin::type_void) {
                bc_emit_instruction(builder, RETURN_VOID, nullptr, nullptr, nullptr);
            } else {
                assert(false && !"Not all control paths return a value, we should catch this in the resolver");
            }
        }

        func->flags |= BC_FUNC_FLAG_EMITTED;
        return func;

    }

    BC_Global_Info bc_emit_global_variable(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);

        Const_Value init_const_val = {};
        bool has_initializer = false;

        auto init_expr = decl->variable.init_expression;

        if (init_expr) {
            has_initializer = true;
            assert(init_expr->expr_flags & AST_EXPR_FLAG_CONST);
            init_const_val = const_interpret_expression(init_expr);
        }

        BC_Global_Info result = {
            .declaration = decl,
            .has_initializer = has_initializer,
            .init_const_val = init_const_val,
            .global_value = bc_global_new(builder, decl->type, decl->identifier->atom),
        };

        array_append(&builder->globals, result);

        return result;
    }

    BC_Function *bc_emit_run_wrapper(BC_Builder *builder, AST_Declaration *decl,
                                     BC_Function *pre_main_func)
    {
        assert(decl->kind == AST_Declaration_Kind::RUN);

        auto run_expr = decl->run.expression;
        assert(run_expr->kind == AST_Expression_Kind::CALL);

        // if (builder->build_data->options->verbose)
        //     printf("[BYTECODE] Emitting run wrapper calling: '%s'\n",
        //            run_expr->call.callee_declaration->identifier->atom.data);

        AST_Type *wrapper_type = build_data_find_function_type(builder->build_data, {},
                                                               run_expr->type);
        assert(wrapper_type);

        auto ta = temp_allocator_get();
        String _wrapper_name = string_append(builder->allocator, string_ref("_run_wrapper_"),
                                             string_from_int(ta, builder->run_wrapper_count));
        builder->run_wrapper_count += 1;

        Atom name = atom_get(&builder->build_data->atom_table,  _wrapper_name);
        BC_Function *result = bc_new_function(builder, wrapper_type, {}, name);

        builder->current_function = result;
        builder->parameters.count = 0;
        builder->locals.count = 0;
        builder->next_temp_index = 0;

        BC_Block *entry_block = bc_new_block(builder, "entry");
        bc_append_block(builder, result, entry_block);
        bc_set_insert_point(builder, entry_block);

        //
        // Emit pre_main() so stdout, etc. are setup
        //

        if (!builder->build_data->options->link_c) {
            assert(pre_main_func);

            BC_Value *func_val = bc_function_value_new(builder, pre_main_func);
            BC_Value *arg_count_val = bc_integer_literal_new(builder,
                                                                         Builtin::type_s64,
                                                                         { .s64 = 0 });

            bc_emit_instruction(builder, CALL, func_val, arg_count_val, nullptr);
        }

        //
        // Emit the actual expression after #run
        //

        BC_Value *return_value = bc_emit_expression(builder, decl->run.expression);

        //
        // Emit the return from the wrapper
        //

        if (run_expr->type->kind == AST_Type_Kind::VOID) {
            bc_emit_instruction(builder, RETURN_VOID, nullptr, nullptr, nullptr);
        } else {
            assert(return_value);
            bc_emit_instruction(builder, RETURN, return_value, nullptr, nullptr);
        }

        auto index = builder->functions.count;
        array_append(&builder->functions, { decl, result, index });

        result->flags |= BC_FUNC_FLAG_EMITTED;

        return result;
    }

    BC_Function *bc_emit_test_wrapper(BC_Builder *builder,
                                                  Array<BC_Function *> test_functions)
    {
        assert(false && "bc_emit_test_wrapper is not implemented!!!");
        return  nullptr;
    }

    BC_Block *bc_new_block(BC_Builder *builder, const char *name)
    {
        BC_Block *result = alloc_type<BC_Block>(builder->allocator);
        result->name = atom_get(&builder->build_data->atom_table, name);
        result->function = nullptr;

        array_init(builder->allocator, &result->instructions);

        return result;
    }

    void bc_append_block(BC_Builder *builder, BC_Function *function,
                               BC_Block *block)
    {
        assert(block->instructions.count == 0);

        assert(builder->current_function == function);
        assert(!block->function);

        block->name = bc_get_unique_block_name(builder, block->name);

        block->function = function;

        BC_Block *last_block = array_last(&function->blocks);
        if (last_block) {
            assert(last_block->instructions.count);
        }

        array_append(&function->blocks, block);
    }

    Atom bc_get_unique_block_name(BC_Builder *builder, Atom name)
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

    void bc_set_insert_point(BC_Builder *builder, BC_Block *block)
    {
        builder->insert_block = block;
    }

    BC_Function *bc_find_function(BC_Builder *builder, AST_Declaration *decl)
    {
        for (int64_t i = 0; i < builder->functions.count; i++)
        {
            if (builder->functions[i].declaration == decl) return builder->functions[i].bc_func;
        }

        return nullptr;
    }

    BC_Function *bc_new_function(BC_Builder *builder, AST_Type *type,
                                             Atom name_prefix, Atom name)
    {
        assert(type->kind == AST_Type_Kind::FUNCTION);

        BC_Function *result = alloc_type<BC_Function>(builder->allocator);

        result->type = type;
        result->name_prefix = name_prefix;
        result->name = name;

        array_init(builder->allocator, &result->parameters);
        array_init(builder->allocator, &result->locals, 4);
        array_init(builder->allocator, &result->temps, 8);
        array_init(builder->allocator, &result->blocks, 4);

        return result;
    }

    void bc_emit_declaration(BC_Builder *builder, AST_Declaration *decl)
    {
        switch (decl->kind) {

            case AST_Declaration_Kind::INVALID: assert(false);
            case AST_Declaration_Kind::IMPORT: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::USING: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::VARIABLE: {
                auto allocl_val = bc_find_variable(builder, decl);
                assert(allocl_val);

                if (decl->variable.init_expression) {
                    auto init_val = bc_emit_expression(builder, decl->variable.init_expression);
                    bc_emit_store(builder, allocl_val, init_val);
                }

                break;
            }

            case AST_Declaration_Kind::CONSTANT: break;

            case AST_Declaration_Kind::PARAMETER: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::FUNCTION: assert(false);

            case AST_Declaration_Kind::TYPE: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::TYPEDEF: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::STRUCTURE: {
                assert(decl->flags & AST_NODE_FLAG_RESOLVED_ID);
                assert(decl->flags & AST_NODE_FLAG_TYPED);
                assert(decl->type);
                assert(decl->type->kind == AST_Type_Kind::STRUCTURE);
                break;
            }

            case AST_Declaration_Kind::UNION: assert(false);

            case AST_Declaration_Kind::ENUM: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::POLY_TYPE: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::RUN: assert(false);
            case AST_Declaration_Kind::STATIC_IF: assert(false); //@@TODO: Implement!
            case AST_Declaration_Kind::STATIC_ASSERT: assert(false); //@@TODO: Implement!

            case AST_Declaration_Kind::IMPORT_LINK: assert(false);

            case AST_Declaration_Kind::TEST: assert(false);
        }
    }

    void bc_emit_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(builder->insert_block);

        switch (stmt->kind) {
            case AST_Statement_Kind::INVALID: assert(false);

            case AST_Statement_Kind::BLOCK: {
                for (int64_t i = 0; i < stmt->block.statements.count; i++) {
                    bc_emit_statement(builder, stmt->block.statements[i]);
                }
                break;
            }

            case AST_Statement_Kind::ASSIGNMENT: {
                auto ident_expr = stmt->assignment.identifier_expression;
                auto rhs_expr = stmt->assignment.rhs_expression;

                BC_Value *dest = bc_emit_lvalue(builder, ident_expr);
                BC_Value *source = bc_emit_expression(builder, rhs_expr);

                bc_emit_store(builder, dest, source);
                break;
            }

            case AST_Statement_Kind::RETURN: {
                if (stmt->expression) {
                    auto ret_val = bc_emit_expression(builder, stmt->expression);
                    assert(ret_val);
                    bc_emit_instruction(builder, RETURN, ret_val, nullptr, nullptr);
                } else {
                    bc_emit_instruction(builder, RETURN_VOID, nullptr, nullptr, nullptr);
                }
                break;
            }

            case AST_Statement_Kind::BREAK: {
                assert(stack_count(&builder->break_block_stack));
                BC_Block *break_block = stack_top(&builder->break_block_stack);
                bc_emit_jump(builder, break_block);
                break;
            }

            case AST_Statement_Kind::DECLARATION: {
                if (stmt->declaration->kind != AST_Declaration_Kind::FUNCTION) {
                    bc_emit_declaration(builder, stmt->declaration);
                }
                break;
            }

            case AST_Statement_Kind::EXPRESSION: {
                bc_emit_expression(builder, stmt->expression);
                break;
            }

            case AST_Statement_Kind::WHILE: {
                auto cond_block = bc_new_block(builder, "while_cond");
                auto body_block = bc_new_block(builder, "while_body");
                auto post_while_block = bc_new_block(builder, "post_while");

                bc_emit_jump(builder, cond_block);

                bc_append_block(builder, builder->current_function, cond_block);
                bc_set_insert_point(builder, cond_block);

                auto cond_val = bc_emit_expression(builder, stmt->while_stmt.cond_expr);

                bc_emit_jump_if(builder, cond_val, body_block, post_while_block);

                bc_append_block(builder, builder->current_function, body_block);
                bc_set_insert_point(builder, body_block);

                bc_push_break_block(builder, post_while_block);
                bc_emit_statement(builder, stmt->while_stmt.body);
                bc_pop_break_block(builder);

                bc_emit_jump(builder, cond_block);

                bc_append_block(builder, builder->current_function, post_while_block);
                bc_set_insert_point(builder, post_while_block);

                break;
            }

            case AST_Statement_Kind::FOR: {
                bc_emit_for_statement(builder, stmt);
                break;
            }

            case AST_Statement_Kind::IF: {
                bc_emit_if_statement(builder, stmt);
                break;
            }

            case AST_Statement_Kind::SWITCH: {
                auto switch_expr = stmt->switch_stmt.expression;
                BC_Value *switch_val = bc_emit_expression(builder, switch_expr);

                BC_Block *default_block = nullptr;
                BC_Block *post_switch_block = bc_new_block(builder, "post_switch");

                auto func = builder->current_function;

                BC_Instruction *switch_inst = bc_emit_instruction(builder, SWITCH, switch_val,
                                                                  nullptr, nullptr);
                BC_Value *switch_data = bc_value_new(builder, BC_Value_Kind::SWITCH_DATA, nullptr);
                switch_inst->b = switch_data;

                array_init(builder->allocator, &switch_data->switch_data.cases,
                           stmt->switch_stmt.cases.count);

                auto ta = temp_allocator_get();

                Array<BC_Block *> case_blocks = {};
                array_init(ta, &case_blocks, stmt->switch_stmt.cases.count);

                for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++) {
                    AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];

                    const char *block_name = (switch_case->is_default ? "default" : "case");
                    BC_Block *case_block = bc_new_block(builder, block_name);

                    bc_append_block(builder, func, case_block);
                    if (switch_case->is_default) {
                        assert(!default_block);
                        default_block = case_block;
                    }

                    bc_set_insert_point(builder, case_block);
                    bc_push_break_block(builder, post_switch_block);

                    bc_emit_statement(builder, switch_case->body);
                    case_block = builder->insert_block;

                    bc_pop_break_block(builder);

                    if (!bc_block_ends_with_terminator(case_block)) {
                        bc_emit_jump(builder, post_switch_block);
                    }

                    array_append(&case_blocks, case_block);
                }

                if (!default_block) {
                    default_block = post_switch_block;
                }

                bool added_default = false;

                for (int64_t i = 0; i < stmt->switch_stmt.cases.count; i++) {
                    AST_Switch_Case *switch_case = stmt->switch_stmt.cases[i];
                    BC_Block *case_block = case_blocks[i];

                    if (switch_case->is_default) {
                        bc_add_default_switch_case(switch_inst, case_block);
                        added_default = true;
                        continue;
                    }

                    auto el = bucket_array_first(&switch_case->expressions);
                    while (el.bucket) {
                        auto expr = *bucket_locator_get_ptr(el);
                        assert(expr->expr_flags & AST_EXPR_FLAG_CONST);

                        BC_Value *expr_val = bc_emit_expression(builder, expr);
                        bc_add_switch_case(switch_inst, expr_val, case_block);

                        bucket_locator_advance(&el);
                    }
                }

                if (!added_default) {
                    bc_add_default_switch_case(switch_inst, post_switch_block);
                }

                bc_append_block(builder, func, post_switch_block);
                bc_set_insert_point(builder, post_switch_block);

                break;
            }
        }
    }

    void bc_emit_for_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(false && "bc_emit_for_statement is not implemented!!!");
    }

    void bc_emit_if_statement(BC_Builder *builder, AST_Statement *stmt)
    {
        assert(stmt->kind == AST_Statement_Kind::IF);

        auto cond_expr = stmt->if_stmt.cond_expr;
        auto then_stmt = stmt->if_stmt.then_stmt;
        auto else_stmt = stmt->if_stmt.else_stmt;

        auto cond_val = bc_emit_expression(builder, cond_expr);
        assert(cond_val->type->kind == AST_Type_Kind::BOOL);

        BC_Block *then_block = bc_new_block(builder, "then");

        BC_Block *else_block = nullptr;
        if (else_stmt) {
            else_block = bc_new_block(builder, "else");
        }
        BC_Block *post_if_block = bc_new_block(builder, "post_if");

        bc_emit_jump_if(builder, cond_val, then_block,
                              else_block ? else_block : post_if_block);

        bc_append_block(builder, builder->current_function, then_block);
        bc_set_insert_point(builder, then_block);
        bc_emit_statement(builder, then_stmt);
        then_block = builder->insert_block;

        if (!bc_block_ends_with_terminator(then_block)) {
            bc_emit_jump(builder, post_if_block);
        }

        if (else_stmt) {
            bc_append_block(builder, builder->current_function, else_block);
            bc_set_insert_point(builder, else_block);

            bc_emit_statement(builder, else_stmt);
            else_block = builder->insert_block;

            if (!bc_block_ends_with_terminator(else_block)) {
                bc_emit_jump(builder, post_if_block);
            }
        }

        bc_append_block(builder, builder->current_function, post_if_block);
        bc_set_insert_point(builder, post_if_block);

    }

    BC_Value *bc_emit_expression(BC_Builder *builder, AST_Expression *expr)
    {
        BC_Value *result = nullptr;

        switch (expr->kind)
        {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = expr->identifier->declaration;

                if (decl->kind == AST_Declaration_Kind::CONSTANT) {
                    assert(decl->constant.init_expression);
                    result = bc_emit_expression(builder, decl->constant.init_expression);
                } else {
                    BC_Value *source_val = bc_emit_lvalue(builder, expr);
                    result = bc_emit_load(builder, source_val);
                }

                if (decl->decl_flags & AST_DECL_FLAG_IS_ENUM_MEMBER &&
                    result->type->kind != AST_Type_Kind::ENUM) {
                    assert(result->type->kind == AST_Type_Kind::INTEGER);
                    assert(result->type == expr->type->enum_type.base_type);
                    result->type = expr->type;
                }
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::DOT: {

                switch (expr->dot.kind) {
                    case AST_Dot_Expression_Kind::INVALID: assert(false);
                    case AST_Dot_Expression_Kind::UNKNOWN: assert(false);

                    case AST_Dot_Expression_Kind::AGGREGATE_OFFSET: {
                        BC_Value *lvalue = bc_emit_lvalue(builder, expr);
                        result = bc_emit_load(builder, lvalue);
                        break;
                    }

                    case AST_Dot_Expression_Kind::ARRAY_COUNT: {
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
                        result = bc_integer_literal_new(builder, expr->type, il);
                        break;
                    }

                    case AST_Dot_Expression_Kind::ENUM_MEMBER: {
                        assert(expr->dot.child_decl);
                        assert(expr->dot.child_decl->kind == AST_Declaration_Kind::CONSTANT);

                        auto decl = expr->dot.child_decl;
                        auto init_expr = decl->constant.init_expression;
                        result = bc_emit_expression(builder, init_expr);

                        assert(decl->decl_flags & AST_DECL_FLAG_IS_ENUM_MEMBER);
                        assert(result->type->kind == AST_Type_Kind::ENUM);
                        break;
                    }

                    case AST_Dot_Expression_Kind::MODULE_MEMBER: {
                        assert(expr->dot.child_identifier);
                        assert(expr->dot.child_decl);
                        result = bc_emit_identifier(builder, expr->dot.child_identifier);
                        break;
                    }

                    case AST_Dot_Expression_Kind::CONSTANT_MEMBER: {
                        auto child_decl = expr->dot.child_decl;
                        assert(child_decl->kind == AST_Declaration_Kind::CONSTANT);
                        assert(child_decl->constant.init_expression);

                        result = bc_emit_expression(builder,
                                                    child_decl->constant.init_expression);
                        break;
                    }

                    case AST_Dot_Expression_Kind::FUNCTION_CALL: {
                        assert(false);
                        break;
                    }
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
    BC_Value *lhs = bc_emit_expression(builder, lhs_expr); \
    BC_Value *rhs = bc_emit_expression(builder, rhs_expr); \
    result = bc_temporary_new(builder, type); \
    if (type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::ENUM) { \
        if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
        if (type->integer.sign) { \
            bc_emit_instruction(builder, int_signed_op, lhs, rhs, result); \
        } else { \
            bc_emit_instruction(builder, int_unsigned_op, lhs, rhs, result); \
        } \
    } else if (type->kind == AST_Type_Kind::FLOAT) { \
        assert(float_op != NOP); \
        bc_emit_instruction(builder, float_op, lhs, rhs, result); \
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
    auto lhs = bc_emit_expression(builder, lhs_expr); \
    auto rhs = bc_emit_expression(builder, rhs_expr); \
    result = bc_temporary_new(builder, Builtin::type_bool); \
    if (type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::ENUM) { \
        if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
        if (type->integer.sign) { \
            bc_emit_instruction(builder, int_sign_op, lhs, rhs, result); \
        } else { \
            bc_emit_instruction(builder, int_unsigned_op, lhs, rhs, result); \
        } \
    } else if (type->kind == AST_Type_Kind::FLOAT) { \
        bc_emit_instruction(builder, float_op, lhs, rhs, result); \
    } else if (type->kind == AST_Type_Kind::POINTER) { \
        bc_emit_instruction(builder, int_unsigned_op, lhs, rhs, result); \
    } else {\
        assert(false && "Unsupported binop compare"); \
    } \
    break; \
}

                switch (expr->binary.op) {
                    case BINOP_INVALID: assert(false);

                    case BINOP_EQ:   _binop_compare(EQ_S, EQ_U, EQ_F);
                    case BINOP_NEQ:  _binop_compare(NEQ_S, NEQ_U, NEQ_F);
                    case BINOP_LT:   _binop_compare(LT_S, LT_U, LT_F);
                    case BINOP_LTEQ: _binop_compare(LTEQ_S, LTEQ_U, LTEQ_F);
                    case BINOP_GT:   _binop_compare(GT_S, GT_U, GT_F);
                    case BINOP_GTEQ: _binop_compare(GTEQ_S, GTEQ_U, GTEQ_F);

                    case BINOP_ADD:       _binop_arithmetic(ADD_S, ADD_U, ADD_F);
                    case BINOP_SUB:       _binop_arithmetic(SUB_S, SUB_U, SUB_F);
                    case BINOP_REMAINDER: _binop_arithmetic(REM_S, REM_U, NOP);
                    case BINOP_MUL:       _binop_arithmetic(MUL_S, MUL_U, MUL_F);
                    case BINOP_DIV:       _binop_arithmetic(DIV_S, DIV_U, DIV_F);

                }

#undef _binop_compare
#undef _binop_arithmetic
                break;
            }

            case AST_Expression_Kind::UNARY: {

                auto operand_expr = expr->unary.operand_expression;
                auto operand_type = operand_expr->type;

                switch (expr->unary.op) {
                    case UNOP_INVALID: assert(false);

                    case UNOP_DEREF: {
                        assert(operand_type->kind == AST_Type_Kind::POINTER);
                        assert(expr->type == operand_type->pointer.base);

                        BC_Value *ptr_val = bc_emit_expression(builder, operand_expr);
                        result = bc_emit_load(builder, ptr_val);
                        break;
                    }

                    case UNOP_MINUS: {
                        BC_Opcode op = NOP;

                        if (operand_type->kind == AST_Type_Kind::INTEGER) {
                            assert(operand_type->integer.sign);
                            op = SUB_S;
                        } else {
                            assert(operand_type->kind == AST_Type_Kind::FLOAT);
                            op = SUB_F;
                        }

                        assert(op != NOP);

                        BC_Value *zero = bc_emit_zero_value(builder, operand_type);
                        BC_Value *operand_value = bc_emit_expression(builder,
                                                                                 operand_expr);

                        result = bc_temporary_new(builder, operand_type);
                        bc_emit_instruction(builder, op, zero, operand_value, result);

                        break;
                    }

                    case UNOP_NOT: {
                        assert(operand_type->kind == AST_Type_Kind::BOOL ||
                               operand_type->kind == AST_Type_Kind::INTEGER ||
                               operand_type->kind == AST_Type_Kind::POINTER);

                        BC_Value *operand_value = bc_emit_expression(builder,
                                                                                 operand_expr);

                        result = bc_temporary_new(builder, Builtin::type_bool);
                        bc_emit_instruction(builder, NEG_LOG, operand_value, nullptr,
                                                  result);
                        break;
                    }
                }
                break;
            }

            case AST_Expression_Kind::CALL: {
                result = bc_emit_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::BUILTIN_CALL: {
                result = bc_emit_builtin_call(builder, expr);
                break;
            }

            case AST_Expression_Kind::ADDROF: {
                result = bc_emit_lvalue(builder, expr->addrof.operand_expr);
                if (result->kind == BC_Value_Kind::ALLOCL) {
                    // assert(false);
                } else {
                    assert(result->kind == BC_Value_Kind::TEMP);
                }
                break;
            }

            case AST_Expression_Kind::COMPOUND: assert(false); //@TODO: Implement!

            case AST_Expression_Kind::SUBSCRIPT: {
                auto lvalue = bc_emit_lvalue(builder, expr);
                result = bc_emit_load(builder, lvalue);
                break;
            }

            case AST_Expression_Kind::CAST: {
                result = bc_emit_cast(builder, expr);
                break;
            }

            case AST_Expression_Kind::INTEGER_LITERAL: {
                if (expr->type->kind == AST_Type_Kind::INTEGER) {
                    result = bc_integer_literal_new(builder, expr->type,
                                                          expr->integer_literal);
                } else if (expr->type->kind == AST_Type_Kind::ENUM) {
                    result = bc_integer_literal_new(builder, expr->type,
                                                          expr->integer_literal);
                } else if (expr->type->kind == AST_Type_Kind::FLOAT) {
                    result = bc_emit_float_literal(builder, expr);
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Expression_Kind::FLOAT_LITERAL: {
                assert(expr->type->kind == AST_Type_Kind::FLOAT);
                result = bc_emit_float_literal(builder, expr);
                break;
            }

            case AST_Expression_Kind::STRING_LITERAL: {
                // result = bc_string_literal_new(builder, expr->string_literal.atom);
                result = bc_get_string_literal(builder, expr->string_literal.atom);
                break;
            }

            case AST_Expression_Kind::CHAR_LITERAL: {
                Integer_Literal il = { .u8 = (uint8_t)expr->char_literal.c };
                result = bc_integer_literal_new(builder, Builtin::type_u8, il);
                break;
            }
            case AST_Expression_Kind::BOOL_LITERAL: {
                result = bc_bool_literal_new(builder, expr->type, expr->bool_literal.value);
                break;
            }

            case AST_Expression_Kind::NULL_LITERAL:
            {
                result = bc_null_literal_new(builder, expr->type);
                break;
            }

            // Illegal, should be converted to different expressions (depending on the context).
            case AST_Expression_Kind::RANGE: assert(false);
        }

        assert(result || expr->type->kind == AST_Type_Kind::VOID);
        if (result) assert(result->type == expr->type);

        return result;
    }

    BC_Value *bc_emit_lvalue(BC_Builder *builder, AST_Expression *expr)
    {
        BC_Value *result = nullptr;

        switch (expr->kind) {
            case AST_Expression_Kind::INVALID: assert(false);

            case AST_Expression_Kind::IDENTIFIER: {
                auto decl = expr->identifier->declaration;
                assert(decl);

                BC_Value *source_val = nullptr;
                if (decl->kind == AST_Declaration_Kind::VARIABLE) {
                    source_val = bc_find_variable(builder, decl);
                }
                else if (decl->kind == AST_Declaration_Kind::PARAMETER) {
                    source_val = bc_find_parameter(builder, decl);
                } else {
                    assert(false);
                }

                assert(source_val);
                result = source_val;
                break;
            }

            case AST_Expression_Kind::POLY_IDENTIFIER: assert(false);

            case AST_Expression_Kind::DOT: {
                assert(expr->dot.kind == AST_Dot_Expression_Kind::AGGREGATE_OFFSET);
                assert(expr->dot.child_decl);

                auto parent_expr = expr->dot.parent_expression;

                BC_Value *parent_lvalue = bc_emit_lvalue(builder, parent_expr);

                AST_Type *aggregate_pointer_type = parent_lvalue->type;
                assert(aggregate_pointer_type->kind == AST_Type_Kind::POINTER);

                AST_Type *aggregate_type = aggregate_pointer_type->pointer.base;
                if (aggregate_type->kind != AST_Type_Kind::STRUCTURE &&
                    aggregate_type->kind != AST_Type_Kind::UNION) {

                    assert(aggregate_type->kind == AST_Type_Kind::POINTER);
                    assert(aggregate_type->pointer.base->kind == AST_Type_Kind::STRUCTURE ||
                           aggregate_type->pointer.base->kind == AST_Type_Kind::UNION);

                    aggregate_pointer_type = aggregate_type;
                    aggregate_type = aggregate_type->pointer.base;

                    parent_lvalue = bc_emit_load(builder, parent_lvalue);
                }

                AST_Type *result_type =
                    build_data_find_or_create_pointer_type(builder->allocator,
                                                           builder->build_data, expr->type);

                assert(result_type);

                if (aggregate_type->kind == AST_Type_Kind::STRUCTURE) {

                    if (expr->dot.child_decl->kind == AST_Declaration_Kind::VARIABLE) {
                        auto index = expr->dot.child_decl->variable.index_in_parent;
                        assert(index >= 0);

                        Integer_Literal il = { .u32 = (uint32_t)index };
                        BC_Value *index_value =
                            bc_integer_literal_new(builder, Builtin::type_u32, il);

                        result = bc_temporary_new(builder, result_type);
                        bc_emit_instruction(builder, AGG_OFFSET, parent_lvalue, index_value,
                                                  result);
                    } else if (expr->dot.child_decl->kind == AST_Declaration_Kind::CONSTANT) {
                        auto const_decl = expr->dot.child_decl;
                        assert(const_decl->constant.init_expression);
                        result = bc_emit_expression(builder,
                                                          const_decl->constant.init_expression);
                    } else {
                        assert(expr->dot.child_decl->kind == AST_Declaration_Kind::IMPORT_LINK);
                        result = bc_emit_struct_dereference(builder, aggregate_type,
                                                                  parent_lvalue,
                                                                  expr->dot.child_decl,
                                                                  result_type);
                    }

                } else if (aggregate_type->kind == AST_Type_Kind::UNION) {
                    assert(expr->dot.child_decl->kind == AST_Declaration_Kind::VARIABLE);
                    assert(parent_lvalue->type->kind == AST_Type_Kind::POINTER);
                    assert(parent_lvalue->type->pointer.base->kind == AST_Type_Kind::UNION);

                    result = bc_temporary_new(builder, result_type);
                    bc_emit_instruction(builder, PTR_TO_PTR, parent_lvalue, nullptr,
                                              result);
                } else {
                    assert(false);
                }


                assert(result);
                break;
            }

            case AST_Expression_Kind::BINARY: assert(false);

            case AST_Expression_Kind::UNARY: {
                assert(expr->unary.op == UNOP_DEREF);

                auto op_lvalue = bc_emit_expression(builder,
                                                          expr->unary.operand_expression);
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

                auto ptr_val = bc_emit_lvalue(builder, ptr_expr);
                if (ptr_val->kind != BC_Value_Kind::ALLOCL &&
                    ptr_val->kind != BC_Value_Kind::GLOBAL) {
                    if (ptr_expr->type->kind == AST_Type_Kind::ARRAY) {
                        // We don't need to load, the PTR_OFFSET below will do this
                    } else {
                        ptr_val = bc_emit_load(builder, ptr_val);
                    }
                }

                auto offset_val = bc_emit_expression(builder, index_expr);

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

                result = bc_temporary_new(builder, result_type);
                bc_emit_instruction(builder, PTR_OFFSET, ptr_val, offset_val, result);
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

    BC_Value *bc_emit_struct_dereference(BC_Builder *builder,
                                                     AST_Type *struct_type,
                                                     BC_Value *parent_lvalue,
                                                     AST_Declaration *import_link,
                                                     AST_Type *result_type)
    {
        assert(false && "bc_emit_struct_dereference is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_identifier(BC_Builder *builder,
                                             AST_Identifier *identifier)
    {
        assert(false && "bc_emit_identifier is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_identifier_lvalue(BC_Builder *builder,
                                                    AST_Identifier *identifier)
    {
        assert(false && "bc_emit_identifier_lvalue is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_call(BC_Builder *builder,
                                       BC_Function *callee,
                                       const Array_Ref<BC_Value *> args)
    {
        assert(false && "bc_emit_call is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_emit_call(BC_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::CALL);

        auto decl = expr->call.callee_declaration;
        assert(decl);
        assert(decl->kind == AST_Declaration_Kind::FUNCTION);

        auto func = bc_find_function(builder, decl);
        assert(func);

        auto arg_exprs = expr->call.arg_expressions;
        assert(arg_exprs.count == func->parameters.count);

        for (int64_t i = 0; i < arg_exprs.count; i++) {
            BC_Value *arg_val = bc_emit_expression(builder, arg_exprs[i]);
            bc_emit_instruction(builder, PUSH_ARG, arg_val, nullptr, nullptr);
        }

        BC_Value *return_value = nullptr;
        if (func->type->function.return_type->kind != AST_Type_Kind::VOID) {
            return_value = bc_temporary_new(builder, func->type->function.return_type);
        }

        auto func_val = bc_function_value_new(builder, func);
        auto arg_count_val = bc_integer_literal_new(builder, Builtin::type_s64,
                                                          { .s64 = arg_exprs.count });

        bc_emit_instruction(builder, CALL, func_val, arg_count_val, return_value);

        return return_value;
    }

    BC_Value *bc_emit_builtin_call(BC_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::BUILTIN_CALL);

        auto args = expr->builtin_call.arg_expressions;
        auto name = expr->builtin_call.identifier->atom;

        if (name == Builtin::atom_static_assert) { assert(false);
        } else if (name == Builtin::atom_exit) {
            assert(args.count == 1);

            auto exit_code_val = bc_emit_expression(builder, args[0]);
            bc_emit_instruction(builder, EXIT, exit_code_val, nullptr, nullptr);
            return nullptr;

        } else if (name == Builtin::atom_syscall) {

#ifndef linux
            assert(false && "Syscall is only supported on linux");
#endif

            for (int64_t i = 0; i < args.count; i++) {
                BC_Value *arg_val = bc_emit_expression(builder, args[i]);
                bc_emit_instruction(builder, PUSH_ARG, arg_val, nullptr, nullptr);

                assert(arg_val->type->bit_size % 8 == 0);
            }

            auto arg_count_val = bc_integer_literal_new(builder, Builtin::type_u64,
                                                        { .u64 = (uint64_t)args.count });

            auto result = bc_temporary_new(builder, expr->type);

            bc_emit_instruction(builder, SYSCALL, arg_count_val, nullptr, result);
            return result;

        } else if (name == Builtin::atom_cast) {
            assert(args.count == 2);
            auto target_type = args[0]->type;
            auto operand_expr = args[1];
            return bc_emit_cast(builder, operand_expr, target_type);

        } else if (name == Builtin::atom_sizeof) {
            assert(args.count == 1);

            auto type = args[0]->type;

            auto type_val = bc_type_value_new(builder, type);
            auto result = bc_temporary_new(builder, Builtin::type_s64);
            bc_emit_instruction(builder, SIZEOF, type_val, nullptr, result);
            return result;

        } else if (name == Builtin::atom_offsetof) {
            assert(args.count == 2);

            auto mem_name = args[0];
            auto struct_name = args[1];

            auto struct_decl = struct_name->identifier->declaration;
            assert(struct_decl);

            auto mem_decl = mem_name->identifier->declaration;
            assert(mem_decl);

            int64_t member_index = 0;
            bool found = false;

            for (int64_t i = 0; i < struct_decl->structure.member_declarations.count; i++) {
                auto struct_mem = struct_decl->structure.member_declarations[i];
                if (struct_mem == mem_decl) {
                    found = true;
                    member_index = i;
                    break;
                }
            }

            if (!found) assert(false && "Member not found");

            auto struct_type = struct_decl->type;
            assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

            auto type_val = bc_type_value_new(builder, struct_type);
            Integer_Literal index_literal = { .s64 = member_index };
            auto member_index_val = bc_integer_literal_new(builder, Builtin::type_s64,
                                                           index_literal);
            auto result = bc_temporary_new(builder, Builtin::type_s64);

            bc_emit_instruction(builder, OFFSETOF, type_val, member_index_val, result);
            return result;

        } else if (name == Builtin::atom_assert) {
            assert(args.count == 1);

            Scope *entry_scope = builder->build_data->entry_module->module_scope;
            assert(entry_scope);

            auto default_handler_decl = scope_find_declaration(entry_scope,
                                                               Builtin::atom_default_assert_handler);
            assert(default_handler_decl);
            auto default_handler_func = bc_find_function(builder, default_handler_decl);
            assert(default_handler_func);

            auto fp = expr->begin_file_pos;
            // printf("Inserting assert at: %s:%lu\n", fp.file_name.data, fp.line);

            BC_Value *cond_val = bc_emit_expression(builder, args[0]);
            BC_Value *file_name = bc_get_string_literal(builder, fp.file_name);

            Integer_Literal il = { .s64 = (int64_t)fp.line };
            BC_Value *line_num = bc_integer_literal_new(builder, Builtin::type_s64,
                                                                    il);

            bc_emit_instruction(builder, PUSH_ARG, cond_val, nullptr, nullptr);
            bc_emit_instruction(builder, PUSH_ARG, file_name, nullptr, nullptr);
            bc_emit_instruction(builder, PUSH_ARG, line_num, nullptr, nullptr);

            BC_Value *func_val = bc_function_value_new(builder, default_handler_func);
            BC_Value *arg_count_val = bc_integer_literal_new(builder,
                                                                         Builtin::type_s64,
                                                                         { .s64 = 3 });

            bc_emit_instruction(builder, CALL, func_val, arg_count_val, nullptr);
            return nullptr;
        } else {
            assert(false);
        }

        assert(false);
        return nullptr;
    }

    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *expr)
    {
        assert(expr->kind == AST_Expression_Kind::CAST);
        return bc_emit_cast(builder, expr->cast.operand_expression, expr->cast.target_type);
    }

    BC_Value *bc_emit_cast(BC_Builder *builder, AST_Expression *operand, AST_Type *target_type)
    {
        switch (target_type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                return bc_emit_cast_to_int(builder, operand, target_type);
                break;
            }

            case AST_Type_Kind::FLOAT: {
                return bc_emit_cast_to_float(builder, operand, target_type);
                break;
            }
            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return nullptr;
    }

    BC_Value *bc_emit_cast_to_int(BC_Builder *builder, AST_Expression *operand_expr,
                                  AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::INTEGER);

        auto operand_type = operand_expr->type;
        if (operand_type->kind == AST_Type_Kind::ENUM) {
            operand_type = operand_type->enum_type.base_type;
        }

        BC_Value *operand_value = bc_emit_expression(builder, operand_expr);

        if (operand_value->type == target_type) {
            return operand_value;
        }

        BC_Value *result = bc_temporary_new(builder, target_type);

        switch (operand_type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                bool extend = target_type->bit_size > operand_type->bit_size;

                if (operand_type->integer.sign == target_type->integer.sign) {
                    if (extend) {
                        if (operand_type->integer.sign) {
                            bc_emit_instruction(builder, SEXT, operand_value, nullptr, result);
                        } else {
                            bc_emit_instruction(builder, ZEXT, operand_value, nullptr, result);
                        }
                    } else {
                        if (operand_type->integer.sign) {
                            bc_emit_instruction(builder, TRUNC, operand_value, nullptr, result);
                        } else {
                            bc_emit_instruction(builder, TRUNC, operand_value, nullptr, result);
                        }
                    }
                } else if (operand_type->integer.sign) {
                    // Signed --> Unsigned
                    if (extend) {
                        bc_emit_instruction(builder, SEXT, operand_value, nullptr, result);
                    } else {
                        bc_emit_instruction(builder, TRUNC, operand_value, nullptr, result);
                    }
                } else {
                    // Unsigned --> signed
                    assert(target_type->integer.sign);
                    if (extend) {
                        bc_emit_instruction(builder, ZEXT, operand_value, nullptr, result);
                    } else {
                        bc_emit_instruction(builder, TRUNC, operand_value, nullptr, result);
                    }
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                bc_emit_instruction(builder, F_TO_S, operand_value, nullptr, result);
                break;
            }

            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER: {
                bc_emit_instruction(builder, PTR_TO_INT, operand_value, nullptr, result);
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(result);

        return result;
    }

    BC_Value *bc_emit_cast_to_float(BC_Builder *builder,
                                                AST_Expression *operand_expr,
                                                AST_Type *target_type)
    {
        assert(target_type->kind == AST_Type_Kind::FLOAT);

        auto operand_type = operand_expr->type;

        BC_Value *operand_value = bc_emit_expression(builder, operand_expr);
        BC_Value *result = bc_temporary_new(builder, target_type);

        switch (operand_type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                if (operand_type->integer.sign) {
                    bc_emit_instruction(builder, S_TO_F, operand_value, nullptr, result);
                } else {
                    bc_emit_instruction(builder, U_TO_F, operand_value, nullptr, result);
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                assert(operand_expr->type != target_type);
                bc_emit_instruction(builder, F_TO_F, operand_value, nullptr, result);
                break;
            }
            case  AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }

        return result;
    }

    void bc_emit_jump(BC_Builder *builder, BC_Block *dest)
    {
        auto block_value = bc_block_value_new(builder, dest);
        bc_emit_instruction(builder, JUMP, block_value, nullptr, nullptr);
    }

    void bc_emit_jump_if(BC_Builder *builder, BC_Value *cond_val,
                               BC_Block *then_dest, BC_Block *else_dest)
    {
        auto then_block_val = bc_block_value_new(builder, then_dest);
        auto else_block_val = bc_block_value_new(builder, else_dest);
        bc_emit_instruction(builder, JUMP_IF, cond_val, then_block_val, else_block_val);
    }

    void bc_emit_store(BC_Builder *builder, BC_Value *dest,
                             BC_Value *source)
    {
        switch (dest->kind) {
            case BC_Value_Kind::INVALID: assert(false);

            case BC_Value_Kind::TEMP: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bc_emit_instruction(builder, STORE_PTR, dest, source, nullptr);
                break;
            }

            case BC_Value_Kind::INTEGER_LITERAL: assert(false);
            case BC_Value_Kind::FLOAT_LITERAL: assert(false);
            case BC_Value_Kind::STRING_LITERAL: assert(false);
            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::ALLOCL: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bc_emit_instruction(builder, STOREL, dest, source, nullptr);
                break;
            }

            case BC_Value_Kind::PARAM: {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bc_emit_instruction(builder, STORE_ARG, dest, source, nullptr);
                break;
            }

            case BC_Value_Kind::GLOBAL:
            {
                assert(dest->type->kind == AST_Type_Kind::POINTER);
                assert(dest->type->pointer.base == source->type);
                bc_emit_instruction(builder, STORE_GLOBAL, dest, source, nullptr);
                break;
            }

            case BC_Value_Kind::FUNCTION: assert(false);
            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }
    }

    BC_Value *bc_emit_load(BC_Builder *builder, BC_Value *source)
    {
        assert(source->type->kind == AST_Type_Kind::POINTER);

        BC_Value *result = bc_temporary_new(builder, source->type->pointer.base);

        switch (source->kind) {
            case BC_Value_Kind::INVALID: assert(false);

            case BC_Value_Kind::TEMP: {
                assert(source->type->kind == AST_Type_Kind::POINTER);
                bc_emit_instruction(builder, LOAD_PTR, source, nullptr, result);
                break;
            }

            case BC_Value_Kind::INTEGER_LITERAL: assert(false);
            case BC_Value_Kind::FLOAT_LITERAL: assert(false);
            case BC_Value_Kind::STRING_LITERAL: assert(false);
            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::ALLOCL: {
                bc_emit_instruction(builder, LOADL, source, nullptr, result);
                break;
            }

            case BC_Value_Kind::PARAM: {
                bc_emit_instruction(builder, LOAD_PARAM, source, nullptr, result);
                break;
            }

            case BC_Value_Kind::GLOBAL: {
                bc_emit_instruction(builder, LOAD_GLOBAL, source, nullptr, result);
                break;
            }

            case BC_Value_Kind::FUNCTION: assert(false);
            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    BC_Value *bc_emit_float_literal(BC_Builder *builder,
                                                AST_Expression *literal_expr)
    {
        auto type = literal_expr->type;
        assert(type->kind == AST_Type_Kind::FLOAT);

        float f = 0.0;
        double d = 0.0;

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

        return bc_float_literal_new(builder, literal_expr->type, f, d);
    }

    BC_Value *bc_emit_zero_value(BC_Builder *builder, AST_Type *type)
    {
        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                return bc_integer_literal_new(builder, type, { });
                break;
            }

            case AST_Type_Kind::FLOAT: {
                return bc_float_literal_new(builder, type, 0.0, 0.0);
                break;
            }

            case AST_Type_Kind::BOOL: assert(false);
            case AST_Type_Kind::POINTER: assert(false);
            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }

        assert(false);
        return nullptr;
    }

    BC_Instruction *bc_emit_instruction(BC_Builder *builder, BC_Opcode op,
                                        BC_Value *a, BC_Value *b,
                                        BC_Value *result_value)
    {
        assert(builder->insert_block);

        BC_Function *func = builder->current_function;
        BC_Block *block = array_last(&func->blocks);
        assert(block == builder->insert_block);

        // BC_Instruction *result =
        //     bucket_array_add_uninitialized(&builder->current_function->instructions);
        // assert(result);
        // result->op = op;
        // result->a = a;
        // result->b = b;
        // result->result = result_value;

        BC_Instruction result = {
            .op = op, .a = a, .b = b, .result = result_value,
        };

        array_append(&block->instructions, result);

        // block->instruction_count += 1;
        // block->last_instruction = result;
        builder->build_data->bytecode_instruction_count += 1;

        return &block->instructions[block->instructions.count - 1];
    }

    void bc_add_default_switch_case(BC_Instruction *inst, BC_Block *block)
    {
        assert(false && "bc_add_default_switch_case is not implemented!!!");
    }

    void bc_add_switch_case(BC_Instruction *inst, BC_Value *case_value,
                                  BC_Block *case_block)
    {
        assert(false && "bc_add_switch_case is not implemented!!!");
    }

    void bc_push_break_block(BC_Builder *builder, BC_Block *block)
    {
        stack_push(&builder->break_block_stack, block);
    }

    void bc_pop_break_block(BC_Builder *builder)
    {
        stack_pop(&builder->break_block_stack);
    }

    bool bc_block_ends_with_terminator(BC_Block *block)
    {
        if (block->instructions.count <= 0) return false;

        auto last_instruction = block->instructions[block->instructions.count - 1];

        return last_instruction.op == RETURN ||
               last_instruction.op == RETURN_VOID ||
               last_instruction.op == JUMP ||
               last_instruction.op == JUMP_IF ||
               last_instruction.op == EXIT;
    }

    BC_Value *bc_find_parameter(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::PARAMETER);

        for (int64_t i = 0; i < builder->parameters.count; i++) {
            auto info = builder->parameters[i];

            if (info.declaration == decl) {
                assert(info.allocl_value->kind == BC_Value_Kind::PARAM);
                return info.allocl_value;
            }
        }

        assert(false);
        return nullptr;
    }

    BC_Value *bc_find_variable(BC_Builder *builder, AST_Declaration *decl)
    {
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);

        if (decl->decl_flags & AST_DECL_FLAG_GLOBAL) {
            for (int64_t i = 0; i < builder->globals.count; i++) {
                auto info = builder->globals[i];
                if (info.declaration == decl) {
                    assert(info.global_value->kind == BC_Value_Kind::GLOBAL);
                    return info.global_value;
                }
            }
        } else {
            for (int64_t i = 0; i < builder->locals.count; i++) {
                auto info = builder->locals[i];
                if (info.declaration == decl) {
                    assert(info.allocl_value->kind == BC_Value_Kind::ALLOCL);
                    return info.allocl_value;
                }
            }
        }

        assert(false);
        return nullptr;
    }

    BC_Value *bc_value_new(BC_Builder *builder, BC_Value_Kind kind, AST_Type *type)
    {
        assert(kind != BC_Value_Kind::INVALID);

        assert(type || (kind == BC_Value_Kind::SWITCH_DATA ||
                        kind == BC_Value_Kind::BLOCK));

        auto result = alloc_type<BC_Value>(builder->allocator);
        result->kind = kind;
        result->type = type;

        return result;
    }

    BC_Value *bc_integer_literal_new(BC_Builder *builder, AST_Type *type,
                                                 Integer_Literal integer_literal)
    {
        assert(type->kind == AST_Type_Kind::INTEGER ||  type->kind == AST_Type_Kind::ENUM);

        auto result = bc_value_new(builder, BC_Value_Kind::INTEGER_LITERAL, type);
        result->integer_literal = integer_literal;
        return result;
    }

    BC_Value *bc_float_literal_new(BC_Builder *builder, AST_Type *type,
                                               float r32, double r64)
    {
        assert(type->kind == AST_Type_Kind::FLOAT);

        auto result = bc_value_new(builder, BC_Value_Kind::FLOAT_LITERAL, type);
        result->float_literal.r32 = r32;
        result->float_literal.r64 = r64;
        return result;
    }

    BC_Value *bc_string_literal_new(BC_Builder *builder, Atom string_literal)
    {
        auto result = bc_value_new(builder, BC_Value_Kind::STRING_LITERAL, Builtin::type_ptr_u8);
        result->string_literal = string_literal;
        return result;
    }

    BC_Value *bc_bool_literal_new(BC_Builder *builder, AST_Type *type,
                                              bool value)
    {
        assert(type == Builtin::type_bool);

        auto result = bc_value_new(builder, BC_Value_Kind::BOOL_LITERAL, Builtin::type_bool);
        result->bool_literal = value;
        return result;
    }

    BC_Value *bc_null_literal_new(BC_Builder *builder, AST_Type *type)
    {
        assert(type->kind == AST_Type_Kind::POINTER);

        auto result = bc_value_new(builder, BC_Value_Kind::NULL_LITERAL, type);
        return result;
    }

    BC_Value *bc_local_alloc_new(BC_Builder *builder, AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }
        assert(pointer_type);

        auto result = bc_value_new(builder, BC_Value_Kind::ALLOCL, pointer_type);
        result->allocl.name = name;

        auto index = builder->current_function->locals.count;
        array_append(&builder->current_function->locals, result);
        result->allocl.index = index;
        return result;
    }

    BC_Value *bc_parameter_new(BC_Builder *builder, BC_Function *func,
                                           AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }

        assert(pointer_type);
        auto result = bc_value_new(builder, BC_Value_Kind::PARAM, pointer_type);
        result->parameter.name = name;

        auto index = func->parameters.count;
        array_append(&func->parameters, result);
        result->parameter.index = index;

        return result;
    }

    BC_Value *bc_global_new(BC_Builder *builder, AST_Type *type, Atom name)
    {
        auto pointer_type = type->pointer_to;
        if (!pointer_type) {
            pointer_type = build_data_find_or_create_pointer_type(builder->allocator,
                                                                  builder->build_data,
                                                                  type);
        }

        assert(pointer_type);
        auto result = bc_value_new(builder, BC_Value_Kind::GLOBAL, pointer_type);
        result->global.name = name;

        return result;
    }

    BC_Value *bc_temporary_new(BC_Builder *builder, AST_Type *type)
    {
        auto result = bc_value_new(builder, BC_Value_Kind::TEMP, type);

        result->temp.index = builder->next_temp_index;
        builder->next_temp_index += 1;

        array_append(&builder->current_function->temps, result);

        return result;
    }

    BC_Value *bc_function_value_new(BC_Builder *builder,
                                                BC_Function *func)
    {
        auto result = bc_value_new(builder, BC_Value_Kind::FUNCTION, func->type);
        result->function = func;
        return result;
    }

    BC_Value *bc_block_value_new(BC_Builder *builder, BC_Block *block)
    {
        auto result = bc_value_new(builder, BC_Value_Kind::BLOCK, nullptr);
        result->block = block;
        return result;
    }

    BC_Value *bc_type_value_new(BC_Builder *builder, AST_Type *type)
    {
        assert(false && "bc_type_value_new is not implemented!!!");
        return nullptr;
    }

    BC_Value *bc_get_string_literal(BC_Builder *builder, const String &str)
    {
        auto atom = atom_get(&builder->build_data->atom_table, str);
        return bc_get_string_literal(builder, atom);
    }

    BC_Value *bc_get_string_literal(BC_Builder *builder, const Atom &atom)
    {
        for (int64_t i = 0; i < builder->string_literals.count; i++) {
            BC_Value *sl = builder->string_literals[i];

            if (sl->string_literal == atom) {
                return sl;
            }
        }

        BC_Value *result = bc_string_literal_new(builder, atom);
        array_append(&builder->string_literals, result);
        return result;
    }

    bool bc_ready_to_run(BC_Builder *builder)
    {
        auto bd = builder->build_data;

        auto default_assert_handler_decl =
            scope_find_declaration(bd, bd->entry_module->module_scope,
                                   "default_assert_handler");
        assert(default_assert_handler_decl);

        auto default_assert_handler_func =
            bc_find_function(builder, default_assert_handler_decl);

        if (!default_assert_handler_func) return false;
        if (!(default_assert_handler_func->flags & BC_FUNC_FLAG_EMITTED)) return false;

        return true;
    }

    void bc_print(Allocator *allocator, BC_Builder *builder)
    {
        String_Builder sb = {};
        string_builder_init(allocator, &sb);

        for (int64_t i = 0; i < builder->functions.count; i++) {
            bc_print_function(&sb, builder->functions[i].bc_func);
        }

        String str = string_builder_to_string(allocator, &sb);
        string_builder_free(&sb);

        printf("%s\n", str.data);

        free(allocator, str.data);
    }

    void bc_print_function(String_Builder *sb, BC_Function *func)
    {
        bool is_foreign = func->flags & BC_FUNC_FLAG_FOREIGN;

        if (is_foreign) string_builder_append(sb, "#foreign ");
        string_builder_appendf(sb, "%s(", func->name.data);

        for (int64_t i = 0; i < func->parameters.count; i++) {
            if (i > 0) string_builder_append(sb, ", ");
            auto param = func->parameters[i];
            bc_print_value(sb, param);
            string_builder_append(sb, " : ");
            assert(param->type->kind == AST_Type_Kind::POINTER);
            ast_print_type(sb, param->type->pointer.base);
        }

        string_builder_append(sb, ")\n");

        if (is_foreign) return;

        for (int64_t i = 0; i < func->blocks.count; i++) {
            auto block = func->blocks[i];
            string_builder_appendf(sb, " %s:\n", block->name.data);

            for (int64_t j = 0; j < block->instructions.count; j++) {
                auto inst = &block->instructions[j];
                bc_print_instruction(sb, inst);
            }
        }

        string_builder_append(sb, "\n");
    }

    void bc_print_instruction(String_Builder *sb, BC_Instruction *inst)
    {
        string_builder_append(sb, "    ");

        if (inst->result &&
            inst->op != JUMP_IF &&
            inst->op != SWITCH) {
            bc_print_value(sb, inst->result);
            string_builder_append(sb, " = ");
        }

        bool print_args = true;

        switch (inst->op) {
            case NOP: assert(false);

            case ALLOCL: {
                string_builder_append(sb, "ALLOCL ");
                assert(inst->result->type->kind == AST_Type_Kind::POINTER);
                ast_print_type(sb, inst->result->type->pointer.base);
                break;
            }

            case STOREL: string_builder_append(sb, "STOREL "); break;
            case STORE_ARG: string_builder_append(sb, "STORE_ARG "); break;
            case STORE_GLOBAL: string_builder_append(sb, "STORE_GLOBAL "); break;
            case STORE_PTR: string_builder_append(sb, "STORE_PTR "); break;

            case LOADL: string_builder_append(sb, "LOADL "); break;
            case LOAD_PARAM: string_builder_append(sb, "LOAD_PARAM "); break;
            case LOAD_GLOBAL: string_builder_append(sb, "LOAD_GLOBAL "); break;
            case LOAD_PTR: string_builder_append(sb, "LOAD_PTR "); break;

            case ADD_S: string_builder_append(sb, "ADD_S "); break;
            case SUB_S: string_builder_append(sb, "SUB_S "); break;
            case REM_S: string_builder_append(sb, "REM_S "); break;
            case MUL_S: string_builder_append(sb, "MUL_S "); break;
            case DIV_S: string_builder_append(sb, "DIV_S "); break;

            case EQ_S: string_builder_append(sb, "EQ_S "); break;
            case NEQ_S: string_builder_append(sb, "NEQ_S "); break;
            case LT_S: string_builder_append(sb, "LT_S "); break;
            case LTEQ_S: string_builder_append(sb, "LTEQ_S "); break;
            case GT_S: string_builder_append(sb, "GT_S "); break;
            case GTEQ_S: string_builder_append(sb, "GTEQ_S "); break;

            case ADD_U: string_builder_append(sb, "ADD_U "); break;
            case SUB_U: string_builder_append(sb, "SUB_U "); break;
            case REM_U: string_builder_append(sb, "REM_U "); break;
            case MUL_U: string_builder_append(sb, "MUL_U "); break;
            case DIV_U: string_builder_append(sb, "DIV_U "); break;

            case EQ_U: string_builder_append(sb, "EQ_U "); break;
            case NEQ_U: string_builder_append(sb, "NEQ_U "); break;
            case LT_U: string_builder_append(sb, "LT_U "); break;
            case LTEQ_U: string_builder_append(sb, "LTEQ_U "); break;
            case GT_U: string_builder_append(sb, "GT_U "); break;
            case GTEQ_U: string_builder_append(sb, "GTEQ_U "); break;

            case ADD_F: string_builder_append(sb, "ADD_F "); break;
            case SUB_F: string_builder_append(sb, "SUB_F "); break;
            case MUL_F: string_builder_append(sb, "MUL_F "); break;
            case DIV_F: string_builder_append(sb, "DIV_F "); break;

            case EQ_F: string_builder_append(sb, "EQ_F "); break;
            case NEQ_F: string_builder_append(sb, "NEQ_F "); break;
            case LT_F: string_builder_append(sb, "LT_F "); break;
            case LTEQ_F: string_builder_append(sb, "LTEQ_F "); break;
            case GT_F: string_builder_append(sb, "GT_F "); break;
            case GTEQ_F: string_builder_append(sb, "GTEQ_F "); break;

            case NEG_LOG: string_builder_append(sb, "NEG_LOG "); break;

            case PUSH_ARG: string_builder_append(sb, "PUSH_ARG "); break;

            case CALL: {
                print_args = false;
                string_builder_append(sb, "CALL ");
                bc_print_value(sb, inst->a);
                string_builder_append(sb, "()");
                string_builder_append(sb, ", ");
                bc_print_value(sb, inst->b);
                break;
            }

            case RETURN: string_builder_append(sb, "RETURN "); break;
            case RETURN_VOID: string_builder_append(sb, "RETURN_VOID "); break;
            case JUMP: string_builder_append(sb, "JUMP "); break;

            case JUMP_IF: {
                print_args = false;
                string_builder_append(sb, "JUMP_IF ");
                bc_print_value(sb, inst->a);
                string_builder_append(sb, ", ");
                bc_print_value(sb, inst->b);
                string_builder_append(sb, ", ");
                bc_print_value(sb, inst->result);
                break;
            }

            case SWITCH: assert(false);

            case PTR_OFFSET: string_builder_append(sb, "PTR_OFFSET "); break;

            case AGG_OFFSET: assert(false);

            case ZEXT: string_builder_append(sb, "ZEXT "); break;
            case SEXT: string_builder_append(sb, "SEXT "); break;
            case TRUNC: string_builder_append(sb, "TRUNC "); break;
            case F_TO_S: string_builder_append(sb, "F_TO_S "); break;
            case S_TO_F: string_builder_append(sb, "S_TO_F "); break;
            case U_TO_F: string_builder_append(sb, "U_TO_F "); break;
            case F_TO_F: string_builder_append(sb, "F_TO_F "); break;


            case PTR_TO_INT: string_builder_append(sb, "PTR_TO_INT "); break;
            case PTR_TO_PTR: string_builder_append(sb, "PTR_TO_PTR "); break;

            case SIZEOF: string_builder_append(sb, "SIZEOF "); break;
            case OFFSETOF: string_builder_append(sb, "OFFSETOF "); break;

            case EXIT: string_builder_append(sb, "EXIT "); break;
            case SYSCALL: string_builder_append(sb, "SYSCALL "); break;
        }

        if (print_args && inst->a) {
            bc_print_value(sb, inst->a);

            if (inst->b) {
                string_builder_append(sb, ", ");
                bc_print_value(sb, inst->b);
            }
        } else if (!inst->a) {
            assert(!inst->b);
        }

        string_builder_append(sb, "\n");
    }

    void bc_print_value(String_Builder *sb, BC_Value *value)
    {
        switch (value->kind) {
            case BC_Value_Kind::INVALID: assert(false);

            case BC_Value_Kind::INTEGER_LITERAL:
            {
                if (value->type->integer.sign) {
                    switch (value->type->bit_size) {
                        default: assert(false);
                        case 8: {
                             string_builder_appendf(sb, "%" PRId8 " ('",
                                                    value->integer_literal.s8);
                             char c;
                             if (parser_make_escape_char(value->integer_literal.s8, &c)) {
                                 string_builder_appendf(sb, "\\%c')", c);
                             } else {
                                 string_builder_appendf(sb, "%c')", c);
                             }
                             break;
                        }
                        case 16: {
                             string_builder_appendf(sb, "%" PRId16,
                                                    value->integer_literal.s16);
                             break;
                         }
                        case 32: {
                             string_builder_appendf(sb, "%" PRId32,
                                                    value->integer_literal.s32);
                             break;
                         }
                        case 64: {
                             string_builder_appendf(sb, "%" PRId64,
                                                    value->integer_literal.s64);
                             break;
                         }
                    }
                } else {
                    switch (value->type->bit_size) {
                        default: assert(false);
                        case 8: {
                             string_builder_appendf(sb, "%" PRIu8 " ('",
                                                    value->integer_literal.u8);
                             char c;
                             if (parser_make_escape_char(value->integer_literal.u8, &c)) {
                                 string_builder_appendf(sb, "\\%c')", c);
                             } else {
                                 string_builder_appendf(sb, "%c')", c);
                             }
                             break;
                         }
                        case 16: {
                             string_builder_appendf(sb, "%" PRIu16,
                                                    value->integer_literal.u16);
                             break;
                         }
                        case 32: {
                             string_builder_appendf(sb, "%" PRIu32,
                                                    value->integer_literal.u32);
                             break;
                         }
                        case 64: {
                             string_builder_appendf(sb, "%" PRIu64,
                                                    value->integer_literal.u64);
                             break;
                         }
                    }
                }
                break;
            }

            case BC_Value_Kind::FLOAT_LITERAL: {
                if (value->type == Builtin::type_float) {
                    string_builder_appendf(sb, "%f", value->float_literal.r32);
                } else if (value->type == Builtin::type_double) {
                    string_builder_appendf(sb, "%f", value->float_literal.r64);
                } else {
                    assert(false);
                }
                break;
            }

            case BC_Value_Kind::STRING_LITERAL: {
                string_builder_append(sb, "\"");

                for (uint64_t i = 0; i < value->string_literal.length; i++) {
                    char c;
                    if (parser_make_escape_char(value->string_literal.data[i], &c)) {
                        string_builder_appendf(sb, "\\%c", c);
                    } else {
                        string_builder_appendf(sb, "%c", c);
                    }
                }

                string_builder_append(sb, "\"");

                break;
            }

            case BC_Value_Kind::BOOL_LITERAL: {
                string_builder_append(sb, value->bool_literal ? "true" : "false");
                break;
            }

            case BC_Value_Kind::NULL_LITERAL: {
                string_builder_append(sb, "null");
            }

            case BC_Value_Kind::TEMP: {
                string_builder_appendf(sb, "%%%" PRId64, value->temp.index);
                break;
            }

            case BC_Value_Kind::ALLOCL:
            case BC_Value_Kind::PARAM: {
                string_builder_appendf(sb, "%%%s", value->global.name.data);
                break;
            }

            case BC_Value_Kind::GLOBAL: assert(false);

            case BC_Value_Kind::FUNCTION: {
                string_builder_appendf(sb, "%s", value->function->name.data);
                break;
            }

            case BC_Value_Kind::BLOCK: {
                string_builder_appendf(sb, "%s", value->block->name.data);
                break;
            }

            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }
    }

}
