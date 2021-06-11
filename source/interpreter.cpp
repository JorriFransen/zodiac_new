#include "interpreter.h"

#include "builtin.h"
#include "os.h"

#include <stdio.h>

namespace Zodiac
{
    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {
            .allocator = allocator,
            .build_data = build_data,
            .running = false,
            .aborted = false,
            .exit_code = 0,
        };

        stack_init(allocator, &result.temp_stack);
        stack_init(allocator, &result.local_stack);
        stack_init(allocator, &result.arg_stack);
        stack_init(allocator, &result.frames);

        const auto mb = 1024 * 1024;
        auto stack_size = 1 * mb;
        result.alloc_stack = alloc_array<uint8_t>(allocator, stack_size);
        result.alloc_sp = result.alloc_stack;
        result.alloc_stack_end = result.alloc_stack + stack_size;

        result.ffi = ffi_create(allocator, build_data);

        result.functions = {};

        return result;
    }

    void interpreter_free(Interpreter *interp)
    {
        stack_free(&interp->temp_stack);
        stack_free(&interp->local_stack);
        stack_free(&interp->arg_stack);
        stack_free(&interp->frames);

        free(interp->allocator, interp->alloc_stack);

        if (interp->global_mem) {
            free(interp->allocator, interp->global_mem);
        }

        *interp = {};
    }

    void interpreter_start(Interpreter *interp, BC_Function *entry_func,
                           Array<BC_Global_Info> global_info, int64_t global_size,
                           Array<BC_Function *> functions,
                           Array<BC_Function *> foreign_functions)
    {
        assert(interp->functions.count == 0 && interp->functions.capacity == 0);
        interp->functions = functions; 

        interpreter_initialize_globals(interp, global_info, global_size);
        interpreter_initialize_foreigns(interp, foreign_functions);

        AST_Type *return_type = entry_func->type->function.return_type;
        bool returns_void = return_type->kind == AST_Type_Kind::VOID;
        assert(return_type->kind == AST_Type_Kind::INTEGER || returns_void);

        Interp_Stack_Frame first_frame = {
            .function = entry_func,
            .ip = {
                .index = 0,
                .block = entry_func->blocks[0],
            },
            .first_temp_index = 1,
            .result_index = returns_void ? -1 : 0,
            .previous_alloc_sp = interp->alloc_sp,
        };

        stack_push(&interp->frames, first_frame);

        if (!returns_void) {
            Interpreter_Value return_val = { .type = return_type, .integer_literal = {} };
            stack_push(&interp->temp_stack, return_val);
        }

        assert(entry_func->locals.count == 0);
        assert(entry_func->parameters.count == 0);

        for (int64_t i = 0; i < entry_func->temps.count; i++) {
            Interpreter_Value temp { .type = entry_func->temps[i]->type };
            stack_push(&interp->temp_stack, temp);
        }

        interp->running = true;

        while (interp->running) {

            bool advance_ip = true;

            auto frame = stack_top_ptr(&interp->frames);
            auto inst = frame->ip.block->instructions[frame->ip.index];

            switch (inst.op) {
                case NOP: assert(false);

                case ALLOCL: {
                    break;
                }

                case STOREL: {
                    assert(inst.a->kind == BC_Value_Kind::ALLOCL);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);

                    if (dest.type->kind == AST_Type_Kind::ARRAY ||
                        dest.type->kind == AST_Type_Kind::STRUCTURE ||
                        dest.type->kind == AST_Type_Kind::ARRAY) {

                        assert(dest.kind == Interp_LValue_Kind::ALLOCL);
                        auto dest_ptr = interp->local_stack.buffer[dest.index];
                        assert(dest_ptr.pointer);
                        assert(dest.type->bit_size % 8 == 0);
                        auto byte_size = dest.type->bit_size / 8;
                        memcpy(dest_ptr.pointer, source.pointer, byte_size);
                    } else {
                        interp_store(interp, source, dest);
                    }

                    break;
                }

                case STORE_ARG: {
                    assert(inst.a->kind == BC_Value_Kind::PARAM);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);
                    interp_store(interp, source, dest);
                    break;
                }

                case STORE_GLOBAL: {
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.a);
                    Interpreter_Value source = interp_load_value(interp, inst.b);

                    assert(dest.type == source.type);
                    interp_store(interp, source, dest);
                    break;
                }

                case STORE_PTR: {
                    assert(inst.a->kind == BC_Value_Kind::TEMP);
                    Interpreter_Value dest_pointer_val = interp_load_value(interp, inst.a);
                    Interpreter_Value source_val = interp_load_value(interp, inst.b);

                    assert(dest_pointer_val.type->kind == AST_Type_Kind::POINTER);
                    assert(source_val.type == dest_pointer_val.type->pointer.base);

                    interp_store(interp, source_val, dest_pointer_val.pointer,
                                 dest_pointer_val.type);
                    break;
                }

                case LOADL: {
                    Interpreter_LValue source_lval = interp_load_lvalue(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(source_lval.type == dest.type);
                    Interpreter_Value value = interp->local_stack.buffer[source_lval.index];

                    assert(dest.type == value.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_PARAM: {
                    Interpreter_Value param_val = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(dest.type == param_val.type);
                    interp_store(interp, param_val, dest);
                    break;
                }

                case LOAD_GLOBAL: {
                    Interpreter_Value value = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    assert(value.type == dest.type);
                    interp_store(interp, value, dest);
                    break;
                }

                case LOAD_PTR: {
                    assert(inst.a->kind == BC_Value_Kind::TEMP);
                    auto ptr_val = interp_load_value(interp, inst.a);
                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);

                    auto dest_val = interp_load_lvalue(interp, inst.result);
                    assert(dest_val.type == ptr_val.type->pointer.base);

                    assert(ptr_val.pointer);
                    interp_store(interp, ptr_val.pointer, ptr_val.type, dest_val);
                    break;
                }

#define MAKE_INT_NAME(sign, size) sign##size

#define BINOP_INT_CASE(sign, size, op) \
    case size: {  (r.integer_literal. MAKE_INT_NAME(sign, size))  = \
                   (lhs.integer_literal. MAKE_INT_NAME(sign, size)) op \
                   (rhs.integer_literal. MAKE_INT_NAME(sign, size)); \
               break; }

#define IS_CMP_OP(op) \
    ((#op[0] == '=' && #op[1] == '=') || \
     (#op[0] == '!' && #op[1] == '=') || \
     (#op[0] == '<') || \
     (#op[0] == '>'))

#define BINOP_INT_(sign_, op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(lhs.type == rhs.type); \
    assert(!IS_CMP_OP(op)); \
    assert(dest.type == lhs.type); \
    auto type = dest.type; \
    if (type->kind == AST_Type_Kind::ENUM) { \
        type = type->enum_type.base_type; \
        dest.type = type; \
    } \
    assert(type->kind == AST_Type_Kind::INTEGER); \
    if (#sign_[0] == 's') { assert(type->integer.sign); } \
    Interpreter_Value r = { \
        .type = type, \
    }; \
    switch (type->bit_size) { \
        BINOP_INT_CASE(sign_, 8, op) \
        BINOP_INT_CASE(sign_, 16, op) \
        BINOP_INT_CASE(sign_, 32, op) \
        BINOP_INT_CASE(sign_, 64, op) \
        default: assert(false); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_INT(op) BINOP_INT_(s, op)
#define BINOP_UINT(op) BINOP_INT_(u, op)

#define BINOP_CMP_INT_CASE(sign, size, op) \
    case size: {  (r.boolean_literal)  = \
                   (lhs.integer_literal. MAKE_INT_NAME(sign, size)) op \
                   (rhs.integer_literal. MAKE_INT_NAME(sign, size)); \
               break; }

#define BINOP_CMP_INT_(sign_, op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(lhs.type == rhs.type); \
    assert(IS_CMP_OP(op)); \
    assert(dest.type == Builtin::type_bool); \
    auto type = lhs.type; \
    if (type->kind == AST_Type_Kind::ENUM) { \
        type = type->enum_type.base_type; \
        assert(type->kind == AST_Type_Kind::INTEGER); \
    } else { \
        assert(type->kind == AST_Type_Kind::INTEGER || type->kind == AST_Type_Kind::POINTER); \
    } \
    if (#sign_[0] == 's') { assert(type->integer.sign); } \
    Interpreter_Value r = { \
        .type = dest.type, \
    }; \
    switch (type->bit_size) { \
        BINOP_CMP_INT_CASE(sign_, 8, op) \
        BINOP_CMP_INT_CASE(sign_, 16, op) \
        BINOP_CMP_INT_CASE(sign_, 32, op) \
        BINOP_CMP_INT_CASE(sign_, 64, op) \
        default: assert(false); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_CMP_INT(op) BINOP_CMP_INT_(s, op)
#define BINOP_CMP_UINT(op) BINOP_CMP_INT_(u, op)


                case ADD_S: BINOP_INT(+);
                case SUB_S: BINOP_INT(-);
                case REM_S: BINOP_INT(%);
                case MUL_S: BINOP_INT(*);
                case DIV_S: BINOP_INT(/);

                case ADD_U: BINOP_UINT(+);
                case SUB_U: BINOP_UINT(-);
                case REM_U: BINOP_UINT(%);
                case MUL_U: BINOP_UINT(*);
                case DIV_U: BINOP_UINT(/);

                case EQ_S:   BINOP_CMP_INT(==);
                case NEQ_S:  BINOP_CMP_INT(!=);

                case LT_S:   BINOP_CMP_INT(<);
                case LTEQ_S: BINOP_CMP_INT(<=);
                case GT_S:   BINOP_CMP_INT(>);
                case GTEQ_S: BINOP_CMP_INT(>=);

                case EQ_U:   BINOP_CMP_UINT(==);
                case NEQ_U: {
                    Interpreter_Value lhs = interp_load_value(interp, inst.a);
                    Interpreter_Value rhs = interp_load_value(interp, inst.b);
                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);
                    assert(lhs.type == rhs.type);
                    assert(IS_CMP_OP(!=));
                    assert(dest.type == Builtin::type_bool);
                    auto type = lhs.type;
                    if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type;
                    Interpreter_Value r = {
                        .type = dest.type,
                    };
                    switch (type->bit_size) {
                        BINOP_CMP_INT_CASE(u, 8, !=)
                        BINOP_CMP_INT_CASE(u, 16, !=)
                        BINOP_CMP_INT_CASE(u, 32, !=)
                        BINOP_CMP_INT_CASE(u, 64, !=)
                        default: assert(false);
                    }
                    interp_store(interp, r, dest);
                    break;
                }

                case LT_U:   BINOP_CMP_UINT(<);
                case LTEQ_U: BINOP_CMP_UINT(<=);
                case GT_U:   BINOP_CMP_UINT(>);
                case GTEQ_U: BINOP_CMP_UINT(>=);

#undef MAKE_INT_NAME
#undef BINOP_INT_CASE
#undef BINOP_INT_CASES
#undef BINOP_INT_
#undef BINOP_INT
#undef BINOP_UINT
#undef BINOP_CMP_INT_
#undef BINOP_CMP_INT
#undef BINOP_CMP_UINT

#define BINOP_FLOAT_CASE(op, size) case size: \
    r.float_literal.r##size = lhs.float_literal.r##size op rhs.float_literal.r##size; break;

#define BINOP_FLOAT(op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(!IS_CMP_OP(op)); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type == dest.type); \
    assert(lhs.type->kind == AST_Type_Kind::FLOAT); \
    Interpreter_Value r = { .type = lhs.type }; \
    switch (lhs.type->bit_size) { \
        default: assert(false); \
        BINOP_FLOAT_CASE(op, 32); \
        BINOP_FLOAT_CASE(op, 64); \
    } \
    interp_store(interp, r, dest); \
    break; \
}

#define BINOP_CMP_FLOAT_CASE(op, size) case size: \
    r.boolean_literal = lhs.float_literal.r##size op rhs.float_literal.r##size; break;

#define BINOP_CMP_FLOAT(op) { \
    Interpreter_Value lhs = interp_load_value(interp, inst.a); \
    Interpreter_Value rhs = interp_load_value(interp, inst.b); \
    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result); \
    assert(IS_CMP_OP(op)); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type->kind == AST_Type_Kind::FLOAT); \
    assert(dest.type == Builtin::type_bool); \
    Interpreter_Value r = { .type = dest.type }; \
    switch (lhs.type->bit_size) { \
        default: assert(false); \
        BINOP_CMP_FLOAT_CASE(op, 32) \
        BINOP_CMP_FLOAT_CASE(op, 64) \
    } \
    interp_store(interp, r, dest); \
    break; \
}

                case ADD_F: BINOP_FLOAT(+);
                case SUB_F: BINOP_FLOAT(-);
                case MUL_F: BINOP_FLOAT(*);
                case DIV_F: BINOP_FLOAT(/);

                case EQ_F:   BINOP_CMP_FLOAT(==);
                case NEQ_F:  BINOP_CMP_FLOAT(!=);
                case LT_F:   BINOP_CMP_FLOAT(<);
                case LTEQ_F: BINOP_CMP_FLOAT(<=);
                case GT_F:   BINOP_CMP_FLOAT(>);
                case GTEQ_F: BINOP_CMP_FLOAT(>=);

#undef IS_CMP_OP
#undef BINOP_FLOAT_CASE
#undef BINOP_FLOAT
#undef BINOP_CMP_FLOAT_CASE
#undef BINOP_CMP_FLOAT

                case NEG_LOG: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::BOOL ||
                           operand.type->kind == AST_Type_Kind::INTEGER ||
                           operand.type->kind == AST_Type_Kind::POINTER);
                    assert(dest_lval.type->kind == AST_Type_Kind::BOOL);

                    bool result_value;

                    if (operand.type->kind == AST_Type_Kind::BOOL ||
                        operand.type->kind == AST_Type_Kind::INTEGER) {
                        // @TODO: @Cleanup: I think we should check all sizes and the sign here?
                        result_value = operand.integer_literal.s64 == 0;
                    } else if (operand.type->kind == AST_Type_Kind::POINTER) {
                        // @Cleanup: this could be merged with the case above?
                        result_value = operand.pointer == nullptr;
                    }

                    AST_Type *source_type = dest_lval.type->pointer_to;
                    assert(source_type);
                    interp_store(interp, &result_value, source_type, dest_lval);

                    break;
                }

                case PUSH_ARG: {
                    Interpreter_Value arg_val = interp_load_value(interp, inst.a);
                    stack_push(&interp->arg_stack, arg_val);
                    break;
                }

                case CALL: {
                    auto callee_val = inst.a;
                    BC_Function *callee = nullptr;
                    if (callee_val->kind != BC_Value_Kind::FUNCTION) {
                        assert(callee_val->type->kind == AST_Type_Kind::POINTER &&
                               callee_val->type->pointer.base->kind == AST_Type_Kind::FUNCTION);
                        auto _callee = interp_load_value(interp, callee_val);
                        assert(_callee.type);
                        callee = (BC_Function *)_callee.pointer;
                        assert(interp_is_known_function_pointer(interp, callee));
                    } else {
                        callee = callee_val->function;
                    }
                    assert(callee);

                    assert((callee->flags & BC_FUNC_FLAG_EMITTED) ||
                           (callee->flags & BC_FUNC_FLAG_FOREIGN));

                    auto arg_count_val = inst.b;
                    assert(arg_count_val->type == Builtin::type_s64);
                    assert(arg_count_val->kind == BC_Value_Kind::INTEGER_LITERAL);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    if (callee->flags & BC_FUNC_FLAG_FOREIGN) {
                        interpreter_execute_foreign_function(interp, callee, arg_count,
                                                             inst.result);
                        break;
                    }

                    if (callee->flags & BC_FUNC_FLAG_COMPILER_FUNC) {
                        interpreter_execute_compiler_function(interp, callee, arg_count);
                        break;
                    }

                    int64_t result_index = -1;

                    if (inst.result) {
                        Interpreter_LValue result_value = interp_load_lvalue(interp, inst.result);
                        result_index = result_value.index;
                    }

                    assert(stack_count(&interp->temp_stack) > result_index);

                    Interp_Stack_Frame new_frame = {
                        .function = callee,
                        .ip = {
                            .index = 0,
                            .block = callee->blocks[0],
                        },
                        .result_index = result_index,
                        .previous_alloc_sp = interp->alloc_sp,
                    };

                    if (arg_count) {
                        assert(stack_count(&interp->arg_stack) >= arg_count);
                        new_frame.first_arg_index = stack_count(&interp->arg_stack) - arg_count;
                    }

                    new_frame.first_temp_index = stack_count(&interp->temp_stack);
                    new_frame.first_alloc_index = stack_count(&interp->local_stack);

                    for (int64_t i = 0; i < callee->temps.count; i++) {
                        Interpreter_Value new_temp = {
                            .type = callee->temps[i]->type,
                        };
                        stack_push(&interp->temp_stack, new_temp);
                    }

                    for (int64_t i = 0; i < callee->locals.count; i++) {
                        auto bc_allocl = callee->locals[i];
                        assert(stack_count(&interp->local_stack) ==
                               bc_allocl->allocl.index + new_frame.first_alloc_index);

                        auto allocl_type = bc_allocl->type->pointer.base;

                        Interpreter_Value alloc_value = {
                            .type = allocl_type,
                        };

                        if (allocl_type->kind == AST_Type_Kind::ARRAY ||
                            allocl_type->kind == AST_Type_Kind::STRUCTURE ||
                            allocl_type->kind == AST_Type_Kind::UNION) {

                            assert(allocl_type->bit_size % 8 == 0);
                            auto byte_size = allocl_type->bit_size / 8;
                            assert(interp->alloc_sp + byte_size < interp->alloc_stack_end);
                            alloc_value.pointer = interp->alloc_sp;
                            interp->alloc_sp += byte_size;
                        }

                        stack_push(&interp->local_stack, alloc_value);
                    }

                    advance_ip = false;
                    frame->ip.index += 1;
                    assert(frame->ip.index <= frame->ip.block->instructions.count);

                    stack_push(&interp->frames, new_frame);
                    frame = stack_peek_ptr(&interp->frames, 1);

                    break;
                }

                case RETURN: {
                    advance_ip = false;

                    assert(inst.a);
                    Interpreter_Value return_value = interp_load_value(interp, inst.a);
                    assert(return_value.type);

                    auto current_frame = stack_pop(&interp->frames);

                    if (current_frame.function->parameters.count) {
                        stack_pop(&interp->arg_stack, current_frame.function->parameters.count);
                    }

                    if (current_frame.function->locals.count) {
                        stack_pop(&interp->local_stack, current_frame.function->locals.count);
                    }

                    assert(current_frame.result_index >= 0);
                    assert(stack_count(&interp->temp_stack) > current_frame.result_index);

                    if (current_frame.function->temps.count) {
                        stack_pop(&interp->temp_stack, current_frame.function->temps.count);
                    }

                    interp->alloc_sp = current_frame.previous_alloc_sp;

                    Interpreter_LValue dest = {
                        .kind = Interp_LValue_Kind::TEMP,
                        .type = return_value.type,
                        .index = current_frame.result_index,
                    };

                    interp_store(interp, return_value, dest);
                    break;
                }

                case RETURN_VOID: {
                    advance_ip = false;
                    auto old_frame = stack_pop(&interp->frames);
                    assert(old_frame.result_index == -1);

                    if (old_frame.function->parameters.count) {
                        stack_pop(&interp->arg_stack, old_frame.function->parameters.count);
                    }

                    if (old_frame.function->temps.count) {
                        stack_pop(&interp->temp_stack, old_frame.function->temps.count);
                    }

                    if (old_frame.function->locals.count) {
                        stack_pop(&interp->local_stack, old_frame.function->locals.count);
                    }

                    interp->alloc_sp = old_frame.previous_alloc_sp;
                    break;
                }

                case JUMP: {
                    auto block_value = inst.a;
                    assert(block_value->kind == BC_Value_Kind::BLOCK);

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = block_value->block,
                    };
                    break;
                }

                case JUMP_IF: {
                    auto cond_val = interp_load_value(interp, inst.a);
                    assert(cond_val.type->kind == AST_Type_Kind::BOOL);

                    assert(inst.b->kind == BC_Value_Kind::BLOCK);
                    assert(inst.result->kind == BC_Value_Kind::BLOCK);

                    BC_Block *then_block = inst.b->block;
                    BC_Block *else_block = inst.result->block;
                    BC_Block *target_block = nullptr;

                    if (cond_val.boolean_literal) {
                        target_block = then_block;
                    } else {
                        target_block = else_block;
                    }

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = target_block,
                    };
                    break;
                }

                case SWITCH: {
                    advance_ip = false;

                    BC_Block *default_block = nullptr;
                    BC_Block *target_block = nullptr;

                    assert(inst.b->kind == BC_Value_Kind::SWITCH_DATA);
                    auto switch_data = inst.b->switch_data;

                    Interpreter_Value operand_val = interp_load_value(interp, inst.a);

                    for (int64_t i = 0; i < switch_data.cases.count; i++) {
                        auto case_info = switch_data.cases[i];

                        if (case_info.case_value) {
                            assert(inst.a->type == case_info.case_value->type);
                            auto type = inst.a->type;

                            if (type->kind == AST_Type_Kind::ENUM) {
                                type = type->enum_type.base_type;
                            }

                            assert(type->kind == AST_Type_Kind::INTEGER);
                            assert(type->bit_size == 64);

                            bool match;
                            if (type->integer.sign) {
                                match = operand_val.integer_literal.s64 ==
                                        case_info.case_value->integer_literal.s64;
                            } else {
                                match = operand_val.integer_literal.u64 ==
                                        case_info.case_value->integer_literal.u64;
                            }

                            if (match) {
                                target_block = case_info.target_block;
                                break;
                            }

                        } else {
                            assert(!default_block);
                            default_block = case_info.target_block;
                        }
                    }

                    assert(target_block || default_block);
                    auto dest = target_block;
                    if (!dest) dest = default_block;
                    assert(dest);

                    advance_ip = false;

                    frame->ip = {
                        .index = 0,
                        .block = dest,
                    };

                    break;
                }

                case PTR_OFFSET: {
                    auto offset_val = interp_load_value(interp, inst.b);

                    assert(offset_val.type->kind == AST_Type_Kind::INTEGER);

                    auto result_val = interp_load_lvalue(interp, inst.result);

                    void *ptr = nullptr;

                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::ARRAY) {
                            Interpreter_Value *pointer_val =
                                &interp->local_stack.buffer[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::GLOBAL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::ARRAY) {
                            auto global_val = &interp->globals[pointer_lval.index];
                            assert(global_val->pointer);
                            ptr = global_val->pointer;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::PARAM) {
                        assert(false);
                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        auto pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        if (pointer_val.type->pointer.base->kind == AST_Type_Kind::ARRAY) {
#ifndef NDEBUG
                            auto array_type = pointer_val.type->pointer.base;
#endif
                            assert(array_type->array.element_type->pointer_to);
                            assert(result_val.type == array_type->array.element_type->pointer_to);
                        } else {
                            assert(result_val.type == pointer_val.type);
                        }
                        ptr = pointer_val.pointer;
                    } else {
                        assert(false);
                    }

                    AST_Type *element_type = result_val.type->pointer.base;
                    assert(element_type);
                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    assert(offset_val.type == Builtin::type_s64);
                    void *result_ptr = ((uint8_t*)ptr) +
                                       (offset_val.integer_literal.s64 * byte_size);

                    Interpreter_Value result_ptr_val = {
                        .type = result_val.type,
                        .pointer = result_ptr,
                    };

                    interp_store(interp, result_ptr_val, result_val);
                    break;
                }

                case AGG_OFFSET: {
                    auto index_val = interp_load_value(interp, inst.b);
                    assert(index_val.type->kind == AST_Type_Kind::INTEGER);

                    auto result_val = interp_load_lvalue(interp, inst.result);
                    assert(result_val.type);

                    AST_Type *struct_type = nullptr;

                    void *ptr = nullptr;

                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::STRUCTURE) {
                            Interpreter_Value *pointer_val =
                                &interp->local_stack.buffer[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                            struct_type = pointer_lval.type;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::PARAM) {
                         Interpreter_LValue pointer_lval = interp_load_lvalue(interp, inst.a);
                        if (pointer_lval.type->kind == AST_Type_Kind::STRUCTURE) {
                            Interpreter_Value *pointer_val =
                                &interp->arg_stack.buffer[pointer_lval.index];
                            assert(pointer_val->pointer);
                            ptr = pointer_val->pointer;
                            struct_type = pointer_lval.type;
                        } else {
                            assert(false);
                        }
                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        auto pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        assert(pointer_val.type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                        ptr = pointer_val.pointer;
                        struct_type = pointer_val.type->pointer.base;
                    } else {
                        assert(false);
                        // ptr = *(void**)_ptr_ptr;
                    }

                    assert(ptr);
                    assert(struct_type);

                    assert(result_val.type->kind == AST_Type_Kind::POINTER);
#ifndef NDEBUG
                    AST_Type *member_type = result_val.type->pointer.base;
#endif

                    assert(index_val.type == Builtin::type_u32);
                    auto index = index_val.integer_literal.u32;

                    assert(struct_type->structure.member_types.count > index);
                    assert(struct_type->structure.member_types[index] == member_type);

                    int64_t offset = 0;
                    for (int64_t i = 0; i < index; i++) {
                        auto mem_type = struct_type->structure.member_types[i];
                        assert(mem_type->bit_size % 8 == 0);
                        auto byte_size = mem_type->bit_size / 8;
                        offset += byte_size;
                    }

                    void *result_ptr = ((uint8_t*)ptr) + offset;

                    Interpreter_Value result_ptr_val = {
                        .type = result_val.type,
                        .pointer = result_ptr,
                    };

                    interp_store(interp, result_ptr_val, result_val);
                    break;
                }

                case ZEXT: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    AST_Type *op_type = operand.type;
                    if (op_type->kind == AST_Type_Kind::ENUM) op_type = op_type->enum_type.base_type;
                    assert(op_type->kind == AST_Type_Kind::INTEGER);
                    assert(op_type->integer.sign == false);

                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);
                    assert(dest_lval.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lval.type->pointer_to);

                    assert(dest_lval.type->bit_size > operand.type->bit_size);

#define ZEXT_CASE(size) case size: { \
    uint##size##_t new_val = 0; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: new_val = operand.integer_literal.u8; break; \
        case 16: new_val = operand.integer_literal.u16; break; \
        case 32: new_val = operand.integer_literal.u32; break; \
        case 64: new_val = operand.integer_literal.u64; break; \
    } \
    interp_store(interp, &new_val, dest_lval.type->pointer_to, dest_lval); \
    break; \
}
                    switch (dest_lval.type->bit_size) {
                        default: assert(false);
                        ZEXT_CASE(8)
                        ZEXT_CASE(16)
                        ZEXT_CASE(32)
                        ZEXT_CASE(64)
                    }

#undef ZEXT_CASE
                    break;
                }

                case SEXT: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(operand.type->integer.sign);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->integer.sign);

#define SEXT_CASE(size) case size: { \
    int##size##_t new_val; \
    switch (dest_lvalue.type->bit_size) { \
        default: assert(false); \
        case 8: new_val = operand.integer_literal.s8; break;\
        case 16: new_val = operand.integer_literal.s16; break;\
        case 32: new_val = operand.integer_literal.s32; break;\
        case 64: new_val = operand.integer_literal.s64; break;\
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &new_val, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        SEXT_CASE(8);
                        SEXT_CASE(16);
                        SEXT_CASE(32);
                        SEXT_CASE(64);
                    }
                    break;

#undef SEXT_CASE
                }

                case TRUNC: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);

#define TRUNC_CASE(size) case size: { \
    uint##size##_t new_val; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: new_val = operand.integer_literal.u8; break;\
        case 16: new_val = operand.integer_literal.u16; break;\
        case 32: new_val = operand.integer_literal.u32; break;\
        case 64: new_val = operand.integer_literal.u64; break;\
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &new_val, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        TRUNC_CASE(8)
                        TRUNC_CASE(16)
                        TRUNC_CASE(32)
                        TRUNC_CASE(64)
                    }

#undef TRUNC_CASE

                    break;
                }

                case F_TO_S: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::FLOAT);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_lvalue.type->integer.sign);

#define F_TO_S_CASE_INT_SIZE(size) case size: { \
    int##size##_t new_value = v; \
    assert(dest_lvalue.type->pointer_to); \
    interp_store(interp, &new_value, dest_lvalue.type->pointer_to, \
                 dest_lvalue); \
    break; \
}

#define F_TO_S_CASE_FLOAT_SIZE(size) case size: { \
    auto v = operand.float_literal.r##size; \
    switch (dest_lvalue.type->bit_size) { \
        default: assert(false); \
        F_TO_S_CASE_INT_SIZE(8) \
        F_TO_S_CASE_INT_SIZE(16) \
        F_TO_S_CASE_INT_SIZE(32) \
        F_TO_S_CASE_INT_SIZE(64) \
    } \
    break; \
}

                    switch (operand.type->bit_size) {
                        default: assert(false);
                        F_TO_S_CASE_FLOAT_SIZE(32);
                        F_TO_S_CASE_FLOAT_SIZE(64);
                    }


#undef F_TO_S_CASE_INT_SIZE
#undef F_TO_S_CASE_FLOAT_SIZE

                    break;
                }

                case S_TO_F: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::INTEGER);
                    assert(operand.type->integer.sign);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::FLOAT);

#define S_TO_F_CASE(size) case size: { \
    Float_Literal fl; \
    switch (operand.type->bit_size) { \
        default: assert(false); \
        case 8: fl.r##size = operand.integer_literal.s8; break; \
        case 16: fl.r##size = operand.integer_literal.s16; break; \
        case 32: fl.r##size = operand.integer_literal.s32; break; \
        case 64: fl.r##size = operand.integer_literal.s64; break; \
    } \
    auto source_type = dest_lvalue.type->pointer_to; \
    assert(source_type); \
    interp_store(interp, &fl.r##size, source_type, dest_lvalue); \
    break; \
}

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        S_TO_F_CASE(32);
                        S_TO_F_CASE(64);
                    }

                    break;
                }

                case U_TO_F: assert(false);

                case F_TO_F: {
                    Interpreter_Value operand = interp_load_value(interp, inst.a);
                    Interpreter_LValue dest_lvalue = interp_load_lvalue(interp, inst.result);

                    assert(operand.type->kind == AST_Type_Kind::FLOAT);
                    assert(dest_lvalue.type->kind == AST_Type_Kind::FLOAT);
                    assert(operand.type != dest_lvalue.type);

                    Interpreter_Value result_value = {
                        .type = dest_lvalue.type,
                    };

                    switch (dest_lvalue.type->bit_size) {
                        default: assert(false);
                        case 32: result_value.float_literal.r32 = operand.float_literal.r64; break;
                        case 64: result_value.float_literal.r64 = operand.float_literal.r32; break;
                    }

                    interp_store(interp, result_value, dest_lvalue);
                    break;
                }

                case PTR_TO_INT: {
                    assert(inst.a->type->kind == AST_Type_Kind::POINTER);
                    assert(inst.result->type->kind == AST_Type_Kind::INTEGER);

                    AST_Type *pointer_type = nullptr;
                    void *ptr;

                    // @TODO: @Cleanup: There should be a general way to get a pointer from an lvalue
                    if (inst.a->kind == BC_Value_Kind::ALLOCL) {
                        Interpreter_LValue allocl_lval = interp_load_lvalue(interp, inst.a);
                        assert(allocl_lval.kind);
                        pointer_type = allocl_lval.type->pointer_to;
                        assert(pointer_type);
                        auto index = frame->first_alloc_index + inst.a->allocl.index;
                        Interpreter_Value *val_ptr = &interp->local_stack.buffer[index];
                        ptr = &val_ptr->pointer;

                    } else if (inst.a->kind == BC_Value_Kind::TEMP) {
                        Interpreter_Value pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        pointer_type = pointer_val.type;
                        ptr = pointer_val.pointer;

                    } else if (inst.a->kind == BC_Value_Kind::STRING_LITERAL) {
                        Interpreter_Value pointer_val = interp_load_value(interp, inst.a);
                        assert(pointer_val.type->kind == AST_Type_Kind::POINTER);
                        pointer_type = pointer_val.type;
                        ptr = pointer_val.pointer;

                    } else {
                        assert(false);
                    }
                    assert(pointer_type);

                    Interpreter_LValue dest_lval = interp_load_lvalue(interp, inst.result);
                    assert(dest_lval.type->kind == AST_Type_Kind::INTEGER);

                    interp_store(interp, &ptr, pointer_type, dest_lval, true);
                    break;
                }

                case PTR_TO_PTR: assert(false);

                case SIZEOF: {
                    assert(inst.a->kind == BC_Value_Kind::TYPE);
                    auto dest = interp_load_lvalue(interp, inst.result);

                    assert(dest.type == Builtin::type_s64);
                    assert(inst.a->type->bit_size % 8 == 0);
                    int64_t size = inst.a->type->bit_size / 8;

                    assert(dest.type->pointer_to);
                    interp_store(interp, &size, dest.type->pointer_to, dest);
                    break;
                }

                case OFFSETOF: {
                    assert(inst.a->kind == BC_Value_Kind::TYPE);
                    assert(inst.b->kind == BC_Value_Kind::INTEGER_LITERAL);
                    Interpreter_Value index_val = interp_load_value(interp, inst.b);
                    assert(index_val.type == Builtin::type_s64);

                    Interpreter_LValue dest = interp_load_lvalue(interp, inst.result);

                    int64_t index = index_val.integer_literal.s64;
                    int64_t offset = 0;

                    AST_Type *struct_type = inst.a->type;
                    assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                    for (int64_t i = 0; i < index; i++) {
                        auto bit_size = struct_type->structure.member_types[i]->bit_size;
                        assert(bit_size % 8 == 0);
                        offset += (bit_size / 8);
                    }

                    interp_store(interp, &offset, dest.type->pointer_to, dest);
                    break;
                }

                case EXIT: assert(false);

                case SYSCALL: {
                    assert(inst.a->kind == BC_Value_Kind::INTEGER_LITERAL);
                    Interpreter_Value arg_count_val = interp_load_value(interp, inst.a);
                    assert(arg_count_val.type == Builtin::type_u64);

                    // @TODO: @CLEANUP: We are asserting the type is u64 but using the s64?
                    auto arg_count = arg_count_val.integer_literal.s64;

                    assert(arg_count >= 1);

                    assert(stack_count(&interp->arg_stack) >= arg_count);

                    Array<int64_t> args = {};
                    array_init(interp->allocator, &args, arg_count);

                    for (int64_t i = 0; i < arg_count; i++) {
                        auto offset = arg_count - i - 1;
                        Interpreter_Value param = stack_peek(&interp->arg_stack, offset);
                        assert(param.type == Builtin::type_s64);
                        array_append(&args, param.integer_literal.s64);
                    }

                    Interpreter_LValue result_lvalue = interp_load_lvalue(interp, inst.result);
                    assert(result_lvalue.type == Builtin::type_s64);

                    int64_t result = os_syscall(args);

                    Interpreter_Value result_source_val = {
                        .type = Builtin::type_s64,
                        .integer_literal = { .s64 = result },
                    };

                    interp_store(interp, result_source_val, result_lvalue);

                    array_free(&args);

                    stack_pop(&interp->arg_stack, arg_count);

                    break;
                }
            }

            if (advance_ip) {
                frame->ip.index += 1;
                assert(frame->ip.index <= frame->ip.block->instructions.count);
            }

            if (stack_count(&interp->frames) < 1) {
                interp->running = false;
            }
        }

        if (!interp->aborted) {
            if (!returns_void) {
                assert(stack_count(&interp->temp_stack));
                auto exit_val = stack_pop(&interp->temp_stack);
                assert(exit_val.type->kind == AST_Type_Kind::INTEGER);
                assert(exit_val.type == Builtin::type_s64);

                interp->exit_code = exit_val.integer_literal.s64;
            }
        } else {
            fprintf(stderr, "Bytecode aborted!\n");
        }
    }

    Interpreter_Value interp_load_value(Interpreter *interp, BC_Value *bc_val)
    {
        Interpreter_Value result = {
            .type = bc_val->type,
        };

        switch (bc_val->kind) {
            case BC_Value_Kind::INVALID: assert(false);

            case BC_Value_Kind::INTEGER_LITERAL: {
                result.integer_literal = bc_val->integer_literal;
                break;
            }

            case BC_Value_Kind::FLOAT_LITERAL: {
                result.float_literal = bc_val->float_literal;
                break;
            }

            case BC_Value_Kind::STRING_LITERAL: {
                result.string_literal = bc_val->string_literal.data;
                break;
            }

            case BC_Value_Kind::BOOL_LITERAL: {
                result.boolean_literal = bc_val->bool_literal;
                break;
            }

            case BC_Value_Kind::NULL_LITERAL: {
                result.pointer = nullptr;
                break;
            }

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto temp_index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > temp_index);
                result = interp->temp_stack.buffer[temp_index];
                break;
            }

            case BC_Value_Kind::ALLOCL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto frame = stack_top_ptr(&interp->frames);
                auto alloc_index = frame->first_alloc_index + bc_val->allocl.index;
                assert(stack_count(&interp->temp_stack) > alloc_index);

                Interpreter_Value *value = &interp->local_stack.buffer[alloc_index];

                AST_Type *base_type = bc_val->type->pointer.base;

                if (base_type->kind == AST_Type_Kind::ARRAY ||
                    base_type->kind == AST_Type_Kind::STRUCTURE ||
                    base_type->kind == AST_Type_Kind::UNION) {

                    assert(value->pointer);
                    result.pointer = value->pointer;
                } else {
                    result.pointer = &value->integer_literal;
                }
                break;
            }

            case BC_Value_Kind::PARAM: {
                auto frame = stack_top_ptr(&interp->frames);
                auto param_index = frame->first_arg_index + bc_val->parameter.index;
                assert(stack_count(&interp->arg_stack) > param_index);
                result = interp->arg_stack.buffer[param_index];
                break;
            }

            case BC_Value_Kind::GLOBAL: {
                assert(interp->globals.count > bc_val->global.index);
                result = interp->globals[bc_val->global.index];
                break;
            }

            case BC_Value_Kind::FUNCTION: {
               assert(result.type->kind == AST_Type_Kind::POINTER &&
                      result.type->pointer.base->kind == AST_Type_Kind::FUNCTION);
                result.pointer = bc_val->function;
                break;
            }

            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    Interpreter_LValue interp_load_lvalue(Interpreter *interp, BC_Value *bc_val)
    {
        Interpreter_LValue result = {};

        switch (bc_val->kind) {
            case BC_Value_Kind::INVALID: assert(false);
            case BC_Value_Kind::INTEGER_LITERAL: assert(false);
            case BC_Value_Kind::FLOAT_LITERAL: assert(false);
            case BC_Value_Kind::STRING_LITERAL: assert(false);
            case BC_Value_Kind::BOOL_LITERAL: assert(false);
            case BC_Value_Kind::NULL_LITERAL: assert(false);

            case BC_Value_Kind::TEMP: {
                auto frame = stack_top_ptr(&interp->frames);
                auto index = frame->first_temp_index + bc_val->temp.index;
                assert(stack_count(&interp->temp_stack) > index);

                result.kind = Interp_LValue_Kind::TEMP;
                result.index = index;
                result.type = bc_val->type;
                break;
            }

            case BC_Value_Kind::ALLOCL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto frame = stack_top_ptr(&interp->frames);
                auto index = frame->first_alloc_index + bc_val->allocl.index;
                assert(stack_count(&interp->local_stack) > index);


                result.kind = Interp_LValue_Kind::ALLOCL;
                result.index = index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::PARAM: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);

                auto frame = stack_top_ptr(&interp->frames);
                auto index = frame->first_arg_index + bc_val->parameter.index;
                assert(stack_count(&interp->arg_stack) > index);


                result.kind = Interp_LValue_Kind::PARAM;
                result.index = index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::GLOBAL: {
                assert(bc_val->type->kind == AST_Type_Kind::POINTER);
                assert(interp->globals.count > bc_val->global.index);

                result.kind = Interp_LValue_Kind::GLOBAL;
                result.index = bc_val->global.index;
                result.type = bc_val->type->pointer.base;
                break;
            }

            case BC_Value_Kind::FUNCTION: assert(false);
            case BC_Value_Kind::BLOCK: assert(false);
            case BC_Value_Kind::TYPE: assert(false);
            case BC_Value_Kind::SWITCH_DATA: assert(false);
        }

        return result;
    }

    void interp_store(Interpreter *interp, Interpreter_Value source, Interpreter_LValue dest)
    {
        assert(source.type);
        assert(dest.type);
        assert(source.type == dest.type);

        auto type = source.type;

        Interpreter_Value *dest_ptr = nullptr;

        switch (dest.kind) {
            case Interp_LValue_Kind::INVALID: assert(false);

            case Interp_LValue_Kind::TEMP: {
                assert(stack_count(&interp->temp_stack) > dest.index);
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                assert(stack_count(&interp->local_stack) > dest.index);
                dest_ptr = &interp->local_stack.buffer[dest.index];
                break;
            }

            case Interp_LValue_Kind::PARAM: {
                assert(stack_count(&interp->arg_stack) > dest.index);
                dest_ptr = &interp->arg_stack.buffer[dest.index];
                break;
            }

            case Interp_LValue_Kind::GLOBAL: {
                assert(interp->globals.count > dest.index);
                dest_ptr = &interp->globals[dest.index];
                break;
            }
        }

        assert(dest_ptr);
        if (dest_ptr->type->kind == AST_Type_Kind::ENUM &&
            source.type->kind != AST_Type_Kind::ENUM) {
            assert(dest_ptr->type->enum_type.base_type == source.type);
        } else {
            assert(dest_ptr->type == source.type);
        }

        switch (type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                dest_ptr->integer_literal = source.integer_literal;
                break;
            }

            case AST_Type_Kind::FLOAT: {
                dest_ptr->float_literal = source.float_literal;
                break;
            }

            case AST_Type_Kind::BOOL: {
                dest_ptr->boolean_literal = source.boolean_literal;
                break;
            }

            case AST_Type_Kind::POINTER: {
                dest_ptr->pointer = source.pointer;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: {
                dest_ptr->pointer = source.pointer;
                break;
            }

            case AST_Type_Kind::UNION: assert(false);

            case AST_Type_Kind::ENUM: {
                assert(type->enum_type.base_type->kind == AST_Type_Kind::INTEGER);
                dest_ptr->integer_literal = source.integer_literal;
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interp_store(Interpreter *interp, Interpreter_Value source, void *dest_ptr,
                      AST_Type *dest_type)
    {
        assert(dest_ptr);
        assert(dest_type->kind == AST_Type_Kind::POINTER);
        assert(dest_type->pointer.base == source.type);

        switch (source.type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                switch (source.type->bit_size) {
                    default: assert(false);
                    case 8: *(uint8_t*)dest_ptr = source.integer_literal.u8; break;
                    case 16: *(uint16_t*)dest_ptr = source.integer_literal.u16; break;
                    case 32: *(uint32_t*)dest_ptr = source.integer_literal.u32; break;
                    case 64: *(uint64_t*)dest_ptr = source.integer_literal.u64; break;
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                switch (source.type->bit_size) {
                    default: assert(false);
                    case 32: *(float*)dest_ptr = source.float_literal.r32;
                    case 64: *(float*)dest_ptr = source.float_literal.r64;
                }
                break;
            }

            case AST_Type_Kind::BOOL: assert(false);

            case AST_Type_Kind::POINTER: {
                *(void**)dest_ptr = source.pointer;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);

            case AST_Type_Kind::STRUCTURE: {
                assert(source.type->bit_size % 8 == 0);
                auto byte_size = source.type->bit_size / 8;
                assert(source.pointer);
                memcpy(dest_ptr, source.pointer, byte_size);
                break;
            }

            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interp_store(Interpreter *interp, void *source_ptr, AST_Type *source_type,
                      Interpreter_LValue dest, bool allow_type_mismatch /*= false*/)
    {
        assert(source_type->kind == AST_Type_Kind::POINTER);

        if (!allow_type_mismatch) {
            assert(dest.type == source_type->pointer.base);
        }

        Interpreter_Value *dest_ptr = nullptr;

        switch (dest.kind) {
            case Interp_LValue_Kind::INVALID: assert(false);

            case Interp_LValue_Kind::TEMP: {
                dest_ptr = &interp->temp_stack.buffer[dest.index];
                assert(stack_count(&interp->temp_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::ALLOCL: {
                dest_ptr = &interp->local_stack.buffer[dest.index];
                assert(stack_count(&interp->local_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::PARAM: {
                dest_ptr = &interp->arg_stack.buffer[dest.index];
                assert(stack_count(&interp->arg_stack) > dest.index);
                break;
            }

            case Interp_LValue_Kind::GLOBAL: {
                assert(false);
                break;
            }
        }

        assert(dest_ptr);
        if (!allow_type_mismatch) {
            assert(dest_ptr->type == source_type->pointer.base);
        }

        switch(dest.type->kind) {
            case AST_Type_Kind::INVALID: assert(false);
            case AST_Type_Kind::VOID: assert(false);

            case AST_Type_Kind::INTEGER: {
                switch (dest.type->bit_size) {
                    default: assert(false);
                    case 8: dest_ptr->integer_literal.u8 = *((uint8_t*)source_ptr); break;
                    case 16: dest_ptr->integer_literal.u16 = *((uint16_t*)source_ptr); break;
                    case 32: dest_ptr->integer_literal.u32 = *((uint32_t*)source_ptr); break;
                    case 64: dest_ptr->integer_literal.u64 = *((uint64_t*)source_ptr); break;
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (dest.type == Builtin::type_float) {
                    dest_ptr->float_literal.r32 = *((float*)source_ptr); break;
                } else if (dest.type == Builtin::type_double) {
                    dest_ptr->float_literal.r64 = *((double*)source_ptr); break;
                } else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::BOOL: {
                dest_ptr->boolean_literal = *(bool*)source_ptr;
                break;
            }

            case AST_Type_Kind::POINTER:
            {
                dest_ptr->pointer = *(void**)source_ptr;
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);
        }
    }

    void interpreter_initialize_globals(Interpreter *interp, Array<BC_Global_Info> global_info,
                                        int64_t global_data_size)
    {
        if (global_info.count < 1) return;

        array_init(interp->allocator, &interp->globals, global_info.count);

        assert(interp->global_mem == nullptr);
        uint8_t *gm_cursor = nullptr;
        if (global_data_size > 0) {
            interp->global_mem = alloc_array<uint8_t>(interp->allocator, global_data_size);
            gm_cursor = interp->global_mem;
        }

        for (int64_t i = 0; i < global_info.count; i++) {
            auto info = global_info[i];
            auto type = info.declaration->type;
            Interpreter_Value global_value = { };
            global_value.type = type;

            array_append(&interp->globals, global_value);

            Interpreter_LValue dest_lval = {
                .kind = Interp_LValue_Kind::GLOBAL,
                .type = type,
                .index = i,
            };

            if (info.has_initializer) {
                interp_store_constant(interp, info.init_const_val, dest_lval);
            } else if (type->kind == AST_Type_Kind::ARRAY ||
                       type->kind == AST_Type_Kind::STRUCTURE ||
                       type->kind == AST_Type_Kind::UNION) {

                assert(type->bit_size % 8 == 0);
                auto byte_size = type->bit_size / 8;

                auto val_ptr = &interp->globals[dest_lval.index];
                assert(gm_cursor);
                val_ptr->pointer = gm_cursor;
                gm_cursor += byte_size;

                memset(val_ptr->pointer, 0 , byte_size);
            }
        }
    }

    void interp_store_constant(Interpreter *interp, Const_Value const_val,
                               Interpreter_LValue dest)
    {
        assert(const_val.type->kind == AST_Type_Kind::INTEGER);

        Interpreter_Value val = { .type = const_val.type, .integer_literal = const_val.integer };
        interp_store(interp, val, dest);
    }

    void interp_store_constant(Interpreter *interp, Const_Value const_val, void *dest_ptr)
    {
        assert(const_val.type->kind == AST_Type_Kind::INTEGER);
        assert(const_val.type->pointer_to);

        Interpreter_Value val = { .type = const_val.type, .integer_literal = const_val.integer };
        interp_store(interp, val, dest_ptr, val.type->pointer_to);
    }

    void interpreter_initialize_foreigns(Interpreter *interp,
                                         Array<BC_Function *> foreign_functions)
    {
        for (int64_t i = 0; i < foreign_functions.count; i++) {
            auto func = foreign_functions[i];
            bool found = ffi_load_function(&interp->ffi, func->name);
            if (!found) {
                fprintf(stderr, "Did not find foreign function: '%s'\n", func->name.data);
            }
            assert(found);
        }
    }

    void interpreter_execute_foreign_function(Interpreter *interp, BC_Function *func,
                                              int64_t arg_count, BC_Value *result_value)
    {
        assert(func->parameters.count == arg_count);
        assert(stack_count(&interp->arg_stack) >= arg_count);

        if (result_value) {
            assert(result_value->kind == BC_Value_Kind::TEMP);
            assert(func->type->function.return_type);
        }

        ffi_reset(&interp->ffi);

        for (int64_t i = 0; i < arg_count; i++) {
            auto param = func->parameters[i];
            auto param_type = param->type;
            assert(param_type->kind == AST_Type_Kind::POINTER);
            param_type = param_type->pointer.base;

            void *arg_ptr = nullptr;

            auto param_ptr = stack_peek_ptr(&interp->arg_stack, arg_count - 1 - i);

            if (param_type->kind == AST_Type_Kind::ARRAY ||
                param_type->kind == AST_Type_Kind::STRUCTURE ||
                param_type->kind == AST_Type_Kind::UNION) {

                assert(false);
            } else {
                arg_ptr = &param_ptr->pointer;
            }

            assert(arg_ptr);

            ffi_push_arg(&interp->ffi, arg_ptr, param_type);
        }

        AST_Type *return_type = nullptr;
        void *return_val_ptr = nullptr;

        if (result_value) {
            return_type = result_value->type;

            assert(result_value->kind == BC_Value_Kind::TEMP);
            assert(return_type->kind != AST_Type_Kind::ARRAY ||
                   return_type->kind != AST_Type_Kind::STRUCTURE ||
                   return_type->kind != AST_Type_Kind::UNION);
            auto rv_lval = interp_load_lvalue(interp, result_value);
            auto result_val_ptr = interp->temp_stack.buffer[rv_lval.index];
            return_val_ptr = &result_val_ptr.pointer;
        }

        ffi_call(&interp->ffi, func->name, return_val_ptr, return_type);

        stack_pop(&interp->arg_stack, arg_count);
    }

    void interpreter_execute_compiler_function(Interpreter *interp, BC_Function *func,
                                               int64_t arg_count)
    {
        assert(func->flags & BC_FUNC_FLAG_COMPILER_FUNC);

        assert(arg_count == 0);

        if (func->name == Builtin::atom_abort) {
            interp->aborted = true;
            interp->running = false;
            interp->exit_code = 134;
        } else {
            assert(false && "Unimplemented compiler function!");
        }
    }

    bool interp_is_known_function_pointer(Interpreter *interp, BC_Function *func)
    {
        for (int64_t i = 0; i < interp->functions.count; i++) {
            if (interp->functions[i] == func) {
                return true;
            }
        }

        return false;
    }
}
