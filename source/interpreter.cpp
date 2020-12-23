
#include "interpreter.h"

#include "builtin.h"
#include "os.h"

#include <cstdio>

namespace Zodiac
{

    Interpreter interpreter_create(Allocator *allocator, Build_Data *build_data)
    {
        Interpreter result = {};

        result.allocator = allocator;
        result.build_data = build_data;

        result.running = false;

        result.stack_size = 4096;
        result.stack = alloc_array<uint8_t>(allocator, 4096);
        result.sp = 0;

        result.frame_pointer = 0;
        result.ip = {};

        result.global_data = nullptr;
        result.ffi = ffi_create(allocator, build_data);

        result.null_pointer = nullptr;

        result.exit_code = 0;

        return result;
    }

    void interpreter_start(Interpreter *interp, Bytecode_Function *entry_func,
                           int64_t global_data_size, Array<Bytecode_Global_Info> global_info,
                           Array<Bytecode_Function *> foreign_functions)
    {
        assert(entry_func->blocks.count);

        interp->ip = bucket_array_locator_by_index(&entry_func->instructions, 0);

        interpreter_initialize_globals(interp, global_data_size, global_info);
        interpreter_initialize_foreigns(interp, foreign_functions);

        int64_t ret_val;
        void *ret_val_ptr = nullptr;

        AST_Type *ret_type = entry_func->type->function.return_type;

        if (ret_type != Builtin::type_void) {
            if (ret_type->bit_size <= 64) {
                ret_val_ptr = &ret_val;
            } else {
                assert(false);
                // allocated_ret_val = true;
                // assert(ret_type->bit_size % 8 == 0);
                // auto size = ret_type->bit_size / 8;
                // ret_val_ptr = alloc(interp->allocator, size);
            }
        }

        assert(ret_val_ptr || ret_type == Builtin::type_void);

        interp_stack_push(interp, (int64_t)0); // total_arg_size
        interp->frame_pointer += sizeof(int64_t);
        interp_stack_push(interp, (int64_t)-1); // fp
        Instruction_Pointer empty_ip = {};
        interp_stack_push(interp, empty_ip);
        interp_stack_push(interp, ret_val_ptr); // ret_val_ptr

        for (int64_t i = 0; i < entry_func->locals.count; i++)
        {
            auto allocl = entry_func->locals[i];
            assert(allocl->type->kind == AST_Type_Kind::POINTER);
            auto allocl_type = allocl->type->pointer.base;

            assert(allocl_type->bit_size % 8 == 0);
            int64_t size = allocl_type->bit_size / 8;
            assert(interp->sp + size <= interp->stack_size);

            allocl->allocl.byte_offset_from_fp = interp->sp;

            interp->sp += size;
        }

        for (int64_t i = 0; i < entry_func->temps.count; i++)
        {
            auto temp = entry_func->temps[i];

            assert(temp->type->bit_size % 8 == 0);
            int64_t size = temp->type->bit_size / 8;
            assert(interp->sp + size <= interp->stack_size);

            temp->temp.byte_offset_from_fp = interp->sp;

            interp->sp += size;
        }

        interp->running = true;

        while (interp->running)
        {
            Bytecode_Instruction *inst = bucket_locator_get_ptr(interp->ip);

            bool advance_ip = true;

            switch (inst->op)
            {
                case NOP: assert(false);

                case ALLOCL: {
                    break;
                }

                case STOREL:
                case STORE_GLOBAL:
                case STORE_ARG: {
                    uint8_t *dest_ptr = interpreter_load_lvalue(interp, inst->a);

                    if (inst->b->kind == Bytecode_Value_Kind::ALLOCL) {
                        uint8_t *alloc_addr = interpreter_load_lvalue(interp, inst->b);
                        interp_store(dest_ptr, alloc_addr);
                    } else {
                        Bytecode_Value source_val = interpreter_load_value(interp, inst->b);

                        if (source_val.type->kind == AST_Type_Kind::STRUCTURE) {
                            assert(source_val.type->bit_size % 8 == 0);
                            auto byte_size = source_val.type->bit_size / 8;
                            assert(source_val.pointer);
                            memcpy(dest_ptr, source_val.pointer, byte_size);
                        } else {
                            interp_store_value(dest_ptr, source_val);
                        }
                    }
                    break;
                }

                case STORE_PTR: {
                    Bytecode_Value ptr_val = interpreter_load_value(interp, inst->a);
                    Bytecode_Value source_val = interpreter_load_value(interp, inst->b);
                    if (source_val.type->kind == AST_Type_Kind::STRUCTURE) {
                        assert(ptr_val.pointer);
                        assert(source_val.pointer);
                        assert(source_val.type->bit_size % 8 == 0);
                        auto byte_size = source_val.type->bit_size / 8;
                        memcpy(ptr_val.pointer, source_val.pointer, byte_size);
                    } else {
                        interp_store_value((uint8_t*)ptr_val.pointer, source_val);
                    }
                    break;
                }

                case LOADL: {
                    Bytecode_Value result_value = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    interp_store_value(result_addr, result_value);

                    break;
                }

                case LOAD_PARAM: {
                    Bytecode_Value arg_value = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    interp_store_value(result_addr, arg_value);
                    break;
                }

                case LOAD_GLOBAL: {
                    Bytecode_Value glob_value = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    interp_store_value(result_addr, glob_value);
                    break;
                }

                case LOAD_PTR: {
                    Bytecode_Value ptr_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                    auto element_type = ptr_val.type->pointer.base;

                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    memcpy(result_addr, ptr_val.pointer, byte_size);
                    break;
                }

#define _binop_arithmetic_int(op, signed) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    if (signed) {  \
        switch (lhs.type->bit_size) { \
            case 8: { \
                int8_t result_value = lhs.integer_literal.s8 op rhs.integer_literal.s8; \
                interp_store(result_addr, result_value); \
                break; } \
            case 16: { \
                int16_t result_value = lhs.integer_literal.s16 op rhs.integer_literal.s16; \
                interp_store(result_addr, result_value); \
                break; } \
            case 32: { \
                int32_t result_value = lhs.integer_literal.s32 op rhs.integer_literal.s32; \
                interp_store(result_addr, result_value); \
                break; }\
            case 64: {\
                int64_t result_value = lhs.integer_literal.s64 op rhs.integer_literal.s64; \
                interp_store(result_addr, result_value); \
                break; } \
        } \
    } else {\
        switch (lhs.type->bit_size) { \
            case 8: { \
                uint8_t result_value = lhs.integer_literal.u8 op rhs.integer_literal.u8; \
                interp_store(result_addr, result_value); \
                break; } \
            case 16: { \
                uint16_t result_value = lhs.integer_literal.u16 op rhs.integer_literal.u16; \
                interp_store(result_addr, result_value); \
                break; } \
            case 32: { \
                uint32_t result_value = lhs.integer_literal.u32 op rhs.integer_literal.u32; \
                interp_store(result_addr, result_value); \
                break; }\
            case 64: {\
                uint64_t result_value = lhs.integer_literal.u64 op rhs.integer_literal.u64; \
                interp_store(result_addr, result_value); \
                break; } \
        } \
    } \
    break; \
}

                case ADD_S:_binop_arithmetic_int(+, true);
                case SUB_S:_binop_arithmetic_int(-, true);
                case REM_S:_binop_arithmetic_int(%, true);
                case MUL_S:_binop_arithmetic_int(*, true);
                case DIV_S:_binop_arithmetic_int(/, true);

                case ADD_U:_binop_arithmetic_int(+, false);
                case SUB_U:_binop_arithmetic_int(-, false);
                case REM_U:_binop_arithmetic_int(%, false);
                case MUL_U:_binop_arithmetic_int(*, false);
                case DIV_U:_binop_arithmetic_int(/, false);

#undef _binop_arithmetic_int

#define _binop_arithmetic_float(op, signed) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    if (lhs.type == Builtin::type_float) { \
        float result_value = lhs.float_literal.r32 op rhs.float_literal.r32; \
        interp_store(result_addr, result_value); \
    } else if (lhs.type == Builtin::type_double) { \
        double result_value = lhs.float_literal.r64 op rhs.float_literal.r64; \
        interp_store(result_addr, result_value); \
    } else { \
        assert(false); \
    } \
    break; \
}

                case ADD_F: _binop_arithmetic_float(+, false);
                case SUB_F: _binop_arithmetic_float(-, false);
                case MUL_F: _binop_arithmetic_float(*, false);
                case DIV_F: _binop_arithmetic_float(/, false);

#undef _binop_arithmetic_float

#define _binop_compare_int(op) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    auto type = lhs.type; \
    if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    bool result_value = false; \
    if (type->kind == AST_Type_Kind::INTEGER) { \
        if (type->integer.sign) { \
            switch (type->bit_size) { \
                case 8:\
                    result_value = lhs.integer_literal.s8 op rhs.integer_literal.s8; break; \
                case 16: \
                    result_value = lhs.integer_literal.s16 op rhs.integer_literal.s16; break; \
                case 32: \
                    result_value = lhs.integer_literal.s32 op rhs.integer_literal.s32; break; \
                case 64: \
                    result_value = lhs.integer_literal.s64 op rhs.integer_literal.s64; break; \
                default: assert(false); \
            } \
        } else { \
            switch (type->bit_size) { \
                case 8: \
                    result_value = lhs.integer_literal.u8 op rhs.integer_literal.u8; break; \
                case 16: \
                    result_value = lhs.integer_literal.u16 op rhs.integer_literal.u16; break; \
                case 32: \
                    result_value = lhs.integer_literal.u32 op rhs.integer_literal.u32; break; \
                case 64: \
                    result_value = lhs.integer_literal.u64 op rhs.integer_literal.u64; break; \
                default: assert(false); \
            } \
        } \
    } else if (type->kind == AST_Type_Kind::POINTER) { \
        result_value = lhs.pointer op rhs.pointer;\
    } else { \
        assert(false && "Unsupported binop compare"); \
    } \
    assert(sizeof(result_value) == (inst->result->type->bit_size / 8)); \
    interp_store(result_addr, result_value); \
    break; \
}

                case EQ_S:   _binop_compare_int(==);
                case NEQ_S:  _binop_compare_int(!=);
                case LT_S:   _binop_compare_int(<);
                case LTEQ_S: _binop_compare_int(<=);
                case GT_S:   _binop_compare_int(>);
                case GTEQ_S: _binop_compare_int(>=);

                case EQ_U:   _binop_compare_int(==);
                case NEQ_U:  _binop_compare_int(!=);
                case LT_U:   _binop_compare_int(<);
                case LTEQ_U: _binop_compare_int(<=);
                case GT_U:   _binop_compare_int(>);
                case GTEQ_U: _binop_compare_int(>=);

#undef _binop_compare_int

#define _binop_compare_float(op) { \
    auto lhs = interpreter_load_value(interp, inst->a); \
    auto rhs = interpreter_load_value(interp, inst->b); \
    assert(lhs.type == rhs.type); \
    assert(lhs.type->kind == AST_Type_Kind::FLOAT); \
    auto result_addr = interpreter_load_lvalue(interp, inst->result); \
    bool result_value = false; \
    if (lhs.type == Builtin::type_float) { \
        result_value = lhs.float_literal.r32 op rhs.float_literal.r32; \
    } else if (lhs.type == Builtin::type_double) { \
        result_value = lhs.float_literal.r64 op rhs.float_literal.r64; \
    } else { \
        assert(false); \
    } \
    assert(sizeof(result_value) == (inst->result->type->bit_size / 8)); \
    interp_store(result_addr, result_value); \
    break; \
}

                case EQ_F: _binop_compare_float(==);
                case NEQ_F: _binop_compare_float(!=);
                case LT_F: _binop_compare_float(<);
                case LTEQ_F: _binop_compare_float(<=);
                case GT_F: _binop_compare_float(>);
                case GTEQ_F: _binop_compare_float(>=);

#undef _binop_compare_float

                case NEG_LOG: {
                    auto operand = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    bool result_value = false;

                    if (operand.type->kind == AST_Type_Kind::BOOL ||
                        operand.type->kind == AST_Type_Kind::INTEGER) {
                        // @TODO: @Cleanup: I think we should check all sizes and the sign here?
                        result_value = operand.integer_literal.s64 == 0;
                    } else if (operand.type->kind == AST_Type_Kind::POINTER) {
                        result_value = operand.pointer == nullptr;
                    }

                    assert(inst->result->type->bit_size == 8);
                    interp_store(result_addr, (uint8_t)result_value);
                }

                case PUSH_ARG:
                {
                    Bytecode_Value arg_val = {};
                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        auto ptr = interpreter_load_lvalue(interp, inst->a);
                        arg_val.pointer = ptr;
                        arg_val.type = inst->a->type;
                        assert(arg_val.type->kind == AST_Type_Kind::POINTER);
                    } else {
#ifndef NDEBUG
                        auto kind = inst->a->kind;
                        assert(kind == Bytecode_Value_Kind::TEMP ||
                               kind == Bytecode_Value_Kind::INTEGER_LITERAL ||
                               kind == Bytecode_Value_Kind::FLOAT_LITERAL ||
                               kind == Bytecode_Value_Kind::STRING_LITERAL ||
                               kind == Bytecode_Value_Kind::BOOL_LITERAL ||
                               kind == Bytecode_Value_Kind::NULL_LITERAL);
#endif
                        arg_val = interpreter_load_value(interp, inst->a);
                    }

                    assert(arg_val.type->bit_size % 8 == 0);
                    int64_t size = arg_val.type->bit_size / 8;
                    assert(interp->sp + size <= interp->stack_size);

                    uint8_t *arg_ptr = &interp->stack[interp->sp];
                    interp->sp += size;

                    if (arg_val.type->kind == AST_Type_Kind::STRUCTURE) {
                        assert(arg_val.pointer);
                        memcpy(arg_ptr, arg_val.pointer, size);
                    } else {
                        interp_store_value(arg_ptr, arg_val);
                    }

                    break;
                }

                case CALL: {
                    auto func_val = inst->a;
                    assert(func_val->kind == Bytecode_Value_Kind::FUNCTION);
                    auto func = func_val->function;
                    assert((func->flags & BC_FUNC_FLAG_EMITTED) ||
                           (func->flags & BC_FUNC_FLAG_FOREIGN));

                    auto arg_count_val = inst->b;
                    assert(arg_count_val->type == Builtin::type_s64);
                    assert(arg_count_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    if (func->flags & BC_FUNC_FLAG_FOREIGN) {
                        interpreter_execute_foreign_function(interp, func, arg_count,
                                                             inst->result);
                        break;
                    }

                    if (func->flags & BC_FUNC_FLAG_COMPILER_FUNC) {
                        interpreter_execute_compiler_function(interp, func, arg_count,
                                                              inst->result);
                        break;
                    }

                    advance_ip = false;

                    int64_t param_offset = -sizeof(int64_t);
                    int64_t total_arg_size = 0;

                    for (int64_t i = arg_count - 1; i >= 0; i--) {
                        auto param = func->parameters[i];
                        auto param_type = param->type;
                        assert(param_type->kind == AST_Type_Kind::POINTER);
                        param_type = param_type->pointer.base;

                        assert(param_type->bit_size % 8 == 0);
                        auto size = param_type->bit_size / 8;

                        param_offset -= size;
                        total_arg_size += size;

                        param->parameter.byte_offset_from_fp = param_offset;
                    }

                    interp_stack_push(interp, (int64_t)total_arg_size);

                    auto new_fp = interp->sp;
                    interp_stack_push(interp, interp->frame_pointer);
                    interp_stack_push(interp, interp->ip);

                    uint8_t *ret_val_ptr = nullptr;
                    if (inst->result) {
                        ret_val_ptr = interpreter_load_lvalue(interp, inst->result);
                    }
                    interp_stack_push(interp, ret_val_ptr);

                    for (int64_t i = 0; i < func->locals.count; i++) {
                        auto allocl = func->locals[i];
                        assert(allocl->type->kind == AST_Type_Kind::POINTER);
                        auto allocl_type = allocl->type->pointer.base;

                        assert(allocl_type->bit_size % 8 == 0);
                        int64_t size = allocl_type->bit_size / 8;
                        assert(interp->sp + size <= interp->stack_size);

                        auto offset = interp->sp - new_fp;
                        allocl->allocl.byte_offset_from_fp = offset;

                        interp->sp += size;
                    }

                    for (int64_t i = 0; i < func->temps.count; i++) {
                        auto temp = func->temps[i];

                        assert(temp->type->bit_size % 8 == 0);
                        int64_t size = temp->type->bit_size / 8;
                        assert(interp->sp + size <= interp->stack_size);

                        auto offset = interp->sp - new_fp;
                        temp->temp.byte_offset_from_fp = offset;

                        interp->sp += size;
                    }

                    assert(inst->a->kind == Bytecode_Value_Kind::FUNCTION);

                    interp->frame_pointer = new_fp;
                    interp->ip =
                        bucket_array_locator_by_index(&inst->a->function->instructions, 0);

                    break;
                }

                case RETURN: {
                    auto ret_val = interpreter_load_value(interp, inst->a);

                    int64_t offset = 0;
                    int64_t old_fp = *(int64_t*)(&interp->stack[interp->frame_pointer]);

                    if (old_fp != -1) {
                        offset += sizeof(old_fp);
                        interp->ip =
                            *(Instruction_Pointer*)(&interp->stack[interp->frame_pointer +
                                                                   offset]);

                        offset += sizeof(Instruction_Pointer);
                        uint8_t *ret_val_ptr =
                            *(uint8_t**)(&interp->stack[interp->frame_pointer + offset]);

                        interp_store_value(ret_val_ptr, ret_val);

                        interp->sp = interp->frame_pointer;
                        interp->frame_pointer = old_fp;

                        int64_t total_arg_size = interp_stack_pop<int64_t>(interp);
                        interp->sp -= total_arg_size;
                    } else {
                        interp->running = false;
                        advance_ip = false;

                        interp_store_value((uint8_t*)ret_val_ptr, ret_val);
                    }
                    break;
                }

                case RETURN_VOID: {
                    int64_t offset = 0;
                    int64_t old_fp = *(int64_t*)(&interp->stack[interp->frame_pointer]);

                    if (old_fp != -1) {
                        offset += sizeof(old_fp);
                        interp->ip = *(Instruction_Pointer*)(&interp->stack[interp->frame_pointer +
                                                                            offset]);
                        interp->sp = interp->frame_pointer;
                        interp->frame_pointer = old_fp;

                        int64_t total_arg_size = interp_stack_pop<int64_t>(interp);
                        interp->sp -= total_arg_size;

                    } else {
                        interp->running = false;
                        advance_ip = false;
                    }
                    break;
                }

                case JUMP: {
                    advance_ip = false;
                    Bytecode_Block *target_block = inst->a->block;

                    interp->ip = target_block->first_instruction;
                    break;
                }

                case JUMP_IF: {
                    advance_ip = false;
                    Bytecode_Value cond_val = interpreter_load_value(interp, inst->a);
                    assert(cond_val.type->kind == AST_Type_Kind::BOOL);

                    assert(inst->b->kind == Bytecode_Value_Kind::BLOCK);
                    assert(inst->result->kind == Bytecode_Value_Kind::BLOCK);

                    Bytecode_Block *then_block = inst->b->block;
                    Bytecode_Block *else_block = inst->result->block;
                    Bytecode_Block *target_block = nullptr;

                    if (cond_val.integer_literal.u8) target_block = then_block;
                    else target_block = else_block;
                    assert(target_block);

                    interp->ip = target_block->first_instruction;
                    break;
                }

                case SWITCH: {
                    advance_ip = false;
                    auto switch_val = interpreter_load_value(interp, inst->a);

                    Bytecode_Block *default_block = nullptr;
                    Bytecode_Block *target_block = nullptr;

                    assert(inst->b->kind == Bytecode_Value_Kind::SWITCH_DATA);
                    auto switch_data = inst->b->switch_data;

                    for (int64_t i = 0; i < switch_data.cases.count; i++) {
                        auto case_info = switch_data.cases[i];

                        if (case_info.case_value) {
                            assert(switch_val.type == case_info.case_value->type);
                            auto type = switch_val.type;

                            if (type->kind == AST_Type_Kind::ENUM)
                                type = type->enum_type.base_type;

                            assert(type->kind == AST_Type_Kind::INTEGER);
                            assert(type->bit_size == 64);

                            bool match;
                            if (type->integer.sign) {
                                match = switch_val.integer_literal.s64 ==
                                        case_info.case_value->integer_literal.s64;
                            } else {
                                match = switch_val.integer_literal.u64 ==
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

                    interp->ip = dest->first_instruction;
                    break;
                }

                case PTR_OFFSET: {
                    auto ptr_val = interpreter_load_value(interp, inst->a);
                    auto offset_val = interpreter_load_value(interp, inst->b);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                    assert(offset_val.type->kind == AST_Type_Kind::INTEGER);

                    AST_Type *element_type = ptr_val.type->pointer.base;
                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    void *result = ((uint8_t*)ptr_val.pointer) +
                                   (offset_val.integer_literal.s64 * byte_size);
                    interp_store(result_addr, result);
                    break;
                }

                case AGG_OFFSET: {
                    auto ptr_val = interpreter_load_value(interp, inst->a);
                    auto index_val = interpreter_load_value(interp, inst->b);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(ptr_val.pointer);

                    AST_Type *struct_type = nullptr;
                    if (ptr_val.type->kind == AST_Type_Kind::STRUCTURE) {
                        struct_type = ptr_val.type;
                    } else {
                        assert(ptr_val.type->kind == AST_Type_Kind::POINTER);
                        assert(ptr_val.type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                        struct_type = ptr_val.type->pointer.base;
                    }
                    assert(struct_type);

                    assert(index_val.type == Builtin::type_u32);
                    assert(index_val.kind == Bytecode_Value_Kind::INTEGER_LITERAL);

                    int64_t byte_offset = 0;
                    for (int64_t i = 0; i < index_val.integer_literal.u32; i++) {
                        auto mem_type = struct_type->structure.member_types[i];
                        auto bit_size = mem_type->bit_size;
                        assert(bit_size % 8 == 0);
                        byte_offset += (bit_size / 8);
                    }

                    void *result = ((uint8_t*)ptr_val.pointer) + byte_offset;
                    interp_store(result_addr, result);

                    break;
                }

                case ZEXT: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size > operand_val.type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: assert(false);

                        case 64: {
                            uint64_t new_val = 0xdddd;
                            switch (operand_val.type->bit_size) {
                                case 8: new_val = operand_val.integer_literal.u8; break;
                                case 16: new_val = operand_val.integer_literal.u16; break;
                                case 32: new_val = operand_val.integer_literal.u32; break;
                                case 64: new_val = operand_val.integer_literal.u64; break;
                                default: assert(false);
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }
                    }

                    break;
                }

                case SEXT: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size > operand_val.type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: assert(false);
                        case 64: {
                            int64_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 8: new_val = operand_val.integer_literal.s8; break;
                                case 16: new_val = operand_val.integer_literal.s16; break;
                                case 32: new_val = operand_val.integer_literal.s32; break;
                                case 64: new_val = operand_val.integer_literal.s64; break;
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }
                    }
                    break;
                }

                case TRUNC: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size <= operand_val.type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: {
                            uint8_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 64: new_val = operand_val.integer_literal.u64; break;
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }

                        case 16: assert(false);

                        case 32: {
                            uint32_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 64: new_val = operand_val.integer_literal.u64; break;
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }

                        case 64: {
                            uint64_t new_val;
                            switch (operand_val.type->bit_size) {
                                default: assert(false);
                                case 64: new_val = operand_val.integer_literal.u64; break;
                            }
                            interp_store(result_addr, new_val);
                            break;
                        }
                    }

                    break;
                }

                case F_TO_S: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;

                    assert(operand_val.type->kind == AST_Type_Kind::FLOAT);
                    assert(result_type->kind == AST_Type_Kind::INTEGER);

                    if (result_type->integer.sign) {
                        if (operand_val.type == Builtin::type_float) {
                            switch (result_type->bit_size) {
                                default: assert(false);
                                case 8: interp_store(result_addr,(int8_t)operand_val.float_literal.r32); break;
                                case 16: interp_store(result_addr, (int16_t)operand_val.float_literal.r32); break;
                                case 32: interp_store(result_addr, (int32_t)operand_val.float_literal.r32); break;
                                case 64: interp_store(result_addr, (int64_t)operand_val.float_literal.r32); break;
                            }
                        } else {
                            assert(operand_val.type == Builtin::type_double);
                            switch (result_type->bit_size) {
                                default: assert(false);
                                case 8:  interp_store(result_addr, (int8_t)operand_val.float_literal.r64); break;
                                case 16: interp_store(result_addr, (int16_t)operand_val.float_literal.r64); break;
                                case 32: interp_store(result_addr, (int64_t)operand_val.float_literal.r64); break;
                                case 64: interp_store(result_addr, (int64_t)operand_val.float_literal.r64); break;
                            }
                        }
                    } else {
                        if (operand_val.type == Builtin::type_float) {
                            switch (result_type->bit_size) {
                                default: assert(false);
                                case 8: interp_store(result_addr, (uint8_t)operand_val.float_literal.r32); break;
                                case 16: interp_store(result_addr, (uint16_t)operand_val.float_literal.r32); break;
                                case 32: interp_store(result_addr, (uint32_t)operand_val.float_literal.r32); break;
                                case 64: interp_store(result_addr, (uint64_t)operand_val.float_literal.r32); break;
                            }
                        } else {
                            assert(operand_val.type == Builtin::type_double);
                            switch (result_type->bit_size) {
                                default: assert(false);
                                case 8:  interp_store(result_addr, (uint8_t)operand_val.float_literal.r64); break;
                                case 16: interp_store(result_addr, (uint16_t)operand_val.float_literal.r64); break;
                                case 32: interp_store(result_addr, (uint64_t)operand_val.float_literal.r64); break;
                                case 64: interp_store(result_addr, (uint64_t)operand_val.float_literal.r64); break;
                            }
                        }
                    }

                    break;
                }

                case S_TO_F: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;

                    assert(operand_val.type->kind == AST_Type_Kind::INTEGER);
                    assert(operand_val.type->integer.sign);
                    assert(result_type->kind == AST_Type_Kind::FLOAT);

                    if (result_type == Builtin::type_float) {
                        float result_value;
                        switch (operand_val.type->bit_size) {
                            default: assert(false);
                            case 8: result_value = operand_val.integer_literal.s8; break;
                            case 16: result_value = operand_val.integer_literal.s16; break;
                            case 32: result_value = operand_val.integer_literal.s32; break;
                            case 64: result_value = operand_val.integer_literal.s64; break;
                        }
                        interp_store(result_addr, result_value);
                    } else {
                        double result_value;
                        switch (operand_val.type->bit_size) {
                            default: assert(false);
                            case 8: result_value = operand_val.integer_literal.s8; break;
                            case 16: result_value = operand_val.integer_literal.s16; break;
                            case 32: result_value = operand_val.integer_literal.s32; break;
                            case 64: result_value = operand_val.integer_literal.s64; break;
                        }
                        interp_store(result_addr, result_value);
                    }

                    break;
                }

                case U_TO_F: {
                    Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;

                    assert(operand_val.type->kind == AST_Type_Kind::INTEGER);
                    assert(!operand_val.type->integer.sign);
                    assert(result_type->kind == AST_Type_Kind::FLOAT);

                    if (result_type == Builtin::type_float) {
                        float result_value;
                        switch (operand_val.type->bit_size) {
                            default: assert(false);
                            case 8: result_value = operand_val.integer_literal.u8; break;
                            case 16: result_value = operand_val.integer_literal.u16; break;
                            case 32: result_value = operand_val.integer_literal.u32; break;
                            case 64: result_value = operand_val.integer_literal.u64; break;
                        }
                        interp_store(result_addr, result_value);
                    } else {
                        double result_value;
                        switch (operand_val.type->bit_size) {
                            default: assert(false);
                            case 8: result_value = operand_val.integer_literal.u8; break;
                            case 16: result_value = operand_val.integer_literal.u16; break;
                            case 32: result_value = operand_val.integer_literal.u32; break;
                            case 64: result_value = operand_val.integer_literal.u64; break;
                        }
                        interp_store(result_addr, result_value);
                    }

                    break;
                }


                case F_TO_F: {
                     Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                     auto result_addr = interpreter_load_lvalue(interp, inst->result);

                     assert(operand_val.type != inst->result->type);

                     if (operand_val.type == Builtin::type_float) {
                         assert(inst->result->type == Builtin::type_double);
                         interp_store(result_addr, (double)operand_val.float_literal.r32);
                     } else {
                        assert(operand_val.type == Builtin::type_double);
                        assert(inst->result->type = Builtin::type_float);
                        interp_store(result_addr, (float)operand_val.float_literal.r64);
                     }
                     break;
                }

                case PTR_TO_INT: {
                    // Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    // assert(operand_val.type->kind == AST_Type_Kind::POINTER ||
                    //        inst->a->kind == Bytecode_Value_Kind::ALLOCL);
                    assert(inst->result->type->kind == AST_Type_Kind::INTEGER);
                    assert(inst->result->type->bit_size >= Builtin::pointer_size);

                    void *pointer = nullptr;

                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        pointer = interpreter_load_lvalue(interp, inst->a);
                    } else {
                        Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                        pointer = operand_val.pointer;
                    }

                    interp_store(result_addr, (int64_t)pointer);
                    break;
                }

                case PTR_TO_PTR: {
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    void *pointer = nullptr;
                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        pointer = interpreter_load_lvalue(interp, inst->a);
                    } else {
                        Bytecode_Value operand_val = interpreter_load_value(interp, inst->a);
                        pointer = operand_val.pointer;
                    }

                    interp_store(result_addr, pointer);
                    break;
                }

                case SIZEOF: {
                    assert(inst->a->kind == Bytecode_Value_Kind::TYPE);
                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    assert(inst->a->type->bit_size % 8 == 0);
                    int64_t size = inst->a->type->bit_size / 8;

                    interp_store(result_addr, size);
                    break;
                }

                case OFFSETOF: {
                    assert(inst->a->kind == Bytecode_Value_Kind::TYPE);
                    assert(inst->b->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(inst->b->type == Builtin::type_s64);

                    auto result_addr = interpreter_load_lvalue(interp, inst->result);

                    int64_t index = inst->b->integer_literal.s64;
                    int64_t offset = 0;

                    AST_Type *struct_type = inst->a->type;
                    assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                    for (int64_t i = 0; i < index; i++) {
                        auto bit_size = struct_type->structure.member_types[i]->bit_size;
                        assert(bit_size % 8 == 0);
                        offset += (bit_size / 8);
                    }

                    interp_store(result_addr, offset);

                    break;
                }

                case EXIT: {
                    assert(inst->a);

                    auto exit_code_val = interpreter_load_value(interp, inst->a);
                    advance_ip = false;
                    interp->running = false;

                    interp->exit_code = exit_code_val.integer_literal.s64;

                    break;
                }

                case SYSCALL: {
                    Bytecode_Value *arg_count_val = inst->a;
                    assert(arg_count_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(arg_count_val->type == Builtin::type_u64);
                    auto arg_count = arg_count_val->integer_literal.s64;

                    Bytecode_Value *arg_size_val = inst->b;
                    assert(arg_size_val->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(arg_size_val->type == Builtin::type_s64);
                    auto arg_size = arg_size_val->integer_literal.s64;
                    assert(arg_size % 8 == 0); // Should all be 64 bit integers

                    Array<int64_t> args = {};
                    array_init(interp->allocator, &args, arg_count);

                    for (int64_t i = 0; i < arg_count; i++)
                    {
                        auto offset = -((arg_count - i) * sizeof(int64_t));
                        int64_t *param_ptr =
                            (int64_t*)&interp->stack[interp->sp + offset];
                        array_append(&args, *param_ptr);
                    }

                    auto result_addr = interpreter_load_lvalue(interp, inst->result);
                    assert(inst->result->type == Builtin::type_s64);

                    int64_t result = os_syscall(args);
                    interp_store(result_addr, result);

                    array_free(&args);

                    interp->sp -= arg_size;
                    break;
                }
            }

            if (advance_ip) {
                bucket_locator_advance(&interp->ip);
            }

        }

        if (interp->flags & INTERP_FLAG_ABORTED) {
            fprintf(stderr, "Bytecode aborted!\n");
        } else {
            if (ret_val_ptr && ret_type->kind == AST_Type_Kind::INTEGER) {
                interp->exit_code = *(int64_t*)ret_val_ptr;
            }
        }
    }

    void interpreter_initialize_globals(Interpreter *interp, int64_t global_data_size,
                                        Array<Bytecode_Global_Info> global_info)
    {
        assert(global_data_size || global_info.count == 0);

        if (!global_data_size) return;

        interp->global_data = alloc_array<uint8_t>(interp->allocator, global_data_size);

        for (int64_t i = 0; i < global_info.count; i++) {
            auto info = global_info[i];

            auto ptr = interpreter_load_lvalue(interp, info.global_value);

            if (info.has_initializer) {
                interp_store_constant(ptr, info.init_const_val);
            } else {
                assert(info.declaration->type->bit_size % 8 == 0);
                auto size = info.declaration->type->bit_size / 8;
                memset(ptr, 0, size);
            }
        }

    }

    void interpreter_initialize_foreigns(Interpreter *interp,
                                         Array<Bytecode_Function *> foreign_functions)
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

    void interpreter_execute_foreign_function(Interpreter *interp, Bytecode_Function *func,
                                              int64_t arg_count, Bytecode_Value *result_value)
    {
        assert(func->parameters.count == arg_count);
        if (result_value) {
            assert(result_value->kind == Bytecode_Value_Kind::TEMP);
            assert(func->type->function.return_type);
        }

        ffi_reset(&interp->ffi);

        auto old_fp = interp->frame_pointer;
        interp->frame_pointer = interp->sp;

        int64_t total_arg_size = 0;

        for (int64_t i = 0; i < arg_count; i++) {
            auto param = func->parameters[i];
            auto param_type = param->type;
            assert(param_type->kind == AST_Type_Kind::POINTER);
            param_type = param_type->pointer.base;

            assert(param_type->bit_size % 8 == 0);
            auto size = param_type->bit_size / 8;

            total_arg_size += size;
        }

        int64_t param_offset = -total_arg_size;

        for (int64_t i = 0; i < arg_count; i++) {
            auto param = func->parameters[i];
            auto param_type = param->type;
            assert(param_type->kind == AST_Type_Kind::POINTER);
            param_type = param_type->pointer.base;

            auto size = param_type->bit_size / 8;

            param_offset += size;

            param->parameter.byte_offset_from_fp = param_offset - size;

            uint8_t *arg_ptr = interpreter_load_lvalue(interp, param);
            ffi_push_arg(&interp->ffi, arg_ptr, param_type);
        }

        interp->frame_pointer = old_fp;
        interp->sp -= total_arg_size;

        AST_Type *return_type = nullptr;
        uint8_t * return_val_ptr = nullptr;

        if (result_value) {
            return_val_ptr = interpreter_load_lvalue(interp, result_value);
            return_type = result_value->type;
        }


        ffi_call(&interp->ffi, func->name, return_val_ptr, return_type);
    }

    void interpreter_execute_compiler_function(Interpreter *interp, Bytecode_Function *func,
                                               int64_t arg_count, Bytecode_Value *result_value)
    {
        assert(func->flags & BC_FUNC_FLAG_COMPILER_FUNC);

        assert(arg_count == 0);

        if (func->name == Builtin::atom_abort) {
            interp->flags |= INTERP_FLAG_ABORTED;
            interp->running = false;
            interp->exit_code = 134;
        } else {
            assert(false && !"Unimplemented compiler function!");
        }
    }

    Bytecode_Value interpreter_load_value(Interpreter *interp, Bytecode_Value *value)
    {

        uint8_t *source_ptr = interpreter_load_lvalue(interp, value);

        Bytecode_Value result = {};
        result.kind = Bytecode_Value_Kind::TEMP;

        if (value->kind == Bytecode_Value_Kind::ALLOCL ||
            value->kind == Bytecode_Value_Kind::PARAM  ||
            value->kind == Bytecode_Value_Kind::GLOBAL) {
            assert(value->type->kind == AST_Type_Kind::POINTER);
            result.type = value->type->pointer.base;
        } else {
            result.type = value->type;
        }

        if (value->kind == Bytecode_Value_Kind::INTEGER_LITERAL)
            result.kind = Bytecode_Value_Kind::INTEGER_LITERAL;
        else if (value->kind == Bytecode_Value_Kind::NULL_LITERAL) {
            result.kind = Bytecode_Value_Kind::NULL_LITERAL;
            result.pointer = nullptr;
            return result;
        }


        switch (result.type->kind) {

            case AST_Type_Kind::BOOL:
            case AST_Type_Kind::INTEGER:
            case AST_Type_Kind::ENUM: {
                switch (result.type->bit_size) {
                    case 8: result.integer_literal.s8 = *((int8_t*)source_ptr); break;
                    case 16: result.integer_literal.s16 = *((int16_t*)source_ptr); break;
                    case 32: result.integer_literal.s32 = *((int32_t*)source_ptr); break;
                    case 64: result.integer_literal.s64 = *((int64_t*)source_ptr); break;
                    default: assert(false);
                }
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (result.type == Builtin::type_float)       result.float_literal.r32 = *((float *)source_ptr);
                else if (result.type == Builtin::type_double) result.float_literal.r64 = *((double *)source_ptr);
                else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                result.pointer = *(void**)source_ptr;
                break;
            }

            case AST_Type_Kind::ARRAY: {
                if (value->kind == Bytecode_Value_Kind::ALLOCL ||
                    value->kind == Bytecode_Value_Kind::GLOBAL) {
                    result.pointer = source_ptr;
                } else if (value->kind == Bytecode_Value_Kind::TEMP) {
                    result.pointer = *(void**)source_ptr;
                } else {
                    assert(false);
                }
                assert(result.type->array.element_type->pointer_to);
                result.type = result.type->array.element_type->pointer_to;
                break;
            }

            case AST_Type_Kind::STRUCTURE: {
                if (value->kind == Bytecode_Value_Kind::ALLOCL ||
                    value->kind == Bytecode_Value_Kind::PARAM) {
                    result.pointer = source_ptr;
                } else if (value->kind == Bytecode_Value_Kind::TEMP) {
                    result.pointer = *(void**)source_ptr;
                } else {
                    assert(false);
                }
                break;
            }
            default: assert(false);
        }

        return result;
    }

    uint8_t *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value)
    {
        switch (value->kind) {

            case Bytecode_Value_Kind::TEMP: {
                return &interp->stack[interp->frame_pointer + value->temp.byte_offset_from_fp];
                break;
            }

            case Bytecode_Value_Kind::GLOBAL: {
                return &interp->global_data[value->global.byte_offset];
                break;
            }

            case Bytecode_Value_Kind::ALLOCL: {
                return &interp->stack[interp->frame_pointer + value->allocl.byte_offset_from_fp];
                break;
            }

            case Bytecode_Value_Kind::INTEGER_LITERAL: {
                return (uint8_t*)&value->integer_literal;
                break;
            }

            case Bytecode_Value_Kind::FLOAT_LITERAL: {
                if (value->type == Builtin::type_float) {
                    return (uint8_t*)&value->float_literal.r32;
                } else if (value->type == Builtin::type_double) {
                    return (uint8_t*)&value->float_literal.r64;
                } else {
                    assert(false);
                }
                break;
            }

            case Bytecode_Value_Kind::STRING_LITERAL: {
                return (uint8_t*)&value->string_literal.data;
                break;
            }

            case Bytecode_Value_Kind::BOOL_LITERAL: {
                return (uint8_t*)&value->bool_literal;
                break;
            }

            case Bytecode_Value_Kind::NULL_LITERAL: {
                return nullptr;
                break;
            }

            case Bytecode_Value_Kind::PARAM: {
                assert(value->parameter.byte_offset_from_fp < 0);
                auto result = &interp->stack[interp->frame_pointer +
                                      value->parameter.byte_offset_from_fp];
                return result;
                break;
            }

            case Bytecode_Value_Kind::INVALID:
            case Bytecode_Value_Kind::BLOCK:
            case Bytecode_Value_Kind::TYPE:
            case Bytecode_Value_Kind::SWITCH_DATA:
            case Bytecode_Value_Kind::FUNCTION: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void interpreter_free(Interpreter *interp)
    {
        free(interp->allocator, interp->stack);
    }

    void interp_store_value(uint8_t *dest, Bytecode_Value val)
    {
        auto type = val.type;

        if (val.kind == Bytecode_Value_Kind::ALLOCL ||
            val.kind == Bytecode_Value_Kind::PARAM)
        {
            assert(type->kind == AST_Type_Kind::POINTER);
            type = type->pointer.base;
        }

        switch (type->kind) {

            case AST_Type_Kind::INTEGER:
            case AST_Type_Kind::ENUM: {
                switch (val.type->bit_size)
                {
                    case 8: interp_store(dest, val.integer_literal.s8); break;
                    case 16: interp_store(dest, val.integer_literal.s16); break;
                    case 32: interp_store(dest, val.integer_literal.s32); break;
                    case 64: interp_store(dest, val.integer_literal.s64); break;
                    default: assert(false);
                }
                break;
            }

            case AST_Type_Kind::BOOL: {
                interp_store(dest, val.bool_literal);
                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (val.type == Builtin::type_float)
                    interp_store(dest, val.float_literal.r32);
                else if (val.type == Builtin::type_double)
                    interp_store(dest, val.float_literal.r64);
                else {
                    assert(false);
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                interp_store(dest, val.pointer);
                break;
            }

            case AST_Type_Kind::STRUCTURE: {
                interp_store(dest, val.pointer);
                break;
            }

            case AST_Type_Kind::ARRAY: assert(false);

            default: assert(false);
        }
    }

    void interp_store_constant(uint8_t *dest, Const_Value val)
    {
#ifndef NDEBUG
        auto type = val.type;
        assert(type->kind == AST_Type_Kind::INTEGER);
#endif

        switch (val.type->bit_size)
        {
            case 8: interp_store(dest, val.integer.s8); break;
            case 16: interp_store(dest, val.integer.s16); break;
            case 32: interp_store(dest, val.integer.s32); break;
            case 64: interp_store(dest, val.integer.s64); break;
            default: assert(false);
        }
    }
}
