
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
                    assert(inst->a->type->kind == AST_Type_Kind::POINTER);
                    assert(inst->a->type->pointer.base == inst->b->type);

                    void *dest_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *_source_ptr = interpreter_load_lvalue(interp, inst->b);
                    void *source_ptr = _source_ptr;

                    if (inst->b->kind == Bytecode_Value_Kind::ALLOCL) {
                        source_ptr = &_source_ptr;
                    }

                    assert(dest_ptr);
                    assert(source_ptr);
                    interp_store(inst->b->type, dest_ptr, source_ptr);
                    break;

                }

                case STORE_PTR: {
                    void *_dest_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *source_ptr = interpreter_load_lvalue(interp, inst->b);

                    void *dest_ptr = *(void**)_dest_ptr;

                    assert(source_ptr);
                    assert(dest_ptr);

                    interp_store(inst->b->type, dest_ptr, source_ptr);
                    break;
                }

                case LOADL:
                case LOAD_PARAM:
                case LOAD_GLOBAL: {
                    void *source_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    assert(dest_ptr);
                    assert(source_ptr);

                    interp_store(inst->result->type, dest_ptr, source_ptr);
                    break;
                }

                case LOAD_PTR: {
                    void *_source_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    assert(inst->a->type->kind == AST_Type_Kind::POINTER);
                    assert(inst->a->type->pointer.base == inst->result->type);

                    void *source_ptr = *(void**)_source_ptr;

                    assert(dest_ptr);
                    assert(source_ptr);

                    interp_store(inst->result->type, dest_ptr, source_ptr);
                    break;
                }

#define DO_TYPED_BINOP(type, result_type, _op) \
    { *(result_type*)result_ptr = (*(type*)lhs_ptr) _op (*(type*)rhs_ptr); break; }\

#define DO_SAME_TYPED_BINOP(type, _op) DO_TYPED_BINOP(type, type, _op)

#define DO_CMP_BINOP(type, _op) DO_TYPED_BINOP(type, bool, _op)

#define DO_BINOP_ARITHMETIC_INT(op, signed) { \
    assert(inst->a->type == inst->b->type); \
    void *lhs_ptr = interpreter_load_lvalue(interp, inst->a); \
    void *rhs_ptr = interpreter_load_lvalue(interp, inst->b); \
    void *result_ptr = interpreter_load_lvalue(interp, inst->result); \
    assert(lhs_ptr); \
    assert(rhs_ptr); \
    assert(result_ptr); \
    if (signed) {  \
        switch (inst->a->type->bit_size) { \
            case 8:  DO_SAME_TYPED_BINOP(int8_t, op); \
            case 16: DO_SAME_TYPED_BINOP(int16_t, op); \
            case 32: DO_SAME_TYPED_BINOP(int32_t, op); \
            case 64: DO_SAME_TYPED_BINOP(int64_t, op); \
        } \
    } else { \
        switch (inst->a->type->bit_size) { \
            case 8:  DO_SAME_TYPED_BINOP(uint8_t, op); \
            case 16: DO_SAME_TYPED_BINOP(uint16_t, op); \
            case 32: DO_SAME_TYPED_BINOP(uint32_t, op); \
            case 64: DO_SAME_TYPED_BINOP(uint64_t, op); \
        } \
    } \
    break; \
}

                case ADD_S: DO_BINOP_ARITHMETIC_INT(+, true);
                case SUB_S: DO_BINOP_ARITHMETIC_INT(-, true);
                case REM_S: DO_BINOP_ARITHMETIC_INT(%, true);
                case MUL_S: DO_BINOP_ARITHMETIC_INT(*, true);
                case DIV_S: DO_BINOP_ARITHMETIC_INT(/, true);

                case ADD_U: DO_BINOP_ARITHMETIC_INT(+, false);
                case SUB_U: DO_BINOP_ARITHMETIC_INT(-, false);
                case REM_U: DO_BINOP_ARITHMETIC_INT(%, false);
                case MUL_U: DO_BINOP_ARITHMETIC_INT(*, false);
                case DIV_U: DO_BINOP_ARITHMETIC_INT(/, false);

#undef DO_BINOP_ARITHMETIC_INT

#define DO_BINOP_ARITHMETIC_FLOAT(op, signed) { \
    assert(inst->a->type == inst->b->type); \
    void *lhs_ptr = interpreter_load_lvalue(interp, inst->a); \
    void *rhs_ptr = interpreter_load_lvalue(interp, inst->b); \
    void *result_ptr = interpreter_load_lvalue(interp, inst->result); \
    assert(lhs_ptr); \
    assert(rhs_ptr); \
    assert(result_ptr); \
    if (inst->a->type == Builtin::type_float) { \
        *(float*)result_ptr = *(float*)lhs_ptr op *(float*)rhs_ptr; \
    } else if (inst->a->type == Builtin::type_double) { \
        *(double*)result_ptr = *(double*)lhs_ptr op *(double*)rhs_ptr; \
    } else { \
        assert(false); \
    } \
    break; \
}
                case ADD_F: DO_BINOP_ARITHMETIC_FLOAT(+, false);
                case SUB_F: DO_BINOP_ARITHMETIC_FLOAT(-, false);
                case MUL_F: DO_BINOP_ARITHMETIC_FLOAT(*, false);
                case DIV_F: DO_BINOP_ARITHMETIC_FLOAT(/, false);

#undef DO_BINOP_ARITHMETIC_FLOAT

#define DO_BINOP_CMP_INT(_op) { \
    assert(inst->a->type == inst->b->type); \
    void *lhs_ptr = interpreter_load_lvalue(interp, inst->a); \
    void *rhs_ptr = interpreter_load_lvalue(interp, inst->b); \
    void *result_ptr = interpreter_load_lvalue(interp, inst->result); \
    auto type = inst->a->type; \
    if (type->kind == AST_Type_Kind::ENUM) type = type->enum_type.base_type; \
    if (type->kind == AST_Type_Kind::INTEGER) { \
        if (type->integer.sign) { \
            switch (type->bit_size) { \
                case 8: DO_CMP_BINOP(uint8_t, _op) \
                case 16: DO_CMP_BINOP(uint16_t, _op) \
                case 32: DO_CMP_BINOP(uint32_t, _op) \
                case 64: DO_CMP_BINOP(uint64_t, _op) \
                default: assert(false); \
            } \
        } else { \
            switch (type->bit_size) { \
                case 8: DO_CMP_BINOP(int8_t, _op) \
                case 16: DO_CMP_BINOP(int16_t, _op) \
                case 32: DO_CMP_BINOP(int32_t, _op) \
                case 64: DO_CMP_BINOP(int64_t, _op) \
                default: assert(false); \
            } \
        } \
    } else if (type->kind == AST_Type_Kind::POINTER) { \
        assert(inst->op == EQ_U || inst->op == NEQ_U); \
        DO_CMP_BINOP(void*, _op) \
    } else { \
        assert(false && "Unsupported binop compare"); \
    } \
    break; \
}

                case EQ_S:   DO_BINOP_CMP_INT(==);
                case NEQ_S:  DO_BINOP_CMP_INT(!=);
                case LT_S:   DO_BINOP_CMP_INT(<);
                case LTEQ_S: DO_BINOP_CMP_INT(<=);
                case GT_S:   DO_BINOP_CMP_INT(>);
                case GTEQ_S: DO_BINOP_CMP_INT(>=);

                case EQ_U:   DO_BINOP_CMP_INT(==);
                case NEQ_U:  DO_BINOP_CMP_INT(!=);
                case LT_U:   DO_BINOP_CMP_INT(<);
                case LTEQ_U: DO_BINOP_CMP_INT(<=);
                case GT_U:   DO_BINOP_CMP_INT(>);
                case GTEQ_U: DO_BINOP_CMP_INT(>=);

#undef DO_BINOP_CMP_INT

#define DO_BINOP_CMP_FLOAT(_op) { \
    assert(inst->a->type == inst->b->type); \
    void *lhs_ptr = interpreter_load_lvalue(interp, inst->a); \
    void *rhs_ptr = interpreter_load_lvalue(interp, inst->b); \
    void *result_ptr = interpreter_load_lvalue(interp, inst->result); \
    assert(lhs_ptr); \
    assert(rhs_ptr); \
    assert(result_ptr); \
    if (inst->a->type == Builtin::type_float) { \
        *(bool*)result_ptr = *(float*)lhs_ptr _op *(float*)rhs_ptr; \
    } else if (inst->a->type == Builtin::type_double){ \
        *(bool*)result_ptr = *(double*)lhs_ptr _op *(double*)rhs_ptr; \
    } else { \
        assert(false); \
    } \
    break;\
}

                case EQ_F:   DO_BINOP_CMP_FLOAT(==);
                case NEQ_F:  DO_BINOP_CMP_FLOAT(!=);
                case LT_F:   DO_BINOP_CMP_FLOAT(<);
                case LTEQ_F: DO_BINOP_CMP_FLOAT(<=);
                case GT_F:   DO_BINOP_CMP_FLOAT(>);
                case GTEQ_F: DO_BINOP_CMP_FLOAT(>=);

#undef DO_BINOP_CMP_FLOAT
#undef DO_TYPED_BINOP
#undef DO_SAME_TYPED_BINOP
#undef DO_CMP_BINOP

                case NEG_LOG: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    bool result_value = false;

                    if (inst->a->type->kind == AST_Type_Kind::BOOL ||
                        inst->a->type->kind == AST_Type_Kind::INTEGER) {
                        // @TODO: @Cleanup: I think we should check all sizes and the sign here?
                        result_value = *(int64_t*)op_ptr == 0;
                    } else if (inst->a->type->kind == AST_Type_Kind::POINTER) {
                        result_value = *(void**)op_ptr == nullptr;
                    }

                    assert(inst->result->type->bit_size == 8);
                    interp_store(inst->result->type, dest_ptr, &result_value);
                    break;
                }

                case PUSH_ARG: {
                    void *_arg_ptr = interpreter_load_lvalue(interp, inst->a);

                    void *arg_ptr;

                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        assert(inst->a->type->kind == AST_Type_Kind::POINTER);

                        arg_ptr = &_arg_ptr;

                    } else {
                        arg_ptr = _arg_ptr;
                    }

                    assert(inst->a->type->bit_size % 8 == 0);
                    int64_t size = inst->a->type->bit_size / 8;
                    assert(interp->sp + size <= interp->stack_size);

                    void *dest_ptr = &interp->stack[interp->sp];
                    interp->sp += size;

                    interp_store(inst->a->type, dest_ptr, arg_ptr);
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

                    void *ret_val_ptr = nullptr;
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
                    void *ret_val_source_ptr = interpreter_load_lvalue(interp, inst->a);

                    int64_t offset = 0;
                    int64_t old_fp = *(int64_t*)(&interp->stack[interp->frame_pointer]);

                    if (old_fp != -1) {
                        offset += sizeof(old_fp);
                        interp->ip =
                            *(Instruction_Pointer*)(&interp->stack[interp->frame_pointer +
                                                                   offset]);

                        offset += sizeof(Instruction_Pointer);
                        uint8_t *ret_val_dest_ptr =
                            *(uint8_t**)(&interp->stack[interp->frame_pointer + offset]);

                        interp_store(inst->a->type, ret_val_dest_ptr, ret_val_source_ptr);

                        interp->sp = interp->frame_pointer;
                        interp->frame_pointer = old_fp;

                        int64_t total_arg_size = interp_stack_pop<int64_t>(interp);
                        interp->sp -= total_arg_size;
                    } else {
                        interp->running = false;
                        advance_ip = false;

                        // This ret_val_ptr is created at the top of interpreter_start()
                        interp_store(inst->a->type, ret_val_ptr, ret_val_source_ptr);
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
                    void *cond_val_ptr = interpreter_load_lvalue(interp, inst->a);
                    assert(inst->a->type->kind == AST_Type_Kind::BOOL);

                    assert(inst->b->kind == Bytecode_Value_Kind::BLOCK);
                    assert(inst->result->kind == Bytecode_Value_Kind::BLOCK);

                    Bytecode_Block *then_block = inst->b->block;
                    Bytecode_Block *else_block = inst->result->block;
                    Bytecode_Block *target_block = nullptr;

                    assert(inst->a->type->bit_size == 8);

                    if (*(uint8_t*)cond_val_ptr) target_block = then_block;
                    else target_block = else_block;
                    assert(target_block);

                    interp->ip = target_block->first_instruction;
                    break;
                }

                case SWITCH: {
                    advance_ip = false;
                    void *switch_val_ptr = interpreter_load_lvalue(interp, inst->a);

                    Bytecode_Block *default_block = nullptr;
                    Bytecode_Block *target_block = nullptr;

                    assert(inst->b->kind == Bytecode_Value_Kind::SWITCH_DATA);
                    auto switch_data = inst->b->switch_data;

                    for (int64_t i = 0; i < switch_data.cases.count; i++) {
                        auto case_info = switch_data.cases[i];

                        if (case_info.case_value) {
                            assert(inst->a->type == case_info.case_value->type);
                            auto type = inst->a->type;

                            if (type->kind == AST_Type_Kind::ENUM)
                                type = type->enum_type.base_type;

                            assert(type->kind == AST_Type_Kind::INTEGER);
                            assert(type->bit_size == 64);

                            bool match;
                            if (type->integer.sign) {
                                match = *(int64_t*)switch_val_ptr ==
                                        case_info.case_value->integer_literal.s64;
                            } else {
                                match = *(uint64_t*)switch_val_ptr  ==
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
                    void *_ptr_val_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *ptr_val;
                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL ||
                        inst->a->kind == Bytecode_Value_Kind::GLOBAL) {
                        ptr_val = _ptr_val_ptr;
                    } else {
                        ptr_val = *(void**)_ptr_val_ptr;
                    }

                    void *offset_ptr = interpreter_load_lvalue(interp, inst->b);
                    void *result_ptr = interpreter_load_lvalue(interp, inst->result);

                    assert(inst->a->type->kind == AST_Type_Kind::POINTER);
                    assert(inst->b->type == Builtin::type_s64);
                    assert(inst->result->type->kind == AST_Type_Kind::POINTER);

                    AST_Type *element_type = nullptr;
                    if (inst->a->type->pointer.base->kind == AST_Type_Kind::ARRAY) {
                        element_type = inst->a->type->pointer.base->array.element_type;
                    } else {
                        element_type = inst->a->type->pointer.base;
                    }
                    assert(element_type);

                    assert(element_type->bit_size % 8 == 0);
                    auto byte_size = element_type->bit_size / 8;

                    void *result = ((uint8_t*)ptr_val) +
                                   ((*(int64_t*)offset_ptr) * byte_size);
                    interp_store(inst->result->type, result_ptr, &result);
                    break;
                }

                case AGG_OFFSET: {
                    void *ptr_val_ptr = interpreter_load_lvalue(interp, inst->a);

                    void *ptr_val;
                    if (inst->a->kind == Bytecode_Value_Kind::GLOBAL) assert(false);
                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL ||
                        inst->a->kind == Bytecode_Value_Kind::PARAM) {
                        ptr_val = ptr_val_ptr;
                    } else {
                        ptr_val = *(void**)ptr_val_ptr;
                    }
                    assert(ptr_val);

                    void *index_ptr = interpreter_load_lvalue(interp, inst->b);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    assert(inst->b->type == Builtin::type_u32);
                    assert(inst->result->type->kind == AST_Type_Kind::POINTER);

                    AST_Type *struct_type = nullptr;
                    if (inst->a->type->kind == AST_Type_Kind::STRUCTURE) {
                        struct_type = inst->a->type;
                    } else {
                        assert(inst->a->type->kind == AST_Type_Kind::POINTER);
                        assert(inst->a->type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                        struct_type = inst->a->type->pointer.base;
                    }
                    assert(struct_type);

                    uint32_t index_val = *(uint32_t*)index_ptr;

                    int64_t byte_offset = 0;
                    for (int64_t i = 0; i < index_val; i++) {
                        auto mem_type = struct_type->structure.member_types[i];
                        auto bit_size = mem_type->bit_size;
                        assert(bit_size % 8 == 0);
                        byte_offset += (bit_size / 8);
                    }

                    void *result = ((uint8_t*)ptr_val) + byte_offset;
                    interp_store(inst->result->type, dest_ptr, &result);

                    break;
                }

                case ZEXT: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size > inst->a->type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: assert(false);

                        case 64: {
                            uint64_t new_val = 0xdddd;
                            switch (inst->a->type->bit_size) {
                                case 8: new_val = (*(uint8_t*)op_ptr); break;
                                case 16: new_val = (*(uint16_t*)op_ptr); break;
                                case 32: new_val = (*(uint32_t*)op_ptr); break;
                                case 64: new_val = (*(uint64_t*)op_ptr); break;
                                default: assert(false);
                            }
                            interp_store(inst->result->type, dest_ptr, &new_val);
                            break;
                        }
                    }

                    break;
                }

                case SEXT: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size > inst->a->type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: assert(false);
                        case 64: {
                            int64_t new_val;
                            switch (inst->a->type->bit_size) {
                                default: assert(false);
                                case 8: new_val = *(int8_t*)op_ptr; break;
                                case 16: new_val = *(int16_t*)op_ptr; break;
                                case 32: new_val = *(int32_t*)op_ptr; break;
                                case 64: new_val = *(int64_t*)op_ptr; break;
                            }
                            interp_store(inst->result->type, dest_ptr, &new_val);
                            break;
                        }
                    }
                    break;
                }

                case TRUNC: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto result_type = inst->result->type;
                    assert(result_type->bit_size <= inst->a->type->bit_size);

                    switch (result_type->bit_size) {
                        default: assert(false);
                        case 8: {
                            uint8_t new_val;
                            switch (inst->a->type->bit_size) {
                                default: assert(false);
                                case 64: new_val = *(uint64_t*)op_ptr; break;
                            }
                            interp_store(inst->result->type, dest_ptr, &new_val);
                            break;
                        }

                        case 16: assert(false);

                        case 32: {
                            uint32_t new_val;
                            switch (inst->a->type->bit_size) {
                                default: assert(false);
                                case 64: new_val = *(uint64_t*)op_ptr; break;
                            }
                            interp_store(inst->result->type, dest_ptr, &new_val);
                            break;
                        }

                        case 64: {
                            uint64_t new_val;
                            switch (inst->a->type->bit_size) {
                                default: assert(false);
                                case 64: new_val = *(uint64_t*)op_ptr; break;
                            }
                            interp_store(inst->result->type, dest_ptr, &new_val);
                            break;
                        }
                    }

                    break;
                }

                case F_TO_S: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto op_type = inst->a->type;
                    auto result_type = inst->result->type;

                    assert(op_type->kind == AST_Type_Kind::FLOAT);
                    assert(result_type->kind == AST_Type_Kind::INTEGER);

#define FLOAT_TO_INT_CASE(size) case size:{ \
    int##size##_t i = (int##size##_t)v; \
    interp_store(result_type, dest_ptr, &i); \
}

                    assert(result_type->integer.sign);

                    if (op_type == Builtin::type_float) {
                        float v = *(float*)op_ptr;
                        switch (result_type->bit_size) {
                            default: assert(false);
                            FLOAT_TO_INT_CASE(8);
                            FLOAT_TO_INT_CASE(16);
                            FLOAT_TO_INT_CASE(32);
                            FLOAT_TO_INT_CASE(64);
                        }
                    } else {
                        assert(op_type == Builtin::type_double);
                        double v = *(double*)op_ptr;
                        switch (result_type->bit_size) {
                            default: assert(false);
                            FLOAT_TO_INT_CASE(8);
                            FLOAT_TO_INT_CASE(16);
                            FLOAT_TO_INT_CASE(32);
                            FLOAT_TO_INT_CASE(64);
                        }
                    }

#undef FLOAT_TO_INT_CASE

                    break;
                }

                case S_TO_F: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto op_type = inst->a->type;
                    auto result_type = inst->result->type;

                    assert(op_type->kind == AST_Type_Kind::INTEGER);
                    assert(op_type->integer.sign);
                    assert(result_type->kind == AST_Type_Kind::FLOAT);

#define SIGNED_TO_FLOAT_CASE(size) case size: { \
    result_value = *(int##size##_t*)op_ptr; \
    break; \
}

                    if (result_type == Builtin::type_float) {
                        float result_value;
                        switch (op_type->bit_size) {
                            default: assert(false);
                            SIGNED_TO_FLOAT_CASE(8);
                            SIGNED_TO_FLOAT_CASE(16);
                            SIGNED_TO_FLOAT_CASE(32);
                            SIGNED_TO_FLOAT_CASE(64);
                        }
                        interp_store(result_type, dest_ptr, &result_value);
                    } else {
                        double result_value;
                        switch (op_type->bit_size) {
                            default: assert(false);
                            SIGNED_TO_FLOAT_CASE(8);
                            SIGNED_TO_FLOAT_CASE(16);
                            SIGNED_TO_FLOAT_CASE(32);
                            SIGNED_TO_FLOAT_CASE(64);
                        }
                        interp_store(result_type, dest_ptr, &result_value);
                    }

#undef SIGNED_TO_FLOAT_CASE

                    break;
                }

                case U_TO_F: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto op_type = inst->a->type;
                    auto result_type = inst->result->type;

                    assert(op_type->kind == AST_Type_Kind::INTEGER);
                    assert(!op_type->integer.sign);
                    assert(result_type->kind == AST_Type_Kind::FLOAT);

#define UNSIGNED_TO_FLOAT_CASE(size) case size: { \
    result_value = *(uint##size##_t*)op_ptr; \
    break; \
}

                    if (result_type == Builtin::type_float) {
                        float result_value;
                        switch (op_type->bit_size) {
                            default: assert(false);
                            UNSIGNED_TO_FLOAT_CASE(8);
                            UNSIGNED_TO_FLOAT_CASE(16);
                            UNSIGNED_TO_FLOAT_CASE(32);
                            UNSIGNED_TO_FLOAT_CASE(64);
                        }
                        interp_store(result_type, dest_ptr, &result_value);
                    } else {
                        double result_value;
                        switch (op_type->bit_size) {
                            default: assert(false);
                            UNSIGNED_TO_FLOAT_CASE(8);
                            UNSIGNED_TO_FLOAT_CASE(16);
                            UNSIGNED_TO_FLOAT_CASE(32);
                            UNSIGNED_TO_FLOAT_CASE(64);
                        }
                        interp_store(result_type, dest_ptr, &result_value);
                    }

#undef UNSIGNED_TO_FLOAT_CASE

                    break;
                }


                case F_TO_F: {
                    void *op_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto dest_type = inst->result->type;
#ifndef NDEBUG
                    auto op_type = inst->a->type;

                    assert(op_type->kind == AST_Type_Kind::FLOAT);
                    assert(dest_type->kind == AST_Type_Kind::FLOAT);
                    assert(op_type != dest_type);
#endif

                    if (dest_type == Builtin::type_float) {
                        float v_val = (float)*(double*)op_ptr;
                        interp_store(dest_type, dest_ptr, &v_val);
                    } else {
                        assert(dest_type == Builtin::type_double);
                        double d_val = (double)*(float*)op_ptr;
                        interp_store(dest_type, dest_ptr, &d_val);
                    }
                    break;
                }

                case PTR_TO_INT: {
                    void *_source_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *source_ptr;

                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        source_ptr = _source_ptr;
                    } else {
                        source_ptr = *(void**)_source_ptr;
                    }

                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto dest_type = inst->result->type;

                    assert(dest_type->kind == AST_Type_Kind::INTEGER);
                    assert(dest_type->bit_size >= Builtin::pointer_size);

                    interp_store(dest_type, dest_ptr, &source_ptr);
                    break;
                }

                case PTR_TO_PTR: {
                    void *_source_ptr = interpreter_load_lvalue(interp, inst->a);
                    void *source_ptr;

                    if (inst->a->kind == Bytecode_Value_Kind::ALLOCL) {
                        source_ptr = _source_ptr;
                    } else {
                        source_ptr = *(void**)_source_ptr;
                    }
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto dest_type = inst->result->type;

                    assert(dest_type->kind == AST_Type_Kind::POINTER);

                    interp_store(dest_type, dest_ptr, &source_ptr);
                    break;
                }

                case SIZEOF: {
                    assert(inst->a->kind == Bytecode_Value_Kind::TYPE);
                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto dest_type = inst->result->type;
                    assert(dest_type == Builtin::type_s64);

                    assert(inst->a->type->bit_size % 8 == 0);
                    int64_t size = inst->a->type->bit_size / 8;

                    interp_store(dest_type, dest_ptr, &size);
                    break;
                }

                case OFFSETOF: {
                    assert(inst->a->kind == Bytecode_Value_Kind::TYPE);
                    assert(inst->b->kind == Bytecode_Value_Kind::INTEGER_LITERAL);
                    assert(inst->b->type == Builtin::type_s64);

                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);

                    auto dest_type = inst->result->type;
                    assert(dest_type == Builtin::type_s64);

                    int64_t index = inst->b->integer_literal.s64;
                    int64_t offset = 0;

                    AST_Type *struct_type = inst->a->type;
                    assert(struct_type->kind == AST_Type_Kind::STRUCTURE);

                    for (int64_t i = 0; i < index; i++) {
                        auto bit_size = struct_type->structure.member_types[i]->bit_size;
                        assert(bit_size % 8 == 0);
                        offset += (bit_size / 8);
                    }

                    interp_store(dest_type, dest_ptr, &offset);
                    break;
                }

                case EXIT: {
                    void *exit_code_ptr = interpreter_load_lvalue(interp, inst->a);

                    assert(inst->a->type == Builtin::type_s64);

                    advance_ip = false;
                    interp->running = false;

                    interp->exit_code = *(int64_t*)exit_code_ptr;
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

                    void *dest_ptr = interpreter_load_lvalue(interp, inst->result);
                    auto dest_type = inst->result->type;
                    assert(dest_type == Builtin::type_s64);

                    int64_t result = os_syscall(args);
                    interp_store(dest_type, dest_ptr, &result);

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

            void *ptr = interpreter_load_lvalue(interp, info.global_value);

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

            void *arg_ptr = interpreter_load_lvalue(interp, param);
            ffi_push_arg(&interp->ffi, arg_ptr, param_type);
        }

        interp->frame_pointer = old_fp;
        interp->sp -= total_arg_size;

        AST_Type *return_type = nullptr;
        void *return_val_ptr = nullptr;

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

    void *interpreter_load_lvalue(Interpreter *interp, Bytecode_Value *value)
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
                return (void*)&interp->null_pointer;
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

    void interp_store(AST_Type *type, void *dest_ptr, void *source_ptr)
    {
        assert(dest_ptr);
        assert(source_ptr);

        switch (type->kind) {
            default: assert(false);

            case AST_Type_Kind::INTEGER:
            case AST_Type_Kind::FLOAT:
            case AST_Type_Kind::ENUM:
            case AST_Type_Kind::BOOL: {
                switch (type->bit_size) {
                    default: assert(false);
                    case 8: *(uint8_t*)dest_ptr = *(uint8_t*)source_ptr; break;
                    case 16: *(uint16_t*)dest_ptr = *(uint16_t*)source_ptr; break;
                    case 32: *(uint32_t*)dest_ptr = *(uint32_t*)source_ptr; break;
                    case 64: *(uint64_t*)dest_ptr = *(uint64_t*)source_ptr; break;
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                *(void**)dest_ptr = *(void**)source_ptr;
                break;
            }

            case AST_Type_Kind::STRUCTURE:
            case AST_Type_Kind::UNION:
            case AST_Type_Kind::ARRAY: {
                assert(type->bit_size % 8 == 0);
                auto byte_size = type->bit_size / 8;
                memcpy(dest_ptr, source_ptr, byte_size);
                break;
            }
        }
    }

    void interp_store_constant(void *dest, Const_Value val)
    {
#ifndef NDEBUG
        auto type = val.type;
        assert(type->kind == AST_Type_Kind::INTEGER);
#endif

        switch (val.type->bit_size) {
            case 8:  interp_store(val.type, dest, &val.integer.s8); break;
            case 16: interp_store(val.type, dest, &val.integer.s16); break;
            case 32: interp_store(val.type, dest, &val.integer.s32); break;
            case 64: interp_store(val.type, dest, &val.integer.s64); break;
            default: assert(false);
        }
    }
}
