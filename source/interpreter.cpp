
#include "interpreter.h"
#include "builtin.h"
#include "os.h"

#include "common.h"

namespace Zodiac
{
    void interpreter_init(Allocator *allocator, Interpreter *interp, Build_Data *build_data)
    {
        assert(allocator);
        assert(interp);

        interp->allocator = allocator;
        interp->program = nullptr;
        interp->build_data = build_data;

        interp->running = false;
        interp->exited = false;

        array_init(allocator, &interp->globals);

        stack_init(allocator, &interp->stack_frames);
        stack_init(allocator, &interp->temp_stack);
        stack_init(allocator, &interp->allocl_stack);
        stack_init(allocator, &interp->arg_stack);
    }

    void interpreter_free(Interpreter *interp)
    {
        assert(interp);

        stack_free(&interp->stack_frames);
        stack_free(&interp->temp_stack);
        stack_free(&interp->allocl_stack);
        stack_free(&interp->arg_stack);
    }

    void interpreter_execute_entry(Interpreter *interp, Bytecode_Program *program)
    {
        assert(interp);
        assert(program);
        assert(program->bytecode_entry_function);

        assert(interp->program == nullptr);
        assert(interp->running == false);
        interp->running = true;
        interp->program = program;

        for (int64_t i = 0; i < program->globals.count; i++)
        {
            auto &glob = program->globals[i];

#ifndef NDEBUG
            auto type = glob.value->type;
            assert(type->kind == AST_Type_Kind::INTEGER ||
                   type->kind == AST_Type_Kind::FLOAT ||
                   type->kind == AST_Type_Kind::BOOL);
#endif

            Bytecode_Value new_glob = *glob.value;
            array_append(&interp->globals, new_glob);
        }

        interpreter_execute_function(interp, program->bytecode_entry_function, 0);

        if (!interp->exited)
        {
            assert(stack_count(&interp->stack_frames));
            auto frame = stack_pop(&interp->stack_frames);
            assert(frame.returned);
            interp->exit_code_value = frame.return_value;
        }

        for (int64_t i = 0; i < interp->globals.count; i++)
        {
            auto &glob = interp->globals[i];
            if (glob.type->kind == AST_Type_Kind::STRUCTURE) assert(false);
        }
        interp->globals.count = 0;
    }

    void interpreter_execute_function(Interpreter *interp, Bytecode_Function *func,
                                      int64_t arg_count)
    {
        assert(interp);
        assert(func);
        assert(func->blocks.count);

        Stack_Frame frame = interpreter_create_stack_frame(interp->allocator, func);
        stack_push(&interp->stack_frames, frame);
        auto frame_p = interpreter_current_frame(interp);

        for (int64_t i = 0; i < arg_count; i++)
        {
            auto arg_copy = stack_peek(&interp->arg_stack, (arg_count - 1) - i);
            assert(arg_copy.kind == Bytecode_Value_Kind::TEMPORARY);
            arg_copy.kind = Bytecode_Value_Kind::PARAMETER;

            if (arg_copy.type->kind == AST_Type_Kind::STRUCTURE)
            {
                auto bit_size = arg_copy.type->bit_size;
                assert(bit_size);
                auto size = bit_size / 8;
                assert(size);
                auto new_mem = alloc(interp->allocator, size);
                memcpy(new_mem, arg_copy.value.struct_pointer, size);
                arg_copy.value.struct_pointer = new_mem;
            }
            else if (arg_copy.type->kind == AST_Type_Kind::ARRAY) assert(false);

            array_append(&frame_p->parameters, arg_copy);
        }

        for (int64_t i = 0; i < arg_count; i++)
        {
            stack_pop(&interp->arg_stack);
        }

        for (int64_t i = 0; i < func->local_allocs.count; i++)
        {
            Bytecode_Value allocl_val = {};
            allocl_val.kind = Bytecode_Value_Kind::ALLOCL;
            allocl_val.type = func->local_allocs[i].value->type;
            assert(allocl_val.type);

            if (allocl_val.type->kind == AST_Type_Kind::STRUCTURE ||
                allocl_val.type->kind == AST_Type_Kind::ARRAY)
            {
                auto bit_size = allocl_val.type->bit_size;
                assert(bit_size);
                auto size = bit_size / 8;
                assert(size);

                //@TODO: Custom allocator for this
                allocl_val.value.struct_pointer = alloc(interp->allocator, size);
            }


            stack_push(&interp->allocl_stack, allocl_val);
        }

        for (int64_t i = 0; i < func->local_temps.count; i++)
        {
            Bytecode_Value temp_val = {};
            temp_val.kind = Bytecode_Value_Kind::TEMPORARY;
            temp_val.type = func->local_temps[i]->type;
            assert(temp_val.type);

            if (temp_val.type->kind == AST_Type_Kind::STRUCTURE)
            {
                auto bit_size = temp_val.type->bit_size;
                assert(bit_size);
                auto size = bit_size / 8;
                assert(size);

                //@TODO: Custom allocator for this
                temp_val.value.struct_pointer = alloc(interp->allocator, size);
            }

            stack_push(&interp->temp_stack, temp_val);
        }
        frame_p->pushed_local_count = 0;
        frame_p->local_count = func->local_temps.count;

        Bytecode_Block *current_block = interpreter_current_block(interp, frame_p);
        while (!frame_p->returned && interp->running && current_block)
        {
            interpreter_execute_block(interp, current_block); 
            frame_p = interpreter_current_frame(interp);
            auto next_block = interpreter_current_block(interp, frame_p);
            if (frame_p->jumped)
            {
                frame_p->jumped = false;
                frame_p->pushed_local_count = next_block->preceding_temp_count;
            }
            current_block = next_block;
        }

        for (int64_t i = 0; i < frame_p->parameters.count; i++)
        {
            auto &param = frame_p->parameters[i];
            if (param.type->kind == AST_Type_Kind::STRUCTURE)
            {
                free(interp->allocator, param.value.struct_pointer);
            }
        }

        for (int64_t i = 0; i < func->local_allocs.count; i++)
        {
            auto val = stack_pop(&interp->allocl_stack);
            if (val.type->kind == AST_Type_Kind::STRUCTURE ||
                val.type->kind == AST_Type_Kind::ARRAY)
            {
                assert(val.kind == Bytecode_Value_Kind::ALLOCL);
                free(interp->allocator, val.value.struct_pointer);
            }
        }

        for (int64_t i = 0; i < func->local_temps.count; i++)
        {
            auto val = stack_pop(&interp->temp_stack);
            if (val.type->kind == AST_Type_Kind::STRUCTURE)
            {
                assert(val.kind == Bytecode_Value_Kind::TEMPORARY);
                free(interp->allocator, val.value.struct_pointer);
            }
        }

        if (interp->running)
        {
            assert(frame_p->returned);
        }
    }

    void interpreter_execute_block(Interpreter *interp, Bytecode_Block *block)
    {
        assert(interp);
        assert(block);
        assert(block->instructions.count);

        auto frame = interpreter_current_frame(interp);

        frame->instruction_index = 0;

        while (!frame->jumped &&
               !frame->returned &&
               interp->running && 
               frame->instruction_index < block->instructions.count)
        {
            Bytecode_Instruction inst = interpreter_fetch_instruction(interp);


            switch (inst)
            {
                case Bytecode_Instruction::NOP: assert(false);

                case Bytecode_Instruction::EXIT:
                {
                    uint32_t temp_index = interpreter_fetch<uint32_t>(interp);
                    Bytecode_Value *exit_code = interpreter_load_temporary(interp, temp_index);
                    assert(exit_code);
                    
                    interp->running = false;
                    interp->exited = true;
                    interp->exit_code_value = *exit_code;
                    break;
                }

                case Bytecode_Instruction::CALL:
                {
                    uint32_t func_index = interpreter_fetch<uint32_t>(interp);
                    uint32_t arg_count = interpreter_fetch<uint32_t>(interp);

                    auto func = interp->program->functions[func_index];
                    assert(func);

                    interpreter_execute_function(interp, func, arg_count);
                    auto return_frame = stack_pop(&interp->stack_frames);

                    if (return_frame.parameters.count)
                    {
                        array_free(&return_frame.parameters);
                    }

                    if (interp->running)
                    {
                        if (func->ast_decl->type->function.return_type != Builtin::type_void)
                        {
                            assert(return_frame.return_value.type);

                            Bytecode_Value *return_value =
                                interpreter_push_temporary(interp,
                                                           return_frame.return_value.type);

                            assert(return_value->type->kind == AST_Type_Kind::INTEGER);
                            return_value->value = return_frame.return_value.value;
                        }
                    }
                    break;
                }

                case Bytecode_Instruction::RET:
                {
                    uint32_t temp_index = interpreter_fetch<uint32_t>(interp);

                    auto ret_val = interpreter_load_temporary(interp, temp_index);
                    assert(ret_val);

                    assert (ret_val->type->kind == AST_Type_Kind::INTEGER ||
                            ret_val->type->kind == AST_Type_Kind::FLOAT);
                    frame->return_value.value = ret_val->value;
                    frame->return_value.type = ret_val->type;

                    frame->returned = true;
                    break;
                }

                case Bytecode_Instruction::RET_VOID:
                {
                    frame->returned = true;
                    break; 
                }

                case Bytecode_Instruction::ALLOCL:
                {
                    interpreter_fetch<uint32_t>(interp);
                    // Do nothing, handled on function call.
                    break;
                }

                case Bytecode_Instruction::LOAD_FLOAT:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    switch (size_spec)
                    {
                        case Bytecode_Size_Specifier::R32:
                        {
                            float val = interpreter_fetch<float>(interp);
                            auto bc_val = interpreter_push_temporary(interp,
                                                                     Builtin::type_float);
                            bc_val->value.float_literal.r32 = val;
                            break;
                        }

                        case Bytecode_Size_Specifier::R64:
                        {
                            double val = interpreter_fetch<double>(interp);
                            auto bc_val = interpreter_push_temporary(interp,
                                                                     Builtin::type_double);
                            bc_val->value.float_literal.r64 = val;
                            break;
                        }

                        default: assert(false);
                    }
                    break;
                }

                case Bytecode_Instruction::LOAD_INT:
                {
                    Const_Value cval = interpreter_load_int(interp);
                    assert(cval.type);
                    assert(cval.type->kind == AST_Type_Kind::INTEGER);
                    auto bc_val = interpreter_push_temporary(interp, cval.type);
                    bc_val->value.integer = cval.integer;

                    break;
                }

                case Bytecode_Instruction::LOADG:
                {
                    auto glob_index = interpreter_fetch<uint32_t>(interp);

                    assert(interp->globals.count >= glob_index);

                    auto global = &interp->globals[glob_index];

                    assert(global->type->kind == AST_Type_Kind::INTEGER ||
                           global->type->kind == AST_Type_Kind::FLOAT ||
                           global->type->kind == AST_Type_Kind::BOOL);

                    auto dest_val = interpreter_push_temporary(interp, global->type);

                    switch (global->type->kind)
                    {
                        case AST_Type_Kind::INTEGER:
                        case AST_Type_Kind::FLOAT:
                        case AST_Type_Kind::BOOL:
                        {
                            dest_val->value = global->value;
                            break;
                        }

                        default: assert(false);
                     }
                    break;
                }

                case Bytecode_Instruction::LOADL:
                {
                    auto allocl_index = interpreter_fetch<uint32_t>(interp);

                    auto source_allocl = interpreter_load_allocl(interp, allocl_index);
                    assert(source_allocl);
                    assert(source_allocl->type->kind == AST_Type_Kind::INTEGER ||
                           source_allocl->type->kind == AST_Type_Kind::FLOAT ||
                           source_allocl->type->kind == AST_Type_Kind::BOOL ||
                           source_allocl->type->kind == AST_Type_Kind::ENUM ||
                           source_allocl->type->kind == AST_Type_Kind::STRUCTURE);

                    auto dest_val = interpreter_push_temporary(interp, source_allocl->type);

                    switch (source_allocl->type->kind)
                    {
                        case AST_Type_Kind::INTEGER:
                        case AST_Type_Kind::FLOAT:
                        case AST_Type_Kind::BOOL:
                        case AST_Type_Kind::ENUM:
                        {
                            dest_val->value = source_allocl->value;
                            break;
                        }

                        case AST_Type_Kind::STRUCTURE:
                        {
                            assert(dest_val->type->kind == AST_Type_Kind::STRUCTURE);
                            assert(dest_val->value.struct_pointer);

                            auto size = dest_val->type->bit_size / 8;
                            memcpy(dest_val->value.struct_pointer,
                                    source_allocl->value.struct_pointer,
                                    size);
                            break;
                        }

                        default: assert(false);
                    }

                    break;
                }

                case Bytecode_Instruction::LOADP:
                {
                    auto ptr_idx = interpreter_fetch<int32_t>(interp);

                    auto ptr_val = interpreter_load_temporary(interp, ptr_idx);
                    assert(ptr_val->type->kind == AST_Type_Kind::POINTER);

                    auto result_val = interpreter_push_temporary(interp,
                                                                 ptr_val->type->pointer.base);
                    assert(result_val->type->bit_size % 8 == 0);
                    
                    switch (result_val->type->bit_size)
                    {
                        case 8:
                        {
                            result_val->value.integer.s8 =
                                *((int8_t*)ptr_val->value.pointer);
                            break;
                        }

                        case 16:
                        {
                            result_val->value.integer.s16 =
                                *((int16_t*)ptr_val->value.pointer);
                            break;
                        }

                        case 32:
                        {
                            result_val->value.integer.s32 =
                                *((int32_t*)ptr_val->value.pointer);
                            break;
                        }

                        case 64: 
                        {
                            result_val->value.integer.s64 =
                                *((int64_t*)ptr_val->value.pointer);
                            break;
                        }

                        default: assert(false);
                    }
                    
                    break;
                }

                case Bytecode_Instruction::LOAD_PARAM:
                {
                    auto param_index = interpreter_fetch<uint32_t>(interp);
                    assert(param_index < frame->parameters.count);

                    auto& param = frame->parameters[param_index];
                    assert(param.type);

                    auto result_value = interpreter_push_temporary(interp, param.type);

                    assert(param.type->kind == AST_Type_Kind::INTEGER ||
                           param.type->kind == AST_Type_Kind::FLOAT   ||
                           param.type->kind == AST_Type_Kind::ENUM    ||
                           param.type->kind == AST_Type_Kind::POINTER);
                    result_value->value = param.value;
                    break;
                }

                case Bytecode_Instruction::LOAD_BOOL:
                {
                    bool value = interpreter_fetch<uint8_t>(interp);
                    auto result_value = interpreter_push_temporary(interp, Builtin::type_bool);
                    result_value->value.boolean = value;
                    break;
                }

                case Bytecode_Instruction::LOAD_STR:
                {
                    auto str_idx = interpreter_fetch<uint32_t>(interp);
                    auto result_value = interpreter_push_temporary(interp, Builtin::type_ptr_u8);
                    result_value->value.pointer = (void*)interp->program->strings[str_idx].data;
                    break;
                }

                case Bytecode_Instruction::STOREG:
                {
                    auto glob_index = interpreter_fetch<uint32_t>(interp);
                    auto val_index = interpreter_fetch<uint32_t>(interp);

                    auto source_val = interpreter_load_temporary(interp, val_index);
                    auto dest_glob = &interp->globals[glob_index];

                    assert(source_val);
                    assert(dest_glob);

                    assert(source_val->type == dest_glob->type);

                    switch (source_val->type->kind)
                    {
                        case AST_Type_Kind::INTEGER:
                        case AST_Type_Kind::FLOAT:
                        case AST_Type_Kind::BOOL:
                        {
                            dest_glob->value = source_val->value;
                            break;
                        }

                        default: assert(false);
                    }
                    break;
                }

                case Bytecode_Instruction::STOREL:
                {
                    auto allocl_index = interpreter_fetch<uint32_t>(interp);
                    auto val_index = interpreter_fetch<uint32_t>(interp);

                    auto source_val = interpreter_load_temporary(interp, val_index);
                    auto dest_allocl = interpreter_load_allocl(interp, allocl_index);

                    assert(source_val);
                    assert(dest_allocl);
                    
                    assert(source_val->type == dest_allocl->type);

                    switch (source_val->type->kind)
                    {
                        case AST_Type_Kind::INTEGER:
                        case AST_Type_Kind::FLOAT:
                        case AST_Type_Kind::BOOL:
                        case AST_Type_Kind::ENUM:
                        {
                            dest_allocl->value = source_val->value;
                            break;
                        }

                        case AST_Type_Kind::STRUCTURE:
                        {
                            auto size = dest_allocl->type->bit_size / 8;
                            memcpy(dest_allocl->value.struct_pointer,
                                   source_val->value.struct_pointer,
                                   size);
                            break;
                        }

                        default: assert(false);
                    }
                    break;
                }

                case Bytecode_Instruction::STOREP:
                {
                    auto ptr_idx = interpreter_fetch<uint32_t>(interp);
                    auto val_idx = interpreter_fetch<uint32_t>(interp);

                    auto ptr_val = interpreter_load_temporary(interp, ptr_idx);
                    auto new_val = interpreter_load_temporary(interp, val_idx);

                    assert(ptr_val->type->kind == AST_Type_Kind::POINTER);
                    assert(ptr_val->type->pointer.base == new_val->type);

                    auto bit_size = new_val->type->bit_size;
                    assert(bit_size % 8 == 0);

#ifndef NDEBUG
                    auto tk = new_val->type->kind;
                    assert(tk == AST_Type_Kind::INTEGER ||
                           tk == AST_Type_Kind::FLOAT);
#endif

                    switch (bit_size)
                    {
                        case 8:
                        {
                            auto ptr = (int8_t*)ptr_val->value.pointer;
                            *ptr = new_val->value.integer.s8;
                            break;
                        }

                        case 16:
                        {
                            auto ptr = (int16_t*)ptr_val->value.pointer;
                            *ptr = new_val->value.integer.s16;
                            break;
                        }

                        case 32:
                        {
                            auto ptr = (int32_t*)ptr_val->value.pointer;
                            *ptr = new_val->value.integer.s32;
                            break;
                        }

                        case 64: 
                        {
                            auto ptr = (int64_t*)ptr_val->value.pointer;
                            *ptr = new_val->value.integer.s64;
                            break;
                        }

                        default: assert(false);
                    }
                    break;
                }

                case Bytecode_Instruction::STORE_PARAM:
                {
                    auto param_idx = interpreter_fetch<int32_t>(interp);
                    auto val_idx = interpreter_fetch<int32_t>(interp);

                    auto& param = frame->parameters[param_idx];
                    auto value = interpreter_load_temporary(interp, val_idx);

                    assert(param.type->kind == AST_Type_Kind::INTEGER ||
                           param.type->kind == AST_Type_Kind::POINTER);
                    param.value = value->value;
                    break;
                }

                case Bytecode_Instruction::ADDROF:
                {
                    auto alloc_index = interpreter_fetch<int32_t>(interp);
                    auto alloc = interpreter_load_allocl(interp, alloc_index);
                    assert(alloc->kind == Bytecode_Value_Kind::ALLOCL);
                    auto pointer_type =
                        build_data_find_or_create_pointer_type(interp->allocator,
                                                               interp->build_data,
                                                               alloc->type);
                    auto result = interpreter_push_temporary(interp, pointer_type);

                    if (alloc->type->kind == AST_Type_Kind::STRUCTURE)
                    {
                        result->value.pointer = alloc->value.struct_pointer;
                    }
                    else if (alloc->type->kind == AST_Type_Kind::INTEGER)
                    {
                        result->value.pointer = &alloc->value.integer;
                    }
                    else if (alloc->type->kind == AST_Type_Kind::ARRAY)
                    {
                        result->value.pointer = alloc->value.pointer;
                    }
                    else assert(false);

                    break;
                }

                case Bytecode_Instruction::PUSH_ARG:
                {
                    auto temp_index = interpreter_fetch<int32_t>(interp);
                    auto arg_val = interpreter_load_temporary(interp, temp_index);
                    assert(arg_val->type);
                    stack_push(&interp->arg_stack, *arg_val);
                    break;
                }

                case Bytecode_Instruction::EQ:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.boolean =
                                lhs_val->value.integer.s64 ==
                                rhs_val->value.integer.s64;
                            break;
                        }
                        default: assert(false);

                    }
                    break;
                }

                case Bytecode_Instruction::NEQ:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.boolean =
                                lhs_val->value.integer.s64 !=
                                rhs_val->value.integer.s64;
                            break;
                        }
                        default: assert(false);

                    }
                    break;
                }

                case Bytecode_Instruction::GT:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.boolean =
                                lhs_val->value.integer.s64 > rhs_val->value.integer.s64;
                            break;
                        }

                        case Bytecode_Size_Specifier::R32:
                        {
                            result_val->value.boolean = 
                                lhs_val->value.float_literal.r32 >
                                rhs_val->value.float_literal.r32;
                            break;
                        }

                        case Bytecode_Size_Specifier::R64: assert(false);
                        default: assert(false);

                    }
                    break;
                }

                case Bytecode_Instruction::GTEQ: assert(false);

                case Bytecode_Instruction::LT:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.boolean =
                                lhs_val->value.integer.s64 < rhs_val->value.integer.s64;
                            break;
                        }

                        case Bytecode_Size_Specifier::R32:
                        {
                            result_val->value.boolean = 
                                lhs_val->value.float_literal.r32 <
                                rhs_val->value.float_literal.r32;
                            break;
                        }

                        case Bytecode_Size_Specifier::R64: assert(false);
                        default: assert(false);

                    }
                    break;
                }

                case Bytecode_Instruction::LTEQ:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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

                        case Bytecode_Size_Specifier::U64:
                        {
                            result_val->value.boolean =
                                lhs_val->value.integer.u64 <= rhs_val->value.integer.u64;
                            break;
                        }

                        case Bytecode_Size_Specifier::S64:
                        {
                            result_val->value.boolean =
                                lhs_val->value.integer.s64 <= rhs_val->value.integer.s64;
                            break;
                        }

                        case Bytecode_Size_Specifier::R32:
                        {
                            result_val->value.boolean = 
                                lhs_val->value.float_literal.r32 <=
                                rhs_val->value.float_literal.r32;
                            break;
                        }

                        case Bytecode_Size_Specifier::R64: assert(false);
                        default: assert(false);

                    }
                    break;
                }

                case Bytecode_Instruction::ADD:
                {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

                    switch (size_spec)
                    {
                        case Bytecode_Size_Specifier::INVALID: assert(false);
                        case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                        case Bytecode_Size_Specifier::U8: 
                        {
                            result_val->value.integer.u8 =
                                lhs_val->value.integer.u8 + rhs_val->value.integer.u8;
                            break;
                        }

                        case Bytecode_Size_Specifier::S8: assert(false);
                        case Bytecode_Size_Specifier::U16: assert(false);
                        case Bytecode_Size_Specifier::S16: assert(false);
                        case Bytecode_Size_Specifier::U32: assert(false);
                        case Bytecode_Size_Specifier::S32: assert(false);

                        case Bytecode_Size_Specifier::R32:
                        {
                            result_val->value.float_literal.r32 =
                                lhs_val->value.float_literal.r32 +
                                rhs_val->value.float_literal.r32;
                            break;
                        }

                        case Bytecode_Size_Specifier::U64:
                        case Bytecode_Size_Specifier::S64:
                        {
                            result_val->value.integer.s64 =
                                lhs_val->value.integer.s64 + rhs_val->value.integer.s64;
                            break;
                        }
                        default: assert(false);

                    }
                    break;
                }

               case Bytecode_Instruction::SUB:
               {
                   auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                   auto lhs_index = interpreter_fetch<uint32_t>(interp);
                   auto rhs_index = interpreter_fetch<uint32_t>(interp);

                   auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                   auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                   auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                   assert(lhs_val->type == rhs_val->type);

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
                           result_val->value.integer.s64 =
                               lhs_val->value.integer.s64 - rhs_val->value.integer.s64;
                           break;
                       }

                       case Bytecode_Size_Specifier::R32:
                       {
                            result_val->value.float_literal.r32 = 
                                lhs_val->value.float_literal.r32 -
                                rhs_val->value.float_literal.r32;
                            break;
                       }

                       case Bytecode_Size_Specifier::R64: assert(false);
                       default: assert(false);

                   }
                   break;
               }

               case Bytecode_Instruction::REM:
               {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.integer.s64 =
                                lhs_val->value.integer.s64 % rhs_val->value.integer.s64;
                            break;
                        }
                        default: assert(false);

                    }
                   break;
               }

               case Bytecode_Instruction::MUL:
               {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.integer.s64 =
                                lhs_val->value.integer.s64 * rhs_val->value.integer.s64;
                            break;
                        }

                        case Bytecode_Size_Specifier::R32:
                        {
                            result_val->value.float_literal.r32 =
                                lhs_val->value.float_literal.r32 *
                                rhs_val->value.float_literal.r32;
                            break;
                        }

                        case Bytecode_Size_Specifier::R64: assert(false);
                        default: assert(false);

                    }
                   break;
               }

               case Bytecode_Instruction::DIV:
               {
                    auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                    auto lhs_index = interpreter_fetch<uint32_t>(interp);
                    auto rhs_index = interpreter_fetch<uint32_t>(interp);

                    auto lhs_val = interpreter_load_temporary(interp, lhs_index);
                    auto rhs_val = interpreter_load_temporary(interp, rhs_index);


                    auto result_val = interpreter_push_temporary(interp, lhs_val->type);

                    assert(lhs_val->type == rhs_val->type);

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
                            result_val->value.integer.s64 =
                                lhs_val->value.integer.s64 / rhs_val->value.integer.s64;
                            break;
                        }
                        default: assert(false);

                    }
                   break;
               }

               case Bytecode_Instruction::JUMP: 
               {
                   auto block_idx = interpreter_fetch<uint32_t>(interp);
                   assert(block_idx < frame->func->blocks.count);
                   frame->block_index = block_idx;
                   frame->jumped = true;
                   break;
               }

               case Bytecode_Instruction::JUMP_IF:
               {
                   auto val_idx = interpreter_fetch<uint32_t>(interp);
                   auto block_idx = interpreter_fetch<uint32_t>(interp);

                   assert(block_idx < frame->func->blocks.count);

                   auto val = interpreter_load_temporary(interp, val_idx);
                   if (val->value.boolean)
                   {
                       frame->block_index = block_idx;
                       frame->jumped = true;
                   }
                   break;
               }

               case Bytecode_Instruction::SWITCH:
               {
                   auto type_idx = interpreter_fetch<uint32_t>(interp);
                   assert(type_idx < interp->program->types->count);
                   auto val_idx = interpreter_fetch<uint32_t>(interp);
                   auto case_count = interpreter_fetch<uint32_t>(interp);

                   uint32_t default_block_index = 0;
                   default_block_index = interpreter_fetch<uint32_t>(interp);
                   assert(default_block_index < frame->func->blocks.count);

                   auto *switch_val = interpreter_load_temporary(interp, val_idx);
                   assert(switch_val->type == (*interp->program->types)[type_idx]);
                   assert(switch_val->type->kind == AST_Type_Kind::INTEGER ||
                          switch_val->type->kind == AST_Type_Kind::ENUM);

                   for (int64_t i = 0; i < case_count; i++)
                   {
                       Const_Value case_value = interpreter_load_int(interp);
                       assert(case_value.type == switch_val->type ||
                              switch_val->type->kind == AST_Type_Kind::ENUM);

                       auto block_idx = interpreter_fetch<uint32_t>(interp);

                       if (case_value.integer.u64 == switch_val->value.integer.u64)
                       {
                           frame->block_index = block_idx;
                           frame->jumped = true;
                           break;
                       }
                   }

                   if (!frame->jumped)
                   {
                       frame->block_index = default_block_index;
                       frame->jumped = true;
                   }

                   assert(frame->jumped);
                   break;
               }

               case Bytecode_Instruction::CAST_INT:
               case Bytecode_Instruction::CAST_ENUM:
               {
                   auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                   auto val_idx = interpreter_fetch<uint32_t>(interp);

                   auto val = interpreter_load_temporary(interp, val_idx);
                   assert(val->type->kind == AST_Type_Kind::INTEGER ||
                          val->type->kind == AST_Type_Kind::ENUM);

                   AST_Type *target_type = bytecode_type_from_size_spec(size_spec);
                   auto result = interpreter_push_temporary(interp, target_type);

                   uint64_t new_val;
                   switch (val->type->bit_size)
                   {
                       case 8: new_val = val->value.integer.u8; break;
                       case 16: new_val = val->value.integer.u16; break;
                       case 32: new_val = val->value.integer.u32; break;
                       case 64: new_val = val->value.integer.u64; break;
                       default: assert(false);
                   }

                   switch (size_spec)
                   {
                        
                       case Bytecode_Size_Specifier::INVALID: assert(false);
                       case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                       case Bytecode_Size_Specifier::U8:
                       {
                           result->value.integer.u8 = new_val;
                           break;
                       }

                       case Bytecode_Size_Specifier::S8: assert(false);
                       case Bytecode_Size_Specifier::U16: assert(false);
                       case Bytecode_Size_Specifier::S16: assert(false);

                       case Bytecode_Size_Specifier::U32:
                       {
                           result->value.integer.u32 = new_val;
                           break;
                       }

                       case Bytecode_Size_Specifier::S32:
                       {
                           result->value.integer.s32 = new_val;
                           break;
                       }

                       case Bytecode_Size_Specifier::U64: assert(false);
                       case Bytecode_Size_Specifier::S64:
                       {
                           result->value.integer.s64 = new_val;
                           break;
                       }

                       case Bytecode_Size_Specifier::R32:
                       {
                           result->value.float_literal.r32 = new_val;
                           break;
                       }

                       case Bytecode_Size_Specifier::R64: assert(false);

                       default: assert(false);

                   }

                   break;
               }

               case Bytecode_Instruction::CAST_FLOAT:
               {
                   auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);
                   auto val_idx = interpreter_fetch<uint32_t>(interp);

                   auto val = interpreter_load_temporary(interp, val_idx);
                   assert(val->type->kind == AST_Type_Kind::FLOAT);

                   auto target_type = bytecode_type_from_size_spec(size_spec);
                   int64_t new_val;
                   switch (val->type->bit_size)
                   {
                       case 8: assert(false);
                       case 16: assert(false);
                       case 32: new_val = val->value.float_literal.r32; break;
                       case 64: new_val = val->value.float_literal.r64; break;
                       default: assert(false);
                   }

                   auto result = interpreter_push_temporary(interp, target_type);

                   switch (size_spec)
                   {
                        
                       case Bytecode_Size_Specifier::INVALID: assert(false);
                       case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                       case Bytecode_Size_Specifier::U8: assert(false);
                       case Bytecode_Size_Specifier::S8: assert(false);
                       case Bytecode_Size_Specifier::U16: assert(false);
                       case Bytecode_Size_Specifier::S16: assert(false);
                       case Bytecode_Size_Specifier::U32: assert(false);

                       case Bytecode_Size_Specifier::S32:
                       {
                           result->value.integer.s32 = new_val;
                           break;

                       }

                       case Bytecode_Size_Specifier::U64: assert(false);
                       case Bytecode_Size_Specifier::S64:
                       {
                           result->value.integer.s64 = new_val;
                           break;
                       }

                       default: assert(false);

                   }

                   break;
               }

               case Bytecode_Instruction::SYSCALL:
               {
                   auto arg_count = interpreter_fetch<uint32_t>(interp);
                   Array<int64_t> args = {};
                   array_init(interp->allocator, &args, arg_count);
                   for (int64_t i = 0; i < arg_count; i++)
                   {
                       auto arg_val = stack_peek_ptr(&interp->arg_stack, (arg_count - 1) - i);
                       assert(arg_val->type == Builtin::type_s64 ||
                              arg_val->type->kind == AST_Type_Kind::POINTER);
                       array_append(&args, arg_val->value.integer.s64);
                   }

                   os_syscall(args);
                   array_free(&args);

                   for (int64_t i = 0; i < arg_count; i++) stack_pop(&interp->arg_stack);
                   break;
               }

               case Bytecode_Instruction::AGG_OFFSET_PTR:
               {
                   auto store_kind = interpreter_fetch<Bytecode_Value_Type_Specifier>(interp);
                   auto store_idx = interpreter_fetch<uint32_t>(interp);
                   auto offset_idx = interpreter_fetch<uint32_t>(interp);

                   //assert(store_kind == Bytecode_Value_Type_Specifier::ALLOCL ||
                          //store_kind == Bytecode_Value_Type_Specifier::PARAMETER);

                   Bytecode_Value *store_val = nullptr;
                   switch (store_kind)
                   {
                       case Bytecode_Value_Type_Specifier::INVALID: assert(false);

                       case Bytecode_Value_Type_Specifier::ALLOCL:
                       {
                           store_val = interpreter_load_allocl(interp, store_idx);
                           break;
                       }

                       case Bytecode_Value_Type_Specifier::PARAMETER:
                       {
                           store_val = &frame->parameters[store_idx];
                           break;
                       }

                       case Bytecode_Value_Type_Specifier::TEMPORARY:
                       {
                           store_val = interpreter_load_temporary(interp, store_idx);
                           break;
                       }
                   }

                   assert(store_val);

                   assert(store_val->kind == Bytecode_Value_Kind::ALLOCL ||
                          store_val->kind == Bytecode_Value_Kind::PARAMETER ||
                          store_val->kind == Bytecode_Value_Kind::TEMPORARY);

                   Bytecode_Value *offset_val = interpreter_load_temporary(interp, offset_idx);
                   assert(offset_val->type == Builtin::type_u32);
                   int64_t offset = offset_val->value.integer.u32;

                   AST_Type *struct_type = nullptr;
                   if (store_val->type->kind == AST_Type_Kind::STRUCTURE)
                   {
                       struct_type = store_val->type;
                   }
                   else if (store_val->type->kind == AST_Type_Kind::POINTER)
                   {
                       assert(store_val->type->pointer.base->kind == AST_Type_Kind::STRUCTURE);
                       struct_type = store_val->type->pointer.base;
                   }

                   assert(struct_type);

                   auto mem_types = struct_type->structure.member_types;
                   assert(offset < mem_types.count);

                   auto ptr_type = build_data_find_or_create_pointer_type(interp->allocator,
                                                                          interp->build_data,
                                                                          mem_types[offset]);
                   auto result = interpreter_push_temporary(interp, ptr_type);
                   assert(result);

                   auto byte_offset = 0;

                   for (int64_t i = 0; i < offset; i++)
                   {
                       byte_offset += (mem_types[i]->bit_size / 8);
                   }
                   //@TODO: When we do padding/alignment this byte_offset will need to change
                   //       accordingly 

                   assert(store_val->value.struct_pointer);

                   char *result_p = (char*)store_val->value.struct_pointer;
                   result->value.pointer = &result_p[byte_offset];
                   break;
               }

               case Bytecode_Instruction::ARR_OFFSET_PTR:
               {
                   auto store_kind = interpreter_fetch<Bytecode_Value_Type_Specifier>(interp);
                   auto store_idx = interpreter_fetch<uint32_t>(interp);
                   auto offset_idx = interpreter_fetch<uint32_t>(interp);

                   Bytecode_Value *store_val = nullptr;
                   switch (store_kind)
                   {
                       case Bytecode_Value_Type_Specifier::INVALID: assert(false);

                       case Bytecode_Value_Type_Specifier::ALLOCL:
                       {
                           store_val = interpreter_load_allocl(interp, store_idx);
                           break;
                       }

                       case Bytecode_Value_Type_Specifier::PARAMETER:
                       {
                           store_val = &frame->parameters[store_idx];
                           break;
                       }

                       case Bytecode_Value_Type_Specifier::TEMPORARY:
                       {
                           store_val = interpreter_load_temporary(interp, store_idx);
                           break;
                       }
                   }

                   assert(store_val);

                   auto offset_val = interpreter_load_temporary(interp, offset_idx);
                   assert(offset_val->type == Builtin::type_u32);
                   int64_t offset = offset_val->value.integer.u32;

                   AST_Type *element_type = nullptr;
                   AST_Type *elem_ptr_type = nullptr;
                   if (store_val->type->kind == AST_Type_Kind::POINTER)
                   {
                       element_type = store_val->type->pointer.base;
                       elem_ptr_type = store_val->type;
                   }
                   else if (store_val->type->kind == AST_Type_Kind::ARRAY)
                   {
                        element_type = store_val->type->array.element_type;
                        elem_ptr_type =
                            build_data_find_or_create_pointer_type(interp->allocator,
                                                                   interp->build_data,
                                                                   element_type);
                   }
                   else assert(false);
                   assert(element_type);

                   auto elem_byte_size = element_type->bit_size / 8;
                   auto byte_offset = offset * elem_byte_size;


                   auto result = interpreter_push_temporary(interp, elem_ptr_type);
                   char *result_p = (char*)store_val->value.pointer;
                   result->value.pointer = &result_p[byte_offset];
                   break;
               }
            }
        }
    }

    Stack_Frame interpreter_create_stack_frame(Allocator *allocator, Bytecode_Function *func)
    {
        assert(func);
        assert(func->blocks.count);

        Stack_Frame result = {};

        result.instruction_index = 0;
        result.block_index = 0;
        result.pushed_local_count = 0;
        result.local_count = 0;
        result.alloc_count = func->local_allocs.count;
        result.returned = false;
        result.jumped = false;
        result.func = func;

        array_init(allocator, &result.parameters, func->parameters.count);

        return result;
    }

    Stack_Frame *interpreter_current_frame(Interpreter *interp)
    {
        assert(interp);
        assert(stack_count(&interp->stack_frames));

        return stack_top_ptr(&interp->stack_frames);
    }

    Bytecode_Block *interpreter_current_block(Interpreter *interp)
    {
        assert(interp);

        if (interp->running && stack_count(&interp->stack_frames))
        {
            auto frame = interpreter_current_frame(interp);
            return interpreter_current_block(interp, frame);
        }

        return nullptr;
    }

    Bytecode_Block *interpreter_current_block(Interpreter *interp, Stack_Frame *frame)
    {
        assert(interp);
        assert(frame);

        assert(frame->block_index >= 0);
        assert(frame->block_index < frame->func->blocks.count);

        return frame->func->blocks[frame->block_index];
    }

    Bytecode_Value *interpreter_push_temporary(Interpreter *interp, AST_Type *type)
    {
        assert(type);


        auto frame = interpreter_current_frame(interp);
        auto local_index = frame->pushed_local_count;
        frame->pushed_local_count += 1;
        auto result = interpreter_load_temporary(interp, local_index);
        assert(result->type == type ||
               (result->type->kind == AST_Type_Kind::BOOL &&
                type->kind == AST_Type_Kind::INTEGER) ||
               (result->type->kind == AST_Type_Kind::ENUM &&
                type->kind == AST_Type_Kind::INTEGER));
        return result;
    }

    Bytecode_Value *interpreter_load_temporary(Interpreter *interp, int32_t local_index)
    {
        assert(interp);
        assert(interp->running);

        auto frame = interpreter_current_frame(interp);
        auto offset = (frame->local_count - 1) - local_index;
        assert(offset >= 0 && offset < frame->local_count);

        return stack_peek_ptr(&interp->temp_stack, offset);
    }

    Bytecode_Value *interpreter_load_allocl(Interpreter *interp, int32_t allocl_index)
    {
        assert(interp);

        auto frame = interpreter_current_frame(interp);
        auto offset = (frame->alloc_count - 1) - allocl_index;
        auto allocl = stack_peek_ptr(&interp->allocl_stack, offset);
        assert(allocl);

        assert(allocl->kind == Bytecode_Value_Kind::ALLOCL);
        return allocl;
    }

    Const_Value interpreter_load_int(Interpreter *interp)
    {
        auto size_spec = interpreter_fetch<Bytecode_Size_Specifier>(interp);

        Const_Value result = {};
        result.type = bytecode_type_from_size_spec(size_spec);

        switch (size_spec)
        {
            case Bytecode_Size_Specifier::INVALID: assert(false);
            case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

            case Bytecode_Size_Specifier::U8:
            {
                result.integer.u8 = interpreter_fetch<uint8_t>(interp);
                break;
            }

            case Bytecode_Size_Specifier::S8: assert(false);
            case Bytecode_Size_Specifier::U16: assert(false);
            case Bytecode_Size_Specifier::S16: assert(false);

            case Bytecode_Size_Specifier::U32:
            {
                result.integer.u32 = interpreter_fetch<uint32_t>(interp);
                break;
            }

            case Bytecode_Size_Specifier::S32: assert(false);

            case Bytecode_Size_Specifier::U64:
            case Bytecode_Size_Specifier::S64:
            {
                result.integer.u64 = interpreter_fetch<uint64_t>(interp);
                break;
            }
            default: assert(false);
        }

        return result;
    }

    Bytecode_Instruction interpreter_fetch_instruction(Interpreter *interp)

    {
        assert(interp);
        assert(interp->running);

        return (Bytecode_Instruction)interpreter_fetch<uint8_t>(interp);
    }
}
