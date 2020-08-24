#include "llvm_builder.h"

#include "builtin.h"
#include "os.h"

#include <llvm-c/Analysis.h>
#include <llvm/IR/Module.h>
#include <llvm-c/TargetMachine.h>

#include <stdio.h>


namespace Zodiac
{ 
    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder)
    {
        assert(allocator);
        assert(llvm_builder);

        llvm_builder->allocator = allocator;
        llvm_builder->llvm_module = LLVMModuleCreateWithName("root_module");
        llvm_builder->llvm_builder = LLVMCreateBuilder();

        array_init(allocator, &llvm_builder->functions);
        array_init(allocator, &llvm_builder->temps);
        array_init(allocator, &llvm_builder->allocas);

        stack_init(allocator, &llvm_builder->arg_stack);

        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        LLVMInitializeNativeAsmParser();

        auto target_triple = string_ref(LLVMGetDefaultTargetTriple());
        llvm_builder->target_triple = target_triple;

        if (string_contains(target_triple, "windows"))
        {
            llvm_builder->target_platform = Zodiac_Target_Platform::WINDOWS;
        }
        else if (string_contains(target_triple, "linux"))
        {
            llvm_builder->target_platform = Zodiac_Target_Platform::LINUX;
        }
        else assert(false);
    }

    bool llvm_emit_binary(LLVM_Builder *builder, const char *output_file_name)
    {
        assert(builder);
        assert(output_file_name);

        char *error = nullptr;
        bool verify_error = LLVMVerifyModule(builder->llvm_module, LLVMAbortProcessAction, &error);
        
        if (verify_error)
        {
            fprintf(stderr, "%s\n", error);
            assert(false);
        }
        LLVMDisposeMessage(error);
        error = nullptr;

        LLVMSetTarget(builder->llvm_module, builder->target_triple.data);
        LLVMTargetRef llvm_target = nullptr;
        bool target_error = LLVMGetTargetFromTriple(builder->target_triple.data,
                                                    &llvm_target, &error);
        if (target_error)
        {
            fprintf(stderr, "%s\n", error);
        }
        LLVMDisposeMessage(error);
        error = nullptr;

        auto cpu = "generic";
        auto features = "";

        LLVMTargetMachineRef llvm_target_machine = LLVMCreateTargetMachine(
                llvm_target,
                builder->target_triple.data,
                cpu,
                features,
                LLVMCodeGenLevelNone,
                LLVMRelocPIC,
                LLVMCodeModelDefault
            );

        String_Builder sb = {};
        string_builder_init(builder->allocator, &sb);
        string_builder_appendf(&sb, "%s.o", output_file_name);
        auto obj_file_name = string_builder_to_string(builder->allocator, &sb);
        string_builder_free(&sb);

        bool obj_emit_failed = LLVMTargetMachineEmitToFile(
                llvm_target_machine,
                builder->llvm_module,
                (char*)obj_file_name.data,
                LLVMObjectFile,
                &error
            );

        free(builder->allocator, obj_file_name.data);

        if (obj_emit_failed)
        {
            fprintf(stderr, "%s\n", error);
        }
        LLVMDisposeMessage(error);
        error = nullptr;

        LLVMDisposeTargetMachine(llvm_target_machine);

        return llvm_run_linker(builder->allocator, output_file_name);
    }

    bool llvm_run_linker(Allocator *allocator, const char *output_file_name)
    {
        assert(allocator);
        assert(output_file_name);

        String_Builder _sb = {};
        auto sb = &_sb;
        string_builder_init(allocator, sb);

#if linux
        string_builder_appendf(sb, "ld -static %s.o -o %s", output_file_name, output_file_name);

        auto link_cmd = string_builder_to_string(allocator, sb);
        printf("Running linker: %s\n", link_cmd.data);
        char out_buf[1024];
        FILE *link_process_handle = popen(link_cmd.data, "r");
        assert(link_process_handle);

        bool result = true;

        while (fgets(out_buf, sizeof(out_buf), link_process_handle) != nullptr)
        {
            fprintf(stderr, "%s", out_buf);
        }
        assert(feof(link_process_handle));
        int close_ret = pclose(link_process_handle);
        close_ret = WEXITSTATUS(close_ret);
        assert(close_ret >= 0);
        if (close_ret != 0)
        {
            result = false;
            fprintf(stderr, "Link command failed with exit code: %d\n", close_ret);
        }

        free(allocator, link_cmd.data);

        string_builder_free(sb);

        return result;
#elif _WIN32

        auto linker_path = string_ref("C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/14.27.29110/bin/Hostx64/x64/link.exe");

        string_builder_append(sb, linker_path.data);
        //string_builder_append(sb, " /nologo /wx /subsystem:CONSOLE ");
        string_builder_append(sb, " /nologo /wx /subsystem:CONSOLE /NODEFAULTLIB");
        string_builder_append(sb, " kernel32.lib");
        string_builder_append(sb, " msvcrtd.lib");
        string_builder_appendf(sb, " %s.o", output_file_name);

        auto arg_str = string_builder_to_string(allocator, sb);
        printf("Running link command: %s\n", arg_str.data);
        auto result = execute_process(allocator, {}, arg_str);
        free(allocator, arg_str.data);

        string_builder_free(sb);
        
        return result.success;
#endif

    }

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        assert(builder);
        assert(bc_func);

        auto func_decl = bc_func->ast_decl;

        builder->temps.count = 0;
        builder->allocas.count = 0;

        LLVMTypeRef llvm_func_type = llvm_type_from_ast(builder, func_decl->type);

        LLVMValueRef llvm_func_val = LLVMAddFunction(builder->llvm_module,
                                                     func_decl->identifier->atom.data,
                                                     llvm_func_type);
        assert(llvm_func_val);
        array_append(&builder->functions, { llvm_func_val, bc_func });

        if (bc_func->flags & BYTECODE_FUNC_FLAG_FOREIGN)
        {
            //assert(false);
        }
        else
        {
            for (int64_t i = 0; i < bc_func->blocks.count; i++)
            {
                auto bc_block = bc_func->blocks[i];
                LLVMAppendBasicBlock(llvm_func_val, bc_block->name.data);
            }

            LLVMPositionBuilderAtEnd(builder->llvm_builder, LLVMGetFirstBasicBlock(llvm_func_val));

            for (int64_t i = 0; i < bc_func->local_allocs.count; i++)
            {
                auto decl = bc_func->local_allocs[i].ast_decl;
                assert(decl->kind == AST_Declaration_Kind::VARIABLE);
                LLVMTypeRef llvm_type = llvm_type_from_ast(builder, decl->type);
                LLVMValueRef alloca_val = LLVMBuildAlloca(builder->llvm_builder, llvm_type,
                                                          decl->identifier->atom.data);
                array_append(&builder->allocas, alloca_val);
            }

            auto llvm_block = LLVMGetFirstBasicBlock(llvm_func_val);
            for (int64_t i = 0; i < bc_func->blocks.count; i++)
            {
                LLVM_Function_Context func_context =
                    llvm_create_function_context(llvm_func_val, bc_func, llvm_block,
                                                 bc_func->blocks[i]);
                llvm_emit_block(builder, &func_context);
                llvm_block = LLVMGetNextBasicBlock(llvm_block);
            }
        }
    }

    void llvm_emit_block(LLVM_Builder *builder, LLVM_Function_Context *func_context)
    {
        assert(builder);
        assert(func_context);

        LLVMPositionBuilderAtEnd(builder->llvm_builder, func_context->llvm_block);

        while (func_context->ip < func_context->bc_block->instructions.count)
        {
            auto inst =
                (Bytecode_Instruction)func_context->bc_block->instructions[func_context->ip++];
            llvm_emit_instruction(builder, inst, func_context);
        }

        if (!llvm_block_ends_with_terminator(func_context->llvm_block))
        {
            auto func = func_context->bc_func;

            if (func->flags & BYTECODE_FUNC_FLAG_NORETURN)
            {
                LLVMBuildUnreachable(builder->llvm_builder);
            }
            else if (func->ast_decl->type->function.return_type == Builtin::type_void)
            {
                LLVMBuildRetVoid(builder->llvm_builder);
            }
            else assert(false);
        }
    }

    void llvm_emit_instruction(LLVM_Builder *builder, Bytecode_Instruction inst,
                               LLVM_Function_Context *func_context)
    {
        switch (inst)
        {
            case Bytecode_Instruction::NOP: assert(false);

            case Bytecode_Instruction::EXIT:
            {
                llvm_emit_exit(builder, func_context);
                break;
            }

            case Bytecode_Instruction::CALL:
            {
                auto func_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                   &func_context->ip);
                auto arg_count = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);

                Array<LLVMValueRef> args = {};
                if (arg_count)
                {
                    array_init(builder->allocator, &args, arg_count);
                    for (uint32_t i = 0; i < arg_count; i++)
                    {
                        auto offset = (arg_count - 1) - i;
                        LLVMValueRef arg_val = stack_peek(&builder->arg_stack, offset);
                        array_append(&args, arg_val);
                    }
                }

                auto func = builder->functions[func_idx];
                LLVMValueRef func_val = func.llvm_func;
                LLVMValueRef ret_val = LLVMBuildCall(builder->llvm_builder, func_val,
                                                     args.data, args.count, "");
                llvm_push_temporary(builder, ret_val);

                if (func.bc_func->flags & BYTECODE_FUNC_FLAG_NORETURN)
                {
                    LLVMBuildUnreachable(builder->llvm_builder);
                }

                if (arg_count)
                {
                    array_free(&args);
                }
                break;
            }

            case Bytecode_Instruction::RET:
            {
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                LLVMValueRef ret_val = builder->temps[val_idx];
                LLVMBuildRet(builder->llvm_builder, ret_val);
                break;
            }

            case Bytecode_Instruction::RET_VOID: assert(false);

            case Bytecode_Instruction::ALLOCL:
            {
                llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block, &func_context->ip);
                break;
            }

            case Bytecode_Instruction::LOAD_IM:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);
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
                        auto val = llvm_fetch_from_bytecode<int64_t>(func_context->bc_block,
                                                                     &func_context->ip);
                        LLVMTypeRef type = LLVMIntType(64);
                        LLVMValueRef result = LLVMConstInt(type, val, true);
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::LOADL: 
            {
                auto allocl_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                       &func_context->ip);
                LLVMValueRef llvm_alloca = builder->allocas[allocl_index];
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, llvm_alloca, "");
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOADP: 
            {
                auto ptr_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef ptr_val = builder->temps[ptr_idx];
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, ptr_val, "");
                llvm_push_temporary(builder, result);
                break; 
            }

            case Bytecode_Instruction::LOAD_PARAM:
            {
                auto param_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                      &func_context->ip);
                LLVMValueRef llvm_param_val = LLVMGetParam(func_context->llvm_function,
                                                           param_index);
                llvm_push_temporary(builder, llvm_param_val);
                break;
            }

            case Bytecode_Instruction::STOREL:
            {
                auto dest_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);
                auto val_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);

                LLVMValueRef source_val = builder->temps[val_index];
                LLVMValueRef dest_val = builder->allocas[dest_index];

                LLVMBuildStore(builder->llvm_builder, source_val, dest_val);
                break;
            }

            case Bytecode_Instruction::STOREP: 
            {
                auto ptr_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef ptr_val = builder->temps[ptr_idx];
                LLVMValueRef val = builder->temps[val_idx];

                LLVMBuildStore(builder->llvm_builder, val, ptr_val);
                break;
            }

            case Bytecode_Instruction::PUSH_ARG:
            {
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                 &func_context->ip);
                LLVMValueRef val = builder->temps[val_idx];
                stack_push(&builder->arg_stack, val);
                break;
            }

            case Bytecode_Instruction::ADD:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = builder->temps[lhs_idx];
                LLVMValueRef rhs_val = builder->temps[rhs_idx];

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
                        LLVMValueRef result = LLVMBuildAdd(builder->llvm_builder,
                                                           lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::OFFSET_PTR:
            {
                auto store_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                auto offset_val = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);

                auto store_val = builder->allocas[store_idx];

                LLVMTypeRef llvm_idx_type = llvm_type_from_ast(builder, Builtin::type_u32);

                LLVMValueRef zero_val = LLVMConstNull(llvm_idx_type);
                LLVMValueRef llvm_offset_val = LLVMConstInt(llvm_idx_type, offset_val, true);
                LLVMValueRef indices[] = { zero_val, llvm_offset_val };


                LLVMValueRef result = LLVMBuildGEP(builder->llvm_builder, store_val,
                                                   indices, 2, "");

                llvm_push_temporary(builder, result);
                break;
            }
        }
    }

    void llvm_emit_exit(LLVM_Builder *builder, LLVM_Function_Context *func_context)
    {
        auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                          &func_context->ip);
        LLVMValueRef val = builder->temps[val_idx];

        switch (builder->target_platform)
        {
            case Zodiac_Target_Platform::INVALID: assert(false);

            case Zodiac_Target_Platform::LINUX:
            {
                auto asm_string = string_ref("syscall");

                String_Builder sb = {};
                string_builder_init(builder->allocator, &sb);

                string_builder_append(&sb, "=r,{rax},{rdi}");

                LLVMTypeRef asm_fn_type = llvm_asm_function_type(builder, 1);

                auto constraint_string = string_builder_to_string(sb.allocator, &sb);
                string_builder_free(&sb);

                LLVMValueRef asm_val = LLVMGetInlineAsm(asm_fn_type,
                                                        asm_string.data,
                                                        asm_string.length,
                                                        constraint_string.data,
                                                        constraint_string.length,
                                                        true,
                                                        false,
                                                        LLVMInlineAsmDialectATT);

                LLVMTypeRef arg_type = llvm_type_from_ast(builder, Builtin::type_s64);
                LLVMValueRef syscall_num = LLVMConstInt(arg_type, 60, true);
                LLVMValueRef args[2] = { syscall_num, val };
                LLVMBuildCall(builder->llvm_builder, asm_val, args, 2, "");

                LLVMBuildUnreachable(builder->llvm_builder);

                free(sb.allocator, constraint_string.data);
                break;
            }

            case Zodiac_Target_Platform::WINDOWS:
            {
                LLVMValueRef exitprocess_func = LLVMGetNamedFunction(builder->llvm_module,
                                                                     "ExitProcess"); 
                assert(exitprocess_func);

                LLVMBuildCall(builder->llvm_builder, exitprocess_func, &val, 1, "");
                LLVMBuildUnreachable(builder->llvm_builder);
                break;
            }
        }

    }

    void llvm_push_temporary(LLVM_Builder *builder, LLVMValueRef temp_val)
    {
        assert(builder);
        assert(temp_val);

        array_append(&builder->temps, temp_val);
    }

    LLVMTypeRef llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        assert(builder);
        assert(ast_type);

        assert(ast_type->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(ast_type->flags & AST_NODE_FLAG_TYPED);
        assert(ast_type->flags & AST_NODE_FLAG_SIZED);

        switch (ast_type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::VOID:
            {
                return LLVMVoidType();
                break;
            }

            case AST_Type_Kind::INTEGER:
            {
                return LLVMIntType(ast_type->bit_size);
                break;
            }

            case AST_Type_Kind::POINTER:
            {
                LLVMTypeRef base_type = llvm_type_from_ast(builder, ast_type->pointer.base);
                return LLVMPointerType(base_type, 0);
                break;
            }

            case AST_Type_Kind::FUNCTION:
            {
                LLVMTypeRef llvm_ret_type = llvm_type_from_ast(builder,
                                                               ast_type->function.return_type);
                Array<LLVMTypeRef> llvm_arg_types = {};
                if (ast_type->function.param_types.count)
                {
                    array_init(builder->allocator, &llvm_arg_types, 
                               ast_type->function.param_types.count);
                    for (int64_t i = 0; i < ast_type->function.param_types.count; i++)
                    {
                        LLVMTypeRef arg_type =
                            llvm_type_from_ast(builder, ast_type->function.param_types[i]);
                        array_append(&llvm_arg_types, arg_type);
                    }
                }

                bool is_vararg = false;

                LLVMTypeRef result = LLVMFunctionType(llvm_ret_type, llvm_arg_types.data,
                                                      llvm_arg_types.count, is_vararg);

                if (llvm_arg_types.count)
                {
                    array_free(&llvm_arg_types);
                }
                return result;
                break;
            }

            case AST_Type_Kind::STRUCTURE:
            {
                auto name = ast_type->structure.declaration->identifier->atom;
                LLVMTypeRef result = LLVMGetTypeByName(builder->llvm_module, name.data);
                if (result)
                {
                    assert(LLVMGetTypeKind(result) == LLVMStructTypeKind);
                }
                else
                {
                    result = LLVMStructCreateNamed(LLVMGetGlobalContext(), name.data);

                    auto ast_mem_types = ast_type->structure.member_types;
                    assert(ast_mem_types.count);

                    Array<LLVMTypeRef> mem_types = {};
                    array_init(builder->allocator, &mem_types, ast_mem_types.count);
                    for (int64_t i = 0; i < ast_mem_types.count; i++)
                    {
                        array_append(&mem_types, llvm_type_from_ast(builder, ast_mem_types[i]));
                    }

                    LLVMStructSetBody(result, mem_types.data, mem_types.count, false);
                }

                return result;
                break;
            }
        }

        assert(false);
        return {};
    }

    LLVMTypeRef llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count)
    {
        assert(builder);
        assert(arg_count >= 0);
        assert(arg_count < 6);

        LLVMTypeRef ret_type = llvm_type_from_ast(builder, Builtin::type_s64);
        LLVMTypeRef param_types[7] = { ret_type, ret_type, ret_type, ret_type,
                                       ret_type, ret_type, ret_type };
        LLVMTypeRef result = LLVMFunctionType(ret_type, param_types, arg_count + 1, false);

        return result;
    }

    bool llvm_block_ends_with_terminator(LLVMBasicBlockRef llvm_block)
    {
        assert(llvm_block);

        LLVMValueRef term_val = LLVMGetBasicBlockTerminator(llvm_block);
        
        if (term_val == nullptr) return false;
        return true;
    }

    LLVM_Function_Context llvm_create_function_context(LLVMValueRef llvm_func,
                                                       Bytecode_Function *bc_func,
                                                       LLVMBasicBlockRef llvm_block,
                                                       Bytecode_Block *bc_block)
    {
        LLVM_Function_Context result = {};
        result.llvm_function = llvm_func;
        result.llvm_block = llvm_block;
        result.bc_func = bc_func;
        result.bc_block = bc_block;
        result.ip = 0;

        return result;
    }

    void llvm_print(Allocator *allocator, LLVM_Builder *builder)
    {
        assert(allocator);
        assert(builder);

        const char* llvm_module_string = LLVMPrintModuleToString(builder->llvm_module);
        printf("%s", llvm_module_string);
        ::free((void*)llvm_module_string);
    }
}
