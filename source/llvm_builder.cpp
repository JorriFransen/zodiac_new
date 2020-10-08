#include "llvm_builder.h"

#include "builtin.h"
#include "os.h"

#include <llvm-c/TargetMachine.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/FileSystem.h>

#include <stdio.h>

#include <tracy/Tracy.hpp>

namespace Zodiac
{ 
    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder,
                           Build_Data *build_data, Bytecode_Program *bc_program)
    {
        assert(allocator);
        assert(llvm_builder);
        assert(build_data);

        llvm_builder->allocator = allocator;
        llvm_builder->build_data = build_data;
        llvm_builder->llvm_context = (llvm::LLVMContext*)LLVMGetGlobalContext();
        llvm_builder->llvm_module = LLVMModuleCreateWithName("root_module");
        llvm_builder->llvm_builder = LLVMCreateBuilder();
        llvm_builder->_llvm_builder = llvm::unwrap(llvm_builder->llvm_builder);

        array_init(allocator, &llvm_builder->functions);
        array_init(allocator, &llvm_builder->temps);
        array_init(allocator, &llvm_builder->allocas);
        array_init(allocator, &llvm_builder->params);
        array_init(allocator, &llvm_builder->globals);

        stack_init(allocator, &llvm_builder->arg_stack);

        llvm_builder->bc_program = bc_program;

        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();

        auto _target_triple = llvm::sys::getDefaultTargetTriple();
        auto target_triple = string_copy(allocator,
                                         _target_triple.data(), _target_triple.length());
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
        ZoneScoped

        assert(builder);
        assert(output_file_name);

        bool verify_error = llvm::verifyModule(*llvm::unwrap(builder->llvm_module),
                                               &llvm::errs());
        
        if (verify_error)
        {
            assert(false);
        }

        llvm::unwrap(builder->llvm_module)->setTargetTriple(builder->target_triple.data);
        std::string error;
        const llvm::Target *llvm_target =
            llvm::TargetRegistry::lookupTarget(builder->target_triple.data, error);
        assert(llvm_target);
        
        auto cpu = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto rm = llvm::Optional<llvm::Reloc::Model>();

        llvm::TargetMachine *llvm_target_machine =
            llvm_target->createTargetMachine(builder->target_triple.data,
                                             cpu, features, opt, rm);

        llvm::unwrap(builder->llvm_module)->setDataLayout(
                llvm_target_machine->createDataLayout());

        String_Builder sb = {};
        string_builder_init(builder->allocator, &sb);
        string_builder_appendf(&sb, "%s.o", output_file_name);
        auto obj_file_name = string_builder_to_string(builder->allocator, &sb);
        string_builder_free(&sb);


        { ZoneScopedN("LLVMTargetMachineEmitToFile")

        std::error_code err_code;
        llvm::raw_fd_ostream dest(obj_file_name.data, err_code, llvm::sys::fs::OF_None);
        if (err_code)
        {
            fprintf(stderr, "Could not open file: %s\n", obj_file_name.data);
            assert(false);
        }
       
        llvm::legacy::PassManager pass;
        auto filetype = llvm::CGFT_ObjectFile;
        if (llvm_target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype))
        {
            fprintf(stderr, "TargetMachine can't emit a file of this type");
            assert(false);
        }
        pass.run(*llvm::unwrap(builder->llvm_module));
        

        }

        free(builder->allocator, obj_file_name.data);

        delete llvm_target_machine;

        return llvm_run_linker(builder, output_file_name);
    }

    bool llvm_run_linker(LLVM_Builder *builder, const char *output_file_name)
    {
        ZoneScopedNCS("llvm_run_linker", 0x00ffff, 32);

        assert(output_file_name);

        String_Builder _sb = {};
        auto sb = &_sb;
        string_builder_init(builder->allocator, sb);

        auto options = builder->build_data->options;
        bool print_command = options->print_link_command || options->verbose;

#if linux
        string_builder_appendf(sb, "ld -static %s.o -o %s", output_file_name, output_file_name);

        auto link_cmd = string_builder_to_string(builder->allocator, sb);
        if (print_command) printf("Running linker: %s\n", link_cmd.data);
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
        if (print_command) printf("Running link command: %s\n", arg_str.data);
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
        builder->params.count = 0;

        auto llvm_func_type = llvm_type_from_ast<llvm::FunctionType>(builder,
                                                                     func_decl->type);

        llvm::Function *llvm_func_val =
            llvm::Function::Create(llvm_func_type, llvm::GlobalValue::ExternalLinkage,
                                   func_decl->identifier->atom.data,
                                   llvm::unwrap(builder->llvm_module));
        assert(llvm_func_val);
        array_append(&builder->functions, { llvm_func_val, bc_func });

        if (bc_func->flags & BYTECODE_FUNC_FLAG_FOREIGN)
        {
            //assert(false);
        }
        else
        {
            Array<llvm::BasicBlock *> llvm_blocks = {};
            array_init(builder->allocator, &llvm_blocks, bc_func->blocks.count);

            for (int64_t i = 0; i < bc_func->blocks.count; i++)
            {
                auto bc_block = bc_func->blocks[i];
                auto llvm_block = llvm::BasicBlock::Create(*builder->llvm_context,
                                                           bc_block->name.data,
                                                           llvm_func_val);
                array_append(&llvm_blocks, llvm_block);
            }

            builder->_llvm_builder->SetInsertPoint(&llvm_func_val->front());

            for (int64_t i = 0; i < bc_func->parameters.count; i++)
            {
                llvm::Type *param_type =
                    llvm_type_from_ast(builder, bc_func->parameters[i].value->type);

                auto name = bc_func->parameters[i].value->name;

                auto param_alloca =
                    builder->_llvm_builder->CreateAlloca(param_type, nullptr, name.data);

                array_append(&builder->params, param_alloca);
            }

            for (int64_t i = 0; i < bc_func->parameters.count; i++)
            {
                auto param_val = llvm_func_val->getArg(i);
                auto param_alloca = builder->params[i];
                builder->_llvm_builder->CreateStore(param_val, param_alloca);
            }

            for (int64_t i = 0; i < bc_func->local_allocs.count; i++)
            {
                auto decl = bc_func->local_allocs[i].ast_decl;
                assert(decl->kind == AST_Declaration_Kind::VARIABLE);
                llvm::Type *llvm_type = llvm_type_from_ast(builder, decl->type);
                auto alloca_val =
                    builder->_llvm_builder->CreateAlloca(llvm_type, nullptr,
                                                         decl->identifier->atom.data);

                array_append(&builder->allocas, alloca_val);
            }

            auto block_it = llvm_func_val->begin();
            for (int64_t i = 0; i < bc_func->blocks.count; i++)
            {
                LLVM_Function_Context func_context =
                    llvm_create_function_context(llvm_func_val, bc_func, &*block_it,
                                                 bc_func->blocks[i], llvm_blocks);
                llvm_emit_block(builder, &func_context);
                block_it++;
            }

            array_free(&llvm_blocks);
        }

        //printf("Emitted function: %s\n", bc_func->ast_decl->identifier->atom.data);
        //printf("%s\n\n", LLVMPrintValueToString(llvm_func_val));
    }

    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Global bc_glob)
    {
        auto decl = bc_glob.decl;
        assert(decl->kind == AST_Declaration_Kind::VARIABLE);
        assert(decl->decl_flags & AST_DECL_FLAG_GLOBAL);

        auto name = decl->identifier->atom.data;

        llvm::Type *llvm_type = llvm_type_from_ast(builder, decl->type);
        LLVMValueRef llvm_glob = LLVMAddGlobal(builder->llvm_module,
                                               llvm::wrap(llvm_type),
                                               name);
        LLVMSetLinkage(llvm_glob, LLVMPrivateLinkage);

        auto bc_val = bc_glob.value;
        if (bc_val)
        {

#ifndef NDEBUG
            auto bc_idx = bc_glob.value->glob_index;
            auto dest_idx = builder->globals.count;
            assert(bc_idx == dest_idx);
#endif

            assert(bc_val->kind == Bytecode_Value_Kind::GLOBAL);
            LLVMValueRef init_val = llvm_emit_constant(builder, bc_val);
            LLVMSetInitializer(llvm_glob, init_val);
        }

        array_append(&builder->globals, llvm::unwrap(llvm_glob));
    }

    void llvm_emit_block(LLVM_Builder *builder, LLVM_Function_Context *func_context)
    {
        assert(builder);
        assert(func_context);

        builder->_llvm_builder->SetInsertPoint(func_context->llvm_block);

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
                        llvm::Value *arg_val = stack_peek(&builder->arg_stack, offset);
                        array_append(&args, llvm::wrap(arg_val));
                    }

                    for (uint32_t i = 0; i < arg_count; i++) stack_pop(&builder->arg_stack);
                }

                auto func = builder->functions[func_idx];
                LLVMValueRef func_val = llvm::wrap(func.llvm_func);
                LLVMValueRef ret_val = LLVMBuildCall(builder->llvm_builder, func_val,
                                                     args.data, args.count, "");


                if (!(func.bc_func->ast_decl->type->function.return_type == Builtin::type_void))
                {
                    llvm_push_temporary(builder, ret_val);
                }

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
                LLVMValueRef ret_val = llvm::wrap(builder->temps[val_idx]);
                LLVMBuildRet(builder->llvm_builder, ret_val);
                break;
            }

            case Bytecode_Instruction::RET_VOID:
            {
                LLVMBuildRetVoid(builder->llvm_builder);
                break;
            }

            case Bytecode_Instruction::ALLOCL:
            {
                llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block, &func_context->ip);
                break;
            }

            case Bytecode_Instruction::LOAD_FLOAT:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);
                LLVMValueRef result = nullptr;

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
                    case Bytecode_Size_Specifier::FLOAT_FLAG: assert(false);

                    case Bytecode_Size_Specifier::R32:
                    {
                        auto val = llvm_fetch_from_bytecode<float>(func_context->bc_block,
                                                                   &func_context->ip);
                        LLVMTypeRef type = LLVMFloatType();
                        result = LLVMConstReal(type, val);
                        break;
                    }

                    case Bytecode_Size_Specifier::R64:
                    {
                        auto val = llvm_fetch_from_bytecode<double>(func_context->bc_block,
                                                                    &func_context->ip);
                        LLVMTypeRef type = LLVMDoubleType();
                        result = LLVMConstReal(type, val);
                        break;
                    }
                    default: assert(false);

                }

                assert(result);
                llvm_push_temporary(builder, result);
                break;

            }

            case Bytecode_Instruction::LOAD_INT:
            {
                Const_Value cv = llvm_load_int(func_context->bc_block, &func_context->ip);
                LLVMValueRef result = llvm_const_int(cv);
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOADG:
            {
                auto glob_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);
                assert(glob_index < builder->globals.count);

                LLVMValueRef llvm_glob = llvm::wrap(builder->globals[glob_index]);
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, llvm_glob, "");
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOADL: 
            {
                auto allocl_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                       &func_context->ip);
                LLVMValueRef llvm_alloca = llvm::wrap(builder->allocas[allocl_index]);
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, llvm_alloca, "");
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOADP: 
            {
                auto ptr_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef ptr_val = llvm::wrap(builder->temps[ptr_idx]);
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, ptr_val, "");
                llvm_push_temporary(builder, result);
                break; 
            }

            case Bytecode_Instruction::LOAD_PARAM:
            {
                auto param_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                      &func_context->ip);
                LLVMValueRef result =
                    LLVMBuildLoad(builder->llvm_builder,
                                  llvm::wrap(builder->params[param_index]), "");
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOAD_BOOL:
            {
                bool value = llvm_fetch_from_bytecode<uint8_t>(func_context->bc_block,
                                                               &func_context->ip);
                llvm::Type *llvm_bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                LLVMValueRef result = LLVMConstInt(llvm::wrap(llvm_bool_type), value, false);
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::LOAD_STR:
            {
                auto str_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto str = builder->bc_program->strings[str_idx];
                LLVMValueRef llvm_str = LLVMConstString(str.data, str.length, false);
                LLVMValueRef llvm_str_glob = LLVMAddGlobal(builder->llvm_module,
                                                           LLVMTypeOf(llvm_str),
                                                           "_string_const");
                LLVMSetInitializer(llvm_str_glob, llvm_str);
                LLVMSetLinkage(llvm_str_glob, LLVMPrivateLinkage);
                LLVMSetUnnamedAddress(llvm_str_glob, LLVMGlobalUnnamedAddr);
                LLVMSetAlignment(llvm_str_glob, 1);
                LLVMSetGlobalConstant(llvm_str_glob, true);

                llvm::Type *dest_type = llvm_type_from_ast(builder, Builtin::type_ptr_u8);
                LLVMValueRef llvm_str_ptr = LLVMConstPointerCast(llvm_str_glob,
                                                                 llvm::wrap(dest_type));
                llvm_push_temporary(builder, llvm_str_ptr);

                break;
            }

            case Bytecode_Instruction::STOREG:
            {

                auto glob_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);
                auto val_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);

                assert(glob_index < builder->globals.count);

                LLVMValueRef source_val = llvm::wrap(builder->temps[val_index]);
                LLVMValueRef llvm_glob = llvm::wrap(builder->globals[glob_index]);

                LLVMBuildStore(builder->llvm_builder, source_val, llvm_glob);
                break;
            }

            case Bytecode_Instruction::STOREL:
            {
                auto dest_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);
                auto val_index = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);

                LLVMValueRef source_val = llvm::wrap(builder->temps[val_index]);
                LLVMValueRef dest_val = llvm::wrap(builder->allocas[dest_index]);

                LLVMBuildStore(builder->llvm_builder, source_val, dest_val);
                break;
            }

            case Bytecode_Instruction::STOREP: 
            {
                auto ptr_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef ptr_val = llvm::wrap(builder->temps[ptr_idx]);
                LLVMValueRef val = llvm::wrap(builder->temps[val_idx]);

                LLVMBuildStore(builder->llvm_builder, val, ptr_val);
                break;
            }

            case Bytecode_Instruction::STORE_PARAM:
            {
                auto param_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef param_alloca = llvm::wrap(builder->params[param_idx]);
                LLVMValueRef val = llvm::wrap(builder->temps[val_idx]);

                LLVMBuildStore(builder->llvm_builder, val, param_alloca);
                break;
            }

            case Bytecode_Instruction::ADDROF:
            {
                auto alloc_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                LLVMValueRef alloca = llvm::wrap(builder->allocas[alloc_idx]);
                llvm_push_temporary(builder, alloca);
                break;
            }

            case Bytecode_Instruction::PUSH_ARG:
            {
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                 &func_context->ip);
                llvm::Value *val = builder->temps[val_idx];
                stack_push(&builder->arg_stack, val);
                break;
            }

            case Bytecode_Instruction::EQ:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::U64:
                    case Bytecode_Size_Specifier::S64:
                    {
                        LLVMValueRef result = LLVMBuildICmp(builder->llvm_builder, LLVMIntEQ,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
                break;
            }

            case Bytecode_Instruction::NEQ:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::U64:
                    case Bytecode_Size_Specifier::S64:
                    {
                        LLVMValueRef result = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
                break;
            }

            case Bytecode_Instruction::GT:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                bool sign = (uint16_t)size_spec & 
                            (uint16_t)Bytecode_Size_Specifier::SIGN_FLAG;

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::U64:
                    case Bytecode_Size_Specifier::S64:
                    {
                        LLVMIntPredicate op = LLVMIntUGT;
                        if (sign) op = LLVMIntSGT;
                        LLVMValueRef result = LLVMBuildICmp(builder->llvm_builder, op,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    case Bytecode_Size_Specifier::R64:
                    {
                        LLVMValueRef result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOGT,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::GTEQ: assert(false);

            case Bytecode_Instruction::LT:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                bool sign = (uint16_t)size_spec & 
                            (uint16_t)Bytecode_Size_Specifier::SIGN_FLAG;


                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::U64:
                    case Bytecode_Size_Specifier::S64:
                    {
                        LLVMIntPredicate op = LLVMIntULT;
                        if (sign) op = LLVMIntSLT;
                        LLVMValueRef result = LLVMBuildICmp(builder->llvm_builder, op,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    case Bytecode_Size_Specifier::R64:
                    {
                        LLVMValueRef result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOLT,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::LTEQ:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                bool sign = (uint16_t)size_spec &
                            (uint16_t)Bytecode_Size_Specifier::SIGN_FLAG;

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);

                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::U64:
                    case Bytecode_Size_Specifier::S64:
                    {
                        LLVMIntPredicate op = LLVMIntULE;
                        if (sign) op = LLVMIntSLE;
                        LLVMValueRef result = LLVMBuildICmp(builder->llvm_builder, op,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    case Bytecode_Size_Specifier::R64:
                    {
                        LLVMValueRef result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOLE,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
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

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);


                    case Bytecode_Size_Specifier::U8:
                    case Bytecode_Size_Specifier::S8:
                    case Bytecode_Size_Specifier::U16:
                    case Bytecode_Size_Specifier::S16:
                    case Bytecode_Size_Specifier::U32:
                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::S64:
                    case Bytecode_Size_Specifier::U64:
                    {
                        LLVMValueRef result = LLVMBuildAdd(builder->llvm_builder,
                                                           lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    {
                        LLVMValueRef result = LLVMBuildFAdd(builder->llvm_builder,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::SUB:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

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
                        LLVMValueRef result = LLVMBuildSub(builder->llvm_builder,
                                                           lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    case Bytecode_Size_Specifier::R64:
                    {
                        LLVMValueRef result = LLVMBuildFSub(builder->llvm_builder,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }
                                            
            case Bytecode_Instruction::REM:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

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
                        LLVMValueRef result = LLVMBuildSRem(builder->llvm_builder,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::MUL:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

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
                        LLVMValueRef result = LLVMBuildMul(builder->llvm_builder,
                                                           lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    case Bytecode_Size_Specifier::R32:
                    case Bytecode_Size_Specifier::R64:
                    {
                        LLVMValueRef result = LLVMBuildFMul(builder->llvm_builder,
                                                            lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::DIV:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);

                auto lhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto rhs_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                LLVMValueRef lhs_val = llvm::wrap(builder->temps[lhs_idx]);
                LLVMValueRef rhs_val = llvm::wrap(builder->temps[rhs_idx]);

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
                        LLVMValueRef result = LLVMBuildSDiv(builder->llvm_builder,
                                                           lhs_val, rhs_val, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                    default: assert(false);
                }
                break;
            }

            case Bytecode_Instruction::JUMP:
            {
                auto block_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);

                assert(block_idx < func_context->llvm_blocks.count);
                llvm::BasicBlock *target_block = func_context->llvm_blocks[block_idx];

                builder->_llvm_builder->CreateBr(target_block);
                break;
            }

            case Bytecode_Instruction::JUMP_IF:
            {
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto then_block_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                         &func_context->ip);


#ifndef NDEBUG
                auto next_inst =
#endif
                    llvm_fetch_from_bytecode<Bytecode_Instruction>(func_context->bc_block,
                                                                   &func_context->ip);
#ifndef NDEBUG
                assert(next_inst == Bytecode_Instruction::JUMP);
#endif

                auto else_block_idx =
                    llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                       &func_context->ip);

                LLVMValueRef cond_val = llvm::wrap(builder->temps[val_idx]);

                assert(then_block_idx < func_context->llvm_blocks.count);
                llvm::BasicBlock *then_block = func_context->llvm_blocks[then_block_idx];

                assert(else_block_idx < func_context->llvm_blocks.count);
                llvm::BasicBlock *else_block = func_context->llvm_blocks[else_block_idx];


                builder->_llvm_builder->CreateCondBr(llvm::unwrap(cond_val), then_block,
                                                     else_block);
                // LLVMBuildCondBr(builder->llvm_builder, cond_val, then_block, else_block);
                break;
            }


            case Bytecode_Instruction::SWITCH:
            {
                /*auto type_idx =*/ llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);
                auto case_count = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                     &func_context->ip);
                uint32_t default_block_idx =
                        llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                           &func_context->ip);
                // case_count -= 1;

                auto switch_val = llvm::wrap(builder->temps[val_idx]);

                assert(default_block_idx < func_context->llvm_blocks.count);
                auto default_block = func_context->llvm_blocks[default_block_idx];


                auto switch_inst_val = LLVMBuildSwitch(builder->llvm_builder, switch_val,
                                                       llvm::wrap(default_block),
                                                       case_count);

                for (int64_t i = 0; i < case_count; i++)
                {
                    Const_Value case_value = llvm_load_int(func_context->bc_block,
                                                           &func_context->ip);

                    auto block_idx =
                        llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                           &func_context->ip);
                    LLVMValueRef llvm_on_val = llvm_const_int(case_value);
                    assert(block_idx < func_context->llvm_blocks.count);
                    llvm::BasicBlock *dest_block = func_context->llvm_blocks[block_idx];
                    LLVMAddCase(switch_inst_val, llvm_on_val, llvm::wrap(dest_block));
                }

                break;
            }

            case Bytecode_Instruction::CAST_INT:
            case Bytecode_Instruction::CAST_ENUM:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                auto val = llvm::wrap(builder->temps[val_idx]);
                assert(LLVMGetTypeKind(LLVMTypeOf(val)) == LLVMIntegerTypeKind);

                AST_Type *target_type = bytecode_type_from_size_spec(size_spec);
                assert(target_type);

                LLVMValueRef result = nullptr;

                bool is_signed = false;
                if (target_type->integer.sign) is_signed = true;

                llvm::Type *_llvm_dest_ty = llvm_type_from_ast(builder, target_type);
                LLVMTypeRef llvm_dest_ty = llvm::wrap(_llvm_dest_ty);

                if (target_type->kind == AST_Type_Kind::INTEGER)
                {
                    result = LLVMBuildIntCast2(builder->llvm_builder, val, llvm_dest_ty,
                                               is_signed, "");
                }
                else if (target_type->kind == AST_Type_Kind::FLOAT)
                {
                    if (is_signed)
                    {
                        result = LLVMBuildSIToFP(builder->llvm_builder, val, llvm_dest_ty, "");
                    }
                    else
                    {
                        result = LLVMBuildUIToFP(builder->llvm_builder, val, llvm_dest_ty, "");
                    }
                }
                else assert(false);

                assert(result);
                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::CAST_FLOAT:
            {
                auto size_spec =
                    llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(func_context->bc_block,
                                                                      &func_context->ip);
                auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                  &func_context->ip);

                auto val = llvm::wrap(builder->temps[val_idx]);
#ifndef NDEBUG
                LLVMTypeKind tk = LLVMGetTypeKind(LLVMTypeOf(val));
                assert(tk == LLVMFloatTypeKind ||
                       tk == LLVMDoubleTypeKind);
#endif

                switch (size_spec)
                {
                    case Bytecode_Size_Specifier::INVALID: assert(false);
                    case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
                    case Bytecode_Size_Specifier::FLOAT_FLAG: assert(false);
                    case Bytecode_Size_Specifier::BIT_SIZE_MASK: assert(false);

                    case Bytecode_Size_Specifier::U8:  assert(false);
                    case Bytecode_Size_Specifier::S8:  assert(false);
                    case Bytecode_Size_Specifier::U16: assert(false);
                    case Bytecode_Size_Specifier::S16: assert(false);
                    case Bytecode_Size_Specifier::U32: assert(false);
                    case Bytecode_Size_Specifier::U64: assert(false);

                    case Bytecode_Size_Specifier::S32:
                    case Bytecode_Size_Specifier::S64:
                    {
                        auto dest_ty = bytecode_type_from_size_spec(size_spec);
                        llvm::Type *_llvm_dest_ty = llvm_type_from_ast(builder, dest_ty);
                        auto llvm_dest_ty = llvm::wrap(_llvm_dest_ty);
                        LLVMValueRef result = LLVMBuildFPToSI(builder->llvm_builder, val,
                                                              llvm_dest_ty, "");
                        llvm_push_temporary(builder, result);
                        break;
                    }
                                                       
                    case Bytecode_Size_Specifier::R32: assert(false);
                    case Bytecode_Size_Specifier::R64: assert(false);

                }

                break;

            }

            case Bytecode_Instruction::SYSCALL:
            {
                auto arg_count = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                llvm_emit_syscall(builder, arg_count);                
                break;
            }

            case Bytecode_Instruction::AGG_OFFSET_PTR:
            {
                auto store_kind =
                    llvm_fetch_from_bytecode<Bytecode_Value_Type_Specifier>(
                            func_context->bc_block,
                            &func_context->ip);

                assert(store_kind == Bytecode_Value_Type_Specifier::ALLOCL ||
                       store_kind == Bytecode_Value_Type_Specifier::PARAMETER ||
                       store_kind == Bytecode_Value_Type_Specifier::TEMPORARY);

                auto store_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                auto offset_val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                         &func_context->ip);

                LLVMValueRef store_val = nullptr;
                switch (store_kind)
                {
                    case Bytecode_Value_Type_Specifier::INVALID: assert(false);

                    case Bytecode_Value_Type_Specifier::ALLOCL:
                    {
                        store_val = llvm::wrap(builder->allocas[store_idx]);
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::PARAMETER:
                    {
                        store_val = llvm::wrap(builder->params[store_idx]);
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::TEMPORARY:
                    {
                        store_val = llvm::wrap(builder->temps[store_idx]);
                        break;
                    }
                }
                assert(store_val);

                llvm::Type *llvm_idx_type = llvm_type_from_ast(builder, Builtin::type_u32);
                LLVMValueRef llvm_offset_val = llvm::wrap(builder->temps[offset_val_idx]);
                assert(LLVMTypeOf(llvm_offset_val) == llvm::wrap(llvm_idx_type));

                LLVMValueRef zero_val = LLVMConstNull(llvm::wrap(llvm_idx_type));
                //LLVMValueRef llvm_offset_val = LLVMConstInt(llvm_idx_type, offset_val, true);
                LLVMValueRef indices[] = { zero_val, llvm_offset_val };


                LLVMValueRef result = LLVMBuildGEP(builder->llvm_builder, store_val,
                                                   indices, 2, "");

                llvm_push_temporary(builder, result);
                break;
            }

            case Bytecode_Instruction::ARR_OFFSET_PTR:
            {
                auto store_kind =
                    llvm_fetch_from_bytecode<Bytecode_Value_Type_Specifier>(
                            func_context->bc_block,
                            &func_context->ip);

                assert(store_kind == Bytecode_Value_Type_Specifier::ALLOCL ||
                       store_kind == Bytecode_Value_Type_Specifier::PARAMETER ||
                       store_kind == Bytecode_Value_Type_Specifier::TEMPORARY);

                auto store_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                    &func_context->ip);
                auto offset_val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                                         &func_context->ip);

                bool is_array = false;

                LLVMValueRef store_val = nullptr;
                switch (store_kind)
                {
                    case Bytecode_Value_Type_Specifier::INVALID: assert(false); 

                    case Bytecode_Value_Type_Specifier::ALLOCL:
                    {
                        store_val = llvm::wrap(builder->allocas[store_idx]);
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::PARAMETER:
                    {
                        store_val = llvm::wrap(builder->params[store_idx]);
                        store_val = LLVMBuildLoad(builder->llvm_builder, store_val, "");
                        break;
                    }

                    case Bytecode_Value_Type_Specifier::TEMPORARY:
                    {
                        store_val = llvm::wrap(builder->temps[store_idx]);
                        break;
                    }
                }

                assert(store_val);

                LLVMTypeRef store_type = LLVMTypeOf(store_val);
                assert(LLVMGetTypeKind(store_type) == LLVMPointerTypeKind);
                store_type = LLVMGetElementType(store_type);
                LLVMTypeKind store_type_kind = LLVMGetTypeKind(store_type);
                if (store_type_kind == LLVMArrayTypeKind)
                {
                    is_array = true; 
                }

                LLVMValueRef llvm_offset_val = llvm::wrap(builder->temps[offset_val_idx]);
                llvm::Type *llvm_idx_type = llvm_type_from_ast(builder, Builtin::type_u32);
                assert(LLVMTypeOf(llvm_offset_val) == llvm::wrap(llvm_idx_type));

                LLVMValueRef llvm_zero_val = LLVMConstNull(llvm::wrap(llvm_idx_type));

                LLVMValueRef indices[2];
                unsigned index_count = 0;
                if (is_array)
                {
                    indices[0] = llvm_zero_val;
                    indices[1] = llvm_offset_val;
                    index_count = 2;
                }
                else
                {
                    indices[0] = llvm_offset_val;
                    index_count = 1;
                }

                LLVMValueRef result = LLVMBuildGEP(builder->llvm_builder, store_val,
                                                   indices, index_count, "");
                llvm_push_temporary(builder, result);
                break;
            }
        }
    }

    LLVMValueRef llvm_emit_constant(LLVM_Builder *builder, Bytecode_Value *value)
    {
        assert(value->kind == Bytecode_Value_Kind::GLOBAL);

        auto type = value->type;

        switch (type->kind)
        {
            case AST_Type_Kind::INTEGER:
            {
                llvm::Type *llvm_type = llvm_type_from_ast(builder, type);
                return LLVMConstInt(llvm::wrap(llvm_type), value->value.integer.s64,
                                     type->integer.sign);
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void llvm_emit_exit(LLVM_Builder *builder, LLVM_Function_Context *func_context)
    {
        auto val_idx = llvm_fetch_from_bytecode<uint32_t>(func_context->bc_block,
                                                          &func_context->ip);
        LLVMValueRef val = llvm::wrap(builder->temps[val_idx]);

        switch (builder->target_platform)
        {
            case Zodiac_Target_Platform::INVALID: assert(false);

            case Zodiac_Target_Platform::LINUX:
            {
                auto asm_string = string_ref("syscall");

                String_Builder sb = {};
                string_builder_init(builder->allocator, &sb);

                string_builder_append(&sb, "=r,{rax},{rdi}");

                llvm::Type *asm_fn_type = llvm_asm_function_type(builder, 2);

                auto constraint_string = string_builder_to_string(sb.allocator, &sb);
                string_builder_free(&sb);

                LLVMValueRef asm_val = LLVMGetInlineAsm(llvm::wrap(asm_fn_type),
                                                        asm_string.data,
                                                        asm_string.length,
                                                        constraint_string.data,
                                                        constraint_string.length,
                                                        true,
                                                        false,
                                                        LLVMInlineAsmDialectATT);

                llvm::Type *arg_type = llvm_type_from_ast(builder, Builtin::type_s64);
                LLVMValueRef syscall_num = LLVMConstInt(llvm::wrap(arg_type), 60, true);
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

    void llvm_emit_syscall(LLVM_Builder *builder, int32_t arg_count)
    {
        assert(arg_count >= 1 && arg_count <= 7);

        auto asm_string = string_ref("syscall");

        String_Builder sb = {};
        string_builder_init(builder->allocator, &sb);

        string_builder_append(&sb, "=r,{rax}");
    
        if (arg_count >= 2) string_builder_append(&sb, ",{rdi}");
        if (arg_count >= 3) string_builder_append(&sb, ",{rsi}");
        if (arg_count >= 4) string_builder_append(&sb, ",{rdx}");
        if (arg_count >= 5) string_builder_append(&sb, ",{r10}");
        if (arg_count >= 6) string_builder_append(&sb, ",{r9}");
        if (arg_count >= 7) string_builder_append(&sb, ",{r8}");

        llvm::Type *asm_fn_type = llvm_asm_function_type(builder, arg_count);

        Array<LLVMValueRef> llvm_args = {};
        array_init(builder->allocator, &llvm_args, arg_count);
        for (int64_t i = 0 ; i < arg_count; i++)
        {
            llvm::Value *arg_val = stack_peek(&builder->arg_stack, (arg_count - 1) - i);

            llvm::Type *arg_type = arg_val->getType();
            llvm::Type *dest_type = llvm_type_from_ast(builder, Builtin::type_s64);

            if (arg_type != dest_type)
            {
                // LLVMTypeKind type_kind = LLVMGetTypeKind(arg_type);
                // switch (arg_type->getKind())
                // {
                //     case LLVMPointerTypeKind:
                //     {
                //         arg_val = llvm::unwrap(LLVMBuildPtrToInt(builder->llvm_builder,
                //                                                  llvm::wrap(arg_val),
                //                                                  dest_type, ""));
                //         break;
                //     }

                //     default: assert(false); 
                // }
                if (arg_type->isPointerTy())
                {
                    arg_val = llvm::unwrap(LLVMBuildPtrToInt(builder->llvm_builder,
                                                             llvm::wrap(arg_val),
                                                             llvm::wrap(dest_type),
                                                             ""));
                }
                else
                {
                    assert(false);
                }

            }
            array_append(&llvm_args, llvm::wrap(arg_val));
        }

        for (int64_t i = 0; i < arg_count; i++) stack_pop(&builder->arg_stack);

        auto constraint_str = string_builder_to_string(builder->allocator, &sb);

        LLVMValueRef asm_val = LLVMGetInlineAsm(llvm::wrap(asm_fn_type),
                                                (char*)asm_string.data,
                                                asm_string.length,
                                                (char*)constraint_str.data,
                                                constraint_str.length,
                                                true,
                                                false,
                                                LLVMInlineAsmDialectATT);

#ifndef NDEBUG
        LLVMValueRef result =
#endif
            LLVMBuildCall(builder->llvm_builder, asm_val, llvm_args.data,
                          llvm_args.count, "");
        
        assert(result);

        free(builder->allocator, constraint_str.data);

        array_free(&llvm_args);


        string_builder_free(&sb);
    }

    void llvm_push_temporary(LLVM_Builder *builder, LLVMValueRef temp_val)
    {
        assert(builder);
        assert(temp_val);

        array_append(&builder->temps, llvm::unwrap(temp_val));
    }

    llvm::Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        assert(builder);
        assert(ast_type);

        assert(ast_type->flags & AST_NODE_FLAG_RESOLVED_ID);
        assert(ast_type->flags & AST_NODE_FLAG_TYPED);
        assert(ast_type->flags & AST_NODE_FLAG_SIZED);

        auto &c = *builder->llvm_context;

        switch (ast_type->kind)
        {
            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::VOID:
            {
                return llvm::Type::getVoidTy(c);
                break;
            }

            case AST_Type_Kind::INTEGER:
            {
                return llvm::Type::getIntNTy(c, ast_type->bit_size);
                break;
            }

            case AST_Type_Kind::FLOAT:
            {
                if (ast_type->bit_size == 32)
                    return llvm::Type::getFloatTy(c);
                else if (ast_type->bit_size == 64)
                    return llvm::Type::getDoubleTy(c);
                break;
            }

            case AST_Type_Kind::BOOL:
            {
                return llvm::Type::getIntNTy(c, 1);
                break;
            }

            case AST_Type_Kind::POINTER:
            {
                llvm::Type *base_type = llvm_type_from_ast(builder, ast_type->pointer.base);
                return base_type->getPointerTo();
                break;
            }

            case AST_Type_Kind::FUNCTION:
            {
                llvm::Type *llvm_ret_type = llvm_type_from_ast(builder,
                                                               ast_type->function.return_type);
                Array<llvm::Type *> llvm_arg_types = {};
                if (ast_type->function.param_types.count)
                {
                    array_init(builder->allocator, &llvm_arg_types, 
                               ast_type->function.param_types.count);
                    for (int64_t i = 0; i < ast_type->function.param_types.count; i++)
                    {
                        llvm::Type *arg_type =
                            llvm_type_from_ast(builder, ast_type->function.param_types[i]);
                        array_append(&llvm_arg_types, arg_type);
                    }
                }

                bool is_vararg = false;

                llvm::Type *result = llvm::FunctionType::get(llvm_ret_type,
                                                             { llvm_arg_types.data,
                                                               (size_t)llvm_arg_types.count },
                                                             is_vararg);

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
                llvm::StructType *result =
                    llvm::unwrap(builder->llvm_module)->getTypeByName(name.data);
                if (result)
                {
                    assert(result->isStructTy());
                }
                else
                {
                    result = llvm::StructType::create(c, name.data);

                    auto ast_mem_types = ast_type->structure.member_types;
                    assert(ast_mem_types.count);

                    Array<llvm::Type *> mem_types = {};
                    array_init(builder->allocator, &mem_types, ast_mem_types.count);
                    for (int64_t i = 0; i < ast_mem_types.count; i++)
                    {
                        array_append(&mem_types, llvm_type_from_ast(builder, ast_mem_types[i]));
                    }
                    
                    result->setBody({ mem_types.data, (size_t)mem_types.count }, false);
                }

                return result;
                break;
            }

            case AST_Type_Kind::ENUM:
            {
                return llvm_type_from_ast(builder, ast_type->enum_type.base_type);
                break;
            }

            case AST_Type_Kind::ARRAY:
            {
                llvm::Type *llvm_elem_type = llvm_type_from_ast(builder,
                                                                ast_type->array.element_type);
                return llvm::ArrayType::get(llvm_elem_type,
                                            ast_type->array.element_count);
                break;
            }
        }

        assert(false);
        return {};
    }

    llvm::FunctionType *llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count)
    {
        assert(builder);
        assert(arg_count >= 0);
        assert(arg_count < 7);

        auto ret_type = llvm_type_from_ast(builder, Builtin::type_s64);
        llvm::Type *param_types[7] = { ret_type, ret_type, ret_type, ret_type,
                                       ret_type, ret_type, ret_type };
        return llvm::FunctionType::get(ret_type, { param_types, (size_t)arg_count },
                                       false);
    }

    bool llvm_block_ends_with_terminator(llvm::BasicBlock *llvm_block)
    {
        assert(llvm_block);

        if (llvm_block->getTerminator() == nullptr) return false;
        
        return true;
    }

    LLVM_Function_Context llvm_create_function_context(llvm::Function *llvm_func,
                                                       Bytecode_Function *bc_func,
                                                       llvm::BasicBlock *llvm_block,
                                                       Bytecode_Block *bc_block,
                                                       Array<llvm::BasicBlock *> llvm_blocks)
    {
        LLVM_Function_Context result = {};
        result.llvm_function = llvm_func;
        result.llvm_block = llvm_block;
        result.bc_func = bc_func;
        result.bc_block = bc_block;
        result.llvm_blocks = llvm_blocks;
        result.ip = 0;

        return result;
    }

    Const_Value llvm_load_int(Bytecode_Block *block, int64_t *ipp)
    {
        auto size_spec =
            llvm_fetch_from_bytecode<Bytecode_Size_Specifier>(block, ipp);
        Const_Value result = {};
        result.type = bytecode_type_from_size_spec(size_spec);
        assert(result.type->kind == AST_Type_Kind::INTEGER);

        switch (size_spec)
        {
           case Bytecode_Size_Specifier::INVALID: assert(false);
           case Bytecode_Size_Specifier::SIGN_FLAG: assert(false);
           case Bytecode_Size_Specifier::FLOAT_FLAG: assert(false);
           case Bytecode_Size_Specifier::BIT_SIZE_MASK: assert(false);

           case Bytecode_Size_Specifier::U8:
           case Bytecode_Size_Specifier::S8:
           {
                result.integer.u8 = llvm_fetch_from_bytecode<uint8_t>(block, ipp);
                break;
           }

           case Bytecode_Size_Specifier::U16: assert(false);
           case Bytecode_Size_Specifier::S16: assert(false);
           {
                result.integer.u16 = llvm_fetch_from_bytecode<uint16_t>(block, ipp);
                break;
           }

           case Bytecode_Size_Specifier::U32:
           case Bytecode_Size_Specifier::S32:
           {
                result.integer.u32 = llvm_fetch_from_bytecode<uint32_t>(block, ipp);
                break;
           }

           case Bytecode_Size_Specifier::R32: assert(false);

           case Bytecode_Size_Specifier::U64:
           case Bytecode_Size_Specifier::S64:
           {
                result.integer.u64 = llvm_fetch_from_bytecode<uint64_t>(block, ipp);
                break;
           }

           case Bytecode_Size_Specifier::R64: assert(false);
        }

        return result;
    }

    LLVMValueRef llvm_const_int(Const_Value cv)
    {
        assert(cv.type->kind == AST_Type_Kind::INTEGER);

        LLVMTypeRef llvm_type = LLVMIntType(cv.type->bit_size);

        LLVMValueRef result = nullptr;

        switch (cv.type->bit_size)
        {
            case 8:
            {
                result = LLVMConstInt(llvm_type, cv.integer.u8, cv.type->integer.sign);
                break;
            }

            case 16:
            {
                result = LLVMConstInt(llvm_type, cv.integer.u16, cv.type->integer.sign);
                break;
            }

            case 32:
            {
                result = LLVMConstInt(llvm_type, cv.integer.u32, cv.type->integer.sign);
                break;
            }

            case 64:
            {
                result = LLVMConstInt(llvm_type, cv.integer.u64, cv.type->integer.sign);
                break;
            }

        }

        assert(result);
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
