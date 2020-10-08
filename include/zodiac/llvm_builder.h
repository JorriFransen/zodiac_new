#pragma once

#include "allocator.h"
#include "bytecode.h"

#include <llvm-c/Core.h>

#include <llvm/IR/IRBuilder.h>

namespace Zodiac
{
    struct LLVM_Function
    {
        llvm::Function *llvm_func = nullptr;
        Bytecode_Function *bc_func = nullptr;
    };

    enum class Zodiac_Target_Platform
    {
        INVALID,
        LINUX,
        WINDOWS,
    };

    struct LLVM_Builder
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;

        LLVMModuleRef llvm_module;
        LLVMBuilderRef llvm_builder;

        llvm::LLVMContext *llvm_context = nullptr;
        llvm::IRBuilder<> *_llvm_builder = nullptr;

        Array<LLVM_Function> functions = {};

        Array<llvm::Value *> temps = {};
        Array<llvm::AllocaInst *> allocas = {};
        Array<llvm::AllocaInst *> params = {};
        Array<llvm::Value *> globals = {};

        Stack<llvm::Value *> arg_stack = {};

        String target_triple = {};
        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;

        Bytecode_Program *bc_program = {};
    };

    struct LLVM_Function_Context
    {
        llvm::Function *llvm_function = nullptr;
        llvm::BasicBlock* llvm_block = {};

        Bytecode_Function *bc_func = nullptr;
        Bytecode_Block *bc_block = nullptr;

        Array<llvm::BasicBlock *> llvm_blocks = {};

        int64_t ip = 0;
    };

    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder,
                           Build_Data *build_data, Bytecode_Program *bc_program);

    bool llvm_emit_binary(LLVM_Builder *builder, const char *output_file_name);
    bool llvm_run_linker(LLVM_Builder *builder, const char *output_file_name);

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Global bc_glob);
    void llvm_emit_block(LLVM_Builder *builder, LLVM_Function_Context *func_context);
    void llvm_emit_instruction(LLVM_Builder *builder, Bytecode_Instruction inst,
                               LLVM_Function_Context *func_context);

    LLVMValueRef llvm_emit_constant(LLVM_Builder *builder, Bytecode_Value *value);
    void llvm_emit_exit(LLVM_Builder *builder, LLVM_Function_Context *func_context);
    void llvm_emit_syscall(LLVM_Builder *builder, int32_t arg_count);

    void llvm_push_temporary(LLVM_Builder *builder, LLVMValueRef temp_val);

    llvm::Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type);

    template <typename Type>
    Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        return static_cast<Type *>(llvm_type_from_ast(builder, ast_type));
    }


    llvm::FunctionType *llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count);

    bool llvm_block_ends_with_terminator(llvm::BasicBlock *llvm_block);

    LLVM_Function_Context llvm_create_function_context(llvm::Function *llvm_func,
                                                       Bytecode_Function *bc_func,
                                                       llvm::BasicBlock *llvm_block,
                                                       Bytecode_Block *bc_block,
                                                       Array<llvm::BasicBlock *> llvm_blocks);

    Const_Value llvm_load_int(Bytecode_Block *block, int64_t *ipp);
    LLVMValueRef llvm_const_int(Const_Value cv);

    template <typename T> 
    T llvm_fetch_from_bytecode(Bytecode_Block *block, int64_t *ipp)
    {
        T result = *((T*)&block->instructions[*ipp]);
        *ipp += sizeof(T);
        return result;
    }

    void llvm_print(Allocator *allocator, LLVM_Builder *builder);
}
