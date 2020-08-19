#pragma once

#include "allocator.h"
#include "bytecode.h"

#include <llvm-c/Core.h>

namespace Zodiac
{
    struct LLVM_Function
    {
        LLVMValueRef llvm_func = {};
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
        LLVMModuleRef llvm_module;
        LLVMBuilderRef llvm_builder;

        Array<LLVM_Function> functions = {};

        Array<LLVMValueRef> temps = {};
        Array<LLVMValueRef> allocas = {};

        Stack<LLVMValueRef> arg_stack = {};

        String target_triple = {};
        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;
    };

    struct LLVM_Function_Context
    {
        LLVMValueRef llvm_function = {};
        LLVMBasicBlockRef llvm_block = {};

        Bytecode_Function *bc_func = nullptr;
        Bytecode_Block *bc_block = nullptr;

        int64_t ip = 0;
    };

    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder);

    bool llvm_emit_binary(LLVM_Builder *builder, const char *output_file_name);
    bool llvm_run_linker(Allocator *allocator, const char *output_file_name);

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_block(LLVM_Builder *builder, LLVM_Function_Context *func_context);
    void llvm_emit_instruction(LLVM_Builder *builder, Bytecode_Instruction inst,
                               LLVM_Function_Context *func_context);

    void llvm_emit_exit(LLVM_Builder *builder, LLVM_Function_Context *func_context);

    void llvm_push_temporary(LLVM_Builder *builder, LLVMValueRef temp_val);

    LLVMTypeRef llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type);
    LLVMTypeRef llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count);

    bool llvm_block_ends_with_terminator(LLVMBasicBlockRef llvm_block);

    LLVM_Function_Context llvm_create_function_context(LLVMValueRef llvm_func,
                                                       Bytecode_Function *bc_func,
                                                       LLVMBasicBlockRef llvm_bock,
                                                       Bytecode_Block *bc_block);

    template <typename T> 
    T llvm_fetch_from_bytecode(Bytecode_Block *block, int64_t *ipp)
    {
        T result = *((T*)&block->instructions[*ipp]);
        *ipp += sizeof(T);
        return result;
    }

    void llvm_print(Allocator *allocator, LLVM_Builder *builder);
}
