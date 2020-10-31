#pragma once

#include "build_data.h"
#include "bytecode.h"
#include "stack.h"

#include <llvm/IR/IRBuilder.h>

namespace Zodiac
{
    enum class Zodiac_Target_Platform
    {
        INVALID,
        LINUX,
        WINDOWS,
    };

    struct LLVM_Function_Info
    {
        Bytecode_Function *bytecode_function = nullptr;
        llvm::Function *llvm_function = nullptr;
    };

    struct LLVM_Builder
    {
        Allocator *allocator = nullptr;
        Build_Data *build_data = nullptr;
        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;
        String target_triple = {};

        llvm::LLVMContext *llvm_context = nullptr;
        llvm::Module *llvm_module = nullptr;
        llvm::IRBuilder<> *llvm_builder = nullptr;

        Array<LLVM_Function_Info> registered_functions = {};

        Stack<llvm::Value *> arg_stack = {};

        // For the function currently being emitted
        Array<llvm::AllocaInst *> parameters = {};
        Array<llvm::AllocaInst *> locals = {};
        Array<llvm::Value *> temps = {};
    };

    LLVM_Builder llvm_builder_create(Allocator *allocator, Build_Data *build_data);

    void llvm_register_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func);
    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Value *bc_val);

    void llvm_emit_block(LLVM_Builder *builder, Bytecode_Block *bc_block);
    void llvm_emit_instruction(LLVM_Builder *builder, Bytecode_Instruction *inst);
    llvm::Value *llvm_emit_value(LLVM_Builder *builder, Bytecode_Value *bc_value);

    void llvm_emit_exit(LLVM_Builder *builder, llvm::Value *exit_code_val);
    llvm::Value *llvm_emit_syscall(LLVM_Builder *builder, uint64_t arg_count);

    template <typename T>
    T* llvm_emit_value(LLVM_Builder *builder, Bytecode_Value *bc_value)
    {
        return static_cast<T*>(llvm_emit_value(builder, bc_value));
    }

    bool llvm_emit_binary(LLVM_Builder *builder, const char * output_file_name);
    bool llvm_run_linker(LLVM_Builder *builder, const char *output_file_name);

    llvm::Function *llvm_find_function(LLVM_Builder *builder, Bytecode_Function *bc_func);

    llvm::Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type);

    template <typename T>
    T *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        return static_cast<T *>(llvm_type_from_ast(builder, ast_type));
    }

    llvm::FunctionType *llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count);


}
