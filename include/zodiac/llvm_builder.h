#pragma once

#include "bc.h"
#include "build_data.h"
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
        BC_Function *bytecode_function = nullptr;
        llvm::Function *llvm_function = nullptr;
    };

    struct LLVM_Block_Info
    {
        BC_Block *bytecode_block = nullptr;
        llvm::BasicBlock *llvm_block = nullptr;
    };

    struct LLVM_Struct_Type_Info
    {
        AST_Type *ast_type = nullptr;
        llvm::StructType *llvm_type = nullptr;
    };

    struct LLVM_String_Literal
    {
        BC_Value *bc_value = nullptr;
        llvm::GlobalValue *llvm_global = nullptr;
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
        llvm::DataLayout *llvm_datalayout = nullptr;

        Array<LLVM_Struct_Type_Info> struct_types_to_finalize = {};
        Array<LLVM_Struct_Type_Info> union_types_to_finalize = {};

        Array<LLVM_Function_Info> registered_functions = {};
        Array<llvm::GlobalVariable *> globals = {};
        Array<LLVM_String_Literal> string_literals = {};

        Stack<llvm::Value *> arg_stack = {};

        // For the function currently being emitted
        Array<LLVM_Block_Info > blocks = {};
        Array<llvm::AllocaInst *> parameters = {};
        Array<llvm::AllocaInst *> locals = {};
        Array<llvm::Value *> temps = {};
    };

    LLVM_Builder llvm_builder_create(Allocator *allocator, Build_Data *build_data);

    void llvm_register_function(LLVM_Builder *builder, BC_Function *bc_func);
    void llvm_emit_function(LLVM_Builder *builder, BC_Function *bc_func);
    void llvm_emit_global(LLVM_Builder *builder, BC_Global_Info global_info);
    llvm::Constant *llvm_emit_constant(LLVM_Builder *builder, Const_Value const_val);

    void llvm_emit_instruction(LLVM_Builder *builder, BC_Instruction *inst);
    llvm::Value *llvm_emit_value(LLVM_Builder *builder, BC_Value *bc_value);

    void llvm_emit_exit(LLVM_Builder *builder, llvm::Value *exit_code_val);
    llvm::Value *llvm_emit_syscall(LLVM_Builder *builder, uint64_t arg_count);

    template <typename T>
    T* llvm_emit_value(LLVM_Builder *builder, BC_Value *bc_value)
    {
        return static_cast<T*>(llvm_emit_value(builder, bc_value));
    }

    bool llvm_emit_binary(LLVM_Builder *builder, const char * output_file_name);
    bool llvm_run_linker(LLVM_Builder *builder, const char *output_file_name);

    llvm::Function *llvm_find_function(LLVM_Builder *builder, BC_Function *bc_func);
    llvm::BasicBlock *llvm_find_block(LLVM_Builder *builder, BC_Block *bc_block);

    llvm::Type *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type);

    template <typename T>
    T *llvm_type_from_ast(LLVM_Builder *builder, AST_Type *ast_type)
    {
        return static_cast<T *>(llvm_type_from_ast(builder, ast_type));
    }

    void llvm_finalize_struct_type(LLVM_Builder *builder, llvm::StructType *llvm_type,
                                   AST_Type *ast_type);
    void llvm_finalize_union_type(LLVM_Builder *builder, llvm::StructType *llvm_type,
                                  AST_Type *ast_type);

    llvm::FunctionType *llvm_asm_function_type(LLVM_Builder *builder, int64_t arg_count);

    bool llvm_ready_to_emit(LLVM_Builder *builder);


}
