#include "llvm_builder.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/FileSystem.h>


namespace Zodiac
{
    LLVM_Builder llvm_builder_create(Allocator *allocator, Build_Data *build_data)
    {
        auto _target_triple = llvm::sys::getDefaultTargetTriple();
        auto target_triple = string_copy(allocator,
                                         _target_triple.data(), _target_triple.length());

        Zodiac_Target_Platform target_platform = Zodiac_Target_Platform::INVALID;

        if (string_contains(target_triple, "windows"))
        {
            target_platform = Zodiac_Target_Platform::WINDOWS;
        }
        else if (string_contains(target_triple, "linux"))
        {
            target_platform = Zodiac_Target_Platform::LINUX;
        }
        else assert(false);   

        LLVM_Builder result = {};
        result.build_data = build_data;
        result.target_platform = target_platform;

        return result;
    }

    void llvm_register_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        assert(false);
    }

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        assert(false);
    }

    void llvm_emit_global(LLVM_Builder *builder, Bytecode_Value *bc_val)
    {
        assert(false);
    }

    bool llvm_emit_binary(LLVM_Builder *builder, const char * out_file_name)
    {
        assert(false);
    }
}
