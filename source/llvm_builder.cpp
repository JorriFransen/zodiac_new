#include "llvm_builder.h"

#include <cassert>

namespace Zodiac
{
    void llvm_builder_init(Allocator *allocator, LLVM_Builder *llvm_builder)
    {
        assert(allocator);
        assert(llvm_builder);
    }

    void llvm_emit_function(LLVM_Builder *builder, Bytecode_Function *bc_func)
    {
        assert(builder);
        assert(bc_func);
        assert(false);
    }
}
