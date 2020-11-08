#pragma once

#include "ast.h"
#include "bytecode.h"

#include <bits/stdint-uintn.h>

namespace Zodiac
{

    struct FFI_Context
    {

    };

    void ffi_call(FFI_Context *ffi, Bytecode_Function *func, uint8_t *return_val_ptr,
                  AST_Type *return_type);
    void ffi_push_arg(FFI_Context *ffi, uint8_t *arg_ptr, AST_Type *type);
}
