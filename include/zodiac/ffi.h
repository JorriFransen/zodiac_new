#pragma once

#include "ast.h"
#include "bytecode.h"

#include <dyncall.h>
#include <dynload.h>

#ifdef linux
#include <bits/stdint-uintn.h>
#endif

namespace Zodiac
{
    struct FFI_Context
    {
        Allocator *allocator = nullptr;

        DCCallVM *dc_vm = nullptr;

        Hash_Table<String, DCpointer> functions = {};

        Array<DLLib *> libs = {};
    };

    FFI_Context ffi_create(Allocator *allocator, Build_Data *build_data);

    bool ffi_load_function(FFI_Context *ffi, const String &name);

    void ffi_call(FFI_Context *ffi, const String &name, uint8_t *return_val_ptr,
                  AST_Type *return_type);
    void ffi_reset(FFI_Context *ffi);
    void ffi_push_arg(FFI_Context *ffi, uint8_t *arg_ptr, AST_Type *type);
}
