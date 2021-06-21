#pragma once

#include "ast.h"
#include "bytecode.h"

#include <dyncall.h>
#include <dynload.h>

namespace Zodiac
{
    struct FFI_Context
    {
        Allocator *allocator = nullptr;

        DCCallVM *dc_vm = nullptr;

        Hash_Table<Atom, DCpointer> foreign_functions = {};

        Array<DLLib *> libs = {};
    };

    FFI_Context ffi_create(Allocator *allocator, Build_Data *build_data);

    bool ffi_load_function(FFI_Context *ffi, const Atom &name);

    void ffi_call(FFI_Context *ffi, void *fn_ptr, void *return_val_ptr,
                  AST_Type *return_type);
    void ffi_call(FFI_Context *ffi, const Atom &name, void *return_val_ptr,
                  AST_Type *return_type);

    void *ffi_find_function(FFI_Context *ffi, const Atom &name, bool *found);

    void ffi_reset(FFI_Context *ffi);
    void ffi_push_arg(FFI_Context *ffi, void *arg_ptr, AST_Type *type);

    char ffi_dcb_type_sig_char(AST_Type *type);
    String ffi_dcb_func_sig(Allocator *allocator, AST_Type *func_type);
}
