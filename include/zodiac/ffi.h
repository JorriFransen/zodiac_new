#pragma once

#include "ast.h"

#include <dyncall_callback.h>
#include <dyncall.h>
#include <dynload.h>

namespace Zodiac
{
    struct FFI_Function_Data
    {
        DCpointer c_fn_ptr = nullptr;
        void *interpreter = nullptr;
    };

    typedef DCCallbackHandler FFI_Callback_Handler;

    struct FFI_Context
    {
        Allocator *allocator = nullptr;

        DCCallVM *dc_vm = nullptr;

        FFI_Callback_Handler *callback_handler = nullptr;

        Array<DLLib *> libs = {};
    };

    FFI_Context ffi_create(Allocator *allocator, Build_Data *build_data,
                           FFI_Callback_Handler *callback_handler);

    /////////////////////////////////////////////////////////////////
    ///// Symbol loading interface //////////////////////////////////
    /////////////////////////////////////////////////////////////////
    bool ffi_load_function(FFI_Context *ffi, const Atom &fn_name, AST_Type *fn_type,
                           FFI_Function_Data *fn_data);

    /////////////////////////////////////////////////////////////////
    ///// Call interface ////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
    void ffi_call(FFI_Context *ffi, void *fn_ptr, void *return_val_ptr,
                  AST_Type *return_type);
    void ffi_reset(FFI_Context *ffi);
    void ffi_push_arg(FFI_Context *ffi, void *arg_ptr, AST_Type *type);

    /////////////////////////////////////////////////////////////////
    ///// Callback interface ////////////////////////////////////////
    /////////////////////////////////////////////////////////////////
    void ffi_create_callback(FFI_Context *ffi, FFI_Function_Data *func, AST_Type *fn_type);
    char ffi_dcb_type_sig_char(AST_Type *type);
    String ffi_dcb_func_sig(Allocator *allocator, AST_Type *func_type);
}
