#include "ffi.h"

#include "builtin.h"
#include "string_builder.h"
#include "temp_allocator.h"

#include <cstdio>

namespace Zodiac {

    bool __bytecode_fn_ptr_eq(BC_Function *const &a, BC_Function *const &b) {
        return a == b;
    }

    FFI_Context ffi_create(Allocator *allocator, Build_Data *build_data,
                           FFI_Callback_Handler *callback_handler)
    {
        FFI_Context result = {};

        result.allocator = allocator;

        array_init(allocator, &result.libs);

        result.dc_vm = dcNewCallVM(4096);
        dcMode(result.dc_vm, DC_CALL_C_DEFAULT);
        dcReset(result.dc_vm);

        result.callback_handler = callback_handler;

        DLLib *this_exe_lib = dlLoadLibrary(nullptr);
        assert(this_exe_lib);
        array_append(&result.libs, this_exe_lib);

#if _WIN32
        DLLib *kernel32_lib = dlLoadLibrary("kernel32.dll");
        assert(kernel32_lib);
        array_append(&result.libs, kernel32_lib);

        DLLib *ucrtbase_lib = dlLoadLibrary("ucrtbase.dll");
        assert(ucrtbase_lib);
        array_append(&result.libs, ucrtbase_lib);

        if (build_data->options->link_c) {
            DLLib* msvcrt_lib = dlLoadLibrary("msvcrt.dll");
            assert(msvcrt_lib);
            array_append(&result.libs, msvcrt_lib);
        }
#endif


        return result;
    }

    bool ffi_load_function(FFI_Context *ffi, const Atom &fn_name, FFI_Function_Data *fn_data)
    {
        assert(fn_data);

        DCpointer symbol = nullptr;
        for (int64_t i = 0; i < ffi->libs.count; i++) {
            auto sym = dlFindSymbol(ffi->libs[i], fn_name.data);
            if (sym) {
                symbol = sym;
                break;
            }
        }

        if (symbol) {
            // printf("Loaded symbol '%s': %p\n", fn_name.data, symbol);
            fn_data->c_fn_ptr = symbol;
            return true;
        }

        return false;
    }

    void ffi_call(FFI_Context *ffi, void *fn_ptr, void *return_val_ptr,
                  AST_Type *return_type)
    {
        switch (return_type->kind) {
            default: assert(false);

            case AST_Type_Kind::VOID: {
                assert(!return_val_ptr);
                dcCallVoid(ffi->dc_vm, fn_ptr);
                break;
            }

            case AST_Type_Kind::INTEGER: {
                if (return_type->integer.sign) {
                    switch (return_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);

                        case 32: {
                            assert(sizeof(int) == 4);
                            assert(sizeof(DCint) == 4);
                            int32_t result = dcCallInt(ffi->dc_vm, fn_ptr);
                            if (return_val_ptr) *(int32_t*)return_val_ptr = result;
                            break;
                        }

                        case 64: {
                            assert(sizeof(DClonglong) == 8);
                            int64_t result = dcCallLongLong(ffi->dc_vm, fn_ptr);
                            if (return_val_ptr) *(int64_t*)return_val_ptr = result;
                            break;
                        }

                    }
                } else {
                    switch (return_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);

                        case 32: {
                            assert(sizeof(int) == 4);
                            assert(sizeof(DCint) == 4);
                            int32_t result = dcCallInt(ffi->dc_vm, fn_ptr);
                            if (return_val_ptr) *(uint32_t*)return_val_ptr = result;
                            break;
                        }

                        case 64: {
                            assert(sizeof(DClonglong) == 8);
                            int64_t result = dcCallLongLong(ffi->dc_vm, fn_ptr);
                            if (return_val_ptr) *(uint64_t*)return_val_ptr = result;
                            break;
                        }

                    }
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                void *result = dcCallPointer(ffi->dc_vm, fn_ptr);
                if (return_val_ptr) *(void**)return_val_ptr = result;
                break;
            }
        }
    }

    void ffi_reset(FFI_Context *ffi)
    {
        dcMode(ffi->dc_vm, DC_CALL_C_DEFAULT);
        dcReset(ffi->dc_vm);
    }

    void ffi_push_arg(FFI_Context *ffi, void *arg_ptr, AST_Type *type)
    {
        switch (type->kind) {
            default: assert(false);

            case AST_Type_Kind::INTEGER: {
                switch (type->bit_size) {
                    default: assert(false);
                    case 32: {
                         assert(sizeof(DCint) == 4);
                         dcArgInt(ffi->dc_vm, *((DCint *)arg_ptr));
                         break;
                     }

                    case 64: {
                         assert(sizeof(DClonglong) == 8);
                         dcArgLongLong(ffi->dc_vm, *((DClonglong *)arg_ptr));
                         break;
                     }
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                dcArgPointer(ffi->dc_vm, *(void**)arg_ptr);
                break;
            }
        }
    }

    void *ffi_create_callback(FFI_Context *ffi, FFI_Function_Data *func,
                              AST_Type *fn_type)
    {
        assert(!func->c_fn_ptr);
        assert(fn_type->kind == AST_Type_Kind::FUNCTION);
        assert(ffi->callback_handler);

        // @TODO: @CLEANUP: Can we safely free this string?
        auto sig = ffi_dcb_func_sig(ffi->allocator, fn_type);

        auto callback_ptr = dcbNewCallback(sig.data, ffi->callback_handler, func);
        assert(callback_ptr);

        func->c_fn_ptr = callback_ptr;

        return callback_ptr;
    }

    char ffi_dcb_type_sig_char(AST_Type *type)
    {
        char result = 0;

        switch (type->kind) {

            case AST_Type_Kind::INVALID: assert(false);

            case AST_Type_Kind::VOID: result = 'v'; break;

            case AST_Type_Kind::INTEGER: {
                if (type->integer.sign) {
                    switch (type->bit_size) {
                        default: assert(false);
                        case 8:  result = 'c'; break;
                        case 16: result = 's'; break;
                        case 32: result = 'i'; break;
                        case 64: result = 'l'; break;
                    }
                } else {
                    switch (type->bit_size) {
                        default: assert(false);
                        case 8:  result = 'C'; break;
                        case 16: result = 'S'; break;
                        case 32: result = 'I'; break;
                        case 64: result = 'L'; break;
                    }
                }

                break;
            }

            case AST_Type_Kind::FLOAT: {
                if (type == Builtin::type_float) {
                    result = 'f';
                } else if (type == Builtin::type_double) {
                    result = 'd';
                } else { assert(false); }
                break;
            }

            case AST_Type_Kind::BOOL: result = 'B'; break;

            case AST_Type_Kind::POINTER: {
                if (type == Builtin::type_ptr_u8) {
                    result = 'Z';
                } else {
                    result = 'p';
                }
                break;
            }

            case AST_Type_Kind::FUNCTION: assert(false);
            case AST_Type_Kind::STRUCTURE: assert(false);
            case AST_Type_Kind::UNION: assert(false);
            case AST_Type_Kind::ENUM: assert(false);
            case AST_Type_Kind::ARRAY: assert(false);

        }

        return result;
    }

    String ffi_dcb_func_sig(Allocator *allocator, AST_Type *func_type)
    {
        assert(func_type->kind == AST_Type_Kind::FUNCTION);

        auto ta = temp_allocator_get();
        temp_allocator_reset(ta);

        String_Builder _sb; auto sb = &_sb;
        string_builder_init(ta, sb);

        for (int64_t i = 0; i < func_type->function.param_types.count; i++) {
            auto param_type = func_type->function.param_types[i];
            char c = ffi_dcb_type_sig_char(param_type);
            string_builder_appendf(sb, "%c", c);
        }

        string_builder_append(sb, ")");

        char c = ffi_dcb_type_sig_char(func_type->function.return_type);
        string_builder_appendf(sb, "%c", c);

        return string_builder_to_string(allocator, sb);
    }
}
