#include "ffi.h"
#include <cstdio>

namespace Zodiac {

    bool __bytecode_fn_ptr_eq(Bytecode_Function *const &a, Bytecode_Function *const &b) {
        return a == b;
    }

    FFI_Context ffi_create(Allocator *allocator)
    {
        FFI_Context result = {};

        result.allocator = allocator;

        array_init(allocator, &result.libs);

        result.dc_vm = dcNewCallVM(4096);
        dcMode(result.dc_vm, DC_CALL_C_DEFAULT);
        dcReset(result.dc_vm);

        hash_table_init(allocator, &result.functions, hash_table_strings_equal);

        DLLib *this_exe_lib = dlLoadLibrary(nullptr);
        array_append(&result.libs, this_exe_lib);

#if _WIN32
        DLLib *kernel32_lib = dlLoadLibrary("kernel32.dll");
        array_append(&result.libs, this_exe_lib);
#endif

        return result;
    }

    bool ffi_load_function(FFI_Context *ffi, const String &name)
    {
        DCpointer symbol = nullptr;
        for (int64_t i = 0; i < ffi->libs.count; i++) {
            auto sym = dlFindSymbol(ffi->libs[i], name.data);
            if (sym) {
                symbol = sym;
                break;
            }
        }

        if (symbol) {
            hash_table_add(&ffi->functions, name, symbol);
            return true;
        }
        return false;
    }

    void ffi_call(FFI_Context *ffi, const String &name, uint8_t *return_val_ptr,
                  AST_Type *return_type)
    {
        DCpointer func_ptr = nullptr;
        bool found = hash_table_find(&ffi->functions, name, &func_ptr);
        assert(found);

        assert(return_val_ptr);

        switch (return_type->kind) {
            default: assert(false);

            case AST_Type_Kind::INTEGER:
            {
                if (return_type->integer.sign) {
                    switch (return_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);
                        case 32: {
                            assert(sizeof(int) == 4);
                            assert(sizeof(DCint) == 4);
                            int result = dcCallInt(ffi->dc_vm, func_ptr);
                            *(int*)return_val_ptr = result;
                            break;
                        }
                        case 64: assert(false);
                    }
                } else {
                    assert(false);
                }
                break;
            }
        }
    }

    void ffi_reset(FFI_Context *ffi)
    {
        dcMode(ffi->dc_vm, DC_CALL_C_DEFAULT);
        dcReset(ffi->dc_vm);
    }

    void ffi_push_arg(FFI_Context *ffi, uint8_t *arg_ptr, AST_Type *type)
    {
        switch (type->kind) {
            default: assert(false);

            case AST_Type_Kind::POINTER: {
                dcArgPointer(ffi->dc_vm, *(void**)arg_ptr);
                break;
            }
        }
    }
}
