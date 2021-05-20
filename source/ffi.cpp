#include "ffi.h"

#include <cstdio>

namespace Zodiac {

    bool __bytecode_fn_ptr_eq(BC_Function *const &a, BC_Function *const &b) {
        return a == b;
    }

    FFI_Context ffi_create(Allocator *allocator, Build_Data *build_data)
    {
        FFI_Context result = {};

        result.allocator = allocator;

        array_init(allocator, &result.libs);

        result.dc_vm = dcNewCallVM(4096);
        dcMode(result.dc_vm, DC_CALL_C_DEFAULT);
        dcReset(result.dc_vm);

        hash_table_init(allocator, &result.functions, operator==);

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

    bool ffi_load_function(FFI_Context *ffi, const Atom &name)
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
            // printf("Loaded symbol '%s': %p\n", name.data, symbol);
            hash_table_add(&ffi->functions, name, symbol);
            return true;
        }
        return false;
    }

    void ffi_call(FFI_Context *ffi, const Atom &name, void *return_val_ptr,
                  AST_Type *return_type)
    {
        DCpointer func_ptr = nullptr;
        bool found = hash_table_find(&ffi->functions, name, &func_ptr);
        assert(found);
        if (!found) {
            fprintf(stderr, "Failed to find foreign function: '%s'\n", name.data);
            return;
        }

        // printf("Calling function '%s': %p\n", name.data, func_ptr);

        assert(return_val_ptr);

        switch (return_type->kind) {
            default: assert(false);

            case AST_Type_Kind::INTEGER: {
                if (return_type->integer.sign) {
                    switch (return_type->bit_size) {
                        default: assert(false);
                        case 8: assert(false);
                        case 16: assert(false);

                        case 32: {
                            assert(sizeof(int) == 4);
                            assert(sizeof(DCint) == 4);
                            int32_t result = dcCallInt(ffi->dc_vm, func_ptr);
                            *(int32_t*)return_val_ptr = result;
                            break;
                        }

                        case 64: {
                            assert(sizeof(DClonglong) == 8);
                            int64_t result = dcCallLongLong(ffi->dc_vm, func_ptr);
                            *(int64_t*)return_val_ptr = result;
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
                            int32_t result = dcCallInt(ffi->dc_vm, func_ptr);
                            *(uint32_t*)return_val_ptr = result;
                            break;
                        }

                        case 64: {
                            assert(sizeof(DClonglong) == 8);
                            int64_t result = dcCallLongLong(ffi->dc_vm, func_ptr);
                            *(uint64_t*)return_val_ptr = result;
                            break;
                        }

                    }
                }
                break;
            }

            case AST_Type_Kind::POINTER: {
                void *result = dcCallPointer(ffi->dc_vm, func_ptr);
                *(void**)return_val_ptr = result;
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
}
