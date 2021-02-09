#pragma once

#include "build_data.h"
#include "struct_predecls.h"
#include "atom.h"

enum class Builtin_Type_Kind
{
    INVALID,

    VOID,
    INTEGER,
    FLOAT,
    BOOL,
};

#define BUILTIN_TYPE_LIST                          \
    DEFINE_BUILTIN_TYPE(void, VOID, 0, false)      \
    DEFINE_BUILTIN_TYPE(bool, BOOL, 8, false)      \
    DEFINE_BUILTIN_TYPE(float,  FLOAT,  32, true)  \
    DEFINE_BUILTIN_TYPE(double,  FLOAT,  64, true) \
    DEFINE_BUILTIN_TYPE(s8,  INTEGER,  8, true)    \
    DEFINE_BUILTIN_TYPE(u8,  INTEGER,  8, false)   \
    DEFINE_BUILTIN_TYPE(s16, INTEGER, 16, true)    \
    DEFINE_BUILTIN_TYPE(u16, INTEGER, 16, false)   \
    DEFINE_BUILTIN_TYPE(s32, INTEGER, 32, true)    \
    DEFINE_BUILTIN_TYPE(u32, INTEGER, 32, false)   \
    DEFINE_BUILTIN_TYPE(s64, INTEGER, 64, true)    \
    DEFINE_BUILTIN_TYPE(u64, INTEGER, 64, false)   \

#define BUILTIN_ATOM_LIST                       \
    DEFINE_BUILTIN_ATOM(_start)                 \
    DEFINE_BUILTIN_ATOM(mainCRTStartup)         \
    DEFINE_BUILTIN_ATOM(call_main_and_exit)     \
    DEFINE_BUILTIN_ATOM(main)                   \
    DEFINE_BUILTIN_ATOM(count)                  \
    DEFINE_BUILTIN_ATOM(it)                     \
    DEFINE_BUILTIN_ATOM(it_index)               \
    DEFINE_BUILTIN_ATOM(run)                    \
    DEFINE_BUILTIN_ATOM(test)                   \
    DEFINE_BUILTIN_ATOM(elseif)                 \
    DEFINE_BUILTIN_ATOM(naked)                  \
    DEFINE_BUILTIN_ATOM(noreturn)               \
    DEFINE_BUILTIN_ATOM(foreign)                \
    DEFINE_BUILTIN_ATOM(allow_incomplete)       \
    DEFINE_BUILTIN_ATOM(assert)                 \
    DEFINE_BUILTIN_ATOM(default_assert_handler) \
    DEFINE_BUILTIN_ATOM(static_assert)          \
    DEFINE_BUILTIN_ATOM(syscall)                \
    DEFINE_BUILTIN_ATOM(exit)                   \
    DEFINE_BUILTIN_ATOM(cast)                   \
    DEFINE_BUILTIN_ATOM(sizeof)                 \
    DEFINE_BUILTIN_ATOM(offsetof)               \
    DEFINE_BUILTIN_ATOM(pre_main)               \
    DEFINE_BUILTIN_ATOM(compiler)               \
    DEFINE_BUILTIN_ATOM(abort)                  \
    DEFINE_BUILTIN_ATOM(PLATFORM_LINUX)         \
    DEFINE_BUILTIN_ATOM(PLATFORM_WINDOWS)       \

#undef DEFINE_BUILTIN_TYPE

namespace Zodiac
{
    struct Builtin
    {
        static uint64_t pointer_size;

#define DEFINE_BUILTIN_ATOM(name) static Atom atom_ ##name;
#define DEFINE_BUILTIN_TYPE(name, kind, size, signed) static Atom atom_ ##name;
        BUILTIN_TYPE_LIST
        BUILTIN_ATOM_LIST
#undef DEFINE_BUILTIN_ATOM
#undef DEFINE_BUILTIN_TYPE

#define DEFINE_BUILTIN_TYPE(name, kind, size, signed) static AST_Type *type_ ##name;
        BUILTIN_TYPE_LIST
#undef DEFINE_BUILTIN_TYPE

        static AST_Type *type_ptr_u8;
    };

    void builtin_initialize_atoms(Atom_Table *at);
    void builtin_initialize_types(Allocator *allocator, Build_Data *build_data);
    AST_Type *builtin_initialize_type(Allocator *allocator, Builtin_Type_Kind kind, uint64_t size,
                                      bool sign);

    Array<AST_Declaration *> builtin_populate_scope(Allocator *allocator, Scope *global_scope);
}
