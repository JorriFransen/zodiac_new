#pragma once

#include "allocator.h"
#include "struct_predecls.h"

#include <cstdarg>

namespace Zodiac
{

    struct String_Builder_Block;
    struct String_Builder
    {
        String_Builder_Block *current_block = nullptr;
        String_Builder_Block *first_block = nullptr;

        Allocator *allocator = nullptr;
    };

    struct String_Builder_Block
    {
        uint8_t *buf = nullptr;
        uint64_t used = 0;
        uint64_t capacity = 0;

        String_Builder_Block *next;
    };

    void string_builder_init(Allocator *allocator, String_Builder *sb,
                             uint64_t default_block_cap = 2048);
    void string_builder_free(String_Builder *sb);

    String_Builder_Block *string_builder_allocate_block(Allocator *allocator, uint64_t capacity);
    void string_builder_push_new_block(String_Builder *sb, uint64_t capacity);

    void string_builder_append(String_Builder *sb, const char *cstr);
    void string_builder_append(String_Builder *sb, const char *cstr, uint64_t length);
    void string_builder_appendf(String_Builder *sb, const char *fmt ...);
    void string_builder_appendf(String_Builder *sb, const char *fmt, va_list args);
    void string_builder_append_to_block(String_Builder_Block *sbb, const char *cstr, uint64_t length);

    String string_builder_to_string(Allocator *allocator, String_Builder *sb);
}
