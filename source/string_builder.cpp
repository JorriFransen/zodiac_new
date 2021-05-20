
#include "common.h"
#include "string_builder.h"
#include "zodiac_string.h"

#include <stdio.h>

namespace Zodiac
{
    void string_builder_init(Allocator *allocator, String_Builder *sb, uint64_t default_block_cap/*=2048*/)
    {
        assert(allocator);
        assert(sb);
        assert(default_block_cap);

        auto first_block = string_builder_allocate_block(allocator, default_block_cap);
        assert(first_block);

        sb->first_block = first_block;
        sb->current_block = first_block;
        sb->allocator = allocator;

    }

    void string_builder_free(String_Builder *sb)
    {
        assert(sb);

        assert(sb->allocator);
        assert(sb->first_block);

        auto block = sb->first_block;
        while (block)
        {
            auto next = block->next;

            free(sb->allocator, block);

            block = next;
        }
    }

    String_Builder_Block *string_builder_allocate_block(Allocator *allocator, uint64_t capacity)
    {
        auto block_size = sizeof(String_Builder_Block) + capacity;
        auto mem = alloc(allocator, block_size);

        String_Builder_Block *block = (String_Builder_Block*)mem;
        assert(block);

        block->buf = ((uint8_t*)block) + sizeof(String_Builder_Block);
        block->used = 0;
        block->capacity = capacity;
        block->next = nullptr;

        return block;
    }

    void string_builder_push_new_block(String_Builder *sb, uint64_t capacity)
    {
        auto new_block = string_builder_allocate_block(sb->allocator, capacity);
        assert(sb->current_block);
        assert(sb->current_block->next == nullptr);
        sb->current_block->next = new_block;
        sb->current_block = new_block;
    }

    void string_builder_append(String_Builder *sb, const char *cstr)
    {
        string_builder_append(sb, cstr, strlen(cstr));
    }

    void string_builder_append(String_Builder *sb, const char *cstr, uint64_t length)
    {
        auto block = sb->current_block;
        auto block_rem = block->capacity - block->used;

        auto size_for_cur_block = min(block_rem, length);

        if (size_for_cur_block > 0)
        {
            string_builder_append_to_block(block, cstr, size_for_cur_block);
        }
        else assert(size_for_cur_block == 0);

        auto rem = length - size_for_cur_block;

        if (rem > 0)
        {
            auto new_block_cap = max(block->capacity * 2, rem);
            string_builder_push_new_block(sb, new_block_cap);
            string_builder_append_to_block(sb->current_block, cstr + size_for_cur_block, rem);
        }
    }

    void string_builder_appendf(String_Builder *sb, const char *fmt ...)
    {
        va_list args;
        va_start(args, fmt);

        string_builder_appendf(sb, fmt, args);

        va_end(args);
    }

    void string_builder_appendf(String_Builder *sb, const char *fmt, va_list args)
    {
        auto allocator = sb->allocator;

        va_list args_copy;
        va_copy(args_copy, args);
        auto size = vsnprintf(nullptr, 0, fmt, args_copy);
        va_end(args_copy);

        char *buf = alloc_array<char>(allocator, size + 1);
        assert(buf);

        auto written_size = vsnprintf(buf, size + 1, fmt, args);
        assert(written_size <= size);

        string_builder_append(sb, buf, written_size);

        free(allocator, buf);
    }

    void string_builder_append_to_block(String_Builder_Block *sbb, const char *cstr, uint64_t length)
    {
#ifndef NDEBUG
        auto rem = sbb->capacity - sbb->used;
        assert(rem >= length);

        assert(strlen(cstr) >= length);
#endif

        memcpy(sbb->buf + sbb->used, cstr, length);
        sbb->used += length;
    }

    String string_builder_to_string(Allocator *allocator, String_Builder *sb)
    {
        uint64_t required_cap = 0;

        auto block = sb->first_block;
        while (block)
        {
            required_cap += block->used;
            block = block->next;
        }

        char *cstr = alloc_array<char>(allocator, required_cap + 1);
        auto dest_cur = cstr;
        block = sb->first_block;

        while (block)
        {
            if (block->used)
            {
                memcpy(dest_cur, block->buf, block->used);
            }
            dest_cur += block->used;
            block = block->next;
        }

        *dest_cur = '\0';

        return string_ref(cstr, required_cap);
    }
}
