#pragma once

#include "allocator.h"

#include "common.h"

#include <inttypes.h>

namespace Zodiac
{
    template <typename Element_Type>
    struct Stack
    {
        Element_Type *buffer = nullptr;
        int64_t sp = -1;
        int64_t capacity = -1;

        Allocator *allocator = nullptr;
    };

    template <typename Element_Type>
    void stack_init(Allocator *allocator, Stack<Element_Type> *stack, int64_t initial_cap = 8)
    {
        assert(allocator);
        assert(stack);
        assert(initial_cap);

        stack->buffer = alloc_array<Element_Type>(allocator, initial_cap);
        stack->sp = 0;
        stack->capacity = initial_cap;
        stack->allocator = allocator;
    }

    template <typename Element_Type>
    void stack_free(Stack<Element_Type> *stack)
    {
        assert(stack);
        free(stack->allocator, stack->buffer);
    }
    
    template <typename Element_Type>
    void stack_ensure_capacity(Stack<Element_Type> *stack)
    {
        if (stack->sp >= stack->capacity)
        {
            auto new_cap = stack->capacity * 2;
            auto new_buf = alloc_array<Element_Type>(stack->allocator, new_cap);
            memcpy(new_buf, stack->buffer, stack->capacity * sizeof(Element_Type));
            free(stack->allocator, stack->buffer);
            stack->buffer = new_buf;
            stack->capacity = new_cap;
        } 
    }

    template <typename Element_Type>
    void stack_push(Stack<Element_Type> *stack, Element_Type element)
    {
        stack_ensure_capacity(stack);

        stack->buffer[stack->sp] = element;
        stack->sp += 1;
    }

    template <typename Element_Type>
    Element_Type stack_peek(Stack<Element_Type> *stack, int64_t offset = 0)
    {
        assert(stack);
        assert(stack->sp >= 1);

        assert(stack->sp > offset);

        return stack->buffer[(stack->sp - 1) - offset];
    }

    template <typename Element_Type>
    Element_Type *stack_peek_ptr(Stack<Element_Type> *stack, int64_t offset = 0)
    {
        assert(stack);
        assert(stack->sp >= 1);
        assert(offset >= 0);

        assert(stack->sp > offset);

        return &stack->buffer[(stack->sp - 1) - offset];
    }

    template <typename Element_Type>
    Element_Type stack_top(Stack<Element_Type> *stack)
    {
        return stack_peek(stack);
    }

    template <typename Element_Type>
    Element_Type *stack_top_ptr(Stack<Element_Type> *stack)
    {
        return stack_peek_ptr(stack);
    }

    template <typename Element_Type>
    Element_Type stack_pop(Stack<Element_Type> *stack)
    {
        assert(stack->sp >= 1);

        auto result = stack_peek(stack);
        stack->sp -= 1;

        return result;
    }

    template <typename Element_Type>
    int64_t stack_count(Stack<Element_Type> *stack)
    {
        return stack->sp;
    }
}
