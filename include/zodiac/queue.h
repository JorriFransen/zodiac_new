#pragma once

namespace Zodiac
{

    template <typename Element_Type>
    struct Queue
    {
         Element_Type *buffer = nullptr;
         int64_t capacity = 0;
         int64_t front_index = -1;
         int64_t back_index = -1;

         Allocator *allocator = nullptr;
    };


    template <typename Element_Type>
    void queue_init(Allocator *allocator, Queue<Element_Type> *queue, int64_t capacity = 8)
    {
        assert(allocator);
        assert(queue);
        assert(capacity > 0);

        queue->buffer = alloc_array<Element_Type>(allocator, capacity);
        assert(queue->buffer);

        queue->capacity = capacity;
        queue->front_index = -1;
        queue->back_index = -1;

        queue->allocator = allocator;
    }

    template <typename Element_Type>
    void queue_enqueue(Queue<Element_Type> *queue, Element_Type value)
    {
        assert(queue);
        
        queue_ensure_capacity(queue);

        queue->buffer[queue->back_index] = value;
        queue->back_index++;
    }

    template <typename Element_Type>
    void queue_ensure_capacity(Queue<Element_Type> *queue)
    {
        assert(queue);

        if (queue->back_index == -1)
        {
            assert(queue->front_index == -1);
            if (queue->capacity > 0) return;
            else assert(false);
        }
        else assert(false);
    }
}
