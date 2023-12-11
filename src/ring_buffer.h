#pragma once

#include <pthread.h>

// #define RING_BUFFER_SIZE 32 // 8 for P-V, 32 for the rest ...

struct ring_buffer
{
    volatile int start;
    volatile int end;
    volatile int capacity;
    void **data;
    // void *data[RING_BUFFER_SIZE];

    // a mutex to protect read/write
    pthread_mutex_t ring_mtx;
};

void init_ring_buffer(struct ring_buffer *rb, int capacity);
void delete_ring_buffer(struct ring_buffer *rb);
void ring_put(struct ring_buffer *rb, void *item);
void *ring_get(struct ring_buffer *rb);