#include <stdlib.h>
#include <stdio.h>

#include "ring_buffer.h"

void init_ring_buffer(struct ring_buffer *rb, int capacity)
{
    if (rb == NULL)
        return;

    rb->start = 0;
    rb->end = 0;
    rb->capacity = 0;

    if (capacity <= 0)
        return;

    // allocate memory for data
    rb->data = (void **)malloc(sizeof(void *) * capacity);

    if (rb->data == NULL)
    {
        printf("[C] init_ring_buffer: failed to allocate memory for data.\n");
        return;
    }

    rb->capacity = capacity;

    // init data to NULL
    for (int i = 0; i < capacity; i++)
        rb->data[i] = NULL;

    pthread_mutex_init(&rb->ring_mtx, NULL);
}

void delete_ring_buffer(struct ring_buffer *rb)
{
    if (rb == NULL)
        return;

    pthread_mutex_lock(&rb->ring_mtx);

#ifdef DEBUG
    printf("[C] delete_ring_buffer: start=%d, end=%d.\n", rb->start, rb->end);
#endif

    // free all data
    if (rb->data != NULL)
    {
        for (int i = 0; i < rb->capacity; i++)
            if (rb->data[i] != NULL)
            {
                printf("[C] freeing ring buffer data at position %d.\n", i);
                free(rb->data[i]);
            }

        free(rb->data);
    }

    pthread_mutex_unlock(&rb->ring_mtx);
    pthread_mutex_destroy(&rb->ring_mtx);
}

void ring_put(struct ring_buffer *rb, void *item)
{
    if (rb == NULL)
        return;

    // lock the mutex
    pthread_mutex_lock(&rb->ring_mtx);

    // first delete any existing data so that there are no memory leaks
    if (rb->data[rb->end] != NULL)
        free(rb->data[rb->end]);

    // put (overwrite) the new data
    rb->data[rb->end++] = item;
    rb->end %= rb->capacity;

#ifdef DEBUG
    printf("[C] ring_put: start=%d, end=%d.\n", rb->start, rb->end);
#endif

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);
}

void *ring_get(struct ring_buffer *rb)
{
    if (rb == NULL)
        return NULL;

    // lock the mutex
    pthread_mutex_lock(&rb->ring_mtx);

    void *item = rb->data[rb->start];

#ifdef DEBUG
    printf("[C] ring_get: read from start=%d, item=%p.\n", rb->start, item);
#endif

    // move the start pointer only if the item is not NULL
    if (item != NULL)
    {
        rb->data[rb->start++] = NULL;
        rb->start %= rb->capacity;
    }

#ifdef DEBUG
    printf("[C] ring_get: start=%d, end=%d.\n", rb->start, rb->end);
#endif

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);

    return item;
}