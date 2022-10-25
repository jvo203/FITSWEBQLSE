#include <stdlib.h>

#include "ring_buffer.h"

void init_ring_buffer(struct ring_buffer *rb)
{
    if (rb == NULL)
        return;

    rb->start = 0;
    rb->end = 0;

    // init data to NULL
    for (int i = 0; i < RING_BUFFER_SIZE; i++)
        rb->data[i] = NULL;

    pthread_mutex_init(&rb->ring_mtx, NULL);
}

void delete_ring_buffer(struct ring_buffer *rb)
{
    if (rb == NULL)
        return;

    pthread_mutex_lock(&rb->ring_mtx);

    printf("[C] delete_ring_buffer: start=%d, end=%d.\n", rb->start, rb->end);

    // free all data
    for (int i = 0; i < RING_BUFFER_SIZE; i++)
        if (rb->data[i] != NULL)
        {
            printf("[C] freeing ring buffer data at position %d.\n", i);
            free(rb->data[i]);
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
    rb->end %= RING_BUFFER_SIZE;

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);
}

void *ring_get(struct ring_buffer *rb)
{
    if (rb == NULL)
        return;

    // lock the mutex
    pthread_mutex_lock(&rb->ring_mtx);

    void *item = rb->data[rb->start];

    // move the start pointer only if the item is not NULL
    if (item != NULL)
    {
        rb->data[rb->start++] = NULL;
        rb->start %= RING_BUFFER_SIZE;
    }

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);

    return item;
}