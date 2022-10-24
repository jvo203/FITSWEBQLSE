#include <stdlib.h>

#include "ring_buffer.h"

void init_ring_buffer(struct ring_buffer *rb)
{
    rb->start = 0;
    rb->end = 0;

    // init data to NULL
    for (int i = 0; i < RING_BUFFER_SIZE; i++)
        rb->data[i] = NULL;

    pthread_mutex_init(&rb->ring_mtx, NULL);
}

void delete_ring_buffer(struct ring_buffer *rb)
{
    pthread_mutex_destroy(&rb->ring_mtx);

    // free all data
    for (int i = 0; i < RING_BUFFER_SIZE; i++)
        if (rb->data[i] != NULL)
            free(rb->data[i]);
}

void put(struct ring_buffer *rb, void *item)
{
    // lock the mutex
    pthread_mutex_lock(&rb->ring_mtx);

    rb->data[rb->end++] = item;
    rb->end %= RING_BUFFER_SIZE;

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);
}

void *get(struct ring_buffer *rb)
{
    // lock the mutex
    pthread_mutex_lock(&rb->ring_mtx);

    void *item = rb->data[rb->start];
    rb->data[rb->start++] = NULL; // nullify the location after taking out an item
    rb->start %= RING_BUFFER_SIZE;

    // unlock the mutex
    pthread_mutex_unlock(&rb->ring_mtx);

    return item;
}