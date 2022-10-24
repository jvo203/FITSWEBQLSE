#include <stdlib.h>

#include "ring_buffer.h"

void init_ring_buffer(struct ring_buffer *rb)
{
    rb->head = 0;
    rb->tail = 0;

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

/*void put(int item)
{
    buf[end++] = item;
    end %= BUFLEN;
}*/

/*int get()
{
    int item = buf[start++];
    start %= BUFLEN;
    return item;
}*/