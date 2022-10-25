#pragma one

#include <pthread.h>

#define RING_BUFFER_SIZE 8

struct ring_buffer
{
    int start;
    int end;
    void *data[RING_BUFFER_SIZE];

    // a mutex to protect read/write
    pthread_mutex_t ring_mtx;
};

void init_ring_buffer(struct ring_buffer *rb);
void delete_ring_buffer(struct ring_buffer *rb);
void ring_put(struct ring_buffer *rb, void *item);
void *ring_get(struct ring_buffer *rb);