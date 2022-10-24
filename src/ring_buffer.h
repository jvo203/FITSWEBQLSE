#pragma one

#include <pthread.h>

#define RING_BUFFER_SIZE 8

struct ring_buffer
{
    int head;
    int tail;
    void *data[RING_BUFFER_SIZE];

    // a mutex to protect read/write
    pthread_mutex_t ring_mtx;
};