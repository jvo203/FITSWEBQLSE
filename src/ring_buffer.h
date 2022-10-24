#pragma one

// use a spin lock
#include <pthread.h>

#define RING_BUFFER_SIZE 8

struct ring_buffer
{
    int head;
    int tail;
    void *data[RING_BUFFER_SIZE];

    // a spin lock to protect read/write
    pthread_spinlock_t lock;
};