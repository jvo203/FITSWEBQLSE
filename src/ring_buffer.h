#pragma one

// use a spin lock
#include <pthread.h>

#define RING_BUFFER_SIZE 8

struct ring_buffer
{
    int head;
    int tail;
    int data[RING_BUFFER_SIZE];
    // a spin lock
    pthread_spinlock_t lock;
};