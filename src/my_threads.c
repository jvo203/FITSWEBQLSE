#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

// #define __PTHREAD_SIZE__ 596

// struct __darwin_pthread_handler_rec
//{
//     void (*__routine)(void *); /* Routine to call */
//     void *__arg;               /* Argument to pass */
//     struct __darwin_pthread_handler_rec *__next;
// };

/*struct _opaque_pthread_t
{
    long __sig;
    struct __darwin_pthread_handler_rec *__cleanup_stack;
    char __opaque[__PTHREAD_SIZE__];
};*/

extern void *my_pthread_create(void *(*start_routine)(void *), void *arg, int *rc)
{
    pthread_t *ptid = (pthread_t *)malloc(sizeof(pthread_t));
    *rc = pthread_create(ptid, NULL, start_routine, arg);
    printf("[C] pthread_create rc = %d\n", rc);
    return ptid;
}

extern int my_pthread_create_detached(void *(*start_routine)(void *), void *arg)
{
    // create a detached thread
    pthread_t thread;
    pthread_attr_t attr;
    int rc;

    rc = pthread_attr_init(&attr);

    if (rc != 0)
    {
        printf("[C] pthread_attr_init rc = %d\n", rc);
        return rc;
    }

    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    if (rc != 0)
    {
        printf("[C] pthread_attr_setdetachstate rc = %d\n", rc);
        pthread_attr_destroy(&attr);
        return rc;
    }

    rc = pthread_create(&thread, &attr, start_routine, arg);
    pthread_attr_destroy(&attr);

    printf("[C] pthread_create_detached rc = %d\n", rc);
    return rc;
}

extern int my_pthread_join(pthread_t *tid)
{
    int rc = pthread_join(*tid, NULL);
    printf("[C] pthread_join rc = %d\n", rc);
    free(tid);
    return rc;
}