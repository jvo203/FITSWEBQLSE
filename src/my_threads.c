#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

extern void *my_pthread_create(void *(*start_routine)(void *), void *arg)
{
    pthread_t *ptid = malloc(sizeof(pthread_t));
    int rc = pthread_create(ptid, NULL, start_routine, arg);
    printf("pthread_create rc = %d\n", rc);
    // return rc;

    return ptid;
}

extern int my_pthread_create_detached(void *(*start_routine)(void *), void *arg)
{
    pthread_t thread;
    int rc = pthread_create(&thread, NULL, start_routine, arg);

    // detach the thread
    if (rc == 0)
        pthread_detach(thread);

    printf("pthread_create_detached rc = %d\n", rc);
    return rc;
}

extern int my_pthread_join(pthread_t *tid)
{
    // int rc = pthread_join(thread, NULL);
    int rc = pthread_join(*tid, NULL);
    printf("pthread_join rc = %d\n", rc);
    free(tid);
    return rc;
}

extern int my_pthread_detach(pthread_t *tid)
{
    // int rc = pthread_detach(thread);
    int rc = pthread_detach(*tid);
    printf("pthread_detach rc = %d\n", rc);
    return rc;
}