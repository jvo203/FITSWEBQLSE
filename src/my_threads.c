#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

extern void *my_pthread_create(void *(*start_routine)(void *), void *arg, int *rc)
{
    pthread_t *ptid = malloc(sizeof(pthread_t));
    *rc = pthread_create(ptid, NULL, start_routine, arg);
    printf("pthread_create rc = %d\n", rc);
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
        printf("pthread_attr_init rc = %d\n", rc);
        return rc;
    }

    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

    if (rc != 0)
    {
        printf("pthread_attr_setdetachstate rc = %d\n", rc);
        pthread_attr_destroy(&attr);
        return rc;
    }

    rc = pthread_create(&thread, &attr, start_routine, arg);
    pthread_attr_destroy(&attr);

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