#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

extern int my_pthread_create(pthread_t *thread, void *(*start_routine)(void *), void *arg)
{
    int rc = pthread_create(thread, NULL, start_routine, arg);
    printf("pthread_create rc = %d\n", rc);
    return rc;
}

extern int my_pthread_join(pthread_t thread)
{
    int rc = pthread_join(thread, NULL);
    printf("pthread_join rc = %d\n", rc);
    return rc;
}

extern int my_pthread_detach(pthread_t thread)
{
    int rc = pthread_detach(thread);
    printf("pthread_detach rc = %d\n", rc);
    return rc;
}