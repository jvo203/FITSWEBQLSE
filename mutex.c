#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void *foo(void *)
{
    printf("foo.\n")

        return NULL;
}

int main()
{
    pthread_t t1;
    int rc;

    rc = pthread_create(&t1, NULL, foo, NULL);
    printf("pthread_create rc = %d", rc);

    rc = pthread_join(t1, NULL);
    printf("pthread_join rc = %d", rc);

    return 0;
}