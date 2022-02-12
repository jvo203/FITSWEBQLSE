#include <stdlib.h>
#include <stdio.h>

#include <pthread.h>

int main()
{
	printf("__PTHREAD_SIZE__: %d\n",PTHREAD_SIZE);
	printf("__PTHREAD_MUTEX_SIZE__: %d\n",PTHREAD_MUTEX_SIZE);
};
