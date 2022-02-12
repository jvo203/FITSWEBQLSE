#include <stdlib.h>
#include <stdio.h>

#include <pthread.h>

int main()
{
	printf("__PTHREAD_SIZE__: %d\n",__PTHREAD_SIZE__);
	printf("__PTHREAD_MUTEX_SIZE__: %d\n",__PTHREAD_MUTEX_SIZE__);
};
