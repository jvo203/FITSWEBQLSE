#include <stdio.h>
#include <inttypes.h>

#include "cluster.h"
#include "http.h"

extern options_t options; // <options> is defined in main.c

void forward_exit_event(void *item, void *)
{
    char url[256];

    sprintf(url, "http://%s:%" PRIu16 "/exit", (const char *)item, options.http_port);
    printf("Exit URL: %s\n", url);

    return;
}

void distributed_exit()
{
    g_mutex_lock(&cluster_mtx);

    g_slist_foreach(cluster, forward_exit_event, NULL);

    g_mutex_unlock(&cluster_mtx);
}