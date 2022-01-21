#include <stdio.h>
#include <inttypes.h>

#include "mongoose.h"
#include "cluster.h"
#include "http.h"

GSList *cluster = NULL;
GMutex cluster_mtx;

extern options_t options; // definition in main.c

void init_cluster()
{
    g_mutex_init(&cluster_mtx);
}

void delete_cluster()
{
    GSList *iterator = NULL;
    for (iterator = cluster; iterator; iterator = iterator->next)
        printf("cluster node %s\n", (char *)iterator->data);

    if (cluster != NULL)
        g_slist_free_full(cluster, free);

    g_mutex_clear(&cluster_mtx);
}

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