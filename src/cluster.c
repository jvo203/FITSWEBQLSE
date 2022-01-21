#include <stdio.h>
#include <inttypes.h>

#include "mongoose.h"
#include "cluster.h"
#include "http.h"

GSList *cluster = NULL;
GMutex cluster_mtx;

extern options_t options; // definition in main.c
extern struct mg_mgr mgr; // definition in main.c

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

static void exit_fn(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    char *host = (char *)fn_data;

    if (ev == MG_EV_CONNECT)
    {
        // get the client host
        struct mg_str host = mg_str((char *)fn_data);

        // Send request
        mg_printf(c,
                  "GET /exit HTTP/1.0\r\n"
                  "Host: %.*s:%u\r\n"
                  "\r\n",
                  (int)host.len, host.ptr, options.http_port);
    }
    else if (ev == MG_EV_HTTP_MSG)
    {
        // Response is received. Print it
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        printf("%.*s", (int)hm->message.len, hm->message.ptr);
        c->is_closing = 1; // Tell mongoose to close this connection
        //*(bool *)fn_data = true; // Tell event loop to stop
    }
    else if (ev == MG_EV_ERROR)
    {
        printf("MG_EV_ERROR\n");
        c->is_closing = 1; // Tell mongoose to close this connection
        //*(bool *)fn_data = true; // Error, tell event loop to stop
    }
}

void forward_exit_event(void *item, void *)
{
    char url[256];

    sprintf(url, "http://%s:%" PRIu16 "/exit", (const char *)item, options.http_port);
    printf("Exit URL: %s\n", url);

    // pass the node IP address to mongoose
    mg_http_connect(&mgr, url, exit_fn, item); // Create client connection

    return;
}

void distributed_exit()
{
    g_mutex_lock(&cluster_mtx);

    g_slist_foreach(cluster, forward_exit_event, NULL);

    g_mutex_unlock(&cluster_mtx);
}