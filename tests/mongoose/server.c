// Copyright (c) 2020 Cesanta Software Limited
// All rights reserved

#include <signal.h>
#include "mongoose.h"

static const char *s_url = "http://0.0.0.0:8000";

// Event handler for the listening connection.
static void cb(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_HTTP_MSG)
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;

        // MG_INFO(("New request to: [%.*s], body size: %lu", (int)hm->uri.len, hm->uri.ptr, (unsigned long)hm->body.len));

        if (mg_strstr(hm->uri, mg_str("/float")) != NULL)
        {
            int i, n;

            n = hm->body.len / sizeof(float);

            printf("received <%d> floating-point numbers.\n", n);

            if (n > 0)
            {
                float *numbers = (float *)hm->body.ptr;

                for (i = 0; i < n; i++)
                    printf("%f ", numbers[i]);
                printf("\n");
            }
        }
    }

    mg_http_reply(c, 200, NULL, "OK");

    (void)fn_data;
}

int main(int argc, char *argv[])
{
    struct mg_mgr mgr; // Event manager
    mg_mgr_init(&mgr); // Initialise event manager
    mg_log_set("3");
    printf("Starting WS listener on %s\n", s_url);

    mg_http_listen(&mgr, s_url, cb, NULL); // Create HTTP listener

    while (true == 0)
        mg_mgr_poll(&mgr, 1000); // Infinite event loop

    mg_mgr_free(&mgr);
}