#include "http.h"
#include "ws.h"

extern options_t options; // <options> is defined in main.c
extern sig_atomic_t s_received_signal;

static void mg_ws_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_OPEN)
    {
        // c->is_hexdumping = 1;
    }
    else if (ev == MG_EV_HTTP_MSG)
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        if (mg_http_match_uri(hm, "/websocket"))
        {
            // Upgrade to websocket. From now on, a connection is a full-duplex
            // Websocket connection, which will receive MG_EV_WS_MSG events.
            mg_ws_upgrade(c, hm, NULL);
        }
        else
        {
            printf("rejecting the connection.\n");

            // close the connection
            mg_http_reply(c, 404, "", "Rejected");
        }
    }
    else if (ev == MG_EV_WS_MSG)
    {
        // Got websocket frame. Received data is wm->data. Echo it back!
        struct mg_ws_message *wm = (struct mg_ws_message *)ev_data;
        mg_ws_send(c, wm->data.ptr, wm->data.len, WEBSOCKET_OP_TEXT);
    }
    (void)fn_data;
}

void start_ws()
{
    char url[256] = "";
    sprintf(url, "ws://0.0.0.0:%d", options.ws_port);

    struct mg_mgr mgr; // Event manager
    mg_mgr_init(&mgr); // Initialise event manager
    printf("Starting WS listener on %s\n", url);

    mg_http_listen(&mgr, url, mg_ws_callback, NULL); // Create HTTP listener

    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Infinite event loop

    mg_mgr_free(&mgr);
}