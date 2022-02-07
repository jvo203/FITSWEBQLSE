#include "http.h"
#include "ws.h"

extern options_t options; // <options> is defined in main.c
extern sig_atomic_t s_received_signal;

static void mg_ws_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    switch (ev)
    {
    case MG_EV_OPEN:
    {
        // c->is_hexdumping = 1;
        break;
    }
    case MG_EV_CLOSE:
    {
        if (c->is_websocket)
        {
            printf("WEBSOCKET CONNECTION CLOSED.\n");
            printf("closing a websocket connection for c->fn_data(%s)\n", (char *)c->fn_data);
            printf("closing a websocket connection for fn_data(%s)\n", (char *)fn_data);
        }

        // free any user data <c->fn_data>

        break;
    }
    case MG_EV_HTTP_MSG:
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;

        if (mg_strstr(hm->uri, mg_str("/websocket")) != NULL)
        {
            printf("ACCEPTED WEBSOCKET URI:\t%.*s\n", (int)hm->uri.len, hm->uri.ptr);

            // extract the datasetId

            // set the <c->fn_data> to datasetId

            // Upgrade to websocket. From now on, a connection is a full-duplex
            // Websocket connection, which will receive MG_EV_WS_MSG events.
            mg_ws_upgrade(c, hm, NULL);
            break;
        }

        // is it "/range" ?
        if (mg_strstr(hm->uri, mg_str("/range")) != NULL)
        {

            // signal a catastrophic error
            mg_http_reply(c, 200, "", "{\"startindex\":0,\"endindex\":0,\"status\":-2}");

            // accept the request
            // mg_http_reply(c, 202, "", "Accepted");
            break;
        }

        // else
        {
            printf("rejecting the connection.\n");

            c->is_closing = 1; // Tell mongoose to close this connection

            // reject the connection
            mg_http_reply(c, 404, "", "Rejected");
        }

        break;
    }
    case MG_EV_WS_OPEN:
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        printf("WEBSOCKET OPEN; URI:\t%.*s\n", (int)hm->uri.len, hm->uri.ptr);
        break;
    }
    case MG_EV_WS_MSG:
    {
        // Got websocket frame. Received data is wm->data. Echo it back!
        struct mg_ws_message *wm = (struct mg_ws_message *)ev_data;

        printf("[WS] %.*s\n", (int)wm->data.len, wm->data.ptr);

        mg_ws_send(c, wm->data.ptr, wm->data.len, WEBSOCKET_OP_TEXT);
        break;
    }
    default:
        break;
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