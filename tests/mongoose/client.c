// Copyright (c) 2021 Cesanta Software Limited
// All rights reserved
//
// Example HTTP client. Connect to `s_url`, send request, wait for a response,
// print the response and exit.
// You can change `s_url` from the command line by executing: ./example YOUR_URL
//
// To enable SSL/TLS for this client, build it like this:
//    make MBEDTLS=/path/to/your/mbedtls/installation
//    make OPENSSL=/path/to/your/openssl/installation

#include "mongoose.h"

// The very first web page in history. You can replace it from command line
static const char *s_url = "http://localhost:8080/float";
// static const char *s_post = NULL; // POST data

#define N 16

float post_buf[N];

// Print HTTP response and signal that we're done
static void fn(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_CONNECT)
    {
        // Connected to server. Extract host name from URL
        struct mg_str host = mg_url_host(s_url);

        // If s_url is https://, tell client connection to use TLS
        if (mg_url_is_ssl(s_url))
        {
            struct mg_tls_opts opts = {.ca = "ca.pem", .srvname = host};
            mg_tls_init(c, &opts);
        }

        // Send request
        int content_length = sizeof(post_buf);
        mg_printf(c,
                  "%s %s HTTP/1.0\r\n"
                  "Host: %.*s:8080\r\n"
                  "Content-Type: octet-stream\r\n"
                  "Content-Length: %d\r\n"
                  "\r\n",
                  "POST", mg_url_uri(s_url), (int)host.len,
                  host.ptr, content_length);
        mg_send(c, post_buf, content_length);
    }
    else if (ev == MG_EV_HTTP_MSG)
    {
        // Response is received. Print it
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        printf("%.*s", (int)hm->message.len, hm->message.ptr);
        c->is_closing = 1;       // Tell mongoose to close this connection
        *(bool *)fn_data = true; // Tell event loop to stop
    }
    else if (ev == MG_EV_ERROR)
    {
        *(bool *)fn_data = true; // Error, tell event loop to stop
    }
}

int main(int argc, char *argv[])
{
    struct mg_mgr mgr; // Event manager
    bool done = false; // Event handler flips it to true
    int i;

    // fill-in the floating-point POST buffer
    for (i = 0; i < N; i++)
        post_buf[i] = 1 + i;

    mg_log_set("3");                         // Set to 0 to disable debug
    mg_mgr_init(&mgr);                       // Initialise event manager
    mg_http_connect(&mgr, s_url, fn, &done); // Create client connection
    while (!done)
        mg_mgr_poll(&mgr, 1000); // Infinite event loop
    mg_mgr_free(&mgr);           // Free resources
    return 0;
}