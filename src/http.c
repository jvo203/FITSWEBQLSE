#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>

#include <microhttpd.h>
#include <libwebsockets.h>

typedef void (*sighandler_t)(int);

extern void register_kill_signal_handler_(sighandler_t handler)
{
    signal(SIGINT, handler);
    signal(SIGTERM, handler);
}

extern void exit_fortran_();
extern void http_request_();

#define HTTP_PORT 8080
#define WS_PORT (HTTP_PORT + 1)

// WS
#define LWS_PLUGIN_STATIC
#include "protocol_lws_minimal.c"

static struct lws_protocols protocols[] = {
	{ "http", lws_callback_http_dummy, 0, 0 },
	LWS_PLUGIN_PROTOCOL_MINIMAL,
	{ NULL, NULL, 0, 0 } /* terminator */
};

static const lws_retry_bo_t retry = {
	.secs_since_valid_ping = 3,
	.secs_since_valid_hangup = 10,
};

// HTML
#define PAGE "<html><head><title>FITSWEBQL SE</title>" \
             "</head><body>FITSWEBQLSE (libmicrohttpd)</body></html>"

struct MHD_Daemon *http_server = NULL;

static int on_client_connect(void *cls,
                             const struct sockaddr *addr,
                             socklen_t addrlen);

static enum MHD_Result on_http_connection(void *cls, struct MHD_Connection *connection,
                                          const char *url, const char *method,
                                          const char *version, const char *upload_data,
                                          size_t *upload_data_size, void **con_cls);

static enum MHD_Result
ahc_echo(void *cls,
         struct MHD_Connection *connection,
         const char *url,
         const char *method,
         const char *version,
         const char *upload_data,
         size_t *upload_data_size,
         void **ptr)
{
    static int dummy;
    const char *page = cls;
    struct MHD_Response *response;
    int ret;

    if (0 != strcmp(method, "GET"))
        return MHD_NO; /* unexpected method */
    if (&dummy != *ptr)
    {
        /* The first time only the headers are valid,
         do not respond in the first round... */
        *ptr = &dummy;
        return MHD_YES;
    }
    if (0 != *upload_data_size)
        return MHD_NO; /* upload data in a GET!? */
    *ptr = NULL;       /* clear context pointer */

    printf("URL:%s\n", url);    

    // pass the request to FORTRAN
    http_request_();

    response = MHD_create_response_from_buffer(strlen(page),
                                               (void *)page,
                                               MHD_RESPMEM_PERSISTENT);
    ret = MHD_queue_response(connection,
                             MHD_HTTP_OK,
                             response);
    MHD_destroy_response(response);    

    return ret;
}

void SIGINTHandler(int sigint)
{
    printf("\tCTRL-C detected. Exiting the C part.\n");

    if (http_server != NULL)
    {
        printf("shutting down the HTTP server... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;

        //stop accepting new connections
        //MHD_quiesce_daemon(http_server);
    };

    printf("clean shutdown completed.\n");

    exit_fortran_();
};

extern void start_http_()
{

    signal(SIGPIPE, SIG_IGN); //ignore SIGPIPE
    //signal(SIGINT, SIGINTHandler); //intercept CTRL+C to trigger a clean shutdown

    /*http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC,
                                   HTTP_PORT,
                                   NULL, NULL, &on_http_connection, NULL,
                                   MHD_OPTION_END);*/

    http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION,
                                   HTTP_PORT,
                                   NULL,
                                   NULL,
                                   &ahc_echo,
                                   PAGE,
                                   MHD_OPTION_END);

    if (http_server == NULL)
    {
        printf("Could not start a libmicrohttpd web server.\n");
        return;
    }
    else
    {
        printf("µHTTP daemon listening on port %d... Press CTRL-C to stop it.\n", HTTP_PORT);
    }
}

extern void stop_http_()
{
    if (http_server != NULL)
    {
        printf("shutting down the HTTP server... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;
        printf("done\n");
    }
}