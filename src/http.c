#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <pthread.h>

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
static volatile int interrupted = 0;
pthread_t lws_tid;

#define LWS_PLUGIN_STATIC
#include "protocol_lws_minimal.c"

static struct lws_protocols protocols[] = {
    {"http", lws_callback_http_dummy, 0, 0},
    LWS_PLUGIN_PROTOCOL_MINIMAL,
    {NULL, NULL, 0, 0} /* terminator */
};

static const lws_retry_bo_t retry = {
    .secs_since_valid_ping = 3,
    .secs_since_valid_hangup = 10,
};

static const struct lws_http_mount mount = {
    /* .mount_next */ NULL,         /* linked-list "next" */
    /* .mountpoint */ "/",          /* mountpoint URL */
    /* .origin */ "./mount-origin", /* serve from dir */
    /* .def */ "index.html",        /* default filename */
    /* .protocol */ NULL,
    /* .cgienv */ NULL,
    /* .extra_mimetypes */ NULL,
    /* .interpret */ NULL,
    /* .cgi_timeout */ 0,
    /* .cache_max_age */ 0,
    /* .auth_mask */ 0,
    /* .cache_reusable */ 0,
    /* .cache_revalidate */ 0,
    /* .cache_intermediaries */ 0,
    /* .origin_protocol */ LWSMPRO_FILE, /* files in a dir */
    /* .mountpoint_len */ 1,             /* char count */
    /* .basic_auth_login_file */ NULL,
};

void *start_ws(void *ignore)
{
    struct lws_context_creation_info info;
    struct lws_context *context;
    const char *p;
    int n = 0, logs = LLL_USER | LLL_ERR | LLL_WARN | LLL_NOTICE
        /* for LLL_ verbosity above NOTICE to be built into lws,
			 * lws must have been configured and built with
			 * -DCMAKE_BUILD_TYPE=DEBUG instead of =RELEASE */
        /* | LLL_INFO */ /* | LLL_PARSER */ /* | LLL_HEADER */
        /* | LLL_EXT */ /* | LLL_CLIENT */  /* | LLL_LATENCY */
        /* | LLL_DEBUG */;

    lws_set_log_level(logs, NULL);
    lwsl_user("starting LWS ws server\n");

    memset(&info, 0, sizeof info); /* otherwise uninitialized garbage */
    info.port = WS_PORT;
    info.mounts = &mount;
    info.protocols = protocols;
    info.vhost_name = "localhost";
    info.options =
        LWS_SERVER_OPTION_HTTP_HEADERS_SECURITY_BEST_PRACTICES_ENFORCE;

    // assume default websocket timeouts
    // info.retry_and_idle_policy = &retry;

    context = lws_create_context(&info);
    if (!context)
    {
        lwsl_err("lws init failed\n");
        return NULL;
    }

    lwsl_user("entering LWS event loop\n");
    while (n >= 0 && !interrupted)
        n = lws_service(context, 0);

    lws_context_destroy(context);
    lwsl_user("LWS ws server stopped\n");

    return NULL;
}

// HTML
#define PAGE "<html><head><title>FITSWEBQL SE</title>" \
             "</head><body>FITSWEBQLSE (libmicrohttpd)</body></html>"

struct MHD_Daemon *http_server = NULL;

static enum MHD_Result print_out_key(void *cls, enum MHD_ValueKind kind, const char *key, const char *value, size_t value_size)
{
    printf("%s: %s\n", key, value);
    return MHD_YES;
}

static enum MHD_Result http_not_found(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "<html><body>404 Not Found</body></html>";

    response =
        MHD_create_response_from_buffer(strlen(errorstr),
                                        (void *)errorstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_NOT_FOUND,
                               response);
        MHD_destroy_response(response);

        return ret;
    }
    else
        return MHD_NO;
};

static enum MHD_Result serve_file(struct MHD_Connection *connection, const char *url, int scan)
{
    return http_not_found(connection);
}

static enum MHD_Result on_http_connection(void *cls,
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

    const char *user_agent = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, MHD_HTTP_HEADER_USER_AGENT);
    const char *forwarded_for = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "X-Forwarded-For");
    //MHD_get_connection_values(connection, MHD_HEADER_KIND, (MHD_KeyValueIterator)&print_out_key, NULL);

    printf("URL:\t%s\n", url);

    // static resources
    if (url[strlen(url) - 1] != '/')
        return serve_file(connection, url, 1);
    else
    {
        // root document
#ifdef LOCAL
        return serve_file(connection, "/local.html", 0);
#else
        return serve_file(connection, "/test.html,", 0);
#endif
    }

    /*
    // pass the request to FORTRAN
    http_request_();

    response = MHD_create_response_from_buffer(strlen(page),
                                               (void *)page,
                                               MHD_RESPMEM_PERSISTENT);
    ret = MHD_queue_response(connection,
                             MHD_HTTP_OK,
                             response);
    MHD_destroy_response(response);

    return ret;*/

    return http_not_found(connection);
}

void SIGINTHandler(int sigint)
{
    printf("\tCTRL-C detected. Exiting the C part.\n");

    interrupted = 1; // this will terminate websockets

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
                                   &on_http_connection,
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

        // create a websockets thread
        int stat = pthread_create(&lws_tid, NULL, start_ws, NULL);
        stat = pthread_detach(lws_tid);
    }
}

extern void stop_http_()
{
    interrupted = 1; // this should terminate websockets

    if (http_server != NULL)
    {
        printf("shutting down the HTTP server... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;
        printf("done\n");
    }
}