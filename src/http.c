#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <pthread.h>

#include <microhttpd.h>
#include <libwebsockets.h>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/types.h>
#include <pwd.h>
#include <dirent.h>

#ifndef S_ISREG
#define S_ISREG(x) (S_IFREG == (x & S_IFREG))
#endif /* S_ISREG */

#include "json.h"

typedef void (*sighandler_t)(int);

extern void register_kill_signal_handler_(sighandler_t handler)
{
    signal(SIGINT, handler);
    signal(SIGTERM, handler);
}

extern void exit_fortran_();
extern void http_request_(char *string, int n);

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

static enum MHD_Result http_ok(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *okstr =
        "<html><body><div align='center'><p>200 OK Processing a request.</p></div><div align='center'><img src=\"/fortran.webp\" alt=\" Powered by Fortran 2018\" style = \"height:40px; margin-top:25px;\" ></div></ body></ html>";

    response =
        MHD_create_response_from_buffer(strlen(okstr),
                                        (void *)okstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_OK,
                               response);
        MHD_destroy_response(response);

        return ret;
    }
    else
        return MHD_NO;
};

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

static enum MHD_Result serve_file(struct MHD_Connection *connection, const char *url)
{
    int fd;
    struct stat buf;
    char path[1024];

    /* check for NULL strings */
    if (NULL == url)
        return http_not_found(connection);

    if (NULL != strstr(url, "../")) /* Very simplified check! */
        fd = -1;                    /* Do not allow usage of parent directories. */
    else
    {
        snprintf(path, sizeof(path), "htdocs%s", url);
        fd = open(path, O_RDONLY);
    }

    if (-1 != fd)
    {
        if ((0 != fstat(fd, &buf)) ||
            (!S_ISREG(buf.st_mode)))
        {
            /* not a regular file, refuse to serve */
            if (0 != close(fd))
                abort();
            fd = -1;
        }
    }

    if (-1 == fd)
        return http_not_found(connection);
    else
    {
        struct MHD_Response *response = MHD_create_response_from_fd64(buf.st_size, fd);

        if (NULL == response)
        {
            if (0 != close(fd))
                abort();
            return MHD_NO;
        }

        enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
        MHD_destroy_response(response);
        return ret;
    }
}

const char *get_filename_ext(const char *filename)
{
    const char *dot = strrchr(filename, '.');

    if (!dot || dot == filename)
        return "";

    return dot + 1;
}

static enum MHD_Result get_directory(struct MHD_Connection *connection, char *dir)
{
    printf("get_directory(%s)\n", dir);

    if (NULL == dir)
        return http_not_found(connection);

    char tmp[1024];
    size_t len = 0x4000;
    char *json = malloc(len);

    if (NULL == json)
        return MHD_NO;

    memset(json, '\0', len);

    struct dirent **namelist = NULL;
    int i, n;

    n = scandir(dir, &namelist, 0, alphasort);

    char *encoded = json_encode_string(dir);

    snprintf(tmp, sizeof(tmp), "{\"location\" : %s, \"contents\" : [", encoded);
    strncat(json, tmp, sizeof(tmp));

    if (encoded != NULL)
        free(encoded);

    int has_contents = 0;

    if (n < 0)
    {
        perror("scandir");

        strcat(json, "]}");
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            //printf("%s\n", namelist[i]->d_name);

            char pathname[1024];

            snprintf(pathname, sizeof(pathname), "%s/%s", dir, namelist[i]->d_name);

            struct stat sbuf;

            int err = stat(pathname, &sbuf);

            if (err == 0)
            {
                char last_modified[255];

                struct tm lm;
                localtime_r(&sbuf.st_mtime, &lm);
                strftime(last_modified, sizeof(last_modified) - 1, "%a, %d %b %Y %H:%M:%S %Z", &lm);

                size_t filesize = sbuf.st_size;

                if (S_ISDIR(sbuf.st_mode) && namelist[i]->d_name[0] != '.')
                {
                    char *encoded = json_encode_string(namelist[i]->d_name);

                    snprintf(tmp, sizeof(tmp), "{\"type\" : \"dir\", \"name\" : %s, \"last_modified\" : \"%s\"},", encoded, last_modified);
                    strncat(json, tmp, sizeof(tmp));
                    has_contents = 1;

                    if (encoded != NULL)
                        free(encoded);
                }

                if (S_ISREG(sbuf.st_mode))
                    if (!strcasecmp(get_filename_ext(namelist[i]->d_name), "fits"))
                    {
                        char *encoded = json_encode_string(namelist[i]->d_name);

                        snprintf(tmp, sizeof(tmp), "{\"type\" : \"file\", \"name\" : %s, \"size\" : %zu, \"last_modified\" : \"%s\"},", encoded, filesize, last_modified);
                        strncat(json, tmp, sizeof(tmp));
                        has_contents = 1;

                        if (encoded != NULL)
                            free(encoded);
                    };
            }
            else
                perror("stat64");

            free(namelist[i]);
        };

        //overwrite the the last ',' with a list closing character
        if (has_contents)
            json[strlen(json) - 1] = '\0';

        strcat(json, "]}");
    };

    if (namelist != NULL)
        free(namelist);

    if (dir != NULL)
        free(dir);

    struct MHD_Response *response = MHD_create_response_from_buffer(strlen(json), (void *)json, MHD_RESPMEM_MUST_FREE);

    MHD_add_response_header(response, "Cache-Control", "no-cache");
    MHD_add_response_header(response, "Cache-Control", "no-store");
    MHD_add_response_header(response, "Pragma", "no-cache");
    MHD_add_response_header(response, "Content-Type", "application/json; charset=utf-8");

    enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

    MHD_destroy_response(response);

    return ret;
}

static enum MHD_Result get_home_directory(struct MHD_Connection *connection)
{
    struct passwd *passwdEnt = getpwuid(getuid());
    char *home = passwdEnt->pw_dir;

    if (home != NULL)
    {
        char *dir = strdup(home);

        return get_directory(connection, dir);
    }
    else
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
    enum MHD_Result ret;

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

#ifdef LOCAL
    if (0 == strcmp(url, "/get_directory"))
    {
        char *pointer = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");

        if (NULL != pointer)
        {
            char *dir = strdup(pointer);

            return get_directory(connection, dir);
        }
        else
            return get_home_directory(connection);
    };
#endif

    if (strstr(url, "FITSWebQL.html") != NULL)
    {
#ifndef LOCAL
        //get the root path
        char *proot = (char *)strstr(url, "FITSWebQL.html");

        int len = proot - url;
        char *root = strndup(url, len);

        printf("URL root path: %s\n", root);
#endif

        //get datasetId
        char **datasetId = NULL;
        int va_count = 0;

#ifdef LOCAL
        char *directory = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");
        char *extension = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ext");
        char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

        //auto-detect multiple entries
        if (tmp == NULL)
        {
            char str_key[255] = "";

            sprintf(str_key, "filename%d", va_count + 1);

            while ((tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, str_key)) != NULL)
            {
                va_count++;
                printf("argument %d:%s\n", va_count, tmp);
                sprintf(str_key, "filename%d", va_count + 1);

                datasetId = (char **)realloc(datasetId, va_count * sizeof(char *));
                datasetId[va_count - 1] = tmp;
            }

            printf("number of arguments: %d\n", va_count);
        }
        else
        {
            va_count = 1;

            //allocate datasetId
            datasetId = (char **)malloc(sizeof(char *));
            datasetId[0] = tmp;
        }
#else
        char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "datasetId");

        //auto-detect multiple lines
        if (tmp == NULL)
        {
            char str_key[255] = "";

            sprintf(str_key, "datasetId%d", va_count + 1);

            while ((tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, str_key)) != NULL)
            {
                va_count++;
                printf("argument %d:%s\n", va_count, tmp);
                sprintf(str_key, "datasetId%d", va_count + 1);

                datasetId = (char **)realloc(datasetId, va_count * sizeof(char *));
                datasetId[va_count - 1] = tmp;
            }

            printf("number of arguments: %d\n", va_count);
        }
        else
        {
            va_count = 1;

            //allocate datasetId
            datasetId = (char **)malloc(sizeof(char *));
            datasetId[0] = tmp;
        }
#endif

        if (datasetId != NULL)
        {
            ret = http_ok(connection);

            // pass the filepath to FORTRAN
#ifdef LOCAL
            // make a filepath from the dir/extension
            int i;
            char filepath[1024];
            memset(filepath, '\0', sizeof(filepath));

            for (i = 0; i < va_count; i++)
            {
                if (directory != NULL)
                {
                    if (extension == NULL)
                        snprintf(filepath, sizeof(filepath), "%s/%s.fits", directory, datasetId[i]);
                    else
                        snprintf(filepath, sizeof(filepath), "%s/%s.%s", directory, datasetId[i], extension);
                }

                printf("[C] FITS filepath:\t%s\n", filepath);
                http_request_(filepath, strlen(filepath));
            }

            // directory/extension should not be freed (libmicrohttpd does that)

#else
            // get the full path from the postgresql db, then call FORTRAN
#endif
        }
        else
            ret = http_not_found(connection);

        // deallocate datasetId
        if (datasetId != NULL)
            free(datasetId);

        return ret;
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

    // static resources
    if (url[strlen(url) - 1] != '/')
        return serve_file(connection, url);
    else
    {
        // root document
#ifdef LOCAL
        return serve_file(connection, "/local.html");
#else
        return serve_file(connection, "/test.html");
#endif
    }

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

    http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
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