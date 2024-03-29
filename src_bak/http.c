#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#include <signal.h>
#include <pthread.h>
#include <libcpuid.h>

#define __USE_XOPEN
#include <time.h>

#include <sqlite3.h>

inline const char *denull(const char *str)
{
    if (str != NULL)
        return str;
    else
        return "\"\"";
};

#ifndef LOCAL
#if defined(__APPLE__) && defined(__MACH__)
#include <libpq-fe.h>
#else
#include <libpq-fe.h>
//#include <pgsql/libpq-fe.h>
#endif
#endif

static sqlite3 *splat_db = NULL;

#include <microhttpd.h>
#include <libwebsockets.h>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <pwd.h>
#include <dirent.h>

#include "hash_table.h"

// ZFP floating-point compressor
#include <zfp.h>

#define ZFP_HIGH_PRECISION 16
#define ZFP_MEDIUM_PRECISION 11
#define ZFP_LOW_PRECISION 8

// FPzip
#include <fpzip.h>

#define FPZIP_MEDIUM_PRECISION 16
#define FPZIP_HIGH_PRECISION 24

// LZ4 character streams compressor
#include <lz4hc.h>

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

extern void exit_fortran();
extern void fitswebql_request(char *uri, size_t n);
extern void image_spectrum_request(void *item, int width, int height, int precision, int fetch_data, int fd);
extern int get_error_status(void *item);
extern int get_header_status(void *item);
extern int get_ok_status(void *item);
extern float get_progress(void *item);
extern float get_elapsed(void *item);
extern void get_frequency_range(void *item, double *freq_start_ptr, double *freq_end_ptr);

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

#include <zlib.h>

/* CHUNK is the size of the memory chunk used by the zlib & other routines. */

#define CHUNK 0x4000
#define _windowBits 15
#define GZIP_ENCODING 16

/* The following macro calls a zlib routine and checks the return
   value. If the return value ("status") is not OK, it prints an error
   message and exits the program. Zlib's error statuses are all less
   than zero. */

#define CALL_ZLIB(x)                                                              \
    {                                                                             \
        int status;                                                               \
        status = x;                                                               \
        if (status < 0)                                                           \
        {                                                                         \
            fprintf(stderr, "%s:%d: %s returned a bad status of %d.\n", __FILE__, \
                    __LINE__, #x, status);                                        \
            /*exit(EXIT_FAILURE);*/                                               \
        }                                                                         \
    }

size_t chunked_write(int fd, const char *src, size_t n);
extern void write_header(int fd, const char *header_str, int str_len);

struct arg_struct
{
    void *item;
    int width;
    int height;
    int precision;
    int fetch_data;
    int fd;
};

struct splat_req
{
    bool first;
    bool compression;
    double freq_start;
    double freq_end;
    int fd;

    // optional gzip compression
    z_stream z;
    unsigned char out[CHUNK];
};

void *handle_image_spectrum_request(void *args);
void *handle_fitswebql_request(void *uri);
void *stream_molecules(void *args);
static int sqlite_callback(void *userp, int argc, char **argv, char **azColName);

#define VERSION_MAJOR 5
#define VERSION_MINOR 0
#define VERSION_SUB 0

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "21.04.XX.X"
#define VERSION_STRING "SV2021-06-XX.X-ALPHA"

#define HTTP_PORT 8080
#define WS_PORT (HTTP_PORT + 1)

// WS
static volatile int interrupted = 0;
pthread_t lws_tid;

#define LWS_PLUGIN_STATIC
#include "ws_ring_thread.c"

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

/*
 * This demonstrates how to pass a pointer into a specific protocol handler
 * running on a specific vhost.  In this case, it's our default vhost and
 * we pass the pvo named "config" with the value a const char * "myconfig".
 *
 * This is the preferred way to pass configuration into a specific vhost +
 * protocol instance.
 */

static const struct lws_protocol_vhost_options pvo_ops = {
    NULL,
    NULL,
    "config",          /* pvo name */
    (void *)"myconfig" /* pvo value */
};

static const struct lws_protocol_vhost_options pvo = {
    NULL,          /* "next" pvo linked-list */
    &pvo_ops,      /* "child" pvo linked-list */
    "fitswebqlse", /* protocol name we belong to on this vhost */
    ""             /* ignored */
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
    info.pvo = &pvo; /* per-vhost options */
    // info.vhost_name = "localhost";
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
static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite);

static enum MHD_Result print_out_key(void *cls, enum MHD_ValueKind kind, const char *key, const char *value, size_t value_size)
{
    printf("[C] %s: %s\n", key, value);
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

static enum MHD_Result http_accepted(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "<html><body>202 Accepted</body></html>";

    response =
        MHD_create_response_from_buffer(strlen(errorstr),
                                        (void *)errorstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_ACCEPTED,
                               response);
        MHD_destroy_response(response);

        return ret;
    }
    else
        return MHD_NO;
};

static enum MHD_Result http_internal_server_error(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "<html><body>500 Internal Server Error</body></html>";

    response =
        MHD_create_response_from_buffer(strlen(errorstr),
                                        (void *)errorstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_INTERNAL_SERVER_ERROR,
                               response);
        MHD_destroy_response(response);

        return ret;
    }
    else
        return MHD_NO;
};

static enum MHD_Result http_not_implemented(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "<html><body>501 Not Implemented</body></html>";

    response =
        MHD_create_response_from_buffer(strlen(errorstr),
                                        (void *)errorstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_NOT_IMPLEMENTED,
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
    char last_modified[255];
    char last_etag[255];

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

        struct tm tm = {0};
        const char *modified = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "If-Modified-Since");
        const char *etag = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "If-None-Match");

        if (modified != NULL)
        {
            if (strptime(modified, "%a, %d %b %Y %H:%M:%S %Z", &tm) != NULL)
                printf("[C] If-Modified-Since: %s\n", modified);
        };

        struct tm lm;
        gmtime_r(&buf.st_mtime, &lm);
        strftime(last_modified, sizeof(last_modified) - 1, "%a, %d %b %Y %H:%M:%S %Z", &lm);
        strftime(last_etag, sizeof(last_etag) - 1, "%a%d%b%Y%H%M%S%Z", &lm);

        bool unmodified = false;

        // if(difftime(mktime(&lm), mktime(&tm)) <= 0)
        if (difftime(timegm(&lm), timegm(&tm)) <= 0)
            unmodified = true;

        if (etag != NULL)
        {
            if (strcmp(etag, last_etag) == 0)
                unmodified = true;
        };

        if (unmodified)
        {
            printf("[C] %s: sending HTTP 304\n", url);

            close(fd);

            struct MHD_Response *response = MHD_create_response_from_buffer(0, NULL, MHD_RESPMEM_PERSISTENT);

            MHD_add_response_header(response, "Cache-Control", "public, max-age=86400");
            MHD_add_response_header(response, MHD_HTTP_HEADER_ETAG, last_etag);

            enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_NOT_MODIFIED, response);
            MHD_destroy_response(response);

            return ret;
        };
    }

    if (-1 == fd)
        return http_not_found(connection);
    else
    {
        struct MHD_Response *response = MHD_create_response_from_fd(buf.st_size, fd);

        if (NULL == response)
        {
            if (0 != close(fd))
                abort();
            return MHD_NO;
        }

        // detect mime-types
        char *pos = NULL;

        pos = (char *)strstr(url, ".htm");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "text/html");

        pos = (char *)strstr(url, ".html");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "text/html");

        pos = (char *)strstr(url, ".txt");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "text/plain");

        pos = (char *)strstr(url, ".js");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "application/javascript; charset=utf-8");

        pos = (char *)strstr(url, ".wasm");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "application/wasm");

        pos = (char *)strstr(url, ".ico");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/x-icon");

        pos = (char *)strstr(url, ".png");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/png");

        pos = (char *)strstr(url, ".gif");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/gif");

        pos = (char *)strstr(url, ".webp");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/webp");

        pos = (char *)strstr(url, ".jpeg");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/jpeg");

        pos = (char *)strstr(url, ".mp4");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "video/mp4");

        pos = (char *)strstr(url, ".css");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "text/css");

        pos = (char *)strstr(url, ".pdf");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "application/pdf");

        pos = (char *)strstr(url, ".svg");
        if (pos != NULL)
            MHD_add_response_header(response, "Content-Type", "image/svg+xml");

        MHD_add_response_header(response, "Cache-Control", "public, max-age=86400"); // 86400
        MHD_add_response_header(response, MHD_HTTP_HEADER_ETAG, last_etag);

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
    printf("[C] get_directory(%s)\n", dir);

    if (NULL == dir)
        return http_not_found(connection);

    GString *json = g_string_sized_new(1024);

    if (NULL == json)
        return MHD_NO;

    struct dirent **namelist = NULL;
    int i, n;

    n = scandir(dir, &namelist, 0, alphasort);

    char *encoded = json_encode_string(dir);

    g_string_printf(json, "{\"location\" : %s, \"contents\" : [", encoded);

    if (encoded != NULL)
        free(encoded);

    int has_contents = 0;

    if (n < 0)
    {
        perror("scandir");
        g_string_append(json, "]}");
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            // printf("%s\n", namelist[i]->d_name);

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

                    g_string_append_printf(json, "{\"type\" : \"dir\", \"name\" : %s, \"last_modified\" : \"%s\"},", encoded, last_modified);
                    has_contents = 1;

                    if (encoded != NULL)
                        free(encoded);
                }

                if (S_ISREG(sbuf.st_mode))
                    if (!strcasecmp(get_filename_ext(namelist[i]->d_name), "fits"))
                    {
                        char *encoded = json_encode_string(namelist[i]->d_name);

                        g_string_append_printf(json, "{\"type\" : \"file\", \"name\" : %s, \"size\" : %zu, \"last_modified\" : \"%s\"},", encoded, filesize, last_modified);
                        has_contents = 1;

                        if (encoded != NULL)
                            free(encoded);
                    };
            }
            else
                perror("stat64");

            free(namelist[i]);
        };

        // overwrite the the last ',' with a list closing character
        if (has_contents)
            g_string_truncate(json, json->len - 1);

        g_string_append(json, "]}");
    };

    if (namelist != NULL)
        free(namelist);

    if (dir != NULL)
        free(dir);

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json->len, (void *)json->str, g_free);
    g_string_free(json, FALSE);

    MHD_add_response_header(response, "Cache-Control", "no-cache");
    MHD_add_response_header(response, "Cache-Control", "no-store");
    MHD_add_response_header(response, "Pragma", "no-cache");
    MHD_add_response_header(response, "Content-Type", "application/json; charset=utf-8");

    enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

    MHD_destroy_response(response);

    return ret;
}

static enum MHD_Result send_progress(struct MHD_Connection *connection, float progress, float elapsed)
{
    GString *json = g_string_sized_new(128);

    g_string_printf(json, "{\"progress\" : %f, \"elapsed\" : %f}", progress, elapsed);

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json->len, (void *)json->str, g_free);
    g_string_free(json, FALSE);

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

    // accept both "GET" and "PUT"
    // if (0 != strcmp(method, "GET"))
    //    return MHD_NO; /* unexpected method */

    if (&dummy != *ptr)
    {
        /* The first time only the headers are valid,
         do not respond in the first round... */
        *ptr = &dummy;
        return MHD_YES;
    }

    if (0 != *upload_data_size && 0 == strcmp(method, "GET"))
        return MHD_NO; /* upload data in a GET!? */

    *ptr = NULL; /* clear context pointer */

    const char *user_agent = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, MHD_HTTP_HEADER_USER_AGENT);
    const char *forwarded_for = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "X-Forwarded-For");
    const char *encoding = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "Accept-Encoding");
    // MHD_get_connection_values(connection, MHD_HEADER_KIND, (MHD_KeyValueIterator)&print_out_key, NULL);

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

    if (strstr(url, "/heartbeat/") != NULL)
    {
        char *timestamp = strrchr(url, '/');

        if (timestamp != NULL)
        {
            timestamp++;

            struct MHD_Response *response =
                MHD_create_response_from_buffer(strlen(timestamp),
                                                (void *)timestamp,
                                                MHD_RESPMEM_MUST_COPY);
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
        }
        else
            return http_not_found(connection);
    }

    if (strstr(url, "/progress/") != NULL)
    {
        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
        {
            datasetId++;

            void *item = get_dataset(datasetId);

            if (item == NULL)
                return http_accepted(connection);

            float progress = get_progress(item);
            float elapsed = get_elapsed(item);

            // printf("[C] [progress] datasetId(%s): %f%% in %f [s]\n", datasetId, progress, elapsed);

            return send_progress(connection, progress, elapsed);
        }
        else
            return http_not_found(connection);
    }

    if (strstr(url, "/get_molecules") != NULL)
    {
        char *datasetId = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "datasetId");
        char *freqStartStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "freq_start");
        char *freqEndStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "freq_end");

        bool compress = false;
        double freq_start = 0.0;
        double freq_end = 0.0;

        double *freq_start_ptr = &freq_start;
        double *freq_end_ptr = &freq_end;

        int status;
        int pipefd[2];
        pthread_t tid;

        if (freqStartStr != NULL)
            freq_start = atof(freqStartStr);

        if (freqEndStr != NULL)
            freq_end = atof(freqEndStr);

        printf("[C] Accept-Encoding: %s\n", encoding);

        if (strstr(encoding, "gzip") != NULL)
            compress = true;

        if (splat_db == NULL)
            return http_internal_server_error(connection);

        if (datasetId == NULL)
            return http_not_found(connection);

        if (freq_start == 0.0 || freq_end == 0.0)
        {
            // get the frequency range from the FITS header
            void *item = get_dataset(datasetId);

            if (item == NULL)
                return http_accepted(connection);

            if (get_error_status(item))
                return http_internal_server_error(connection);

            if (!get_header_status(item))
                return http_accepted(connection);

            get_frequency_range(item, freq_start_ptr, freq_end_ptr);
        }

        printf("[C] get_molecules: datasetId(%s); freq_start: %gGHz, freq_end: %gGHz\n", datasetId, freq_start, freq_end);

        if (freq_start > 0.0 && freq_end > 0.0)
        {
            // open a pipe
            status = pipe(pipefd);

            if (0 != status)
                return http_internal_server_error(connection);

            // create a response from the pipe by passing the read end of the pipe
            struct MHD_Response *response = MHD_create_response_from_pipe(pipefd[0]);

            // add headers
            /*MHD_add_response_header(response, "Cache-Control", "no-cache");
            MHD_add_response_header(response, "Cache-Control", "no-store");
            MHD_add_response_header(response, "Pragma", "no-cache");*/

            MHD_add_response_header(response, "Cache-Control", "public, max-age=86400");
            MHD_add_response_header(response, "Content-Type", "application/json");

            if (compress)
                MHD_add_response_header(response, "Content-Encoding", "gzip");

            // queue the response
            enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

            MHD_destroy_response(response);

            printf("[C] calling stream_molecules with the pipe file descriptor %d\n", pipefd[1]);

            struct splat_req *args = malloc(sizeof(struct splat_req));

            if (args != NULL)
            {
                args->compression = compress;
                args->freq_start = freq_start;
                args->freq_end = freq_end;
                args->fd = pipefd[1];

                // create and detach the thread
                pthread_create(&tid, NULL, &stream_molecules, args);
                pthread_detach(tid);
            }
            else
                close(pipefd[1]);

            return ret;
        }
        else
            return http_not_found(connection);
    }

    if (strstr(url, "/image_spectrum") != NULL)
    {
        int fetch_data = 0;
        int width, height;
        int precision = ZFP_MEDIUM_PRECISION; // default ZFP precision

        int status;
        int pipefd[2];
        pthread_t tid;

        char *datasetId = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "datasetId");
        char *widthStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "width");
        char *heightStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "height");
        char *qualityStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "quality");
        char *fetch_dataStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "fetch_data");

        if (datasetId == NULL || widthStr == NULL || heightStr == NULL)
            return http_not_found(connection);

        if (fetch_dataStr != NULL)
            if (0 == strcmp(fetch_dataStr, "true"))
                fetch_data = 1;

        width = atoi(widthStr);
        height = atoi(heightStr);

        if (qualityStr != NULL)
        {
            if (0 == strcmp(qualityStr, "high"))
                precision = ZFP_HIGH_PRECISION;

            if (0 == strcmp(qualityStr, "medium"))
                precision = ZFP_MEDIUM_PRECISION;

            if (0 == strcmp(qualityStr, "low"))
                precision = ZFP_LOW_PRECISION;
        }

        // printf("[C] datasetId(%s), width(%d), height(%d), quality(%s), fetch_data: %s\n", datasetId, width, height, quality, (fetch_data ? "true" : "false"));

        if (width <= 0 || height <= 0)
            return http_not_implemented(connection);

        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_accepted(connection);

        if (get_error_status(item))
            return http_internal_server_error(connection);

        if (!get_ok_status(item))
            return http_accepted(connection);

        // open a pipe
        status = pipe(pipefd);

        if (0 != status)
            return http_internal_server_error(connection);

        // create a response from the pipe by passing the read end of the pipe
        struct MHD_Response *response = MHD_create_response_from_pipe(pipefd[0]);

        // add headers
        MHD_add_response_header(response, "Cache-Control", "no-cache");
        MHD_add_response_header(response, "Cache-Control", "no-store");
        MHD_add_response_header(response, "Pragma", "no-cache");
        MHD_add_response_header(response, "Content-Type", "application/octet-stream");

        // queue the response
        enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

        MHD_destroy_response(response);

        // the code below should be run in a separate thread
        // otherwise libmicrohttpd will not have a chance to read from the pipe
        // alternatively the pipe capacity should be increased
        // with fcntl(F_SETPIPE_SZ)

        // respond with the image + JSON data (header, spectrum, histogram)
        // pass the write end of the pipe to Fortran
        // the binary response data will be generated in Fortran
        printf("[C] calling image_spectrum_request with the pipe file descriptor %d\n", pipefd[1]);

        struct arg_struct *args = malloc(sizeof(struct arg_struct));

        if (args != NULL)
        {
            args->item = get_dataset(datasetId);
            args->width = width;
            args->height = height;
            args->precision = precision;
            args->fetch_data = fetch_data;
            args->fd = pipefd[1];

            // create and detach the thread
            pthread_create(&tid, NULL, &handle_image_spectrum_request, args);
            pthread_detach(tid);
        }
        else
            close(pipefd[1]);

        return ret;
    }

    if (strstr(url, "FITSWebQL.html") != NULL)
    {
#ifndef LOCAL
        // get the root path
        char *proot = (char *)strstr(url, "FITSWebQL.html");

        int len = proot - url;
        char *root = strndup(url, len);

        printf("[C] URL root path: %s\n", root);
#endif

        // get datasetId
        char **datasetId = NULL;
        int va_count = 0;

#ifdef LOCAL
        char *directory = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");
        char *extension = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ext");
        char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

        // auto-detect multiple entries
        if (tmp == NULL)
        {
            char str_key[255] = "";

            sprintf(str_key, "filename%d", va_count + 1);

            while ((tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, str_key)) != NULL)
            {
                va_count++;
                printf("[C] argument %d:%s\n", va_count, tmp);
                sprintf(str_key, "filename%d", va_count + 1);

                datasetId = (char **)realloc(datasetId, va_count * sizeof(char *));
                datasetId[va_count - 1] = tmp;
            }

            printf("[C] number of arguments: %d\n", va_count);
        }
        else
        {
            va_count = 1;

            // allocate datasetId
            datasetId = (char **)malloc(sizeof(char *));
            datasetId[0] = tmp;
        }
#else
        char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "datasetId");

        // auto-detect multiple lines
        if (tmp == NULL)
        {
            char str_key[255] = "";

            sprintf(str_key, "datasetId%d", va_count + 1);

            while ((tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, str_key)) != NULL)
            {
                va_count++;
                printf("[C] argument %d:%s\n", va_count, tmp);
                sprintf(str_key, "datasetId%d", va_count + 1);

                datasetId = (char **)realloc(datasetId, va_count * sizeof(char *));
                datasetId[va_count - 1] = tmp;
            }

            printf("[C] number of arguments: %d\n", va_count);
        }
        else
        {
            va_count = 1;

            // allocate datasetId
            datasetId = (char **)malloc(sizeof(char *));
            datasetId[0] = tmp;
        }
#endif

        char *view = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "view");
        char *flux = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "flux");
        char *db = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "db");
        char *table = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "table");

        int composite = 0;

        if (view != NULL)
            composite = (strcasecmp("composite", view) == 0) ? 1 : 0;

        if (datasetId != NULL)
        {
            ret = execute_alma(connection, datasetId, va_count, composite);

            // pass the filepath to FORTRAN
#ifdef LOCAL
            // make a filepath from the dir/extension
            int i;
            char filepath[1024];
            memset(filepath, '\0', sizeof(filepath));

            for (i = 0; i < va_count; i++)
            {
                pthread_t tid;

                // try to insert a NULL dataset

                if (insert_if_not_exists(datasetId[i], NULL))
                    continue;

                if (directory != NULL)
                {
                    if (extension == NULL)
                        snprintf(filepath, sizeof(filepath), "%s/%s.fits", directory, datasetId[i]);
                    else
                        snprintf(filepath, sizeof(filepath), "%s/%s.%s", directory, datasetId[i], extension);
                }

                printf("[C] FITS filepath:\t%s\n", filepath);

                pthread_create(&tid, NULL, &handle_fitswebql_request, strndup(filepath, sizeof(filepath)));
                pthread_detach(tid);
            }

            // directory/extension should not be freed (libmicrohttpd does that)

#else
            ret = http_ok(connection);
            // get the full path from the postgresql db

            // if a file does not exist form a download URL (jvox...)

            // then call FORTRAN with a filepath or URL
#endif
        }
        else
            ret = http_not_found(connection);

        // deallocate datasetId
        if (datasetId != NULL)
            free(datasetId);

#ifndef LOCAL
        if (root != NULL)
            free(root);
#endif

        return ret;
    }

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
        printf("[C] shutting down the HTTP server... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;

        // stop accepting new connections
        // MHD_quiesce_daemon(http_server);
    };

    printf("[C] clean shutdown completed.\n");

    exit_fortran();
};

extern void close_pipe(int fd)
{
    int status;

    // close a pipe (to be called from Fortran)
    status = close(fd);

    if (0 != status)
        printf("[C] close_pipe status: %d\n", status);
}

extern int get_physical_cores()
{
    struct cpu_raw_data_t raw;
    struct cpu_id_t data;

    cpuid_get_raw_data(&raw);
    cpu_identify(&raw, &data);
    // printf("[C] No. of Physical Core(s) : %d\n", data.num_cores);

    return data.num_cores;
}

extern void start_http()
{

    signal(SIGPIPE, SIG_IGN); // ignore SIGPIPE
    // signal(SIGINT, SIGINTHandler); //intercept CTRL+C to trigger a clean shutdown

    // http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
    http_server = MHD_start_daemon(MHD_USE_AUTO | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
                                   HTTP_PORT,
                                   NULL,
                                   NULL,
                                   &on_http_connection,
                                   PAGE,
                                   MHD_OPTION_END);

    if (http_server == NULL)
    {
        printf("[C] Could not start a libmicrohttpd web server.\n");
        return;
    }
    else
    {
        int rc = sqlite3_open_v2("splatalogue_v3.db", &splat_db, SQLITE_OPEN_READONLY | SQLITE_OPEN_FULLMUTEX, NULL);

        if (rc)
        {
            fprintf(stderr, "Can't open local splatalogue database: %s\n", sqlite3_errmsg(splat_db));
            sqlite3_close(splat_db);
            splat_db = NULL;
        }

        printf("[C] µHTTP daemon listening on port %d... Press CTRL-C to stop it.\n", HTTP_PORT);

        // create a websockets thread
        /*int stat = pthread_create(&lws_tid, NULL, start_ws, NULL);
        stat = pthread_detach(lws_tid);*/

        // a blocking mode
        start_ws(NULL);
    }
}

extern void stop_http()
{
    interrupted = 1; // this should terminate websockets

    if (http_server != NULL)
    {
        printf("[C] shutting down the HTTP server... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;
        printf("done\n");
    }

    if (splat_db != NULL)
    {
        sqlite3_close(splat_db);
        splat_db = NULL;
    }
}

void include_file(GString *str, const char *filename)
{
    int fd = -1;
    void *buffer = NULL;

    struct stat st;
    stat(filename, &st);
    long size = st.st_size;

    fd = open(filename, O_RDONLY);

    if (fd != -1)
    {
        buffer = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);

        if (buffer != MAP_FAILED)
        {
            g_string_append_len(str, (const char *)buffer, size);

            if (munmap(buffer, size) == -1)
                perror("un-mapping error");
        }
        else
            perror("error mapping a file");

        close(fd);
    };
}

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite)
{
    unsigned int i;
    bool has_fits = true;

    // go through the dataset list looking up entries in the hash table
    for (i = 0; i < va_count; i++)
        has_fits = has_fits && dataset_exists(va_list[i]);

    // the string holding the dynamically generated HTML content
    GString *html = g_string_new("<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n");

    g_string_append(html,
                    "<link href=\"https://fonts.googleapis.com/css?family=Inconsolata\" "
                    "rel=\"stylesheet\"/>\n");
    g_string_append(html,
                    "<link href=\"https://fonts.googleapis.com/css?family=Material+Icons\" "
                    "rel=\"stylesheet\"/>\n");
    g_string_append(html, "<script src=\"https://d3js.org/d3.v5.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/reconnecting-websocket.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"//cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.6/"
                          "numeral.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/ra_dec_conversion.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/sylvester.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/shortcut.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/colourmaps.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/lz4.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/marchingsquares-isocontours.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/marchingsquares-isobands.min.js\"></script>\n");

    // OpenEXR WASM decoder
    g_string_append(html, "<script "
                          "src=\"client." WASM_VERSION ".js\"></script>\n");
    /*html.append("<script "
              "src=\"https://cdn.jsdelivr.net/gh/jvo203/FITSWebQL@master/" +
              docs_root +
              "/"
              "fitswebql/exr." WASM_VERSION ".min.js\"></script>\n");*/
    g_string_append_printf(html, "<script>\n"
                                 "Module.ready\n"
                                 "\t.then(status => console.log(status))\n"
                                 "\t.catch(e => console.error(e));\n"
                                 "</script>\n");

    // bootstrap
    g_string_append(html,
                    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, "
                    "user-scalable=no, minimum-scale=1, maximum-scale=1\">\n");

    // version 3.3.7
    /*g_string_append(html, "<link rel=\"stylesheet\" "
                          "href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/"
                          "bootstrap.min.css\">\n");
    g_string_append(html, "<script "
                          "src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/"
                          "jquery.min.js\"></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/"
                          "bootstrap.min.js\"></script>\n");*/

    // version 3.4.1
    g_string_append(html, "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css\" integrity=\"sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu\" crossorigin=\"anonymous\">");
    g_string_append(html, "<script src=\"https://code.jquery.com/jquery-1.12.4.min.js\" integrity=\"sha384-nvAa0+6Qg9clwYCGGPpDQLVpLNn0fRaROjHqs13t4Ggj3Ez50XnGQqc/r8MhnRDZ\" crossorigin=\"anonymous\"></script>");
    g_string_append(html, "<script src=\"https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js\" integrity=\"sha384-aJ21OjlMXNL5UyIl/XNwTMqvzeRMZH2w8c5cRVpzpU8Y5bApTppSuUkhZXN0VxHd\" crossorigin=\"anonymous\"></script>");

    // GLSL vertex shader
    g_string_append(html, "<script id=\"vertex-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/vertex-shader.vert");
    g_string_append(html, "</script>\n");

    g_string_append(html,
                    "<script id=\"legend-vertex-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/legend-vertex-shader.vert");
    g_string_append(html, "</script>\n");

    // GLSL fragment shaders
    g_string_append(html, "<script id=\"common-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/common-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html,
                    "<script id=\"legend-common-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/legend-common-shader.frag");
    g_string_append(html, "</script>\n");

    // tone mappings
    g_string_append(html, "<script id=\"ratio-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/ratio-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"logistic-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/logistic-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"square-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/square-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"legacy-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/legacy-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"linear-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/linear-shader.frag");
    g_string_append(html, "</script>\n");

    // colourmaps
    g_string_append(html, "<script id=\"greyscale-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/greyscale-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"negative-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/negative-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"amber-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/amber-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"red-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/red-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"green-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/green-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"blue-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/blue-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"hot-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/hot-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"rainbow-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/rainbow-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"parula-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/parula-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"inferno-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/inferno-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"magma-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/magma-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"plasma-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/plasma-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"viridis-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/viridis-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"cubehelix-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/cubehelix-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"jet-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/jet-shader.frag");
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"haxby-shader\" type=\"x-shader/x-vertex\">\n");
    include_file(html, "htdocs/fitswebql/haxby-shader.frag");
    g_string_append(html, "</script>\n");

    // FITSWebQL main JavaScript + CSS
    g_string_append(html, "<script src=\"fitswebqlse_for.js?" VERSION_STRING "\"></script>\n");
    g_string_append(html, "<link rel=\"stylesheet\" href=\"fitswebqlse.css?" VERSION_STRING
                          "\"/>\n");

    // HTML content
    g_string_append(html, "<title>FITSWEBQLSE</title></head><body>\n");
    g_string_append_printf(html, "<div id='votable' style='width: 0; height: 0;' data-va_count='%d' ", va_count);

    if (va_count == 1)
        g_string_append_printf(html, "data-datasetId='%s' ", va_list[0]);
    else
    {
        for (i = 0; i < va_count; i++)
            g_string_append_printf(html, "data-datasetId%d='%s' ", (i + 1), va_list[i]);

        if (composite && va_count <= 3)
            g_string_append(html, "data-composite='1' ");
    }

#ifndef LOCAL
    g_string_append_printf(html, "data-root-path='%s/' ", root);
#else
    g_string_append(html, "data-root-path='/' ");
#endif

    g_string_append(html, " data-server-version='" VERSION_STRING "' data-server-string='" SERVER_STRING);
#ifdef LOCAL
    g_string_append(html, "' data-server-mode='LOCAL");
#else
    g_string_append(html, "' data-server-mode='SERVER");
#endif
    g_string_append_printf(html, "' data-has-fits='%d'></div>\n", (has_fits ? 1 : 0));

#ifdef PRODUCTION
    g_string_append(html, "<script>var WS_SOCKET = 'wss://';</script>\n");
#else
    g_string_append(html, "<script>var WS_SOCKET = 'ws://';</script>\n");
#endif

    g_string_append_printf(html, "<script>var WS_PORT = %d;</script>\n", WS_PORT);

    // the page entry point
    g_string_append(
        html, "<script>"
              "const golden_ratio = 1.6180339887;"
              "var ALMAWS = null ;"
              "var wsVideo = null ;"
              "var wsConn = null;"
              "var firstTime = true;"
              "var has_image = false;"
              "var PROGRESS_VARIABLE = 0.0;"
              "var PROGRESS_INFO = '' ;"
              "var RESTFRQ = 0.0;"
              "var USER_SELFRQ = 0.0;"
              "var USER_DELTAV = 0.0;"
              "var ROOT_PATH = '/fitswebql/';"
              "var idleResize = -1;"
              "window.onresize = resizeMe;"
              "window.onbeforeunload = function() {"
              "    if (wsConn != null)"
              "    {"
              "        for (let i = 0; i < va_count; i++)"
              "            wsConn[i].close();"
              "    }"
              ""
              "          if (wsVideo != null)"
              "             wsVideo.close();"
              "    };"
              "mainRenderer(); </script>\n");

    g_string_append(html, "</body></html>");

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(html->len, (void *)html->str, g_free);
    // deallocate the html content after libmicrohttpd has taken ownership of the string
    g_string_free(html, FALSE);

    MHD_add_response_header(response, "Cache-Control", "no-cache");
    MHD_add_response_header(response, "Cache-Control", "no-store");
    MHD_add_response_header(response, "Pragma", "no-cache");
    MHD_add_response_header(response, "Content-Type", "text/html; charset=utf-8");

    enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

    MHD_destroy_response(response);

    return ret;
}

extern void write_spectrum(int fd, const float *spectrum, int n, int precision)
{
    bool success;
    void *compressed;
    size_t bufbytes, outbytes;
    uint32_t length;
    uint32_t transmitted_size;
    FPZ *fpz;

    length = n;
    bufbytes = 1024 + length * sizeof(float);
    outbytes = 0;

    success = false;
    compressed = malloc(bufbytes);

    if (compressed != NULL)
    {
        // compress to memory
        fpz = fpzip_write_to_buffer(compressed, bufbytes);
        fpz->type = FPZIP_TYPE_FLOAT;
        fpz->prec = precision;
        fpz->nx = n;
        fpz->ny = 1;
        fpz->nz = 1;
        fpz->nf = 1;

        // write header
        if (!fpzip_write_header(fpz))
            fprintf(stderr, "[C] cannot write the FPzip header: %s\n", fpzip_errstr[fpzip_errno]);
        else
        {
            outbytes = fpzip_write(fpz, spectrum);

            if (!outbytes)
                fprintf(stderr, "[C] FPzip compression failed: %s\n", fpzip_errstr[fpzip_errno]);
            else
                success = true;
        }

        fpzip_write_close(fpz);

        if (success)
        {
            printf("[C] float array size: %zu, compressed: %zu bytes\n", length * sizeof(float), outbytes);

            // transmit the data
            uint32_t transmitted_size = outbytes;

            chunked_write(fd, (const char *)&length, sizeof(length));                     // spectrum length after decompressing
            chunked_write(fd, (const char *)&transmitted_size, sizeof(transmitted_size)); // compressed buffer size
            chunked_write(fd, compressed, outbytes);
        }

        free(compressed);
    }
}

extern void write_json(int fd, GString *json)
{
    if (json == NULL)
        return;

    printf("[C] JSON(%s)\n", json->str);

    write_header(fd, json->str, json->len);
}

extern void write_header(int fd, const char *header_str, int str_len)
{
    char *compressed_header = NULL;

    if (header_str == NULL)
        return;

    int worst_size;
    int compressed_size = 0;

    if (str_len == 0)
        return;

    worst_size = LZ4_compressBound(str_len);

    compressed_header = (char *)malloc(worst_size);

    if (compressed_header != NULL)
    {
        // compress JSON as much as possible
        // no, to speed up the process switched to the default compression level (.eq. 0)
        compressed_size = LZ4_compress_HC((const char *)header_str, compressed_header, str_len, worst_size, 0 /*LZ4HC_CLEVEL_MAX*/);

        printf("[C] HEADER length: %d; compressed: %d bytes\n", str_len, compressed_size);

        // send off the compressed data
        if (compressed_size > 0)
        {
            uint32_t header_size = str_len;
            uint32_t transmitted_size = compressed_size;

            chunked_write(fd, (const char *)&header_size, sizeof(header_size));           // header size after decompressing
            chunked_write(fd, (const char *)&transmitted_size, sizeof(transmitted_size)); // compressed buffer size
            chunked_write(fd, compressed_header, compressed_size);
        }
    }

    if (compressed_header != NULL)
        free(compressed_header);
}

extern void write_viewport(int fd, int width, int height, const float *pixels, const bool *mask, int precision)
{
    uchar *compressed_pixels = NULL;
    char *compressed_mask = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = width;
    uint ny = height;

    int mask_size, worst_size;
    int compressed_size = 0;

    if (pixels == NULL || mask == NULL)
        return;

    if (width <= 0 || height <= 0)
        return;

    // compress pixels with ZFP
    field = zfp_field_2d((void *)pixels, data_type, nx, ny);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    // zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed_pixels = (uchar *)malloc(bufsize);

    if (compressed_pixels != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed_pixels, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_MODE);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] viewport pixels compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
        }

        // clean up
        zfp_field_free(field);
        zfp_stream_close(zfp);
    }
    else
        printf("[C] a NULL compressed_pixels buffer!\n");

    // compress mask with LZ4-HC
    mask_size = width * height;

    worst_size = LZ4_compressBound(mask_size);

    compressed_mask = (char *)malloc(worst_size);

    if (compressed_mask != NULL)
    {
        // compress the mask as much as possible
        compressed_size = LZ4_compress_HC((const char *)mask, compressed_mask, mask_size, worst_size, LZ4HC_CLEVEL_MAX);

        printf("[C] viewport mask raw size: %d; compressed: %d bytes\n", mask_size, compressed_size);
    }

    // pipe the compressed viewport
    uint32_t view_width = width;
    uint32_t view_height = height;
    uint32_t pixels_len = zfpsize;
    uint32_t mask_len = compressed_size;

    chunked_write(fd, (const char *)&view_width, sizeof(view_width));
    chunked_write(fd, (const char *)&view_height, sizeof(view_height));

    // pixels (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&pixels_len, sizeof(pixels_len));
    if (compressed_pixels != NULL)
        chunked_write(fd, (char *)compressed_pixels, pixels_len);

    // mask (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&mask_len, sizeof(mask_len));
    if (compressed_mask != NULL)
        chunked_write(fd, compressed_mask, mask_len);

    // release the memory
    if (compressed_pixels != NULL)
        free(compressed_pixels);

    if (compressed_mask != NULL)
        free(compressed_mask);
}

extern void write_image_spectrum(int fd, const char *flux, float pmin, float pmax, float pmedian, float black, float white, float sensitivity, float ratio_sensitivity, int width, int height, int precision, const float *pixels, const bool *mask)
{
    uchar *compressed_pixels = NULL;
    char *compressed_mask = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = width;
    uint ny = height;

    int mask_size, worst_size;
    int compressed_size = 0;

    if (flux == NULL || pixels == NULL || mask == NULL)
        return;

    if (width <= 0 || height <= 0)
        return;

    printf("[C] fd: %d, flux: %s, pmin: %f, pmax: %f, pmedian: %f, black: %f, white: %f, sensitivity: %f, ratio_sensitivity: %f, width: %d, height: %d\n", fd, flux, pmin, pmax, pmedian, black, white, sensitivity, ratio_sensitivity, width, height);

    /*for (i = 0; i < width; i++)
        printf("|%f,%d", pixels[i], mask[i]);
    printf("\n\n\n");

    for (i = width * height - width; i < width * height; i++)
        printf("|%f,%d", pixels[i], mask[i]);
    printf("\n");*/

    // compress pixels with ZFP
    field = zfp_field_2d((void *)pixels, data_type, nx, ny);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    // zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed_pixels = (uchar *)malloc(bufsize);

    if (compressed_pixels != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed_pixels, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_MODE);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] image pixels compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
        }

        // clean up
        zfp_field_free(field);
        zfp_stream_close(zfp);
    }
    else
        printf("[C] a NULL compressed_pixels buffer!\n");

    // compress mask with LZ4-HC
    mask_size = width * height;

    worst_size = LZ4_compressBound(mask_size);

    compressed_mask = (char *)malloc(worst_size);

    if (compressed_mask != NULL)
    {
        // compress the mask as much as possible
        compressed_size = LZ4_compress_HC((const char *)mask, compressed_mask, mask_size, worst_size, LZ4HC_CLEVEL_MAX);

        printf("[C] image mask raw size: %d; compressed: %d bytes\n", mask_size, compressed_size);
    }

    // transmit the data
    float tmp;
    uint32_t flux_len = strlen(flux);

    uint32_t img_width = width;
    uint32_t img_height = height;
    uint32_t pixels_len = zfpsize;
    uint32_t mask_len = compressed_size;

    // the flux length
    chunked_write(fd, (const char *)&flux_len, sizeof(flux_len));

    // flux
    chunked_write(fd, flux, flux_len);

    // pmin
    tmp = pmin;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // pmax
    tmp = pmax;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // pmedian
    tmp = pmedian;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // sensitivity
    tmp = sensitivity;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // ratio_sensitivity
    tmp = ratio_sensitivity;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // white
    tmp = white;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // black
    tmp = black;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // the image + mask
    chunked_write(fd, (const char *)&img_width, sizeof(img_width));
    chunked_write(fd, (const char *)&img_height, sizeof(img_height));

    // pixels (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&pixels_len, sizeof(pixels_len));
    if (compressed_pixels != NULL)
        chunked_write(fd, (char *)compressed_pixels, pixels_len);

    // mask (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&mask_len, sizeof(mask_len));
    if (compressed_mask != NULL)
        chunked_write(fd, compressed_mask, mask_len);

    // release the memory
    if (compressed_pixels != NULL)
        free(compressed_pixels);

    if (compressed_mask != NULL)
        free(compressed_mask);
}

size_t chunked_write(int fd, const char *src, size_t n)
{
    size_t nchar, remaining, offset;
    ssize_t written;

    remaining = n;
    offset = 0;

    while (remaining > 0)
    {
        nchar = MIN(remaining, CHUNK);
        written = write(fd, src + offset, nchar);

        if (written > 0)
        {
            remaining -= written;
            offset += written;
        }

        // the connection might have been closed, bail out
        if (written < 0)
        {
            printf("[C] write returned %ld, aborting.\n", written);
            return offset;
        }

        printf("[C] chars written: %zu out of %zu bytes.\n", offset, n);
    }

    return offset;
}

void *handle_fitswebql_request(void *uri)
{
    if (uri == NULL)
        pthread_exit(NULL);

    fitswebql_request((char *)uri, strlen((char *)uri));

    free(uri);

    pthread_exit(NULL);
}

void *handle_image_spectrum_request(void *args)
{
    if (args == NULL)
        pthread_exit(NULL);

    struct arg_struct *params = (struct arg_struct *)args;

    if (params->item == NULL)
    {
        free(params);
        pthread_exit(NULL);
    }

    image_spectrum_request(params->item, params->width, params->height, params->precision, params->fetch_data, params->fd);

    // close the write end of the pipe
    close(params->fd);

    free(params);

    pthread_exit(NULL);
}

void *stream_molecules(void *args)
{
    if (args == NULL)
        pthread_exit(NULL);

    struct splat_req *req = (struct splat_req *)args;

    char strSQL[256];
    char chunk_data[256];
    int rc;
    char *zErrMsg = 0;

    snprintf(strSQL, sizeof(strSQL), "SELECT * FROM lines WHERE frequency>=%f AND frequency<=%f;", req->freq_start, req->freq_end);
    printf("%s\n", strSQL);

    req->first = true;

    if (req->compression)
    {
        req->z.zalloc = Z_NULL;
        req->z.zfree = Z_NULL;
        req->z.opaque = Z_NULL;
        req->z.next_in = Z_NULL;
        req->z.avail_in = 0;

        CALL_ZLIB(deflateInit2(&(req->z), Z_BEST_COMPRESSION, Z_DEFLATED, _windowBits | GZIP_ENCODING, 9, Z_DEFAULT_STRATEGY));
    }

    rc = sqlite3_exec(splat_db, strSQL, sqlite_callback, req, &zErrMsg);

    if (rc != SQLITE_OK)
    {
        fprintf(stderr, "SQL error: %s\n", zErrMsg);
        sqlite3_free(zErrMsg);
    }

    if (req->first)
        snprintf(chunk_data, sizeof(chunk_data), "{\"molecules\" : []}");
    else
        snprintf(chunk_data, sizeof(chunk_data), "]}");

    if (req->compression)
    {
        req->z.avail_in = strlen(chunk_data);
        req->z.next_in = (unsigned char *)chunk_data;

        do
        {
            req->z.avail_out = CHUNK;   // size of output
            req->z.next_out = req->out; // output char array
            CALL_ZLIB(deflate(&(req->z), Z_FINISH));
            size_t have = CHUNK - req->z.avail_out;

            if (have > 0)
            {
                // printf("Z_FINISH avail_out: %zu\n", have);
                chunked_write(req->fd, (const char *)req->out, have);
            }
        } while (req->z.avail_out == 0);

        CALL_ZLIB(deflateEnd(&(req->z)));
    }
    else
        chunked_write(req->fd, (const char *)chunk_data, strlen(chunk_data));

    // close the write end of the pipe
    close(req->fd);

    free(req);

    pthread_exit(NULL);
}

static int sqlite_callback(void *userp, int argc, char **argv, char **azColName)
{
    struct splat_req *req = (struct splat_req *)userp;

    if (argc == 8)
    {
        /*printf("sqlite_callback::molecule:\t");
        for (int i = 0; i < argc; i++)
            printf("%s:%s\t", azColName[i], argv[i]);
        printf("\n");*/

        GString *json = g_string_sized_new(1024);

        if (req->first)
        {
            req->first = false;

            g_string_printf(json, "{\"molecules\" : [");
        }
        else
            g_string_printf(json, ",");

        // json-encode a spectral line
        char *encoded;

        // species
        encoded = json_encode_string(denull(argv[0]));
        g_string_append_printf(json, "{\"species\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // name
        encoded = json_encode_string(denull(argv[1]));
        g_string_append_printf(json, "\"name\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // frequency
        g_string_append_printf(json, "\"frequency\" : %s,", denull(argv[2]));

        // quantum numbers
        encoded = json_encode_string(denull(argv[3]));
        g_string_append_printf(json, "\"quantum\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // cdms_intensity
        encoded = json_encode_string(denull(argv[4]));
        g_string_append_printf(json, "\"cdms\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // lovas_intensity
        encoded = json_encode_string(denull(argv[5]));
        g_string_append_printf(json, "\"lovas\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // E_L
        encoded = json_encode_string(denull(argv[6]));
        g_string_append_printf(json, "\"E_L\" : %s,", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        // linelist
        encoded = json_encode_string(denull(argv[7]));
        g_string_append_printf(json, "\"list\" : %s}", denull(encoded));
        if (encoded != NULL)
            free(encoded);

        if (req->compression)
        {
            req->z.avail_in = json->len;                 // size of input
            req->z.next_in = (unsigned char *)json->str; // input char array

            do
            {
                req->z.avail_out = CHUNK;   // size of output
                req->z.next_out = req->out; // output char array
                CALL_ZLIB(deflate(&(req->z), Z_NO_FLUSH));
                size_t have = CHUNK - req->z.avail_out;

                if (have > 0)
                {
                    // printf("ZLIB avail_out: %zu\n", have);
                    chunked_write(req->fd, (const char *)req->out, have);
                }
            } while (req->z.avail_out == 0);
        }
        else
            chunked_write(req->fd, (const char *)json->str, json->len);

        g_string_free(json, TRUE);
    }

    return 0;
}