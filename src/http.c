#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>

#define __USE_XOPEN
#include <time.h>

#include <inttypes.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <pwd.h>
#include <dirent.h>

#include <glib.h>
#include <microhttpd.h>
#include <curl/curl.h>

// LZ4 character streams compressor
#include <lz4hc.h>

// ZFP floating-point compressor
#include <zfp.h>

#include "json.h"
#include "http.h"
#include "mjson.h"

#include "cluster.h"

extern GSList *cluster;
extern GMutex cluster_mtx;

#include "hash_table.h"

#include "version.h"

#include <sqlite3.h>
static sqlite3 *splat_db = NULL;
extern options_t options; // <options> is defined in main.c

#ifdef MONGOOSE_HTTP_CLIENT
#include "mongoose.h"

typedef struct
{
    char *url;
    int port;
    int progress;
    int *counter;
    bool done;
} progress_t;

static void progress_fn(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    progress_t *req = (progress_t *)fn_data;

    if (ev == MG_EV_CONNECT)
    {
        // Connected to server. Extract host name from URL
        struct mg_str host = mg_url_host(req->url);

        // Send request
        mg_printf(c,
                  "POST %s HTTP/1.0\r\n"
                  "Host: %.*s:%d\r\n"
                  "Content-Type: octet-stream\r\n"
                  "Content-Length: %d\r\n"
                  "\r\n",
                  mg_url_uri(req->url),
                  (int)host.len, host.ptr, req->port, (int)sizeof(int));

        // append the "POST" data buffer
        mg_send(c, &(req->progress), sizeof(int));
    }
    else if (ev == MG_EV_HTTP_MSG)
    {
        // Response is received. Print it
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        // printf("%.*s", (int)hm->message.len, hm->message.ptr);

        // Examine the HTTP response code. Upon success ('OK') set the return value <counter> to <progress>
        if (mg_http_status(hm) == 200)
            // if (atoi(hm->uri.ptr) == 200)
            *(req->counter) = req->progress;

        c->is_closing = 1; // Tell mongoose to close this connection
        req->done = true;  // Tell event loop to stop
    }
    else if (ev == MG_EV_ERROR)
    {
        req->done = true; // Error, tell event loop to stop
    }
}

#endif

inline const char *denull(const char *str)
{
    if (str != NULL)
        return str;
    else
        return "\"\"";
};

// HTML
#define PAGE "<html><head><title>FITSWEBQL SE</title>" \
             "</head><body>FITSWEBQLSE (libmicrohttpd)</body></html>"

struct MHD_Daemon *http_server = NULL;

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite, char *root);
void *forward_fitswebql_request(void *ptr);
void *handle_fitswebql_request(void *ptr);
void *handle_image_spectrum_request(void *args);
void *handle_image_request(void *args);
extern void *viewport_request(void *req); // a FORTRAN subroutine
void fetch_channel_range(char *root, char *datasetid, int len, int *start, int *end, int *status, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
void *fetch_inner_dimensions(void *ptr);
void *fetch_image(void *ptr);
void *fetch_realtime_image_spectrum(void *ptr);
int submit_progress(char *root, char *datasetid, int len, int progress);

extern void load_fits_file(char *datasetid, size_t datasetid_len, char *filepath, size_t filepath_len, char *flux, size_t flux_len, char *root, char *dir, int len);
extern void image_spectrum_request(void *item, int width, int height, int precision, int fetch_data, int fd);
extern void image_request(void *item, int width, int height, int fd);
extern int get_error_status(void *item);
extern int get_header_status(void *item);
extern int get_ok_status(void *item);
extern int get_image_status(void *item);
extern float get_progress(void *item);
extern float get_elapsed(void *item);
extern void get_frequency_range(void *item, double *freq_start_ptr, double *freq_end_ptr);

size_t chunked_write(int fd, const char *src, size_t n);
void write_json(int fd, GString *json);
void write_header(int fd, const char *header_str, int str_len);
void write_elapsed(int fd, const float *elapsed);
void write_spectrum(int fd, const float *spectrum, int n, int precision);
void write_viewport(int fd, int width, int height, const float *pixels, const bool *mask, int precision);
void write_image_spectrum(int fd, const char *flux, float pmin, float pmax, float pmedian, float black, float white, float sensitivity, float ratio_sensitivity, int width, int height, int precision, const float *pixels, const bool *mask);

void *stream_molecules(void *args);
static int sqlite_callback(void *userp, int argc, char **argv, char **azColName);

size_t html_encode(char *source, size_t len, char *dest, size_t max);

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
        "404 Not Found";

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

static enum MHD_Result http_bad_request(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "400 Bad Request";

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
        "202 Accepted";

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

static enum MHD_Result http_acknowledge(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *okstr =
        "200 OK Request Acknowledged\n";

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

static enum MHD_Result http_internal_server_error(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "500 Internal Server Error";

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
        "501 Not Implemented";

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

                    free(encoded);
                }

                if (S_ISREG(sbuf.st_mode))
                    if (!strcasecmp(get_filename_ext(namelist[i]->d_name), "fits"))
                    {
                        char *encoded = json_encode_string(namelist[i]->d_name);

                        g_string_append_printf(json, "{\"type\" : \"file\", \"name\" : %s, \"size\" : %zu, \"last_modified\" : \"%s\"},", encoded, filesize, last_modified);
                        has_contents = 1;

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

    free(namelist);
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

static enum MHD_Result get_home_directory(struct MHD_Connection *connection)
{
    return get_directory(connection, strdup(options.home_dir));
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

static enum MHD_Result on_http_connection(void *cls,
                                          struct MHD_Connection *connection,
                                          const char *url,
                                          const char *method,
                                          const char *version,
                                          const char *upload_data,
                                          size_t *upload_data_size,
                                          void **ptr)
{
    (void)cls;         // silence gcc warnings
    (void)version;     // silence gcc warnings
    (void)upload_data; // silence gcc warnings

    static int dummy;
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

    // const char *user_agent = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, MHD_HTTP_HEADER_USER_AGENT);
    // const char *forwarded_for = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "X-Forwarded-For");
    const char *encoding = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "Accept-Encoding");
    // MHD_get_connection_values(connection, MHD_HEADER_KIND, (MHD_KeyValueIterator)&print_out_key, NULL);

    if (0 == strcmp(url, "/exit"))
    {
        // distributed exit
        distributed_exit();

        // raise SIGINT
        int ret = raise(SIGINT);

        if (ret != 0)
        {
            printf("[C] Error: unable to raise SIGINT signal.\n");
            return http_not_found(connection);
        }
        else
            return http_ok(connection);
    }

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

    if (strstr(url, "/cluster/") != NULL)
    {
        char *timestamp = strrchr(url, '/');

        if (timestamp != NULL)
        {
            timestamp++;

            // forward the cluster ping across the cluster (only from the receiving root node)
            int i;
            GSList *iterator = NULL;

            g_mutex_lock(&cluster_mtx);

            int handle_count = g_slist_length(cluster);

            char *nodes[handle_count];
            bool status[handle_count];

            CURL *handles[handle_count];
            CURLM *multi_handle;

            int still_running = 1; /* keep number of running handles */

            CURLMsg *msg;  /* for picking up messages with the transfer status */
            int msgs_left; /* how many messages are left */

            /* Allocate one CURL handle per transfer */
            for (i = 0; i < handle_count; i++)
            {
                handles[i] = curl_easy_init();
                nodes[i] = NULL;
                status[i] = false;
            }

            /* init a multi stack */
            multi_handle = curl_multi_init();

            for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
            {
                GString *url = g_string_new("http://");
                g_string_append_printf(url, "%s:", (char *)iterator->data);
                g_string_append_printf(url, "%" PRIu16 "/heartbeat/%s", options.http_port, timestamp);
                // printf("[C] URL: '%s'\n", url->str);

                nodes[i] = strdup((char *)iterator->data);

                // set the individual URL
                curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

                /* get us the resource without a body - use HEAD! */
                curl_easy_setopt(handles[i], CURLOPT_NOBODY, 1L);

                // add the individual transfer
                curl_multi_add_handle(multi_handle, handles[i]);

                g_string_free(url, TRUE);

                // move on to the next cluster node
                i++;
            }

            g_mutex_unlock(&cluster_mtx);

            /* Wait for the transfers */
            while (still_running)
            {
                CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

                if (still_running)
                    /* wait for activity, timeout or "nothing" */
                    mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

                if (mc)
                    break;
            }

            /* See how the transfers went */
            while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
            {
                if (msg->msg == CURLMSG_DONE)
                {
                    int idx;
                    /* Find out which handle this message is about */
                    for (idx = 0; idx < handle_count; idx++)
                    {
                        int found = (msg->easy_handle == handles[idx]);
                        if (found)
                            break;
                    }

                    long response_code = 0;
                    curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

                    // printf("[C] HTTP transfer completed; cURL status %d, HTTP code %ld.\n", msg->data.result, response_code);

                    if (msg->data.result == CURLE_OK)
                        status[idx] = true;
                }
            }

            // prepare a JSON response before releasing the memory
            GString *json = g_string_sized_new(1024);

            g_string_printf(json, "{\"timestamp\" : %s, \"nodes\" : [", timestamp);

            /* remove the transfers and cleanup the handles */
            for (i = 0; i < handle_count; i++)
            {
                curl_multi_remove_handle(multi_handle, handles[i]);
                curl_easy_cleanup(handles[i]);

                // append and free a cluster node
                g_string_append_printf(json, "{\"node\" : \"%s\", \"status\" : %s},", nodes[i], status[i] ? "true" : "false");
                free(nodes[i]);
            }

            curl_multi_cleanup(multi_handle);

            // append the ROOT node and close JSON
            g_string_append_printf(json, "{\"node\" : \"%s\", \"status\" : true}]}", options.root);

            // printf("[C] %s\n", json->str);

            struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json->len, (void *)json->str, g_free);
            g_string_free(json, FALSE);

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
            return http_bad_request(connection);
    }

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
            return http_bad_request(connection);
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

    if (strstr(url, "/range/") != NULL)
    {
        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
        {
            datasetId++; // skip the slash character

            void *item = get_dataset(datasetId);

            if (item == NULL)
            {
                if (dataset_exists(datasetId)) // a <NULL> entry should have been created prior to loading the FITS file
                    return http_accepted(connection);
                else
                {
                    // signal a catastrophic error
                    GString *json = g_string_new("{\"startindex\":0,\"endindex\":0,\"status\":-2}");

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
            }

            int start, end, status;

            // get the channel range from FORTRAN
            get_channel_range_C(item, 0, &start, &end, &status);

            // make JSON
            GString *json = g_string_sized_new(1024);

            g_string_printf(json, "{\"startindex\":%d,\"endindex\":%d,\"status\":%d}", start, end, status);

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
                int stat = pthread_create(&tid, NULL, &stream_molecules, args);

                if (stat == 0)
                    pthread_detach(tid);
                else
                    close(pipefd[1]);
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
            return http_bad_request(connection);

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

        // printf("[C] datasetId(%s), width(%d), height(%d), quality(%s), fetch_data: %s\n", datasetId, width, height, qualityStr, (fetch_data ? "true" : "false"));

        if (width <= 0 || height <= 0)
            return http_not_implemented(connection);

        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_accepted(connection);

        if (get_error_status(item))
            return http_internal_server_error(connection);

        if (!get_ok_status(item))
            return http_accepted(connection);

        // if !is_root_rank && item%depth > 1 && cluster_size > 0 {
        // forward the URL across the cluster, collect the (downsized) {pixels, mask} & spectrum
        // and then pass the collated {pixels, mask} & spectrum to the FORTRAN side below
        //}

        if (!get_image_status(item))
            return http_accepted(connection);

        // open a pipe
        status = pipe(pipefd);

        if (0 != status)
            return http_internal_server_error(connection);

        // create a response from a pipe by passing the read end of the pipe
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
            int stat = pthread_create(&tid, NULL, &handle_image_spectrum_request, args);

            if (stat == 0)
                pthread_detach(tid);
            else
                close(pipefd[1]);
        }
        else
            close(pipefd[1]);

        return ret;
    }

    if (strstr(url, "/viewport/") != NULL)
    {
        int x1, y1, x2, y2;
        double frame_start, frame_end, ref_freq;
        bool image;
        enum zoom_shape beam;
        enum intensity_mode intensity;

        int status;
        int pipefd[2];
        pthread_t tid;

        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
            datasetId++; // skip the slash character

        if (datasetId == NULL)
            return http_bad_request(connection);

        char *x1str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "x1");

        if (x1str == NULL)
            return http_bad_request(connection);
        else
            x1 = atoi(x1str);

        char *y1str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "y1");

        if (y1str == NULL)
            return http_bad_request(connection);
        else
            y1 = atoi(y1str);

        char *x2str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "x2");

        if (x2str == NULL)
            return http_bad_request(connection);
        else
            x2 = atoi(x2str);

        char *y2str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "y2");

        if (y2str == NULL)
            return http_bad_request(connection);
        else
            y2 = atoi(y2str);

        char *frame_start_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "frame_start");

        if (frame_start_str == NULL)
            return http_bad_request(connection);
        else
            frame_start = atof(frame_start_str);

        char *frame_end_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "frame_end");

        if (frame_end_str == NULL)
            return http_bad_request(connection);
        else
            frame_end = atof(frame_end_str);

        char *ref_freq_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ref_freq");

        if (ref_freq_str == NULL)
            return http_bad_request(connection);
        else
            ref_freq = atof(ref_freq_str);

        char *image_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "image");

        if (image_str == NULL)
            return http_bad_request(connection);
        else
            image = strcmp(image_str, "true") == 0 ? true : false;

        char *beam_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "beam");

        if (beam_str == NULL)
            return http_bad_request(connection);
        else
            beam = strcmp(beam_str, "circle") == 0 ? circle : square;

        char *intensity_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "intensity");

        if (intensity_str == NULL)
            return http_bad_request(connection);
        else
            intensity = strcmp(intensity_str, "integrated") == 0 ? integrated : mean;

        // do we have a dataset?
        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_not_found(connection);

        if (get_error_status(item))
            return http_internal_server_error(connection);

        if (!get_image_status(item))
            return http_internal_server_error(connection);

        // open a pipe
        status = pipe(pipefd);

        if (0 != status)
            return http_internal_server_error(connection);

        // create a response from a pipe by passing the read end of the pipe
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

        // pass the write end of the pipe to Fortran
        // the binary response data will be generated in Fortran
        // printf("[C] calling viewport_request with the pipe file descriptor %d\n", pipefd[1]);

        // got all the data, prepare a request structure and pass it to FORTRAN
        struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

        if (req != NULL)
        {
            req->dx = 0;
            req->image = image;
            req->quality = medium;
            req->x1 = x1;
            req->x2 = x2;
            req->y1 = y1;
            req->y2 = y2;
            req->width = 0;
            req->height = 0;
            req->beam = beam;
            req->intensity = intensity;
            req->frame_start = frame_start;
            req->frame_end = frame_end;
            req->ref_freq = ref_freq;
            req->seq_id = 0;
            req->timestamp = 0.0;

            req->fd = pipefd[1];
            req->ptr = item;

            // create and detach the FORTRAN thread
            int stat = pthread_create(&tid, NULL, &viewport_request, req);

            if (stat == 0)
                pthread_detach(tid);
            else
            {
                close(pipefd[1]);
                free(req);
            }
        }
        else
            close(pipefd[1]);

        return ret;
    }

    if (strstr(url, "/image/") != NULL)
    {
        int width, height;

        int status;
        int pipefd[2];
        pthread_t tid;

        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
            datasetId++; // skip the slash character

        char *widthStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "width");
        char *heightStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "height");

        if (datasetId == NULL || widthStr == NULL || heightStr == NULL)
            return http_bad_request(connection);

        width = atoi(widthStr);
        height = atoi(heightStr);

        printf("[C] gathering pixels for datasetId(%s), width(%d), height(%d)\n", datasetId, width, height);

        if (width <= 0 || height <= 0)
            return http_not_implemented(connection);

        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_not_found(connection);

        if (!get_image_status(item))
            return http_internal_server_error(connection);

        // open a pipe
        status = pipe(pipefd);

        if (0 != status)
            return http_internal_server_error(connection);

        // create a response from a pipe by passing the read end of the pipe
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

        // pass the write end of the pipe to Fortran
        // the binary response data will be generated in Fortran
        printf("[C] calling image_request with the pipe file descriptor %d\n", pipefd[1]);

        struct arg_struct *args = malloc(sizeof(struct arg_struct));

        if (args != NULL)
        {
            args->item = get_dataset(datasetId);
            args->width = width;
            args->height = height;
            args->precision = ZFP_HIGH_PRECISION; // in the future might decide to compress the data
            args->fetch_data = false;
            args->fd = pipefd[1];

            // create and detach the thread
            int stat = pthread_create(&tid, NULL, &handle_image_request, args);

            if (stat == 0)
                pthread_detach(tid);
            else
            {
                close(pipefd[1]);
                free(args);
            }
        }
        else
            close(pipefd[1]);

        return ret;
    }

    // WebQL main entry page
    if (strstr(url, "FITSWebQL.html") != NULL)
    {
        char *root = NULL;

        if (!options.local)
        {
            // get the root path
            char *proot = (char *)strstr(url, "FITSWebQL.html");

            int len = proot - url;
            char *root = strndup(url, len);

            printf("[C] URL root path: %s\n", root);
        }

        // to be forwarded to cluster nodes
        GString *uri = g_string_new(url);

        g_string_append(uri, "?");

        // get datasetId
        char **datasetId = NULL;
        int va_count = 0;

        char *directory = NULL; // only needed by <options.local>
        char *extension = NULL; // only needed by <options.local>

        if (options.local)
        {
            directory = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");
            extension = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ext");
            char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

            if (directory != NULL)
            {
                char enc[256];
                size_t len;

                GString *value = g_string_new(directory);

                len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                enc[len] = '\0';

                g_string_append_printf(uri, "dir=%s&", enc);

                g_string_free(value, TRUE);
            }

            if (extension != NULL)
            {
                char enc[256];
                size_t len;

                GString *value = g_string_new(extension);

                len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                enc[len] = '\0';

                g_string_append_printf(uri, "ext=%s&", enc);

                g_string_free(value, TRUE);
            }

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

                    char enc[256];
                    size_t len;

                    GString *value = g_string_new(tmp);

                    len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                    enc[len] = '\0';

                    g_string_append_printf(uri, "filename%d=%s&", va_count, enc);

                    g_string_free(value, TRUE);
                }

                printf("[C] number of arguments: %d\n", va_count);
            }
            else
            {
                va_count = 1;

                // allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = tmp;

                char enc[256];
                size_t len;

                GString *value = g_string_new(tmp);

                len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                enc[len] = '\0';

                g_string_append_printf(uri, "filename=%s&", enc);

                g_string_free(value, TRUE);
            }
        }
        else
        {
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

                    char enc[256];
                    size_t len;

                    GString *value = g_string_new(tmp);

                    len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                    enc[len] = '\0';

                    g_string_append_printf(uri, "datasetId%d=%s&", va_count, enc);

                    g_string_free(value, TRUE);
                }

                printf("[C] number of arguments: %d\n", va_count);
            }
            else
            {
                va_count = 1;

                // allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = tmp;

                char enc[256];
                size_t len;

                GString *value = g_string_new(tmp);

                len = html_encode(value->str, value->len, enc, sizeof(enc) - 1);
                enc[len] = '\0';

                g_string_append_printf(uri, "datasetId=%s&", enc);

                g_string_free(value, TRUE);
            }
        }

        char *view = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "view");
        char *flux = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "flux");
        char *db = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "db");
        char *table = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "table");

        if (view != NULL)
            g_string_append_printf(uri, "view=%s&", view);

        if (flux != NULL)
            g_string_append_printf(uri, "flux=%s&", flux);

        if (db != NULL)
            g_string_append_printf(uri, "db=%s&", db);

        if (table != NULL)
            g_string_append_printf(uri, "table=%s&", table);

        // remove the last '&' from uri
        if (uri->str[uri->len - 1] == '&')
            g_string_truncate(uri, uri->len - 1);

        int composite = 0;

        if (view != NULL)
            composite = (strcasecmp("composite", view) == 0) ? 1 : 0;

        bool is_root_rank = true;

        char *root_ip = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "root");

        if (root_ip != NULL)
            is_root_rank = false;

        // broadcast the FITS request across the cluster
        if (is_root_rank)
        {
            pthread_t tid;

            // launch a pthread, release memory inside the thread
            char *req = strdup(uri->str);
            int stat = pthread_create(&tid, NULL, &forward_fitswebql_request, req);

            if (stat != 0)
            {
                // release memory
                free(req);
            }
            else
                pthread_detach(tid);
        };

        if (datasetId != NULL)
        {
            if (is_root_rank)
                ret = execute_alma(connection, datasetId, va_count, composite, root);
            else
                ret = http_acknowledge(connection);

            // pass the filepath to FORTRAN
            if (options.local)
            {
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

                    // C -> FORTRAN
                    fits_req_t *req = (fits_req_t *)malloc(sizeof(fits_req_t));

                    if (req != NULL)
                    {
                        req->datasetid = strdup(datasetId[i]);
                        req->filepath = strndup(filepath, sizeof(filepath));

                        if (flux != NULL)
                            req->flux = strdup(flux);
                        else
                            req->flux = strdup("NULL");

                        if (db != NULL)
                            if (strstr(db, "hsc") != NULL)
                            {
                                free(req->flux);
                                req->flux = strdup("ratio");
                            }

                        if (table != NULL)
                            if (strstr(table, "fugin") != NULL)
                            {
                                free(req->flux);
                                req->flux = strdup("logistic");
                            }

                        if (root_ip != NULL)
                            req->root = strdup(root_ip);
                        else
                            req->root = NULL;

                        int stat = pthread_create(&tid, NULL, &handle_fitswebql_request, req);

                        if (stat != 0)
                        {
                            // release memory
                            free(req->datasetid);
                            free(req->filepath);
                            free(req->flux);
                            free(req->root);
                            free(req);
                        }
                        else
                            pthread_detach(tid);
                    }
                }

                // directory/extension should not be freed (libmicrohttpd does that)
            }
            else
            {
                ret = http_ok(connection);
                // get the full path from the postgresql db

                // if a file does not exist form a download URL (jvox...)

                // then call FORTRAN with a filepath or URL
            }
        }
        else
            ret = http_not_found(connection);

        // deallocate datasetId
        free(datasetId);

        // deallocate the root path
        free(root);

        g_string_free(uri, TRUE);

        return ret;
    }

    // static resources
    if (url[strlen(url) - 1] != '/')
        return serve_file(connection, url);
    else
    {
        // root document

        if (options.local)
            return serve_file(connection, "/local.html");
        else
            return serve_file(connection, "/test.html");
    }

    return http_not_found(connection);
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

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite, char *root)
{
    int i;
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
    g_string_append(html, "<script src=\"https://d3js.org/d3.v6.min.js\"></script>\n");
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

    // Font Awesome
    g_string_append(html, "<script src=\"https://kit.fontawesome.com/8433b7dde2.js\" crossorigin=\"anonymous\"></script>\n");

    // HTML5 FileSaver
    g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/FileSaver.js\"></script>\n");

    // WebAssembly
    g_string_append(html, "<script "
                          "src=\"client." WASM_VERSION ".js\"></script>\n");
    g_string_append_printf(html, "<script>\n"
                                 "Module.ready\n"
                                 "\t.then(status => console.log(status))\n"
                                 "\t.catch(e => console.error(e));\n"
                                 "</script>\n");

    // bootstrap
    g_string_append(html,
                    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, "
                    "user-scalable=no, minimum-scale=1, maximum-scale=1\">\n");

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
    g_string_append(html, "<script src=\"fitswebqlse.js?" VERSION_STRING "\"></script>\n");
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

    if (!options.local)
    {
        if (root != NULL)
            g_string_append_printf(html, "data-root-path='%s/' ", root);
        else
            g_string_append(html, "data-root-path='/' ");
    }
    else
        g_string_append(html, "data-root-path='/' ");

    g_string_append(html, " data-server-version='" VERSION_STRING "' data-server-string='" SERVER_STRING);

    if (options.local)
        g_string_append(html, "' data-server-mode='LOCAL");
    else
        g_string_append(html, "' data-server-mode='SERVER");

    g_string_append_printf(html, "' data-has-fits='%d'></div>\n", (has_fits ? 1 : 0));

    if (options.production)
        g_string_append(html, "<script>var WS_SOCKET = 'wss://';</script>\n");
    else
        g_string_append(html, "<script>var WS_SOCKET = 'ws://';</script>\n");

    g_string_append_printf(html, "<script>var WS_PORT = %" PRIu16 ";</script>\n", options.ws_port);

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

void start_http()
{
    signal(SIGPIPE, SIG_IGN); // ignore SIGPIPE
    // signal(SIGINT, SIGINTHandler); //intercept CTRL+C to trigger a clean shutdown

    // http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
    http_server = MHD_start_daemon(MHD_USE_AUTO | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
                                   options.http_port,
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
            fprintf(stderr, "[C] Can't open local splatalogue database: %s\n", sqlite3_errmsg(splat_db));
            sqlite3_close(splat_db);
            splat_db = NULL;
        }

        printf("[C] µHTTP daemon listening on port %" PRIu16 "... Press CTRL-C to stop it.\n", options.http_port);
    }
};

void stop_http()
{
    if (http_server != NULL)
    {
        printf("[C] shutting down the µHTTP daemon... ");
        MHD_stop_daemon(http_server);
        http_server = NULL;
        printf("done\n");
    }

    if (splat_db != NULL)
    {
        sqlite3_close(splat_db);
        splat_db = NULL;
    }
};

size_t html_encode(char *source, size_t len, char *dest, size_t max)
{
    unsigned int pos = 0;

    if (source == NULL)
        return -1;

    for (unsigned int i = 0; i < len; i++)
    {
        char c = source[i];
        switch (c)
        {
        case '&':
            strcat(dest, "&amp;");
            pos += 5;
            break;
        case '\"':
            strcat(dest, "&quot;");
            pos += 6;
            break;
        case '\'':
            strcat(dest, "&apos;");
            pos += 6;
            break;
        case '<':
            strcat(dest, "&lt;");
            pos += 4;
            break;
        case '>':
            strcat(dest, "&gt;");
            pos += 4;
            break;
        default:
            dest[pos++] = c;
            break;
        }

        if (pos >= max)
            break;
    };

    return pos;
}

void *forward_fitswebql_request(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    char *uri = (char *)ptr;

    printf("[C] forwarding '%s' across the cluster.\n", uri);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting forward_fitswebql_request (no cluster nodes found)\n");

        g_mutex_unlock(&cluster_mtx);

        // release memory
        free(uri);

        pthread_exit(NULL);
    };

    CURL *handles[handle_count];
    CURLM *multi_handle;

    int still_running = 1; /* keep number of running handles */

    CURLMsg *msg;  /* for picking up messages with the transfer status */
    int msgs_left; /* how many messages are left */

    /* Allocate one CURL handle per transfer */
    for (i = 0; i < handle_count; i++)
        handles[i] = curl_easy_init();

    /* init a multi stack */
    multi_handle = curl_multi_init();

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "%s&root=%s", options.http_port, uri, options.root);
        // printf("[C] URL: '%s'\n", url->str);

        // set the individual URL
        curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

        // ignore the response body
        curl_easy_setopt(handles[i], CURLOPT_NOBODY, 1);

        // add the individual transfer
        curl_multi_add_handle(multi_handle, handles[i]);

        g_string_free(url, TRUE);

        // move on to the next cluster node
        i++;
    }

    g_mutex_unlock(&cluster_mtx);

    // release memory
    free(uri);

    /* Wait for the transfers */
    while (still_running)
    {
        CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

        if (still_running)
            /* wait for activity, timeout or "nothing" */
            mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

        if (mc)
            break;
    }

    /* See how the transfers went */
    while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
    {
        if (msg->msg == CURLMSG_DONE)
        {
            long response_code = 0;
            curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

            printf("[C] HTTP transfer completed; cURL status %d, HTTP code %ld.\n", msg->data.result, response_code);
        }
    }

    /* remove the transfers and cleanup the handles */
    for (i = 0; i < handle_count; i++)
    {
        curl_multi_remove_handle(multi_handle, handles[i]);
        curl_easy_cleanup(handles[i]);
    }

    curl_multi_cleanup(multi_handle);

    pthread_exit(NULL);
}

void *handle_fitswebql_request(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    fits_req_t *req = (fits_req_t *)ptr;

    printf("[C] datasetid: '%s', flux: '%s', filepath: '%s'; over to FORTRAN\n", req->datasetid, req->flux, req->filepath);

    // call FORTRAN
    load_fits_file(req->datasetid, strlen(req->datasetid), req->filepath, strlen(req->filepath), req->flux, strlen(req->flux), req->root, options.cache, strlen(options.cache));

    free(req->datasetid);
    free(req->filepath);
    free(req->flux);
    free(req->root);
    free(req);

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

    // call FORTRAN
    image_spectrum_request(params->item, params->width, params->height, params->precision, params->fetch_data, params->fd);

    // close the write end of the pipe
    close(params->fd);

    free(params);

    pthread_exit(NULL);
}

void *handle_image_request(void *args)
{
    if (args == NULL)
        pthread_exit(NULL);

    struct arg_struct *params = (struct arg_struct *)args;

    if (params->item == NULL)
    {
        free(params);
        pthread_exit(NULL);
    }

    // call FORTRAN
    image_request(params->item, params->width, params->height, params->fd);

    // close the write end of the pipe
    close(params->fd);

    free(params);

    pthread_exit(NULL);
}

static size_t
WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    size_t realsize = size * nmemb;
    struct MemoryStruct *mem = (struct MemoryStruct *)userp;

    char *ptr = realloc(mem->memory, mem->size + realsize + 1);
    if (!ptr)
    {
        /* out of memory! */
        printf("[C] not enough memory (realloc returned NULL)\n");
        return 0;
    }

    mem->memory = ptr;
    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;

    return realsize;
}

void fetch_channel_range(char *root, char *datasetid, int len, int *start, int *end, int *status, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum)
{
    int progress;

    if (*start > 0 && *end > 0)
        progress = *end - *start + 1;
    else
        progress = 0;

    char *id = strndup(datasetid, len);

    if (id == NULL)
    {
        *start = 0;
        *end = 0;
        *status = -1;
        return;
    }

    if (root == NULL)
    {
        void *item = get_dataset(id);

        if (item == NULL)
        {
            *start = 0;
            *end = 0;

            if (dataset_exists(id))
                *status = 1; // accepted
            else
                *status = -2; // a catastrophic error
        }
        else
        {
            // get the channel range from FORTRAN
            get_channel_range_C(item, progress, start, end, status);
        }

        free(id);
        return;
    }

    // a POST buffer
    char *post_buffer = NULL;
    size_t offset = 0;

    size_t post_size = sizeof(int); // space for at least <num_per_node (a.k.a. progress)>
    int idx = (*start - 1);         // FORTRAN arrays memory offset

    if (progress > 0)
    {
        // the starting offset <idx>
        post_size += sizeof(int);

        // include five floating-point arrays too ... for the range [start, end]
        post_size += 5 * progress * sizeof(float);
    }

    // assume a catastrophic error
    *start = 0;
    *end = 0;
    *status = -2;

    // allocate memory for the POST buffer
    post_buffer = (char *)malloc(post_size);

    if (post_buffer != NULL)
    {
        // fill-in the POST buffer
        memcpy(post_buffer, &progress, sizeof(progress));
        offset += sizeof(progress);

        // FORTRAN arrays
        if (progress > 0)
        {
            // idx
            memcpy(post_buffer + offset, &idx, sizeof(idx));
            offset += sizeof(idx);

            // frame_min
            memcpy(post_buffer + offset, &(frame_min[idx]), progress * sizeof(float));
            offset += progress * sizeof(float);

            // frame_max
            memcpy(post_buffer + offset, &(frame_max[idx]), progress * sizeof(float));
            offset += progress * sizeof(float);

            // frame_median
            memcpy(post_buffer + offset, &(frame_median[idx]), progress * sizeof(float));
            offset += progress * sizeof(float);

            // mean_spectrum
            memcpy(post_buffer + offset, &(mean_spectrum[idx]), progress * sizeof(float));
            offset += progress * sizeof(float);

            // integrated_spectrum
            memcpy(post_buffer + offset, &(integrated_spectrum[idx]), progress * sizeof(float));
            offset += progress * sizeof(float);
        };

        // form an HTTP request URL
        CURL *curl;
        CURLcode res;

        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", root);
        g_string_append_printf(url, "%" PRIu16 "/range/%s", options.ws_port, id);
        // printf("[C] URL: '%s'\n", url->str);

        curl = curl_easy_init();

        if (curl)
        {
            struct MemoryStruct chunk;

            chunk.memory = malloc(1); /* will be grown as needed by the realloc above */
            chunk.size = 0;           /* no data at this point */
            chunk.memory[0] = 0;

            curl_easy_setopt(curl, CURLOPT_URL, url->str);

            /* send all data to this function  */
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

            /* we pass our 'chunk' struct to the callback function */
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

            /* size of the POST data */
            curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, post_size);

            /* the actual POST data */
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_buffer);

            /* Perform the request, res will get the return code */
            res = curl_easy_perform(curl);

            /* Check for errors */
            if (res != CURLE_OK)
                fprintf(stderr, "[C] fetch_channel_range: curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
            else
            {
                // printf("%lu bytes retrieved\n", (unsigned long)chunk.size);
                // printf("cURL response: %s\n", chunk.memory);

                // assume accepted (no header yet)
                *status = 1;

                // parse the JSON response
                double val;

                if (mjson_get_number(chunk.memory, chunk.size, "$.startindex", &val))
                    *start = (int)val;

                if (mjson_get_number(chunk.memory, chunk.size, "$.endindex", &val))
                    *end = (int)val;

                if (mjson_get_number(chunk.memory, chunk.size, "$.status", &val))
                    *status = (int)val;
            }

            free(chunk.memory);

            /* always cleanup */
            curl_easy_cleanup(curl);
        };

        g_string_free(url, TRUE);
        free(post_buffer);
    }

    free(id);
}

int submit_progress(char *root, char *datasetid, int len, int progress)
{
    char *id = strndup(datasetid, len);

    if (id == NULL)
        return 0;

    if (root == NULL)
    {
        free(id);
        return 0;
    }

    int counter = 0; // by default return 0, i.e. no progress could be submitted to the cluster root

    if (progress > 0)
    {
        // form an HTTP request URL
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", root);
        g_string_append_printf(url, "%" PRIu16 "/progress/%s", options.ws_port, id);

#ifdef MONGOOSE_HTTP_CLIENT
        // use mongoose HTTP client as libcURL leaks memory in Intel Clear Linux ...
        // apparently it's OK, it is not a real memory leak, just DBus caching ...
        // https://github.com/clearlinux/distribution/issues/2574#issuecomment-1058618721
        progress_t req = {url->str, options.ws_port, progress, &counter, false};

        struct mg_mgr mgr; // Event manager

        mg_log_set("3");                                    // Set to 0 to disable debug
        mg_mgr_init(&mgr);                                  // Initialise event manager
        mg_http_connect(&mgr, url->str, progress_fn, &req); // Create client connection
        while (!req.done)
            mg_mgr_poll(&mgr, 1000); // Infinite event loop

        mg_mgr_free(&mgr); // Free resources
#else
        CURL *curl;
        CURLcode res;

        curl = curl_easy_init();

        if (curl)
        {
            struct MemoryStruct chunk;

            chunk.memory = malloc(1); /* will be grown as needed by the realloc above */
            chunk.size = 0;           /* no data at this point */
            chunk.memory[0] = 0;

            curl_easy_setopt(curl, CURLOPT_URL, url->str);

            /* send all data to this function  */
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

            /* we pass our 'chunk' struct to the callback function */
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

            /* size of the POST data */
            curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, sizeof(int));

            /* the actual POST data */
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, (void *)&progress);

            /* Perform the request, res will get the return code */
            res = curl_easy_perform(curl);

            /* Check for errors */
            if (res != CURLE_OK)
                fprintf(stderr, "[C] submit_progress: curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
            else
            {
                // Examine the HTTP response code. Upon success ('OK') set the return value <counter> to <progress>
                long http_code = 0;
                curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

                if (http_code == 200)
                    counter = progress;
            }

            free(chunk.memory);

            /* always cleanup */
            curl_easy_cleanup(curl);
        }
#endif

        g_string_free(url, TRUE);
    }

    free(id);

    return counter;
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

        // printf("[C] chars written: %zu out of %zu bytes.\n", offset, n);
    }

    return offset;
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
    (void)azColName; // silence gcc warnings

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
        g_string_append_printf(json, "\"qn\" : %s,", denull(encoded));
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
        g_string_append_printf(json, "\"linelist\" : %s}", denull(encoded));
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

void write_json(int fd, GString *json)
{
    if (json == NULL)
        return;

    printf("[C] JSON(%s)\n", json->str);

    write_header(fd, json->str, json->len);
}

void write_header(int fd, const char *header_str, int str_len)
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

void write_elapsed(int fd, const float *elapsed)
{
    chunked_write(fd, (const char *)elapsed, sizeof(float)); // elapsed compute time
}

void write_spectrum(int fd, const float *spectrum, int n, int precision)
{
    uchar *compressed;
    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = n;

    uint32_t length = n;

    if (spectrum == NULL || n <= 0)
    {
        // emit a zero-size spectrum
        length = 0;

        uint32_t compressed_size = 0;

        // transmit the data
        chunked_write(fd, (const char *)&length, sizeof(length));                   // spectrum length after decompressing
        chunked_write(fd, (const char *)&compressed_size, sizeof(compressed_size)); // compressed buffer size

        return;
    }

    // spectrum with ZFP
    field = zfp_field_1d((void *)spectrum, data_type, nx);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed = (uchar *)malloc(bufsize);

    if (compressed != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] spectrum compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
            printf("[C] float array size: %zu, compressed: %zu bytes\n", length * sizeof(float), zfpsize);

            // transmit the data
            uint32_t compressed_size = zfpsize;

            chunked_write(fd, (const char *)&length, sizeof(length));                   // spectrum length after decompressing
            chunked_write(fd, (const char *)&compressed_size, sizeof(compressed_size)); // compressed buffer size
            chunked_write(fd, (const char *)compressed, zfpsize);
        }

        free(compressed);
    }
    else
        printf("[C] a NULL compressed buffer!\n");

    // clean up
    zfp_field_free(field);
    zfp_stream_close(zfp);
}

void write_viewport(int fd, int width, int height, const float *pixels, const bool *mask, int precision)
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

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

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

void rpad(char *dst, const char *src, const char pad, const size_t sz)
{
    memset(dst, pad, sz);
    dst[sz] = 0x0;
    memcpy(dst, src, strlen(src));
}

void write_image_spectrum(int fd, const char *flux, float pmin, float pmax, float pmedian, float black, float white, float sensitivity, float ratio_sensitivity, int width, int height, int precision, const float *pixels, const bool *mask)
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

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] image pixels compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
        }
    }
    else
        printf("[C] a NULL compressed_pixels buffer!\n");

    // clean up
    zfp_field_free(field);
    zfp_stream_close(zfp);

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

    // pad the flux with spaces so that the length is a multiple of 4 (JavaScript needs it ...)
    int padded_len = 4 * (strlen(flux) / 4 + 1);

    // memory for a new padded flux
    char padded_flux[padded_len + 1];

    // right-pad the flux with spaces
    rpad(padded_flux, flux, ' ', padded_len);

    // transmit the data
    float tmp;
    uint32_t flux_len = strlen(padded_flux);

    uint32_t img_width = width;
    uint32_t img_height = height;
    uint32_t pixels_len = zfpsize;
    uint32_t mask_len = compressed_size;

    // the flux length
    chunked_write(fd, (const char *)&flux_len, sizeof(flux_len));

    // flux
    chunked_write(fd, padded_flux, flux_len);

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

void *fetch_inner_dimensions(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct inner_dims_req *req = (struct inner_dims_req *)ptr;

    printf("[C] calling fetch_inner_dimensions across the cluster for '%.*s' with {W:%d,H:%d}\n", req->len, req->datasetid, req->width, req->height);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_inner_dimensions (no cluster nodes found)\n");

        g_mutex_unlock(&cluster_mtx);
        pthread_exit(NULL);
    };

    CURL *handles[handle_count];
    struct MemoryStruct chunks[handle_count];
    CURLM *multi_handle;

    int still_running = 1; /* keep number of running handles */

    CURLMsg *msg;  /* for picking up messages with the transfer status */
    int msgs_left; /* how many messages are left */

    /* Allocate one CURL handle per transfer */
    for (i = 0; i < handle_count; i++)
    {
        handles[i] = curl_easy_init();

        chunks[i].memory = malloc(1);
        chunks[i].size = 0;
        chunks[i].memory[0] = 0;
    }

    /* init a multi stack */
    multi_handle = curl_multi_init();

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/inner/%.*s", options.ws_port, req->len, req->datasetid);
        // printf("[C] URL: '%s'\n", url->str);

        // set the individual URL
        curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

        /* send all data to this function  */
        curl_easy_setopt(handles[i], CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

        /* we pass our 'chunk' struct to the callback function */
        curl_easy_setopt(handles[i], CURLOPT_WRITEDATA, (void *)&(chunks[i]));

        // add the individual transfer
        curl_multi_add_handle(multi_handle, handles[i]);

        g_string_free(url, TRUE);

        // move on to the next cluster node
        i++;
    }

    g_mutex_unlock(&cluster_mtx);

    /* Wait for the transfers */
    while (still_running)
    {
        CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

        if (still_running)
            /* wait for activity, timeout or "nothing" */
            mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

        if (mc)
            break;
    }

    /* See how the transfers went */
    while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
    {
        if (msg->msg == CURLMSG_DONE)
        {
            int idx;
            /* Find out which handle this message is about */
            for (idx = 0; idx < handle_count; idx++)
            {
                int found = (msg->easy_handle == handles[idx]);
                if (found)
                    break;
            }

            long response_code = 0;
            curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

            printf("[C] HTTP transfer completed; cURL status %d, HTTP code %ld.\n", msg->data.result, response_code);

            // parse the JSON response
            if (response_code == 200)
            {
                double val;

                int width = 0;
                int height = 0;

                // width
                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.width", &val))
                    width = (int)val;

                // height
                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.height", &val))
                    height = (int)val;

                if (width > req->width)
                    req->width = width;

                if (height > req->height)
                    req->height = height;
            }
        }
    }

    /* remove the transfers and cleanup the handles */
    for (i = 0; i < handle_count; i++)
    {
        curl_multi_remove_handle(multi_handle, handles[i]);
        curl_easy_cleanup(handles[i]);
        free(chunks[i].memory);
    }

    curl_multi_cleanup(multi_handle);

    pthread_exit(NULL);
}

void *fetch_realtime_image_spectrum(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct http_image_spectrum_request *req = (struct http_image_spectrum_request *)ptr;
    printf("[C] calling fetch_realtime_image_spectrum across the cluster for '%.*s'\n", req->len, req->datasetid);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_realtime_image_spectrum (no cluster nodes found)\n");

        g_mutex_unlock(&cluster_mtx);
        pthread_exit(NULL);
    };

    CURL *handles[handle_count];
    struct MemoryStruct chunks[handle_count];
    CURLM *multi_handle;

    int still_running = 1; /* keep number of running handles */

    CURLMsg *msg;  /* for picking up messages with the transfer status */
    int msgs_left; /* how many messages are left */

    /* Allocate one CURL handle per transfer */
    for (i = 0; i < handle_count; i++)
    {
        handles[i] = curl_easy_init();

        chunks[i].memory = malloc(1);
        chunks[i].size = 0;
        chunks[i].memory[0] = 0;
    }

    /* init a multi stack */
    multi_handle = curl_multi_init();

    // html-encode the datasetid
    char datasetid[2 * req->len];
    size_t len = html_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/viewport/%.*s?x1=%d&y1=%d&x2=%d&y2=%d&frame_start=%f&frame_end=%f&ref_freq=%f", options.http_port, (int)len, datasetid, req->x1, req->y1, req->x2, req->y2, req->frame_start, req->frame_end, req->ref_freq);
        g_string_append_printf(url, "&image=%s", req->image ? "true" : "false");                            // image
        g_string_append_printf(url, "&beam=%s", req->beam == circle ? "circle" : "square");                 // beam
        g_string_append_printf(url, "&intensity=%s", req->intensity == integrated ? "integrated" : "mean"); // intensity
        printf("[C] URL: '%s'\n", url->str);

        // set the individual URL
        curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

        /* send all data to this function  */
        curl_easy_setopt(handles[i], CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

        /* we pass our 'chunk' struct to the callback function */
        curl_easy_setopt(handles[i], CURLOPT_WRITEDATA, (void *)&(chunks[i]));

        // add the individual transfer
        curl_multi_add_handle(multi_handle, handles[i]);

        g_string_free(url, TRUE);

        // move on to the next cluster node
        i++;
    }

    g_mutex_unlock(&cluster_mtx);

    /* Wait for the transfers */
    while (still_running)
    {
        CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

        if (still_running)
            /* wait for activity, timeout or "nothing" */
            mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

        if (mc)
            break;
    }

    /* See how the transfers went */
    while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
    {
        if (msg->msg == CURLMSG_DONE)
        {
            int idx;
            /* Find out which handle this message is about */
            for (idx = 0; idx < handle_count; idx++)
            {
                int found = (msg->easy_handle == handles[idx]);
                if (found)
                    break;
            }

            long response_code = 0;
            curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

            printf("[C] HTTP transfer #%d completed; cURL status %d, HTTP code %ld.\n", idx + 1, msg->data.result, response_code);

            // reduce (gather) the spectrum, pixels and mask
            if (response_code == 200 && chunks[idx].size > 0)
            {
                // printf("[C] fetch_realtime_image_spectrum received %zu bytes.\n", chunks[idx].size);

                size_t spectrum_size = req->length * sizeof(float);
                size_t pixels_size = req->dimx * req->dimy * sizeof(float);
                size_t mask_size = req->dimx * req->dimy * sizeof(uint8);

                size_t offset = 0;

                // do we have the spectrum?
                if (chunks[idx].size >= spectrum_size)
                {
                    // printf("[C] fetch_realtime_image_spectrum gathering the spectrum.\n");

                    const float *spectrum = (float *)&(chunks[idx].memory[offset]);

                    for (int i = 0; i < req->length; i++)
                        req->spectrum[i] += spectrum[i];

                    offset += spectrum_size;
                    req->valid = true;
                }

                // are there pixels and a mask
                if (chunks[idx].size == spectrum_size + pixels_size + mask_size)
                {
                    size_t plane_size = req->dimx * req->dimy;

                    const float *pixels = (float *)&(chunks[idx].memory[offset]);
                    offset += pixels_size;

                    const bool *mask = (bool *)&(chunks[idx].memory[offset]);
                    offset += mask_size;

                    // gather pixels / mask
                    for (size_t i = 0; i < plane_size; i++)
                    {
                        req->pixels[i] += pixels[i];
                        req->mask[i] |= mask[i];
                    }
                }
            }
        }
    }

    /* remove the transfers and cleanup the handles */
    for (i = 0; i < handle_count; i++)
    {
        curl_multi_remove_handle(multi_handle, handles[i]);
        curl_easy_cleanup(handles[i]);
        free(chunks[i].memory);
    }

    curl_multi_cleanup(multi_handle);

    pthread_exit(NULL);
}

void *fetch_image(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct image_req *req = (struct image_req *)ptr;

    printf("[C] calling fetch_image across the cluster for '%.*s' with {W:%d,H:%d}\n", req->len, req->datasetid, req->width, req->height);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_image (no cluster nodes found)\n");

        g_mutex_unlock(&cluster_mtx);
        pthread_exit(NULL);
    };

    CURL *handles[handle_count];
    struct MemoryStruct chunks[handle_count];
    CURLM *multi_handle;

    int still_running = 1; /* keep number of running handles */

    CURLMsg *msg;  /* for picking up messages with the transfer status */
    int msgs_left; /* how many messages are left */

    /* Allocate one CURL handle per transfer */
    for (i = 0; i < handle_count; i++)
    {
        handles[i] = curl_easy_init();

        chunks[i].memory = malloc(1);
        chunks[i].size = 0;
        chunks[i].memory[0] = 0;
    }

    /* init a multi stack */
    multi_handle = curl_multi_init();

    // html-encode the datasetid
    char datasetid[2 * req->len];
    size_t len = html_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/image/%.*s?width=%d&height=%d", options.http_port, (int)len, datasetid, req->width, req->height);
        // printf("[C] URL: '%s'\n", url->str);

        // set the individual URL
        curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

        /* send all data to this function  */
        curl_easy_setopt(handles[i], CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

        /* we pass our 'chunk' struct to the callback function */
        curl_easy_setopt(handles[i], CURLOPT_WRITEDATA, (void *)&(chunks[i]));

        // add the individual transfer
        curl_multi_add_handle(multi_handle, handles[i]);

        g_string_free(url, TRUE);

        // move on to the next cluster node
        i++;
    }

    g_mutex_unlock(&cluster_mtx);

    /* Wait for the transfers */
    while (still_running)
    {
        CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

        if (still_running)
            /* wait for activity, timeout or "nothing" */
            mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

        if (mc)
            break;
    }

    /* See how the transfers went */
    while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
    {
        if (msg->msg == CURLMSG_DONE)
        {
            int idx;
            /* Find out which handle this message is about */
            for (idx = 0; idx < handle_count; idx++)
            {
                int found = (msg->easy_handle == handles[idx]);
                if (found)
                    break;
            }

            long response_code = 0;
            curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

            printf("[C] HTTP transfer completed; cURL status %d, HTTP code %ld.\n", msg->data.result, response_code);

            // reduce (gather) the pixels & mask
            if (response_code == 200)
            {
                size_t plane_size = req->width * req->height;
                size_t expected = (1 + sizeof(float)) * plane_size;
                size_t received = chunks[idx].size;

                printf("[C] pixels/mask received: %zu,  expected: %zu bytes.\n", received, expected);

                if (received == expected)
                {
                    const float *pixels = (float *)&(chunks[idx].memory[0]);
                    const bool *mask = (bool *)&(chunks[idx].memory[sizeof(float) * plane_size]);

                    // gather pixels / mask
                    for (size_t i = 0; i < plane_size; i++)
                    {
                        req->pixels[i] += pixels[i];
                        req->mask[i] |= mask[i];
                    }
                }
            }
        }
    }

    /* remove the transfers and cleanup the handles */
    for (i = 0; i < handle_count; i++)
    {
        curl_multi_remove_handle(multi_handle, handles[i]);
        curl_easy_cleanup(handles[i]);
        free(chunks[i].memory);
    }

    curl_multi_cleanup(multi_handle);

    pthread_exit(NULL);
}