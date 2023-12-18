#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <stdbool.h>
#include <math.h>

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
#include <libtar.h>
#include "microtar.h"

// LZ4 character streams compressor
#include <lz4hc.h>

// ZFP floating-point compressor
#include <zfp.h>

// GZIP
#include <zlib.h>

// BZIP2
#include <bzlib.h>

// NASA CFITSIO
#include <fitsio.h>

// PostgreSQL
#include <libpq-fe.h>

#include "json.h"
#include "http.h"
#include "mjson.h"

#include "cluster.h"
#include "webql.h"

extern GSList *cluster;
extern GMutex cluster_mtx;

#include "hash_table.h"

#include "version.h"

#include <sqlite3.h>
static sqlite3 *splat_db = NULL;
extern options_t options; // <options> is defined in main.c

#include "mongoose.h" // mg_url_encode

#ifdef MONGOOSE_HTTP_CLIENT
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
#define PAGE "<html lang=\"en\"><head><title>FITSWEBQL SE</title>" \
             "</head><body>FITSWEBQLSE (libmicrohttpd)</body></html>"

struct MHD_Daemon *http_server = NULL;

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite, char *root);
void create_root_path(const char *root);
void *forward_fitswebql_request(void *ptr);
void *handle_fitswebql_request(void *ptr);
void *handle_notify_request(void *ptr);
void *handle_image_spectrum_request(void *args);
void *handle_image_request(void *ptr);
void *handle_composite_download_request(void *ptr);
extern int decompress(int fdin, int fdout); // Z decompression
extern int inf(int source, int dest);       // GZIP decompression
extern void zerr(int ret);                  // GZIP error reporting
extern int bunzip2(int fdin, int fdout);    // BZIP2 decompression
extern int unzip(int fdin, int fdout);      // ZIP decompression
void *decompress_Z(void *user);
void *decompress_GZ(void *user);
void *decompress_ZIP(void *user);
void *decompress_BZIP2(void *user);
void *decompress_read(void *user);
extern void *video_request(void *req);
extern void *download_request(void *req);   // a FORTRAN subroutine
extern void *viewport_request(void *req);   // a FORTRAN subroutine
extern void *cluster_pv_request(void *req); // a FORTRAN subroutine
void download_response(int fd, const char *filename);
void fetch_channel_range(char *root, char *datasetid, int len, int *start, int *end, int *status, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
void *fetch_inner_dimensions(void *ptr);
void *fetch_video_frame(void *ptr);
void *fetch_global_statistics(void *ptr);
void *fetch_image(void *ptr);
void *fetch_realtime_image_spectrum(void *ptr);
void *fetch_pv_diagram(void *ptr);
int submit_progress(char *root, char *datasetid, int len, int progress);

PGconn *jvo_db_connect(char *db);
char *get_jvo_path(PGconn *jvo_db, char *db, char *table, char *data_id);

extern void load_fits_header(char *datasetid, size_t datasetid_len, char *filepath, size_t filepath_len, char *flux, size_t flux_len);
extern void read_fits_header(void *item, int unit, size_t filesize);
extern void load_fits_file(char *datasetid, size_t datasetid_len, char *filepath, size_t filepath_len, char *flux, size_t flux_len, char *root, char *dir, size_t len);
extern void process_frame(void *item, int frame, float *data, float *pixels, bool *mask, int64_t npixels);
extern void notify_root(void *item, char *root);
extern void set_error_status_C(void *item, bool status);
extern void image_spectrum_request(void *item, int width, int height, int precision, int fetch_data, int fd);
extern void image_request(void *item, int width, int height, int fd);
extern int get_error_status(void *item);
extern int get_header_status(void *item);
extern int get_ok_status(void *item);
extern int get_image_status(void *item);
extern float get_progress(void *item);
extern float get_elapsed(void *item);
extern void get_frequency_range(void *item, double *freq_start_ptr, double *freq_end_ptr);

static size_t parse2stream(void *ptr, size_t size, size_t nmemb, void *user);
static size_t parse2file(void *ptr, size_t size, size_t nmemb, void *user);
size_t chunked_write(int fd, const char *src, size_t n);
size_t chunked_write_with_chunk(int fd, const char *src, size_t n, size_t chunk);
void write_json(int fd, GString *json);
void write_header(int fd, const char *header_str, int str_len);
void write_elapsed(int fd, const float *elapsed);
void write_csv_comments(int fd, const char *ra, const char *dec, double lng, double lat, int beam, double beam_width, double beam_height, float cx, float cy, int dimx, int dimy, double deltaV, double ref_freq, const char *specsys);
void write_csv_row(int fd, int channel, double f, double v, float intensity, int intensity_type, bool rest, const char *bunit, bool has_velocity, bool header);
void write_partial_statistics(int fd, const float *sumP, const int64_t *countP, const float *sumN, const int64_t *countN);
void write_statistics(int fd, float *dmin, float *dmax, float *dmedian, float *dmadN, float *dmadP);
void write_spectrum(int fd, const float *spectrum, int n, int precision);
void write_histogram(int fd, const int *hist, int n);
void write_viewport(int fd, int width, int height, const float *restrict pixels, const bool *restrict mask, int precision);
void write_image_spectrum(int fd, const char *flux, float pmin, float pmax, float pmedian, float black, float white, float sensitivity, float ratio_sensitivity, int width, int height, int precision, const float *restrict pixels, const bool *restrict mask);
void write_pv_diagram(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax, const int xmin, const int xmax, const double vmin, const double vmax, const int x1, const int y1, const int x2, const int y2);
void write_composite_pv_diagram(int fd, int width, int height, int precision, const float *restrict pv, const float *restrict pmean, const float *restrict pstd, const float *restrict pmin, const float *restrict pmax, const int xmin, const int xmax, const double vmin, const double vmax, const int x1, const int y1, const int x2, const int y2, int va_count);

void *stream_molecules(void *args);
static int sqlite_callback(void *userp, int argc, char **argv, char **azColName);

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

static enum MHD_Result http_no_content(struct MHD_Connection *connection)
{
    struct MHD_Response *response;
    int ret;
    const char *errorstr =
        "204 No Content";

    response =
        MHD_create_response_from_buffer(strlen(errorstr),
                                        (void *)errorstr,
                                        MHD_RESPMEM_PERSISTENT);
    if (NULL != response)
    {
        ret =
            MHD_queue_response(connection, MHD_HTTP_NO_CONTENT,
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
#ifdef SHARE
        snprintf(path, sizeof(path), SHARE "/htdocs%s", url);
#else
        snprintf(path, sizeof(path), "htdocs%s", url);
#endif
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
            MHD_add_response_header(response, "Content-Type", "text/css; charset=utf-8");

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
    if (NULL == dir)
        return http_not_found(connection);

    printf("[C] get_directory(%s)\n", dir);

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

    size_t json_len = json->len;
    gchar *json_str = g_string_free(json, FALSE);

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json_len, (void *)json_str, g_free);

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

    size_t json_len = json->len;
    gchar *json_str = g_string_free(json, FALSE);

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json_len, (void *)json_str, g_free);

    MHD_add_response_header(response, "Cache-Control", "no-cache");
    MHD_add_response_header(response, "Cache-Control", "no-store");
    MHD_add_response_header(response, "Pragma", "no-cache");
    MHD_add_response_header(response, "Content-Type", "application/json; charset=utf-8");

    enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

    MHD_destroy_response(response);

    return ret;
}

int hdr_get_int_value(char *hdr)
{
    // printf("VALUE(%s)\n", hdr);
    return atoi(hdr);
};

bool scan_fits_header(struct FITSDownloadStream *stream)
{
    char hdrLine[FITS_LINE_LENGTH + 1];
    bool end = false;

    // scan the FITS header as much as we can
    char *buffer = stream->buffer + stream->cursor;
    size_t buffer_size = stream->running_size - stream->cursor;

    hdrLine[sizeof(hdrLine) - 1] = '\0';

    int no_lines = 0;

    // process in multiples of <FITS_CHUNK_LENGTH>
    size_t work_size = buffer_size - (buffer_size % FITS_CHUNK_LENGTH);

    // printf("[C] work_size=%zu, buffer_size=%zu, cursor=%zu\n", work_size, buffer_size, stream->cursor);

    // process one line at a time
    for (size_t offset = 0; offset < work_size; offset += FITS_LINE_LENGTH)
    {
        strncpy(hdrLine, buffer + offset, FITS_LINE_LENGTH);
        // printf("%s\n", hdrLine);
        no_lines++;

        if (strncmp(hdrLine, "END       ", 10) == 0)
        {
            end = true;
            printf("[C] FITS HEADER END DETECTED.\n");
            break; // end the loop early to prevent overrunning into the data part
        };

        if (strncmp(hdrLine, "BITPIX  = ", 10) == 0)
            stream->bitpix = hdr_get_int_value(hdrLine + 10);

        if (strncmp(hdrLine, "NAXIS   = ", 10) == 0)
            stream->naxis = hdr_get_int_value(hdrLine + 10);

        if (strncmp(hdrLine, "NAXIS1  = ", 10) == 0)
            stream->naxes[0] = hdr_get_int_value(hdrLine + 10);

        if (strncmp(hdrLine, "NAXIS2  = ", 10) == 0)
            stream->naxes[1] = hdr_get_int_value(hdrLine + 10);

        if (strncmp(hdrLine, "NAXIS3  = ", 10) == 0)
            stream->naxes[2] = hdr_get_int_value(hdrLine + 10);

        if (strncmp(hdrLine, "NAXIS4  = ", 10) == 0)
            stream->naxes[3] = hdr_get_int_value(hdrLine + 10);
    }

    printf("[C] scan_fits_header:\tlines = %d\n", no_lines);

    // actual number of processed bytes
    size_t actual = no_lines * FITS_LINE_LENGTH;

    // make actual a multiple of FITS_CHUNK_LENGTH
    if (actual > 0)
        actual = FITS_CHUNK_LENGTH * ((actual - 1) / FITS_CHUNK_LENGTH + 1);

    // round up to the next FITS_CHUNK_LENGTH
    // size_t rounded = actual + (FITS_CHUNK_LENGTH - (actual % FITS_CHUNK_LENGTH));

    // move the cursor forward
    stream->cursor += actual;

    return end;
}

void scan_fits_data(struct FITSDownloadStream *stream)
{
    if (stream->frame == stream->naxes[2])
    {
        printf("[C] scan_fits_data:\tend of the stream.\n");
        return; // there is no more work to do
    }

    // check the bitpix value
    if (!(stream->bitpix == 8 || stream->bitpix == 16 || stream->bitpix == 32 || stream->bitpix == 64 || stream->bitpix == -32 || stream->bitpix == -64))
    {
        void *item = get_dataset(stream->datasetid);

        if (item != NULL)
            set_error_status_C(item, true);

        return;
    }

    size_t bytes_per_pixel = (size_t)abs(stream->bitpix) / 8;

    // scan as much FITS data as we can within a current 2D frame boundary
    char *buffer = stream->buffer + stream->cursor;
    size_t buffer_size = stream->running_size - stream->cursor;

    // process in multiples of <bytes_per_pixel>
    size_t available = buffer_size / bytes_per_pixel;

    if (available == 0)
        return;

    size_t remaining = stream->pixels_per_frame - stream->processed;
    size_t work_size = available > remaining ? remaining : available;
    // printf("[C] scan_fits_data:\tavailable #pixels = %zu, remaining: %zu, work_size = %zu.\n", available, remaining, work_size);

    // call ispc to convert the data from BIG ENDIAN to LITTLE ENDIAN  and coerce into <float>
    bool floatError = false;

    switch (stream->bitpix)
    {
    case 8:
        fits2uint8((uint8_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    case 16:
        fits2int16((int16_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    case 32:
        fits2int32((int32_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    case 64:
        fits2int64((int64_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    case -32:
        fits2float32((int32_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    case -64:
        floatError = fits2float64((int64_t *)buffer, stream->data + stream->processed, (uint32_t)work_size);
        break;
    default:
        break;
    }

    if (floatError)
    {
        void *item = get_dataset(stream->datasetid);

        if (item != NULL)
            set_error_status_C(item, true);
    }

    stream->processed += work_size;

    // printf("[C] scan_fits_data:\tstream->frame: %d, processed #pixels = %zu.\n", (stream->frame + 1), stream->processed);

    if (stream->processed == stream->pixels_per_frame)
    {
        stream->frame++;
        printf("[C] scan_fits_data:\tframe %d complete.\n", stream->frame);

        // pass the frame to FORTRAN
        void *item = get_dataset(stream->datasetid);

        if (item != NULL)
            process_frame(item, stream->frame, stream->data, stream->pixels, stream->mask, (int64_t)stream->pixels_per_frame);

        // reset the frame data
        for (size_t i = 0; i < stream->pixels_per_frame; i++)
            stream->data[i] = NAN;

        stream->processed = 0;
    }

    // move the cursor forward
    stream->cursor += (size_t)work_size * (size_t)bytes_per_pixel;

    // and then move remove the stream->cursor bytes from the head of the buffer
    memmove(stream->buffer, stream->buffer + stream->cursor, stream->running_size - stream->cursor);
    stream->running_size -= stream->cursor;
    stream->cursor = 0;

    // notify FORTRAN of the progress made in this step
    void *item = get_dataset(stream->datasetid);

    if (item != NULL)
        update_progress_C(item, (int)work_size); // <size_t> used to get downcasted to int

    // call itself again to process any leftovers
    return scan_fits_data(stream);
}

/*---------------------   NASA CFITSIO printerror() taken from cookbook.c    -----------------------------*/
void printerror(int status)
{
    /*****************************************************/
    /* Print out cfitsio error messages */
    /*****************************************************/

    if (status)
        fits_report_error(stderr, status); /* print error report */

    return;
}

// cURL FITS-parsing entry handler
static size_t parse2stream(void *ptr, size_t size, size_t nmemb, void *user)
{
    size_t written;
    size_t realsize = size * nmemb;
    // printf("[C] parse2stream(): size = %zu, nmemb = %zu, realsize = %zu.\n", size, nmemb, realsize);

    if (user == NULL) // do nothing in particular
    {
        printf("[C] parse2stream::user == NULL; terminating the download.\n");
        return 0;
    }

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;

    // switch compression type
    switch (stream->compression)
    {
    case fits_compression_unknown:
        goto default_download_handler;
        break;
    case fits_compression_none:
        goto default_download_handler;
        break;
    default:
        // write the data to the compressor input queue
        written = chunked_write_with_chunk(stream->comp_in[1], (const char *)ptr, realsize, 1024);

        if (written != realsize)
        {
            printf("[C] parse2stream::chunked_write() failed; terminating the download.\n");
            return 0;
        }
        else
        {
            // printf("[C] parse2stream::chunked_write() succeeded; %zu bytes written.\n", written);
            return realsize;
        }

        break;
    }

default_download_handler:
    return parse2file(ptr, size, nmemb, user);
}

// cURL FITS-parsing download handler
static size_t parse2file(void *ptr, size_t size, size_t nmemb, void *user)
{
    size_t realsize = size * nmemb;
    // printf("[C] parse2file(): size = %zu, nmemb = %zu, realsize = %zu.\n", size, nmemb, realsize);

    if (user == NULL) // do nothing in particular
    {
        printf("[C] parse2file::user == NULL; terminating the download.\n");
        return 0;
    }

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;

    // only pass-through uncompressed streams to the disk file
    if (stream->compression == fits_compression_none)
    {
        size_t written = fwrite(ptr, size, nmemb, stream->fp);
        // printf("[C] fwrite(): written %zu versus requested %zu bytes.\n", written, realsize);
        realsize = written;
    }

    // only process <realsize> bytes
    stream->total_size += realsize;

    if (stream->buffer == NULL)
    {
        printf("[C] stream->buffer == NULL; parse2file returns %zu bytes.\n", realsize);
        return realsize;
    }

    size_t new_size = stream->running_size + realsize;

    // resize the buffer if needed
    if (new_size > stream->buffer_size)
    {
        stream->buffer_size *= 2;
        stream->buffer = (char *)realloc(stream->buffer, stream->buffer_size);
    };

    if (stream->buffer == NULL)
    {
        printf("[C] parse2file returns %zu bytes.\n", realsize);
        return realsize;
    }

    // append the data to the buffer
    memcpy(stream->buffer + stream->running_size, ptr, realsize);
    stream->running_size += realsize;

    // infer the compression type
    if ((stream->compression == fits_compression_unknown) && ((stream->running_size - stream->cursor) >= 10))
    {
        unsigned char *header = (unsigned char *)stream->buffer + stream->cursor;

        // test the PLAIN FITS (.fits)
        if (strncmp((char *)header, "SIMPLE", 6) == 0)
            stream->compression = fits_compression_none;

        // test compress (.Z)
        if (header[0] == 0x1f && header[1] == 0x9d)
            stream->compression = fits_compression_compress;

        // test gzip (.gz)
        if (header[0] == 0x1f && header[1] == 0x8b && header[2] == 0x08)
            stream->compression = fits_compression_gzip;

        // test zip (.zip)
        if (header[0] == 'P' && header[1] == 'K' && header[2] == 0x03 && header[3] == 0x04)
            stream->compression = fits_compression_zip;

        // test bzip2 (.bz2)
        if (header[0] == 'B' && header[1] == 'Z' && header[2] == 'h')
            stream->compression = fits_compression_bzip2;

        if (stream->compression == fits_compression_unknown)
        {
            printf("[C] parse2file(): unknown compression type, aborting the download.\n");

            // end the download prematurely if the file is not a plain and / or compressed FITS file
            void *item = get_dataset(stream->datasetid);

            if (item != NULL)
                set_error_status_C(item, true);

            return 0;
        }

        if (stream->compression != fits_compression_none)
        {
            int pipefd[2];
            int status_in, status_out;

            // create the decompression pipes (in-out queues)
            status_in = pipe(pipefd);

            if (status_in == 0)
            {
                stream->comp_in[0] = pipefd[0];
                stream->comp_in[1] = pipefd[1];
            }

            status_out = pipe(pipefd);

            if (status_out == 0)
            {
                stream->comp_out[0] = pipefd[0];
                stream->comp_out[1] = pipefd[1];
            }

            // handle errors
            if (status_in != 0 || status_out != 0)
            {
                printf("[C] parse2file(): error creating the decompression pipes, aborting the download.\n");

                // close the pipes if they were created
                if (status_in == 0)
                {
                    close(stream->comp_in[0]); // close the read end
                    stream->comp_in[0] = -1;

                    close(stream->comp_in[1]); // close the write end
                    stream->comp_in[1] = -1;
                }

                if (status_out == 0)
                {
                    close(stream->comp_out[0]); // close the read end
                    stream->comp_out[0] = -1;

                    close(stream->comp_out[1]); // close the write end
                    stream->comp_out[1] = -1;
                }

                // end the download prematurely
                void *item = get_dataset(stream->datasetid);

                if (item != NULL)
                    set_error_status_C(item, true);

                return 0;
            }

            // create and detach the decompression read/write threads
            pthread_t tid;
            int stat;

            stat = pthread_create(&(stream->tid), NULL, decompress_read, (void *)stream);

            if (stat != 0)
            {
                printf("[C] parse2file(): error creating the decompress_read thread, aborting the download.\n");

                // close the pipes
                close(stream->comp_in[0]); // close the read end
                stream->comp_in[0] = -1;

                close(stream->comp_in[1]); // close the write end
                stream->comp_in[1] = -1;

                close(stream->comp_out[0]); // close the read end
                stream->comp_out[0] = -1;

                close(stream->comp_out[1]); // close the write end
                stream->comp_out[1] = -1;

                // end the download prematurely
                void *item = get_dataset(stream->datasetid);

                if (item != NULL)
                    set_error_status_C(item, true);

                return 0;
            }
            else
                stream->tid_created = true;

            switch (stream->compression)
            {
            case fits_compression_compress:
                stat = pthread_create(&tid, NULL, decompress_Z, (void *)stream);
                break;

            case fits_compression_gzip:
                stat = pthread_create(&tid, NULL, decompress_GZ, (void *)stream);
                break;

            case fits_compression_zip:
                stat = pthread_create(&tid, NULL, decompress_ZIP, (void *)stream);
                break;

            case fits_compression_bzip2:
                stat = pthread_create(&tid, NULL, decompress_BZIP2, (void *)stream);
                break;

            default:
                printf("[C] parse2file(): unknown compression type, aborting the download.\n");
                close(stream->comp_in[0]);
                stream->comp_in[0] = -1;
                close(stream->comp_out[1]);
                stream->comp_out[1] = -1;
                return 0;
                break;
            }

            if (stat != 0)
            {
                printf("[C] parse2file(): error creating the decompress_write thread, aborting the download.\n");

                // close the pipes
                close(stream->comp_in[0]); // close the read end
                stream->comp_in[0] = -1;

                close(stream->comp_in[1]); // close the write end
                stream->comp_in[1] = -1;

                close(stream->comp_out[0]); // close the read end
                stream->comp_out[0] = -1;

                close(stream->comp_out[1]); // close the write end
                stream->comp_out[1] = -1;

                // end the download prematurely
                void *item = get_dataset(stream->datasetid);

                if (item != NULL)
                    set_error_status_C(item, true);

                return 0;
            }
            else
                pthread_detach(tid);

            // pass the whole data to the decompression thread
            char *buffer = stream->buffer + stream->cursor;
            size_t buffer_size = stream->running_size - stream->cursor;
            size_t written = chunked_write_with_chunk(stream->comp_in[1], (const char *)buffer, buffer_size, 1024);

            if (written != buffer_size)
            {
                printf("[C] parse2file(): error writing to the decompression pipe, aborting the download.\n");

                // close the pipes
                close(stream->comp_in[0]); // close the read end
                stream->comp_in[0] = -1;

                close(stream->comp_in[1]); // close the write end
                stream->comp_in[1] = -1;

                close(stream->comp_out[0]); // close the read end
                stream->comp_out[0] = -1;

                close(stream->comp_out[1]); // close the write end
                stream->comp_out[1] = -1;

                // end the download prematurely
                void *item = get_dataset(stream->datasetid);

                if (item != NULL)
                    set_error_status_C(item, true);

                return 0;
            }

            // move the cursor forward
            stream->cursor += written;

            // and then move remove the stream->cursor bytes from the head of the buffer
            memmove(stream->buffer, stream->buffer + stream->cursor, stream->running_size - stream->cursor);
            stream->running_size -= stream->cursor;
            stream->cursor = 0;

            return written;
        }
        else
        {
            size_t written = fwrite(ptr, size, nmemb, stream->fp);
            // printf("[C] fwrite(): written %zu versus requested %zu bytes.\n", written, realsize);
            realsize = written;
        }
    }

    // parse the header
    if (!stream->hdrEnd)
    {
        if ((stream->running_size - stream->cursor) >= FITS_CHUNK_LENGTH)
        {
            bool end = scan_fits_header(stream);

            if (end && stream->naxis > 0)
            {
                stream->hdrEnd = true;

                // print the header string from 0 to cursor, passing the string length to printf
                // printf("[C] FITS HEADER:\n%.*s\n", stream->cursor, stream->buffer);

                size_t total_size = 1;

                for (int ii = 0; ii < stream->naxis; ii++)
                    total_size *= stream->naxes[ii];

                total_size *= abs(stream->bitpix) / 8;

                // make total_size a multiple of FITS_CHUNK_LENGTH
                if (total_size > 0)
                    total_size = FITS_CHUNK_LENGTH * ((total_size - 1) / FITS_CHUNK_LENGTH + 1);

                // finally add the header size
                total_size += stream->cursor;

                printf("[C] FITS TOTAL SIZE:\t%zu bytes\n", total_size);

                fitsfile *fptr; /* pointer to the FITS file, defined in fitsio.h */
                int status = 0;
                int unit = 0; // FORTRAN unit number

                // open an in-memory FITS file from the buffer
                if (fits_open_memfile(&fptr, stream->fname, READONLY, (void **)&stream->buffer, &stream->cursor, 0, NULL, &status))
                {
                    printf("[C] fits_open_file failed(%s) with status %d.\n", stream->fname, status);
                    printerror(status);
                }

                // get the FORTRAN unit number
                unit = CFITS2Unit(fptr);
                printf("[C] FITS FORTRAN unit number = %d\n", unit);

                // pass the header to FORTRAN for full parsing
                if (unit != 0)
                {
                    void *item = get_dataset(stream->datasetid);

                    if (item != NULL)
                        read_fits_header(item, unit, total_size);
                }

                // finally close the file
                status = 0;
                if (fits_close_file(fptr, &status))
                    printerror(status);

                // and then move remove the stream->cursor bytes from the head of the buffer
                // printf stream->cursor and stream->running_size
                printf("[C] stream->cursor = %zu, stream->running_size = %zu\n", stream->cursor, stream->running_size);
                memmove(stream->buffer, stream->buffer + stream->cursor, stream->running_size - stream->cursor);
                stream->running_size -= stream->cursor;
                stream->cursor = 0;

                stream->pixels_per_frame = stream->naxes[0] * stream->naxes[1];
                stream->data = (float *)calloc(stream->pixels_per_frame, sizeof(float));
                stream->pixels = (float *)calloc(stream->pixels_per_frame, sizeof(float));
                stream->mask = (bool *)calloc(stream->pixels_per_frame, sizeof(bool));

                for (size_t ii = 0; ii < stream->pixels_per_frame; ii++)
                {
                    stream->data[ii] = NAN;
                    stream->pixels[ii] = 0.0;
                    stream->mask[ii] = false;
                }
            }
        }
    }

    // parse the data
    if (stream->hdrEnd && (stream->frame < stream->naxes[2]))
        scan_fits_data(stream);

    // printf("[C] parse2file returns %zu bytes.\n", realsize);
    return realsize;
}

static void *handle_url_download(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    url_req_t *req = (url_req_t *)arg;

    char *url = req->url;

    char fname[1024]; // needed by the URL part
    memset(fname, '\0', sizeof(fname));

    char filepath[1024 + 6];
    memset(filepath, '\0', sizeof(filepath));

    char *extension = NULL;

    // find the last '/'
    char *last_slash = strrchr(url, '/');

    if (last_slash != NULL)
        strncpy(fname, last_slash + 1, sizeof(fname) - 1); // skip the slash character
    else
        strncpy(fname, url, sizeof(fname) - 1);

    printf("[C] filename: %s\n", fname);

    // find the last '.'
    char *ext = strrchr(fname, '.');
    if (ext != NULL)
    {
        *ext = '\0'; // terminate the fname string
        ext++;       // skip the dot character

        // strip the supported compression extensions
        if (strcmp(ext, "gz") == 0 || strcmp(ext, "bz2") == 0 || strcmp(ext, "zip") == 0 || strcmp(ext, "Z") == 0 || strcmp(ext, "GZ") == 0 || strcmp(ext, "BZ2") == 0 || strcmp(ext, "ZIP") == 0)
        {
            // skip the compression extension
            ext = strrchr(fname, '.');
            if (ext != NULL)
            {
                *ext = '\0'; // terminate the fname string
                ext++;       // skip the dot character
            }
        };

        extension = ext;
    }

    if (req->datasetid == NULL)
    {
        if (extension == NULL)
            snprintf(filepath, sizeof(filepath), "%s/%s.fits", options.fits_home, fname);
        else
            snprintf(filepath, sizeof(filepath), "%s/%s.%s", options.fits_home, fname, extension);
    }
    else
        snprintf(filepath, sizeof(filepath), "%s/%s.fits", options.fits_home, req->datasetid);

    // if the file does not exist download it
    if (access(filepath, R_OK) == -1)
    {
        char download[sizeof(filepath) + 5];
        memset(download, '\0', sizeof(download));

        // append ".tmp" to the filename
        snprintf(download, sizeof(download) - 1, "%s.tmp", filepath);

        printf("[C] '%s' does not exist, attempting a download.\n", filepath);

        // download the file to the temporary location with cURL
        CURL *curl;
        CURLcode res;

        curl = curl_easy_init();

        if (curl)
        {
            curl_easy_setopt(curl, CURLOPT_URL, url);
            curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, parse2stream);

            /* disable progress meter, set to 0L to enable it */
            /* enable progress meter, set to 1L to disable it */
            curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);

            // a download file
            FILE *downloadfile;

            // a download structure
            struct FITSDownloadStream stream;

            if (req->datasetid == NULL)
                stream.datasetid = fname;
            else
                stream.datasetid = req->datasetid;

            stream.url = url;
            stream.fname = filepath;

            /* open the file */
            downloadfile = fopen(download, "wb");

            stream.fp = downloadfile;
            stream.buffer = (char *)malloc(CURL_MAX_WRITE_SIZE + FITS_CHUNK_LENGTH);

            if (stream.buffer != NULL)
                stream.buffer_size = CURL_MAX_WRITE_SIZE + FITS_CHUNK_LENGTH;
            else
                stream.buffer_size = 0;

            stream.running_size = 0;
            stream.cursor = 0;
            stream.total_size = 0;
            stream.compression = fits_compression_unknown;
            stream.comp_in[0] = -1;
            stream.comp_in[1] = -1;
            stream.comp_out[0] = -1;
            stream.comp_out[1] = -1;

            stream.tid_created = false;

            // header
            stream.hdrEnd = false;
            stream.bitpix = 0;
            stream.naxis = 0;
            stream.naxes[0] = 0;
            stream.naxes[1] = 0;
            stream.naxes[2] = 1;
            stream.naxes[3] = 1;

            // data
            stream.data = NULL;
            stream.pixels = NULL;
            stream.mask = NULL;
            stream.pixels_per_frame = 0;
            stream.processed = 0;
            stream.frame = 0;

            // create a new FITS dataset
            char flux[] = "NULL";
            load_fits_header(stream.datasetid, strlen(stream.datasetid), stream.fname, strlen(stream.fname), flux, strlen(flux));

            if (downloadfile)
            {
                curl_easy_setopt(curl, CURLOPT_WRITEDATA, &stream);
                // curl_easy_setopt(curl, CURLOPT_BUFFERSIZE, FITS_CHUNK_LENGTH); // no longer needed, the code can hadle larger chunks

                /* Perform the request, res will get the return code */
                res = curl_easy_perform(curl);

                /*int retry = 0;

                do
                {
                    // Perform the request, res will get the return code
                    res = curl_easy_perform(curl);

                    if (res != CURLE_OK)
                    {
                        fprintf(stderr, "[C] handle_url_download: curl_easy_perform() failed: %s : %s, retrying after 1s...\n", url, curl_easy_strerror(res));
                        sleep(1);
                    }
                } while (res != CURLE_OK && retry++ < 3);*/

                // close the write pipe
                if (stream.comp_in[1] != -1)
                {
                    close(stream.comp_in[1]);
                    stream.comp_in[1] = -1;
                }

                // join the decompression read thread
                if (stream.tid_created)
                    pthread_join(stream.tid, NULL);

                // close the file
                fclose(downloadfile);

                if (res != CURLE_OK)
                {
                    fprintf(stderr, "[C] handle_url_download: curl_easy_perform() failed: %s : %s\n", url, curl_easy_strerror(res));

                    // remove the download file
                    remove(download);
                }
                else
                {
                    // get the cURL HTTP response code
                    long http_code = 0;
                    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

                    printf("[C] HTTP transfer completed; HTTP code %ld.\n", http_code);

                    if (http_code == 200)
                    {
                        // report buffer_size, running_size, total_size & cursor
                        printf("[C] FITSDownloadStream buffer_size: %zu, running_size: %zu, total_size: %zu bytes, frame: %d, cursor is at %zu.\n", stream.buffer_size, stream.running_size, stream.total_size, stream.frame, stream.cursor);

                        // report the FITS header (hdrEnd, bitpix, naxis, naxes)
                        printf("[C] FITSDownloadStream hdrEnd: %d, bitpix: %d, naxis: %d, naxes: %d, %d, %d, %d.\n", stream.hdrEnd, stream.bitpix, stream.naxis, stream.naxes[0], stream.naxes[1], stream.naxes[2], stream.naxes[3]);

                        // remove the file if the processed size was less than FITS_CHUNK_LENGTH
                        if (stream.total_size < FITS_CHUNK_LENGTH)
                        {
                            printf("[C] '%s' is less than %d bytes, removing the file.\n", filepath, FITS_CHUNK_LENGTH);
                            // remove the download file
                            remove(download);
                        }
                        else
                        {
                            void *item = get_dataset(stream.datasetid);

                            // check the error status
                            if (item != NULL)
                            {
                                if (get_error_status(item))
                                {
                                    // remove the download file
                                    remove(download);
                                }
                                else
                                {
                                    // rename the download file
                                    rename(download, filepath);
                                }
                            }
                            else
                            {
                                // remove the download file
                                remove(download);
                            }
                        }
                    }
                    else
                    {
                        // remove the download file
                        remove(download);
                    }
                }
            }

            bool error = false;
            void *item = get_dataset(stream.datasetid);

            if (item != NULL)
            {
                if (get_error_status(item))
                    error = true;
                else if (!get_header_status(item) || !get_ok_status(item))
                {
                    set_error_status_C(item, true);
                    error = true;
                }
            }
            else
                error = true;

            if (error)
            {
                // remove the download file
                remove(download);

                // remove the target download file
                remove(filepath);
            }

            free(stream.data);
            free(stream.buffer);
            free(stream.pixels);
            free(stream.mask);

            // close all the remaining Unix pipes (just to be on the safe side)
            if (stream.comp_in[0] != -1)
                close(stream.comp_in[0]);

            if (stream.comp_in[1] != -1)
                close(stream.comp_in[1]);

            if (stream.comp_out[0] != -1)
                close(stream.comp_out[0]);

            if (stream.comp_out[1] != -1)
                close(stream.comp_out[1]);

            curl_easy_cleanup(curl);
        }
    }

    // release memory, exit the thread
    free(req->datasetid);
    free(req->url);
    free(req);

    pthread_exit(NULL);
}

static enum MHD_Result on_client_connect(void *cls,
                                         const struct sockaddr *addr,
                                         socklen_t addrlen)
{
    (void)cls;     // silence gcc warnings
    (void)addrlen; // silence gcc warnings

#ifdef DEBUG
    char clienthost[NI_MAXHOST] = ""; // The clienthost will hold the IP address.
    char clientservice[NI_MAXSERV] = "";

    int stat = getnameinfo(addr, sizeof(*addr), clienthost, sizeof(clienthost), clientservice, sizeof(clientservice), NI_NUMERICHOST | NI_NUMERICSERV);

    if (stat != 0)
    {
        perror("getnameinfo");
    }
    else
    {

        printf("[C] client IP address %s, client service %s\n", clienthost, clientservice);
    }
#else
    (void)addr; // silence gcc warnings
#endif

    return MHD_YES;
};

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

    const char *user_agent = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, MHD_HTTP_HEADER_USER_AGENT);
    const char *forwarded_for = MHD_lookup_connection_value(connection, MHD_HEADER_KIND, "X-Forwarded-For");
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

            size_t json_len = json->len;
            gchar *json_str = g_string_free(json, FALSE);

            struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json_len, (void *)json_str, g_free);

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

            MHD_add_response_header(response, "Content-Type", "text/plain; charset=utf-8");

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

                    size_t json_len = json->len;
                    gchar *json_str = g_string_free(json, FALSE);

                    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json_len, (void *)json_str, g_free);

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

            size_t json_len = json->len;
            gchar *json_str = g_string_free(json, FALSE);

            struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(json_len, (void *)json_str, g_free);

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

    if (strstr(url, "/get_fits") != NULL)
    {
        int x1, y1, x2, y2;
        double frame_start, frame_end, ref_freq;

        int status;
        int pipefd[2];
        pthread_t tid;

        // get datasetId
        char **datasetId = NULL;
        int va_count = 0;

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
                datasetId[va_count - 1] = strdup(tmp);

                char enc[256];

                GString *value = g_string_new(tmp);

                mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                g_string_free(value, TRUE);
            }
        }
        else
        {
            va_count = 1;

            // allocate datasetId
            datasetId = (char **)malloc(sizeof(char *));
            datasetId[0] = strdup(tmp);

            char enc[256];

            GString *value = g_string_new(tmp);

            mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

            g_string_free(value, TRUE);
        }

        // no datasets have been found by this point
        if (va_count == 0)
            return http_bad_request(connection);

        char *_filename = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

        char *x1str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "x1");

        if (x1str == NULL)
            x1 = -1;
        else
            x1 = atoi(x1str);

        char *y1str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "y1");

        if (y1str == NULL)
            y1 = -1;
        else
            y1 = atoi(y1str);

        char *x2str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "x2");

        if (x2str == NULL)
            x2 = -1;
        else
            x2 = atoi(x2str);

        char *y2str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "y2");

        if (y2str == NULL)
            y2 = -1;
        else
            y2 = atoi(y2str);

        char *frame_start_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "frame_start");

        if (frame_start_str == NULL)
            frame_start = 0.0;
        else
            frame_start = atof(frame_start_str);

        char *frame_end_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "frame_end");

        if (frame_end_str == NULL)
            frame_end = 0.0;
        else
            frame_end = atof(frame_end_str);

        char *ref_freq_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ref_freq");

        if (ref_freq_str == NULL)
            ref_freq = 0.0;
        else
            ref_freq = atof(ref_freq_str);

        // do we have at least one dataset?
        void *item = NULL;

        for (int i = 0; i < va_count; i++)
        {
            item = get_dataset(datasetId[i]);

            if (item != NULL)
                break;
        }

        if (item == NULL)
        {
            // deallocate datasetId
            for (int i = 0; i < va_count; i++)
                free(datasetId[i]);
            free(datasetId);

            return http_not_found(connection);
        }

        if (get_error_status(item))
        {
            // deallocate datasetId
            for (int i = 0; i < va_count; i++)
                free(datasetId[i]);
            free(datasetId);

            return http_internal_server_error(connection);
        }

        if (!get_header_status(item))
        {
            // deallocate datasetId
            for (int i = 0; i < va_count; i++)
                free(datasetId[i]);
            free(datasetId);

            return http_not_found(connection);
        }

        // open a pipe
        status = pipe(pipefd);

        if (0 != status)
        {
            // deallocate datasetId
            for (int i = 0; i < va_count; i++)
                free(datasetId[i]);
            free(datasetId);

            return http_internal_server_error(connection);
        }

        char filename[1024];

        // suggest a filename for saving the downloaded file
        if (va_count == 1)
        {
            if (_filename != NULL)
                snprintf(filename, sizeof(filename) - 1, "attachment; filename=%s", _filename);
            else
                snprintf(filename, sizeof(filename) - 1, "attachment; filename=%s-subregion.fits", datasetId[0]);
        }
        else
        {
            // make a filename using a timestamp in the format "FITSWEBQLSE_%Y-%m-%d_%H-%M-%S.tar" (a thread-safe method)
            struct tm result;
            time_t now = time(NULL);
            localtime_r(&now, &result);

            char timestamp[32];
            strftime(timestamp, sizeof(timestamp) - 1, "%Y-%m-%d_%H-%M-%S", &result);

            snprintf(filename, sizeof(filename) - 1, "attachment; filename=FITSWEBQLSE_%s.tar", timestamp);
        }

        // create a response from a pipe by passing the read end of the pipe
        struct MHD_Response *response = MHD_create_response_from_pipe(pipefd[0]);

        // add headers
        MHD_add_response_header(response, "Cache-Control", "no-cache");
        MHD_add_response_header(response, "Cache-Control", "no-store");
        MHD_add_response_header(response, "Pragma", "no-cache");

        MHD_add_response_header(response, "Content-Type", "application/force-download");
        MHD_add_response_header(response, "Content-Disposition", filename);
        MHD_add_response_header(response, "Content-Transfer-Encoding", "binary");
        MHD_add_response_header(response, "Accept-Ranges", "bytes");

        // queue the response
        enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

        MHD_destroy_response(response);

        // the code below should be run in a separate thread
        // otherwise libmicrohttpd will not have a chance to read from the pipe

        // pass the write end of the pipe to Fortran
        // the binary response data will be generated in Fortran
        // printf("[C] calling viewport_request with the pipe file descriptor %d\n", pipefd[1]);

        // got all the data, prepare a request structure and pass it to FORTRAN
        struct download_request *req = (struct download_request *)malloc(sizeof(struct image_spectrum_request));

        if (req != NULL)
        {
            req->x1 = x1;
            req->x2 = x2;
            req->y1 = y1;
            req->y2 = y2;
            req->frame_start = frame_start;
            req->frame_end = frame_end;
            req->ref_freq = ref_freq;

            req->fd = pipefd[1];
            req->ptr = item;

            // create and detach the FORTRAN thread
            int stat = -1;

            if (va_count == 1)
            {
                stat = pthread_create(&tid, NULL, &download_request, req);

                if (stat == 0)
                    pthread_detach(tid);
                else
                {
                    close(pipefd[1]);
                    free(req);
                }
            }
            else
            {
                // create a new composite_download_request
                struct composite_download_request *creq = (struct composite_download_request *)malloc(sizeof(struct composite_download_request));

                if (creq == NULL)
                {
                    close(pipefd[1]);
                    free(req);

                    // deallocate datasetId
                    for (int i = 0; i < va_count; i++)
                        free(datasetId[i]);
                    free(datasetId);

                    return http_internal_server_error(connection);
                }

                // duplicate the datasetId
                creq->datasetId = (char **)malloc(va_count * sizeof(char *));
                for (int i = 0; i < va_count; i++)
                    creq->datasetId[i] = strdup(datasetId[i]);

                creq->va_count = va_count;
                creq->req = req;

                // launch a C thread calling handle_composite_download_request
                stat = pthread_create(&tid, NULL, &handle_composite_download_request, creq);

                if (stat == 0)
                    pthread_detach(tid);
                else
                {
                    close(pipefd[1]);
                    free(creq);
                    free(req);
                }
            }
        }
        else
            close(pipefd[1]);

        // deallocate datasetId
        for (int i = 0; i < va_count; i++)
            free(datasetId[i]);
        free(datasetId);

        return ret;
    }

    if (strstr(url, "/get_splatalogue") != NULL)
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

        // printf("[C] Accept-Encoding: %s\n", encoding);

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

        printf("[C] get_splatalogue: datasetId(%s); freq_start: %gGHz, freq_end: %gGHz\n", datasetId, freq_start, freq_end);

        if (freq_start > 0.0 && freq_end > 0.0)
        {
            // open a pipe
            status = pipe(pipefd);

            if (0 != status)
                return http_internal_server_error(connection);

            // create a response from the pipe by passing the read end of the pipe
            struct MHD_Response *response = MHD_create_response_from_pipe(pipefd[0]);

            // add headers
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
            return http_no_content(connection);

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
        {
            printf("[C] pipe() failed: %s\n", strerror(errno));
            return http_internal_server_error(connection);
        }

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

    if (strstr(url, "/timeout/") != NULL)
    {
        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
            datasetId++; // skip the slash character

        if (datasetId == NULL)
            return http_bad_request(connection);

        int error = 0;

        char *error_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "error");

        if (error_str != NULL)
            error = atoi(error_str);

        void *item = get_dataset(datasetId);

        if (item != NULL && error)
            set_error_status_C(item, true);

        // launch a delete thread
        {
            pthread_t tid;
            pthread_attr_t attr; // thread's attribute
            int rc;              // return code

            printf("[C] received a timeout request for %s, error status: %d.\n", datasetId, error);

            rc = pthread_attr_init(&attr);

            if (rc == 0)
            {
                rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

                if (rc == 0)
                {
                    // duplicate the datasetid, launch a 'delete' pthread in a detached state
                    char *key = strdup(datasetId);

                    rc = pthread_create(&tid, &attr, delete_hash_data_no_timeout, key);

                    if (rc != 0)
                        free(key);
                }

                pthread_attr_destroy(&attr);
            }
        }

        // a dummy response
        return http_ok(connection);
    }

    if (strstr(url, "/statistics/") != NULL)
    {
        float median;
        int first, last;

        char *datasetId = strrchr(url, '/');

        if (datasetId != NULL)
            datasetId++; // skip the slash character

        if (datasetId == NULL)
            return http_bad_request(connection);

        char *median_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "median");

        if (median_str == NULL)
            return http_bad_request(connection);
        else
            median = atof(median_str);

        char *first_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "first");

        if (first_str == NULL)
            return http_bad_request(connection);
        else
            first = atoi(first_str);

        char *last_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "last");

        if (last_str == NULL)
            return http_bad_request(connection);
        else
            last = atoi(last_str);

        // do we have a dataset?
        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_not_found(connection);

        char *json = NULL;
        float sumP, sumN;
        int64_t countP, countN;

        calculate_global_statistics_C(item, median, &sumP, &countP, &sumN, &countN, first, last);
        // printf("[C] calculate_global_statistics_C sumP = %f, countP = %ld, sumN = %f, countN = %ld\n", sumP, countP, sumN, countN);

        mjson_printf(mjson_print_dynamic_buf, &json, "{%Q:%.*g,%Q:%ld,%Q:%.*g,%Q:%ld}", "sumP", 12, sumP, "countP", countP, "sumN", 12, sumN, "countN", countN);

        // the response will be freed by libmicrohttpd
        struct MHD_Response *response = MHD_create_response_from_buffer(strlen(json), (void *)json, MHD_RESPMEM_MUST_FREE);

        MHD_add_response_header(response, "Cache-Control", "no-cache");
        MHD_add_response_header(response, "Cache-Control", "no-store");
        MHD_add_response_header(response, "Pragma", "no-cache");
        MHD_add_response_header(response, "Content-Type", "application/json; charset=utf-8");

        enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);

        MHD_destroy_response(response);

        return ret;
    }

    if (strstr(url, "/viewport/") != NULL)
    {
        int x1, y1, x2, y2;
        double frame_start, frame_end, ref_freq;
        float median;
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

        char *median_str = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "median");

        if (median_str == NULL)
            return http_bad_request(connection);
        else
        {
            // check for a NaN value, if not NaN parse the string
            median = strcmp(median_str, "nan") == 0 ? NAN : atof(median_str);
        }

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
            req->median = median;
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

    if (strstr(url, "/pv/") != NULL)
    {
        int x1, y1, x2, y2, first, last, npoints;

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

        char *firststr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "first");

        if (firststr == NULL)
            return http_bad_request(connection);
        else
            first = atoi(firststr);

        char *laststr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "last");

        if (laststr == NULL)
            return http_bad_request(connection);
        else
            last = atoi(laststr);

        char *npointsstr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "npoints");

        if (npointsstr == NULL)
            return http_bad_request(connection);
        else
            npoints = atoi(npointsstr);

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
        struct cluster_pv_request *req = (struct cluster_pv_request *)malloc(sizeof(struct image_spectrum_request));

        if (req != NULL)
        {
            req->x1 = x1;
            req->x2 = x2;
            req->y1 = y1;
            req->y2 = y2;
            req->first = first;
            req->last = last;
            req->npoints = npoints;

            req->fd = pipefd[1];
            req->ptr = item;

            // create and detach the FORTRAN thread
            int stat = pthread_create(&tid, NULL, &cluster_pv_request, req);

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

    if (strstr(url, "/video/") != NULL)
    {
        int frame, fill, width, height;
        bool downsize, keyframe, mask;
        char *flux;
        float dmin, dmax, dmedian;
        float sensitivity, slope;
        float white, black;

        int status;
        int pipefd[2];
        pthread_t tid;

        char *datasetId = strrchr(url, '/');
        if (datasetId == NULL)
            return http_bad_request(connection);

        datasetId++; // skip the slash character

        char *frameStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "frame");
        if (frameStr == NULL)
            return http_bad_request(connection);

        frame = atoi(frameStr);

        char *fillStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "fill");
        if (fillStr == NULL)
            return http_bad_request(connection);

        fill = atoi(fillStr);

        char *keyframeStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "keyframe");
        if (keyframeStr == NULL)
            return http_bad_request(connection);

        if (atoi(keyframeStr) == 1)
            keyframe = true;
        else
            keyframe = false;

        char *widthStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "width");
        if (widthStr == NULL)
            return http_bad_request(connection);

        width = atoi(widthStr);

        char *heightStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "height");
        if (heightStr == NULL)
            return http_bad_request(connection);

        height = atoi(heightStr);

        char *downsizeStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "downsize");
        if (downsizeStr == NULL)
            return http_bad_request(connection);

        if (atoi(downsizeStr) == 1)
            downsize = true;
        else
            downsize = false;

        char *maskStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "mask");
        if (maskStr == NULL)
            return http_bad_request(connection);

        if (atoi(maskStr) == 1)
            mask = true;
        else
            mask = false;

        flux = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "flux");
        if (flux == NULL)
            return http_bad_request(connection);

        char *dminStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dmin");
        if (dminStr == NULL)
            return http_bad_request(connection);

        dmin = atof(dminStr);

        char *dmaxStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dmax");
        if (dmaxStr == NULL)
            return http_bad_request(connection);

        dmax = atof(dmaxStr);

        char *dmedianStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dmedian");
        if (dmedianStr == NULL)
            return http_bad_request(connection);

        dmedian = atof(dmedianStr);

        char *sensitivityStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "sensitivity");
        if (sensitivityStr == NULL)
            return http_bad_request(connection);

        sensitivity = atof(sensitivityStr);

        char *slopeStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "slope");
        if (slopeStr == NULL)
            return http_bad_request(connection);

        slope = atof(slopeStr);

        char *whiteStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "white");
        if (whiteStr == NULL)
            return http_bad_request(connection);

        white = atof(whiteStr);

        char *blackStr = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "black");
        if (blackStr == NULL)
            return http_bad_request(connection);

        black = atof(blackStr);

        void *item = get_dataset(datasetId);

        if (item == NULL)
            return http_not_found(connection);

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
        printf("[C] calling video_request with the pipe file descriptor %d\n", pipefd[1]);

        struct video_req *req = malloc(sizeof(struct video_req));

        if (req != NULL)
        {
            req->keyframe = keyframe;
            req->frame = frame;
            req->fill = fill;
            req->flux = strdup(flux);
            req->len = strlen(req->flux);

            req->dmin = dmin;
            req->dmax = dmax;
            req->dmedian = dmedian;

            req->sensitivity = sensitivity;
            req->slope = slope;
            req->white = white;
            req->black = black;

            req->width = width;
            req->height = height;
            req->downsize = downsize;
            req->mask = mask;

            req->fd = pipefd[1];
            req->ptr = item;

            // create and detach the thread
            int stat = pthread_create(&tid, NULL, &video_request, req);

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

    // WebQL main entry page
    if (strstr(url, "FITSWebQL.html") != NULL)
    {
        char *root = NULL;

        if (!options.local)
        {
            // get the root path
            char *proot = (char *)strstr(url, "FITSWebQL.html");

            int len = proot - url;
            root = strndup(url, len);

            printf("[C] URL root path: %s\n", root);

            create_root_path(root);
        }

        // to be forwarded to cluster nodes
        GString *uri = g_string_new(url);

        g_string_append(uri, "?");

        // get datasetId
        char **datasetId = NULL;
        int va_count = 0;

        char *directory = NULL;
        char *extension = NULL;

        char *url = NULL;
        char *fallback = NULL;
        char fname[1024]; // needed by the URL part
        memset(fname, '\0', sizeof(fname));

        if (va_count == 0) // if (options.local)
        {
            directory = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");
            extension = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ext");
            char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

            if (directory != NULL)
            {
                char enc[256];

                GString *value = g_string_new(directory);

                mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                g_string_append_printf(uri, "dir=%s&", enc);

                g_string_free(value, TRUE);
            }

            if (extension != NULL)
            {
                char enc[256];

                GString *value = g_string_new(extension);

                mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

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
                    datasetId[va_count - 1] = strdup(tmp);

                    char enc[256];

                    GString *value = g_string_new(tmp);

                    mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                    g_string_append_printf(uri, "filename%d=%s&", va_count, enc);

                    g_string_free(value, TRUE);
                }
            }
            else
            {
                va_count = 1;

                // allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = strdup(tmp);

                char enc[256];

                GString *value = g_string_new(tmp);

                mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                g_string_append_printf(uri, "filename=%s&", enc);

                g_string_free(value, TRUE);
            }
        }

        if (va_count == 0)
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
                    datasetId[va_count - 1] = strdup(tmp);

                    char enc[256];

                    GString *value = g_string_new(tmp);

                    mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                    g_string_append_printf(uri, "datasetId%d=%s&", va_count, enc);

                    g_string_free(value, TRUE);
                }
            }
            else
            {
                va_count = 1;

                // allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = strdup(tmp);

                char enc[256];

                GString *value = g_string_new(tmp);

                mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                g_string_append_printf(uri, "datasetId=%s&", enc);

                g_string_free(value, TRUE);
            }
        }

        // no datasets have been found by this point, check if an external URL has been passed
        if (va_count == 0)
        {
            url = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "url");

            if (url != NULL)
            {
                char filepath[1024 + 6];
                memset(filepath, '\0', sizeof(filepath));

                printf("[C] url: %s\n", url);

                directory = options.fits_home;

                // find the last '/'
                char *last_slash = strrchr(url, '/');

                if (last_slash != NULL)
                    strncpy(fname, last_slash + 1, sizeof(fname) - 1); // skip the slash character
                else
                    strncpy(fname, url, sizeof(fname) - 1);

                printf("[C] filename: %s\n", fname);

                // find the last '.'
                char *ext = strrchr(fname, '.');
                if (ext != NULL)
                {
                    *ext = '\0'; // terminate the fname string
                    ext++;       // skip the dot character

                    // strip the supported compression extensions
                    if (strcmp(ext, "gz") == 0 || strcmp(ext, "bz2") == 0 || strcmp(ext, "zip") == 0 || strcmp(ext, "Z") == 0 || strcmp(ext, "GZ") == 0 || strcmp(ext, "BZ2") == 0 || strcmp(ext, "ZIP") == 0)
                    {
                        // skip the compression extension
                        ext = strrchr(fname, '.');
                        if (ext != NULL)
                        {
                            *ext = '\0'; // terminate the fname string
                            ext++;       // skip the dot character
                        }
                    };

                    extension = ext;
                }

                if (extension == NULL)
                    snprintf(filepath, sizeof(filepath), "%s/%s.fits", options.fits_home, fname);
                else
                    snprintf(filepath, sizeof(filepath), "%s/%s.%s", options.fits_home, fname, extension);

                // check if the file exists, if not try to download it in a new thread
                if (access(filepath, R_OK) == 0)
                {
                    va_count = 1;

                    // allocate datasetId
                    datasetId = (char **)malloc(sizeof(char *));
                    datasetId[0] = strdup(fname);

                    // check if the item already exists in the hash table
                    // if so do not forward the requests across the cluster
                    void *item = get_dataset(datasetId[0]);

                    if (item == NULL)
                    {
                        char enc[256];

                        GString *value = g_string_new(fname);

                        mg_url_encode(value->str, value->len, enc, sizeof(enc) - 1);

                        g_string_append_printf(uri, "filename=%s&", enc);

                        g_string_free(value, TRUE);

                        if (directory != NULL)
                            g_string_append_printf(uri, "dir=%s&", directory);

                        if (extension != NULL)
                            g_string_append_printf(uri, "ext=%s&", extension);
                    }
                }
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
        if (is_root_rank && va_count > 0)
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

        // logging
        if (is_root_rank && datasetId != NULL)
        {
            char stime[32] = "";
            struct tm result;
            time_t ltime = time(NULL);
            localtime_r(&ltime, &result);
            strftime(stime, sizeof(stime) - 1, "%a, %d %b %Y %H:%M:%S %Z", &result);

            FILE *accesslog = NULL;
            GString *log_file = g_string_new(options.logs);

            if (log_file != NULL)
            {
                g_string_append(log_file, "/fitswebqlse.log");
                accesslog = fopen(log_file->str, "a");
                g_string_free(log_file, TRUE);
            }

            if (accesslog != NULL)
            {
                fprintf(accesslog, "%s\t%s\t%s\t%s\t%s\t%s\t", denull(stime), (forwarded_for != NULL) ? forwarded_for : "<IP>", denull(user_agent), denull(method), (url != NULL) ? url : "<URL>", denull(version));

                for (int i = 0; i < va_count; i++)
                    fprintf(accesslog, "%s\t", denull(datasetId[i]));

                fprintf(accesslog, "\n");
                fclose(accesslog);
            }
        }

        if (datasetId != NULL)
        {
            if (is_root_rank)
                ret = execute_alma(connection, datasetId, va_count, composite, root);
            else
                ret = http_acknowledge(connection);

            int i;
            char filepath[1024];
            memset(filepath, '\0', sizeof(filepath));

            // pass the filepath to FORTRAN

            // make a filepath from the dir/extension
            for (i = 0; i < va_count; i++)
            {
                // try to insert a NULL dataset
                if (insert_if_not_exists(datasetId[i], NULL))
                {
                    // the dataset has already been loaded
                    if (!is_root_rank)
                    {
                        // notify the root of the progress (in a detached thread)
                        fits_req_t *req = (fits_req_t *)malloc(sizeof(fits_req_t));

                        if (req != NULL)
                        {
                            req->datasetid = strdup(datasetId[i]);

                            if (root_ip != NULL)
                                req->root = strdup(root_ip);
                            else
                                req->root = NULL;

                            // ignore all the other fields
                            req->filepath = NULL;
                            req->flux = NULL;

                            pthread_t tid;
                            int stat = pthread_create(&tid, NULL, &handle_notify_request, req);

                            if (stat != 0)
                            {
                                // release memory
                                free(req->datasetid);
                                free(req->root);
                                free(req);
                            }
                            else
                                pthread_detach(tid);
                        }
                    }

                    continue;
                }

                // handle both local and server cases in one go
                if (directory != NULL)
                { // options.local == true

                    if (extension == NULL)
                        snprintf(filepath, sizeof(filepath), "%s/%s.fits", directory, datasetId[i]);
                    else
                        snprintf(filepath, sizeof(filepath), "%s/%s.%s", directory, datasetId[i], extension);
                }
                else
                { // options.local == false
                    // try the <FITS_HOME> first
                    if (extension == NULL)
                        snprintf(filepath, sizeof(filepath) - 1, "%s/%s.fits", options.fits_home, datasetId[i]);
                    else
                        snprintf(filepath, sizeof(filepath) - 1, "%s/%s.%s", options.fits_home, datasetId[i], extension);

                    // if the file does not exist get the JVO path from PostgreSQL
                    if (access(filepath, R_OK) == -1)
                    {
                        printf("[C] '%s' cannot be accessed for reading, trying a JVO path next.\n", filepath);

                        char *path = NULL;
                        char dataid[256] = "";

                        PGconn *jvo_db = NULL;

                        if (db != NULL)
                        {
                            if (strcmp(db, "alma") == 0)
                                snprintf(dataid, sizeof(dataid) - 1, "%s_00_00_00", datasetId[i]);
                            else
                                snprintf(dataid, sizeof(dataid) - 1, "%s", datasetId[i]);

                            jvo_db = jvo_db_connect(db);
                        }
                        else
                        {
                            // assume db == "alma"
                            snprintf(dataid, sizeof(dataid) - 1, "%s_00_00_00", datasetId[i]);
                        }

                        if (jvo_db != NULL && table != NULL)
                        {
                            path = get_jvo_path(jvo_db, db, table, dataid);

                            if (path != NULL)
                                snprintf(filepath, sizeof(filepath) - 1, "%s", path);
                        }

                        if (jvo_db != NULL)
                            PQfinish(jvo_db);

                        // the last resort: try to download a FITS file from jvox
                        if (access(filepath, R_OK) == -1)
                        {
                            // disabled as FITSIO handling of downloads is inefficient
                            // TO-DO: a manual cluster-aware implementation is needed
                            // snprintf(filepath, sizeof(filepath) - 1, "%s://%s:%" PRIu32 "/skynode/getDataForALMA.do?db=%s&table=cube&data_id=%s", options.url_protocol, options.url_host, options.url_port, db, dataid);

                            // fill-in the fallback URL;
                            size_t url_size = strlen(options.url_protocol) + strlen(options.url_host) + 32 + strlen(dataid) + 1;

                            if (db != NULL)
                                url_size += strlen(db);
                            else
                                url_size += 4; // alma

                            url_size += 64; // for the rest of the URL

                            fallback = (char *)malloc(url_size);

                            if (fallback != NULL)
                            {
                                if (db != NULL)
                                    snprintf(fallback, url_size, "%s://%s:%" PRIu32 "/skynode/getDataForALMA.do?db=%s&table=cube&data_id=%s", options.url_protocol, options.url_host, options.url_port, db, dataid);
                                else
                                    snprintf(fallback, url_size, "%s://%s:%" PRIu32 "/skynode/getDataForALMA.do?db=alma&table=cube&data_id=%s", options.url_protocol, options.url_host, options.url_port, dataid);
                            }
                        }

                        free(path);
                    }
                }

                printf("[C] FITS filepath: '%s'\n", filepath);

                // C -> FORTRAN
                if (fallback != NULL && is_root_rank) // use the download functionality only on the root node
                {
                    printf("[C] falling back onto the URL: '%s'\n", fallback);

                    url_req_t *req = (url_req_t *)malloc(sizeof(url_req_t));

                    if (req != NULL)
                    {
                        req->datasetid = strdup(datasetId[i]);
                        req->url = fallback;

                        // create and detach a cURL download thread
                        pthread_t tid;

                        int stat = pthread_create(&tid, NULL, &handle_url_download, req);

                        if (stat != 0)
                        {
                            printf("[C] failed to create a cURL download thread.\n");

                            free(req->datasetid);
                            free(req->url);
                            free(req);
                        }
                        else
                            pthread_detach(tid);
                    }
                    else
                    {
                        free(fallback);
                    }
                }
                else
                {
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

                        pthread_t tid;
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
            }

            // directory/extension should not be freed (libmicrohttpd does that)
        }
        else
        {
            if (url != NULL)
            {
                printf("[C] external URL download: '%s'\n", url);

                va_count = 1;

                // allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = strdup(fname);

                // the URL download is supposed to be a single-server operation, running on the root node only
                if (is_root_rank)
                    ret = execute_alma(connection, datasetId, va_count, composite, root);
                else
                    ret = http_acknowledge(connection);

                // pass the URL to FORTRAN (single-server only)

                // try to insert a NULL dataset, launch a download thread if the dataset does not exist yet
                if (!insert_if_not_exists(datasetId[0], NULL))
                {
                    printf("[C] '%s' is not in the database, launching a download thread.\n", datasetId[0]);

                    url_req_t *req = (url_req_t *)malloc(sizeof(url_req_t));

                    if (req != NULL)
                    {
                        req->datasetid = NULL;
                        req->url = strdup(url);

                        // create and detach a cURL download thread
                        pthread_t tid;

                        int stat = pthread_create(&tid, NULL, &handle_url_download, req);

                        if (stat != 0)
                        {
                            printf("[C] failed to create a cURL download thread.\n");

                            free(req->datasetid);
                            free(req->url);
                            free(req);
                        }
                        else
                            pthread_detach(tid);
                    }
                }
            }
            else
                ret = http_not_found(connection);
        }

        // deallocate datasetId
        for (int i = 0; i < va_count; i++)
            free(datasetId[i]);

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

void create_root_path(const char *root)
{
    if (root == NULL)
        return;

// prepend root by "htdocs/"
#ifdef SHARE
    char *link = malloc(strlen(root) + strlen(SHARE "/htdocs") + 1);
    sprintf(link, SHARE "/htdocs%s", root); // '/' is added by the server
#else
    char *link = malloc(strlen(root) + strlen("htdocs") + 1);
    sprintf(link, "htdocs%s", root); // '/' is added by the server
#endif

    // remove the last slash
    if (link[strlen(link) - 1] == '/')
        link[strlen(link) - 1] = '\0';

    // check if <link> is not already a symlink
    struct stat st;
    stat(link, &st);

    if (S_ISLNK(st.st_mode))
    {
        free(link);
        return;
    }

    // create a symbolic link to the "htdocs/fitswebql" directory
    int stat = symlink("fitswebql", link);

    if (stat == -1)
        perror(link);
    else
        printf("[C] symlink created: %s -> \"fitswebql\"\n", link);

    free(link);
}

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite, char *root)
{
    int i;
    bool has_fits = true;

    // go through the dataset list looking up entries in the hash table
    for (i = 0; i < va_count; i++)
        has_fits = has_fits && dataset_exists(va_list[i]);

    // the string holding the dynamically generated HTML content
    GString *html = g_string_new("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n");

    g_string_append(html,
                    "<link href=\"https://fonts.googleapis.com/css?family=Inconsolata\" "
                    "rel=\"stylesheet\"/>\n");
    g_string_append(html,
                    "<link href=\"https://fonts.googleapis.com/css?family=Material+Icons\" "
                    "rel=\"stylesheet\"/>\n");
    g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/npm/d3@7\"></script>\n");
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
                          "fitswebql/marchingsquares-isocontours.min.js\" defer></script>\n");
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/marchingsquares-isobands.min.js\" defer></script>\n");

    // Font Awesome
    g_string_append(html, "<script src=\"https://kit.fontawesome.com/8433b7dde2.js\" crossorigin=\"anonymous\"></script>\n");

    // HTML5 FileSaver
    g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/FileSaver.min.js\"></script>\n");

    // client-side colourmaps (used by the composite legend)
    g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/colourmaps.min.js\"></script>\n");

    // d3.js colour scale legend
    g_string_append(html, "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3-legend/2.25.6/d3-legend.min.js\" integrity=\"sha512-wNH6xsp2n8CfB91nrBtfc4sfLwYPBMjSWVUwQOp60AYYXH6i8yCwuKFZ4rgK2i6pQek/b+bSyR7b01/922IBzQ==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\"></script>\n");

    // mathjs
    g_string_append(html, "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjs/11.3.3/math.min.js\" integrity =\"sha512-AZlpUxTyDgo/Ne1TyeXv345mwyAKh646DnZSsEt0GAF7aSFWu94UjCBcgm+yqKyH/6g9boKAzIEMO+wEGc6mJQ==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\"></script>");

    // WebAssembly JS+binary
    if (options.local)
    {
        // local version
        g_string_append(html, "<script src=\"client." WASM_VERSION ".js\"></script>\n");
    }
    else
    {
        // server version: use the CDN version of WASM files
        g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR(VERSION_SUB) "/htdocs/fitswebql/client." WASM_VERSION ".min.js\"></script>\n");
    }

    // WebAssembly JavaScript initialisation
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
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/vertex-shader.vert");
#else
    include_file(html, "htdocs/fitswebql/vertex-shader.vert");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"legend-vertex-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/legend-vertex-shader.vert");
#else
    include_file(html, "htdocs/fitswebql/legend-vertex-shader.vert");
#endif
    g_string_append(html, "</script>\n");

    // GLSL fragment shaders
    g_string_append(html, "<script id=\"common-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/common-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/common-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"legend-common-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/legend-common-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/legend-common-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    // tone mappings
    g_string_append(html, "<script id=\"ratio-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/ratio-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/ratio-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"ratio-composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/ratio-composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/ratio-composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"logistic-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/logistic-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/logistic-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"logistic-composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/logistic-composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/logistic-composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"square-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/square-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/square-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"square-composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/square-composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/square-composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"legacy-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/legacy-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/legacy-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"legacy-composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/legacy-composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/legacy-composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"linear-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/linear-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/linear-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"linear-composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/linear-composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/linear-composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    // colourmaps
    g_string_append(html, "<script id=\"composite-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/composite-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/composite-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"greyscale-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/greyscale-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/greyscale-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"negative-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/negative-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/negative-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"amber-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/amber-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/amber-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"red-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/red-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/red-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"green-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/green-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/green-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"blue-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/blue-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/blue-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"hot-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/hot-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/hot-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"rainbow-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/rainbow-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/rainbow-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"parula-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/parula-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/parula-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"inferno-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/inferno-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/inferno-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"magma-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/magma-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/magma-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"plasma-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/plasma-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/plasma-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"viridis-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/viridis-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/viridis-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"cubehelix-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/cubehelix-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/cubehelix-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"jet-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/jet-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/jet-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    g_string_append(html, "<script id=\"haxby-shader\" type=\"x-shader/x-vertex\">\n");
#ifdef SHARE
    include_file(html, SHARE "/htdocs/fitswebql/haxby-shader.frag");
#else
    include_file(html, "htdocs/fitswebql/haxby-shader.frag");
#endif
    g_string_append(html, "</script>\n");

    // FITSWebQL main JavaScript + CSS
    if (options.local)
    {
        // local version
        g_string_append(html, "<script src=\"fitswebqlse.js?" VERSION_STRING "\"></script>\n");
        g_string_append(html, "<link rel=\"stylesheet\" href=\"fitswebqlse.css?" VERSION_STRING
                              "\"/>\n");
    }
    else
    {
        // server version: use automatically minified files served from the CDN
        g_string_append(html, "<script src=\"https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR(VERSION_SUB) "/htdocs/fitswebql/fitswebqlse.min.js\"></script>\n");
        g_string_append(html, "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR(VERSION_SUB) "/htdocs/fitswebql/fitswebqlse.min.css\"/>\n");
    }

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
            g_string_append_printf(html, "data-root-path='%s' ", root);
        else
            g_string_append(html, "data-root-path='/' ");
    }
    else
        g_string_append(html, "data-root-path='/' ");

    g_string_append(html, " data-server-version='" VERSION_STRING "' data-server-string='" SERVER_STRING);

    g_string_append_printf(html, "' data-version-major='%d", VERSION_MAJOR);
    g_string_append_printf(html, "' data-version-minor='%d", VERSION_MINOR);
    g_string_append_printf(html, "' data-version-sub='%d", VERSION_SUB);

    if (options.local)
        g_string_append(html, "' data-server-mode='LOCAL");
    else
        g_string_append(html, "' data-server-mode='SERVER");

    g_string_append_printf(html, "' data-has-fits='%d'></div>\n", (has_fits ? 1 : 0));

    g_string_append_printf(html, "<script>var WS_PORT = %" PRIu16 ";</script>\n", options.ws_port);

    // scrollIntoView with ZenScroll (the original one does not work in Safari)
    g_string_append(html, "<script "
                          "src=\"https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/"
                          "fitswebql/zenscroll-min.js\"></script>\n");

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
              "var idleSearch = -1;"
              "var idleResize = -1;"
              "var idleWindow = -1;"
              "var idlePV = -1;"
              "window.onresize = resizeMe;"
              "window.onbeforeunload = close_websocket_connections;"
              "mainRenderer(); </script>\n");

    g_string_append(html, "</body></html>");

    size_t html_len = html->len;
    gchar *html_str = g_string_free(html, FALSE);

    struct MHD_Response *response = MHD_create_response_from_buffer_with_free_callback(html_len, (void *)html_str, g_free);
    // deallocate the html content after libmicrohttpd has taken ownership of the string

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

    // http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
    http_server = MHD_start_daemon(MHD_USE_AUTO | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
                                   options.http_port,
                                   &on_client_connect,
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
#ifdef SHARE
        int rc = sqlite3_open_v2(SHARE "/splatalogue_v3.db", &splat_db, SQLITE_OPEN_READONLY | SQLITE_OPEN_FULLMUTEX, NULL);
#else
        int rc = sqlite3_open_v2("splatalogue_v3.db", &splat_db, SQLITE_OPEN_READONLY | SQLITE_OPEN_FULLMUTEX, NULL);
#endif

        if (rc)
        {
            fprintf(stderr, "[C] Can't open local splatalogue database: %s\n", sqlite3_errmsg(splat_db));
            sqlite3_close(splat_db);
            splat_db = NULL;
        }

#ifdef DEBUG
        printf("[C] µHTTP daemon listening on port %" PRIu16 "... Press CTRL-C to stop it.\n", options.http_port);
#endif
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

void *handle_notify_request(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    fits_req_t *req = (fits_req_t *)ptr;

    if (req->root == NULL)
        goto quit_notify_request;

    printf("[C] datasetid: '%s', root IP: '%s'; over to FORTRAN\n", req->datasetid, req->root);

    // get a dataset
    void *item = get_dataset(req->datasetid);

    if (item == NULL)
        goto quit_notify_request;

    // call FORTRAN
    notify_root(item, req->root);

quit_notify_request:
    free(req->datasetid);
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

void *handle_composite_download_request(void *ptr)
{
    int i;

    if (ptr == NULL)
        pthread_exit(NULL);

    struct composite_download_request *composite_req = (struct composite_download_request *)ptr;

    // check the va_count first
    if (composite_req->va_count == 0)
    {
        if (composite_req->req != NULL)
        {
            close(composite_req->req->fd);
            free(composite_req->req);
        }

        free(composite_req);
        pthread_exit(NULL);
    }

    if (composite_req->req == NULL)
    {
        // iterate through va_count and free the datasetId
        for (i = 0; i < composite_req->va_count; i++)
            free(composite_req->datasetId[i]);

        // free the datasetId array
        free(composite_req->datasetId);
        free(composite_req);

        pthread_exit(NULL);
    }

    // open for writing an in-memory tar archive using libtar
    /*TAR *pTar = NULL;

    if (tar_fdopen(&pTar, composite_req->req->fd, "FITSWEBQLSE", NULL, O_WRONLY | O_CREAT, 0644, TAR_GNU | TAR_VERBOSE) != 0)
    {
        // iterate through va_count and free the datasetId
        for (i = 0; i < composite_req->va_count; i++)
            free(composite_req->datasetId[i]);

        // free the datasetId array
        free(composite_req->datasetId);
        close(composite_req->req->fd);
        free(composite_req->req);
        free(composite_req);

        perror("[C] handle_composite_download_request tar_fdopen");
        pthread_exit(NULL);
    }*/

    mtar_t tar;

    /* Open archive for writing */
    int stat = mtar_open(&tar, composite_req->req->fd, "w");

    if (stat == MTAR_EOPENFAIL)
        close(composite_req->req->fd);

    if (stat != MTAR_ESUCCESS)
    {
        // iterate through va_count and free the datasetId
        for (i = 0; i < composite_req->va_count; i++)
            free(composite_req->datasetId[i]);

        // free the datasetId array
        free(composite_req->datasetId);
        free(composite_req->req);
        free(composite_req);

        perror("[C] handle_composite_download_request mtar_open");
        pthread_exit(NULL);
    }

    // iterate through datasets (duplicate the <download_request> structure as <req> will be freed from within FORTRAN)
    for (i = 0; i < composite_req->va_count; i++)
    {
        /*if (pTar == NULL)
            break;*/

        pthread_t tid;
        int tstat = -1;

        // create a new Unix pipe
        int pipefd[2];

        if (pipe(pipefd) != 0)
        {
            perror("[C] handle_composite_download_request pipe");
            continue;
        }

        struct download_request *req = (struct download_request *)malloc(sizeof(struct download_request));
        if (req == NULL)
            continue;

        req->x1 = composite_req->req->x1;
        req->x2 = composite_req->req->x2;
        req->y1 = composite_req->req->y1;
        req->y2 = composite_req->req->y2;
        req->frame_start = composite_req->req->frame_start;
        req->frame_end = composite_req->req->frame_end;
        req->ref_freq = composite_req->req->ref_freq;
        req->fd = pipefd[1];
        req->ptr = composite_req->req->ptr;

        // call FORTRAN, the result will be written to the pipe and FORTRAN will close the write end of the pipe
        tstat = pthread_create(&tid, NULL, &download_request, req);

        if (tstat == 0)
            pthread_detach(tid);
        else
        {
            close(pipefd[1]);
            free(req);
        }

        // read from the pipe and write to the tar archive
        ssize_t n = 0;
        size_t offset = 0;
        size_t buf_size = 0x40000;

        char *buf = malloc(buf_size);

        if (buf != NULL)
            while ((n = read(pipefd[0], buf + offset, buf_size - offset)) > 0)
            {
                offset += n;

                // printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

                if (offset == buf_size)
                {
                    printf("[C] OFFSET == BUF_SIZE, re-sizing the buffer\n");

                    size_t new_size = buf_size << 1;
                    char *tmp = realloc(buf, new_size);

                    if (tmp != NULL)
                    {
                        buf = tmp;
                        buf_size = new_size;
                    }
                }
            }

        // close the read pipe
        close(pipefd[0]);

        if (0 == n)
            printf("[C] PIPE_END_OF_STREAM\n");

        if (n < 0)
            printf("[C] PIPE_END_WITH_ERROR\n");

        if (buf != NULL)
        {
#if DEBUG
            printf("[C] calling mtar_write_file_header [%s]::%zu bytes\n", composite_req->datasetId[i], offset);
#endif

            // write the partial FITS file to the tar archive
            // TO-DO: handle 64-bit <size_t> file sizes (right now mtar uses <unsigned int> internally)
            mtar_write_file_header(&tar, composite_req->datasetId[i], (unsigned int)offset);
            mtar_write_data(&tar, buf, (unsigned int)offset);

            free(buf);
        }
    }

    // finalise the tar archive
    /*if (pTar != NULL)
    {
        tar_append_eof(pTar);
        tar_close(pTar);
    }
    else
        close(composite_req->req->fd);*/

    /* Finalize -- this needs to be the last thing done before closing */
    mtar_finalize(&tar);

    /* Close archive */
    mtar_close(&tar);

    // iterate through va_count and free the datasetId
    for (i = 0; i < composite_req->va_count; i++)
        free(composite_req->datasetId[i]);

    // free the datasetId array
    free(composite_req->datasetId);
    free(composite_req->req);
    free(composite_req);

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

    if (root == NULL)
    {
        printf("[C] root == NULL\n");

        char *id = strndup(datasetid, len);
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

    // html-encode the datasetid
    char _id[2 * len];
    size_t _len = mg_url_encode(datasetid, len, _id, sizeof(_id) - 1);

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
        g_string_append_printf(url, "%" PRIu16 "/range/%.*s", options.ws_port, (int)_len, _id);
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

            int retry = 0;

            do
            {
                /* Perform the request, res will get the return code */
                res = curl_easy_perform(curl);

                if (res != CURLE_OK)
                {
                    fprintf(stderr, "[C] fetch_channel_range: curl_easy_perform() failed: %s, retrying after 1s...\n", curl_easy_strerror(res));
                    sleep(1);
                }
            } while (res != CURLE_OK && retry++ < 5);

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
}

int submit_progress(char *root, char *datasetid, int len, int progress)
{
    if (root == NULL)
        return 0;

    // html-encode the datasetid
    char _id[2 * len];
    size_t _len = mg_url_encode(datasetid, len, _id, sizeof(_id) - 1);

    int counter = 0; // by default return 0, i.e. no progress could be submitted to the cluster root

    if (progress > 0)
    {
        // form an HTTP request URL
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", root);
        g_string_append_printf(url, "%" PRIu16 "/progress/%.*s", options.ws_port, (int)_len, _id);

#ifdef MONGOOSE_HTTP_CLIENT
        // use mongoose HTTP client as libcURL leaks memory in Intel Clear Linux ...
        // apparently it's OK, it is not a real memory leak, just DBus caching ...
        // https://github.com/clearlinux/distribution/issues/2574#issuecomment-1058618721
        progress_t req = {url->str, options.ws_port, progress, &counter, false};

        struct mg_mgr mgr; // Event manager

        mg_log_set(MG_LL_DEBUG);                            // Set to 0 to disable debug
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

            int retry = 0;

            do
            {
                /* Perform the request, res will get the return code */
                res = curl_easy_perform(curl);

                if (res != CURLE_OK)
                {
                    fprintf(stderr, "[C] submit_progress: curl_easy_perform() failed: %s, retrying after 1s...\n", curl_easy_strerror(res));
                    sleep(1);
                }
            } while (res != CURLE_OK && retry++ < 5);

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

    return counter;
}

void download_response(int fd, const char *filename)
{
    FILE *fp = fdopen(fd, "w");

    if (fp == NULL)
    {
        perror("[C] download_response");
        close(fd); // close a pipe if it could not have been converted into FILE*
        return;
    }

    fitsfile *fptr;
    int status = 0;

    fits_open_image(&fptr, filename, READONLY, &status);
    if (status)
    {
        printf("[C] error opening a fits file '%s' for reading.\n", filename);
        fits_report_error(stderr, status);

        fclose(fp);
        return;
    }

    fits_write_hdu(fptr, fp, &status);
    if (status) /* print any error messages */
        fits_report_error(stderr, status);

    fits_close_file(fptr, &status);
    if (status) /* print any error messages */
        fits_report_error(stderr, status);

    // otherwise call fclose(fp) and return
    fclose(fp);
    return;
}

size_t chunked_read_with_chunk(int fd, char *dst, size_t n, size_t chunk)
{
    size_t nchar, remaining, offset;
    ssize_t nread;

    remaining = n;
    offset = 0;

    while (remaining > 0)
    {
        nchar = MIN(remaining, chunk);
        nread = read(fd, dst + offset, nchar);

        if (nread > 0)
        {
            remaining -= nread;
            offset += nread;
        }

        // bail out upon errors
        if (nread < 0)
        {
            printf("[C] read returned %ld, aborting.\n", nread);
            return offset;
        }

        // printf("[C] chars read: %zu out of %zu bytes.\n", offset, n);
    }

    return offset;
}

size_t chunked_write(int fd, const char *src, size_t n)
{
    return chunked_write_with_chunk(fd, src, n, CHUNK); // use PIPE_BUF instead of CHUNK?
}

size_t chunked_write_with_chunk(int fd, const char *src, size_t n, size_t chunk)
{
    size_t nchar, remaining, offset;
    ssize_t written;

    remaining = n;
    offset = 0;

    while (remaining > 0)
    {
        nchar = MIN(remaining, chunk);
        written = write(fd, src + offset, nchar);

        if (written > 0)
        {
            remaining -= written;
            offset += written;
        }

        // bail out upon errors
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

void write_partial_statistics(int fd, const float *sumP, const int64_t *countP, const float *sumN, const int64_t *countN)
{
    chunked_write(fd, (const char *)sumP, sizeof(float));
    chunked_write(fd, (const char *)countP, sizeof(int64_t));

    chunked_write(fd, (const char *)sumN, sizeof(float));
    chunked_write(fd, (const char *)countN, sizeof(int64_t));
}

void write_statistics(int fd, float *dmin, float *dmax, float *dmedian, float *dmadN, float *dmadP)
{
    chunked_write(fd, (const char *)dmin, sizeof(float));
    chunked_write(fd, (const char *)dmax, sizeof(float));
    chunked_write(fd, (const char *)dmedian, sizeof(float));
    chunked_write(fd, (const char *)dmadN, sizeof(float));
    chunked_write(fd, (const char *)dmadP, sizeof(float));
}

void write_histogram(int fd, const int *hist, int n)
{
    uint32_t length = n;

    chunked_write(fd, (const char *)&length, sizeof(length)); // histogram length
    chunked_write(fd, (const char *)hist, n * sizeof(int));
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

void write_viewport(int fd, int width, int height, const float *restrict pixels, const bool *restrict mask, int precision)
{
    uchar *restrict compressed_pixels = NULL;
    char *restrict compressed_mask = NULL;

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

void write_image_spectrum(int fd, const char *flux, float pmin, float pmax, float pmedian, float black, float white, float sensitivity, float ratio_sensitivity, int width, int height, int precision, const float *restrict pixels, const bool *restrict mask)
{
    uchar *restrict compressed_pixels = NULL;
    char *restrict compressed_mask = NULL;

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

void write_pv_diagram(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax, const int xmin, const int xmax, const double vmin, const double vmax, const int x1, const int y1, const int x2, const int y2)
{
    uchar *restrict compressed_pv = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = width;
    uint ny = height;

    if (pv == NULL)
        return;

    if (width <= 0 || height <= 0)
        return;

    printf("[C] fd: %d; width: %d; height: %d, precision: %d, pmean: %f, pstd: %f, pmin: %f, pmax: %f, xmin: %d, xmax: %d, vmin: %f, vmax: %f\n", fd, width, height, precision, pmean, pstd, pmin, pmax, xmin, xmax, vmin, vmax);

    // compress PV with ZFP
    field = zfp_field_2d((void *)pv, data_type, nx, ny);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    // zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed_pv = (uchar *)malloc(bufsize);

    if (compressed_pv != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed_pv, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] P-V Diagram compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pv[0..zfpsize-1]
        }
    }
    else
        printf("[C] a NULL compressed_pv buffer!\n");

    // clean up
    zfp_field_free(field);
    zfp_stream_close(zfp);

    const char id[] = "ZFP";

    // pad the id with spaces so that the length is a multiple of 4 (JavaScript needs it ...)
    int padded_len = 4 * (strlen(id) / 4 + 1);

    // memory for a new padded id
    char padded_id[padded_len + 1];

    // right-pad the id with spaces
    rpad(padded_id, id, ' ', padded_len);

    // transmit the data
    float tmp;
    double tmp2;
    uint32_t xlen, ylen;

    uint32_t id_len = strlen(padded_id);

    uint32_t img_width = width;
    uint32_t img_height = height;
    uint32_t pv_len = zfpsize;

    // the id length
    chunked_write(fd, (const char *)&id_len, sizeof(id_len));

    // id
    chunked_write(fd, padded_id, id_len);

    // pmin
    tmp = pmin;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // pmax
    tmp = pmax;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // pmean
    tmp = pmean;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // pstd
    tmp = pstd;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // vmin
    tmp2 = vmin;
    chunked_write(fd, (const char *)&tmp2, sizeof(tmp2));

    // vmax
    tmp2 = vmax;
    chunked_write(fd, (const char *)&tmp2, sizeof(tmp2));

    // x1
    xlen = x1;
    chunked_write(fd, (const char *)&xlen, sizeof(xlen));

    // y1
    ylen = y1;
    chunked_write(fd, (const char *)&ylen, sizeof(ylen));

    // x2
    xlen = x2;
    chunked_write(fd, (const char *)&xlen, sizeof(xlen));

    // y2
    ylen = y2;
    chunked_write(fd, (const char *)&ylen, sizeof(ylen));

    // the P-V diagram
    chunked_write(fd, (const char *)&img_width, sizeof(img_width));
    chunked_write(fd, (const char *)&img_height, sizeof(img_height));

    // pv (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&pv_len, sizeof(pv_len));
    if (compressed_pv != NULL)
        chunked_write(fd, (char *)compressed_pv, pv_len);

    // release the memory
    free(compressed_pv);
}

void write_composite_pv_diagram(int fd, int width, int height, int precision, const float *restrict pv, const float *restrict pmean, const float *restrict pstd, const float *restrict pmin, const float *restrict pmax, const int xmin, const int xmax, const double vmin, const double vmax, const int x1, const int y1, const int x2, const int y2, int va_count)
{
    uchar *restrict compressed_pv = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = width;
    uint ny = height;
    uint nz = va_count;

    if (pv == NULL)
        return;

    if (width <= 0 || height <= 0 || va_count <= 0)
        return;

    printf("[C] fd: %d; width: %d; height: %d, precision: %d, xmin: %d, xmax: %d, vmin: %f, vmax: %f va_count: %d\n", fd, width, height, precision, xmin, xmax, vmin, vmax, va_count);

    // compress PV with ZFP
    field = zfp_field_3d((void *)pv, data_type, nx, ny, nz);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    // zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed_pv = (uchar *)malloc(bufsize);

    if (compressed_pv != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed_pv, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] P-V Diagram compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pv[0..zfpsize-1]
        }
    }
    else
        printf("[C] a NULL compressed_pv buffer!\n");

    // clean up
    zfp_field_free(field);
    zfp_stream_close(zfp);

    const char id[] = "ZFP";

    // pad the id with spaces so that the length is a multiple of 4 (JavaScript needs it ...)
    int padded_len = 4 * (strlen(id) / 4 + 1);

    // memory for a new padded id
    char padded_id[padded_len + 1];

    // right-pad the id with spaces
    rpad(padded_id, id, ' ', padded_len);

    // transmit the data
    double tmp;
    uint32_t xlen, ylen;

    uint32_t id_len = strlen(padded_id);

    uint32_t img_width = width;
    uint32_t img_height = height;
    uint32_t pv_len = zfpsize;

    // the id length
    chunked_write(fd, (const char *)&id_len, sizeof(id_len));

    // id
    chunked_write(fd, padded_id, id_len);

    // pmin
    chunked_write(fd, (const char *)pmin, va_count * sizeof(float));

    // pmax
    chunked_write(fd, (const char *)pmax, va_count * sizeof(float));

    // pmean
    chunked_write(fd, (const char *)pmean, va_count * sizeof(float));

    // pstd
    chunked_write(fd, (const char *)pstd, va_count * sizeof(float));

    // vmin
    tmp = vmin;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // vmax
    tmp = vmax;
    chunked_write(fd, (const char *)&tmp, sizeof(tmp));

    // x1
    xlen = x1;
    chunked_write(fd, (const char *)&xlen, sizeof(xlen));

    // y1
    ylen = y1;
    chunked_write(fd, (const char *)&ylen, sizeof(ylen));

    // x2
    xlen = x2;
    chunked_write(fd, (const char *)&xlen, sizeof(xlen));

    // y2
    ylen = y2;
    chunked_write(fd, (const char *)&ylen, sizeof(ylen));

    // the P-V diagram
    chunked_write(fd, (const char *)&img_width, sizeof(img_width));
    chunked_write(fd, (const char *)&img_height, sizeof(img_height));

    // pv (use a chunked version for larger tranfers)
    chunked_write(fd, (const char *)&pv_len, sizeof(pv_len));
    if (compressed_pv != NULL)
        chunked_write(fd, (char *)compressed_pv, pv_len);

    // release the memory
    free(compressed_pv);
}

void split_wcs(const char *coord, char *key, char *value, const char *null_key)
{
    char *pos = strchr(coord, ':');

    if (pos == NULL)
    {
        // default dummy values
        strcpy(key, null_key);
        strcpy(value, "N/A");
        return;
    }

    size_t len = strlen(coord);
    size_t key_len = pos - coord + 1;

    memcpy(key, coord, key_len);
    memcpy(value, pos + 1, len - key_len);

    // terminate the key with '\0'
    key[key_len - 1] = '\0';

    // terminate the value with '\0'
    value[len - key_len] = '\0';

    // blank out ':' with '\0'
    pos = strrchr(key, ':');
    if (pos != NULL)
        *pos = '\0';

    // remove the '\' character before the final double quote
    pos = strrchr(value, '\\');
    if (pos != NULL)
    {
        *pos = *(pos + 1); // this overwrites '\' with the next character (most likely a double quote)
        *(++pos) = '\0';   // hide the next (last) character
    }
}

void write_csv_row(int fd, int channel, double f, double v, float intensity, int intensity_type, bool rest, const char *bunit, bool has_velocity, bool header)
{
    char line[1024];
    char frequency_column[32] = "";
    char intensity_column[32] = "";

    if (header)
    {
        if (rest)
            snprintf(frequency_column, sizeof(frequency_column) - 1, "rest frequency [GHz]");
        else
            snprintf(frequency_column, sizeof(frequency_column) - 1, "frequency [GHz]");

        // default intensity unit
        snprintf(intensity_column, sizeof(intensity_column) - 1, "intensity [%s]", bunit);

        if (intensity_type == mean)
            snprintf(intensity_column, sizeof(intensity_column) - 1, "mean intensity [%s]", bunit);

        if (intensity_type == integrated)
        {
            if (has_velocity)
                snprintf(intensity_column, sizeof(intensity_column) - 1, "integrated intensity [%s•km/s]", bunit);
            else
                snprintf(intensity_column, sizeof(intensity_column) - 1, "integrated intensity [%s]", bunit);
        }
    }

    if (!isnan(f) && !isnan(v))
    {
        if (header)
        {
            snprintf(line, sizeof(line) - 1, "\"channel\",\"%s\",\"velocity [km/s]\",\"%s\"\n", frequency_column, intensity_column);
            chunked_write(fd, line, strlen(line));
        }

        snprintf(line, sizeof(line) - 1, "%d,%f,%f,%f\n", channel, f, v, intensity);
        chunked_write(fd, line, strlen(line));

        return;
    }

    if (!isnan(v))
    {
        if (header)
        {
            snprintf(line, sizeof(line) - 1, "\"channel\",\"velocity [km/s]\",\"%s\"\n", intensity_column);
            chunked_write(fd, line, strlen(line));
        }

        snprintf(line, sizeof(line) - 1, "%d,%f,%f\n", channel, v, intensity);
        chunked_write(fd, line, strlen(line));

        return;
    }

    if (!isnan(f))
    {
        if (header)
        {
            snprintf(line, sizeof(line) - 1, "\"channel\",\"%s\",\"%s\"\n", frequency_column, intensity_column);
            chunked_write(fd, line, strlen(line));
        }

        snprintf(line, sizeof(line) - 1, "%d,%f,%f\n", channel, f, intensity);
        chunked_write(fd, line, strlen(line));

        return;
    }
}

void write_csv_comments(int fd, const char *ra, const char *dec, double lng, double lat, int beam, double beam_width, double beam_height, float cx, float cy, int dimx, int dimy, double deltaV, double ref_freq, const char *specsys)
{
    char line[1024];
    char ra_key[32], ra_value[32];
    char dec_key[32], dec_value[32];

    split_wcs(ra, ra_key, ra_value, "beam ra");
    split_wcs(dec, dec_key, dec_value, "beam dec");

    // printf("[C] RA(%s) DEC(%s)\n", ra, dec);
    // printf("[C] RA '%s' : '%s'\n", ra_key, ra_value);
    // printf("[C] DEC '%s' : '%s'\n", dec_key, dec_value);
    // printf("# ra (%s):%s, dec (%s):%s\n", ra_key, ra_value, dec_key, dec_value);

    // ra/dec
    snprintf(line, sizeof(line) - 1, "# ra (%s):%s\n# dec (%s):%s\n", ra_key, ra_value, dec_key, dec_value);
    chunked_write(fd, line, strlen(line));

    // lng / lat [deg]
    snprintf(line, sizeof(line) - 1, "# wcs.lng [deg]: %g\n# wcs.lat [deg]: %g\n", lng, lat);
    chunked_write(fd, line, strlen(line));

    // beam type
    strcpy(line, "# region type: N/A\n");

    if (beam == square)
        snprintf(line, sizeof(line) - 1, "# region type: square/rect.\n");

    if (beam == circle)
        snprintf(line, sizeof(line) - 1, "# region type: circle\n");

    chunked_write(fd, line, strlen(line));

    // beam cx / cy [px]
    snprintf(line, sizeof(line) - 1, "# region centre (x) [px]: %g\n# region centre (y) [px]: %g\n", cx, cy);
    chunked_write(fd, line, strlen(line));

    if (beam == square)
    {
        // beam width / height [deg]
        snprintf(line, sizeof(line) - 1, "# region width [deg]: %f\n# region height [deg]: %f\n", beam_width, beam_height);
        chunked_write(fd, line, strlen(line));

        // beam width / height [px]
        snprintf(line, sizeof(line) - 1, "# region width [px]: %d\n# region height [px]: %d\n", dimx, dimy);
        chunked_write(fd, line, strlen(line));
    }

    if (beam == circle)
    {
        // beam diameter [deg]
        snprintf(line, sizeof(line) - 1, "# region diameter [deg]: %f\n", beam_width);
        chunked_write(fd, line, strlen(line));

        // beam diameter [px]
        snprintf(line, sizeof(line) - 1, "# region diameter [px]: %d\n", dimx);
        chunked_write(fd, line, strlen(line));
    }

    // specsys
    snprintf(line, sizeof(line) - 1, "# spectral reference frame: %s\n", specsys);
    chunked_write(fd, line, strlen(line));

    // deltaV [km/s]
    snprintf(line, sizeof(line) - 1, "# source velocity [km/s]: %g\n", deltaV / 1e3);
    chunked_write(fd, line, strlen(line));

    // ref_freq [GHz]
    if (ref_freq > 0.0)
    {
        snprintf(line, sizeof(line) - 1, "# reference frequency [GHz]: %g\n", ref_freq / 1e9);
        chunked_write(fd, line, strlen(line));
    }
}

void *fetch_global_statistics(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct mad_req *req = (struct mad_req *)ptr;

    printf("[C] calling fetch_global_statistics across the cluster for '%.*s' with median = %f for the frame range [%d,%d]\n", req->len, req->datasetid, req->dmedian, req->first, req->last);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_global_statistics (no cluster nodes found)\n");

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
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/statistics/%.*s?median=%f&first=%d&last=%d", options.http_port, (int)len, datasetid, req->dmedian, req->first, req->last);
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
                // printf("%lu bytes retrieved\n", (unsigned long)chunks[idx].size);
                // printf("cURL response: %s\n", chunks[idx].memory);

                double val;

                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.sumP", &val))
                    req->sumP += (float)val;

                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.countP", &val))
                    req->countP += (int64_t)val;

                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.sumN", &val))
                    req->sumN += (float)val;

                if (mjson_get_number(chunks[idx].memory, chunks[idx].size, "$.countN", &val))
                    req->countN += (int64_t)val;
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

void *fetch_video_frame(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct video_fetch *req = (struct video_fetch *)ptr;

    printf("[C] calling fetch_video_frame across the cluster for '%.*s', frame %d\n", req->len, req->datasetid, req->frame);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_video_frame (no cluster nodes found)\n");

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
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/video/%.*s", options.http_port, (int)len, datasetid);
        g_string_append_printf(url, "?frame=%d&fill=%d&keyframe=%d&width=%d&height=%d&downsize=%d", req->frame, req->fill, req->keyframe, req->width, req->height, req->downsize);
        g_string_append_printf(url, "&mask=%d", (req->mask != NULL ? 1 : 0));
        g_string_append_printf(url, "&flux=%s&dmin=%f&dmax=%f&dmedian=%f", req->flux, req->dmin, req->dmax, req->dmedian);
        g_string_append_printf(url, "&sensitivity=%f&slope=%f", req->sensitivity, req->slope);
        g_string_append_printf(url, "&white=%f&black=%f", req->white, req->black);
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

            // copy the pixels & mask
            if (response_code == 200)
            {
                size_t plane_size = req->width * req->height;
                size_t expected = (req->mask != NULL) ? 2 * sizeof(uint8_t) * plane_size : sizeof(uint8_t) * plane_size;
                size_t received = chunks[idx].size;

                printf("[C] pixels/mask received: %zu, expected: %zu bytes.\n", received, expected);

                if (received == expected)
                {
                    if (req->pixels != NULL)
                    {
                        const uint8_t *pixels = (uint8_t *)&(chunks[idx].memory[0]);
                        memcpy(req->pixels, pixels, plane_size);
                    }

                    if (req->mask != NULL)
                    {
                        const uint8_t *mask = (uint8_t *)&(chunks[idx].memory[sizeof(uint8_t) * plane_size]);
                        memcpy(req->mask, mask, plane_size);
                    }

                    req->valid = true;
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

    // html-encode the datasetid
    char datasetid[2 * req->len];
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/inner/%.*s", options.ws_port, (int)len, datasetid);
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

void *fetch_pv_diagram(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct http_pv_diagram_request *req = (struct http_pv_diagram_request *)ptr;
    printf("[C] calling fetch_pv_diagram across the cluster for '%.*s'\n", req->len, req->datasetid);

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting fetch_pv_diagram (no cluster nodes found)\n");

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
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/pv/%.*s?x1=%d&y1=%d&x2=%d&y2=%d&first=%d&last=%d&npoints=%d", options.http_port, (int)len, datasetid, req->x1, req->y1, req->x2, req->y2, req->first, req->last, req->npoints);
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

            printf("[C] HTTP transfer #%d completed; cURL status %d, HTTP code %ld.\n", idx + 1, msg->data.result, response_code);

            // reduce (gather) the PV Diagrams
            if (response_code == 200 && chunks[idx].size > 0)
            {
                // printf("[C] fetch_pv received %zu bytes.\n", chunks[idx].size);

                size_t npixels = req->npoints * (req->last - req->first + 1);

                // cross-check the size of the received data with the expected size
                if (npixels == chunks[idx].size / sizeof(float))
                {
                    printf("[C] /pv/<diagram>::OK.\n");

                    const float *restrict pv = (float *)&(chunks[idx].memory[0]);
#pragma GCC ivdep
                    for (size_t i = 0; i < npixels; i++)
                        req->pv[i] += pv[i];

                    req->valid = true;
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
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/viewport/%.*s?x1=%d&y1=%d&x2=%d&y2=%d&frame_start=%f&frame_end=%f&ref_freq=%f&median=%f", options.http_port, (int)len, datasetid, req->x1, req->y1, req->x2, req->y2, req->frame_start, req->frame_end, req->ref_freq, req->median);
        g_string_append_printf(url, "&image=%s", req->image ? "true" : "false");                            // image
        g_string_append_printf(url, "&beam=%s", req->beam == circle ? "circle" : "square");                 // beam
        g_string_append_printf(url, "&intensity=%s", req->intensity == integrated ? "integrated" : "mean"); // intensity
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

                    const float *restrict spectrum = (float *)&(chunks[idx].memory[offset]);

#pragma GCC ivdep
                    for (int i = 0; i < req->length; i++)
                        req->spectrum[i] += spectrum[i];

                    offset += spectrum_size;
                    req->valid = true;
                }

                // are there pixels and a mask?
                if (chunks[idx].size >= offset + pixels_size + mask_size)
                {
                    size_t plane_size = req->dimx * req->dimy;

                    const float *restrict pixels = (float *)&(chunks[idx].memory[offset]);
                    offset += pixels_size;

                    const bool *restrict mask = (bool *)&(chunks[idx].memory[offset]);
                    offset += mask_size;

                    // gather pixels / mask
#pragma GCC ivdep
                    for (size_t i = 0; i < plane_size; i++)
                    {
                        req->pixels[i] += pixels[i];
                        req->mask[i] |= mask[i];
                    }
                }

                // do we have partial statistics too ?
                if (chunks[idx].size == offset + 2 * sizeof(float) + 2 * sizeof(int64_t))
                {
                    printf("[C] /viewport/<statistics>::OK.\n");

                    const float *sumP = (float *)&(chunks[idx].memory[offset]);
                    offset += sizeof(float);

                    const int64_t *countP = (int64_t *)&(chunks[idx].memory[offset]);
                    offset += sizeof(int64_t);

                    const float *sumN = (float *)&(chunks[idx].memory[offset]);
                    offset += sizeof(float);

                    const int64_t *countN = (int64_t *)&(chunks[idx].memory[offset]);
                    offset += sizeof(int64_t);

                    req->sumP += *sumP;
                    req->countP += *countP;
                    req->sumN += *sumN;
                    req->countN += *countN;
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
    size_t len = mg_url_encode(req->datasetid, req->len, datasetid, sizeof(datasetid) - 1);

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

                printf("[C] pixels/mask received: %zu, expected: %zu bytes.\n", received, expected);

                if (received == expected)
                {
                    const float *pixels = (float *)&(chunks[idx].memory[0]);
                    const bool *mask = (bool *)&(chunks[idx].memory[sizeof(float) * plane_size]);

                    // gather pixels / mask
#pragma GCC ivdep
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

void *http_propagate_timeout(void *user)
{
    if (user == NULL)
        pthread_exit(NULL);

    struct timeout_arg *arg = (struct timeout_arg *)user;

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        printf("[C] aborting http_propagate_timeout (no cluster nodes found)\n");

        g_mutex_unlock(&cluster_mtx);

        free(arg->ptr);
        free(arg);
        pthread_exit(NULL);
    };

    char *id = (char *)arg->ptr;
    size_t idlen = strlen(id);

    // html-encode the datasetid
    char datasetid[2 * idlen];
    size_t len = mg_url_encode(id, idlen, datasetid, sizeof(datasetid) - 1);

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
        g_string_append_printf(url, "%" PRIu16 "/timeout/%.*s?error=%d", options.http_port, (int)len, datasetid, arg->error);
        // printf("[C] URL: '%s'\n", url->str);

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

    free(arg->ptr);
    free(arg);

    pthread_exit(NULL);
}

PGconn *jvo_db_connect(char *db)
{
    PGconn *jvo_db = NULL;
    char strConn[1024] = "";

    if (options.password == NULL)
        snprintf(strConn, sizeof(strConn) - 1, "dbname=%s host=%s port=%" PRIu32 " user=%s", db, options.host, options.port, options.user);
    else
        snprintf(strConn, sizeof(strConn) - 1, "dbname=%s host=%s port=%" PRIu32 " user=%s password=%s", db, options.host, options.port, options.user, options.password);

    jvo_db = PQconnectdb(strConn);

    if (PQstatus(jvo_db) != CONNECTION_OK)
    {
        fprintf(stderr, "[C] PostgreSQL connection failed: %s\n", PQerrorMessage(jvo_db));
        PQfinish(jvo_db);
        jvo_db = NULL;
    }
    else
        printf("[C] PostgreSQL connection successful.\n");

    return jvo_db;
}

char *get_jvo_path(PGconn *jvo_db, char *db, char *table, char *data_id)
{
    char path[1024];
    char strSQL[1024];

    int no_rows = 0;
    int no_fields = 0;

    if (jvo_db == NULL || db == NULL || table == NULL || data_id == NULL)
        return NULL;

    memset(path, 0, sizeof(path));
    snprintf(strSQL, sizeof(strSQL) - 1, "SELECT path FROM %s WHERE data_id = '%s';", table, data_id);

    PGresult *res = PQexec(jvo_db, strSQL);

    if (res != NULL && PQresultStatus(res) == PGRES_TUPLES_OK)
    {
        no_rows = PQntuples(res);
        no_fields = PQnfields(res);
    }

    if (no_rows == 1 && no_fields == 1)
    {
        char *pos = strchr(table, '.');

        if (pos == NULL)
        {
            if (strcmp(db, "spcam") == 0 || strcmp(db, "moircs") == 0)
                snprintf(path, sizeof(path) - 1, "%s/subaru/%s/mosaic/", options.db_home, db);
            else
                snprintf(path, sizeof(path) - 1, "%s/%s/", options.db_home, db);
        }
        else
        {
            snprintf(path, sizeof(path) - 1, "%s/%s/", options.db_home, db);

            bool is_fugin = strncmp(table, "fugin", 5) == 0 ? true : false;
            bool is_coming = strncmp(table, "coming", 6) == 0 ? true : false;
            bool is_sfp = strncmp(table, "sfp", 3) == 0 ? true : false;

            // case-convert a part of table and append it to the path
            int i = 0;
            char ch;

            char *dst = path + strlen(path);

            while (table + i != pos)
            {
                // if the table contains {"fugin","coming","sfp"} use upper case else lower case
                ch = is_fugin || is_coming || is_sfp ? (char)toupper(table[i]) : (char)tolower(table[i]);
                memcpy(dst + (i++), &ch, 1);
            }

            ch = '/';
            memcpy(dst + i, &ch, 1);
        }

        if (!PQgetisnull(res, 0, 0))
        {
            const char *value = PQgetvalue(res, 0, 0);

            if (value != NULL)
                strcat(path, value);
        }
    }

    if (res != NULL)
        PQclear(res);

    return strndup(path, sizeof(path) - 1);
}

// read the decompressed data from the compressor output queue
// and pass it to the FITS parser
void *decompress_read(void *user)
{
    char buf[FITS_CHUNK_LENGTH];
    ssize_t n = 0;

    if (user == NULL)
        pthread_exit(NULL);

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;
    printf("[C] R-Decompress/%s::start.\n", stream->datasetid);

    // a blocking read loop from the decompression queue until there is no data left
    while ((n = read(stream->comp_out[0], buf, FITS_CHUNK_LENGTH)) > 0)
    {
        // printf("[C] PIPE_RECV %zd BYTES.\n", n);

        size_t written = fwrite(buf, 1, (size_t)n, stream->fp);
        // printf("[C] FILE_WRITE %zu BYTES.\n", written);

        if (written != (size_t)n)
        {
            perror("[C] FILE_WRITE_ERROR");

            void *item = get_dataset(stream->datasetid);

            if (item != NULL)
                set_error_status_C(item, true);
        }
        else
        {
            /*size_t processed =*/parse2file(buf, 1, (size_t)n, stream);
            // printf("[C] FITS_PARSE %zu BYTES.\n", processed);
        }
    }

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    printf("[C] R-Decompress/%s::end.\n", stream->datasetid);
    pthread_exit(NULL);
}

void *decompress_Z(void *user)
{
    if (user == NULL)
        pthread_exit(NULL);

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;
    printf("[C] Z-Decompress/%s::start.\n", stream->datasetid);

    int ret = decompress(stream->comp_in[0], stream->comp_out[1]);

    if (ret != Z_OK)
    {
        printf("[C] Z-Decompress/%s::error.\n", stream->datasetid);

        close(stream->comp_in[0]);
        stream->comp_in[0] = -1;
    }

    close(stream->comp_out[1]);
    stream->comp_out[1] = -1;

    printf("[C] Z-Decompress/%s::end.\n", stream->datasetid);
    pthread_exit(NULL);
}

void *decompress_GZ(void *user)
{
    if (user == NULL)
        pthread_exit(NULL);

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;
    printf("[C] GZ-Decompress/%s::start.\n", stream->datasetid);

    int ret = inf(stream->comp_in[0], stream->comp_out[1]);

    if (ret != Z_OK)
    {
        printf("[C] GZ-Decompress/%s::error.\n", stream->datasetid);
        zerr(ret);

        close(stream->comp_in[0]);
        stream->comp_in[0] = -1;
    }

    close(stream->comp_out[1]);
    stream->comp_out[1] = -1;

    printf("[C] GZ-Decompress/%s::end.\n", stream->datasetid);
    pthread_exit(NULL);
}

void *decompress_ZIP(void *user)
{
    if (user == NULL)
        pthread_exit(NULL);

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;
    printf("[C] ZIP-Decompress/%s::start.\n", stream->datasetid);

    int ret = unzip(stream->comp_in[0], stream->comp_out[1]);

    if (ret != 0)
    {
        printf("[C] ZIP-Decompress/%s::error.\n", stream->datasetid);

        close(stream->comp_in[0]);
        stream->comp_in[0] = -1;
    }

    close(stream->comp_out[1]);
    stream->comp_out[1] = -1;

    printf("[C] ZIP-Decompress/%s::end.\n", stream->datasetid);
    pthread_exit(NULL);
}

void *decompress_BZIP2(void *user)
{
    if (user == NULL)
        pthread_exit(NULL);

    struct FITSDownloadStream *stream = (struct FITSDownloadStream *)user;
    printf("[C] BZIP2-Decompress/%s::start.\n", stream->datasetid);

    int ret = bunzip2(stream->comp_in[0], stream->comp_out[1]);

    if (ret != BZ_OK)
    {
        printf("[C] BZIP2-Decompress/%s::error.\n", stream->datasetid);

        close(stream->comp_in[0]);
        stream->comp_in[0] = -1;
    }

    close(stream->comp_out[1]);
    stream->comp_out[1] = -1;

    printf("[C] BZIP2-Decompress/%s::end.\n", stream->datasetid);
    pthread_exit(NULL);
}