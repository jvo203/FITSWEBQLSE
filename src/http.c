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

#include "json.h"
#include "http.h"

#include "cluster.h"
#include "hash_table.h"

#include "version.h"

#include <sqlite3.h>
static sqlite3 *splat_db = NULL;
extern options_t options; // <options> is defined in main.c

// HTML
#define PAGE "<html><head><title>FITSWEBQL SE</title>" \
             "</head><body>FITSWEBQLSE (libmicrohttpd)</body></html>"

struct MHD_Daemon *http_server = NULL;

static enum MHD_Result execute_alma(struct MHD_Connection *connection, char **va_list, int va_count, int composite, char *root);

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

        //if(difftime(mktime(&lm), mktime(&tm)) <= 0)
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

        //detect mime-types
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

        MHD_add_response_header(response, "Cache-Control", "public, max-age=86400"); //86400
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

        //overwrite the the last ',' with a list closing character
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

static enum MHD_Result get_home_directory(struct MHD_Connection *connection)
{
    return get_directory(connection, strdup(options.home_dir));
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
    //if (0 != strcmp(method, "GET"))
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
    //MHD_get_connection_values(connection, MHD_HEADER_KIND, (MHD_KeyValueIterator)&print_out_key, NULL);

    if (0 == strcmp(url, "/exit"))
    {
        // forward the exit events to all other nodes
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

    // WebQL main entry page
    if (strstr(url, "FITSWebQL.html") != NULL)
    {
        char *root = NULL;

        if (!options.local)
        {
            //get the root path
            char *proot = (char *)strstr(url, "FITSWebQL.html");

            int len = proot - url;
            char *root = strndup(url, len);

            printf("[C] URL root path: %s\n", root);
        }

        //get datasetId
        char **datasetId = NULL;
        int va_count = 0;

        char *directory = NULL; // only needed by <options.local>
        char *extension = NULL; // only needed by <options.local>

        if (options.local)
        {
            directory = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "dir");
            extension = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "ext");
            char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "filename");

            //auto-detect multiple entries
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

                //allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = tmp;
            }
        }
        else
        {
            char *tmp = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "datasetId");

            //auto-detect multiple lines
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

                //allocate datasetId
                datasetId = (char **)malloc(sizeof(char *));
                datasetId[0] = tmp;
            }
        }

        char *view = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "view");
        char *flux = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "flux");
        char *db = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "db");
        char *table = (char *)MHD_lookup_connection_value(connection, MHD_GET_ARGUMENT_KIND, "table");

        int composite = 0;

        if (view != NULL)
            composite = (strcasecmp("composite", view) == 0) ? 1 : 0;

        if (datasetId != NULL)
        {
            ret = execute_alma(connection, datasetId, va_count, composite, root);

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

                    // FORTRAN code, not implemented yet (TO-DO)
                    /* pthread_create(&tid, NULL, &handle_fitswebql_request, strndup(filepath, sizeof(filepath)));
                    pthread_detach(tid);*/
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
        if (datasetId != NULL)
            free(datasetId);

        if (root != NULL)
            free(root);

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
    signal(SIGPIPE, SIG_IGN); //ignore SIGPIPE
    //signal(SIGINT, SIGINTHandler); //intercept CTRL+C to trigger a clean shutdown

    //http_server = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION | MHD_USE_INTERNAL_POLLING_THREAD | MHD_USE_ERROR_LOG | MHD_USE_ITC | MHD_USE_TURBO,
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
            fprintf(stderr, "Can't open local splatalogue database: %s\n", sqlite3_errmsg(splat_db));
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