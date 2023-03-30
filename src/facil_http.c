/* include the core library, without any extensions */
#include <fio.h>
#include "../facil/include/http.h"

#include "facil_http.h"
#include "http.h"

#include "json.h"
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

// These will contain pre-allocated values that we will use often
FIOBJ HTTP_X_DATA;

void on_request(http_s *request);

void start_facil()
{
    signal(SIGPIPE, SIG_IGN); // ignore SIGPIPE

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

    char port_s[32];

    // convert options.http_port to a string
    sprintf(port_s, "%" PRIu16 "", options.http_port);

#ifdef DEBUG
    printf("[C] Starting facil.io HTTP daemon listening on port %s... Press CTRL-C to stop it.\n", port_s);
#endif

    // listen on port <options.http_port> and any available network binding (NULL == 0.0.0.0)
    if (http_listen(port_s, NULL, .on_request = on_request, .log = 1) == -1)
    {
        perror("[C] ERROR: facil.io couldn't initialize HTTP service (already running?)");
        exit(1);
    }

    // start the server
    fio_start(.workers = -4, .threads = -2);

    if (splat_db != NULL)
    {
        sqlite3_close(splat_db);
        splat_db = NULL;
    }
}

void http_ok(http_s *h)
{
    h->status = 200;
    http_finish(h);
}

void http_not_found(http_s *h)
{
    http_send_error(h, 404);
}

void http_internal_server_error(http_s *h)
{
    http_send_error(h, 500);
}

void http_not_implemented(http_s *h)
{
    http_send_error(h, 501);
}

void http_serve_file(http_s *h, const char *url)
{
    char path[1024];

    /* check for NULL strings */
    if (NULL == url)
        return http_not_found(h);

    printf("[C] http_serve_file(%s)\n", url);

    if (NULL != strstr(url, "../")) /* Very simplified check! */
        return http_not_found(h);   /* Do not allow usage of parent directories. */

#ifdef SHARE
    snprintf(path, sizeof(path), SHARE "/htdocs%s", url);
#else
    snprintf(path, sizeof(path), "htdocs%s", url);
#endif

    int ret = http_sendfile2(h, path, strlen(path), NULL, 0);

    if (ret == -1)
        return http_internal_server_error(h);
    else
        return;
}

const char *get_filename_ext2(const char *filename)
{
    const char *dot = strrchr(filename, '.');

    if (!dot || dot == filename)
        return "";

    return dot + 1;
}

void get_directory(http_s *h, char *dir)
{
    if (NULL == dir)
        return http_not_found(h);

    printf("[C] get_directory(%s)\n", dir);

    GString *json = g_string_sized_new(1024);

    if (NULL == json)
    {
        free(dir);
        return http_internal_server_error(h);
    }

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
                    if (!strcasecmp(get_filename_ext2(namelist[i]->d_name), "fits"))
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

    g_free(json);

    return http_not_implemented(h);
}

void get_home_directory(http_s *h)
{
    return get_directory(h, strdup(options.home_dir));
}

// the main HTTP connection handler (called for each request)
void on_request(http_s *h)
{
    // get the request's URL
    struct fio_str_info_s path = fiobj_obj2cstr(h->path);
    const char *url = path.data;

    struct fio_str_info_s query = fiobj_obj2cstr(h->query);
    const char *query_s = query.data;

    if (h->query != FIOBJ_INVALID)
        printf("[C] on_request(%s?%s)\n", url, query_s);

    if (0 == strcmp(url, "/exit"))
    {
        // distributed exit
        distributed_exit();

        // raise SIGINT
        int ret = raise(SIGINT);

        if (ret != 0)
        {
            printf("[C] Error: unable to raise SIGINT signal.\n");
            return http_not_found(h);
        }
        else
            return http_ok(h);
    }

    if (0 == strcmp(url, "/get_directory"))
    {
        http_parse_body(h);
        http_parse_query(h);

        FIOBJ params = h->params;
        FIOBJ value = FIOBJ_INVALID;

        if (FIOBJ_TYPE_IS(params, FIOBJ_T_HASH))
        {
            uint64_t dir_hash = fiobj_hash_string("dir", 3);
            value = fiobj_hash_get2(params, dir_hash);
        }

        if (value != FIOBJ_INVALID)
        {
            struct fio_str_info_s dir = fiobj_obj2cstr(value);
            const char *dir_s = query.data;
            return get_directory(h, strdup(dir_s));
        }
        else
            return get_home_directory(h);

        return http_not_implemented(h);
    }

    // static resources
    if (url[strlen(url) - 1] != '/')
        return http_serve_file(h, url);
    else
    {
        // root document
        if (options.local)
            return http_serve_file(h, "/local.html");
        else
            return http_serve_file(h, "/test.html");
    }

    http_not_found(h);
}