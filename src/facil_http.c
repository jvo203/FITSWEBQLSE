/* include the core library, without any extensions */
#include <fio.h>
#include "../facil/include/http.h"

#include "facil_http.h"
#include "http.h"

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
    int fd = -1;
    struct stat buf;
    char path[1024];
    char last_modified[255];
    char last_etag[255];

    /* check for NULL strings */
    if (NULL == url)
        return http_not_found(h);

    printf("[C] http_serve_file(%s)\n", url);

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

    http_not_implemented(h);
}

// the main HTTP connection handler (called for each request)
void on_request(http_s *h)
{
    // get the request's URL
    struct fio_str_info_s path = fiobj_obj2cstr(h->path);
    const char *url = path.data;

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