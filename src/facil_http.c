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

// Easy HTTP handling
void on_request(http_s *request)
{
    http_send_body(request, "Hello World!\r\n", 14);
}