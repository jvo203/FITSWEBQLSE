#include "../facil/include/http.h"

#include "facil_http.h"
#include "http.h"

#include "version.h"

#include <sqlite3.h>
static sqlite3 *splat_db = NULL;
extern options_t options; // <options> is defined in main.c

// These will contain pre-allocated values that we will use often
FIOBJ HTTP_X_DATA;

void start_facil()
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

    // listen on port 8080 and any available network binding (NULL == 0.0.0.0)
    http_listen("8080", NULL, .on_request = on_request, .log = 1);

    // start the server
    facil_run(.threads = 1, .port = options.http_port);

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