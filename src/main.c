/* main.c */
/* FITSWEBQLSE by Christopher Zapart @ Japanese Virtual Observatory (JVO) in NAOJ */
/* chris.zapart@nao.ac.jp */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <getopt.h>
#include <libgen.h>
#include <string.h>
#include <stdbool.h>
#include <dirent.h>
#include <pwd.h>

#include "ini.h"
#include "http.h"

#include <pthread.h>
#define closesocket(x) close(x)

#include "mongoose.h"
#include "mjson.h"

static volatile sig_atomic_t s_received_signal = 0;

static void signal_handler(int sig_num)
{
    printf("Interrupt signal [%d] received.\n", sig_num);

    signal(sig_num, signal_handler);
    s_received_signal = sig_num;
}

#define VERSION_MAJOR 5
#define VERSION_MINOR 0
#define VERSION_SUB 0

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define SERVER_STRING                                                  \
    "FITSWEBQLSE v" STR(VERSION_MAJOR) "." STR(VERSION_MINOR) "." STR( \
        VERSION_SUB)

#define WASM_VERSION "21.09.XX.X"
#define VERSION_STRING "SV2022-01-XX.X-ALPHA"

typedef struct
{
    // fitswebql
    uint32_t http_port;
    uint32_t ws_port;
    bool local;
    bool production;
    uint32_t timeout;
    char *fits_home;
    char *cache;
    char *logs;
    char *home_dir;

    //postgresql
    char *user;
    char *password;
    char *host;
    uint32_t port;
    char *db_home;
} options_t;

#define OPTSTR "p:h"
#define USAGE_FMT "%s [-p HTTP port] [-d home directory] [-h]\n"
#define DEFAULT_PROGNAME "fitswebql"

void usage(char *progname, int opt);

static int handler(void *user, const char *section, const char *name,
                   const char *value);

// HTTP request callback
static void mg_request_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data);

// Pipe event handler
static void mg_pipe_event_handler(struct mg_connection *c, int ev, void *ev_data, void *fn_data);

int main(int argc, char *argv[])
{
    printf("%s %s\n", SERVER_STRING, VERSION_STRING);

    struct passwd *passwdEnt = getpwuid(getuid());

    options_t options = {8080, 8081, true, false, 15, strdup(".cache"), strdup(".cache"), strdup("LOGS"), strdup(passwdEnt->pw_dir), strdup("jvo"), NULL, strdup("p10.vo.nao.ac.jp"), 5433, strdup("/home")}; // default values

    // parse a config.ini config file
    if (ini_parse("config.ini", handler, &options) < 0)
        printf("Can't load 'config.ini', assuming default options.\n");
    else
        printf("Successfully parsed 'config.ini'.\n");

    // parse options command-line options (over-rides the .ini config file)
    int opt;

    while ((opt = getopt(argc, argv, OPTSTR)) != EOF)
        switch (opt)
        {
        case 'p':
            options.http_port = (uint32_t)strtoul(optarg, NULL, 10);
            options.ws_port = options.http_port + 1;
            break;

        case 'd':
            if (options.home_dir != NULL)
                free(options.home_dir);

            options.home_dir = strdup(optarg);
            break;

        case 'h':
        default:
            usage(basename(argv[0]), opt);
            /* NOTREACHED */
            break;
        }

    if (options.local)
        printf("Home Directory: %s\n", options.home_dir);

    printf("Browser URL: http://localhost:%" PRIu32 "\n", options.http_port);
    printf("*** To quit FITSWEBQLSE press Ctrl-C from the command-line terminal or send SIGINT. ***\n");

    // Ctrl-C signal handler
    // ignore SIGPIPE
    signal(SIGTERM, signal_handler);
    signal(SIGINT, signal_handler);

    start_http();

    // a mongoose server
    char url[256] = "";
    sprintf(url, "0.0.0.0:%d", options.ws_port);

    struct mg_mgr mgr;
    struct mg_connection *pipe; // Used to wake up event manager
    mg_mgr_init(&mgr);
    mg_log_set("3");
    pipe = mg_mkpipe(&mgr, mg_pipe_event_handler, NULL);  // Create pipe
    mg_http_listen(&mgr, url, mg_request_callback, pipe); // Create listener

    // a mongoose event loop
    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Event loop

    mg_mgr_free(&mgr); // Cleanup

    stop_http();

    // release any memory allocated in options (really not needed at this point but ...)
    if (options.fits_home != NULL)
        free(options.fits_home);

    if (options.cache != NULL)
        free(options.cache);

    if (options.logs != NULL)
        free(options.logs);

    if (options.home_dir != NULL)
        free(options.home_dir);

    if (options.user != NULL)
        free(options.user);

    if (options.password != NULL)
        free(options.password);

    if (options.host != NULL)
        free(options.host);

    if (options.db_home != NULL)
        free(options.db_home);

    return EXIT_SUCCESS;
}

void usage(char *progname, int opt)
{
    fprintf(stderr, USAGE_FMT, progname ? progname : DEFAULT_PROGNAME);
    exit(EXIT_FAILURE);
    /* NOTREACHED */
}

static int handler(void *user, const char *section, const char *name,
                   const char *value)
{
    options_t *options = (options_t *)user;

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0

    if (MATCH("fitswebql", "local"))
    {
        if (MATCH(value, "true"))
            options->local = true;

        if (MATCH(value, "false"))
            options->local = false;
    }
    else if (MATCH("fitswebql", "production"))
    {
        if (MATCH(value, "true"))
            options->production = true;

        if (MATCH(value, "false"))
            options->production = false;
    }
    else if (MATCH("fitswebql", "timeout"))
    {
        options->timeout = atoi(value);
    }
    else if (MATCH("fitswebql", "home"))
    {
        if (options->fits_home != NULL)
            free(options->fits_home);

        options->fits_home = strdup(value);
    }
    else if (MATCH("fitswebql", "logs"))
    {
        if (options->logs != NULL)
            free(options->logs);

        options->logs = strdup(value);
    }
    else if (MATCH("fitswebql", "cache"))
    {
        if (options->cache != NULL)
            free(options->cache);

        options->cache = strdup(value);
    }
    else if (MATCH("fitswebql", "port"))
    {
        options->http_port = atoi(value);
        options->ws_port = options->http_port + 1;
    }
    else if (MATCH("postgresql", "host"))
    {
        if (options->host != NULL)
            free(options->host);

        options->host = strdup(value);
    }
    else if (MATCH("postgresql", "user"))
    {
        if (options->user != NULL)
            free(options->user);

        options->user = strdup(value);
    }
    else if (MATCH("postgresql", "password"))
    {
        if (options->password != NULL)
            free(options->password);

        options->password = strdup(value);
    }
    else if (MATCH("postgresql", "home"))
    {
        if (options->db_home != NULL)
            free(options->db_home);

        options->db_home = strdup(value);
    }
    else if (MATCH("postgresql", "port"))
    {
        options->port = atoi(value);
    }
    else
    {
        printf("unknown option %s/%s\n", section, name);
        return 0; /* unknown section/name, error */
    }
    return 1;
}

static void start_thread(void (*f)(void *), void *p)
{
    pthread_t thread_id = (pthread_t)0;
    pthread_attr_t attr;
    (void)pthread_attr_init(&attr);
    (void)pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_create(&thread_id, &attr, (void *(*)(void *))f, p);
    pthread_attr_destroy(&attr);
}

static void thread_function(void *param)
{
    struct mg_connection *c = param; // Pipe connection
    sleep(2);                        // Simulate long execution
    mg_mgr_wakeup(c);                // Wakeup event manager
}

// HTTP request callback
static void mg_request_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_HTTP_MSG)
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        if (mg_http_match_uri(hm, "/fast"))
        {
            // Single-threaded code path, for performance comparison
            // The /fast URI responds immediately
            mg_http_reply(c, 200, "Host: foo.com\r\n", "hi\n");
        }
        else
        {
            // Multithreading code path
            c->label[0] = 'W';                      // Mark us as waiting for data
            start_thread(thread_function, fn_data); // Start handling thread
        }
    }
}

// Pipe event handler
static void mg_pipe_event_handler(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_READ)
    {
        struct mg_connection *t;
        for (t = c->mgr->conns; t != NULL; t = t->next)
        {
            if (t->label[0] != 'W')
                continue;                                       // Ignore un-marked connections
            mg_http_reply(t, 200, "Host: foo.com\r\n", "hi\n"); // Respond!
            t->label[0] = 0;                                    // Clear mark
        }
    }
}