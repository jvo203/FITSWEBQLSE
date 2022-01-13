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

int main(int argc, char *argv[])
{
    struct passwd *passwdEnt = getpwuid(getuid());

    options_t options = {8080, 8081, true, false, 15, ".cache", ".cache", "LOGS", passwdEnt->pw_dir, "jvo", NULL, "p10.vo.nao.ac.jp", 5433, "/home"}; // default values

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
            options.home_dir = strdup(optarg);
            break;

        case 'h':
        default:
            usage(basename(argv[0]), opt);
            /* NOTREACHED */
            break;
        }

    printf("%s %s\n", SERVER_STRING, VERSION_STRING);

    if (options.local)
        printf("Home Directory: %s\n", options.home_dir);

    printf("Browser URL: http://localhost:%" PRIu32 "\n", options.http_port);
    printf("*** To quit FITSWEBQLSE press Ctrl-C from the command-line terminal or send SIGINT. ***\n");

    // Ctrl-C signal handler
    // ignore SIGPIPE

    start_http();

    // a mongoose server

    // a mongoose event loop

    stop_http();

    // release any memory allocated in options (really not needed at this point) ...

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
        options->fits_home = strdup(value);
    }
    else if (MATCH("fitswebql", "logs"))
    {
        options->logs = strdup(value);
    }
    else if (MATCH("fitswebql", "cache"))
    {
        options->cache = strdup(value);
    }
    else if (MATCH("fitswebql", "port"))
    {
        options->http_port = atoi(value);
        options->ws_port = options->http_port + 1;
    }
    else if (MATCH("postgresql", "host"))
    {
        options->host = strdup(value);
    }
    else if (MATCH("postgresql", "user"))
    {
        options->user = strdup(value);
    }
    else if (MATCH("postgresql", "password"))
    {
        options->password = strdup(value);
    }
    else if (MATCH("postgresql", "home"))
    {
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