/* main.c */
/* FITSWEBQLSE by Christopher Zapart @ Japanese Virtual Observatory (NAOJ) */
/* chris.zapart@nao.ac.jp */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <getopt.h>
#include <libgen.h>

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
    uint32_t http_port;
    uint32_t ws_port;
} options_t;

#define OPTSTR "p:h"
#define USAGE_FMT "%s [-p HTTP port] [-h]\n"
#define DEFAULT_PROGNAME "fitswebql"

void usage(char *progname, int opt);

int main(int argc, char *argv[])
{
    // parse a .ini config file

    // parse options command-line options (over-rides the .ini config file)
    int opt;
    options_t options = {8080, 8081}; // default values

    while ((opt = getopt(argc, argv, OPTSTR)) != EOF)
        switch (opt)
        {
        case 'p':
            options.http_port = (uint32_t)strtoul(optarg, NULL, 10);
            options.ws_port = options.http_port + 1;
            break;

        case 'h':
        default:
            usage(basename(argv[0]), opt);
            /* NOTREACHED */
            break;
        }

    printf("%s %s\n", SERVER_STRING, VERSION_STRING);
    printf("Browser URL: http://localhost:%" PRIu32 "\n", options.http_port);
    printf("*** To quit FITSWebQL press Ctrl-C from the command-line terminal or send SIGINT. ***\n");
}

void usage(char *progname, int opt)
{
    fprintf(stderr, USAGE_FMT, progname ? progname : DEFAULT_PROGNAME);
    exit(EXIT_FAILURE);
    /* NOTREACHED */
}