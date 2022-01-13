/* main.c */
/* FITSWEBQLSE by Christopher Zapart @ Japanese Virtual Observatory (NAOJ) */
/* chris.zapart@nao.ac.jp */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <getopt.h>

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

int main(int argc, char *argv[])
{
    printf("%s %s\n", SERVER_STRING, VERSION_STRING);
    printf("*** To quit FITSWebQL press Ctrl-C from the command-line terminal or send SIGINT. ***\n");
}