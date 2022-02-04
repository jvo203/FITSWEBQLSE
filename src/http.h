
#pragma once

#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

#include <zlib.h>

/* CHUNK is the size of the memory chunk used by the zlib & other routines. */

#define CHUNK 0x4000
#define _windowBits 15
#define GZIP_ENCODING 16

/* The following macro calls a zlib routine and checks the return
   value. If the return value ("status") is not OK, it prints an error
   message and exits the program. Zlib's error statuses are all less
   than zero. */

#define CALL_ZLIB(x)                                                              \
    {                                                                             \
        int status;                                                               \
        status = x;                                                               \
        if (status < 0)                                                           \
        {                                                                         \
            fprintf(stderr, "%s:%d: %s returned a bad status of %d.\n", __FILE__, \
                    __LINE__, #x, status);                                        \
            /*exit(EXIT_FAILURE);*/                                               \
        }                                                                         \
    }

typedef struct
{
    // fitswebql
    uint16_t http_port;
    uint16_t ws_port;
    bool local;
    bool production;
    uint32_t timeout;
    char *fits_home;
    char *cache;
    char *logs;
    char *home_dir;

    // postgresql
    char *user;
    char *password;
    char *host;
    uint32_t port;
    char *db_home;

    // cluster
    char *root;
} options_t;

typedef struct
{
    char *datasetid;
    char *filepath;
    char *flux;
    char *root;
} fits_req_t;

struct MemoryStruct
{
    char *memory;
    size_t size;
};

struct splat_req
{
    bool first;
    bool compression;
    double freq_start;
    double freq_end;
    int fd;

    // optional gzip compression
    z_stream z;
    unsigned char out[CHUNK];
};

void start_http();
void stop_http();