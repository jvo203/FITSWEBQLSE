
#pragma once

#include <inttypes.h>
#include <stdbool.h>

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

typedef struct
{
    int *start;
    int *end;
    int *status;
} fits_range_t;

void start_http();
void stop_http();