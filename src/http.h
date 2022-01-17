
#pragma once

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

    //postgresql
    char *user;
    char *password;
    char *host;
    uint32_t port;
    char *db_home;
} options_t;

static options_t options;

void start_http();
void stop_http();