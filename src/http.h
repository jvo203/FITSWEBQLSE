
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

#define ZFP_HIGH_PRECISION 16
#define ZFP_MEDIUM_PRECISION 11
#define ZFP_LOW_PRECISION 8

#define FPZIP_MEDIUM_PRECISION 16
#define FPZIP_HIGH_PRECISION 24

#include "fits_types.h"

typedef struct
{
    // fitswebql
    uint16_t http_port;
    uint16_t ws_port;
    bool local;
    uint32_t timeout;
    char *fits_home;
    char *cache;
    uint32_t threshold;
    char *logs;
    char *home_dir;

    // postgresql
    char *user;
    char *password;
    char *host;
    uint32_t port;
    char *db_home;

    // download from URL
    char *url_protocol;
    char *url_host;
    uint32_t url_port;

    // cluster
    char *root;
    uint32_t zmq_port; // ZeroMQ beacon port
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
    char *datasetid;
    char *url;
} url_req_t;

struct MemoryStruct
{
    char *memory;
    size_t size;
};

struct arg_struct
{
    void *item;
    int width;
    int height;
    int precision;
    int fetch_data;
    int fd;
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

struct gzip_req
{
    // Unix pipe ends
    int fd_in;  // reading
    int fd_out; // writing
};

struct mad_req
{
    char *datasetid;
    int len;
    float dmedian;
    float sumP;
    int64_t countP;
    float sumN;
    int64_t countN;
    int first, last;
};

struct inner_dims_req
{
    char *datasetid;
    int len;
    int width;
    int height;
};

struct image_req
{
    char *datasetid;
    int len;
    float *restrict pixels;
    bool *restrict mask;
    int width;
    int height;
};

struct video_fetch
{
    char *datasetid;
    int len;
    bool keyframe;
    int frame;
    int fill;

    char *flux;
    float dmin, dmax, dmedian;
    float sensitivity, slope;
    float white, black;

    int width;
    int height;
    bool downsize;

    int8_t *restrict pixels;
    int8_t *restrict mask;
    bool valid;
};

struct video_req
{
    bool keyframe;
    int frame;
    int fill;

    char *flux;
    int len;
    float dmin, dmax, dmedian;
    float sensitivity, slope;
    float white, black;

    int width;
    int height;
    bool downsize;
    bool mask;

    int fd;
    void *ptr;
};

struct http_pv_diagram_request
{
    char *datasetid;
    int len;

    // inputs
    // input
    int x1;
    int y1;
    int x2;
    int y2;
    int first;
    int last;

    // output
    float *pv;
    int npoints;
    bool valid;
};

struct http_image_spectrum_request
{
    char *datasetid;
    int len;

    // input
    bool image;
    int x1;
    int y1;
    int x2;
    int y2;
    enum zoom_shape beam;
    enum intensity_mode intensity;
    double frame_start;
    double frame_end;
    double ref_freq;
    float median;

    // output
    float *restrict pixels;
    bool *restrict mask;
    float *restrict spectrum;

    int dimx;
    int dimy;
    int length;

    float sumP;
    float sumN;
    int64_t countP;
    int64_t countN;

    bool valid;
};

void start_http();
void stop_http();
void *http_propagate_timeout(void *user);