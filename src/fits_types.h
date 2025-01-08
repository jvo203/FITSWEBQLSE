#pragma once

#include <stdio.h>
#include <stdatomic.h>

#define FITS_CHUNK_LENGTH 2880
#define FITS_LINE_LENGTH 80

// FITS compression file types enum
enum fits_compression
{
    fits_compression_none,
    fits_compression_compress,
    fits_compression_zip,
    fits_compression_gzip,
    fits_compression_bzip2,
    fits_compression_unknown
};

struct FITSDownloadStream
{
    FILE *fp;
    char *datasetid;
    char *url;
    char *fname;
    char *buffer;
    size_t buffer_size;
    size_t running_size;
    size_t cursor;
    size_t total_size;

    enum fits_compression compression;
    atomic_int comp_in[2];
    atomic_int comp_out[2];
    pthread_t tid;
    volatile bool tid_created;

    // a bare-bones FITS header
    bool hdrEnd;
    int bitpix;
    int naxis;
    int naxes[4];

    // data
    float *data;
    float *pixels;
    bool *mask;
    size_t pixels_per_frame;
    size_t processed;
    int frame;
};

enum video_request_type
{
    single,
    composite
};

enum zoom_shape
{
    circle,
    square
};

enum intensity_mode
{
    mean,
    integrated
};

enum image_quality
{
    low,
    medium,
    high
};

struct download_request
{
    // input
    int x1;
    int y1;
    int x2;
    int y2;
    double frame_start;
    double frame_end;
    double ref_freq;

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
};

struct composite_download_request
{
    char **datasetId; // dataset ids
    int va_count;     // number of datasets
    struct download_request *req;
};

struct image_spectrum_request
{
    // input
    int dx;
    int dy;
    bool image;
    enum image_quality quality;
    int x1;
    int y1;
    int x2;
    int y2;
    int width;
    int height;
    enum zoom_shape beam;
    enum intensity_mode intensity;
    double frame_start;
    double frame_end;
    double ref_freq;
    float median;
    int x;
    int y;
    bool tracking;
    int seq_id;
    float timestamp;

    // output (WebSocket session pointer)
    void *session;

    // legacy output (the 'write' end of a Unix pipe)
    int fd;

    // dataset pointer
    void *ptr;
};

struct spectrum_request
{
    // input
    char *ra;
    char *dec;
    int x1;
    int y1;
    int x2;
    int y2;
    enum zoom_shape beam;
    enum intensity_mode intensity;
    double frame_start;
    double frame_end;
    double ref_freq;
    double deltaV;
    bool rest;
    int seq_id;
    float timestamp;

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
};

struct video_request
{
    // common variables for both single and composite
    enum video_request_type video_type;
    char *flux;
    int len;
    int seq_id;
    float timestamp;

    // output (WebSocket session pointer)
    void *session;

    // input
    bool keyframe; // is it a keyframe?
    int frame;
    int fill;

    float dmin, dmax, dmedian;
    float dmadN, dmadP;

    // output
    int width;
    int height;
    bool downsize;

    void *ptr; // item
};

struct composite_video_request
{
    // common variables for both single and composite
    enum video_request_type video_type;
    // the flux is common across the RGB channels
    char *flux;
    int len;
    int seq_id;
    float timestamp;

    // output (WebSocket session pointer)
    void *session;

    // input
    bool keyframe;
    int fill;

    // RGB channels (up to three)
    int va_count;
    void *ptr[3]; // item
    int frame[3];
    float dmin[3], dmax[3], dmedian[3];
    float dmadN[3], dmadP[3];

    // output
    int width;
    int height;
    bool downsize;
};

struct pv_request
{
    // input
    int x1;
    int y1;
    int x2;
    int y2;
    int width;
    int height;
    double frame_start;
    double frame_end;
    double ref_freq;
    double deltaV;
    bool rest;
    int seq_id;
    float timestamp;

    // RGB channels (up to three)
    int va_count;
    void *ptr[3]; // item

    // output (WebSocket session pointer)
    void *session;
};

struct cluster_pv_request
{
    // input
    int x1;
    int y1;
    int x2;
    int y2;
    int first;
    int last;
    int npoints;

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
};

// a custom mg_str structure
struct data_buf
{
    char *buf;
    size_t len;
};

// mapped from FORTRAN
struct image_tone_mapping_type
{
    char flux[16];
    float pmin, pmax, pmedian;
    float sensitivity, ratio_sensitivity;
    float white, black;
};