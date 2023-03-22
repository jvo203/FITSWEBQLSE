#pragma once

#include <stdio.h>

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
    int comp_in[2];
    int comp_out[2];
    pthread_t tid;

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

struct image_spectrum_request
{
    // input
    int dx;
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
    int seq_id;
    float timestamp;

    // output (the 'write' end of a Unix pipe)
    int fd;

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
    // input
    bool keyframe; // is it a keyframe?
    int frame;

    // needed by tone mapping
    char *flux;
    int len;
    float dmin, dmax, dmedian;
    float dmadN, dmadP;

    // output (the 'write' end of a Unix pipe)
    int width;
    int height;
    bool downsize;
    int fd;

    void *ptr;
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

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
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