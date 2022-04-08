#pragma once

#include "mongoose.h"
#include "fits_types.h"

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
    int seq_id;
    float timestamp;

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
};

struct image_spectrum_response
{
    char *session_id;
    int seq_id;
    float timestamp;

    // input (the 'read' end of a Unix pipe)
    int fd;
};

struct websocket_message
{
    char *session_id;
    char *buf;
    size_t len;
};

void start_ws();
extern void submit_channel_range(void *ptr, int idx, int progress, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
extern void *realtime_image_spectrum_request(void *req);
extern void *realtime_image_spectrum_request_simd(void *req);
void *realtime_image_spectrum_response(void *ptr);