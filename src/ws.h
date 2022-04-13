#pragma once

#include "mongoose.h"
#include "fits_types.h"
#include <pthread.h>

struct websocket_session
{
    char *datasetid;
    char *flux;

    int image_width;
    int image_height;
    bool bDownsize;

    // x265 (TO-DO)
    pthread_mutex_t vid_mtx;
    // encoder
    // picture
    // param
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
extern void get_inner_dimensions(void *ptr, int width, int height, int *fits_width, int *fits_height, int *inner_width, int *inner_height, float *scale);