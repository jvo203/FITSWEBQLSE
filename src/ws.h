#pragma once

#include "mongoose.h"
#include "fits_types.h"
#include <pthread.h>
#include <glib.h>
#include <x265.h>

#include "ring_buffer.h"

struct websocket_session
{
    char *datasetid; // a single id
    char *multi;     // multiple ids are separated by a semicolon
    char *id;

    char *flux;
    float dmin, dmax, dmedian;
    float dmadN, dmadP;
    pthread_mutex_t stat_mtx;

    int image_width;
    int image_height;
    bool bDownsize;

    // video
    pthread_mutex_t vid_mtx;
    int last_frame_idx;

    // x265
    x265_param *param;
    x265_encoder *encoder;
    x265_picture *picture;

    // PV-Diagram
    pthread_mutex_t pv_mtx;
    volatile sig_atomic_t pv_exit;

    pthread_cond_t pv_cond;
    pthread_mutex_t cond_mtx;

    pthread_t pv_thread;
    struct ring_buffer *pv_ring;
};

struct websocket_response
{
    char *session_id;
    int seq_id;
    int fps;
    int bitrate;
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

void init_session_table();
void delete_session_table();

void start_ws();
extern void submit_channel_range(void *ptr, int idx, int progress, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
extern void *realtime_image_spectrum_request(void *req);
extern void *realtime_image_spectrum_request_simd(void *req);
extern void *spectrum_request_simd(void *req);
extern void *ws_image_spectrum_request(void *req);
extern void *ws_pv_request(void *req);
extern void *video_request_simd(void *req);
void *spectrum_response(void *ptr);
void *realtime_image_spectrum_response(void *ptr);
void *ws_image_spectrum_response(void *ptr);
void *ws_pv_response(void *ptr);
void *video_response(void *ptr);
extern void get_inner_dimensions(void *ptr, int width, int height, int *fits_width, int *fits_height, int *inner_width, int *inner_height, float *scale);
extern void get_spectrum_range_C(void *ptr, double frame_start, double frame_end, double ref_freq, int *first, int *last);
extern void fill_global_statistics(void *ptr, float *dmin, float *dmax, float *dmedian, float *dmadN, float *dmadP);
extern void update_timestamp(void *ptr);