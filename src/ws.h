#pragma once

#include <glib.h>

#include "mongoose.h"
#include <microhttpd.h>
#include "fits_types.h"
#include "mongoose.h"

#include <pthread.h>
#include <glib.h>
#include <x265.h>

#include "ring_buffer.h"

typedef struct
{
    char *datasetid; // a single id
    char *multi;     // multiple ids are separated by a semicolon
    char *id;        // sessionId

#ifdef MICROWS
    /* the TCP/IP socket for reading/writing */
    MHD_socket fd;

    /* the UpgradeResponseHandle of libmicrohttpd (needed for closing the socket) */
    struct MHD_UpgradeResponseHandle *urh;

    /* the websocket encode/decode stream */
    struct MHD_WebSocketStream *ws;

    /* the possibly read data at the start (only used once) */
    char *extra_in;
    size_t extra_in_size;

    /* a mongoose Single-Producer Single-Consumer queue, writes to be protected by a spinlock or a mutex */
    struct mg_queue queue;
    pthread_mutex_t queue_mtx; // appending to queue one thread at a time
    char *buf;                 // Buffer for messages
    size_t buf_len;            // Buffer length

    /* specifies whether the websocket shall be closed (1) or not (0) */
    volatile sig_atomic_t disconnect;

    /* condition variable to wake up the sender of this connection */
    pthread_cond_t wake_up_sender;
    pthread_mutex_t wake_up_cond_mtx;

    /* mutex to ensure that no send actions are mixed
       (sending can be done by send and recv thread;
        may not be simultaneously locked by the same thread) */
    pthread_mutex_t send_mutex;
#endif

    // the WebSocket communications via mg_wakeup()
    struct mg_mgr *mgr;
    unsigned long conn_id; // Parent connection ID

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

    // WS spectrum ring buffer event loop
    pthread_mutex_t ws_mtx;
    pthread_mutex_t ws_cond_mtx;
    volatile sig_atomic_t ws_exit;
    pthread_cond_t ws_cond;
    pthread_t ws_thread;
    struct ring_buffer *ws_ring;

    // WS video ring buffer event loop
    pthread_mutex_t video_mtx;
    pthread_mutex_t video_cond_mtx;
    volatile sig_atomic_t video_exit;
    pthread_cond_t video_cond;
    pthread_t video_thread;
    struct ring_buffer *video_ring;

    // WS PV-Diagram ring buffer event loop
    pthread_mutex_t pv_mtx;
    volatile sig_atomic_t pv_exit;
    pthread_cond_t pv_cond;
    pthread_mutex_t cond_mtx;
    pthread_t pv_thread;
    struct ring_buffer *pv_ring;
} websocket_session;

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

void init_session_table();
websocket_session *new_session(void);
void remove_session(websocket_session *session);
void delete_session_table();
void delete_session(websocket_session *session);

/*static void release_session(websocket_session *session)
{
    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
};

G_DEFINE_AUTOPTR_CLEANUP_FUNC(websocket_session, release_session)*/

void start_ws();
void close_pipe(int fd);

extern void submit_channel_range(void *ptr, int idx, int progress, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
extern void *realtime_image_spectrum_request(void *req);
extern void *realtime_image_spectrum_request_simd(void *req);
extern void *spectrum_request_simd(void *req);
extern void *ws_image_spectrum_request(void *req);
extern void *ws_pv_request(void *req);
extern void *ws_composite_pv_request(void *req);
extern void *video_request_simd(void *req);
extern void *composite_video_request_simd(void *req);
void *spectrum_response(void *ptr);
void *realtime_image_spectrum_response(void *ptr);
void *ws_image_spectrum_response(void *ptr);
void *ws_pv_response(void *ptr);
void *video_response(void *ptr);
void *composite_video_response(void *ptr);
extern void get_inner_dimensions(void *ptr, int width, int height, int *fits_width, int *fits_height, int *inner_width, int *inner_height, float *scale);
extern void get_spectrum_range_C(void *ptr, double frame_start, double frame_end, double ref_freq, int *first, int *last);
extern void fill_global_statistics(void *ptr, float *dmin, float *dmax, float *dmedian, float *dmadN, float *dmadP);
extern void update_timestamp(void *ptr);

// now used in microws.c too
extern GHashTable *sessions;
extern pthread_mutex_t sessions_mtx;

void *ws_event_loop(void *arg);
void *video_event_loop(void *arg);
void *pv_event_loop(void *arg);
