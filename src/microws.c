#include <microhttpd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

// LZ4 character streams compressor
#include <lz4hc.h>

// ZFP floating-point compressor
#include <zfp.h>

#include "ws.h"
#include "mjson.h"
#include "hash_table.h"

void *send_cluster_heartbeat(void *arg);

#ifdef MICROWS
#include <microhttpd_ws.h>

// combined WebSocket write functions, to be used from FORTRAN
// image/spectrum
void write_ws_spectrum(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const float *spectrum, int n, int precision);
void write_ws_viewport(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, int width, int height, const float *restrict pixels, const bool *restrict mask, int precision);

// video
void write_ws_video(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const uint8_t *restrict pixels, const uint8_t *restrict mask);
void write_ws_composite_video(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const uint8_t *restrict pixels, size_t no_pixels);

#ifdef POLL
#include <poll.h>
#endif

#define PAGE_INVALID_WEBSOCKET_REQUEST "Invalid WebSocket request!"

#ifdef DEBUG
// a function to print characters in a buffer in Ox format
static void print_hex(const char *buf, size_t len)
{
    for (size_t i = 0; i < len; i++)
        printf("%02X ", (unsigned char)buf[i]);

    printf("\n");
}
#endif

static char *append_null(const char *chars, const int size)
{
    char *tmp = (char *)malloc(size + 1);

    if (tmp == NULL)
        return NULL;

    memcpy(tmp, chars, size);
    tmp[size] = '\0';

    return tmp;
}

static int atoi2(const char *chars, const int size)
{
    char *tmp = append_null(chars, size);

    if (tmp == NULL)
        return 0;

    int result = atoi(tmp);

    free(tmp);

    return result;
}

static double atof2(const char *chars, const int size)
{
    char *tmp = append_null(chars, size);

    if (tmp == NULL)
        return 0.0;

    double result = atof(tmp);

    free(tmp);

    return result;
}

static size_t ws_receive_frame(unsigned char *frame, size_t *length, int *type)
{
    unsigned char masks[4];
    unsigned char mask;
    unsigned char flength;
    unsigned char idx_first_mask;
    unsigned char idx_first_data;
    size_t data_length, consumed;
    int i;
    int j;

    *type = frame[0] & 0x0F;
#ifdef DEBUG
    printf("[C] ws_receive_frame type %d, processing %zu bytes.\n", *type, *length);
#endif

    if (frame[0] == (WS_FIN | WS_OPCODE_CON_CLOSE_FRAME))
        return 0;

    consumed = 0; // assume no data is consumed
    if (frame[0] == (WS_FIN | WS_OPCODE_TEXT_FRAME) || frame[0] == (WS_FIN | WS_OPCODE_BINARY_FRAME) || frame[0] == (WS_FIN | WS_OPCODE_PING_FRAME) || frame[0] == (WS_FIN | WS_OPCODE_PONG_FRAME))
    {
        idx_first_mask = 2;
        mask = frame[1];
        flength = mask & 0x7F;

        if (flength == 126)
        {
            idx_first_mask = 4;

            // the following 2 bytes interpreted as a 16-bit unsigned integer are the data length
            data_length = ((size_t)frame[2] << 8) | (size_t)frame[3];
        }
        else if (flength == 127)
        {
            idx_first_mask = 10;

            // the following 8 bytes interpreted as a 64-bit unsigned integer
            // (the most significant bit MUST be 0) are the data length
            data_length = ((size_t)frame[2] << 56) | ((size_t)frame[3] << 48) | ((size_t)frame[4] << 40) | ((size_t)frame[5] << 32) | ((size_t)frame[6] << 24) | ((size_t)frame[7] << 16) | ((size_t)frame[8] << 8) | (size_t)frame[9];
        }
        else
        {
            data_length = (size_t)flength;
        }

#ifdef DEBUG
        printf("[C] ws_receive_frame: flength: %d, data_length: %zu\n", flength, data_length);
#endif

        idx_first_data = (unsigned char)(idx_first_mask + 4);

        masks[0] = frame[idx_first_mask + 0];
        masks[1] = frame[idx_first_mask + 1];
        masks[2] = frame[idx_first_mask + 2];
        masks[3] = frame[idx_first_mask + 3];

        // return if there is insufficient data to complete the frame
        if (idx_first_data + data_length > *length)
        {
            // blank out the type
            *type = 0;
            return 0;
        }

        // decode the message
        for (i = idx_first_data, j = 0; j < (int)data_length; i++, j++)
            frame[j] = frame[i] ^ masks[j % 4]; // neat, overwrite the incoming frame buffer

        // the entire WebSocket frame has been processed
        consumed = i; // the number of bytes consumed (equal to idx_first_data + data_length)

        *length = data_length;
        if (*type == WS_OPCODE_TEXT_FRAME)
            frame[j] = '\0';
    }
    else
        printf("[C] ws_receive_frame: received an unknown frame %02X (%d).\n", frame[0], *type);

    return consumed;
}

size_t preamble_ws_frame(char **frame_data, size_t length, unsigned char type)
{
    unsigned char *frame;
    unsigned char idx_first_data;

    if (length <= 125)
    {
        idx_first_data = 2;
    }
    else if (0xFFFF < length)
    {
        idx_first_data = 10;
    }
    else
    {
        idx_first_data = 4;
    }

    frame = malloc(idx_first_data + length);

    if (frame == NULL)
        return 0;

    frame[0] = type;

    if (length <= 125)
    {
        frame[1] = length & 0x7F;
        idx_first_data = 2;
    }
    else if (0xFFFF < length)
    {
        frame[1] = 127;
        frame[2] = (unsigned char)((length >> 56) & 0xFF);
        frame[3] = (unsigned char)((length >> 48) & 0xFF);
        frame[4] = (unsigned char)((length >> 40) & 0xFF);
        frame[5] = (unsigned char)((length >> 32) & 0xFF);
        frame[6] = (unsigned char)((length >> 24) & 0xFF);
        frame[7] = (unsigned char)((length >> 16) & 0xFF);
        frame[8] = (unsigned char)((length >> 8) & 0xFF);
        frame[9] = (unsigned char)(length & 0xFF);
        idx_first_data = 10;
    }
    else
    {
        frame[1] = 126;
        frame[2] = (length >> 8) & 0xFF;
        frame[3] = length & 0xFF;
        idx_first_data = 4;
    }

    *frame_data = (char *)frame;
    return (size_t)idx_first_data;
}

// TCP_QUICKACK is a Linux-specific socket option to enable or disable quick acknowledgment.
// it seems it needs to be re-enabled periodically (before each send_all())
static void enable_quickack(MHD_socket fd)
{
#if (!defined(__APPLE__) || !defined(__MACH__))
    int flag = 1;
    int result = setsockopt(fd, IPPROTO_TCP, TCP_QUICKACK, (char *)&flag, sizeof(int));

    if (result < 0)
        perror("enable_quickack");
#endif
}

/**
 * @brief Make the socket real-time interactive
 * by disabling the Nagle's algorithm
 * (enabling TCP_NODELAY)
 * and enabling TCP_QUICKACK
 *
 * * @param fd the socket to manipulate
 */
static void make_real_time(MHD_socket fd)
{
    int flag = 1;

    // TCP_NODELAY
    int result = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(int));

    if (result < 0)
        perror("make_real_time (TCP_NODELAY)");
    else
        printf("[C] WebSocket enabled TCP_NODELAY.\n");

    // SO_KEEPALIVE
    result = setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char *)&flag, sizeof(int));

    if (result < 0)
        perror("make_real_time (SO_KEEPALIVE)");
    else
        printf("[C] WebSocket enabled SO_KEEPALIVE.\n");

    // TCP_QUICKACK
    enable_quickack(fd);
}

/**
 * Change socket to blocking.
 *
 * @param fd the socket to manipulate
 */
static void make_blocking(MHD_socket fd)
{
#if defined(MHD_POSIX_SOCKETS)
    int flags;

    flags = fcntl(fd, F_GETFL);
    if (-1 == flags)
        abort();

    if ((flags & ~O_NONBLOCK) != flags)
    {
        printf("[C] make_blocking: currently the socket is non-blocking, will change it.\n");
        if (-1 == fcntl(fd, F_SETFL, flags & ~O_NONBLOCK))
            abort();
    }

#elif defined(MHD_WINSOCK_SOCKETS)
    unsigned long flags = 0;

    if (0 != ioctlsocket(fd, (int)FIONBIO, &flags))
        abort();
#endif /* MHD_WINSOCK_SOCKETS */
}

/**
 * Change socket to non-blocking.
 *
 * @param fd the socket to manipulate
 */
static void make_non_blocking(MHD_socket fd)
{
#if defined(MHD_POSIX_SOCKETS)
    int flags;

    flags = fcntl(fd, F_GETFL);
    if (-1 == flags)
        abort();

    if ((flags | O_NONBLOCK) != flags)
    {
        printf("[C] make_non_blocking: currently the socket is blocking, will change it.\n");
        if (-1 == fcntl(fd, F_SETFL, flags | O_NONBLOCK))
            abort();
    }

#elif defined(MHD_WINSOCK_SOCKETS)
    unsigned long flags = 1;

    if (0 != ioctlsocket(fd, (int)FIONBIO, &flags))
        abort();
#endif /* MHD_WINSOCK_SOCKETS */
}

/**
 * Sends all data of the given buffer via the TCP/IP socket
 *
 * @param fd  The TCP/IP socket which is used for sending
 * @param buf The buffer with the data to send
 * @param len The length in bytes of the data in the buffer
 */
void send_all(websocket_session *session, const char *buf, size_t len)
{
    if (buf == NULL || len == 0)
        return;

    size_t sent = 0;

    if (pthread_mutex_lock(&session->send_mutex) == 0)
    {
        ssize_t ret = 0;
        size_t off;

#ifdef POLL
        struct pollfd fds;
        fds.fd = session->fd;
        fds.events = POLLOUT;
        fds.revents = 0;
#endif

        for (off = 0; off < len; off += ret)
        {
#ifdef POLL
            /* poll the socket for the ability to write */
            int poll_ret = poll(&fds, 1, -1);

            if (poll_ret == 0)
            {
                printf("[C] <send_all(%zu bytes)> poll timeout, retrying.\n", len);
                ret = 0;
                continue;
            }
            else if (poll_ret < 0)
            {
                if (EAGAIN == errno)
                {
                    ret = 0;
                    continue;
                }

                perror("send_all");
                break;
            }
#endif

            // no chunking, send as much as possible as soon as possible
            ret = send(session->fd, &buf[off], (int)(len - off), MSG_DONTWAIT);

            // custom chunking using normal frames
            // ret = send(session->fd, &buf[off], MIN((int)(len - off), 1460), MSG_DONTWAIT);

            // chunking using jumbo frames instead
            // ret = send(session->fd, &buf[off], MIN((int)(len - off), 8960), MSG_DONTWAIT);

            if (0 > ret)
            {
                if (EAGAIN == errno || EWOULDBLOCK == errno)
                {
                    ret = 0;
                    continue;
                }
                else
                {
                    perror("send_all");
                    break;
                }
            }
            else
                sent += (size_t)ret;

#ifdef DEBUG
            printf("[C] <send_all(%zu bytes)> sent %zd bytes.\n", len, ret);
#endif

            if (0 == ret)
                break;
        }

        // don't delay the sending operation, re-enable TCP_QUICKACK here for the next send
        enable_quickack(session->fd);

        pthread_mutex_unlock(&session->send_mutex);
    }
    else
    {
        printf("[C] <send_all(%zu bytes)> failed, cannot lock the WebSocket send_mutex!\n", len);
        return;
    }

    if (sent != len)
        printf("[C] <send_all(%zu bytes)> failed, sent %zu bytes out of %zu bytes!\n", len, sent, len);
#ifdef DEBUG
    else
        printf("[C] <send_all(%zu bytes)> sent %zu bytes.\n", len, sent);
#endif
}

static void *ws_send_messages(void *cls)
{
    if (cls == NULL)
        pthread_exit(NULL);

    websocket_session *session = (websocket_session *)cls;

    // wait for the condition, loop through the queue and send messages
    while (!session->pv_exit)
    {
        if (session->disconnect)
            break;

        size_t len = 0;
        char *buf = NULL;

        // Check if we have a message from the worker
        while ((len = mg_queue_next(&session->queue, &buf)) > 0)
        {
            if (len == sizeof(struct data_buf))
            {
                struct data_buf *msg = (struct data_buf *)buf;

                // Retrieved a message from the queue. Send a response and cleanup
#ifdef DEBUG
                printf("[C] found a WebSocket connection, sending %zu bytes.\n", msg->len);
#endif

                if (msg->len > 0 && msg->buf != NULL && !session->disconnect)
                    send_all(session, msg->buf, msg->len);

                // release memory
                if (msg->buf != NULL)
                {
                    free(msg->buf);
                    msg->buf = NULL;
                    msg->len = 0;
                }
            }

            mg_queue_del(&session->queue, len); // Remove message from the queue
        }

        /* wait on a condition variable */
        pthread_cond_wait(&session->wake_up_sender, &session->wake_up_cond_mtx);
    }

    printf("[C] ws_send_messages terminated.\n");
    pthread_exit(NULL);
}

static int parse_received_websocket_stream(websocket_session *session, char *buf, size_t *buf_len)
{
    char *frame_data = buf;
    size_t frame_len = *buf_len;
    size_t processed = 0;

    do
    {
        int type = 0;
        processed = ws_receive_frame((unsigned char *)frame_data, &frame_len, &type);

        /* application logic */
        switch (type)
        {
        case WS_OPCODE_TEXT_FRAME:
#ifdef DEBUG
            printf("[C] WebSocket received a text frame '%.*s'\n", (int)frame_len, frame_data);
#endif

            // parse the received message
            if (NULL != strstr(frame_data, "[heartbeat]"))
            {
                /* re-transmit the heartbeat 'as-is' */
                char *response = NULL;
                size_t response_len = preamble_ws_frame(&response, frame_len, WS_FRAME_TEXT);

                if (response != NULL)
                {
                    // copy the frame_data into the response buffer
                    memcpy(response + response_len, frame_data, frame_len);
                    response_len += frame_len;

#ifdef DIRECT
                    // a direct send
                    if (!session->disconnect)
                        send_all(session, response, response_len);
                    free(response);
#else
                    // create a queue message
                    struct data_buf msg = {response, response_len};

                    char *msg_buf = NULL;
                    size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                    pthread_spin_lock(&session->queue_lock);
#else
                    pthread_mutex_lock(&session->queue_mtx);
#endif

                    // reserve space for the text message
                    size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                    // pass the message over to the sender via a communications queue
                    if (msg_buf != NULL && queue_len >= _len)
                    {
                        memcpy(msg_buf, &msg, _len);
                        mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif

                        // wake up the sender
                        pthread_cond_signal(&session->wake_up_sender);
                    }
                    else
                    {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif
                        printf("[C] mg_queue_book failed, freeing memory.\n");
                        free(response);
                    }
#endif
                }

                // get all the datasets and update their timestamps
                char *datasetId = NULL;

                // tokenize session->multi
                datasetId = session->multi != NULL ? strdup(session->multi) : NULL;
                char *token = datasetId;
                char *rest = token;

                while ((token = strtok_r(rest, ";", &rest)) != NULL)
                {
                    void *item = get_dataset(token);

                    if (item != NULL)
                    {
#ifdef DEBUG
                        printf("[C] updating the timestamp for '%s'\n", token);
#endif
                        update_timestamp(item);

                        // trigger updates across the cluster too
                        // create and detach a thread to send a message to the cluster
                        pthread_t thread;

                        char *_token = strdup(token);
                        int stat = pthread_create(&thread, NULL, send_cluster_heartbeat, (void *)_token);

                        if (stat != 0)
                        {
                            printf("[C] cannot create a 'send_cluster_heartbeat' thread!\n");
                            free(_token);
                        }
                        else
                            pthread_detach(thread);
                    }
                }

                free(datasetId);
                goto clean_ws_frame;
            }

            // get the JSON message type
            char type[32] = "";
            int koff, klen, voff, vlen, vtype, off;

            if (mjson_get_string(frame_data, (int)frame_len, "$.type", type, sizeof(type)) == -1)
            {
                printf("[C] cannot get the JSON message type!\n");
                goto clean_ws_frame;
            }

            if (strcmp(type, "composite_pv") == 0)
            {
                if (session->pv_exit)
                    goto clean_ws_frame;

                // parse the JSON request
                struct pv_request *req = (struct pv_request *)malloc(sizeof(struct pv_request));

                if (req == NULL)
                    goto clean_ws_frame;

                req->x1 = -1;
                req->y1 = -1;
                req->x2 = -1;
                req->y2 = -1;
                req->width = 0;
                req->height = 0;
                req->frame_start = 0.0;
                req->frame_end = 0.0;
                req->ref_freq = 0.0;
                req->deltaV = 0.0;
                req->rest = false;
                req->seq_id = 0;
                req->timestamp = 0.0;
                req->fd = -1;
                req->va_count = 0;
                req->ptr[0] = NULL;
                req->ptr[1] = NULL;
                req->ptr[2] = NULL;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    //  printf("key: %.*s, value: %.*s\n", klen, frame_data + koff, vlen, frame_data + voff);

                    // 'x1'
                    if (strncmp(frame_data + koff, "\"x1\"", klen) == 0)
                        req->x1 = atoi2(frame_data + voff, vlen);

                    // 'y1'
                    if (strncmp(frame_data + koff, "\"y1\"", klen) == 0)
                        req->y1 = atoi2(frame_data + voff, vlen);

                    // 'x2'
                    if (strncmp(frame_data + koff, "\"x2\"", klen) == 0)
                        req->x2 = atoi2(frame_data + voff, vlen);

                    // 'y2'
                    if (strncmp(frame_data + koff, "\"y2\"", klen) == 0)
                        req->y2 = atoi2(frame_data + voff, vlen);

                    // 'width'
                    if (strncmp(frame_data + koff, "\"width\"", klen) == 0)
                        req->width = atoi2(frame_data + voff, vlen);

                    // 'height'
                    if (strncmp(frame_data + koff, "\"height\"", klen) == 0)
                        req->height = atoi2(frame_data + voff, vlen);

                    // 'frame_start'
                    if (strncmp(frame_data + koff, "\"frame_start\"", klen) == 0)
                        req->frame_start = atof2(frame_data + voff, vlen);

                    // 'frame_end'
                    if (strncmp(frame_data + koff, "\"frame_end\"", klen) == 0)
                        req->frame_end = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        req->ref_freq = atof2(frame_data + voff, vlen);

                    // 'deltaV'
                    if (strncmp(frame_data + koff, "\"deltaV\"", klen) == 0)
                        req->deltaV = atof2(frame_data + voff, vlen);

                    // 'rest'
                    if (strncmp(frame_data + koff, "\"rest\"", klen) == 0)
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            req->rest = true;

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                // next iterate through the multiple datasets launching individual channel threads
                // tokenize session->multi
                char *datasetId = session->multi != NULL ? strdup(session->multi) : NULL;
                char *token = datasetId;
                char *rest = token;
                bool skip_frame = false;

                while ((token = strtok_r(rest, ";", &rest)) != NULL)
                {
                    void *item = get_dataset(token);

                    if (item == NULL)
                        continue;

                    update_timestamp(item);

                    // RGB
                    req->ptr[req->va_count] = item;
                    req->va_count++; // increment the channel count
                }

                free(datasetId);

                if (req->va_count > 0)
                {
                    pthread_mutex_lock(&session->pv_mtx);

                    // add the request to the circular queue
                    ring_put(session->pv_ring, req);

                    if (!session->pv_exit)
                        pthread_cond_signal(&session->pv_cond); // wake up the pv event loop

                    // finally unlock the mutex
                    pthread_mutex_unlock(&session->pv_mtx);
                }
                else
                    free(req);

                goto clean_ws_frame;
            }

            // [WS] {"type":"pv","x1":108,"y1":127,"x2":131,"y2":99,"width":1129,"height":801,"frame_start":146830393957.08142,"frame_end":147767129569,"ref_freq":147300000000,"deltaV":0,"rest":false,"timestamp":10713.300000000745}
            if (strcmp(type, "pv") == 0)
            {
                if (session->pv_exit)
                    goto clean_ws_frame;

                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                // parse the JSON request
                struct pv_request *req = (struct pv_request *)malloc(sizeof(struct pv_request));

                if (req == NULL)
                    goto clean_ws_frame;

                // default values just in case ...
                req->x1 = -1;
                req->y1 = -1;
                req->x2 = -1;
                req->y2 = -1;
                req->width = 0;
                req->height = 0;
                req->frame_start = 0.0;
                req->frame_end = 0.0;
                req->ref_freq = 0.0;
                req->deltaV = 0.0;
                req->rest = false;
                req->seq_id = 0;
                req->timestamp = 0.0;
                req->fd = -1;
                req->va_count = 1;
                req->ptr[0] = item;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    //  printf("key: %.*s, value: %.*s\n", klen, frame_data + koff, vlen, frame_data + voff);

                    // 'x1'
                    if (strncmp(frame_data + koff, "\"x1\"", klen) == 0)
                        req->x1 = atoi2(frame_data + voff, vlen);

                    // 'y1'
                    if (strncmp(frame_data + koff, "\"y1\"", klen) == 0)
                        req->y1 = atoi2(frame_data + voff, vlen);

                    // 'x2'
                    if (strncmp(frame_data + koff, "\"x2\"", klen) == 0)
                        req->x2 = atoi2(frame_data + voff, vlen);

                    // 'y2'
                    if (strncmp(frame_data + koff, "\"y2\"", klen) == 0)
                        req->y2 = atoi2(frame_data + voff, vlen);

                    // 'width'
                    if (strncmp(frame_data + koff, "\"width\"", klen) == 0)
                        req->width = atoi2(frame_data + voff, vlen);

                    // 'height'
                    if (strncmp(frame_data + koff, "\"height\"", klen) == 0)
                        req->height = atoi2(frame_data + voff, vlen);

                    // 'frame_start'
                    if (strncmp(frame_data + koff, "\"frame_start\"", klen) == 0)
                        req->frame_start = atof2(frame_data + voff, vlen);

                    // 'frame_end'
                    if (strncmp(frame_data + koff, "\"frame_end\"", klen) == 0)
                        req->frame_end = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        req->ref_freq = atof2(frame_data + voff, vlen);

                    // 'deltaV'
                    if (strncmp(frame_data + koff, "\"deltaV\"", klen) == 0)
                        req->deltaV = atof2(frame_data + voff, vlen);

                    // 'rest'
                    if (strncmp(frame_data + koff, "\"rest\"", klen) == 0)
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            req->rest = true;

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                // printf("[C] P-V Diagram request: x1: %d, y1: %d, x2: %d, y2: %d, width: %d, height: %d, frame_start: %f, frame_end: %f, ref_freq: %f, deltaV: %f, rest: %d, timestamp: %f, seq_id: %d\n", req->x1, req->y1, req->x2, req->y2, req->width, req->height, req->frame_start, req->frame_end, req->ref_freq, req->deltaV, req->rest, req->timestamp, req->seq_id);

                pthread_mutex_lock(&session->pv_mtx);

                // add the request to the circular queue
                ring_put(session->pv_ring, req);

                if (!session->pv_exit)
                    pthread_cond_signal(&session->pv_cond); // wake up the pv event loop

                // finally unlock the mutex
                pthread_mutex_unlock(&session->pv_mtx);

                goto clean_ws_frame;
            }

            // [WS] {"type":"image","dx":1462,"width":1541.5999755859375,"height":794,"quality":"medium","intensity":"integrated","frame_start":344401602984.4286,"frame_end":344629439356.3494,"ref_freq":345115000000,"timestamp":8141.999999999999}
            if (strcmp(type, "image") == 0)
            {
                struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

                if (req == NULL)
                    goto clean_ws_frame;

                // default values just in case ...
                req->dx = 0;
                req->image = true;
                req->quality = medium;
                req->x1 = -1;
                req->x2 = -1;
                req->y1 = -1;
                req->y2 = -1;
                req->width = 0;
                req->height = 0;
                req->beam = square;
                req->intensity = integrated;
                req->frame_start = 0.0;
                req->frame_end = 0.0;
                req->ref_freq = 0.0;
                req->median = NAN;
                req->seq_id = 0;
                req->timestamp = 0.0;
                req->fd = -1;
                req->ptr = NULL;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // printf("key: %.*s, value: %.*s\n", klen, frame_data + koff, vlen, frame_data + voff);

                    // 'dx'
                    if (strncmp(frame_data + koff, "\"dx\"", klen) == 0)
                        req->dx = atoi2(frame_data + voff, vlen);

                    // 'quality'
                    if (strncmp(frame_data + koff, "\"quality\"", klen) == 0)
                    {
                        // low
                        if (strncmp(frame_data + voff, "\"low\"", vlen) == 0)
                            req->quality = low;

                        // medium
                        if (strncmp(frame_data + voff, "\"medium\"", vlen) == 0)
                            req->quality = medium;

                        // high
                        if (strncmp(frame_data + voff, "\"heigh\"", vlen) == 0)
                            req->quality = high;
                    }

                    // 'width'
                    if (strncmp(frame_data + koff, "\"width\"", klen) == 0)
                        req->width = atoi2(frame_data + voff, vlen);

                    // 'height'
                    if (strncmp(frame_data + koff, "\"height\"", klen) == 0)
                        req->height = atoi2(frame_data + voff, vlen);

                    // 'intensity'
                    if (strncmp(frame_data + koff, "\"intensity\"", klen) == 0)
                    {
                        // mean
                        if (strncmp(frame_data + voff, "\"mean\"", vlen) == 0)
                            req->intensity = mean;

                        // integrated
                        if (strncmp(frame_data + voff, "\"integrated\"", vlen) == 0)
                            req->intensity = integrated;
                    }

                    // 'frame_start'
                    if (strncmp(frame_data + koff, "\"frame_start\"", klen) == 0)
                        req->frame_start = atof2(frame_data + voff, vlen);

                    // 'frame_end'
                    if (strncmp(frame_data + koff, "\"frame_end\"", klen) == 0)
                        req->frame_end = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        req->ref_freq = atof2(frame_data + voff, vlen);

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                // printf("[C] dx: %d, quality: %d, width: %d, height: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, seq_id: %d, timestamp: %f\n", req->dx, req->quality, req->width, req->height, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->seq_id, req->timestamp);

                struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

                if (resp == NULL)
                {
                    free(req);
                    goto clean_ws_frame;
                }

                // pass the request to FORTRAN
                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                int stat;
                int pipefd[2];

                // open a Unix pipe
                stat = pipe(pipefd);

                if (stat == 0)
                {
                    // pass the read end of the pipe to a C thread
                    resp->session = g_atomic_rc_box_acquire(session);
                    resp->fps = 0;
                    resp->bitrate = 0;
                    resp->timestamp = req->timestamp;
                    resp->seq_id = req->seq_id;
                    resp->fd = pipefd[0];

                    // pass the write end of the pipe to a FORTRAN thread
                    req->fd = pipefd[1];
                    req->ptr = item;

                    pthread_t tid_req, tid_resp;

                    // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                    stat = pthread_create(&tid_req, NULL, &ws_image_spectrum_request, req);

                    if (stat == 0)
                    {
                        pthread_detach(tid_req);

                        // launch a pipe read C pthread
                        stat = pthread_create(&tid_resp, NULL, &ws_image_spectrum_response, resp);

                        if (stat == 0)
                            pthread_detach(tid_resp);
                        else
                        {
                            // close the read end of the pipe
                            close(pipefd[0]);

                            // release the response memory since there is no reader
                            g_atomic_rc_box_release_full(resp->session, (GDestroyNotify)delete_session);
                            free(resp);
                        }
                    }
                    else
                    {
                        free(req);

                        // close the write end of the pipe
                        close(pipefd[1]);

                        // close the read end of the pipe
                        close(pipefd[0]);

                        // release the response memory since there is no writer
                        g_atomic_rc_box_release_full(resp->session, (GDestroyNotify)delete_session);
                        free(resp);
                    }
                }
                else
                {
                    free(req);
                    free(resp);
                }

                goto clean_ws_frame;
            }

            // handle CSV spectrum export requests
            if (strcmp(type, "spectrum") == 0)
            {
                struct spectrum_request *req = (struct spectrum_request *)malloc(sizeof(struct image_spectrum_request));

                if (req == NULL)
                    goto clean_ws_frame;

                // default values just in case ...
                req->ra = NULL;
                req->dec = NULL;
                req->x1 = -1;
                req->x2 = -1;
                req->y1 = -1;
                req->y2 = -1;
                req->beam = square; // by default assume a rectangular (square) viewport
                req->intensity = integrated;
                req->frame_start = 0.0;
                req->frame_end = 0.0;
                req->ref_freq = 0.0;
                req->deltaV = 0.0;
                req->rest = false;
                req->seq_id = 0;
                req->timestamp = 0.0;
                req->fd = -1;
                req->ptr = NULL;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // printf("key: '%.*s', value: '%.*s'\n", klen, frame_data + koff, vlen, frame_data + voff);

                    // 'ra'
                    if (strncmp(frame_data + koff, "\"ra\"", klen) == 0)
                        req->ra = strndup(frame_data + voff + 1, vlen - 2); // skip the surrounding ""

                    // 'dec'
                    if (strncmp(frame_data + koff, "\"dec\"", klen) == 0)
                        req->dec = strndup(frame_data + voff + 1, vlen - 2); // skip the surrounding ""

                    // 'x1'
                    if (strncmp(frame_data + koff, "\"x1\"", klen) == 0)
                        req->x1 = atoi2(frame_data + voff, vlen);

                    // 'y1'
                    if (strncmp(frame_data + koff, "\"y1\"", klen) == 0)
                        req->y1 = atoi2(frame_data + voff, vlen);

                    // 'x2'
                    if (strncmp(frame_data + koff, "\"x2\"", klen) == 0)
                        req->x2 = atoi2(frame_data + voff, vlen);

                    // 'y2'
                    if (strncmp(frame_data + koff, "\"y2\"", klen) == 0)
                        req->y2 = atoi2(frame_data + voff, vlen);

                    // 'beam'
                    if (strncmp(frame_data + koff, "\"beam\"", klen) == 0)
                    {
                        // circle
                        if (strncmp(frame_data + voff, "\"circle\"", vlen) == 0)
                            req->beam = circle;

                        // square
                        if (strncmp(frame_data + voff, "\"square\"", vlen) == 0)
                            req->beam = square;
                    }

                    // 'intensity'
                    if (strncmp(frame_data + koff, "\"intensity\"", klen) == 0)
                    {
                        // mean
                        if (strncmp(frame_data + voff, "\"mean\"", vlen) == 0)
                            req->intensity = mean;

                        // integrated
                        if (strncmp(frame_data + voff, "\"integrated\"", vlen) == 0)
                            req->intensity = integrated;
                    }

                    // 'frame_start'
                    if (strncmp(frame_data + koff, "\"frame_start\"", klen) == 0)
                        req->frame_start = atof2(frame_data + voff, vlen);

                    // 'frame_end'
                    if (strncmp(frame_data + koff, "\"frame_end\"", klen) == 0)
                        req->frame_end = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        req->ref_freq = atof2(frame_data + voff, vlen);

                    // 'deltaV'
                    if (strncmp(frame_data + koff, "\"deltaV\"", klen) == 0)
                        req->deltaV = atof2(frame_data + voff, vlen);

                    // 'rest'
                    if (strncmp(frame_data + koff, "\"rest\"", klen) == 0)
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            req->rest = true;

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                printf("[C] CSV spectrum request: ra: %s, dec: %s, x1: %d, y1: %d, x2: %d, y2: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, deltaV: %f, rest: %d, seq_id: %d, timestamp: %f\n", req->ra, req->dec, req->x1, req->y1, req->x2, req->y2, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->deltaV, req->rest, req->seq_id, req->timestamp);

                struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

                if (resp == NULL)
                {
                    free(req->ra);
                    free(req->dec);
                    free(req);
                    goto clean_ws_frame;
                }

                // pass the request to FORTRAN
                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                int stat;
                int pipefd[2];

                // open a Unix pipe
                stat = pipe(pipefd);

                if (stat == 0)
                {
                    // pass the read end of the pipe to a C thread
                    resp->session = g_atomic_rc_box_acquire(session);
                    resp->fps = 0;
                    resp->bitrate = 0;
                    resp->timestamp = req->timestamp;
                    resp->seq_id = req->seq_id;
                    resp->fd = pipefd[0];

                    // pass the write end of the pipe to a FORTRAN thread
                    req->fd = pipefd[1];
                    req->ptr = item;

                    pthread_t tid_req, tid_resp;

                    // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                    stat = pthread_create(&tid_req, NULL, &spectrum_request_simd, req);

                    if (stat == 0)
                    {
                        pthread_detach(tid_req);

                        // launch a pipe read C pthread
                        stat = pthread_create(&tid_resp, NULL, &spectrum_response, resp);

                        if (stat == 0)
                            pthread_detach(tid_resp);
                        else
                        {
                            // close the read end of the pipe
                            close(pipefd[0]);

                            // release the response memory since there is no reader
                            g_atomic_rc_box_release_full(resp->session, (GDestroyNotify)delete_session);
                            free(resp);
                        }
                    }
                    else
                    {
                        free(req->ra);
                        free(req->dec);
                        free(req);

                        // close the write end of the pipe
                        close(pipefd[1]);

                        // close the read end of the pipe
                        close(pipefd[0]);

                        // release the response memory since there is no writer
                        g_atomic_rc_box_release_full(resp->session, (GDestroyNotify)delete_session);
                        free(resp);
                    }
                }
                else
                {
                    free(req->ra);
                    free(req->dec);
                    free(req);
                    free(resp);
                }

                goto clean_ws_frame;
            }

            // handle real-time spectrum/viewport requests
            if (strcmp(type, "realtime_image_spectrum") == 0)
            {
                if (session->ws_exit)
                    goto clean_ws_frame;

                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

                if (req == NULL)
                    goto clean_ws_frame;

                // default values just in case ...
                req->dx = 0;
                req->image = false;
                req->quality = medium;
                req->x1 = -1;
                req->x2 = -1;
                req->y1 = -1;
                req->y2 = -1;
                req->width = 0;
                req->height = 0;
                req->beam = circle;
                req->intensity = integrated;
                req->frame_start = 0.0;
                req->frame_end = 0.0;
                req->ref_freq = 0.0;
                req->median = NAN;
                req->seq_id = 0;
                req->timestamp = 0.0;
                req->session = NULL;
                req->fd = -1;
                req->ptr = item;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // printf("key: %.*s, value: %.*s\n", klen, frame_data + koff, vlen, frame_data + voff);

                    // 'dx'
                    if (strncmp(frame_data + koff, "\"dx\"", klen) == 0)
                        req->dx = atoi2(frame_data + voff, vlen);

                    // 'image'
                    if (strncmp(frame_data + koff, "\"image\"", klen) == 0)
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            req->image = true;

                    // 'quality'
                    if (strncmp(frame_data + koff, "\"quality\"", klen) == 0)
                    {
                        // low
                        if (strncmp(frame_data + voff, "\"low\"", vlen) == 0)
                            req->quality = low;

                        // medium
                        if (strncmp(frame_data + voff, "\"medium\"", vlen) == 0)
                            req->quality = medium;

                        // high
                        if (strncmp(frame_data + voff, "\"heigh\"", vlen) == 0)
                            req->quality = high;
                    }

                    // 'x1'
                    if (strncmp(frame_data + koff, "\"x1\"", klen) == 0)
                        req->x1 = atoi2(frame_data + voff, vlen);

                    // 'y1'
                    if (strncmp(frame_data + koff, "\"y1\"", klen) == 0)
                        req->y1 = atoi2(frame_data + voff, vlen);

                    // 'x2'
                    if (strncmp(frame_data + koff, "\"x2\"", klen) == 0)
                        req->x2 = atoi2(frame_data + voff, vlen);

                    // 'y2'
                    if (strncmp(frame_data + koff, "\"y2\"", klen) == 0)
                        req->y2 = atoi2(frame_data + voff, vlen);

                    // 'width'
                    if (strncmp(frame_data + koff, "\"width\"", klen) == 0)
                        req->width = atoi2(frame_data + voff, vlen);

                    // 'height'
                    if (strncmp(frame_data + koff, "\"height\"", klen) == 0)
                        req->height = atoi2(frame_data + voff, vlen);

                    // 'beam'
                    if (strncmp(frame_data + koff, "\"beam\"", klen) == 0)
                    {
                        // circle
                        if (strncmp(frame_data + voff, "\"circle\"", vlen) == 0)
                            req->beam = circle;

                        // square
                        if (strncmp(frame_data + voff, "\"square\"", vlen) == 0)
                            req->beam = square;
                    }

                    // 'intensity'
                    if (strncmp(frame_data + koff, "\"intensity\"", klen) == 0)
                    {
                        // mean
                        if (strncmp(frame_data + voff, "\"mean\"", vlen) == 0)
                            req->intensity = mean;

                        // integrated
                        if (strncmp(frame_data + voff, "\"integrated\"", vlen) == 0)
                            req->intensity = integrated;
                    }

                    // 'frame_start'
                    if (strncmp(frame_data + koff, "\"frame_start\"", klen) == 0)
                        req->frame_start = atof2(frame_data + voff, vlen);

                    // 'frame_end'
                    if (strncmp(frame_data + koff, "\"frame_end\"", klen) == 0)
                        req->frame_end = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        req->ref_freq = atof2(frame_data + voff, vlen);

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                // printf("[C] dx: %d, image: %d, quality: %d, x1: %d, y1: %d, x2: %d, y2: %d, width: %d, height: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, seq_id: %d, timestamp: %f\n", req->dx, req->image, req->quality, req->x1, req->y1, req->x2, req->y2, req->width, req->height, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->seq_id, req->timestamp);

                pthread_mutex_lock(&session->ws_mtx);

                // add the request to the circular queue
                ring_put(session->ws_ring, req);

                if (!session->ws_exit)
                    pthread_cond_signal(&session->ws_cond); // wake up the ws event loop

                // finally unlock the mutex
                pthread_mutex_unlock(&session->ws_mtx);

                goto clean_ws_frame;
            }

            // init_video
            if (strcmp(type, "init_video") == 0)
            {
                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                // read the parameters
                // last_video_seq
                int width = 0;
                int height = 0;
                int fps = 30; // hard-code the initial FPS
                int bitrate = 1000;

                int fits_width, fits_height, inner_width, inner_height;
                float scale;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // 'width'
                    if (strncmp(frame_data + koff, "\"width\"", klen) == 0)
                        width = atoi2(frame_data + voff, vlen);

                    // 'height'
                    if (strncmp(frame_data + koff, "\"height\"", klen) == 0)
                        height = atoi2(frame_data + voff, vlen);

                    // 'flux'
                    if (strncmp(frame_data + koff, "\"flux\"", klen) == 0)
                    {
                        free(session->flux);
                        session->flux = strndup(frame_data + voff + 1, vlen - 2); // avoid the enclosing double quotes
                    }

                    // 'fps'
                    /*if (strncmp(frame_data + koff, "\"fps\"", klen) == 0)
                        fps = atoi2(frame_data + voff, vlen);*/

                    // 'bitrate'
                    if (strncmp(frame_data + koff, "\"bitrate\"", klen) == 0)
                        bitrate = atoi2(frame_data + voff, vlen);
                }

                // printf("[C]::init_video width: %d, height: %d, flux: %s, fps: %d, bitrate: %d\n", width, height, session->flux, fps, bitrate);

                get_inner_dimensions(item, width, height, &fits_width, &fits_height, &inner_width, &inner_height, &scale);
                // printf("[C] FITS dims: %d x %d, INNER: %d x %d, SCALE: %f\n", fits_width, fits_height, inner_width, inner_height, scale);

                if (scale < 1.0f)
                {
                    session->image_width = roundf(scale * fits_width);
                    session->image_height = roundf(scale * fits_height);
                    session->bDownsize = true;
                }
                else
                {
                    session->image_width = fits_width;
                    session->image_height = fits_height;
                    session->bDownsize = false;
                };

                // send a JSON reply
                char *json = NULL;
                mjson_printf(mjson_print_dynamic_buf, &json, "{%Q:%Q,%Q:%d,%Q:%d,%Q:%d,%Q:%d}", "type", "init_video", "width", session->image_width, "height", session->image_height, "padded_width", session->image_width, "padded_height", session->image_height);

                if (json != NULL)
                {
                    size_t json_len = strlen(json);
                    char *response = NULL;
                    size_t response_len = preamble_ws_frame(&response, json_len, WS_FRAME_TEXT);

                    if (response != NULL)
                    {
                        // copy the frame_data into the response buffer
                        memcpy(response + response_len, json, json_len);
                        response_len += json_len;

#ifdef DIRECT
                        // a direct send
                        if (!session->disconnect)
                            send_all(session, response, response_len);
                        free(response);
#else
                        // create a queue message
                        struct data_buf msg = {response, response_len};

                        char *msg_buf = NULL;
                        size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_lock(&session->queue_lock);
#else
                        pthread_mutex_lock(&session->queue_mtx);
#endif

                        // reserve space for the text message
                        size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                        // pass the message over to the sender via a communications queue
                        if (msg_buf != NULL && queue_len >= _len)
                        {
                            memcpy(msg_buf, &msg, _len);
                            mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                            pthread_spin_unlock(&session->queue_lock);
#else
                            pthread_mutex_unlock(&session->queue_mtx);
#endif

                            // wake up the sender
                            pthread_cond_signal(&session->wake_up_sender);
                        }
                        else
                        {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                            pthread_spin_unlock(&session->queue_lock);
#else
                            pthread_mutex_unlock(&session->queue_mtx);
#endif
                            printf("[C] mg_queue_book failed, freeing memory.\n");
                            free(response);
                        }
#endif
                    }
                }

                free(json);

                // x265
                pthread_mutex_lock(&session->vid_mtx);

                // reset the frame index
                session->last_frame_idx = -1;

                if (session->param != NULL)
                    goto unlock_mutex_and_break;

                // alloc HEVC params
                x265_param *param = x265_param_alloc();
                if (param == NULL)
                    goto unlock_mutex_and_break;

                x265_param_default_preset(param, "superfast", "zerolatency");

                // HEVC param
                param->fpsNum = fps;
                param->fpsDenom = 1;
                param->bRepeatHeaders = 1;
                param->internalCsp = X265_CSP_I444;

                param->internalBitDepth = 8;
                param->sourceWidth = session->image_width;
                param->sourceHeight = session->image_height;

                // constant bitrate
                param->rc.rateControlMode = X265_RC_CRF;
                param->rc.bitrate = bitrate;

                // finally point the user session param
                session->param = param;

                if (session->encoder != NULL)
                    goto unlock_mutex_and_break;

                // HEVC encoder
                session->encoder = x265_encoder_open(param);
                if (session->encoder == NULL)
                    goto unlock_mutex_and_break;

                if (session->picture != NULL)
                    goto unlock_mutex_and_break;

                // HEVC picture
                x265_picture *picture = x265_picture_alloc();
                if (picture == NULL)
                    goto unlock_mutex_and_break;

                x265_picture_init(param, picture);

                // allocate a dummy B channel
                const size_t frame_size = session->image_width * session->image_height;
                uint8_t *B_buf = (uint8_t *)malloc(frame_size);

                if (B_buf != NULL)
                    memset(B_buf, 0, frame_size); // actually use 0 instead of 128 so that the composite RGB case is handled correctly for va_count == 2

                picture->planes[0] = NULL;
                picture->planes[1] = NULL;
                picture->planes[2] = B_buf;

                picture->stride[0] = 0;
                picture->stride[1] = 0;
                picture->stride[2] = session->image_width;

                session->picture = picture;

            unlock_mutex_and_break:
                pthread_mutex_unlock(&session->vid_mtx);
                goto clean_ws_frame;
            }

            // end_video
            if (strcmp(type, "end_video") == 0)
            {
                pthread_mutex_lock(&session->vid_mtx);

                free(session->flux);
                session->flux = NULL;

                if (session->encoder != NULL)
                {
                    x265_encoder_close(session->encoder);
                    session->encoder = NULL;
                }

                if (session->param != NULL)
                {
                    x265_param_free(session->param);
                    session->param = NULL;
                }

                if (session->picture != NULL)
                {
                    // deallocate RGB planes
                    for (int i = 0; i < 3; i++)
                        if (session->picture->planes[i] != NULL)
                            free(session->picture->planes[i]);

                    // finally free the picture
                    x265_picture_free(session->picture);
                    session->picture = NULL;
                }

                pthread_mutex_unlock(&session->vid_mtx);

                goto clean_ws_frame;
            }

            // encode and stream video
            if (strcmp(type, "video") == 0)
            {
                if (session->flux == NULL)
                    goto clean_ws_frame;

                // get the first item
                void *item = session->items[0];

                update_timestamp(item);

                // check if the session video tone mapping has been filled already
                // if not, fetch the global data statistics from FORTRAN
                if (isnan(session->dmin) || isnan(session->dmax) || isnan(session->dmedian) || isnan(session->dmadN) || isnan(session->dmadP))
                {
                    printf("[C] calling 'fill_global_statistics(...)'\n");
                    fill_global_statistics(item, &(session->dmin), &(session->dmax), &(session->dmedian), &(session->dmadN), &(session->dmadP));
                }

                struct video_request *req = (struct video_request *)malloc(sizeof(struct video_request));

                if (req == NULL)
                    goto clean_ws_frame;

                int fps = 30;
                int bitrate = 1000;

                req->video_type = single;
                req->keyframe = false; // is it a keyframe?
                req->fill = 0;
                req->seq_id = -1;

                req->frame = 0;
                req->timestamp = 0.0;

                req->flux = NULL;
                req->len = 0;

                // lock the stat mutex
                pthread_mutex_lock(&session->stat_mtx);

                req->dmin = session->dmin;
                req->dmax = session->dmax;
                req->dmedian = session->dmedian;
                req->dmadN = session->dmadN;
                req->dmadP = session->dmadP;

                // unlock the stat mutex
                pthread_mutex_unlock(&session->stat_mtx);

                req->width = session->image_width;
                req->height = session->image_height;
                req->downsize = session->bDownsize;
                req->session = NULL;
                req->ptr = item;

                double frame = 0.0;
                double ref_freq = 0.0;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // 'fps'
                    if (strncmp(frame_data + koff, "\"fps\"", klen) == 0)
                        fps = atoi2(frame_data + voff, vlen);

                    // 'bitrate'
                    if (strncmp(frame_data + koff, "\"bitrate\"", klen) == 0)
                        bitrate = atoi2(frame_data + voff, vlen);

                    // 'fill'
                    if (strncmp(frame_data + koff, "\"fill\"", klen) == 0)
                        req->fill = atoi2(frame_data + voff, vlen);

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        req->seq_id = atoi2(frame_data + voff, vlen);

                    // 'key'
                    if (strncmp(frame_data + koff, "\"key\"", klen) == 0)
                    {
                        // false
                        if (strncmp(frame_data + voff, "false", vlen) == 0)
                            req->keyframe = false;

                        // true
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            req->keyframe = true;
                    }

                    // 'frame'
                    if (strncmp(frame_data + koff, "\"frame\"", klen) == 0)
                        frame = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        ref_freq = atof2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        req->timestamp = atof2(frame_data + voff, vlen);
                }

                // get the video frame index
                int frame_idx;
                get_spectrum_range_C(item, frame, frame, ref_freq, &frame_idx, &frame_idx);
                req->frame = frame_idx;

                // printf("[C]::video fps: %d, bitrate: %d, seq_id: %d, keyframe: %d, frame: {%f --> %d}, ref_freq: %f, timestamp: %f\n", fps, bitrate, req->seq_id, req->keyframe, frame, req->frame, ref_freq, req->timestamp);

                // skip repeated frames
                if (frame_idx == session->last_frame_idx && !req->keyframe)
                {
                    printf("[C] skipping a repeat video frame #%d\n", frame_idx);
                    free(req); // req->flux is NULL at this point, no need to free it
                    goto clean_ws_frame;
                }
                else
                    session->last_frame_idx = frame_idx;

                req->flux = session->flux != NULL ? strdup(session->flux) : NULL;
                req->len = req->flux != NULL ? strlen(req->flux) : 0;

                pthread_mutex_lock(&session->video_mtx);

                // add the request to the circular queue
                ring_put(session->video_ring, req);

                if (!session->video_exit)
                    pthread_cond_signal(&session->video_cond); // wake up the video event loop

                // finally unlock the mutex
                pthread_mutex_unlock(&session->video_mtx);

                goto clean_ws_frame;
            }

            if (strcmp(type, "composite_video") == 0)
            {
                websocket_session *common_session = session;

                if (common_session == NULL)
                    goto clean_ws_frame;

                if (common_session->flux == NULL)
                    goto clean_ws_frame;

                // copy common variables from the session
                const char *flux = common_session->flux;
                int width = common_session->image_width;
                int height = common_session->image_height;
                bool downsize = common_session->bDownsize;

                // parse the JSON message common for all datasets
                int fps = 30;
                int bitrate = 1000;
                bool keyframe = false;
                int fill = 0;
                double frame = 0.0;
                double ref_freq = 0.0;
                int seq_id = -1;
                float timestamp = 0.0;

                for (off = 0; (off = mjson_next(frame_data, (int)frame_len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
                {
                    // 'fps'
                    if (strncmp(frame_data + koff, "\"fps\"", klen) == 0)
                        fps = atoi2(frame_data + voff, vlen);

                    // 'bitrate'
                    if (strncmp(frame_data + koff, "\"bitrate\"", klen) == 0)
                        bitrate = atoi2(frame_data + voff, vlen);

                    // 'fill'
                    if (strncmp(frame_data + koff, "\"fill\"", klen) == 0)
                        fill = atoi2(frame_data + voff, vlen);

                    // 'seq_id'
                    if (strncmp(frame_data + koff, "\"seq_id\"", klen) == 0)
                        seq_id = atoi2(frame_data + voff, vlen);

                    // 'key'
                    if (strncmp(frame_data + koff, "\"key\"", klen) == 0)
                    {
                        // false
                        if (strncmp(frame_data + voff, "false", vlen) == 0)
                            keyframe = false;

                        // true
                        if (strncmp(frame_data + voff, "true", vlen) == 0)
                            keyframe = true;
                    }

                    // 'frame'
                    if (strncmp(frame_data + koff, "\"frame\"", klen) == 0)
                        frame = atof2(frame_data + voff, vlen);

                    // 'ref_freq'
                    if (strncmp(frame_data + koff, "\"ref_freq\"", klen) == 0)
                        ref_freq = atof2(frame_data + voff, vlen);

                    // 'timestamp'
                    if (strncmp(frame_data + koff, "\"timestamp\"", klen) == 0)
                        timestamp = atof2(frame_data + voff, vlen);
                }

                struct composite_video_request *req = (struct composite_video_request *)malloc(sizeof(struct composite_video_request));

                if (req == NULL)
                    goto clean_ws_frame;

                req->video_type = composite;
                req->seq_id = seq_id;
                req->timestamp = timestamp;
                req->va_count = 0;
                req->flux = flux != NULL ? strdup(flux) : NULL;
                req->len = req->flux != NULL ? strlen(req->flux) : 0;
                req->width = width;
                req->height = height;
                req->downsize = downsize;
                req->keyframe = keyframe;
                req->fill = fill;
                req->session = NULL;

                // next iterate through the multiple datasets launching individual channel threads
                // tokenize session->multi
                char *datasetId = common_session->multi != NULL ? strdup(common_session->multi) : NULL;
                char *token = datasetId;
                char *rest = token;
                bool skip_frame = false;

                while ((token = strtok_r(rest, ";", &rest)) != NULL)
                {
                    void *item = get_dataset(token);

                    if (item == NULL)
                        continue;

                    update_timestamp(item);

                    websocket_session *_session = NULL;

                    // get the session
                    if (pthread_mutex_lock(&sessions_mtx) == 0)
                    {
                        // iterate through the sessions, find the one with the matching dataset
                        GHashTableIter iter;
                        gpointer key, value;

                        g_hash_table_iter_init(&iter, sessions);

                        while (g_hash_table_iter_next(&iter, &key, &value))
                        {
                            websocket_session *__session = (websocket_session *)value;

                            if (strcmp(__session->datasetid, token) == 0)
                            {
                                _session = __session;
                                break;
                            }
                        }

                        pthread_mutex_unlock(&sessions_mtx);
                    }

                    if (_session == NULL)
                        continue;

                    // we have the dataset item and the session, prepare the video request
                    printf("[C] parse_received_websocket_stream: preparing video request for dataset '%s'.\n", token);

                    // check if the session video tone mapping has been filled already
                    // if not, fetch the global data statistics from FORTRAN
                    if (isnan(_session->dmin) || isnan(_session->dmax) || isnan(_session->dmedian) || isnan(_session->dmadN) || isnan(_session->dmadP))
                    {
                        printf("[C] calling 'fill_global_statistics(...)'\n");
                        fill_global_statistics(item, &(_session->dmin), &(_session->dmax), &(_session->dmedian), &(_session->dmadN), &(_session->dmadP));
                    }

                    // get the video frame index
                    int frame_idx;
                    get_spectrum_range_C(item, frame, frame, ref_freq, &frame_idx, &frame_idx);

                    // skip repeated frames
                    if (frame_idx == _session->last_frame_idx && !req->keyframe)
                    {
                        printf("[C] skipping a repeat video frame #%d\n", frame_idx);
                        skip_frame = true;
                    }
                    else
                        _session->last_frame_idx = frame_idx;

                    // RGB
                    req->ptr[req->va_count] = item;
                    req->frame[req->va_count] = frame_idx;
                    req->dmin[req->va_count] = _session->dmin;
                    req->dmax[req->va_count] = _session->dmax;
                    req->dmedian[req->va_count] = _session->dmedian;
                    req->dmadN[req->va_count] = _session->dmadN;
                    req->dmadP[req->va_count] = _session->dmadP;
                    req->va_count++; // increment the channel count
                }

                free(datasetId);

                if (skip_frame)
                {
                    free(req->flux);
                    free(req);
                    goto clean_ws_frame;
                }

                pthread_mutex_lock(&common_session->video_mtx);

                // add the request to the circular queue
                ring_put(common_session->video_ring, req);

                if (!common_session->video_exit)
                    pthread_cond_signal(&common_session->video_cond); // wake up the video event loop

                // finally unlock the mutex
                pthread_mutex_unlock(&common_session->video_mtx);

                goto clean_ws_frame;
            }

        clean_ws_frame:
            break;
        case WS_OPCODE_BINARY_FRAME:
            printf("[C] WebSocket received %zu bytes of binary frame\n", frame_len);
            break;
        case WS_OPCODE_CON_CLOSE_FRAME:
            printf("[C] WebSocket received a close frame\n");
            /* if we receive a close frame, we will respond with one */
            session->disconnect = true;

            char *result = NULL;
            size_t result_len = 0;
            int er = MHD_websocket_encode_close(session->ws,
                                                MHD_WEBSOCKET_CLOSEREASON_REGULAR,
                                                NULL,
                                                0,
                                                &result,
                                                &result_len);

            if (MHD_WEBSOCKET_STATUS_OK == er)
            {
                send_all(session, result, result_len);
                MHD_websocket_free(session->ws, result);
            }

            return 1;
        case WS_OPCODE_PING_FRAME:
            /* if we receive a ping frame, we will respond */
            /* with the corresponding pong frame */
            {
                char *pong = NULL;
                size_t pong_len = preamble_ws_frame(&pong, frame_len, WS_FRAME_PONG);

                if (pong != NULL)
                {
                    // copy the frame_data into the response buffer
                    memcpy(pong + pong_len, frame_data, frame_len);
                    pong_len += frame_len;

#ifdef DIRECT
                    // a direct send
                    if (!session->disconnect)
                        send_all(session, pong, pong_len);
                    free(pong);
#else
                    // create a queue message
                    struct data_buf msg = {pong, pong_len};

                    char *msg_buf = NULL;
                    size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                    pthread_spin_lock(&session->queue_lock);
#else
                    pthread_mutex_lock(&session->queue_mtx);
#endif

                    // reserve space for the text message
                    size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                    // pass the message over to the sender via a communications queue
                    if (msg_buf != NULL && queue_len >= _len)
                    {
                        memcpy(msg_buf, &msg, _len);
                        mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif

                        // wake up the sender
                        pthread_cond_signal(&session->wake_up_sender);
                    }
                    else
                    {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif
                        printf("[C] mg_queue_book failed, freeing memory.\n");
                        free(pong);
                    }
#endif
                }
            }
            break;
        case WS_OPCODE_PONG_FRAME:
            /* if we receive a pong frame, ignore it*/
            break;
        default:
            /* there might have been insufficient data to complete a frame */
            /* break, await more data */
            break;
        }

        if (processed > 0)
        {
#ifdef DEBUG
            printf("[C] WebSocket processed %zu bytes of data\n", processed);
#endif

            // memmove the remaining data to the beginning of the buffer
            memmove(frame_data, frame_data + processed, *buf_len - processed);

            size_t remaining = *buf_len - processed;
            frame_len = remaining;
            *buf_len -= processed;

#ifdef DEBUG
            printf("[C] WebSocket remaining %zu bytes of data, cursor is at %zu\n", remaining, *buf_len);
#endif
        }

        // break out of the loop if there is no more data to process
        if (*buf_len == 0)
            break;
    } while (processed > 0); // processed == 0, remaining may be > 0, need more data

    return 0;
}

static void *ws_receive_messages(void *cls)
{
    if (cls == NULL)
        pthread_exit(NULL);

    char buf[1024]; // a buffer for incoming WebSocket messages
    ssize_t got;
    size_t cursor = 0;
    int result;

    websocket_session *session = (websocket_session *)cls;

    // disable Nagle
    make_real_time(session->fd);

#ifdef POLL
    struct pollfd fds;
    fds.fd = session->fd;
    fds.events = POLLIN;
    fds.revents = 0;

    /* make the socket non-blocking */
    make_non_blocking(session->fd);
#else
    /* make the socket blocking */
    make_blocking(session->fd);
#endif

    /* initialize the wake-up-sender condition variable */
    if (0 != pthread_cond_init(&session->wake_up_sender, NULL))
    {
        MHD_upgrade_action(session->urh, MHD_UPGRADE_ACTION_CLOSE);

        // remove a session pointer from the hash table
        remove_session(session);

        free(session->extra_in);
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
        pthread_exit(NULL);
    }

    /* initialize the send mutex */
    if (0 != pthread_mutex_init(&session->send_mutex, NULL))
    {
        MHD_upgrade_action(session->urh,
                           MHD_UPGRADE_ACTION_CLOSE);

        // remove a session pointer from the hash table
        remove_session(session);

        pthread_cond_destroy(&session->wake_up_sender);
        free(session->extra_in);
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
        pthread_exit(NULL);
    }

    /* initialize the web socket stream for encoding/decoding */
    result = MHD_websocket_stream_init(&session->ws, MHD_WEBSOCKET_FLAG_SERVER | MHD_WEBSOCKET_FLAG_NO_FRAGMENTS | MHD_WEBSOCKET_FLAG_GENERATE_CLOSE_FRAMES_ON_ERROR, 0);

    if (MHD_WEBSOCKET_STATUS_OK != result)
    {
        pthread_cond_destroy(&session->wake_up_sender);
        pthread_mutex_destroy(&session->send_mutex);
        MHD_upgrade_action(session->urh, MHD_UPGRADE_ACTION_CLOSE);

        // remove a session pointer from the hash table
        remove_session(session);

        free(session->extra_in);
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
        pthread_exit(NULL);
    }

    session->disconnect = false;

    /* start the message-send thread */
    pthread_t pt;
    if (0 != pthread_create(&pt, NULL, &ws_send_messages, session))
    {
        pthread_cond_destroy(&session->wake_up_sender);
        pthread_mutex_destroy(&session->send_mutex);
        MHD_upgrade_action(session->urh, MHD_UPGRADE_ACTION_CLOSE);

        // remove a session pointer from the hash table
        remove_session(session);

        free(session->extra_in);
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
        pthread_exit(NULL);
    }

    /* start by parsing extra data MHD may have already read, if any */
    if (0 != session->extra_in_size)
    {
        printf("[C] WebSocket received %zd bytes of extra data\n", session->extra_in_size);

        // finally free the buffer
        free(session->extra_in);
        session->extra_in = NULL;
    }

    // first memset the buffer
    memset(buf, 0, sizeof(buf));

    /* the main loop for receiving data */
    while (1)
    {
#ifdef POLL
        /* poll the socket for incoming data */
        int ret = poll(&fds, 1, -1);

        if (ret < 0)
        {
            if (EAGAIN == errno)
                continue;

            perror("[C] poll");
            break;
        }

        if (0 == ret)
        {
            /* no data available right now, try again later */
            continue;
        }
#endif

        got = recv(session->fd, buf + cursor, sizeof(buf) - cursor, 0);

        if (0 > got)
        {
            if (EAGAIN == errno || EWOULDBLOCK == errno)
            {
                /* no data available right now, try again later */
                // printf("[C] recv: EAGAIN or EWOULDBLOCK\n");
                continue;
            }
            else
            {
                /* a real error occurred */
                perror("[C] recv");
                break;
            }
        }

        if (0 == got)
        {
            /* the TCP/IP socket has been closed */
            break;
        }

        // 0 < got
#ifdef DEBUG
        print_hex(buf, got);
#endif
        cursor += (size_t)got;

#ifdef DEBUG
        printf("[C] WebSocket received %zd bytes, new cursor = %zu\n", got, cursor);
#endif

        // handle the messages
        if (0 != parse_received_websocket_stream(session, buf, &cursor))
        {
            /* A websocket protocol error occurred */
            session->disconnect = true;
            pthread_cond_signal(&session->wake_up_sender);
            pthread_join(pt, NULL);

            struct MHD_UpgradeResponseHandle *urh = session->urh;
            if (NULL != urh)
            {
                session->urh = NULL;
                MHD_upgrade_action(urh, MHD_UPGRADE_ACTION_CLOSE);
            }

            pthread_cond_destroy(&session->wake_up_sender);
            pthread_mutex_destroy(&session->send_mutex);

            MHD_websocket_stream_free(session->ws);

            // remove a session pointer from the hash table
            remove_session(session);

            free(session->extra_in);
            g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
            pthread_exit(NULL);
        }
    }

    // clean-up
    session->disconnect = true;
    pthread_cond_signal(&session->wake_up_sender);
    pthread_join(pt, NULL);
    struct MHD_UpgradeResponseHandle *urh = session->urh;
    if (NULL != urh)
    {
        session->urh = NULL;
        MHD_upgrade_action(urh, MHD_UPGRADE_ACTION_CLOSE);
    }

    pthread_cond_destroy(&session->wake_up_sender);
    pthread_mutex_destroy(&session->send_mutex);

    MHD_websocket_stream_free(session->ws);

    // remove a session pointer from the hash table
    remove_session(session);

    free(session->extra_in);
    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);

    printf("[C] WebSocket receive_messages: exit\n");
    pthread_exit(NULL);
}

/**
 * Function called after a protocol "upgrade" response was sent
 * successfully and the socket should now be controlled by some
 * protocol other than HTTP.
 *
 * Any data already received on the socket will be made available in
 * @e extra_in.  This can happen if the application sent extra data
 * before MHD send the upgrade response.  The application should
 * treat data from @a extra_in as if it had read it from the socket.
 *
 * Note that the application must not close() @a sock directly,
 * but instead use #MHD_upgrade_action() for special operations
 * on @a sock.
 *
 * Data forwarding to "upgraded" @a sock will be started as soon
 * as this function return.
 *
 * Except when in 'thread-per-connection' mode, implementations
 * of this function should never block (as it will still be called
 * from within the main event loop).
 *
 * @param cls closure, whatever was given to #MHD_create_response_for_upgrade().
 * @param connection original HTTP connection handle,
 *                   giving the function a last chance
 *                   to inspect the original HTTP request
 * @param req_cls last value left in `req_cls` of the `MHD_AccessHandlerCallback`
 * @param extra_in if we happened to have read bytes after the
 *                 HTTP header already (because the client sent
 *                 more than the HTTP header of the request before
 *                 we sent the upgrade response),
 *                 these are the extra bytes already read from @a sock
 *                 by MHD.  The application should treat these as if
 *                 it had read them from @a sock.
 * @param extra_in_size number of bytes in @a extra_in
 * @param sock socket to use for bi-directional communication
 *        with the client.  For HTTPS, this may not be a socket
 *        that is directly connected to the client and thus certain
 *        operations (TCP-specific setsockopt(), getsockopt(), etc.)
 *        may not work as expected (as the socket could be from a
 *        socketpair() or a TCP-loopback).  The application is expected
 *        to perform read()/recv() and write()/send() calls on the socket.
 *        The application may also call shutdown(), but must not call
 *        close() directly.
 * @param urh argument for #MHD_upgrade_action()s on this @a connection.
 *        Applications must eventually use this callback to (indirectly)
 *        perform the close() action on the @a sock.
 */
static void
upgrade_handler(void *cls,
                struct MHD_Connection *connection,
                void *req_cls,
                const char *extra_in,
                size_t extra_in_size,
                MHD_socket fd,
                struct MHD_UpgradeResponseHandle *urh)
{
    pthread_t pt;
    (void)connection; /* Unused. Silent compiler warning. */
    (void)req_cls;    /* Unused. Silent compiler warning. */

    /* This callback must return as soon as possible. */

    if (cls == NULL)
        return;

    websocket_session *session = (websocket_session *)cls;
    printf("[C] WebSocket upgrade_handler: %s\n", session->id);

    if (0 != extra_in_size)
    {
        session->extra_in = (char *)malloc(extra_in_size);

        if (session->extra_in != NULL)
            memcpy(session->extra_in, extra_in, extra_in_size);
    }
    else
        session->extra_in = NULL;

    session->extra_in_size = extra_in_size;
    session->fd = fd;
    session->urh = urh;

    /* create a receiver thread */
    if (0 == pthread_create(&pt, NULL, &ws_receive_messages, session))
        pthread_detach(pt);
    else
    {
        printf("[C] cannot create a WebSocket receiver thread!\n");

        // remove a session pointer from the hash table
        remove_session(session);

        free(session->extra_in);
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    }
}

/**
 * Function called by the MHD_daemon when the client tries to access a page.
 *
 * This is used to provide the main page
 * (in this example HTML + CSS + JavaScript is all in the same file)
 * and to initialize a websocket connection.
 * The rules for the initialization of a websocket connection
 * are listed near the URL check of "/ChatServerWebSocket".
 *
 * @param cls closure, whatever was given to #MHD_start_daemon().
 * @param connection The HTTP connection handle
 * @param url The requested URL
 * @param method The request method (typically "GET")
 * @param version The HTTP version
 * @param upload_data Given upload data for POST requests
 * @param upload_data_size The size of the upload data
 * @param req_cls A pointer for request specific data
 * @return MHD_YES on success or MHD_NO on error.
 */
enum MHD_Result
on_ws_connection(void *cls,
                 struct MHD_Connection *connection,
                 const char *url,
                 const char *method,
                 const char *version,
                 const char *upload_data,
                 size_t *upload_data_size,
                 void **ptr)
{
    static int aptr;
    struct MHD_Response *response;
    int ret;
    (void)cls;              /* Unused. Silent compiler warning. */
    (void)version;          /* Unused. Silent compiler warning. */
    (void)upload_data;      /* Unused. Silent compiler warning. */
    (void)upload_data_size; /* Unused. Silent compiler warning. */

    if (0 != strcmp(method, "GET"))
        return MHD_NO; /* unexpected method */
    if (&aptr != *ptr)
    {
        /* do never respond on first call */
        *ptr = &aptr;
        return MHD_YES;
    }
    *ptr = NULL; /* reset when done */

    /**
     * The path for the chat has been accessed.
     * For a valid WebSocket request, at least five headers are required:
     * 1. "Host: <name>"
     * 2. "Connection: Upgrade"
     * 3. "Upgrade: websocket"
     * 4. "Sec-WebSocket-Version: 13"
     * 5. "Sec-WebSocket-Key: <base64 encoded value>"
     * Values are compared in a case-insensitive manner.
     * Furthermore it must be a HTTP/1.1 or higher GET request.
     * See: https://tools.ietf.org/html/rfc6455#section-4.2.1
     *
     * To make this example portable we skip the Host check
     */

    char is_valid = 1;
    const char *value = NULL;
    char sec_websocket_accept[29];

    /* check whether an websocket upgrade is requested */
    if (0 != MHD_websocket_check_http_version(version))
    {
        is_valid = 0;
    }

    value = MHD_lookup_connection_value(connection,
                                        MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_CONNECTION);
    if (0 != MHD_websocket_check_connection_header(value))
    {
        is_valid = 0;
    }

    value = MHD_lookup_connection_value(connection,
                                        MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_UPGRADE);
    if (0 != MHD_websocket_check_upgrade_header(value))
    {
        is_valid = 0;
    }

    value = MHD_lookup_connection_value(connection,
                                        MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_SEC_WEBSOCKET_VERSION);
    if (0 != MHD_websocket_check_version_header(value))
    {
        is_valid = 0;
    }

    value = MHD_lookup_connection_value(connection,
                                        MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_SEC_WEBSOCKET_KEY);
    if (0 != MHD_websocket_create_accept_header(value, sec_websocket_accept))
    {
        is_valid = 0;
    }

    // finally validate the URL (datasetId? sessionId? etc.)
    websocket_session *session = NULL;

    if (strstr(url, "/websocket/") != NULL)
    {
        printf("[C] URL: %s\n", url);

        char *sessionId = strrchr(url, '/');
        if (sessionId != NULL)
        {
            // zero-out the slash (get rid of it) to cancel the session id part
            *sessionId = '\0';

            sessionId++; // skip the slash character

            printf("[C] WEBSOCKET SESSIONID: '%s'\n", sessionId);
        }
        else
        {
            is_valid = 0;
        }

        char *datasetId = strrchr(url, '/');
        if (datasetId != NULL)
        {
            datasetId++; // skip the slash character

            printf("[C] WEBSOCKET DATASETID: '%s'\n", datasetId);
        }
        else
        {
            is_valid = 0;
        }

        char *orig = NULL;
        if (datasetId != NULL)
        {
            orig = strdup(datasetId);

            // split the string by ';', get the leading datasetId
            char *ptr = strchr(datasetId, ';');
            if (ptr != NULL)
                *ptr = '\0';
        }

        // reject connections without an entry in a hash table
        if (!dataset_exists(datasetId))
        {
            printf("[C] rejecting the WebSocket connection for '%s'.\n", datasetId);
            is_valid = 0;
        }
        else if (is_valid)
        {
            session = new_session();

            if (session != NULL)
            {
                // datasetId != NULL && sessionId != NULL
                session->datasetid = strdup(datasetId);
                session->multi = orig != NULL ? strdup(orig) : NULL;
                session->id = strdup(sessionId);

                session->items = NULL;
                session->va_count = 0;

                // tokenize session->multi
                if (orig != NULL)
                {
                    char *_datasetId = strdup(orig);
                    char *token = _datasetId;
                    char *rest = token;

                    while ((token = strtok_r(rest, ";", &rest)) != NULL)
                    {
                        void *item = get_dataset(token);

                        if (session->items == NULL)
                        {
                            // allocate the first item
                            session->items = (void **)malloc(sizeof(void *));
                        }
                        else
                        {
                            // reallocate items
                            session->items = (void **)realloc(session->items, (session->va_count + 1) * sizeof(void *));
                        }

                        session->items[session->va_count++] = item;

                        if (item != NULL)
                        {
                            // increment the reference count
                            increment_refcount(item);
                        }
                        else
                            is_valid = 0;
                    }

                    free(_datasetId);
                }

                // if there are no items by this point, reject the connection
                if (session->items == NULL)
                    is_valid = 0;

                if (!is_valid)
                {
                    printf("[C] rejecting the WebSocket connection for '%s'.\n", datasetId);

                    // clean up the items
                    if (session->items != NULL)
                    {
                        for (int i = 0; i < session->va_count; i++)
                        {
                            void *item = session->items[i];

                            if (item != NULL)
                            {
                                // decrement the reference count
                                free_dataset(item);
                            }
                        }

                        free(session->items);
                    }

                    // clean up the session fields allocated so far
                    if (session->datasetid != NULL)
                        free(session->datasetid);

                    if (session->multi != NULL)
                        free(session->multi);

                    if (session->id != NULL)
                        free(session->id);

                    free(session);
                    session = NULL;

                    goto end_of_session;
                }

                session->buf_len = 1024 * sizeof(struct data_buf);
                session->buf = (char *)malloc(session->buf_len);
                mg_queue_init(&session->queue, session->buf, session->buf_len); // Init queue
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_init(&session->queue_lock, PTHREAD_PROCESS_PRIVATE);
#else
                pthread_mutex_init(&session->queue_mtx, NULL);
#endif
                pthread_mutex_init(&session->wake_up_cond_mtx, NULL);

                session->flux = NULL;
                session->dmin = NAN;
                session->dmax = NAN;
                session->dmedian = NAN;
                session->dmadN = NAN;
                session->dmadP = NAN;
                pthread_mutex_init(&session->stat_mtx, NULL);

                session->image_width = 0;
                session->image_height = 0;
                session->bDownsize = false;

                pthread_mutex_init(&session->vid_mtx, NULL);
                session->last_frame_idx = -1;
                session->param = NULL;
                session->encoder = NULL;
                session->picture = NULL;

                int stat;

                // WS spectrum ring buffer event loop
                session->ws_exit = false;
                pthread_mutex_init(&session->ws_cond_mtx, NULL);
                pthread_cond_init(&session->ws_cond, NULL);

                pthread_mutex_init(&session->ws_mtx, NULL);
                pthread_mutex_lock(&session->ws_mtx);

                // launch a ws_thread
                stat = pthread_create(&session->ws_thread, NULL, ws_event_loop, session);

                if (stat != 0)
                    printf("[C] cannot create a ws_thread!\n");

                session->ws_ring = (struct ring_buffer *)malloc(sizeof(struct ring_buffer));

                if (session->ws_ring != NULL)
                    init_ring_buffer(session->ws_ring, 64);

                pthread_mutex_unlock(&session->ws_mtx);

                // WS video ring buffer event loop
                session->video_exit = false;
                pthread_mutex_init(&session->video_cond_mtx, NULL);
                pthread_cond_init(&session->video_cond, NULL);

                pthread_mutex_init(&session->video_mtx, NULL);
                pthread_mutex_lock(&session->video_mtx);

                // launch a ws_thread
                stat = pthread_create(&session->video_thread, NULL, video_event_loop, session);

                if (stat != 0)
                    printf("[C] cannot create a video_thread!\n");

                session->video_ring = (struct ring_buffer *)malloc(sizeof(struct ring_buffer));

                if (session->video_ring != NULL)
                    init_ring_buffer(session->video_ring, 64);

                pthread_mutex_unlock(&session->video_mtx);

                // WS P-V Diagram ring buffer event loop
                session->pv_exit = false;
                pthread_mutex_init(&session->cond_mtx, NULL);
                pthread_cond_init(&session->pv_cond, NULL);

                pthread_mutex_init(&session->pv_mtx, NULL);
                pthread_mutex_lock(&session->pv_mtx);

                // launch a pv_thread
                stat = pthread_create(&session->pv_thread, NULL, pv_event_loop, session);

                if (stat != 0)
                    printf("[C] cannot create a pv_thread!\n");

                session->pv_ring = (struct ring_buffer *)malloc(sizeof(struct ring_buffer));

                if (session->pv_ring != NULL)
                    init_ring_buffer(session->pv_ring, 8);

                pthread_mutex_unlock(&session->pv_mtx);

                // add a session pointer to the hash table
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    g_hash_table_replace(sessions, (gpointer)strdup(sessionId), g_atomic_rc_box_acquire(session));
                    pthread_mutex_unlock(&sessions_mtx);

                    printf("[C] inserted %s into the hash table\n", sessionId);
                }
                else
                {
                    printf("[C] cannot lock sessions_mtx!\n");
                }

                printf("[C] accepting the WebSocket connection for '%s', va_count: %d.\n", datasetId, session->va_count);

            // hold on to the session pointer, pass it to the WebSocket upgrade handler
            end_of_session:;
            }
        }

        free(orig);
    }
    else
    {
        is_valid = 0;
    }

    if (1 == is_valid)
    {
        /* create the response for upgrade */
        response = MHD_create_response_for_upgrade(&upgrade_handler, session);

        /**
         * For the response we need at least the following headers:
         * 1. "Connection: Upgrade"
         * 2. "Upgrade: websocket"
         * 3. "Sec-WebSocket-Accept: <base64value>"
         * The value for Sec-WebSocket-Accept can be generated with MHD_websocket_create_accept_header.
         * It requires the value of the Sec-WebSocket-Key header of the request.
         * See also: https://tools.ietf.org/html/rfc6455#section-4.2.2
         */
        MHD_add_response_header(response,
                                MHD_HTTP_HEADER_UPGRADE,
                                "websocket");
        MHD_add_response_header(response,
                                MHD_HTTP_HEADER_SEC_WEBSOCKET_ACCEPT,
                                sec_websocket_accept);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_SWITCHING_PROTOCOLS,
                                 response);
        MHD_destroy_response(response);
    }
    else
    {
        /* return error page */
        struct MHD_Response *response;
        response =
            MHD_create_response_from_buffer_static(
                strlen(PAGE_INVALID_WEBSOCKET_REQUEST),
                PAGE_INVALID_WEBSOCKET_REQUEST);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_BAD_REQUEST,
                                 response);
        MHD_destroy_response(response);
    }

    return ret;
}

void write_ws_spectrum(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const float *spectrum, int n, int precision)
{
    uchar *compressed;
    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = n;

    uint32_t length = n;

    if (session == NULL)
    {
        printf("[C] <write_ws_spectrum> NULL session pointer!\n");
        return;
    }

    if (spectrum == NULL || n <= 0)
    {
        printf("[C] <write_ws_spectrum> invalid spectrum data!\n");
        return;
    }

    // spectrum with ZFP
    field = zfp_field_1d((void *)spectrum, data_type, nx);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed = (uchar *)malloc(bufsize);

    if (compressed != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] spectrum compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
            printf("[C] float array size: %zu, compressed: %zu bytes\n", length * sizeof(float), zfpsize);

            // directly prepare and queue the WebSocket message
            if (zfpsize > 0)
            {
                size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + sizeof(uint32_t) + zfpsize;

                char *payload = NULL;
                size_t ws_len = preamble_ws_frame(&payload, msg_len, WS_FRAME_BINARY);
                msg_len += ws_len;

                if (payload != NULL)
                {
                    uint32_t id = (unsigned int)(*seq_id);
                    uint32_t msg_type = 0;
                    // 0 - spectrum, 1 - viewport,
                    // 2 - image, 3 - full, spectrum,  refresh,
                    // 4 - histogram

                    size_t ws_offset = ws_len;

                    memcpy((char *)payload + ws_offset, timestamp, sizeof(float));
                    ws_offset += sizeof(float);

                    memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, (const char *)elapsed, sizeof(float));
                    ws_offset += sizeof(float);

                    memcpy((char *)payload + ws_offset, &length, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, compressed, zfpsize);
                    ws_offset += zfpsize;

                    if (ws_offset != msg_len)
                        printf("[C] <write_ws_spectrum> size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

                    // create a queue message
                    struct data_buf msg = {payload, msg_len};

#ifdef DIRECT
                    if (!session->disconnect)
                        send_all(session, payload, msg_len);
                    free(payload);
#else
                    char *msg_buf = NULL;
                    size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                    pthread_spin_lock(&session->queue_lock);
#else
                    pthread_mutex_lock(&session->queue_mtx);
#endif

                    // reserve space for the binary message
                    size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                    // pass the message over to the sender via a communications queue
                    if (msg_buf != NULL && queue_len >= _len)
                    {
                        memcpy(msg_buf, &msg, _len);
                        mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif

                        // wake up the sender
                        pthread_cond_signal(&session->wake_up_sender);
                    }
                    else
                    {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                        pthread_spin_unlock(&session->queue_lock);
#else
                        pthread_mutex_unlock(&session->queue_mtx);
#endif
                        printf("[C] mg_queue_book failed, freeing memory.\n");
                        free(payload);
                    }
#endif
                }
            }
        }

        free(compressed);
    }
    else
        printf("[C] <write_ws_spectrum> a NULL compressed buffer!\n");

    // clean up
    zfp_field_free(field);
    zfp_stream_close(zfp);
}

void write_ws_viewport(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, int width, int height, const float *restrict pixels, const bool *restrict mask, int precision)
{
    uchar *restrict compressed_pixels = NULL;
    char *restrict compressed_mask = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint nx = width;
    uint ny = height;

    int mask_size, worst_size;
    int compressed_size = 0;

    if (session == NULL)
    {
        printf("[C] <write_ws_viewport> NULL session pointer!\n");
        return;
    }

    if (pixels == NULL || mask == NULL)
    {
        printf("[C] <write_ws_viewport> NULL pixels || mask!\n");
        return;
    }

    if (width <= 0 || height <= 0)
    {
        printf("[C] <write_ws_viewport> invalid image data!\n");
        return;
    }

    // compress pixels with ZFP
    field = zfp_field_2d((void *)pixels, data_type, nx, ny);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);

    // zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    zfp_stream_set_precision(zfp, precision);

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);

    compressed_pixels = (uchar *)malloc(bufsize);

    if (compressed_pixels != NULL)
    {
        // associate bit stream with allocated buffer
        stream = bitstream_open((void *)compressed_pixels, bufsize);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);

            // compress entire array
            zfpsize = zfp_compress(zfp, field);

            if (zfpsize == 0)
                printf("[C] ZFP compression failed!\n");
            else
                printf("[C] viewport pixels compressed size: %zu bytes\n", zfpsize);

            bitstream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
        }

        // clean up
        zfp_field_free(field);
        zfp_stream_close(zfp);
    }
    else
        printf("[C] a NULL compressed_pixels buffer!\n");

    // compress mask with LZ4-HC
    mask_size = width * height;

    worst_size = LZ4_compressBound(mask_size);

    compressed_mask = (char *)malloc(worst_size);

    if (compressed_mask != NULL)
    {
        // compress the mask as much as possible
        compressed_size = LZ4_compress_HC((const char *)mask, compressed_mask, mask_size, worst_size, LZ4HC_CLEVEL_MAX);

        printf("[C] viewport mask raw size: %d; compressed: %d bytes\n", mask_size, compressed_size);
    }

    // directly prepare and queue the WebSocket message
    if (zfpsize > 0 && compressed_size > 0)
    {
        uint32_t view_width = width;
        uint32_t view_height = height;
        uint32_t pixels_len = zfpsize;
        uint32_t mask_len = compressed_size;

        size_t view_size = zfpsize + (size_t)compressed_size;

        // size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + sizeof(uint32_t) + zfpsize;
        //  header
        size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float);
        // body
        msg_len += 4 * sizeof(uint32_t) + view_size;

        char *payload = NULL;
        size_t ws_len = preamble_ws_frame(&payload, msg_len, WS_FRAME_BINARY);
        msg_len += ws_len;

        if (payload != NULL)
        {
            uint32_t id = (unsigned int)(*seq_id);
            uint32_t msg_type = 1;
            // 0 - spectrum, 1 - viewport,
            // 2 - image, 3 - full, spectrum,  refresh,
            // 4 - histogram

#ifdef MICROWS
            size_t ws_offset = ws_len;
#else
            size_t ws_offset = 0;
#endif

            memcpy((char *)payload + ws_offset, timestamp, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
            ws_offset += sizeof(float);

            // view_width
            memcpy((char *)payload + ws_offset, &view_width, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            // view_height
            memcpy((char *)payload + ws_offset, &view_height, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            // pixels_len
            memcpy((char *)payload + ws_offset, &pixels_len, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            // compressed_pixels
            memcpy((char *)payload + ws_offset, compressed_pixels, zfpsize);
            ws_offset += zfpsize;

            // mask_len
            memcpy((char *)payload + ws_offset, &mask_len, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            // compressed_mask
            memcpy((char *)payload + ws_offset, compressed_mask, (size_t)compressed_size);
            ws_offset += (size_t)compressed_size;

            if (ws_offset != msg_len)
                printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

            // create a queue message
            struct data_buf msg = {payload, msg_len};

#ifdef DIRECT
            if (!session->disconnect)
                send_all(session, payload, msg_len);
            free(payload);
#else
            char *msg_buf = NULL;
            size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
            pthread_spin_lock(&session->queue_lock);
#else
            pthread_mutex_lock(&session->queue_mtx);
#endif

            // reserve space for the binary message
            size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

            // pass the message over to the sender via a communications queue
            if (msg_buf != NULL && queue_len >= _len)
            {
                memcpy(msg_buf, &msg, _len);
                mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif

                // wake up the sender
                pthread_cond_signal(&session->wake_up_sender);
            }
            else
            {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif
                printf("[C] mg_queue_book failed, freeing memory.\n");
                free(payload);
            }
#endif
        }
    }

    // release the memory
    if (compressed_pixels != NULL)
        free(compressed_pixels);

    if (compressed_mask != NULL)
        free(compressed_mask);
}

void write_ws_video(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const uint8_t *restrict pixels, const uint8_t *restrict mask)
{
    if (session == NULL)
    {
        printf("[C] <write_ws_video> NULL session pointer!\n");
        return;
    }

    if (pixels == NULL || mask == NULL)
    {
        printf("[C] <write_ws_video> NULL pixels || mask!\n");
        return;
    }

    // compress the planes with x265 and pass the response (payload) over to the WebSocket sender
    uint8_t *luma = (uint8_t *)pixels;
    uint8_t *alpha = (uint8_t *)mask;

    // x265 encoding
    pthread_mutex_lock(&session->vid_mtx);

    if ((session->picture == NULL) || (session->encoder == NULL))
    {
        pthread_mutex_unlock(&session->vid_mtx);
        return;
    }

    session->picture->planes[0] = luma;
    session->picture->planes[1] = alpha;

    session->picture->stride[0] = session->image_width;
    session->picture->stride[1] = session->image_width;

    // RGB-encode
    x265_nal *pNals = NULL;
    uint32_t iNal = 0;

    int ret = x265_encoder_encode(session->encoder, &pNals, &iNal, session->picture, NULL);
    printf("[C] x265_encode::ret = %d, #frames = %d\n", ret, iNal);

    for (unsigned int i = 0; i < iNal; i++)
    {
        size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + pNals[i].sizeBytes;

        printf("[C] video_response elapsed: %f [ms], msg_len: %zu bytes.\n", *elapsed, msg_len);

#ifdef MICROWS
        char *payload = NULL;
        size_t ws_len = preamble_ws_frame(&payload, msg_len, WS_FRAME_BINARY);
        msg_len += ws_len;
#else
        char *payload = malloc(msg_len);
#endif

        if (payload != NULL)
        {
            uint32_t id = (unsigned int)(*seq_id);
            uint32_t msg_type = 5;
            // 0 - spectrum, 1 - viewport,
            // 2 - image, 3 - full, spectrum,  refresh,
            // 4 - histogram, 5 - video frame

#ifdef MICROWS
            size_t ws_offset = ws_len;
#else
            size_t ws_offset = 0;
#endif

            memcpy((char *)payload + ws_offset, timestamp, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, pNals[i].payload, pNals[i].sizeBytes);
            ws_offset += pNals[i].sizeBytes;

            if (ws_offset != msg_len)
                printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

            // create a queue message
            struct data_buf msg = {payload, msg_len};

#ifdef DIRECT
            if (!session->disconnect)
                send_all(session, payload, msg_len);
            free(payload);
#else
            char *msg_buf = NULL;
            size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
            pthread_spin_lock(&session->queue_lock);
#else
            pthread_mutex_lock(&session->queue_mtx);
#endif

            // reserve space for the binary message
            size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

            // pass the message over to the sender via a communications queue
            if (msg_buf != NULL && queue_len >= _len)
            {
                memcpy(msg_buf, &msg, _len);
                mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif

                // wake up the sender
                pthread_cond_signal(&session->wake_up_sender);
            }
            else
            {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif
                printf("[C] mg_queue_book failed, freeing memory.\n");
                free(payload);
            }
#endif
        }
    }

    // done with the planes
    session->picture->planes[0] = NULL;
    session->picture->planes[1] = NULL;

    session->picture->stride[0] = 0;
    session->picture->stride[1] = 0;

    pthread_mutex_unlock(&session->vid_mtx);
}

void write_ws_composite_video(websocket_session *session, const int *seq_id, const float *timestamp, const float *elapsed, const uint8_t *restrict pixels, size_t no_pixels)
{
    if (session == NULL)
    {
        printf("[C] <write_ws_composite_video> NULL session pointer!\n");
        return;
    }

    if (pixels == NULL)
    {
        printf("[C] <write_ws_composite_video> NULL pixels!\n");
        return;
    }

    // compress the planes with x265 and pass the response (payload) over to the WebSocket sender
    size_t plane_size = session->image_width * session->image_height;
    int va_count = 0;

    va_count = no_pixels / plane_size;
    if (va_count > 0)
        printf("[C] va_count: %d\n", va_count);

    if (va_count < 1 || va_count > 3)
    {
        printf("[C] <write_ws_composite_video> invalid va_count: %d\n", va_count);
        return;
    }

    // x265 encoding
    pthread_mutex_lock(&session->vid_mtx);

    if ((session->picture == NULL) || (session->encoder == NULL))
    {
        pthread_mutex_unlock(&session->vid_mtx);
        return;
    }

    size_t plane_offset = 0;
    for (int i = 0; i < va_count; i++)
    {
        if (session->picture->planes[i] != NULL)
            free(session->picture->planes[i]);

        session->picture->planes[i] = (uint8_t *)(pixels + plane_offset);
        session->picture->stride[i] = session->image_width;
        plane_offset += plane_size;
    }

    // RGB-encode
    x265_nal *pNals = NULL;
    uint32_t iNal = 0;

    int ret = x265_encoder_encode(session->encoder, &pNals, &iNal, session->picture, NULL);
    printf("[C] x265_encode::ret = %d, #frames = %d\n", ret, iNal);

    for (unsigned int i = 0; i < iNal; i++)
    {
        size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + pNals[i].sizeBytes;

        printf("[C] video_response elapsed: %f [ms], msg_len: %zu bytes.\n", *elapsed, msg_len);

#ifdef MICROWS
        char *payload = NULL;
        size_t ws_len = preamble_ws_frame(&payload, msg_len, WS_FRAME_BINARY);
        msg_len += ws_len;
#else
        char *payload = malloc(msg_len);
#endif

        if (payload != NULL)
        {
            uint32_t id = (unsigned int)(*seq_id);
            uint32_t msg_type = 5;
            // 0 - spectrum, 1 - viewport,
            // 2 - image, 3 - full, spectrum,  refresh,
            // 4 - histogram, 5 - video frame

#ifdef MICROWS
            size_t ws_offset = ws_len;
#else
            size_t ws_offset = 0;
#endif

            memcpy((char *)payload + ws_offset, timestamp, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, pNals[i].payload, pNals[i].sizeBytes);
            ws_offset += pNals[i].sizeBytes;

            if (ws_offset != msg_len)
                printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

            // create a queue message
            struct data_buf msg = {payload, msg_len};

#ifdef DIRECT
            if (!session->disconnect)
                send_all(session, payload, msg_len);
            free(payload);
#else
            char *msg_buf = NULL;
            size_t _len = sizeof(struct data_buf);

#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
            pthread_spin_lock(&session->queue_lock);
#else
            pthread_mutex_lock(&session->queue_mtx);
#endif

            // reserve space for the binary message
            size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

            // pass the message over to the sender via a communications queue
            if (msg_buf != NULL && queue_len >= _len)
            {
                memcpy(msg_buf, &msg, _len);
                mg_queue_add(&session->queue, _len);
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif

                // wake up the sender
                pthread_cond_signal(&session->wake_up_sender);
            }
            else
            {
#if (!defined(__APPLE__) || !defined(__MACH__)) && defined(SPIN)
                pthread_spin_unlock(&session->queue_lock);
#else
                pthread_mutex_unlock(&session->queue_mtx);
#endif
                printf("[C] mg_queue_book failed, freeing memory.\n");
                free(payload);
            }
#endif
        }
    }

    // done with the planes
    for (int i = 0; i < va_count; i++)
    {
        session->picture->planes[i] = NULL;
        session->picture->stride[i] = 0;
    }

    pthread_mutex_unlock(&session->vid_mtx);
}

#endif // MICROWS