#include <string.h>

#include "http.h"
#include "ws.h"
#include "mjson.h"

#include "hash_table.h"

extern options_t options; // <options> is defined in main.c
extern sig_atomic_t s_received_signal;
extern int get_header_status(void *item);
extern void inherent_image_dimensions_C(void *item, int *width, int *height);

char *append_null(const char *chars, const int size)
{
    char *tmp = (char *)malloc(size + 1);

    memcpy(tmp, chars, size);
    tmp[size] = '\0';

    return tmp;
}

int atoi2(const char *chars, const int size)
{
    char *tmp = append_null(chars, size);

    int result = atoi(tmp);

    free(tmp);

    return result;
}

double atof2(const char *chars, const int size)
{
    char *tmp = append_null(chars, size);

    double result = atof(tmp);

    free(tmp);

    return result;
}

// needs to be global so that various connection handlers can get access to it
static struct mg_connection *udp_pipe; // Used to wake up event manager

static void mg_http_ws_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    switch (ev)
    {
    case MG_EV_OPEN:
    {
        // c->is_hexdumping = 1;
        break;
    }
    case MG_EV_CLOSE:
    {
        if (c->is_websocket)
        {
            printf("WEBSOCKET CONNECTION CLOSED.\n");
            printf("closing a websocket connection for %s/%s\n", (char *)c->fn_data, c->label);
        }

        // free any user data <c->fn_data>
        if (c->fn_data != NULL)
        {
            free(c->fn_data);
            c->fn_data = NULL;
        }

        break;
    }
    case MG_EV_HTTP_MSG:
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;

        // MG_INFO(("New request to: [%.*s], body size: %lu", (int)hm->uri.len, hm->uri.ptr, (unsigned long)hm->body.len));

        // open a WebSocket request
        if (mg_strstr(hm->uri, mg_str("/websocket")) != NULL)
        {
            printf("ACCEPTED WEBSOCKET URI:\t%.*s\n", (int)hm->uri.len, hm->uri.ptr);

            // extract the datasetId

            // set the <c->fn_data> to datasetId

            // Upgrade to websocket. From now on, a connection is a full-duplex
            // Websocket connection, which will receive MG_EV_WS_MSG events.
            mg_ws_upgrade(c, hm, NULL);
            break;
        }

        // a simple progress update
        if (mg_strstr(hm->uri, mg_str("/progress")) != NULL)
        {
            int progress = 0;

            // parse the binary buffer
            if (hm->body.len == sizeof(progress))
                memcpy(&progress, hm->body.ptr, sizeof(progress));

            char *tmp = strndup(hm->uri.ptr, hm->uri.len);
            char *datasetId = strrchr(tmp, '/');

            /*if (datasetId != NULL)
                printf("<progress> POST request for '%s': progress = %d\n", datasetId + 1, progress);*/

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

                void *item = get_dataset(datasetId);

                if (item == NULL)
                {
                    if (dataset_exists(datasetId)) // a <NULL> entry should have been created prior to loading the FITS file
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");
                        free(tmp);
                        break;
                    }
                    else
                    {
                        // signal a catastrophic error
                        mg_http_reply(c, 500, NULL, "Internal Server Error");
                        free(tmp);
                        break;
                    }
                }
                else
                {
                    // check if we've gone past the FITS header stage
                    if (!get_header_status(item))
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");
                        free(tmp);
                        break;
                    }
                }

                // submit the POST progress to FORTRAN
                if (progress > 0)
                    update_progress_C(item, progress);

                mg_http_reply(c, 200, NULL, "OK");
            }
            else
                mg_http_reply(c, 400, NULL, "Bad Request");

            free(tmp);
            break;
        }

        // a FITS channel range combined PUT/GET request
        if (mg_strstr(hm->uri, mg_str("/range")) != NULL)
        {
            int progress = 0;
            int idx = 0;

            // read the binary buffer
            const char *data = hm->body.ptr;
            size_t size = hm->body.len;

            size_t offset = 0;
            size_t expected_size = sizeof(int);

            if (size >= sizeof(progress))
            {
                memcpy(&progress, data, sizeof(progress));
                offset += sizeof(progress);
            }

            if (progress > 0)
                expected_size += sizeof(int) + 5 * progress * sizeof(float);

            // extract the FORTRAN arrays
            float *frame_min = NULL;
            float *frame_max = NULL;
            float *frame_median = NULL;
            float *mean_spectrum = NULL;
            float *integrated_spectrum = NULL;

            if (size != expected_size)
                printf("[C] ERROR expected %zu, received %zu bytes.\n", expected_size, size);
            else if (progress > 0)
            {
                // idx
                memcpy(&idx, data + offset, sizeof(idx));
                offset += sizeof(progress);
                idx++; // convert a C index into a FORTRAN array index

                // frame_min
                frame_min = (float *)(data + offset);
                offset += progress * sizeof(float);

                // frame_max
                frame_max = (float *)(data + offset);
                offset += progress * sizeof(float);

                // frame_median
                frame_median = (float *)(data + offset);
                offset += progress * sizeof(float);

                // mean_spectrum
                mean_spectrum = (float *)(data + offset);
                offset += progress * sizeof(float);

                // integrated_spectrum
                integrated_spectrum = (float *)(data + offset);
                offset += progress * sizeof(float);
            }

            char *tmp = strndup(hm->uri.ptr, hm->uri.len);
            char *datasetId = strrchr(tmp, '/');

            if (datasetId != NULL)
                printf("<range> POST request for '%s': progress = %d, idx = %d\n", datasetId + 1, progress, idx);

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

                void *item = get_dataset(datasetId);

                if (item == NULL)
                {
                    if (dataset_exists(datasetId)) // a <NULL> entry should have been created prior to loading the FITS file
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");

                        free(tmp);
                        break;
                    }
                    else
                    {
                        // signal a catastrophic error
                        mg_http_reply(c, 200, "Content-Type: application/json\r\n", "{\"startindex\":0,\"endindex\":0,\"status\":-2}");

                        free(tmp);
                        break;
                    }
                }

                // submit the POST data arrays to FORTRAN
                if (progress > 0)
                    submit_channel_range(item, idx, progress, frame_min, frame_max, frame_median, mean_spectrum, integrated_spectrum);

                char *json = NULL;
                int start, end, status;

                // get the channel range from FORTRAN
                get_channel_range_C(item, progress, &start, &end, &status);

                mjson_printf(mjson_print_dynamic_buf, &json, "{%Q:%d,%Q:%d,%Q:%d}", "startindex", start, "endindex", end, "status", status);
                mg_http_reply(c, 200, "Content-Type: application/json\r\n", "%s", json);

                free(json);
            }
            else
                mg_http_reply(c, 400, NULL, "Bad Request");

            free(tmp);
            break;
        }

        // inner dimensions
        if (mg_strstr(hm->uri, mg_str("/inner")) != NULL)
        {
            char *tmp = strndup(hm->uri.ptr, hm->uri.len);
            char *datasetId = strrchr(tmp, '/');

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

                void *item = get_dataset(datasetId);

                if (item == NULL)
                {
                    // signal a catastrophic error
                    mg_http_reply(c, 500, NULL, "Internal Server Error");
                }
                else
                {
                    char *json = NULL;
                    int width, height;

                    inherent_image_dimensions_C(item, &width, &height);

                    // reply with JSON ...
                    mjson_printf(mjson_print_dynamic_buf, &json, "{%Q:%d,%Q:%d}", "width", width, "height", height);
                    mg_http_reply(c, 200, "Content-Type: application/json\r\n", "%s", json);

                    free(json);
                }
            }
            else
                mg_http_reply(c, 400, NULL, "Bad Request");

            free(tmp);
            break;
        }

        // all else
        {
            printf("rejecting the connection.\n");

            c->is_closing = 1; // Tell mongoose to close this connection

            // reject the connection
            mg_http_reply(c, 404, "", "Rejected");
        }

        break;
    }
    case MG_EV_WS_OPEN:
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        printf("[C] WEBSOCKET OPEN; URI:\t%.*s\n", (int)hm->uri.len, hm->uri.ptr);

        // extract / validate the datasetid (check if a dataset is in the hash table)
        // use <mg_url_decode()>
        char uri[hm->uri.len + 1]; // leave extra space for the string termination character

        // decode the URI
        mg_url_decode(hm->uri.ptr, hm->uri.len, uri, hm->uri.len + 1, 0);

        char *sessionId = strrchr(uri, '/');
        if (sessionId != NULL)
        {
            // zero-out the slash (get rid of it) to cancel the session id part
            *sessionId = '\0';

            sessionId++; // skip the slash character

            printf("[C] WEBSOCKET SESSIONID: '%s'\n", sessionId);

            // copy the session id into the connection custom label
            strncpy(c->label, sessionId, sizeof(c->label) - 1); // leave enough space for the string termination character
        }

        char *datasetId = strrchr(uri, '/');
        if (datasetId != NULL)
            datasetId++; // skip the slash character

        printf("[C] WEBSOCKET DATASETID: '%s'\n", datasetId);

        // split the string by ';', get the leading datasetId
        char *ptr = strchr(datasetId, ';');
        if (ptr != NULL)
            *ptr = '\0';

        // reject connections without an entry in a hash table
        if (!dataset_exists(datasetId))
            c->is_closing = 1; // Tell mongoose to close this connection
        else
            c->fn_data = strdup(datasetId);

        break;
    }
    case MG_EV_WS_MSG:
    {
        // Got websocket frame. Received data is wm->data. Echo it back!
        struct mg_ws_message *wm = (struct mg_ws_message *)ev_data;

        // re-cast the binary data as a text message
        struct mg_str msg = mg_str_n(wm->data.ptr, wm->data.len);

        if (mg_strstr(msg, mg_str("[heartbeat]")) != NULL)
        {
            mg_ws_send(c, wm->data.ptr, wm->data.len, WEBSOCKET_OP_TEXT);
            break;
        }
        else
            printf("[WS] %.*s\n", (int)wm->data.len, wm->data.ptr);

        // get the JSON message type
        char type[32];
        int koff, klen, voff, vlen, vtype, off;

        if (mjson_get_string(wm->data.ptr, (int)wm->data.len, "$.type", type, sizeof(type)) == -1)
            break;

        // handle real-time spectrum/viewport requests
        if (strcmp(type, "realtime_image_spectrum") == 0)
        {
            struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

            if (req == NULL)
                break;

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
            req->seq_id = 0;
            req->timestamp = 0.0;
            req->fd = -1;
            req->ptr = NULL;

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // printf("key: %.*s, value: %.*s\n", klen, wm->data.ptr + koff, vlen, wm->data.ptr + voff);

                // 'dx'
                if (strncmp(wm->data.ptr + koff, "\"dx\"", klen) == 0)
                    req->dx = atoi2(wm->data.ptr + voff, vlen);

                // 'image'
                if (strncmp(wm->data.ptr + koff, "\"image\"", klen) == 0)
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        req->image = true;

                // 'quality'
                if (strncmp(wm->data.ptr + koff, "\"quality\"", klen) == 0)
                {
                    // low
                    if (strncmp(wm->data.ptr + voff, "\"low\"", vlen) == 0)
                        req->quality = low;

                    // medium
                    if (strncmp(wm->data.ptr + voff, "\"medium\"", vlen) == 0)
                        req->quality = medium;

                    // high
                    if (strncmp(wm->data.ptr + voff, "\"heigh\"", vlen) == 0)
                        req->quality = high;
                }

                // 'x1'
                if (strncmp(wm->data.ptr + koff, "\"x1\"", klen) == 0)
                    req->x1 = atoi2(wm->data.ptr + voff, vlen);

                // 'y1'
                if (strncmp(wm->data.ptr + koff, "\"y1\"", klen) == 0)
                    req->y1 = atoi2(wm->data.ptr + voff, vlen);

                // 'x2'
                if (strncmp(wm->data.ptr + koff, "\"x2\"", klen) == 0)
                    req->x2 = atoi2(wm->data.ptr + voff, vlen);

                // 'y2'
                if (strncmp(wm->data.ptr + koff, "\"y2\"", klen) == 0)
                    req->y2 = atoi2(wm->data.ptr + voff, vlen);

                // 'width'
                if (strncmp(wm->data.ptr + koff, "\"width\"", klen) == 0)
                    req->width = atoi2(wm->data.ptr + voff, vlen);

                // 'height'
                if (strncmp(wm->data.ptr + koff, "\"height\"", klen) == 0)
                    req->height = atoi2(wm->data.ptr + voff, vlen);

                // 'beam'
                if (strncmp(wm->data.ptr + koff, "\"beam\"", klen) == 0)
                {
                    // circle
                    if (strncmp(wm->data.ptr + voff, "\"circle\"", vlen) == 0)
                        req->beam = circle;

                    // square
                    if (strncmp(wm->data.ptr + voff, "\"square\"", vlen) == 0)
                        req->beam = square;
                }

                // 'intensity'
                if (strncmp(wm->data.ptr + koff, "\"intensity\"", klen) == 0)
                {
                    // mean
                    if (strncmp(wm->data.ptr + voff, "\"mean\"", vlen) == 0)
                        req->intensity = mean;

                    // integrated
                    if (strncmp(wm->data.ptr + voff, "\"integrated\"", vlen) == 0)
                        req->intensity = integrated;
                }

                // 'frame_start'
                if (strncmp(wm->data.ptr + koff, "\"frame_start\"", klen) == 0)
                    req->frame_start = atof2(wm->data.ptr + voff, vlen);

                // 'frame_end'
                if (strncmp(wm->data.ptr + koff, "\"frame_end\"", klen) == 0)
                    req->frame_end = atof2(wm->data.ptr + voff, vlen);

                // 'ref_freq'
                if (strncmp(wm->data.ptr + koff, "\"ref_freq\"", klen) == 0)
                    req->ref_freq = atof2(wm->data.ptr + voff, vlen);

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    req->seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'timestamp'
                if (strncmp(wm->data.ptr + koff, "\"timestamp\"", klen) == 0)
                    req->timestamp = atof2(wm->data.ptr + voff, vlen);
            }

            // printf("[C] dx: %d, image: %d, quality: %d, x1: %d, y1: %d, x2: %d, y2: %d, width: %d, height: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, seq_id: %d, timestamp: %f\n", req.dx, req.image, req.quality, req.x1, req.y1, req.x2, req.y2, req.width, req.height, req.beam, req.intensity, req.frame_start, req.frame_end, req.ref_freq, req.seq_id, req.timestamp);

            struct image_spectrum_response *resp = (struct image_spectrum_response *)malloc(sizeof(struct image_spectrum_response));

            if (resp == NULL)
            {
                free(req);
                break;
            }

            // pass the request to FORTRAN
            char *datasetId = (char *)fn_data;
            void *item = get_dataset(datasetId);

            if (item != NULL)
            {
                int stat;
                int pipefd[2];

                // open a Unix pipe
                stat = pipe(pipefd);

                if (stat == 0)
                {
                    // pass the read end of the pipe to a C thread
                    resp->session_id = strdup(c->label);
                    resp->timestamp = req->timestamp;
                    resp->seq_id = req->seq_id;
                    resp->fd = pipefd[0];

                    // pass the write end of the pipe to a FORTRAN thread
                    req->fd = pipefd[1];
                    req->ptr = item;

                    pthread_t tid;

                    // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                    stat = pthread_create(&tid, NULL, &realtime_image_spectrum_request_simd, req);

                    if (stat == 0)
                    {
                        pthread_detach(tid);

                        // launch a pipe read C pthread
                        stat = pthread_create(&tid, NULL, &realtime_image_spectrum_response, resp);

                        if (stat == 0)
                            pthread_detach(tid);
                        else
                        {
                            // close the read end of the pipe
                            close(pipefd[0]);

                            // release the response memory since there is no reader
                            free(resp->session_id);
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
                        free(resp->session_id);
                        free(resp);
                    }
                }
                else
                {
                    free(req);
                    free(resp);
                }
            }
            else
            {
                free(req);
                printf("[C] cannot find '%s' in the hash table\n", datasetId);
            }
        }

        break;
    }
    default:
        break;
    }
}

// Pipe event handler
static void mg_pipe_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_READ)
    {
        if (c->recv.len != sizeof(struct websocket_message))
        {
            printf("[C] mg_pipe_callback::abort!\n");
            return;
        }

        struct websocket_message *msg = (struct websocket_message *)c->recv.buf;

        struct mg_connection *t;
        for (t = c->mgr->conns; t != NULL; t = t->next)
        {
            /*if (t->label[0] != 'W')
                continue; // Ignore un-marked connections
            mg_http_reply(t, 200, "Host: foo.com\r\n", "%.*s\n", c->recv.len,
                          c->recv.buf); // Respond!
            t->label[0] = 0;            // Clear mark*/
        }

        // release memory
        free(msg->session_id);
        free(msg->buf);
    }
}

void start_ws()
{
    char url[256] = "";
    sprintf(url, "ws://0.0.0.0:%d", options.ws_port);

    struct mg_mgr mgr; // Event manager

    mg_mgr_init(&mgr); // Initialise event manager
    // mg_log_set("3");
    printf("Starting WS listener on %s\n", url);

    udp_pipe = mg_mkpipe(&mgr, mg_pipe_callback, NULL);   // Create pipe
    mg_http_listen(&mgr, url, mg_http_ws_callback, NULL); // Create HTTP listener

    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Infinite event loop

    mg_mgr_free(&mgr);
}

extern void close_pipe(int fd)
{
    int status;

    // close a pipe (to be called from Fortran)
    status = close(fd);

    if (0 != status)
        printf("[C] close_pipe status: %d\n", status);
}

void *realtime_image_spectrum_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct image_spectrum_response *resp = (struct image_spectrum_response *)ptr;

    ssize_t n = 0;
    size_t offset = 0;
    size_t buf_size = 0x2000;

    char *buf = malloc(buf_size);

    if (buf != NULL)
        while ((n = read(resp->fd, buf + offset, buf_size - offset)) > 0)
        {
            offset += n;

            printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

            if (offset == buf_size)
            {
                printf("[C] OFFSET == BUF_SIZE, re-sizing the buffer\n");

                size_t new_size = buf_size << 1;
                char *tmp = realloc(buf, new_size);

                if (tmp != NULL)
                {
                    buf = tmp;
                    buf_size = new_size;
                }
            }
        }

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    uint32_t length, view_width, view_height;
    uint32_t compressed_size;
    size_t msg_len, view_size;

    // process the received data, prepare WebSocket response(s)
    if (offset > 8)
    {
        memcpy(&length, buf, sizeof(uint32_t));
        memcpy(&compressed_size, buf + 4, sizeof(uint32_t));
        msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + compressed_size;

        printf("[C] spectrum length: %u, compressed_size: %u, msg_len: %zu\n", length, compressed_size, msg_len);

        char *payload = malloc(msg_len);

        if (payload != NULL)
        {
            float ts = resp->timestamp;
            uint32_t id = resp->seq_id;
            uint32_t msg_type = 0;
            // 0 - spectrum, 1 - viewport,
            // 2 - image, 3 - full, spectrum,  refresh,
            // 4 - histogram
            float elapsed = 0.0f;

            size_t ws_offset = 0;

            memcpy((char *)payload + ws_offset, &ts, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
            ws_offset += sizeof(uint32_t);

            memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
            ws_offset += sizeof(float);

            memcpy((char *)payload + ws_offset, buf + 8, compressed_size);
            ws_offset += compressed_size;

            // create a UDP message
            struct websocket_message msg = {strdup(resp->session_id), payload, ws_offset};

            // pass the message over to mongoose via a UDP pipe (a memory pointer? or data?)
            mg_mgr_wakeup(udp_pipe, &msg, sizeof(struct websocket_message)); // Wakeup event manager
        }
    }

    // then check if there is a viewport available too

    // release the incoming buffer
    if (buf != NULL)
        free(buf);

    // close the read end of the pipe
    close(resp->fd);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}