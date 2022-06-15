#include <math.h>
#include <string.h>

#include "http.h"
#include "ws.h"
#include "mjson.h"

#include "hash_table.h"

static GHashTable *sessions;
pthread_mutex_t sessions_mtx;

extern options_t options; // <options> is defined in main.c
extern sig_atomic_t s_received_signal;
extern int get_header_status(void *item);
extern void inherent_image_dimensions_C(void *item, int *width, int *height);

void init_session_table()
{
    if (pthread_mutex_init(&sessions_mtx, NULL) != 0)
    {
        perror("sessions mutex_init error");
        exit(1);
    }

    sessions = g_hash_table_new_full(g_str_hash, g_str_equal, free, NULL);
}

void delete_session_table()
{
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        g_hash_table_destroy(sessions);
        pthread_mutex_unlock(&sessions_mtx);
    }

    pthread_mutex_destroy(&sessions_mtx);
}

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
static int channel = -1; // Used to wake up event manager

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
        // only WebSocket sessions allocate memory for user data
        if (c->is_websocket)
        {

            if (c->fn_data != NULL)
            {
                struct websocket_session *session = (struct websocket_session *)c->fn_data;
                printf("closing a websocket connection for %s/%s\n", session->datasetid, c->label);

                // remove a session pointer from the hash table
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    if (g_hash_table_remove(sessions, (gpointer)c->label))
                        printf("[C] removed %s from the hash table\n", c->label);
                    pthread_mutex_unlock(&sessions_mtx);
                }
                else
                    printf("[C] cannot lock sessions_mtx!\n");

                pthread_mutex_lock(&session->vid_mtx);

                free(session->datasetid);
                session->datasetid = NULL;

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
                pthread_mutex_destroy(&session->vid_mtx);

                free(session);

                c->fn_data = NULL;
            }

            printf("WEBSOCKET CONNECTION CLOSED.\n");
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
                        // printf("[C] a catastrophic error: cannot find '%s'.\n", datasetId);
                        // mg_http_reply(c, 200, "Content-Type: application/json\r\n", "{\"startindex\":0,\"endindex\":0,\"status\":-2}");
                        mg_http_reply(c, 202, NULL, "Accepted");

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
        {
            struct websocket_session *session = (struct websocket_session *)malloc(sizeof(struct websocket_session));

            if (session != NULL)
            {
                session->datasetid = strdup(datasetId);
                session->flux = NULL;
                session->dmin = NAN;
                session->dmax = NAN;
                session->dmedian = NAN;
                session->dmadN = NAN;
                session->dmadP = NAN;
                session->image_width = 0;
                session->image_height = 0;
                session->bDownsize = false;

                pthread_mutex_init(&session->vid_mtx, NULL);
                session->last_frame_idx = -1;
                session->param = NULL;
                session->encoder = NULL;
                session->picture = NULL;

                c->fn_data = session;

                // add a session pointer to the hash table
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    g_hash_table_replace(sessions, (gpointer)strdup(c->label), session);
                    pthread_mutex_unlock(&sessions_mtx);

                    printf("[C] inserted %s into the hash table\n", c->label);
                }
                else
                    printf("[C] cannot lock sessions_mtx!\n");
            }
        }

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

        // [WS] {"type":"image","dx":1462,"width":1541.5999755859375,"height":794,"quality":"medium","intensity":"integrated","frame_start":344401602984.4286,"frame_end":344629439356.3494,"ref_freq":345115000000,"timestamp":8141.999999999999}
        if (strcmp(type, "image") == 0)
        {
            struct image_spectrum_request *req = (struct image_spectrum_request *)malloc(sizeof(struct image_spectrum_request));

            if (req == NULL)
                break;

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

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // printf("key: %.*s, value: %.*s\n", klen, wm->data.ptr + koff, vlen, wm->data.ptr + voff);

                // 'dx'
                if (strncmp(wm->data.ptr + koff, "\"dx\"", klen) == 0)
                    req->dx = atoi2(wm->data.ptr + voff, vlen);

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

                // 'width'
                if (strncmp(wm->data.ptr + koff, "\"width\"", klen) == 0)
                    req->width = atoi2(wm->data.ptr + voff, vlen);

                // 'height'
                if (strncmp(wm->data.ptr + koff, "\"height\"", klen) == 0)
                    req->height = atoi2(wm->data.ptr + voff, vlen);

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

            // printf("[C] dx: %d, quality: %d, width: %d, height: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, seq_id: %d, timestamp: %f\n", req->dx, req->quality, req->width, req->height, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->seq_id, req->timestamp);

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req);
                break;
            }

            // pass the request to FORTRAN
            char *datasetId = NULL;

            struct websocket_session *session = (struct websocket_session *)c->fn_data;

            if (session != NULL)
                datasetId = session->datasetid;

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
                    resp->fps = 0;
                    resp->bitrate = 0;
                    resp->timestamp = req->timestamp;
                    resp->seq_id = req->seq_id;
                    resp->fd = pipefd[0];

                    // pass the write end of the pipe to a FORTRAN thread
                    req->fd = pipefd[1];
                    req->ptr = item;

                    pthread_t tid;

                    // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                    stat = pthread_create(&tid, NULL, &ws_image_spectrum_request, req);

                    if (stat == 0)
                    {
                        pthread_detach(tid);

                        // launch a pipe read C pthread
                        stat = pthread_create(&tid, NULL, &ws_image_spectrum_response, resp);

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

            break;
        }

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
            req->median = NAN;
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

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req);
                break;
            }

            // pass the request to FORTRAN
            char *datasetId = NULL;

            struct websocket_session *session = (struct websocket_session *)c->fn_data;

            if (session != NULL)
                datasetId = session->datasetid;

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
                    resp->fps = 0;
                    resp->bitrate = 0;
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

            break;
        }

        // init_video
        if (strcmp(type, "init_video") == 0)
        {
            struct websocket_session *session = (struct websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            char *datasetId = session->datasetid;
            void *item = get_dataset(datasetId);

            if (item == NULL)
            {

                printf("[C] cannot find '%s' in the hash table\n", datasetId);
                break;
            }

            // read the parameters
            // last_video_seq
            int width = 0;
            int height = 0;
            int fps = 30; // hard-code the initial FPS
            int bitrate = 1000;

            int fits_width, fits_height, inner_width, inner_height;
            float scale;

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // 'width'
                if (strncmp(wm->data.ptr + koff, "\"width\"", klen) == 0)
                    width = atoi2(wm->data.ptr + voff, vlen);

                // 'height'
                if (strncmp(wm->data.ptr + koff, "\"height\"", klen) == 0)
                    height = atoi2(wm->data.ptr + voff, vlen);

                // 'flux'
                if (strncmp(wm->data.ptr + koff, "\"flux\"", klen) == 0)
                {
                    free(session->flux);
                    session->flux = strndup(wm->data.ptr + voff + 1, vlen - 2); // avoid the enclosing double quotes
                }

                // 'fps'
                /*if (strncmp(wm->data.ptr + koff, "\"fps\"", klen) == 0)
                    fps = atoi2(wm->data.ptr + voff, vlen);*/

                // 'bitrate'
                if (strncmp(wm->data.ptr + koff, "\"bitrate\"", klen) == 0)
                    bitrate = atoi2(wm->data.ptr + voff, vlen);
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
            mg_ws_send(c, json, strlen(json), WEBSOCKET_OP_TEXT);

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
                memset(B_buf, 128, frame_size);

            picture->planes[0] = NULL;
            picture->planes[1] = NULL;
            picture->planes[2] = B_buf;

            picture->stride[0] = 0;
            picture->stride[1] = 0;
            picture->stride[2] = session->image_width;

            session->picture = picture;

        unlock_mutex_and_break:
            pthread_mutex_unlock(&session->vid_mtx);
            break;
        }

        // end_video
        if (strcmp(type, "end_video") == 0)
        {
            struct websocket_session *session = (struct websocket_session *)c->fn_data;

            if (session == NULL)
                break;

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

            break;
        }

        // encode and stream video
        if (strcmp(type, "video") == 0)
        {
            struct websocket_session *session = (struct websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            if (session->flux == NULL)
                break;

            char *datasetId = session->datasetid;
            void *item = get_dataset(datasetId);

            if (item == NULL)
            {

                printf("[C] cannot find '%s' in the hash table\n", datasetId);
                break;
            }

            // check if the session video tone mapping has been filled already
            // if not, fetch the global data statistics from FORTRAN
            if (isnan(session->dmin) || isnan(session->dmax) || isnan(session->dmedian) || isnan(session->dmadN) || isnan(session->dmadP))
            {
                printf("[C] calling 'fill_global_statistics(...)'\n");
                fill_global_statistics(item, &(session->dmin), &(session->dmax), &(session->dmedian), &(session->dmadN), &(session->dmadP));
            }

            struct video_request *req = (struct video_request *)malloc(sizeof(struct video_request));

            if (req == NULL)
                break;

            int fps = 30;
            int bitrate = 1000;
            req->keyframe = false; // is it a keyframe?
            int seq_id = -1;

            req->frame = 0;
            float timestamp = 0.0;

            req->flux = NULL;
            req->len = 0;

            req->dmin = session->dmin;
            req->dmax = session->dmax;
            req->dmedian = session->dmedian;
            req->dmadN = session->dmadN;
            req->dmadP = session->dmadP;

            req->width = session->image_width;
            req->height = session->image_height;
            req->downsize = session->bDownsize;
            req->fd = -1;
            req->ptr = item;

            double frame = 0.0;
            double ref_freq = 0.0;

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // 'fps'
                if (strncmp(wm->data.ptr + koff, "\"fps\"", klen) == 0)
                    fps = atoi2(wm->data.ptr + voff, vlen);

                // 'bitrate'
                if (strncmp(wm->data.ptr + koff, "\"bitrate\"", klen) == 0)
                    bitrate = atoi2(wm->data.ptr + voff, vlen);

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'key'
                if (strncmp(wm->data.ptr + koff, "\"key\"", klen) == 0)
                {
                    // false
                    if (strncmp(wm->data.ptr + voff, "false", vlen) == 0)
                        req->keyframe = false;

                    // true
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        req->keyframe = true;
                }

                // 'frame'
                if (strncmp(wm->data.ptr + koff, "\"frame\"", klen) == 0)
                    frame = atof2(wm->data.ptr + voff, vlen);

                // 'ref_freq'
                if (strncmp(wm->data.ptr + koff, "\"ref_freq\"", klen) == 0)
                    ref_freq = atof2(wm->data.ptr + voff, vlen);

                // 'timestamp'
                if (strncmp(wm->data.ptr + koff, "\"timestamp\"", klen) == 0)
                    timestamp = atof2(wm->data.ptr + voff, vlen);
            }

            // get the video frame index
            int frame_idx;
            get_spectrum_range_C(item, frame, frame, ref_freq, &frame_idx, &frame_idx);
            req->frame = frame_idx;

            printf("[C]::video fps: %d, bitrate: %d, seq_id: %d, keyframe: %d, frame: {%f --> %d}, ref_freq: %f, timestamp: %f\n", fps, bitrate, seq_id, req->keyframe, frame, req->frame, ref_freq, timestamp);

            // skip repeated frames
            if (frame_idx == session->last_frame_idx && !req->keyframe)
            {
                printf("[C] skipping a repeat video frame #%d\n", frame_idx);
                free(req); // req->flux is NULL at this point, no need to free it
                break;
            }
            else
                session->last_frame_idx = frame_idx;

            req->flux = strdup(session->flux);
            req->len = strlen(req->flux);

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req->flux); // req->flux is *NOT* NULL at this point
                free(req);
                break;
            }

            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
                // pass the read end of the pipe to a C thread
                resp->session_id = strdup(c->label);
                resp->fps = fps;
                resp->bitrate = bitrate;
                resp->timestamp = timestamp;
                resp->seq_id = seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];
                req->ptr = item;

                pthread_t tid;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                stat = pthread_create(&tid, NULL, &video_request_simd, req);

                if (stat == 0)
                {
                    pthread_detach(tid);

                    // launch a pipe read C pthread
                    stat = pthread_create(&tid, NULL, &video_response, resp);

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
                    free(req->flux);
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
                free(req->flux); // req->flux is *NOT* NULL at this point
                free(req);
                free(resp);
            }
        }

        break;
    }
    default:
        break;
    }

    (void)fn_data;
}

// Pipe event handler
static void mg_pipe_callback(struct mg_connection *c, int ev, void *ev_data, void *fn_data)
{
    if (ev == MG_EV_READ)
    {
        int i, n;
        size_t offset;

        n = c->recv.len / sizeof(struct websocket_message);
        // printf("[C] mg_pipe_callback: received %d binary message(s).\n", n);

        for (offset = 0, i = 0; i < n; i++)
        {
            struct websocket_message *msg = (struct websocket_message *)(c->recv.buf + offset);
            offset += sizeof(struct websocket_message);

            if (msg->len == 0)
            {
                printf("[C] mg_pipe_callback::continue (an empty message buffer)!\n");
                free(msg->session_id);
                free(msg->buf);
                continue;
            }

            struct mg_connection *t;
            for (t = c->mgr->conns; t != NULL; t = t->next)
            {
                // do not bother comparing strings for non-WebSocket connections
                if (t->is_websocket && (strcmp(t->label, msg->session_id) == 0))
                {
                    // printf("[C] found a WebSocket connection, sending %zu bytes.\n", msg->len);
                    mg_ws_send(t, msg->buf, msg->len, WEBSOCKET_OP_BINARY);
                    break;
                }
            }

            // release memory
            free(msg->session_id);
            free(msg->buf);
        }

        c->recv.len = 0; // Tell Mongoose we've consumed data
    }

    (void)ev_data;
    (void)fn_data;
}

void start_ws()
{
    char url[256] = "";
    sprintf(url, "ws://0.0.0.0:%d", options.ws_port);

    struct mg_mgr mgr; // Event manager

    mg_mgr_init(&mgr); // Initialise event manager
    // mg_log_set("3");
    printf("Starting WS listener on %s\n", url);

    channel = mg_mkpipe(&mgr, mg_pipe_callback, NULL, false); // Create pipe
    mg_http_listen(&mgr, url, mg_http_ws_callback, NULL);     // Create HTTP listener

    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Infinite event loop

    mg_mgr_free(&mgr);

    // close the commucation channel
    if (channel != -1)
        close(channel);
}

extern void close_pipe(int fd)
{
    int status;

    // close a pipe (to be called from Fortran)
    status = close(fd);

    if (0 != status)
        printf("[C] close_pipe status: %d\n", status);
}

void *ws_image_spectrum_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct websocket_response *resp = (struct websocket_response *)ptr;

    ssize_t n = 0;
    size_t offset = 0;
    size_t buf_size = 0x40000;

    char *buf = malloc(buf_size);

    if (buf != NULL)
        while ((n = read(resp->fd, buf + offset, buf_size - offset)) > 0)
        {
            offset += n;

            // printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

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

    // <offset> contains the number of valid bytes in <buf>
    if (offset < sizeof(uint32_t))
        goto free_mem;

    size_t read_offset = 0; // a 'read' cursor into <buf>

    size_t msg_len = 0;

    // image + histogram
    uint32_t flux_len = 0;
    uint32_t pixels_len = 0;
    uint32_t mask_len = 0;
    uint32_t hist_len = 0;

    // spectrum
    uint32_t spectrum_len = 0;
    uint32_t compressed_size = 0;

    memcpy(&flux_len, buf + read_offset, sizeof(uint32_t));

    read_offset += sizeof(uint32_t) + flux_len + 7 * sizeof(float) + 2 * sizeof(uint32_t);

    if (offset < read_offset + sizeof(uint32_t))
        goto free_mem;

    // pixels
    memcpy(&pixels_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + pixels_len;

    if (offset < read_offset + sizeof(uint32_t))
        goto free_mem;

    // mask
    memcpy(&mask_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + mask_len;

    if (offset < read_offset + sizeof(uint32_t))
        goto free_mem;

    // histogram
    memcpy(&hist_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + hist_len * sizeof(int);

    if (offset < read_offset)
        goto free_mem;

    int padding = 4 - read_offset % 4;

    size_t write_offset = 0;
    msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + read_offset + padding;
    char *image_payload = malloc(msg_len);

    if (image_payload != NULL)
    {
        float ts = resp->timestamp;
        uint32_t id = resp->seq_id;
        uint32_t msg_type = 2;
        // 0 - spectrum, 1 - viewport,
        // 2 - image, 3 - full, spectrum,  refresh,
        // 4 - histogram

        size_t ws_offset = 0;

        memcpy((char *)image_payload + ws_offset, &ts, sizeof(float));
        ws_offset += sizeof(float);

        memcpy((char *)image_payload + ws_offset, &id, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        memcpy((char *)image_payload + ws_offset, &msg_type, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        // fill-in the content up to pixels/mask
        write_offset = sizeof(uint32_t) + flux_len + 7 * sizeof(float) + 2 * sizeof(uint32_t) + sizeof(uint32_t) + pixels_len + sizeof(uint32_t) + mask_len;
        memcpy((char *)image_payload + ws_offset, buf, write_offset);
        ws_offset += write_offset;

        // add an optional padding
        if (padding > 0)
        {
            char extra[padding];

            // fill-in the padding with zeroes
            memset(extra, 0, padding);

            memcpy((char *)image_payload + ws_offset, extra, padding);
            ws_offset += padding;
        }

        // and the histogram
        memcpy((char *)image_payload + ws_offset, buf + write_offset, sizeof(uint32_t) + hist_len * sizeof(int));
        ws_offset += sizeof(uint32_t) + hist_len * sizeof(int);
        write_offset += sizeof(uint32_t) + hist_len * sizeof(int);

        if (ws_offset != msg_len)
            printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

        // create a UDP message
        struct websocket_message msg = {strdup(resp->session_id), image_payload, msg_len};

        // pass the message over to mongoose via a communications channel
        ssize_t sent = send(channel, &msg, sizeof(struct websocket_message), 0); // Wakeup event manager

        if (sent != sizeof(struct websocket_message))
        {
            printf("[C] only sent %zd bytes instead of %zu.\n", sent, sizeof(struct websocket_message));

            // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
            free(msg.session_id);
            free(image_payload);
        };
    }
    else
    {
        // all-or-nothing, skip the spectrum if the image message cannot not be created
        goto free_mem;
    }

    // original spectrum length
    memcpy(&spectrum_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t);

    if (offset < read_offset)
        goto free_mem;

    // compressed spectrum
    memcpy(&compressed_size, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + compressed_size;

    if (offset < read_offset)
        goto free_mem;

    printf("[C] offset: %zu, read_offset: %zu\n", offset, read_offset);
    printf("[C] got here; #hist. elements: %u, padding: %d byte(s), orig. spectrum length: %u, compressed_size: %u\n", hist_len, padding, spectrum_len, compressed_size);

    msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(uint32_t) + compressed_size;
    char *spectrum_payload = malloc(msg_len);

    if (spectrum_payload != NULL)
    {
        float ts = resp->timestamp;
        uint32_t id = resp->seq_id;
        uint32_t msg_type = 3;
        // 0 - spectrum, 1 - viewport,
        // 2 - image, 3 - full, spectrum,  refresh,
        // 4 - histogram

        size_t ws_offset = 0;

        memcpy((char *)spectrum_payload + ws_offset, &ts, sizeof(float));
        ws_offset += sizeof(float);

        memcpy((char *)spectrum_payload + ws_offset, &id, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        memcpy((char *)spectrum_payload + ws_offset, &msg_type, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        memcpy((char *)spectrum_payload + ws_offset, &spectrum_len, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        write_offset += 2 * sizeof(uint32_t); // skip the first 2 ints; get to the compressed data
        memcpy((char *)spectrum_payload + ws_offset, buf + write_offset, compressed_size);
        ws_offset += compressed_size;

        if (ws_offset != msg_len)
            printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

        // create a UDP message
        struct websocket_message msg = {strdup(resp->session_id), spectrum_payload, msg_len};

        // pass the message over to mongoose via a communications channel
        ssize_t sent = send(channel, &msg, sizeof(struct websocket_message), 0); // Wakeup event manager

        if (sent != sizeof(struct websocket_message))
        {
            printf("[C] only sent %zd bytes instead of %zu.\n", sent, sizeof(struct websocket_message));

            // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
            free(msg.session_id);
            free(spectrum_payload);
        };
    }

    // release the incoming buffer
free_mem:
    free(buf);

    // close the read end of the pipe
    close(resp->fd);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}

void *realtime_image_spectrum_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct websocket_response *resp = (struct websocket_response *)ptr;

    ssize_t n = 0;
    size_t offset = 0;
    size_t buf_size = 0x2000;

    char *buf = malloc(buf_size);

    if (buf != NULL)
        while ((n = read(resp->fd, buf + offset, buf_size - offset)) > 0)
        {
            offset += n;

            // printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

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
    float elapsed;

    // process the received data, prepare WebSocket response(s)
    if (offset >= 2 * sizeof(uint32_t) + sizeof(float))
    {
        memcpy(&elapsed, buf, sizeof(float));
        memcpy(&length, buf + sizeof(float), sizeof(uint32_t));
        memcpy(&compressed_size, buf + sizeof(float) + sizeof(uint32_t), sizeof(uint32_t));

        // respond with the spectrum only if the spectrum data is available, else proceed to the viewport part
        if (length > 0 && compressed_size > 0)
        {
            msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float) + sizeof(uint32_t) + compressed_size;

            printf("[C] spectrum length: %u, elapsed: %f [ms], compressed_size: %u, msg_len: %zu bytes.\n", length, elapsed, compressed_size, msg_len);

            char *payload = malloc(msg_len);

            if (payload != NULL)
            {
                float ts = resp->timestamp;
                uint32_t id = resp->seq_id;
                uint32_t msg_type = 0;
                // 0 - spectrum, 1 - viewport,
                // 2 - image, 3 - full, spectrum,  refresh,
                // 4 - histogram

                size_t ws_offset = 0;

                memcpy((char *)payload + ws_offset, &ts, sizeof(float));
                ws_offset += sizeof(float);

                memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
                ws_offset += sizeof(uint32_t);

                memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
                ws_offset += sizeof(uint32_t);

                memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
                ws_offset += sizeof(float);

                memcpy((char *)payload + ws_offset, &length, sizeof(uint32_t));
                ws_offset += sizeof(uint32_t);

                memcpy((char *)payload + ws_offset, buf + 2 * sizeof(uint32_t) + sizeof(float), compressed_size);
                ws_offset += compressed_size;

                if (ws_offset != msg_len)
                    printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

                // create a UDP message
                struct websocket_message msg = {strdup(resp->session_id), payload, msg_len};

                // pass the message over to mongoose via a communications channel
                ssize_t sent = send(channel, &msg, sizeof(struct websocket_message), 0); // Wakeup event manager

                if (sent != sizeof(struct websocket_message))
                {
                    printf("[C] only sent %zd bytes instead of %zu.\n", sent, sizeof(struct websocket_message));

                    // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                    free(msg.session_id);
                    free(payload);
                };
            }
        }
    }

    // check if there is an optional viewport too
    size_t base = 2 * sizeof(uint32_t) + sizeof(float) + compressed_size;
    if (offset > base + 2 * sizeof(uint32_t))
    {
        memcpy(&view_width, buf + base, sizeof(uint32_t));
        memcpy(&view_height, buf + base + sizeof(uint32_t), sizeof(uint32_t));
        view_size = offset - base;

        if (view_width > 0 && view_height > 0)
        {
            // header
            msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + sizeof(float);
            // body
            msg_len += view_size;

            printf("[C] viewport elapsed: %f [ms], processing %ux%u viewport, msg_len = %zu\n", elapsed, view_width, view_height, msg_len);

            char *payload = malloc(msg_len);

            if (payload != NULL)
            {
                float ts = resp->timestamp;
                uint32_t id = resp->seq_id;
                uint32_t msg_type = 1;
                // 0 - spectrum, 1 - viewport,
                // 2 - image, 3 - full, spectrum,  refresh,
                // 4 - histogram

                size_t ws_offset = 0;

                memcpy((char *)payload + ws_offset, &ts, sizeof(float));
                ws_offset += sizeof(float);

                memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
                ws_offset += sizeof(uint32_t);

                memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
                ws_offset += sizeof(uint32_t);

                memcpy((char *)payload + ws_offset, &elapsed, sizeof(float));
                ws_offset += sizeof(float);

                memcpy((char *)payload + ws_offset, buf + base, view_size);
                ws_offset += view_size;

                if (ws_offset != msg_len)
                    printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

                // create a UDP message
                struct websocket_message msg = {strdup(resp->session_id), payload, msg_len};

                // pass the message over to mongoose via a communications channel
                ssize_t sent = send(channel, &msg, sizeof(struct websocket_message), 0); // Wakeup event manager

                if (sent != sizeof(struct websocket_message))
                {
                    printf("[C] only sent %zd bytes instead of %zu.\n", sent, sizeof(struct websocket_message));

                    // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                    free(msg.session_id);
                    free(payload);
                };
            }
        }
    }

    // release the incoming buffer
    free(buf);

    // close the read end of the pipe
    close(resp->fd);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}

void *video_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct websocket_response *resp = (struct websocket_response *)ptr;

    ssize_t n = 0;
    size_t offset = 0;
    size_t buf_size = 0x40000;

    char *buf = malloc(buf_size);

    if (buf != NULL)
        while ((n = read(resp->fd, buf + offset, buf_size - offset)) > 0)
        {
            offset += n;

            // printf("[C] PIPE_RECV %zd BYTES, OFFSET: %zu, buf_size: %zu\n", n, offset, buf_size);

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

    struct websocket_session *session = NULL;

    // get the session
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        session = (struct websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);
        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
        goto free_mem;

    // TO-DO - compress the planes with x265 and pass the response (payload) over to mongoose
    size_t plane_size = session->image_width * session->image_height;
    size_t expected = sizeof(float) + 2 * sizeof(uint8_t) * plane_size;
    float elapsed;

    if (offset != expected)
    {
        printf("[C] video_response::received size mismatch; received %zu, expected %zu bytes.\n", offset, expected);
        goto free_mem;
    }

    memcpy(&elapsed, buf, sizeof(float));
    uint8_t *luma = (uint8_t *)(buf + sizeof(float));
    uint8_t *alpha = (uint8_t *)(buf + sizeof(float) + plane_size);

    // x265 encoding
    pthread_mutex_lock(&session->vid_mtx);

    if ((session->picture == NULL) || (session->encoder == NULL))
    {
        pthread_mutex_unlock(&session->vid_mtx);
        goto free_mem;
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

        printf("[C] video_response elapsed: %f [ms], msg_len: %zu bytes.\n", elapsed, msg_len);

        char *payload = malloc(msg_len);

        if (payload != NULL)
        {
            float ts = resp->timestamp;
            uint32_t id = resp->seq_id;
            uint32_t msg_type = 5;
            // 0 - spectrum, 1 - viewport,
            // 2 - image, 3 - full, spectrum,  refresh,
            // 4 - histogram, 5 - video frame

            size_t ws_offset = 0;

            memcpy((char *)payload + ws_offset, &ts, sizeof(float));
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

            // create a UDP message
            struct websocket_message msg = {strdup(resp->session_id), payload, msg_len};

            // pass the message over to mongoose via a communications channel
            ssize_t sent = send(channel, &msg, sizeof(struct websocket_message), 0); // Wakeup event manager

            if (sent != sizeof(struct websocket_message))
            {
                printf("[C] only sent %zd bytes instead of %zu.\n", sent, sizeof(struct websocket_message));

                // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                free(msg.session_id);
                free(payload);
            };
        }
    }

    // done with the planes
    session->picture->planes[0] = NULL;
    session->picture->planes[1] = NULL;

    session->picture->stride[0] = 0;
    session->picture->stride[1] = 0;

    pthread_mutex_unlock(&session->vid_mtx);

    // release the incoming buffer
free_mem:
    free(buf);

    // close the read end of the pipe
    close(resp->fd);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}