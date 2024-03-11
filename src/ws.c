#include <math.h>
#include <string.h>

// LZ4 character streams compressor
#include <lz4hc.h>

#include "http.h"
#include "ws.h"
#include "mjson.h"

#include <curl/curl.h>
#include "cluster.h"

extern GSList *cluster;
extern GMutex cluster_mtx;

#include "hash_table.h"

GHashTable *sessions = NULL;
pthread_mutex_t sessions_mtx;

extern options_t options; // <options> is defined in main.c
extern sig_atomic_t s_received_signal;
extern int get_header_status(void *item);
extern void inherent_image_dimensions_C(void *item, int *width, int *height);

void *send_cluster_heartbeat(void *arg);

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

websocket_session *new_session(void)
{
    return g_atomic_rc_box_new(websocket_session);
}

void remove_session(websocket_session *session)
{
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (g_hash_table_remove(sessions, (gpointer)session->id))
        {
            printf("[C] removed %s from the hash table\n", session->id);
        }
        else
            printf("[C] cannot remove %s from the hash table\n", session->id);

        pthread_mutex_unlock(&sessions_mtx);

        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    }
    else
        printf("[C] cannot lock sessions_mtx!\n");
}

void delete_session(websocket_session *session)
{
    if (session == NULL)
        return;

    printf("[C] deleting a session for %s/%s\n", session->datasetid, session->id);

    pthread_mutex_lock(&session->vid_mtx);

    if (session->datasetid != NULL)
        free(session->datasetid);
    session->datasetid = NULL;

    if (session->multi != NULL)
        free(session->multi);
    session->multi = NULL;

    if (session->id != NULL)
        free(session->id);
    session->id = NULL;

    if (session->flux != NULL)
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

    pthread_mutex_destroy(&session->stat_mtx);
    pthread_mutex_unlock(&session->vid_mtx);
    pthread_mutex_destroy(&session->vid_mtx);

    // WS spectrum thread
    session->ws_exit = true;
    pthread_cond_signal(&session->ws_cond); // wake up the ws event loop
    pthread_join(session->ws_thread, NULL); // wait for the ws thread to end

    pthread_cond_destroy(&session->ws_cond);
    pthread_mutex_destroy(&session->ws_cond_mtx);

    pthread_mutex_lock(&session->ws_mtx);

    if (session->ws_ring != NULL)
    {
        delete_ring_buffer(session->ws_ring);
        free(session->ws_ring);
        session->ws_ring = NULL;
    }

    pthread_mutex_unlock(&session->ws_mtx);
    pthread_mutex_destroy(&session->ws_mtx);

    // WS video thread
    session->video_exit = true;
    pthread_cond_signal(&session->video_cond); // wake up the video event loop
    pthread_join(session->video_thread, NULL); // wait for the video thread to end

    pthread_cond_destroy(&session->video_cond);
    pthread_mutex_destroy(&session->video_cond_mtx);

    pthread_mutex_lock(&session->video_mtx);

    if (session->video_ring != NULL)
    {
        // drain the video ring buffer and release req->flux
        struct video_request *req = NULL;
        while ((req = (struct video_request *)ring_get(session->video_ring)) != NULL)
        {
            printf("[C] video ring buffer: draining a request seq_id %d.\n", req->seq_id);

            if (req->flux != NULL)
                free(req->flux);

            free(req);
        }

        delete_ring_buffer(session->video_ring);
        free(session->video_ring);
        session->video_ring = NULL;
    }

    pthread_mutex_unlock(&session->video_mtx);
    pthread_mutex_destroy(&session->video_mtx);

    // WS P-V Diagram thread
    session->pv_exit = true;
    pthread_cond_signal(&session->pv_cond); // wake up the pv event loop
    pthread_join(session->pv_thread, NULL); // wait for the pv thread to end

    pthread_cond_destroy(&session->pv_cond);
    pthread_mutex_destroy(&session->cond_mtx);

    pthread_mutex_lock(&session->pv_mtx);

    if (session->pv_ring != NULL)
    {
        delete_ring_buffer(session->pv_ring);
        free(session->pv_ring);
        session->pv_ring = NULL;
    }

    pthread_mutex_unlock(&session->pv_mtx);
    pthread_mutex_destroy(&session->pv_mtx);

#ifdef MICROWS
    pthread_mutex_lock(&session->queue_mtx);

    // drain the message queue
    size_t len = 0;
    char *buf = NULL;

    while ((len = mg_queue_next(&session->queue, &buf)) > 0)
    {
        if (len == sizeof(struct data_buf))
        {
            struct data_buf *msg = (struct data_buf *)buf;

#ifdef DEBUG
            printf("[C] found a message %zu-bytes long, releasing the memory.\n", msg->len);
#endif

            // release memory
            free(msg->buf);
            msg->buf = NULL;
            msg->len = 0;
        }

        mg_queue_del(&session->queue, len); // Remove message from the queue
    }

    // finally release the message queue buffer
    if (session->buf != NULL)
        free(session->buf);
    session->buf = NULL;
    session->buf_len = 0;

    // unlock and destroy the queue_mtx
    pthread_mutex_unlock(&session->queue_mtx);
    pthread_mutex_destroy(&session->queue_mtx);
    pthread_mutex_destroy(&session->wake_up_cond_mtx);
#endif

    // free() has been commented out on purpose
    // it was interfering with glib reference counting release mechanism (a double free)
    // free(session);
}

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

static void mg_http_ws_callback(struct mg_connection *c, int ev, void *ev_data)
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
                websocket_session *session = (websocket_session *)c->fn_data;
                printf("[C] closing a websocket connection for %s/%s\n", session->datasetid, c->data);

                // remove a session pointer from the hash table
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    if (g_hash_table_remove(sessions, (gpointer)c->data))
                    {
                        printf("[C] removed %s from the hash table\n", c->data);
                    }
                    else
                        printf("[C] cannot remove %s from the hash table\n", c->data);

                    pthread_mutex_unlock(&sessions_mtx);

                    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
                }
                else
                    printf("[C] cannot lock sessions_mtx!\n");

                g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
                session = NULL;
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
        if (mg_strstr(hm->uri, mg_str("/submit_progress")) != NULL)
        {
            int progress = 0;

            // parse the binary buffer
            if (hm->body.len == sizeof(progress))
                memcpy(&progress, hm->body.ptr, sizeof(progress));

            char uri[hm->uri.len + 1]; // leave extra space for the string termination character

            // decode the URI
            mg_url_decode(hm->uri.ptr, hm->uri.len, uri, hm->uri.len + 1, 0);

            char *datasetId = strrchr(uri, '/');

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

#ifdef DEBUG
                printf("<progress> POST request for '%s': progress = %d\n", datasetId, progress);
#endif

                void *item = get_dataset(datasetId);

                if (item == NULL)
                {
                    if (dataset_exists(datasetId)) // a <NULL> entry should have been created prior to loading the FITS file
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");
                        break;
                    }
                    else
                    {
                        // signal a catastrophic error
                        mg_http_reply(c, 500, NULL, "Internal Server Error");
                        break;
                    }
                }
                else
                {
                    // check if we've gone past the FITS header stage
                    if (!get_header_status(item))
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");
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

            break;
        }

        if (mg_strstr(hm->uri, mg_str("/heartbeat")) != NULL)
        {
            char uri[hm->uri.len + 1]; // leave extra space for the string termination character

            // decode the URI
            mg_url_decode(hm->uri.ptr, hm->uri.len, uri, hm->uri.len + 1, 0);

            char *datasetId = strrchr(uri, '/');

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

#ifdef DEBUG
                printf("[C] <heartbeat> request for '%s'\n", datasetId);
#endif

                void *item = get_dataset(datasetId);

                if (item != NULL)
                {
                    update_timestamp(item);
                    mg_http_reply(c, 200, NULL, "OK");
                }
                else
                    mg_http_reply(c, 404, "", "Not Found");
            }
            else
                mg_http_reply(c, 404, "", "Not Found");

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

            char uri[hm->uri.len + 1]; // leave extra space for the string termination character

            // decode the URI
            mg_url_decode(hm->uri.ptr, hm->uri.len, uri, hm->uri.len + 1, 0);

            char *datasetId = strrchr(uri, '/');

            if (datasetId != NULL)
            {
                datasetId++; // skip the slash character

#ifdef DEBUG
                printf("<range> POST request for '%s': progress = %d, idx = %d\n", datasetId, progress, idx);
#endif

                void *item = get_dataset(datasetId);

                if (item == NULL)
                {
                    if (dataset_exists(datasetId)) // a <NULL> entry should have been created prior to loading the FITS file
                    {
                        mg_http_reply(c, 202, NULL, "Accepted");
                        break;
                    }
                    else
                    {
                        // signal a catastrophic error
                        // printf("[C] a catastrophic error: cannot find '%s'.\n", datasetId);
                        // mg_http_reply(c, 200, "Content-Type: application/json\r\n", "{\"startindex\":0,\"endindex\":0,\"status\":-2}");
                        mg_http_reply(c, 202, NULL, "Accepted");
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

            break;
        }

        // inner dimensions
        if (mg_strstr(hm->uri, mg_str("/inner")) != NULL)
        {
            char uri[hm->uri.len + 1]; // leave extra space for the string termination character

            // decode the URI
            mg_url_decode(hm->uri.ptr, hm->uri.len, uri, hm->uri.len + 1, 0);

            char *datasetId = strrchr(uri, '/');

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
    case MG_EV_WAKEUP:
    {
#ifdef DEBUG
        printf("[C] MG_EV_WAKEUP\n");
#endif
        struct data_buf *msg = (struct data_buf *)ev_data;

        if (msg != NULL && msg->buf != NULL && msg->len > 0)
        {
#ifdef DEBUG
            printf("[C] MG_EV_WAKEUP received %zu bytes.\n", msg->len);
#endif

            if (c->is_websocket)
            {
#ifdef DEBUG
                printf("[C] found a WebSocket connection, sending %zu bytes.\n", msg->len);
#endif
                mg_ws_send(c, msg->buf, msg->len, WEBSOCKET_OP_BINARY);
            }

            // release memory
            if (msg->buf != NULL)
            {
                free(msg->buf);
                msg->buf = NULL;
                msg->len = 0;
            }
        }

        break;
    }
    case MG_EV_WS_OPEN:
    {
        struct mg_http_message *hm = (struct mg_http_message *)ev_data;
        printf("[C] WEBSOCKET OPEN; URI:\t%.*s\n", (int)hm->uri.len, hm->uri.ptr);

        // extract / validate the datasetid (check if a dataset is in the hash table)
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
            strncpy(c->data, sessionId, sizeof(c->data) - 1); // leave enough space for the string termination character
        }

        char *datasetId = strrchr(uri, '/');
        if (datasetId != NULL)
            datasetId++; // skip the slash character

        if (datasetId != NULL)
            printf("[C] WEBSOCKET DATASETID: '%s'\n", datasetId);

        char *orig = NULL;
        if (datasetId != NULL)
        {
            orig = strdup(datasetId);

            // split the string by ';', get the leading datasetId
            char *ptr = strchr(datasetId, ';');
            if (ptr != NULL)
                *ptr = '\0';
        }

        /*actually do not reject the connections, accept all 'as-is' */
        // reject connections without an entry in a hash table
        if (!dataset_exists(datasetId))
        {
            printf("[C] rejecting the connection for '%s'.\n", datasetId);
            c->is_closing = 1; // Tell mongoose to close this connection
        }
        else
        {
            websocket_session *session = new_session();

            if (session != NULL)
            {
                session->datasetid = datasetId != NULL ? strdup(datasetId) : NULL;
                session->multi = orig != NULL ? strdup(orig) : NULL;
                session->id = sessionId != NULL ? strdup(sessionId) : NULL;
                session->conn_id = c->id;
                session->mgr = c->mgr;

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

                c->fn_data = session;

                // add a session pointer to the hash table
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    g_hash_table_replace(sessions, (gpointer)strdup(c->data), g_atomic_rc_box_acquire(session));
                    pthread_mutex_unlock(&sessions_mtx);

                    printf("[C] inserted %s into the hash table\n", c->data);
                }
                else
                {
                    printf("[C] cannot lock sessions_mtx!\n");
                }

                session = NULL;
            }
        }

        free(orig);
        break;
    }
    case MG_EV_WS_MSG:
    {
        // Got websocket frame. Received data is wm->data. Echo it back!
        struct mg_ws_message *wm = (struct mg_ws_message *)ev_data;

        // simulate a message that caused a segmentation fault
        // substitute the wm->data
        // wm->data = mg_str("{\"type\":\"realtime_image_spectrum\",\"dx\":1500,\"image\":false,\"quality\":\"medium\",\"x1\":-43,\"y1\":828,\"x2\":106,\"y2\":978,\"width\":586,\"height\":586,\"beam\":\"circle\",\"intensity\":\"integrated\",\"frame_start\":93171858567.09592,\"frame_end\":93181171825.4,\"ref_freq\":93173400000,\"seq_id\":2,\"timestamp\":1955.4000000003725}");

        // re-cast the binary data as a text message
        struct mg_str msg = mg_str_n(wm->data.ptr, wm->data.len);

        if (mg_strstr(msg, mg_str("[heartbeat]")) != NULL)
        {
            mg_ws_send(c, wm->data.ptr, wm->data.len, WEBSOCKET_OP_TEXT);

            // get the dataset and update its timestamp
            char *datasetId = NULL;

            websocket_session *session = (websocket_session *)c->fn_data;

            if (session != NULL)
            {
                // tokenize session->multi
                datasetId = session->multi != NULL ? strdup(session->multi) : NULL;
                char *token = datasetId;
                char *rest = token;

                while ((token = strtok_r(rest, ";", &rest)) != NULL)
                {
                    void *item = get_dataset(token);

                    if (item != NULL)
                    {
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
            }

            break;
        }
        else
            printf("[WS] %.*s\n", (int)wm->data.len, wm->data.ptr);

        // get the JSON message type
        char type[32] = "";
        int koff, klen, voff, vlen, vtype, off;

        if (mjson_get_string(wm->data.ptr, (int)wm->data.len, "$.type", type, sizeof(type)) == -1)
            break;

        if (strcmp(type, "composite_pv") == 0)
        {
            websocket_session *session = (websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            if (session->pv_exit)
                break;

            // parse the JSON request
            struct pv_request *req = (struct pv_request *)malloc(sizeof(struct pv_request));

            if (req == NULL)
                break;

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

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                //  printf("key: %.*s, value: %.*s\n", klen, wm->data.ptr + koff, vlen, wm->data.ptr + voff);

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

                // 'frame_start'
                if (strncmp(wm->data.ptr + koff, "\"frame_start\"", klen) == 0)
                    req->frame_start = atof2(wm->data.ptr + voff, vlen);

                // 'frame_end'
                if (strncmp(wm->data.ptr + koff, "\"frame_end\"", klen) == 0)
                    req->frame_end = atof2(wm->data.ptr + voff, vlen);

                // 'ref_freq'
                if (strncmp(wm->data.ptr + koff, "\"ref_freq\"", klen) == 0)
                    req->ref_freq = atof2(wm->data.ptr + voff, vlen);

                // 'deltaV'
                if (strncmp(wm->data.ptr + koff, "\"deltaV\"", klen) == 0)
                    req->deltaV = atof2(wm->data.ptr + voff, vlen);

                // 'rest'
                if (strncmp(wm->data.ptr + koff, "\"rest\"", klen) == 0)
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        req->rest = true;

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    req->seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'timestamp'
                if (strncmp(wm->data.ptr + koff, "\"timestamp\"", klen) == 0)
                    req->timestamp = atof2(wm->data.ptr + voff, vlen);
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

            break;
        }

        // [WS] {"type":"pv","x1":108,"y1":127,"x2":131,"y2":99,"width":1129,"height":801,"frame_start":146830393957.08142,"frame_end":147767129569,"ref_freq":147300000000,"deltaV":0,"rest":false,"timestamp":10713.300000000745}
        if (strcmp(type, "pv") == 0)
        {
            websocket_session *session = (websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            if (session->pv_exit)
                break;

            char *datasetId = session->datasetid;
            void *item = get_dataset(datasetId);

            if (item == NULL)
            {
                printf("[C] cannot find '%s' in the hash table\n", datasetId);
                break;
            }

            update_timestamp(item);

            // parse the JSON request
            struct pv_request *req = (struct pv_request *)malloc(sizeof(struct pv_request));

            if (req == NULL)
                break;

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

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                //  printf("key: %.*s, value: %.*s\n", klen, wm->data.ptr + koff, vlen, wm->data.ptr + voff);

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

                // 'frame_start'
                if (strncmp(wm->data.ptr + koff, "\"frame_start\"", klen) == 0)
                    req->frame_start = atof2(wm->data.ptr + voff, vlen);

                // 'frame_end'
                if (strncmp(wm->data.ptr + koff, "\"frame_end\"", klen) == 0)
                    req->frame_end = atof2(wm->data.ptr + voff, vlen);

                // 'ref_freq'
                if (strncmp(wm->data.ptr + koff, "\"ref_freq\"", klen) == 0)
                    req->ref_freq = atof2(wm->data.ptr + voff, vlen);

                // 'deltaV'
                if (strncmp(wm->data.ptr + koff, "\"deltaV\"", klen) == 0)
                    req->deltaV = atof2(wm->data.ptr + voff, vlen);

                // 'rest'
                if (strncmp(wm->data.ptr + koff, "\"rest\"", klen) == 0)
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        req->rest = true;

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    req->seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'timestamp'
                if (strncmp(wm->data.ptr + koff, "\"timestamp\"", klen) == 0)
                    req->timestamp = atof2(wm->data.ptr + voff, vlen);
            }

            // printf("[C] P-V Diagram request: x1: %d, y1: %d, x2: %d, y2: %d, width: %d, height: %d, frame_start: %f, frame_end: %f, ref_freq: %f, deltaV: %f, rest: %d, timestamp: %f\n", req->x1, req->y1, req->x2, req->y2, req->width, req->height, req->frame_start, req->frame_end, req->ref_freq, req->deltaV, req->rest, req->timestamp);

            pthread_mutex_lock(&session->pv_mtx);

            // add the request to the circular queue
            ring_put(session->pv_ring, req);

            if (!session->pv_exit)
                pthread_cond_signal(&session->pv_cond); // wake up the pv event loop

            // finally unlock the mutex
            pthread_mutex_unlock(&session->pv_mtx);

            break;
        }

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

            websocket_session *session = (websocket_session *)c->fn_data;

            if (session != NULL)
                datasetId = session->datasetid;

            void *item = get_dataset(datasetId);

            if (item != NULL)
            {
                update_timestamp(item);

                int stat;
                int pipefd[2];

                // open a Unix pipe
                stat = pipe(pipefd);

                if (stat == 0)
                {
                    // pass the read end of the pipe to a C thread
                    resp->session_id = strdup(c->data);
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

        // handle CSV spectrum export requests
        if (strcmp(type, "spectrum") == 0)
        {
            struct spectrum_request *req = (struct spectrum_request *)malloc(sizeof(struct image_spectrum_request));

            if (req == NULL)
                break;

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

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // printf("key: '%.*s', value: '%.*s'\n", klen, wm->data.ptr + koff, vlen, wm->data.ptr + voff);

                // 'ra'
                if (strncmp(wm->data.ptr + koff, "\"ra\"", klen) == 0)
                    req->ra = strndup(wm->data.ptr + voff + 1, vlen - 2); // skip the surrounding ""

                // 'dec'
                if (strncmp(wm->data.ptr + koff, "\"dec\"", klen) == 0)
                    req->dec = strndup(wm->data.ptr + voff + 1, vlen - 2); // skip the surrounding ""

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

                // 'deltaV'
                if (strncmp(wm->data.ptr + koff, "\"deltaV\"", klen) == 0)
                    req->deltaV = atof2(wm->data.ptr + voff, vlen);

                // 'rest'
                if (strncmp(wm->data.ptr + koff, "\"rest\"", klen) == 0)
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        req->rest = true;

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    req->seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'timestamp'
                if (strncmp(wm->data.ptr + koff, "\"timestamp\"", klen) == 0)
                    req->timestamp = atof2(wm->data.ptr + voff, vlen);
            }

            printf("[C] CSV spectrum request: ra: %s, dec: %s, x1: %d, y1: %d, x2: %d, y2: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, deltaV: %f, rest: %d, seq_id: %d, timestamp: %f\n", req->ra, req->dec, req->x1, req->y1, req->x2, req->y2, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->deltaV, req->rest, req->seq_id, req->timestamp);

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req->ra);
                free(req->dec);
                free(req);
                break;
            }

            // pass the request to FORTRAN
            char *datasetId = NULL;

            websocket_session *session = (websocket_session *)c->fn_data;

            if (session != NULL)
                datasetId = session->datasetid;

            void *item = get_dataset(datasetId);

            if (item != NULL)
            {
                update_timestamp(item);

                int stat;
                int pipefd[2];

                // open a Unix pipe
                stat = pipe(pipefd);

                if (stat == 0)
                {
                    // pass the read end of the pipe to a C thread
                    resp->session_id = strdup(c->data);
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
                            free(resp->session_id);
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
                        free(resp->session_id);
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
            }
            else
            {
                free(req->ra);
                free(req->dec);
                free(req);
                free(resp);
                printf("[C] cannot find '%s' in the hash table\n", datasetId);
            }

            break;
        }

        // handle real-time spectrum/viewport requests
        if (strcmp(type, "realtime_image_spectrum") == 0)
        {
            websocket_session *session = (websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            if (session->ws_exit)
                break;

            char *datasetId = session->datasetid;
            void *item = get_dataset(datasetId);

            if (item == NULL)
            {
                printf("[C] cannot find '%s' in the hash table\n", datasetId);
                break;
            }

            update_timestamp(item);

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
            req->ptr = item;

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

            // printf("[C] dx: %d, image: %d, quality: %d, x1: %d, y1: %d, x2: %d, y2: %d, width: %d, height: %d, beam: %d, intensity: %d, frame_start: %f, frame_end: %f, ref_freq: %f, seq_id: %d, timestamp: %f\n", req->dx, req->image, req->quality, req->x1, req->y1, req->x2, req->y2, req->width, req->height, req->beam, req->intensity, req->frame_start, req->frame_end, req->ref_freq, req->seq_id, req->timestamp);

            pthread_mutex_lock(&session->ws_mtx);

            // add the request to the circular queue
            ring_put(session->ws_ring, req);

            if (!session->ws_exit)
                pthread_cond_signal(&session->ws_cond); // wake up the ws event loop

            // finally unlock the mutex
            pthread_mutex_unlock(&session->ws_mtx);

            break;

            // the previous code starts here (no ring buffer, prone to overwhelming the server)
            /*struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req);
                break;
            }

            // pass the request to FORTRAN
            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
                // pass the read end of the pipe to a C thread
                resp->session_id = session->id != NULL ? strdup(session->id) : NULL;
                resp->fps = 0;
                resp->bitrate = 0;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                stat = pthread_create(&tid_req, NULL, &realtime_image_spectrum_request_simd, req);

                if (stat == 0)
                {
                    pthread_detach(tid_req);

                    // launch a pipe read C pthread
                    stat = pthread_create(&tid_resp, NULL, &realtime_image_spectrum_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
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

            break;*/
        }

        // init_video
        if (strcmp(type, "init_video") == 0)
        {
            websocket_session *session = (websocket_session *)c->fn_data;

            if (session == NULL)
                break;

            char *datasetId = session->datasetid;
            void *item = get_dataset(datasetId);

            if (item == NULL)
            {

                printf("[C] cannot find '%s' in the hash table\n", datasetId);
                break;
            }

            update_timestamp(item);

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
            if (json != NULL)
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
            break;
        }

        // end_video
        if (strcmp(type, "end_video") == 0)
        {
            websocket_session *session = (websocket_session *)c->fn_data;

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
            websocket_session *session = (websocket_session *)c->fn_data;

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
                break;

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

                // 'fill'
                if (strncmp(wm->data.ptr + koff, "\"fill\"", klen) == 0)
                    req->fill = atoi2(wm->data.ptr + voff, vlen);

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    req->seq_id = atoi2(wm->data.ptr + voff, vlen);

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
                    req->timestamp = atof2(wm->data.ptr + voff, vlen);
            }

            // get the video frame index
            int frame_idx;
            get_spectrum_range_C(item, frame, frame, ref_freq, &frame_idx, &frame_idx);
            req->frame = frame_idx;

            printf("[C]::video fps: %d, bitrate: %d, seq_id: %d, keyframe: %d, frame: {%f --> %d}, ref_freq: %f, timestamp: %f\n", fps, bitrate, req->seq_id, req->keyframe, frame, req->frame, ref_freq, req->timestamp);

            // skip repeated frames
            if (frame_idx == session->last_frame_idx && !req->keyframe)
            {
                printf("[C] skipping a repeat video frame #%d\n", frame_idx);
                free(req); // req->flux is NULL at this point, no need to free it
                break;
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

            break;

            // old code starts here
            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req->flux);
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
                resp->session_id = strdup(c->data);
                resp->fps = fps;
                resp->bitrate = bitrate;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];
                req->ptr = item;

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                stat = pthread_create(&tid_req, NULL, &video_request_simd, req);

                if (stat == 0)
                {
                    pthread_detach(tid_req);

                    // launch a pipe read C pthread
                    stat = pthread_create(&tid_resp, NULL, &video_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
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
                free(req->flux);
                free(req);
                free(resp);
            }

            break;
        }

        if (strcmp(type, "composite_video") == 0)
        {
            websocket_session *common_session = (websocket_session *)c->fn_data;

            if (common_session == NULL)
                break;

            if (common_session->flux == NULL)
                break;

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

            for (off = 0; (off = mjson_next(wm->data.ptr, (int)wm->data.len, off, &koff, &klen, &voff, &vlen, &vtype)) != 0;)
            {
                // 'fps'
                if (strncmp(wm->data.ptr + koff, "\"fps\"", klen) == 0)
                    fps = atoi2(wm->data.ptr + voff, vlen);

                // 'bitrate'
                if (strncmp(wm->data.ptr + koff, "\"bitrate\"", klen) == 0)
                    bitrate = atoi2(wm->data.ptr + voff, vlen);

                // 'fill'
                if (strncmp(wm->data.ptr + koff, "\"fill\"", klen) == 0)
                    fill = atoi2(wm->data.ptr + voff, vlen);

                // 'seq_id'
                if (strncmp(wm->data.ptr + koff, "\"seq_id\"", klen) == 0)
                    seq_id = atoi2(wm->data.ptr + voff, vlen);

                // 'key'
                if (strncmp(wm->data.ptr + koff, "\"key\"", klen) == 0)
                {
                    // false
                    if (strncmp(wm->data.ptr + voff, "false", vlen) == 0)
                        keyframe = false;

                    // true
                    if (strncmp(wm->data.ptr + voff, "true", vlen) == 0)
                        keyframe = true;
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

            struct composite_video_request *req = (struct composite_video_request *)malloc(sizeof(struct composite_video_request));

            if (req == NULL)
                break;

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

                websocket_session *session = NULL;

                // get the session
                if (pthread_mutex_lock(&sessions_mtx) == 0)
                {
                    // iterate through the sessions, find the one with the matching dataset
                    GHashTableIter iter;
                    gpointer key, value;

                    g_hash_table_iter_init(&iter, sessions);

                    while (g_hash_table_iter_next(&iter, &key, &value))
                    {
                        websocket_session *_session = (websocket_session *)value;

                        if (strcmp(_session->datasetid, token) == 0)
                        {
                            session = _session;
                            break;
                        }
                    }

                    pthread_mutex_unlock(&sessions_mtx);
                }

                if (session == NULL)
                    continue;

                // we have the dataset item and the session, prepare the video request
                printf("[C] mg_websocket_callback: preparing video request for dataset '%s'.\n", token);

                // check if the session video tone mapping has been filled already
                // if not, fetch the global data statistics from FORTRAN
                if (isnan(session->dmin) || isnan(session->dmax) || isnan(session->dmedian) || isnan(session->dmadN) || isnan(session->dmadP))
                {
                    printf("[C] calling 'fill_global_statistics(...)'\n");
                    fill_global_statistics(item, &(session->dmin), &(session->dmax), &(session->dmedian), &(session->dmadN), &(session->dmadP));
                }

                // get the video frame index
                int frame_idx;
                get_spectrum_range_C(item, frame, frame, ref_freq, &frame_idx, &frame_idx);

                // skip repeated frames
                if (frame_idx == session->last_frame_idx && !req->keyframe)
                {
                    printf("[C] skipping a repeat video frame #%d\n", frame_idx);
                    skip_frame = true;
                }
                else
                    session->last_frame_idx = frame_idx;

                // RGB
                req->ptr[req->va_count] = item;
                req->frame[req->va_count] = frame_idx;
                req->dmin[req->va_count] = session->dmin;
                req->dmax[req->va_count] = session->dmax;
                req->dmedian[req->va_count] = session->dmedian;
                req->dmadN[req->va_count] = session->dmadN;
                req->dmadP[req->va_count] = session->dmadP;
                req->va_count++; // increment the channel count
            }

            free(datasetId);

            if (skip_frame)
            {
                free(req->flux);
                free(req);
                break;
            }

            pthread_mutex_lock(&common_session->video_mtx);

            // add the request to the circular queue
            ring_put(common_session->video_ring, req);

            if (!common_session->video_exit)
                pthread_cond_signal(&common_session->video_cond); // wake up the video event loop

            // finally unlock the mutex
            pthread_mutex_unlock(&common_session->video_mtx);

            break;

            // old code starts here
            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req->flux);
                free(req);
                break;
            }

            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
#ifdef DEBUG
                printf("[C] mg_websocket_callback: composite video pipes: %d:%d\n", pipefd[0], pipefd[1]);
#endif

                // pass the read end of the pipe to a C thread
                resp->session_id = strdup(c->data);
                resp->fps = fps;
                resp->bitrate = bitrate;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                stat = pthread_create(&tid_req, NULL, &composite_video_request_simd, req);

                if (stat == 0)
                {
                    stat = pthread_detach(tid_req);

                    // launch a pipe read C pthread
                    stat = pthread_create(&tid_resp, NULL, &composite_video_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
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
                free(req->flux);
                free(req);
                free(resp);
            }

            break;
        }

        break;
    }
    default:
        break;
    }
}

void start_ws()
{
    char url[256] = "";
#ifdef MICROWS
    sprintf(url, "ws://0.0.0.0:%d", options.ws_port + 1); // effectively disable the mongoose WS server; using libmicrohttpd instead
#else
    sprintf(url, "ws://0.0.0.0:%d", options.ws_port);
#endif

    struct mg_mgr mgr; // Event manager

    mg_mgr_init(&mgr); // Initialise event manager

#ifdef DEBUG
    mg_log_set(MG_LL_DEBUG);
    printf("Starting WS listener on %s\n", url);
#endif

    mg_http_listen(&mgr, url, mg_http_ws_callback, NULL); // Create HTTP listener
    mg_wakeup_init_handler(&mgr);                         // Initialise wakeup socket pair

    while (s_received_signal == 0)
        mg_mgr_poll(&mgr, 1000); // Event loop. Use 1000ms poll interval

    mg_mgr_free(&mgr);
}

void close_pipe(int fd)
{
    int status;

#ifdef DEBUG
    printf("[C] closing pipe %d.\n", fd);
#endif

    // close a pipe (to be called from Fortran)
    status = close(fd);

    if (0 != status)
        printf("[C] close_pipe status: %d\n", status);
}

void *ws_pv_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct websocket_response *resp = (struct websocket_response *)ptr;

    printf("[C] ws_pv_response: session_id: %s, fd: %d\n", resp->session_id, resp->fd);

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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    // <offset> contains the number of valid bytes in <buf>
    if (offset == 0)
        goto free_pv_mem;

    // get a session based on resp->session_id
    websocket_session *session = NULL;

    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
    {
        printf("[C] ws_pv_response session %s not found.\n", resp->session_id);
        goto free_pv_mem;
    }

    size_t msg_len = sizeof(float) + sizeof(uint32_t) + sizeof(uint32_t) + offset;

#ifdef MICROWS
    char *pv_payload = NULL;
    size_t ws_len = preamble_ws_frame(&pv_payload, msg_len, WS_FRAME_BINARY);
    msg_len += ws_len;
#else
    char *pv_payload = malloc(msg_len);
#endif

    if (pv_payload != NULL)
    {
        float ts = resp->timestamp;
        uint32_t id = resp->seq_id;
        uint32_t msg_type = 7; // P-V diagram

#ifdef MICROWS
        size_t ws_offset = ws_len;
#else
        size_t ws_offset = 0;
#endif

        memcpy((char *)pv_payload + ws_offset, &ts, sizeof(float));
        ws_offset += sizeof(float);

        memcpy((char *)pv_payload + ws_offset, &id, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        memcpy((char *)pv_payload + ws_offset, &msg_type, sizeof(uint32_t));
        ws_offset += sizeof(uint32_t);

        memcpy((char *)pv_payload + ws_offset, buf, offset);
        ws_offset += offset;

        if (ws_offset != msg_len)
            printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

        // create a queue message
        struct data_buf msg = {pv_payload, msg_len};

#ifdef MICROWS
        char *msg_buf = NULL;
        size_t _len = sizeof(struct data_buf);

        pthread_mutex_lock(&session->queue_mtx);

        // reserve space for the binary message
        size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

        // pass the message over to the sender via a communications queue
        if (msg_buf != NULL && queue_len >= _len)
        {
            memcpy(msg_buf, &msg, _len);
            mg_queue_add(&session->queue, _len);
            pthread_mutex_unlock(&session->queue_mtx);

            // wake up the sender
            pthread_cond_signal(&session->wake_up_sender);
        }
        else
        {
            pthread_mutex_unlock(&session->queue_mtx);
            printf("[C] mg_queue_book failed, freeing memory.\n");
            free(pv_payload);
        }
#else
        // pass the message over to mongoose via a communications channel
        bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

        if (!sent)
        {
            printf("[C] mg_wakeup() failed.\n");

            // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
            free(pv_payload);
        };
#endif
    }

    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
free_pv_mem:
    free(buf);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    // <offset> contains the number of valid bytes in <buf>
    if (offset < sizeof(uint32_t))
        goto free_image_spectrum_mem;

    websocket_session *session = NULL;

    // get the session
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
        goto free_image_spectrum_mem;

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
        goto free_image_spectrum_session;

    // pixels
    memcpy(&pixels_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + pixels_len;

    if (offset < read_offset + sizeof(uint32_t))
        goto free_image_spectrum_session;

    // mask
    memcpy(&mask_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + mask_len;

    if (offset < read_offset + sizeof(uint32_t))
        goto free_image_spectrum_session;

    // histogram
    memcpy(&hist_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + hist_len * sizeof(int);

    if (offset < read_offset)
        goto free_image_spectrum_session;

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
        // 2 - image, 3 - full spectrum refresh,
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

        // create a queue message
        struct data_buf msg = {image_payload, msg_len};

#ifdef MICROWS
        char *msg_buf = NULL;
        size_t _len = sizeof(struct data_buf);

        pthread_mutex_lock(&session->queue_mtx);

        // reserve space for the binary message
        size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

        // pass the message over to the sender via a communications queue
        if (msg_buf != NULL && queue_len >= _len)
        {
            memcpy(msg_buf, &msg, _len);
            mg_queue_add(&session->queue, _len);
            pthread_mutex_unlock(&session->queue_mtx);

            // wake up the sender
            pthread_cond_signal(&session->wake_up_sender);
        }
        else
        {
            pthread_mutex_unlock(&session->queue_mtx);
            printf("[C] mg_queue_book failed, freeing memory.\n");
            free(image_payload);
        }
#else
        // pass the message over to mongoose via a communications channel
        bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

        if (!sent)
        {
            printf("[C] mg_wakeup() failed.\n");

            // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
            free(image_payload);
        };
#endif
    }
    else
    {
        // all-or-nothing, skip the spectrum if the image message cannot not be created
        goto free_image_spectrum_session;
    }

    // original spectrum length
    memcpy(&spectrum_len, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t);

    if (offset < read_offset)
        goto free_image_spectrum_session;

    // compressed spectrum
    memcpy(&compressed_size, buf + read_offset, sizeof(uint32_t));
    read_offset += sizeof(uint32_t) + compressed_size;

    if (offset < read_offset)
        goto free_image_spectrum_session;

    printf("[C] offset: %zu, read_offset: %zu\n", offset, read_offset);
    printf("[C] #hist. elements: %u, padding: %d byte(s), orig. spectrum length: %u, compressed_size: %u\n", hist_len, padding, spectrum_len, compressed_size);

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

        // create a queue message
        struct data_buf msg = {spectrum_payload, msg_len};

#ifdef MICROWS
        char *msg_buf = NULL;
        size_t _len = sizeof(struct data_buf);

        pthread_mutex_lock(&session->queue_mtx);

        // reserve space for the binary message
        size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

        // pass the message over to the sender via a communications queue
        if (msg_buf != NULL && queue_len >= _len)
        {
            memcpy(msg_buf, &msg, _len);
            mg_queue_add(&session->queue, _len);
            pthread_mutex_unlock(&session->queue_mtx);

            // wake up the sender
            pthread_cond_signal(&session->wake_up_sender);
        }
        else
        {
            pthread_mutex_unlock(&session->queue_mtx);
            printf("[C] mg_queue_book failed, freeing memory.\n");
            free(spectrum_payload);
        }
#else
        // pass the message over to mongoose via a communications channel
        bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

        if (!sent)
        {
            printf("[C] mg_wakeup() failed.\n");

            // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
            free(spectrum_payload);
        };
#endif
    }

    if (offset == read_offset + 5 * sizeof(float))
    {
        printf("[C] extra video tone mapping information detected.\n");

        // lock the stat mutex
        pthread_mutex_lock(&session->stat_mtx);

        // copy the statistics
        memcpy(&(session->dmin), buf + read_offset, sizeof(float));
        read_offset += sizeof(float);

        memcpy(&(session->dmax), buf + read_offset, sizeof(float));
        read_offset += sizeof(float);

        memcpy(&(session->dmedian), buf + read_offset, sizeof(float));
        read_offset += sizeof(float);

        memcpy(&(session->dmadN), buf + read_offset, sizeof(float));
        read_offset += sizeof(float);

        memcpy(&(session->dmadP), buf + read_offset, sizeof(float));
        read_offset += sizeof(float);

        // unlock the stat mutex
        pthread_mutex_unlock(&session->stat_mtx);
    }

free_image_spectrum_session:
    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
free_image_spectrum_mem:
    free(buf);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}

void *spectrum_response(void *ptr)
{
    if (ptr == NULL)
        pthread_exit(NULL);

    struct websocket_response *resp = (struct websocket_response *)ptr;

    ssize_t n = 0;
    size_t offset = 0;
    size_t buf_size = 0x8000;

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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    // get a session based on resp->session_id
    websocket_session *session = NULL;

    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
    {
        printf("[C] realtime_image_spectrum_response session %s not found.\n", resp->session_id);

        // release the incoming buffer
        free(buf);

        // release the memory
        free(resp->session_id);
        free(resp);

        pthread_exit(NULL);
    }

    // process the received data, prepare a WebSocket response
    if (offset > 0)
    {
        printf("\n%.*s\n", (int)offset, buf);

        char *compressed_csv = NULL;
        int compressed_size, worst_size;

        // LZ4-compress the CSV payload
        worst_size = LZ4_compressBound(offset);

        compressed_csv = (char *)malloc(worst_size);

        if (compressed_csv != NULL)
        {
            // compress CSV as much as possible
            compressed_size = LZ4_compress_HC((const char *)buf, compressed_csv, offset, worst_size, LZ4HC_CLEVEL_MAX);

            printf("[C] CSV length: %zu; compressed: %d bytes\n", offset, compressed_size);

            if (compressed_size > 0)
            {
                size_t msg_len = sizeof(float) + 3 * sizeof(uint32_t) + compressed_size;

                char *payload = malloc(msg_len);

                if (payload != NULL)
                {
                    float ts = resp->timestamp;
                    uint32_t id = 0;
                    uint32_t msg_type = 6;
                    uint32_t payload_len = offset; // the original size

                    size_t ws_offset = 0;

                    memcpy((char *)payload + ws_offset, &ts, sizeof(float));
                    ws_offset += sizeof(float);

                    memcpy((char *)payload + ws_offset, &id, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, &msg_type, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, &payload_len, sizeof(uint32_t));
                    ws_offset += sizeof(uint32_t);

                    memcpy((char *)payload + ws_offset, compressed_csv, compressed_size);
                    ws_offset += compressed_size;

                    if (ws_offset != msg_len)
                        printf("[C] size mismatch! ws_offset: %zu, msg_len: %zu\n", ws_offset, msg_len);

                    // create a queue message
                    struct data_buf msg = {payload, msg_len};

#ifdef MICROWS
                    char *msg_buf = NULL;
                    size_t _len = sizeof(struct data_buf);

                    pthread_mutex_lock(&session->queue_mtx);

                    // reserve space for the binary message
                    size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                    // pass the message over to the sender via a communications queue
                    if (msg_buf != NULL && queue_len >= _len)
                    {
                        memcpy(msg_buf, &msg, _len);
                        mg_queue_add(&session->queue, _len);
                        pthread_mutex_unlock(&session->queue_mtx);

                        // wake up the sender
                        pthread_cond_signal(&session->wake_up_sender);
                    }
                    else
                    {
                        pthread_mutex_unlock(&session->queue_mtx);
                        printf("[C] mg_queue_book failed, freeing memory.\n");
                        free(payload);
                    }
#else
                    // pass the message over to mongoose via a communications channel
                    bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

                    if (!sent)
                    {
                        printf("[C] mg_wakeup() failed.\n");

                        // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                        free(payload);
                    };
#endif
                }
            }

            free(compressed_csv);
        }
    }

    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
    free(buf);

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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    uint32_t length, view_width, view_height;
    uint32_t compressed_size = 0;
    size_t msg_len, view_size;
    float elapsed;

    // get a session based on resp->session_id
    websocket_session *session = NULL;

    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
    {
        printf("[C] realtime_image_spectrum_response session %s not found.\n", resp->session_id);

        // release the incoming buffer
        free(buf);

        // release the memory
        free(resp->session_id);
        free(resp);

        pthread_exit(NULL);
    }

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

                // create a queue message
                struct data_buf msg = {payload, msg_len};

#ifdef MICROWS
                char *msg_buf = NULL;
                size_t _len = sizeof(struct data_buf);

                pthread_mutex_lock(&session->queue_mtx);

                // reserve space for the binary message
                size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                // pass the message over to the sender via a communications queue
                if (msg_buf != NULL && queue_len >= _len)
                {
                    memcpy(msg_buf, &msg, _len);
                    mg_queue_add(&session->queue, _len);
                    pthread_mutex_unlock(&session->queue_mtx);

                    // wake up the sender
                    pthread_cond_signal(&session->wake_up_sender);
                }
                else
                {
                    pthread_mutex_unlock(&session->queue_mtx);
                    printf("[C] mg_queue_book failed, freeing memory.\n");
                    free(payload);
                }
#else
                // pass the message over to mongoose via a communications channel
                bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

                if (!sent)
                {
                    printf("[C] mg_wakeup() failed.\n");

                    // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                    free(payload);
                };
#endif
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

                // create a queue message
                struct data_buf msg = {payload, msg_len};

#ifdef MICROWS
                char *msg_buf = NULL;
                size_t _len = sizeof(struct data_buf);

                pthread_mutex_lock(&session->queue_mtx);

                // reserve space for the binary message
                size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

                // pass the message over to the sender via a communications queue
                if (msg_buf != NULL && queue_len >= _len)
                {
                    memcpy(msg_buf, &msg, _len);
                    mg_queue_add(&session->queue, _len);
                    pthread_mutex_unlock(&session->queue_mtx);

                    // wake up the sender
                    pthread_cond_signal(&session->wake_up_sender);
                }
                else
                {
                    pthread_mutex_unlock(&session->queue_mtx);
                    printf("[C] mg_queue_book failed, freeing memory.\n");
                    free(payload);
                }
#else
                // pass the message over to mongoose via a communications channel
                bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

                if (!sent)
                {
                    printf("[C] mg_wakeup() failed.\n");

                    // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                    free(payload);
                };
#endif
            }
        }
    }

    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
    free(buf);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}

void *composite_video_response(void *ptr)
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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    websocket_session *session = NULL;

    // get the session
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
        goto free_composite_video_mem;

    // compress the planes with x265 and pass the response (payload) over to the WebSocket sender
    size_t plane_size = session->image_width * session->image_height;
    // size_t expected = sizeof(float) + va_count * sizeof(uint8_t) * plane_size;
    int va_count = 0;
    float elapsed;

    va_count = (offset - sizeof(float)) / plane_size;
    if (va_count > 0)
        printf("[C] va_count: %d\n", va_count);

    if (va_count < 1 || va_count > 3)
        goto free_composite_video_session;

    memcpy(&elapsed, buf, sizeof(float));
    printf("[C] elapsed: %f [ms]\n", elapsed);

    // x265 encoding
    pthread_mutex_lock(&session->vid_mtx);

    if ((session->picture == NULL) || (session->encoder == NULL))
    {
        pthread_mutex_unlock(&session->vid_mtx);
        goto free_composite_video_session;
    }

    size_t plane_offset = sizeof(float);
    for (int i = 0; i < va_count; i++)
    {
        if (session->picture->planes[i] != NULL)
            free(session->picture->planes[i]);

        session->picture->planes[i] = (uint8_t *)(buf + plane_offset);
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

            // create a queue message
            struct data_buf msg = {payload, msg_len};

#ifdef MICROWS
            char *msg_buf = NULL;
            size_t _len = sizeof(struct data_buf);

            pthread_mutex_lock(&session->queue_mtx);

            // reserve space for the binary message
            size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

            // pass the message over to the sender via a communications queue
            if (msg_buf != NULL && queue_len >= _len)
            {
                memcpy(msg_buf, &msg, _len);
                mg_queue_add(&session->queue, _len);
                pthread_mutex_unlock(&session->queue_mtx);

                // wake up the sender
                pthread_cond_signal(&session->wake_up_sender);
            }
            else
            {
                pthread_mutex_unlock(&session->queue_mtx);
                printf("[C] mg_queue_book failed, freeing memory.\n");
                free(payload);
            }
#else
            // pass the message over to mongoose via a communications channel
            bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

            if (!sent)
            {
                printf("[C] mg_wakeup() failed.\n");

                // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                free(payload);
            };
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

free_composite_video_session:
    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
free_composite_video_mem:
    free(buf);

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

    // close the read end of the pipe
    close(resp->fd);

    if (0 == n)
        printf("[C] PIPE_END_OF_STREAM\n");

    if (n < 0)
        printf("[C] PIPE_END_WITH_ERROR\n");

    websocket_session *session = NULL;

    // get the session
    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        if (resp->session_id != NULL)
            session = (websocket_session *)g_hash_table_lookup(sessions, (gconstpointer)resp->session_id);

        if (session != NULL)
            g_atomic_rc_box_acquire(session);

        pthread_mutex_unlock(&sessions_mtx);
    }

    if (session == NULL)
        goto free_video_mem;

    // compress the planes with x265 and pass the response (payload) over to the WebSocket sender
    size_t plane_size = session->image_width * session->image_height;
    size_t expected = sizeof(float) + 2 * sizeof(uint8_t) * plane_size;
    float elapsed;

    if (offset != expected)
    {
        printf("[C] video_response::received size mismatch; received %zu, expected %zu bytes.\n", offset, expected);
        goto free_video_session;
    }

    memcpy(&elapsed, buf, sizeof(float));
    uint8_t *luma = (uint8_t *)(buf + sizeof(float));
    uint8_t *alpha = (uint8_t *)(buf + sizeof(float) + plane_size);

    // x265 encoding
    pthread_mutex_lock(&session->vid_mtx);

    if ((session->picture == NULL) || (session->encoder == NULL))
    {
        pthread_mutex_unlock(&session->vid_mtx);
        goto free_video_session;
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

            // create a queue message
            struct data_buf msg = {payload, msg_len};

#ifdef MICROWS
            char *msg_buf = NULL;
            size_t _len = sizeof(struct data_buf);

            pthread_mutex_lock(&session->queue_mtx);

            // reserve space for the binary message
            size_t queue_len = mg_queue_book(&session->queue, &msg_buf, _len);

            // pass the message over to the sender via a communications queue
            if (msg_buf != NULL && queue_len >= _len)
            {
                memcpy(msg_buf, &msg, _len);
                mg_queue_add(&session->queue, _len);
                pthread_mutex_unlock(&session->queue_mtx);

                // wake up the sender
                pthread_cond_signal(&session->wake_up_sender);
            }
            else
            {
                pthread_mutex_unlock(&session->queue_mtx);
                printf("[C] mg_queue_book failed, freeing memory.\n");
                free(payload);
            }
#else
            // pass the message over to mongoose via a communications channel
            bool sent = mg_wakeup(session->mgr, session->conn_id, &msg, sizeof(struct data_buf)); // Wakeup event manager

            if (!sent)
            {
                printf("[C] mg_wakeup() failed.\n");

                // free memory upon a send failure, otherwise memory will be freed in the mongoose pipe event loop
                free(payload);
            };
#endif
        }
    }

    // done with the planes
    session->picture->planes[0] = NULL;
    session->picture->planes[1] = NULL;

    session->picture->stride[0] = 0;
    session->picture->stride[1] = 0;

    pthread_mutex_unlock(&session->vid_mtx);

free_video_session:
    g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
    session = NULL;

    // release the incoming buffer
free_video_mem:
    free(buf);

    // release the memory
    free(resp->session_id);
    free(resp);

    pthread_exit(NULL);
}

void *pv_event_loop(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    websocket_session *session = (websocket_session *)arg;

    printf("[C] pv_event_loop started.\n");

    pthread_mutex_lock(&session->cond_mtx);

    while (!session->pv_exit)
    {
        /* wait on a condition variable */
        pthread_cond_wait(&session->pv_cond, &session->cond_mtx);

        if (session->pv_exit)
            break;

        printf("[C] pv_event_loop::wakeup.\n");

        struct pv_request *req = NULL;
        int last_seq_id = -1;

        // get the requests from the ring buffer
        while ((req = (struct pv_request *)ring_get(session->pv_ring)) != NULL)
        {
            if (session->pv_exit)
            {
                free(req);
                break;
            }

            printf("[C] pv_event_loop::got a request id %d.\n", req->seq_id);

            if (req->seq_id <= last_seq_id)
            {
                printf("[C] pv_event_loop::seq_id mismatch! last_seq_id: %d, req->seq_id: %d\n", last_seq_id, req->seq_id);
                free(req);
                continue;
            }
            else
            {
                last_seq_id = req->seq_id;
            }

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req);
                continue;
            }

            // pass the request to FORTRAN
            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
                // pass the read end of the pipe to a C thread
                resp->session_id = session->id != NULL ? strdup(session->id) : NULL;
                resp->fps = 0;
                resp->bitrate = 0;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                if (session->id != NULL)
                {
                    if (req->va_count == 1)
                        stat = pthread_create(&tid_req, NULL, &ws_pv_request, req);
                    else
                        stat = pthread_create(&tid_req, NULL, &ws_composite_pv_request, req);
                }
                else
                    stat = -1;

                if (stat == 0)
                {
                    // launch a pipe read C pthread
                    stat = pthread_create(&tid_resp, NULL, &ws_pv_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
                    else
                    {
                        // close the read end of the pipe
                        close(pipefd[0]);

                        // release the response memory since there is no reader
                        free(resp->session_id);
                        free(resp);
                    }

                    // finally wait for the request thread to end before handling another one
                    pthread_join(tid_req, NULL);
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
                printf("[C] pv_event_loop::pipe() failed.\n");
                free(req);
                free(resp);
                continue;
            }
        }
    }

    pthread_mutex_unlock(&session->cond_mtx);

    session = NULL;

    printf("[C] pv_event_loop terminated.\n");

    pthread_exit(NULL);
}

void *ws_event_loop(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    websocket_session *session = (websocket_session *)arg;

    printf("[C] ws_event_loop started.\n");

    pthread_mutex_lock(&session->ws_cond_mtx);

    while (!session->ws_exit)
    {
        /* wait on a condition variable */
        pthread_cond_wait(&session->ws_cond, &session->ws_cond_mtx);

        if (session->ws_exit)
            break;

        printf("[C] ws_event_loop::wakeup.\n");

        struct image_spectrum_request *req = NULL;
        int last_seq_id = -1;

        // get the requests from the ring buffer
        while ((req = (struct image_spectrum_request *)ring_get(session->ws_ring)) != NULL)
        {
            if (session->ws_exit)
            {
                free(req);
                break;
            }

            printf("[C] ws_event_loop::got a request id %d.\n", req->seq_id);

            if (req->seq_id <= last_seq_id)
            {
                printf("[C] ws_event_loop::seq_id mismatch! last_seq_id: %d, req->seq_id: %d\n", last_seq_id, req->seq_id);
                free(req);
                continue;
            }
            else
            {
                last_seq_id = req->seq_id;
            }

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req);
                continue;
            }

            // pass the request to FORTRAN
            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
                // pass the read end of the pipe to a C thread
                resp->session_id = session->id != NULL ? strdup(session->id) : NULL;
                resp->fps = 0;
                resp->bitrate = 0;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                if (session->id != NULL)
                    stat = pthread_create(&tid_req, NULL, &realtime_image_spectrum_request_simd, req);
                else
                    stat = -1;

                if (stat == 0)
                {
                    // launch a pipe read C pthread
                    stat = pthread_create(&tid_resp, NULL, &realtime_image_spectrum_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
                    else
                    {
                        // close the read end of the pipe
                        close(pipefd[0]);

                        // release the response memory since there is no reader
                        free(resp->session_id);
                        free(resp);
                    }

                    // finally wait for the request thread to end before handling another one
                    pthread_join(tid_req, NULL);
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
                printf("[C] ws_event_loop::pipe() failed.\n");
                free(req);
                free(resp);
                continue;
            }
        }
    }

    pthread_mutex_unlock(&session->ws_cond_mtx);

    session = NULL;

    printf("[C] ws_event_loop terminated.\n");

    pthread_exit(NULL);
}

void *video_event_loop(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    websocket_session *session = (websocket_session *)arg;

    printf("[C] video_event_loop started.\n");

    pthread_mutex_lock(&session->video_cond_mtx);

    while (!session->video_exit)
    {
        /* wait on a condition variable */
        pthread_cond_wait(&session->video_cond, &session->video_cond_mtx);

        if (session->video_exit)
            break;

        printf("[C] video_event_loop::wakeup.\n");

        struct video_request *req = NULL;
        int last_seq_id = -1;

        // get the requests from the ring buffer
        while ((req = (struct video_request *)ring_get(session->video_ring)) != NULL)
        {
            if (session->video_exit)
            {
                free(req->flux);
                free(req);
                break;
            }

            printf("[C] video_event_loop::got a request seq_id %d.\n", req->seq_id);

            if (req->seq_id <= last_seq_id)
            {
                printf("[C] video_event_loop::seq_id mismatch! last_seq_id: %d, req->seq_id: %d\n", last_seq_id, req->seq_id);
                free(req->flux);
                free(req);
                continue;
            }
            else
            {
                last_seq_id = req->seq_id;
            }

            struct websocket_response *resp = (struct websocket_response *)malloc(sizeof(struct websocket_response));

            if (resp == NULL)
            {
                free(req->flux);
                free(req);
                continue;
            }

            // pass the request to FORTRAN
            int stat;
            int pipefd[2];

            // open a Unix pipe
            stat = pipe(pipefd);

            if (stat == 0)
            {
                // pass the read end of the pipe to a C thread
                resp->session_id = session->id != NULL ? strdup(session->id) : NULL;
                resp->fps = 0;
                resp->bitrate = 0;
                resp->timestamp = req->timestamp;
                resp->seq_id = req->seq_id;
                resp->fd = pipefd[0];

                // pass the write end of the pipe to a FORTRAN thread
                req->fd = pipefd[1];

                pthread_t tid_req, tid_resp;

                // launch a FORTRAN pthread directly from C, <req> will be freed from within FORTRAN
                if (session->id != NULL)
                {
                    // check the video type (single or composite)
                    if (req->video_type == single)
                        stat = pthread_create(&tid_req, NULL, &video_request_simd, req);
                    else
                        stat = pthread_create(&tid_req, NULL, &composite_video_request_simd, req);
                }
                else
                    stat = -1;

                if (stat == 0)
                {
                    // launch a pipe read C pthread
                    // check the video type (single or composite)
                    if (req->video_type == single)
                        stat = pthread_create(&tid_resp, NULL, &video_response, resp);
                    else
                        stat = pthread_create(&tid_resp, NULL, &composite_video_response, resp);

                    if (stat == 0)
                        pthread_detach(tid_resp);
                    else
                    {
                        // close the read end of the pipe
                        close(pipefd[0]);

                        // release the response memory since there is no reader
                        free(resp->session_id);
                        free(resp);
                    }

                    // finally wait for the request thread to end before handling another one
                    pthread_join(tid_req, NULL);
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
                printf("[C] video_event_loop::pipe() failed.\n");
                free(req->flux);
                free(req);
                free(resp);
                continue;
            }
        }
    }

    pthread_mutex_unlock(&session->video_cond_mtx);

    session = NULL;

    printf("[C] video_event_loop terminated.\n");

    pthread_exit(NULL);
}

void *send_cluster_heartbeat(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    char *datasetId = (char *)arg;

    int i;
    GSList *iterator = NULL;

    g_mutex_lock(&cluster_mtx);

    int handle_count = g_slist_length(cluster);

    if (handle_count == 0)
    {
        // printf("[C] aborting send_cluster_heartbeat (no cluster nodes found)\n");
        g_mutex_unlock(&cluster_mtx);

        // release memory
        free(datasetId);
        pthread_exit(NULL);
    };

    CURL *handles[handle_count];
    CURLM *multi_handle;

    int still_running = 1; /* keep number of running handles */

    CURLMsg *msg;  /* for picking up messages with the transfer status */
    int msgs_left; /* how many messages are left */

    /* Allocate one CURL handle per transfer */
    for (i = 0; i < handle_count; i++)
        handles[i] = curl_easy_init();

    /* init a multi stack */
    multi_handle = curl_multi_init();

    // html-encode the datasetId
    char datasetid[2 * strlen(datasetId)];
    size_t len = mg_url_encode(datasetId, strlen(datasetId), datasetid, sizeof(datasetid) - 1);

    for (i = 0, iterator = cluster; iterator; iterator = iterator->next)
    {
        // URL: http://cluster_ip:ws_port/heartbeat/id
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/heartbeat/%.*s", options.ws_port, (int)len, datasetid);
        // printf("[C] URL: '%s'\n", url->str);

        // set the individual URL
        curl_easy_setopt(handles[i], CURLOPT_URL, url->str);

        // ignore the response body
        curl_easy_setopt(handles[i], CURLOPT_NOBODY, 1);

        // add the individual transfer
        curl_multi_add_handle(multi_handle, handles[i]);

        g_string_free(url, TRUE);

        // move on to the next cluster node
        i++;
    }

    g_mutex_unlock(&cluster_mtx);

    // release memory
    free(datasetId);

    /* Wait for the transfers */
    while (still_running)
    {
        CURLMcode mc = curl_multi_perform(multi_handle, &still_running);

        if (still_running)
            /* wait for activity, timeout or "nothing" */
            mc = curl_multi_poll(multi_handle, NULL, 0, 1000, NULL);

        if (mc)
            break;
    }

    /* See how the transfers went */
    while ((msg = curl_multi_info_read(multi_handle, &msgs_left)))
    {
        if (msg->msg == CURLMSG_DONE)
        {
            long response_code = 0;
            curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code);

#ifdef DEBUG
            printf("[C] HTTP transfer completed; cURL status %d, HTTP code %ld.\n", msg->data.result, response_code);
#endif
        }
    }

    /* remove the transfers and cleanup the handles */
    for (i = 0; i < handle_count; i++)
    {
        curl_multi_remove_handle(multi_handle, handles[i]);
        curl_easy_cleanup(handles[i]);
    }

    curl_multi_cleanup(multi_handle);

    pthread_exit(NULL);
}