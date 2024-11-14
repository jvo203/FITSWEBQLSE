#include <math.h>
#include <string.h>

// LZ4 character streams compressor
#include <lz4hc.h>

#include "http.h"
#include "ws.h"
#include "mjson.h"

#include <microhttpd_ws.h>

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

int close_sessions()
{
    int count = 0;

    if (pthread_mutex_lock(&sessions_mtx) == 0)
    {
        GList *keys = g_hash_table_get_keys(sessions);

        for (GList *it = keys; it != NULL; it = it->next)
        {
            char *key = (char *)it->data;
            websocket_session *session = (websocket_session *)g_hash_table_lookup(sessions, key);

            if (session != NULL)
            {
                g_atomic_rc_box_acquire(session);
                printf("[C] closing a websocket connection for %s/%s\n", session->datasetid, key);

                // send a close WebSocket frame
                // the mongoose event loop is already closed by this point so we cannot use mg_ws_send() here
                // this code path is only valid for the custom microws solution

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

                // remove a session pointer from the hash table
                printf("[C] removing '%s' from the hash table...", key);

                if (g_hash_table_remove(sessions, (gpointer)key))
                    printf("done.\n");
                else
                    printf("cannot remove '%s' from the hash table.\n", key);

                g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
                session = NULL;

                count++;
            }
        }

        g_list_free(keys);

        pthread_mutex_unlock(&sessions_mtx);
    }

    return count;
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

// to be called mainly from FORTRAN
void release_session(websocket_session *session)
{
    if (session != NULL)
        g_atomic_rc_box_release_full(session, (GDestroyNotify)delete_session);
};

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
    pthread_join(session->ws_thread, NULL); // wait for the ws thread to end (this does not work under valgrind, ret = 35 (EDEADLK))

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

    // drop the message queue
    // any remaining items will be freed automatically since a destroy notify function has been set up
    if (session->send_queue != NULL)
        g_async_queue_unref(session->send_queue);

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

    // free() has been commented out on purpose
    // it was interfering with glib reference counting release mechanism (a double free)
    // free(session);
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

            if (session->disconnect)
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

            // pass the session to FORTRAN
            req->session = g_atomic_rc_box_acquire(session);

            if (req->va_count == 1)
                ws_pv_request(req);
            else
                ws_composite_pv_request(req);
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

            if (session->disconnect)
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

            // pass the session to FORTRAN
            req->session = g_atomic_rc_box_acquire(session);
            req->fd = -1; // not used anymore

            // launch a FORTRAN subroutine directly from C, the session will be released and <req> freed from within FORTRAN
            realtime_image_spectrum_request_simd(req);
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

            if (session->disconnect)
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

            // pass the session to FORTRAN
            req->session = g_atomic_rc_box_acquire(session);

            if (req->video_type == single)
                video_request_simd(req);
            else
                composite_video_request_simd(req);
        }
    }

    pthread_mutex_unlock(&session->video_cond_mtx);

    session = NULL;

    printf("[C] video_event_loop terminated.\n");

    pthread_exit(NULL);
}

// taken from mongoose.c
static bool mg_is_url_safe(int c)
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') || c == '.' || c == '_' || c == '-' || c == '~';
}

// taken from mongoose.c
char *mg_hex(const void *buf, size_t len, char *to)
{
    const unsigned char *p = (const unsigned char *)buf;
    const char *hex = "0123456789abcdef";
    size_t i = 0;
    for (; len--; p++)
    {
        to[i++] = hex[p[0] >> 4];
        to[i++] = hex[p[0] & 0x0f];
    }
    to[i] = '\0';
    return to;
}

// taken from mongoose.c
size_t mg_url_encode(const char *s, size_t sl, char *buf, size_t len)
{
    size_t i, n = 0;
    for (i = 0; i < sl; i++)
    {
        int c = *(unsigned char *)&s[i];
        if (n + 4 >= len)
            return 0;
        if (mg_is_url_safe(c))
        {
            buf[n++] = s[i];
        }
        else
        {
            buf[n++] = '%';
            mg_hex(&s[i], 1, &buf[n]);
            n += 2;
        }
    }
    if (len > 0 && n < len - 1)
        buf[n] = '\0'; // Null-terminate the destination
    if (len > 0)
        buf[len - 1] = '\0'; // Always.
    return n;
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
        // URL: http://cluster_ip:ws_port/cluster_heartbeat/id
        GString *url = g_string_new("http://");
        g_string_append_printf(url, "%s:", (char *)iterator->data);
        g_string_append_printf(url, "%" PRIu16 "/cluster_heartbeat/%.*s", options.http_port, (int)len, datasetid);
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