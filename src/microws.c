#include <microhttpd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "ws.h"
#include "hash_table.h"

#ifdef MICROWS
#include <microhttpd_ws.h>

#define PAGE_INVALID_WEBSOCKET_REQUEST "Invalid WebSocket request!"

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

        /*actually do not reject the connections, accept all 'as-is' */
        // reject connections without an entry in a hash table
        if (!dataset_exists(datasetId))
        {
            printf("[C] rejecting the WebSocket connection for '%s'.\n", datasetId);
            is_valid = 0;
        }
        else if (is_valid)
        {
            printf("[C] accepting the WebSocket connection for '%s'.\n", datasetId);

            session = new_session();

            if (session != NULL)
            {
                // datasetId != NULL && sessionId != NULL
                session->datasetid = strdup(datasetId);
                session->multi = orig != NULL ? strdup(orig) : NULL;
                session->id = strdup(sessionId);
                session->conn_id = -1;
                session->mgr = NULL;

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

                // hold on to the session pointer, pass it to the WebSocket upgrade handler
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

#endif // MICROWS