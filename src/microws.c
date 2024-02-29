#include <microhttpd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef MICROWS
#include <microhttpd_ws.h>

#define PAGE_INVALID_WEBSOCKET_REQUEST "Invalid WebSocket request!"

enum MHD_Result
on_dummy_connection(void *cls,
                    struct MHD_Connection *connection,
                    const char *url,
                    const char *method,
                    const char *version,
                    const char *upload_data,
                    size_t *upload_data_size,
                    void **ptr)
{
    (void)cls; // silence gcc warnings

    static int dummy;
    struct MHD_Response *response;
    int ret;

    if (0 != strcmp(method, "GET"))
        return MHD_NO; /* unexpected method */

    if (&dummy != *ptr)
    {
        /* The first time only the headers are valid,
           do not respond in the first round... */
        *ptr = &dummy;
        return MHD_YES;
    }

    if (0 != *upload_data_size)
        return MHD_NO; /* upload data in a GET!? */

    *ptr = NULL; /* clear context pointer */

    const char *page = "200 OK";
    response = MHD_create_response_from_buffer(strlen(page),
                                               (void *)page,
                                               MHD_RESPMEM_PERSISTENT);
    ret = MHD_queue_response(connection,
                             MHD_HTTP_OK,
                             response);
    MHD_destroy_response(response);

    return ret;
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
    struct ConnectedUser *cu;
    pthread_t pt;
    (void)cls;        /* Unused. Silent compiler warning. */
    (void)connection; /* Unused. Silent compiler warning. */
    (void)req_cls;    /* Unused. Silent compiler warning. */

    /* This callback must return as soon as possible. */

    return;
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

    if (1 == is_valid)
    {
        /* create the response for upgrade */
        response = MHD_create_response_for_upgrade(&upgrade_handler,
                                                   NULL);

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