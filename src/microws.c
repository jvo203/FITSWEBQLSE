#include <microhttpd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define PAGE_INVALID_WEBSOCKET_REQUEST "Invalid WebSocket request!"

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

static enum MHD_Result
my_ws_connection(void *cls,
                 struct MHD_Connection *connection,
                 const char *url,
                 const char *method,
                 const char *version,
                 const char *upload_data,
                 size_t *upload_data_size,
                 void **ptr)
{
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
     * To ease this example we skip the following checks:
     * - Whether the HTTP version is 1.1 or newer
     * - Whether Connection is Upgrade, because this header may
     *   contain multiple values.
     * - The requested Host (because we don't know)
     */

    enum MHD_Result ret;

    /* check whether a websocket upgrade is requested */
    const char *value = MHD_lookup_connection_value(connection,
                                                    MHD_HEADER_KIND,
                                                    MHD_HTTP_HEADER_UPGRADE);
    if ((0 == value) || (0 != strcasecmp(value, "websocket")))
    {
        struct MHD_Response *response = MHD_create_response_from_buffer(strlen(
                                                                            PAGE_INVALID_WEBSOCKET_REQUEST),
                                                                        (void *)PAGE_INVALID_WEBSOCKET_REQUEST,
                                                                        MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_BAD_REQUEST,
                                 response);
        MHD_destroy_response(response);
        return ret;
    }

    /* check the protocol version */
    value = MHD_lookup_connection_value(connection,
                                        MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_SEC_WEBSOCKET_VERSION);
    if ((0 == value) || (0 != strcasecmp(value, "13")))
    {
        struct MHD_Response *response = MHD_create_response_from_buffer(strlen(
                                                                            PAGE_INVALID_WEBSOCKET_REQUEST),
                                                                        (void *)PAGE_INVALID_WEBSOCKET_REQUEST,
                                                                        MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_BAD_REQUEST,
                                 response);
        MHD_destroy_response(response);
        return ret;
    }

    /* read the websocket key (required for the response) */
    value = MHD_lookup_connection_value(connection, MHD_HEADER_KIND,
                                        MHD_HTTP_HEADER_SEC_WEBSOCKET_KEY);
    if (0 == value)
    {
        struct MHD_Response *response = MHD_create_response_from_buffer(strlen(
                                                                            PAGE_INVALID_WEBSOCKET_REQUEST),
                                                                        (void *)PAGE_INVALID_WEBSOCKET_REQUEST,
                                                                        MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_BAD_REQUEST,
                                 response);
        MHD_destroy_response(response);
        return ret;
    }

    /* generate the response accept header */
    char sec_websocket_accept[29];
    if (0 != MHD_websocket_create_accept(value, sec_websocket_accept))
    {
        struct MHD_Response *response = MHD_create_response_from_buffer(strlen(
                                                                            PAGE_INVALID_WEBSOCKET_REQUEST),
                                                                        (void *)PAGE_INVALID_WEBSOCKET_REQUEST,
                                                                        MHD_RESPMEM_PERSISTENT);
        ret = MHD_queue_response(connection,
                                 MHD_HTTP_BAD_REQUEST,
                                 response);
        MHD_destroy_response(response);
        return ret;
    }

    /* create the response for upgrade */
    struct MHD_Response *response = MHD_create_response_for_upgrade(&upgrade_handler, ws_handler);

    /**
     * For the response we need at least the following headers:
     * 1. "Connection: Upgrade"
     * 2. "Upgrade: websocket"
     * 3. "Sec-WebSocket-Accept: <base64value>"
     * The value for Sec-WebSocket-Accept can be generated with MHD_websocket_create_accept.
     * It requires the value of the Sec-WebSocket-Key header of the request.
     * See also: https://tools.ietf.org/html/rfc6455#section-4.2.2
     */
    MHD_add_response_header(response,
                            MHD_HTTP_HEADER_CONNECTION,
                            "Upgrade");
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
    return ret;
}