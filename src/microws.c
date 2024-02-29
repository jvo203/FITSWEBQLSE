#include <microhttpd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef MICROWS
#include <microhttpd_ws.h>

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

#endif // MICROWS