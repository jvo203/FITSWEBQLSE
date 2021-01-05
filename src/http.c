#include <microhttpd.h>

struct MHD_Daemon *http_server = NULL;

static int on_http_connection(void *cls, struct MHD_Connection *connection,
                              const char *url, const char *method,
                              const char *version, const char *upload_data,
                              size_t *upload_data_size, void **con_cls);
