#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

    struct MHD_WebSocketStream;
    struct MHD_UpgradeResponseHandle;

    enum MHD_WEBSOCKET_FLAG
    {
        MHD_WEBSOCKET_FLAG_SERVER = 0,
        MHD_WEBSOCKET_FLAG_CLIENT = 1,
        MHD_WEBSOCKET_FLAG_NO_FRAGMENTS = 0,
        MHD_WEBSOCKET_FLAG_WANT_FRAGMENTS = 2,
        MHD_WEBSOCKET_FLAG_GENERATE_CLOSE_FRAMES_ON_ERROR = 4
    };

    enum MHD_WEBSOCKET_CLOSEREASON
    {
        MHD_WEBSOCKET_CLOSEREASON_NORMAL = 1000,
        MHD_WEBSOCKET_CLOSEREASON_GOING_AWAY = 1001,
        MHD_WEBSOCKET_CLOSEREASON_PROTOCOL_ERROR = 1002,
        MHD_WEBSOCKET_CLOSEREASON_UNSUPPORTED = 1003,
        MHD_WEBSOCKET_CLOSEREASON_NO_STATUS = 1005,
        MHD_WEBSOCKET_CLOSEREASON_ABNORMAL = 1006,
        MHD_WEBSOCKET_CLOSEREASON_INVALID_PAYLOAD = 1007,
        MHD_WEBSOCKET_CLOSEREASON_POLICY_VIOLATION = 1008,
        MHD_WEBSOCKET_CLOSEREASON_MESSAGE_TOO_BIG = 1009,
        MHD_WEBSOCKET_CLOSEREASON_EXTENSION_REQUIRED = 1010,
        MHD_WEBSOCKET_CLOSEREASON_INTERNAL_ERROR = 1011,
        MHD_WEBSOCKET_CLOSEREASON_TLS_ERROR = 1015,
        MHD_WEBSOCKET_CLOSEREASON_REGULAR = MHD_WEBSOCKET_CLOSEREASON_NORMAL
    };

    enum MHD_WEBSOCKET_FRAGMENTATION
    {
        MHD_WEBSOCKET_FRAGMENTATION_NONE = 0,
        MHD_WEBSOCKET_FRAGMENTATION_FIRST = 1,
        MHD_WEBSOCKET_FRAGMENTATION_FOLLOWING = 2,
        MHD_WEBSOCKET_FRAGMENTATION_LAST = 3
    };

    enum MHD_WEBSOCKET_STATUS
    {
        MHD_WEBSOCKET_STATUS_OK = 0,
        MHD_WEBSOCKET_STATUS_TEXT_FRAME = 0x1,
        MHD_WEBSOCKET_STATUS_BINARY_FRAME = 0x2,
        MHD_WEBSOCKET_STATUS_CLOSE_FRAME = 0x8,
        MHD_WEBSOCKET_STATUS_PING_FRAME = 0x9,
        MHD_WEBSOCKET_STATUS_PONG_FRAME = 0xA,
        MHD_WEBSOCKET_STATUS_TEXT_FIRST_FRAGMENT = 0x11,
        MHD_WEBSOCKET_STATUS_BINARY_FIRST_FRAGMENT = 0x12,
        MHD_WEBSOCKET_STATUS_TEXT_NEXT_FRAGMENT = 0x21,
        MHD_WEBSOCKET_STATUS_BINARY_NEXT_FRAGMENT = 0x22,
        MHD_WEBSOCKET_STATUS_TEXT_LAST_FRAGMENT = 0x31,
        MHD_WEBSOCKET_STATUS_BINARY_LAST_FRAGMENT = 0x32
    };

    int MHD_websocket_stream_init(struct MHD_WebSocketStream **stream, int flags, int reserved);
    void MHD_websocket_stream_free(struct MHD_WebSocketStream *stream);

    int MHD_websocket_encode_close(struct MHD_WebSocketStream *stream,
                                   enum MHD_WEBSOCKET_CLOSEREASON reason,
                                   const char *reason_phrase,
                                   size_t reason_phrase_len,
                                   char **payload,
                                   size_t *payload_len);

    void MHD_websocket_free(struct MHD_WebSocketStream *stream, void *payload);

    int MHD_websocket_check_http_version(const char *version);
    int MHD_websocket_check_connection_header(const char *value);
    int MHD_websocket_check_upgrade_header(const char *value);
    int MHD_websocket_check_version_header(const char *value);
    int MHD_websocket_create_accept_header(const char *value, char accept[29]);

#ifdef __cplusplus
}
#endif
