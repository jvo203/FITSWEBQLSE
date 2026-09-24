#include "microhttpd_ws_compat.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <arpa/inet.h>
#include <strings.h>

struct MHD_WebSocketStream
{
    int flags;
    int reserved;
};

typedef struct
{
    uint32_t state[5];
    uint64_t bitlen;
    uint8_t data[64];
    uint32_t datalen;
} SHA1_CTX;

static uint32_t rol32(uint32_t value, uint32_t bits)
{
    return (value << bits) | (value >> (32 - bits));
}

static void sha1_transform(SHA1_CTX *ctx, const uint8_t data[])
{
    uint32_t a, b, c, d, e, i, t, m[80];

    for (i = 0; i < 16; ++i)
    {
        m[i] = ((uint32_t)data[i * 4] << 24) |
               ((uint32_t)data[i * 4 + 1] << 16) |
               ((uint32_t)data[i * 4 + 2] << 8) |
               ((uint32_t)data[i * 4 + 3]);
    }

    for (i = 16; i < 80; ++i)
        m[i] = rol32(m[i - 3] ^ m[i - 8] ^ m[i - 14] ^ m[i - 16], 1);

    a = ctx->state[0];
    b = ctx->state[1];
    c = ctx->state[2];
    d = ctx->state[3];
    e = ctx->state[4];

    for (i = 0; i < 80; ++i)
    {
        if (i < 20)
            t = rol32(a, 5) + ((b & c) | ((~b) & d)) + e + m[i] + 0x5A827999u;
        else if (i < 40)
            t = rol32(a, 5) + (b ^ c ^ d) + e + m[i] + 0x6ED9EBA1u;
        else if (i < 60)
            t = rol32(a, 5) + ((b & c) | (b & d) | (c & d)) + e + m[i] + 0x8F1BBCDCu;
        else
            t = rol32(a, 5) + (b ^ c ^ d) + e + m[i] + 0xCA62C1D6u;

        e = d;
        d = c;
        c = rol32(b, 30);
        b = a;
        a = t;
    }

    ctx->state[0] += a;
    ctx->state[1] += b;
    ctx->state[2] += c;
    ctx->state[3] += d;
    ctx->state[4] += e;
}

static void sha1_init(SHA1_CTX *ctx)
{
    ctx->datalen = 0;
    ctx->bitlen = 0;
    ctx->state[0] = 0x67452301u;
    ctx->state[1] = 0xEFCDAB89u;
    ctx->state[2] = 0x98BADCFEu;
    ctx->state[3] = 0x10325476u;
    ctx->state[4] = 0xC3D2E1F0u;
}

static void sha1_update(SHA1_CTX *ctx, const uint8_t data[], size_t len)
{
    size_t i;

    for (i = 0; i < len; ++i)
    {
        ctx->data[ctx->datalen++] = data[i];
        if (ctx->datalen == 64)
        {
            sha1_transform(ctx, ctx->data);
            ctx->bitlen += 512;
            ctx->datalen = 0;
        }
    }
}

static void sha1_final(SHA1_CTX *ctx, uint8_t hash[])
{
    uint32_t i;

    ctx->bitlen += (uint64_t)ctx->datalen * 8u;
    ctx->data[ctx->datalen++] = 0x80;

    if (ctx->datalen > 56)
    {
        while (ctx->datalen < 64)
            ctx->data[ctx->datalen++] = 0;

        sha1_transform(ctx, ctx->data);
        ctx->datalen = 0;
    }

    while (ctx->datalen < 56)
        ctx->data[ctx->datalen++] = 0;

    for (i = 0; i < 8; ++i)
        ctx->data[56 + i] = (uint8_t)(ctx->bitlen >> (8 * (7 - i)));

    sha1_transform(ctx, ctx->data);

    for (i = 0; i < 5; ++i)
    {
        hash[i * 4] = (uint8_t)(ctx->state[i] >> 24);
        hash[i * 4 + 1] = (uint8_t)(ctx->state[i] >> 16);
        hash[i * 4 + 2] = (uint8_t)(ctx->state[i] >> 8);
        hash[i * 4 + 3] = (uint8_t)(ctx->state[i]);
    }
}

static char *trim_header_value(const char *value)
{
    if (value == NULL)
        return NULL;

    while (*value != '\0' && isspace((unsigned char)*value))
        value++;

    size_t len = strlen(value);
    while (len > 0 && isspace((unsigned char)value[len - 1]))
        len--;

    char *copy = (char *)malloc(len + 1);
    if (copy == NULL)
        return NULL;

    memcpy(copy, value, len);
    copy[len] = '\0';
    return copy;
}

static int header_contains_token(const char *value, const char *token)
{
    if (value == NULL || token == NULL)
        return -1;

    char *trimmed = trim_header_value(value);
    if (trimmed == NULL)
        return -1;

    const char *p = trimmed;
    int result = -1;

    while (*p != '\0')
    {
        while (*p == ' ' || *p == '\t' || *p == ',' || *p == ';' || *p == '\r' || *p == '\n')
            p++;

        if (*p == '\0')
            break;

        size_t len = strcspn(p, ",; \t\r\n");
        if (len == strlen(token) && strncasecmp(p, token, len) == 0)
        {
            result = 0;
            break;
        }

        p += len;
    }

    free(trimmed);
    return result;
}

static int header_has_only_single_token(const char *value, const char *token)
{
    if (value == NULL || token == NULL)
        return -1;

    char *trimmed = trim_header_value(value);
    if (trimmed == NULL || trimmed[0] == '\0')
    {
        free(trimmed);
        return -1;
    }

    if (strcmp(trimmed, token) == 0)
    {
        free(trimmed);
        return 0;
    }

    char *copy = strdup(trimmed);
    free(trimmed);
    if (copy == NULL)
        return -1;

    int matches = 0;
    char *saveptr = NULL;
    char *entry = strtok_r(copy, ",; 	\r\n", &saveptr);

    while (entry != NULL)
    {
        if (strcasecmp(entry, token) == 0)
            matches++;
        else
        {
            free(copy);
            return -1;
        }

        entry = strtok_r(NULL, ",; \t\r\n", &saveptr);
    }

    free(copy);
    return (matches == 1) ? 0 : -1;
}

static void b64_encode(const unsigned char *input, size_t input_len, char *output, size_t output_size)
{
    static const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    if (output_size < ((input_len + 2) / 3) * 4 + 1)
        return;

    size_t out = 0;
    for (size_t i = 0; i < input_len; i += 3)
    {
        unsigned char a = input[i];
        unsigned char b = i + 1 < input_len ? input[i + 1] : 0;
        unsigned char c = i + 2 < input_len ? input[i + 2] : 0;

        output[out++] = alphabet[(a >> 2) & 0x3F];
        output[out++] = alphabet[((a & 0x03) << 4) | ((b >> 4) & 0x0F)];
        output[out++] = (i + 1 < input_len) ? alphabet[((b & 0x0F) << 2) | ((c >> 6) & 0x03)] : '=';
        output[out++] = (i + 2 < input_len) ? alphabet[c & 0x3F] : '=';
    }

    output[out] = '\0';
}

int MHD_websocket_stream_init(struct MHD_WebSocketStream **stream, int flags, int reserved)
{
    (void)reserved;

    if (stream == NULL)
        return -1;

    *stream = (struct MHD_WebSocketStream *)calloc(1, sizeof(struct MHD_WebSocketStream));
    if (*stream == NULL)
        return -1;

    (*stream)->flags = flags;
    (*stream)->reserved = reserved;
    return 0;
}

void MHD_websocket_stream_free(struct MHD_WebSocketStream *stream)
{
    free(stream);
}

int MHD_websocket_encode_close(struct MHD_WebSocketStream *stream,
                               enum MHD_WEBSOCKET_CLOSEREASON reason,
                               const char *reason_phrase,
                               size_t reason_phrase_len,
                               char **payload,
                               size_t *payload_len)
{
    (void)stream;

    if (payload == NULL || payload_len == NULL)
        return -1;

    *payload = NULL;
    *payload_len = 0;

    if (reason_phrase_len == 0 && reason_phrase != NULL)
        reason_phrase_len = strlen(reason_phrase);

    size_t total = 2 + reason_phrase_len;
    char *frame = (char *)malloc(total);
    if (frame == NULL)
        return -1;

    uint16_t code = (uint16_t)htons((uint16_t)reason);
    memcpy(frame, &code, 2);

    if (reason_phrase != NULL && reason_phrase_len > 0)
    {
        memcpy(frame + 2, reason_phrase, reason_phrase_len);
    }

    *payload = frame;
    *payload_len = total;
    return 0;
}

void MHD_websocket_free(struct MHD_WebSocketStream *stream, void *payload)
{
    (void)stream;
    free(payload);
}

int MHD_websocket_check_http_version(const char *version)
{
    if (version == NULL)
        return -1;

    char *trimmed = trim_header_value(version);
    if (trimmed == NULL)
        return -1;

    int result = -1;
    if (strcmp(trimmed, "HTTP/1.0") == 0 || strcmp(trimmed, "HTTP/1.1") == 0 ||
        strcmp(trimmed, "1.0") == 0 || strcmp(trimmed, "1.1") == 0)
    {
        result = 0;
    }

    free(trimmed);
    return result;
}

int MHD_websocket_check_connection_header(const char *value)
{
    if (value == NULL)
        return -1;

    if (header_contains_token(value, "Upgrade") == 0)
        return 0;

    return -1;
}

int MHD_websocket_check_upgrade_header(const char *value)
{
    if (value == NULL)
        return -1;

    if (header_contains_token(value, "websocket") == 0)
        return 0;

    return -1;
}

int MHD_websocket_check_version_header(const char *value)
{
    if (value == NULL)
        return -1;

    if (header_has_only_single_token(value, "13") == 0)
        return 0;

    return -1;
}

int MHD_websocket_create_accept_header(const char *value, char accept[29])
{
    if (value == NULL || accept == NULL)
        return -1;

    char *trimmed = trim_header_value(value);
    if (trimmed == NULL || trimmed[0] == '\0')
    {
        free(trimmed);
        return -1;
    }

    const char *guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    size_t key_len = strlen(trimmed);
    size_t total = key_len + strlen(guid);

    if (total > 4096)
    {
        free(trimmed);
        return -1;
    }

    unsigned char buffer[4096];
    unsigned char hash[20];
    SHA1_CTX ctx;

    memset(buffer, 0, sizeof(buffer));
    memcpy(buffer, trimmed, key_len);
    memcpy(buffer + key_len, guid, strlen(guid));

    sha1_init(&ctx);
    sha1_update(&ctx, buffer, total);
    sha1_final(&ctx, hash);
    b64_encode(hash, sizeof(hash), accept, 29);

    free(trimmed);
    return 0;
}
