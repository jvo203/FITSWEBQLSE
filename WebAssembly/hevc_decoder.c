#include <emscripten.h>

#include <libavcodec/hevc_parse.h>
#include <libavutil/common.h>

#include <string.h>

#include "hevc_decoder.h"

// colourmaps
#include "colourmap.h"

// CONREC
#include "conrec.h"

static AVCodec *codec;
static AVCodecContext **avctx = NULL;
static AVFrame **avframe = NULL;
static AVPacket **avpkt = NULL;

extern AVCodec ff_hevc_decoder;

void apply_contour(float *canvas, unsigned int w, unsigned int h, const unsigned char *luma, unsigned int stride_luma, const unsigned char *alpha, unsigned int stride_alpha, int nc);

void hevc_init(int va_count)
{
    // the "standard" way
    codec = &ff_hevc_decoder;

    if (avctx == NULL)
    {
        avctx = malloc(va_count * sizeof(AVCodecContext *));

        for (int i = 0; i < va_count; i++)
        {
            avctx[i] = avcodec_alloc_context3(codec);

            if (!avctx[i])
                printf("Failed to initialize HEVC decoder[%d].\n", i);
        }
    }

    if (avpkt == NULL)
    {
        avpkt = malloc(va_count * sizeof(AVPacket *));

        for (int i = 0; i < va_count; i++)
        {
            avpkt[i] = av_packet_alloc();

            if (!avpkt[i])
                printf("Failed to allocate HEVC packet[%d].\n", i);
            else
                av_init_packet(avpkt[i]);
        }
    }

    if (avframe == NULL)
    {
        avframe = malloc(va_count * sizeof(AVFrame *));

        for (int i = 0; i < va_count; i++)
        {
            avframe[i] = av_frame_alloc();

            if (!avframe[i])
                printf("Failed to allocate HEVC frame[%d].\n", i);
        }
    }

    for (int i = 0; i < va_count; i++)
    {
        avctx[i]->err_recognition |= AV_EF_CRCCHECK;
        if (avcodec_open2(avctx[i], codec, NULL) < 0)
        {
            printf("Failed to open the HEVC codec[%d].\n", i);
        }
    }
}

void hevc_destroy(int va_count)
{
    if (avctx != NULL)
    {
        for (int i = 0; i < va_count; i++)
            if (avctx[i] != NULL)
                avcodec_free_context(&(avctx[i]));

        free(avctx);
        avctx = NULL;
    }

    if (avframe != NULL)
    {
        for (int i = 0; i < va_count; i++)
            if (avframe[i] != NULL)
                av_frame_free(&(avframe[i]));

        free(avframe);
        avframe = NULL;
    }

    if (avpkt != NULL)
    {
        for (int i = 0; i < va_count; i++)
            if (avpkt[i] != NULL)
                av_packet_free(&(avpkt[i]));

        free(avpkt);
        avpkt = NULL;
    }
}

double hevc_decode_nal_unit(int index, const unsigned char *data, size_t data_len, float *canvas, unsigned int _w, unsigned int _h, const char *colourmap, unsigned char fill, int nc)
{
    if (avctx == NULL || avpkt == NULL || avframe == NULL)
        return 0.0;

    if (avctx[index] == NULL || avpkt[index] == NULL || avframe[index] == NULL)
        return 0.0;

    double start = emscripten_get_now();
    double stop = 0.0;

    printf("HEVC: decoding a NAL unit of length %zu bytes, nc = %d\n", data_len, nc);

    uint8_t *buf = malloc(data_len + AV_INPUT_BUFFER_PADDING_SIZE);
    memcpy(buf, data, data_len);
    memset(buf + data_len, 0, AV_INPUT_BUFFER_PADDING_SIZE);

    avpkt[index]->data = (uint8_t *)buf;
    avpkt[index]->size = data_len;

    int ret = avcodec_send_packet(avctx[index], avpkt[index]);

    stop = emscripten_get_now();

    printf("[wasm hevc] ret = %d, elapsed time %5.2f [ms]\n", ret, (stop - start));

    if (ret == AVERROR(EAGAIN))
        printf("avcodec_receive_frame() is needed to remove decoded video frames\n");

    if (ret == AVERROR_EOF)
        printf("the decoder has been flushed\n");

    if (ret == AVERROR(EINVAL))
        printf("codec not opened or requires flush\n");

    if (ret == AVERROR(ENOMEM))
        printf("failed to add packet to internal queue etc.\n");

    while ((ret = avcodec_receive_frame(avctx[index], avframe[index])) == 0)
    {
        enum AVColorSpace cs = avframe[index]->colorspace;
        int format = avframe[index]->format;

        printf("[wasm hevc] decoded a %d x %d frame in a colourspace:format %d:%d, elapsed time %5.2f [ms], colourmap: %s\n", avframe[index]->width, avframe[index]->height, cs, format, (stop - start), colourmap);

        if (format == AV_PIX_FMT_YUV444P)
        {
            printf("processing a YUV444P format\n");

            // apply a colourmap etc.
            int w = avframe[index]->width;
            int h = avframe[index]->height;

            const unsigned char *luma = avframe[index]->data[0];
            int stride_luma = avframe[index]->linesize[0];

            const unsigned char *alpha = avframe[index]->data[1];
            int stride_alpha = avframe[index]->linesize[1];

            size_t len = _w * _h * 4;

            if (w == _w && h == _h && canvas != NULL)
            {
                // apply a colourmap
                if (strcmp(colourmap, "red") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, ocean_g, ocean_r, ocean_b, fill);
                }
                else if (strcmp(colourmap, "green") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, ocean_r, ocean_g, ocean_b, fill);
                }
                else if (strcmp(colourmap, "blue") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, ocean_b, ocean_r, ocean_g, fill);
                }
                else if (strcmp(colourmap, "hot") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, hot_r, hot_g, hot_b, fill);
                }
                else if (strcmp(colourmap, "haxby") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, haxby_r, haxby_g, haxby_b, fill);
                }
                else if (strcmp(colourmap, "rainbow") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, true, rainbow_r, rainbow_g, rainbow_b, fill);
                }
                else if (strcmp(colourmap, "cubehelix") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, cubehelix_r, cubehelix_g, cubehelix_b, fill);
                }
                else if (strcmp(colourmap, "parula") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, parula_r, parula_g, parula_b, fill);
                }
                else if (strcmp(colourmap, "inferno") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, inferno_r, inferno_g, inferno_b, fill);
                }
                else if (strcmp(colourmap, "magma") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, magma_r, magma_g, magma_b, fill);
                }
                else if (strcmp(colourmap, "plasma") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, plasma_r, plasma_g, plasma_b, fill);
                }
                else if (strcmp(colourmap, "viridis") == 0)
                {
                    apply_colourmap(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, viridis_r, viridis_g, viridis_b, fill);
                }
                else if (strcmp(colourmap, "negative") == 0)
                {
                    apply_greyscale(canvas, w, h, luma, stride_luma, alpha, stride_alpha, true, fill);
                }
                else if (strcmp(colourmap, "amber") == 0)
                {
                    apply_amber(canvas, w, h, luma, stride_luma, alpha, stride_alpha, fill);
                }
                else if (strcmp(colourmap, "composite") == 0)
                {
                    const unsigned char *r = avframe[index]->data[0];
                    int stride_r = avframe[index]->linesize[0];

                    const unsigned char *g = avframe[index]->data[1];
                    int stride_g = avframe[index]->linesize[1];

                    const unsigned char *b = avframe[index]->data[2];
                    int stride_b = avframe[index]->linesize[2];

                    apply_composite(canvas, w, h, r, g, b, stride_r, stride_g, stride_b);
                }
                else
                {
                    // no colour by default
                    apply_greyscale(canvas, w, h, luma, stride_luma, alpha, stride_alpha, false, fill);
                };

                // apply contouring (optional)
                if (nc > 0)
                    apply_contour(canvas, w, h, luma, stride_luma, alpha, stride_alpha, nc);
            }
            else
                printf("[wasm hevc] canvas image dimensions %d x %d do not match the decoded image size, doing nothing\n", _w, _h);
        }

        av_frame_unref(avframe[index]);
    }

    printf("avcodec_receive_frame returned = %d\n", ret);

    double elapsed = stop - start;

    if (buf != NULL)
        free(buf);

    return elapsed;
}

void apply_contour(float *canvas, unsigned int w, unsigned int h, const unsigned char *luma, unsigned int stride_luma, const unsigned char *alpha, unsigned int stride_alpha, int nc)
{
    if (canvas == NULL || luma == NULL || alpha == NULL)
        return;

    size_t luma_offset = 0;
    size_t alpha_offset = 0;

    // make a copy of pixels (re-arrange) for the CONREC algorithm
    float **d = (float **)calloc(h, sizeof(float *));
    for (int i = 0; i < h; i++)
    {
        // Y-mirror-flip the image
        size_t luma_offset = (h - 1 - i) * stride_luma;
        size_t alpha_offset = (h - 1 - i) * stride_alpha;

        d[i] = (float *)calloc(w, sizeof(float));
        for (int j = 0; j < w; j++)
        {
            unsigned char pixel = luma[luma_offset++];
            unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;
            pixel = (mask == 0) ? 0 : pixel;

            d[i][j] = (float)pixel;
        }
    }

    // CONREC variables
    float xc[h];
    float yc[w];
    float zc[nc];

    const int ilb = 0;
    const int iub = h - 1;
    const int jlb = 0;
    const int jub = w - 1;

    for (int i = 0; i < h; i++)
        xc[i] = i;

    for (int i = 0; i < w; i++)
        yc[i] = i;

    // contour levels between 0 and 255
    for (int i = 0; i < nc; i++)
        zc[i] = (float)(i + 1) * 255.0f / (float)nc;

    // CONREC algorithm
    conrec(d, ilb, iub, jlb, jub, xc, yc, nc, zc, canvas, w, h);

    // free d
    for (int i = 0; i < h; i++)
        free(d[i]);

    free(d);
}