#include "colourmap.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

void apply_colourmap(float *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, bool invert, const float *r, const float *g, const float *b, float fill)
{
    if (canvas == NULL || luma == NULL || alpha == NULL)
        return;

    size_t luma_offset = 0;
    size_t alpha_offset = 0;
    size_t dst_offset = 0;

    int no_colours = 64;
    float interp_factor = no_colours / 256.0f;

    for (int j = 0; j < h; j++)
    {
        size_t luma_offset = j * stride_luma;
        size_t alpha_offset = j * stride_alpha;

        for (int i = 0; i < w; i++)
        {
            unsigned char pixel = luma[luma_offset++];
            pixel = invert ? (255 - pixel) : pixel;
            unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;

            float pos = pixel * interp_factor;
            float frac = pos - floorf(pos);
            int x0 = floorf(pos);

            float r_pixel = r[x0] + (r[x0 + 1] - r[x0]) * frac;
            float g_pixel = g[x0] + (g[x0 + 1] - g[x0]) * frac;
            float b_pixel = b[x0] + (b[x0 + 1] - b[x0]) * frac;

            r_pixel = (mask == 0) ? fill : r_pixel;
            g_pixel = (mask == 0) ? fill : g_pixel;
            b_pixel = (mask == 0) ? fill : b_pixel;

            canvas[dst_offset++] = r_pixel;
            canvas[dst_offset++] = g_pixel;
            canvas[dst_offset++] = b_pixel;
            canvas[dst_offset++] = 1.0f; // the alpha channel
        }
    }
}

float clamp(float x, float min, float max)
{
    if (x < min)
        return min;

    if (x > max)
        return max;

    return x;
}

void apply_amber(float *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, float fill)
{
    if (canvas == NULL || luma == NULL || alpha == NULL)
        return;

    size_t luma_offset = 0;
    size_t alpha_offset = 0;
    size_t dst_offset = 0;

    for (int j = 0; j < h; j++)
    {
        size_t luma_offset = j * stride_luma;
        size_t alpha_offset = j * stride_alpha;

        for (int i = 0; i < w; i++)
        {
            float pixel = luma[luma_offset++] / 255.0f;
            unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;
            pixel = (mask == 0) ? fill : pixel;

            canvas[dst_offset++] = pixel;
            canvas[dst_offset++] = clamp(roundf(pixel * 204.0f / 255.0f), 0.0f, 1.0f);
            canvas[dst_offset++] = 0.0f;
            canvas[dst_offset++] = 1.0f; // the alpha channel
        }
    }
}

void apply_greyscale(float *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, bool invert, float fill)
{
    if (canvas == NULL || luma == NULL || alpha == NULL)
        return;

    size_t luma_offset = 0;
    size_t alpha_offset = 0;
    size_t dst_offset = 0;

    for (int j = 0; j < h; j++)
    {
        size_t luma_offset = j * stride_luma;
        size_t alpha_offset = j * stride_alpha;

        for (int i = 0; i < w; i++)
        {
            float pixel = invert ? (1.0f - luma[luma_offset++] / 255.0f) : luma[luma_offset++] / 255.0f;
            unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;
            pixel = (mask == 0) ? fill : pixel;

            canvas[dst_offset++] = pixel;
            canvas[dst_offset++] = pixel;
            canvas[dst_offset++] = pixel;
            canvas[dst_offset++] = 1.0f; // the alpha channel
        }
    }
}

void apply_composite(float *canvas, int w, int h, const unsigned char *_r, const unsigned char *_g, const unsigned char *_b, int stride_r, int stride_g, int stride_b)
{
    if (canvas == NULL || _r == NULL || _g == NULL || _b == NULL)
        return;

    size_t dst_offset = 0;

    for (int j = 0; j < h; j++)
    {
        size_t r_offset = j * stride_r;
        size_t g_offset = j * stride_g;
        size_t b_offset = j * stride_b;

        for (int i = 0; i < w; i++)
        {
            canvas[dst_offset++] = _r[r_offset++] / 255.0f;
            canvas[dst_offset++] = _g[g_offset++] / 255.0f;
            canvas[dst_offset++] = _b[b_offset++] / 255.0f;
            canvas[dst_offset++] = 1.0f; // the alpha channel
        }
    }
}