#include "colourmap.h"

#include <stdio.h>
#include <math.h>

void apply_colourmap(unsigned char *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, bool invert, const float *r, const float *g, const float *b, unsigned char fill)
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
		// Y-mirror-flip the image
		size_t luma_offset = (h - 1 - j) * stride_luma;
		size_t alpha_offset = (h - 1 - j) * stride_alpha;

		for (int i = 0; i < w; i++)
		{
			unsigned char pixel = luma[luma_offset++];
			pixel = invert ? (255 - pixel) : pixel;
			unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;

			float pos = pixel * interp_factor;
			float frac = pos - floorf(pos);
			int x0 = floorf(pos);

			unsigned char r_pixel = 0xFF * (r[x0] + (r[x0 + 1] - r[x0]) * frac);
			unsigned char g_pixel = 0xFF * (g[x0] + (g[x0 + 1] - g[x0]) * frac);
			unsigned char b_pixel = 0xFF * (b[x0] + (b[x0 + 1] - b[x0]) * frac);

			r_pixel = (mask == 0) ? fill : r_pixel;
			g_pixel = (mask == 0) ? fill : g_pixel;
			b_pixel = (mask == 0) ? fill : b_pixel;

			canvas[dst_offset++] = r_pixel;
			canvas[dst_offset++] = g_pixel;
			canvas[dst_offset++] = b_pixel;
			canvas[dst_offset++] = 255; // the alpha channel
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

void apply_amber(unsigned char *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, unsigned char fill)
{
	if (canvas == NULL || luma == NULL || alpha == NULL)
		return;

	size_t luma_offset = 0;
	size_t alpha_offset = 0;
	size_t dst_offset = 0;

	for (int j = 0; j < h; j++)
	{

		// Y-mirror-flip the image
		size_t luma_offset = (h - 1 - j) * stride_luma;
		size_t alpha_offset = (h - 1 - j) * stride_alpha;

		for (int i = 0; i < w; i++)
		{
			unsigned char pixel = luma[luma_offset++];
			unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;
			pixel = (mask == 0) ? fill : pixel;

			canvas[dst_offset++] = pixel;
			canvas[dst_offset++] = clamp(roundf(pixel * 204.0f / 255.0f), 0.0f, 255.0f);
			canvas[dst_offset++] = 0;
			canvas[dst_offset++] = 255; // the alpha channel
		}
	}
}

void apply_greyscale(unsigned char *canvas, int w, int h, const unsigned char *luma, int stride_luma, const unsigned char *alpha, int stride_alpha, bool invert, unsigned char fill)
{
	if (canvas == NULL || luma == NULL || alpha == NULL)
		return;

	size_t luma_offset = 0;
	size_t alpha_offset = 0;
	size_t dst_offset = 0;

	for (int j = 0; j < h; j++)
	{

		// Y-mirror-flip the image
		size_t luma_offset = (h - 1 - j) * stride_luma;
		size_t alpha_offset = (h - 1 - j) * stride_alpha;

		for (int i = 0; i < w; i++)
		{
			unsigned char pixel = invert ? (255 - luma[luma_offset++]) : luma[luma_offset++];
			unsigned char mask = alpha[alpha_offset++] < 128 ? 0 : 255;
			pixel = (mask == 0) ? fill : pixel;

			canvas[dst_offset++] = pixel;
			canvas[dst_offset++] = pixel;
			canvas[dst_offset++] = pixel;
			canvas[dst_offset++] = 255; // the alpha channel
		}
	}
}

void apply_yuv(unsigned char *canvas, const unsigned char *_y, const unsigned char *_u, const unsigned char *_v, int w, int h, int stride, const unsigned char *alpha)
{
	if (canvas == NULL || _y == NULL || _u == NULL || _v == NULL || alpha == NULL)
		return;

	size_t src_offset = 0;
	size_t dst_offset = 0;

	for (int j = 0; j < h; j++)
	{
		size_t offset = j * stride;

		for (int i = 0; i < w; i++)
		{
			unsigned char r = _y[offset];
			unsigned char g = _u[offset];
			unsigned char b = _v[offset];
			offset++;

			// ITU-R
			/*float Y = _y[offset];
		float Cb = _u[offset];
		float Cr = _v[offset];
		offset++;

		unsigned char r = clamp(Y + 1.402f * (Cr - 128.0f), 0.0f, 255.0f);
		unsigned char g = clamp(Y - 0.344f * (Cb - 128.0f) - 0.714f * (Cr - 128.0f), 0.0f, 255.0f);
		unsigned char b = clamp(Y + 1.772f * (Cb - 128.0f), 0.0f, 255.0f);*/

			canvas[dst_offset++] = r;
			canvas[dst_offset++] = g;
			canvas[dst_offset++] = b;
			canvas[dst_offset++] = alpha[src_offset++]; // the alpha channel
		}
	}
}