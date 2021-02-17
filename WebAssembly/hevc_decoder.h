#pragma once

#include <emscripten.h>

static unsigned char *canvasBuffer = NULL;
static size_t canvasLength = 0;

void hevc_init(int va_count);
void hevc_destroy(int va_count);
double hevc_decode_nal_unit(int index, const unsigned char *data, size_t data_len, unsigned char *canvas, unsigned int _w, unsigned int _h, const char *colourmap, unsigned char fill);
