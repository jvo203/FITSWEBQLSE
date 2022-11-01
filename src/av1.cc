// AV1 image compressor
#include <avif/avif.h>

// JPEG-TURBO header
#include <jpeglib.h>

int write_pv_diagram_av1(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax);
void write_pv_diagram_jpeg(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax);


int write_pv_diagram_av1(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax)
{
    printf("[C] write_pv_diagram_av1()\n");

    int returnCode = 1;
    avifEncoder *encoder = NULL;
    avifRWData avifOutput = AVIF_DATA_EMPTY;
    avifRGBImage rgb;
    memset(&rgb, 0, sizeof(rgb));

    avifImage *image = avifImageCreate(width, height, 8, AVIF_PIXEL_FORMAT_YUV444); // these values dictate what goes into the final AVIF
    memset(image->alphaPlane, 255, image->alphaRowBytes * image->height);
    avifRGBImageSetDefaults(&rgb, image);

    rgb.width = width;
    rgb.height = height;
    rgb.depth = 8;
    rgb.format = AVIF_RGB_FORMAT_RGB;
    rgb.ignoreAlpha = AVIF_TRUE;
    rgb.rowBytes = rgb.width * 3;

    /*printf("[C] rgb width: %d; height: %d; depth: %d; ignoreAlpha: %d\n", rgb.width, rgb.height, rgb.depth, rgb.ignoreAlpha);
    printf("[C] rowBytes: %d\n", rgb.rowBytes);*/

    avifRGBImageAllocatePixels(&rgb);
    memset(rgb.pixels, 255, rgb.rowBytes * rgb.height);

    // convert pixels to RGB using the ERF colourmap
    size_t img_size = (size_t)width * (size_t)height;

    unsigned char *pvBuffer = rgb.pixels;
    size_t pvOffset = 0;

    for (size_t i = 0; i < img_size; i++)
    {
        float value = pv[i];

        if (value < 0.0f)
            value = 0.0f;
        else if (value > 1.0f)
            value = 1.0f;

        unsigned char r = 0;
        unsigned char g = 0;
        unsigned char b = 0;

        float pos = value * (NO_COLOURS - 1);
        float frac = pos - floorf(pos);
        int x0 = floorf(pos);

        r = 0xFF * (math_r[x0] + (math_r[x0 + 1] - math_r[x0]) * frac);
        g = 0xFF * (math_g[x0] + (math_g[x0 + 1] - math_g[x0]) * frac);
        b = 0xFF * (math_b[x0] + (math_b[x0 + 1] - math_b[x0]) * frac);

        pvBuffer[pvOffset++] = r;
        pvBuffer[pvOffset++] = g;
        pvBuffer[pvOffset++] = b;
    }

    avifResult convertResult = avifImageRGBToYUV(image, &rgb);
    if (convertResult != AVIF_RESULT_OK)
    {
        fprintf(stderr, "[C] Failed to convert to YUV(A): %s\n", avifResultToString(convertResult));
        goto cleanup;
    }

    encoder = avifEncoderCreate();
    encoder->maxThreads = 4; // default: 1, i.e. no multi-threading
    encoder->speed = AVIF_SPEED_FASTEST;
    encoder->minQuantizer = 1;
    encoder->maxQuantizer = 30;
    encoder->autoTiling = AVIF_TRUE;

    avifResult addImageResult = avifEncoderAddImage(encoder, image, 1, AVIF_ADD_IMAGE_FLAG_SINGLE);
    if (addImageResult != AVIF_RESULT_OK)
    {
        fprintf(stderr, "[C] Failed to add image to encoder: %s\n", avifResultToString(addImageResult));
        goto cleanup;
    }

    avifResult finishResult = avifEncoderFinish(encoder, &avifOutput);
    if (finishResult != AVIF_RESULT_OK)
    {
        fprintf(stderr, "[C] AVIF Failed to finish encode: %s\n", avifResultToString(finishResult));
        goto cleanup;
    }

    printf("[C] AVIF Encode success: %zu total bytes\n", avifOutput.size);

    const char *outputFilename = "test.avif";

    FILE *f = fopen(outputFilename, "wb");
    size_t bytesWritten = fwrite(avifOutput.data, 1, avifOutput.size, f);
    fclose(f);

    if (bytesWritten != avifOutput.size)
    {
        fprintf(stderr, "[C] Failed to write %zu bytes\n", avifOutput.size);
        goto cleanup;
    }

    printf("[C] Wrote: %s\n", outputFilename);

cleanup:
    if (image)
        avifImageDestroy(image);
    if (encoder)
        avifEncoderDestroy(encoder);

    avifRWDataFree(&avifOutput);
    avifRGBImageFreePixels(&rgb); // Only use in conjunction with avifRGBImageAllocatePixels()

    return returnCode;
}

void write_pv_diagram_jpeg(int fd, int width, int height, int precision, const float *restrict pv, const float pmean, const float pstd, const float pmin, const float pmax)
{
    printf("[C] write_pv_diagram_jpeg()\n");

    // prepare pixels first
    size_t img_size = (size_t)width * (size_t)height;
    unsigned char *pixels = (unsigned char *)malloc(img_size * 3);

    size_t pvOffset = 0;

    for (size_t i = 0; i < img_size; i++)
    {
        float value = pv[i];

        if (value < 0.0f)
            value = 0.0f;
        else if (value > 1.0f)
            value = 1.0f;

        unsigned char r = 0;
        unsigned char g = 0;
        unsigned char b = 0;

        float pos = value * (NO_COLOURS - 1);
        float frac = pos - floorf(pos);
        int x0 = floorf(pos);

        r = 0xFF * (math_r[x0] + (math_r[x0 + 1] - math_r[x0]) * frac);
        g = 0xFF * (math_g[x0] + (math_g[x0 + 1] - math_g[x0]) * frac);
        b = 0xFF * (math_b[x0] + (math_b[x0 + 1] - math_b[x0]) * frac);

        pixels[pvOffset++] = r;
        pixels[pvOffset++] = g;
        pixels[pvOffset++] = b;
    }

    const char *filename = "test.jpeg";

    struct jpeg_compress_struct cinfo;
    struct jpeg_error_mgr jerr;

    FILE *outfile;           /* target file */
    JSAMPROW row_pointer[1]; /* pointer to JSAMPLE row[s] */
    int row_stride;          /* physical row width in image buffer */

    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_compress(&cinfo);

    int quality = 30;

    unsigned long jpegSize = 0;
    unsigned char *jpegBuf = NULL;

    if ((outfile = fopen(filename, "wb")) == NULL)
    {
        fprintf(stderr, "can't open %s\n", filename);
        exit(1);
    }

    jpeg_mem_dest(&cinfo, &jpegBuf, &jpegSize);
    // jpeg_stdio_dest(&cinfo, outfile);

    cinfo.image_width = width; /* image width and height, in pixels */
    cinfo.image_height = height;
    cinfo.input_components = 3;     /* # of color components per pixel */
    cinfo.in_color_space = JCS_RGB; /* colorspace of input image */

    jpeg_set_defaults(&cinfo);
    jpeg_set_quality(&cinfo, quality, TRUE /* limit to baseline-JPEG values */);

    jpeg_start_compress(&cinfo, TRUE);
    row_stride = width * 3; /* JSAMPLEs per row in image_buffer */

    while (cinfo.next_scanline < cinfo.image_height)
    {
        row_pointer[0] = &pixels[cinfo.next_scanline * row_stride];
        (void)jpeg_write_scanlines(&cinfo, row_pointer, 1);
    }

    jpeg_finish_compress(&cinfo);
    jpeg_destroy_compress(&cinfo);

    printf("[C] JPEG Encode success: %lu total bytes\n", jpegSize);

    size_t bytesWritten = fwrite(jpegBuf, 1, jpegSize, outfile);
    fclose(outfile);

    free(pixels);
}

  // try AV1 first (libavif)
    write_pv_diagram_av1(fd, width, height, precision, pv, pmean, pstd, pmin, pmax); // disabled: too slow (and too big compressed buffer sizes)

    // try JPEG too
    write_pv_diagram_jpeg(fd, width, height, precision, pv, pmean, pstd, pmin, pmax);

