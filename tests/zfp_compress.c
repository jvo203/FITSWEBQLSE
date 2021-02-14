#include <zfp.h>

#include <stdlib.h>
#include <stdio.h>

int main()
{
    int i, j;
    uchar *compressed_pixels = NULL;

    // ZFP variables
    zfp_type data_type = zfp_type_float;
    zfp_field *field = NULL;
    zfp_stream *zfp = NULL;
    size_t bufsize = 0;
    bitstream *stream = NULL;
    size_t zfpsize = 0;
    uint precision = 11;
    uint nx = 512;
    uint ny = 512;

    float *_pixels = (float *)malloc(nx * ny * sizeof(float));

    for (j = 0; j < ny; j++)
        for (i = 0; i < nx; i++)
            _pixels[i + nx * j] = i * j;

    field = zfp_field_2d((void *)_pixels, data_type, nx, ny);
    printf("got here#0, field = %p\n", field);

    // allocate metadata for a compressed stream
    zfp = zfp_stream_open(NULL);
    printf("got here#1, zfp = %p\n", zfp);

    zfp_stream_set_rate(zfp, 8.0, data_type, 2, 0);
    //zfp_stream_set_precision(zfp, precision);
    printf("got here#2\n");

    // allocate buffer for compressed data
    bufsize = zfp_stream_maximum_size(zfp, field);
    printf("got here#3, bufsize = %zd\n", bufsize);

    compressed_pixels = (uchar *)malloc(bufsize);
    printf("got here#4\n");

    if (compressed_pixels != NULL)
    {
        printf("calling stream_open...");
        // associate bit stream with allocated buffer
        stream = stream_open((void *)compressed_pixels, bufsize);
        printf("got here#5, bufsize = %zu, stream = %p\n", bufsize, stream);

        if (stream != NULL)
        {
            zfp_stream_set_bit_stream(zfp, stream);
            printf("got here#6\n");

            zfp_write_header(zfp, field, ZFP_HEADER_FULL);
            printf("got here#7\n");

            // compress entire array
            zfpsize = zfp_compress(zfp, field);
            printf("got here#8\n");

            if (zfpsize == 0)
                printf("ZFP compression failed!\n");
            else
                printf("compressed size: %zd bytes\n", zfpsize);

            stream_close(stream);

            // the compressed part is available at compressed_pixels[0..zfpsize-1]
        }

        /* clean up */
        zfp_field_free(field);
        zfp_stream_close(zfp);

        // release memory
        free(compressed_pixels);
    }
    else
        printf("a NULL compressed_pixels buffer!\n");

    free(_pixels);

    return 0;
}
