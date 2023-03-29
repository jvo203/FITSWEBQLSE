#include <bzlib.h>
#include <stdio.h>
#include <unistd.h>

#define CHUNK 16384

int bunzip2(int fdin, int fdout)
{
    FILE *f;
    BZFILE *b;
    int nBuf;
    char buf[CHUNK];
    int bzerror;

    f = fdopen(fdin, "rb");
    if (!f)
    {
        /* handle error */
        printf("[C] fdopen failed.\n");
        return BZ_IO_ERROR;
    }

    b = BZ2_bzReadOpen(&bzerror, f, 0, 0, NULL, 0);
    if (bzerror != BZ_OK)
    {
        printf("[C] BZ2_bzReadOpen failed with error %d.\n", bzerror);
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
        fclose(f);
        return BZ_IO_ERROR;
    }

    bzerror = BZ_OK;
    while (bzerror == BZ_OK)
    {
        nBuf = BZ2_bzRead(&bzerror, b, buf, CHUNK);
        if (bzerror == BZ_OK || bzerror == BZ_STREAM_END)
        {
            if (nBuf <= 0)
                continue;

            /* do something with buf[0 .. nBuf-1] */
            if (write(fdout, buf, (size_t)nBuf) != nBuf)
            {
                BZ2_bzReadClose(&bzerror, b);
                /* handle error */
                printf("[C] writing %d bytes to fdout failed.\n", nBuf);
                fclose(f);
                return BZ_IO_ERROR;
            }
        }
    }

    if (bzerror != BZ_STREAM_END)
    {
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
        printf("[C] BZ_STREAM_END not found.\n");
        fclose(f);
        return BZ_MEM_ERROR;
    }
    else
    {
        BZ2_bzReadClose(&bzerror, b);
    }

    fclose(f);

    return BZ_OK;
}