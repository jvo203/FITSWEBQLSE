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
    int nWritten;

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
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
        printf("[C] BZ2_bzReadOpen failed with error %d.\n", bzerror);
        fclose(f);
        return bzerror;
    }

    bzerror = BZ_OK;
    while (bzerror == BZ_OK)
    {
        nBuf = BZ2_bzRead(&bzerror, b, buf, CHUNK);
        if (bzerror == BZ_OK || bzerror == BZ_STREAM_END)
        {
            /* do something with buf[0 .. nBuf-1] */
            if (nBuf <= 0)
                continue;

            if (write(fdout, buf, (size_t)nBuf) != (size_t)nBuf)
            {
                BZ2_bzReadClose(&bzerror, b);
                /* handle error */
                printf("[C] writing %d to fdout failed.\n", nBuf);
                fclose(f);
                return bzerror;
            }
        }
    }

    if (bzerror != BZ_STREAM_END)
    {
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
        printf("[C] BZ_STREAM_END not found with error %d.\n", bzerror);
        fclose(f);
        return bzerror;
    }
    else
    {
        BZ2_bzReadClose(&bzerror, b);
    }

    fclose(f);
    close(fdout);

    return BZ_OK;
}