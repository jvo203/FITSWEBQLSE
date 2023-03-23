#include <bzlib.h>
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

    f = fdopen(fdin, "r");
    if (!f)
    {
        /* handle error */
        return BZ_IO_ERROR;
    }

    b = BZ2_bzReadOpen(&bzerror, f, 0, 0, NULL, 0);
    if (bzerror != BZ_OK)
    {
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
        fclose(f);
        return bzerror;
    }

    bzerror = BZ_OK;
    while (bzerror == BZ_OK)
    {
        nBuf = BZ2_bzRead(&bzerror, b, buf, CHUNK);
        if (bzerror == BZ_OK)
        {
            /* do something with buf[0 .. nBuf-1] */
            if (write(fdout, buf, (size_t)nBuf) != (size_t)nBuf)
            {
                BZ2_bzReadClose(&bzerror, b);
                /* handle error */
                fclose(f);
                return bzerror;
            }
        }
    }

    if (bzerror != BZ_STREAM_END)
    {
        BZ2_bzReadClose(&bzerror, b);
        /* handle error */
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