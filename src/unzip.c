#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "junzip.h"

int processFile(JZFile *zip, int fdout)
{
    JZFileHeader header;
    char filename[1024];
    unsigned char *data;

    if (jzReadLocalFileHeader(zip, &header, filename, sizeof(filename)))
    {
        printf("[C] Couldn't read local file header!");
        return -1;
    }

    if ((data = (unsigned char *)malloc(header.uncompressedSize)) == NULL)
    {
        printf("[C] Couldn't allocate memory!");
        return -1;
    }

    printf("[C] %s, %d / %d bytes at offset %08X\n", filename,
           header.compressedSize, header.uncompressedSize, header.offset);

    if (jzReadData(zip, &header, data) != Z_OK)
    {
        printf("[C] Couldn't read file data!");
        free(data);
        return -1;
    }

    free(data);

    return 0;
}

int unzip(int fdin, int fdout)
{
    FILE *fp;
    int retval = -1;

    JZFile *zip;

    fp = fdopen(fdin, "rb");
    if (!fp)
    {
        /* handle error */
        printf("[C] fdopen failed.\n");
        return -1;
    }

    zip = jzfile_from_stdio_file(fp);
    if (zip == NULL)
    {
        printf("[C] jzfile_from_stdio_file failed.\n");
        goto endClose;
    }

    retval = processFile(zip, fdout);

endClose:
    if (zip != NULL)
        zip->close(zip);

    fclose(fp);
    close(fdout);

    return retval;
}