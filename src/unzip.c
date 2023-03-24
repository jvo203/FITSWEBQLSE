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
    FILE *f;
    JZFile *zip;
    JZEndRecord endRecord;

    int retval = -1;

    f = fdopen(fdin, "rb");
    if (!f)
        return -1;

    zip = jzfile_from_stdio_file(f);

    if (jzReadEndRecord(zip, &endRecord))
    {
        printf("[C] Couldn't read ZIP file end record\n.");
        goto endClose;
    }

    retval = processFile(zip, fdout);

endClose:
    zip->close(zip);
    fclose(f);
    close(fdout);

    return retval;
}