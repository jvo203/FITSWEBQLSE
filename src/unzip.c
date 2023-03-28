#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "junzip.h"

int processFile(JZFile *zip, int fdout)
{
    JZFileHeader header;
    char filename[1024];

    if (jzReadLocalFileHeader(zip, &header, filename, sizeof(filename)))
    {
        printf("[C] Couldn't read local file header!\n");
        return -1;
    }

    printf("[C] %s, %d / %d bytes at offset %08X\n", filename,
           header.compressedSize, header.uncompressedSize, header.offset);

    if (jzReadData(zip, &header, fdout) != Z_OK)
    {
        printf("[C] Couldn't read file data!\n");
        close(fdout);
        return -1;
    }

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
        /* handle error */
        printf("[C] jzfile_from_stdio_file failed.\n");
        fclose(fp);
        return -1;
    }

    retval = processFile(zip, fdout);

    // fp will be closed by zip->close(zip), in "stdio_read_file_handle_close()"
    zip->close(zip);
    close(fdout);

    return retval;
}