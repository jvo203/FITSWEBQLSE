#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "junzip.h"

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

endClose:
    if (zip != NULL)
        zip->close(zip);

    fclose(fp);
    close(fdout);

    return retval;
}