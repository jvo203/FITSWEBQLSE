#include <stdio.h>
#include <unistd.h>

#include "junzip.h"

int unzip(int fdin, int fdout)
{
    FILE *f;
    JZFile *zip;

    f = fdopen(fdin, "rb");
    if (!f)
        return -1;

    fclose(f);
    close(fdout);

    return 0;
}