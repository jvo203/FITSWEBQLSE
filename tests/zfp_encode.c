#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

int main()
{
    int i, j;
    int offset;

    float fblock[16];

    offset = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            fblock[offset++] = (i + 1) * (j + 1);

    for (i = 0; i < 16; i++)
        printf("%f\t", fblock[i]);
    printf("\n");
}