#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

#define BLOCK_SIZE 16

int main()
{
    int i, j;
    int offset;

    float fblock[BLOCK_SIZE];

    offset = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            fblock[offset++] = (i + 1) * (j + 1);

    for (i = 0; i < BLOCK_SIZE; i++)
        printf("%f\t", fblock[i]);
    printf("\n");
}