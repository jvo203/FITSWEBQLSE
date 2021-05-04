#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

#define DIMS 2
#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

#define BLOCK_SIZE 16

#define FREXP(x, e) frexpf(x, e)
#define FABS(x) fabsf(x)

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

unsigned int precision(int maxexp, uint maxprec, int minexp, int dims)
{
    return MIN(maxprec, (uint)MAX(0, maxexp - minexp + 2 * (dims + 1)));
}

int exponent(float x)
{
    if (x > 0)
    {
        int e;
        FREXP(x, &e);
        /* clamp exponent in case x is denormal */
        return MAX(e, 1 - EBIAS);
    }
    return -EBIAS;
}

int exponent_block(const float *p, unsigned int n)
{
    float max = 0;
    do
    {
        float f = FABS(*p++);
        if (max < f)
            max = f;
    } while (--n);
    return exponent(max);
}

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

    int maxbits = 8;
    int rate = maxbits / BLOCK_SIZE;
    int minbits = maxbits;
    int minexp = ZFP_MIN_EXP;

    unsigned int bits = 1;
    /* compute maximum exponent */
    int emax = exponent_block(fblock, BLOCK_SIZE);
    int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
    unsigned int e = maxprec ? emax + EBIAS : 0;

    printf("emax: %d, maxprec: %d, e: %u\n", emax, maxprec, e);
}