#include <stdlib.h>
#include <stdio.h>

#include <limits.h>
#include <math.h>

#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

#define DIMS 2
#define BLOCK_SIZE (1 << (2 * DIMS)) /* values per block */

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

#define FREXP(x, e) frexpf(x, e)
#define FABS(x) fabsf(x)
#define LDEXP(x, e) ldexpf(x, e)

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

unsigned int precision(int maxexp, unsigned int maxprec, int minexp, int dims)
{
    return MIN(maxprec, (unsigned int)MAX(0, maxexp - minexp + 2 * (dims + 1)));
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

float quantize(float x, int e)
{
    return LDEXP(x, (CHAR_BIT * (int)sizeof(float) - 2) - e);
}

/* forward block-floating-point transform to signed integers */
void fwd_cast(int *iblock, const float *fblock, unsigned int n, int emax)
{
    /* compute power-of-two scale factor s */
    float s = quantize(1, emax);
    /* compute p-bit int y = s*x where x is floating and |y| <= 2^(p-2) - 1 */
    do
        *iblock++ = (int)(s * *fblock++);
    while (--n);
}

/* forward lifting transform of 4-vector */
void fwd_lift(int *p, unsigned int s)
{
    int x, y, z, w;
    x = *p;
    p += s;
    y = *p;
    p += s;
    z = *p;
    p += s;
    w = *p;
    p += s;

    /*
  ** non-orthogonal transform
  **        ( 4  4  4  4) (x)
  ** 1/16 * ( 5  1 -1 -5) (y)
  **        (-4  4  4 -4) (z)
  **        (-2  6 -6  2) (w)
  */
    x += w;
    x >>= 1;
    w -= x;
    z += y;
    z >>= 1;
    y -= z;
    x += z;
    x >>= 1;
    z -= x;
    w += y;
    w >>= 1;
    y -= w;
    w += y >> 1;
    y -= w >> 1;

    p -= s;
    *p = w;
    p -= s;
    *p = z;
    p -= s;
    *p = y;
    p -= s;
    *p = x;
}

/* forward decorrelating 2D transform */
void fwd_xform(int *p)
{
    unsigned int x, y;
    /* transform along x */
    for (y = 0; y < 4; y++)
        fwd_lift(p + 4 * y, 1);
    /* transform along y */
    for (x = 0; x < 4; x++)
        fwd_lift(p + 1 * x, 4);
}

#define index(i, j) ((i) + 4 * (j))

/* order coefficients (i, j) by i + j, then i^2 + j^2 */
static const unsigned char perm_2[16] = {
    index(0, 0), /*  0 : 0 */

    index(1, 0), /*  1 : 1 */
    index(0, 1), /*  2 : 1 */

    index(1, 1), /*  3 : 2 */

    index(2, 0), /*  4 : 2 */
    index(0, 2), /*  5 : 2 */

    index(2, 1), /*  6 : 3 */
    index(1, 2), /*  7 : 3 */

    index(3, 0), /*  8 : 3 */
    index(0, 3), /*  9 : 3 */

    index(2, 2), /* 10 : 4 */

    index(3, 1), /* 11 : 4 */
    index(1, 3), /* 12 : 4 */

    index(3, 2), /* 13 : 5 */
    index(2, 3), /* 14 : 5 */

    index(3, 3), /* 15 : 6 */
};

#undef index

/* encode block of integers */
unsigned int encode_block(int minbits, int maxbits, int maxprec, int *iblock)
{
    int bits;
    unsigned int ublock[BLOCK_SIZE];

    /* perform decorrelating transform */
    fwd_xform(iblock);

    /* reorder signed coefficients and convert to unsigned integer */
    //fwd_order(ublock, iblock, perm_2, BLOCK_SIZE); // for DIMS == 2

    /* encode integer coefficients */
    //if (BLOCK_SIZE <= 64)
    //    bits = encode_ints(/*stream,*/ maxbits, maxprec, ublock, BLOCK_SIZE);
    //else
    //    bits = encode_many_ints(/*stream,*/ maxbits, maxprec, ublock, BLOCK_SIZE);

    /* write at least minbits bits by padding with zeros */
    if (bits < minbits)
    {
        //stream_pad(stream, minbits - bits);
        printf("all-zeroes, padding stream with %d\n", minbits - bits);
        bits = minbits;
    }
    return bits;
}

int main()
{
    int i, j;
    int offset;

    float fblock[BLOCK_SIZE];
    int iblock[BLOCK_SIZE];

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

    if (!e)
    {
        // all-zeroes, no need to encode, padding with minbits - bits
        //stream_pad(stream, minbits - bits);
        printf("all-zeroes, padding stream with %d\n", minbits - bits);
    }
    else
    {
        // continue encoding the block

        /* encode common exponent; LSB indicates that exponent is nonzero */
        bits += EBITS;
        printf("stream_write: %u, bits: %u\n", 2 * e + 1, bits);

        /* perform forward block-floating-point transform */
        fwd_cast(iblock, fblock, BLOCK_SIZE, emax);

        for (i = 0; i < BLOCK_SIZE; i++)
            printf("%d\t", iblock[i]);
        printf("\n");

        bits += encode_block(minbits - bits, maxbits - bits, maxprec, iblock);
    }
}