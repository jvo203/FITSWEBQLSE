#include <stdlib.h>
#include <stdio.h>

#include <limits.h>
#include <math.h>

#include <stdint.h>
#include <inttypes.h>

#include <string.h>

typedef unsigned char uchar;
typedef int8_t int8;
typedef uint8_t uint8;
typedef int16_t int16;
typedef uint16_t uint16;
typedef int32_t int32;
typedef uint32_t uint32;
typedef int64_t int64;
typedef uint64_t uint64;

#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

#define DIMS 2
#define BLOCK_SIZE (1 << (2 * DIMS)) /* values per block */

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */
#define NBMASK 0xaaaaaaaau             /* negabinary mask */

#define FREXP(x, e) frexpf(x, e)
#define FABS(x) fabsf(x)
#define LDEXP(x, e) ldexpf(x, e)

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define index(i, j) ((i) + 4 * (j))

/* order coefficients (i, j) by i + j, then i^2 + j^2 */
static const uchar perm_2[16] = {
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

uint precision(int maxexp, uint maxprec, int minexp, int dims)
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

int exponent_block(const float *p, uint n)
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
void fwd_cast(int *iblock, const float *fblock, uint n, int emax)
{
    /* compute power-of-two scale factor s */
    float s = quantize(1, emax);
    /* compute p-bit int y = s*x where x is floating and |y| <= 2^(p-2) - 1 */
    do
        *iblock++ = (int)(s * *fblock++);
    while (--n);
}

/* forward lifting transform of 4-vector */
void fwd_lift(int *p, uint s)
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

/* map two's complement signed integer to negabinary unsigned integer */
uint int2uint(int x)
{
    return ((uint)x + NBMASK) ^ NBMASK;
}

/* forward decorrelating 2D transform */
void fwd_xform(int *p)
{
    uint x, y;
    /* transform along x */
    for (y = 0; y < 4; y++)
        fwd_lift(p + 4 * y, 1);
    /* transform along y */
    for (x = 0; x < 4; x++)
        fwd_lift(p + 1 * x, 4);
}

/* reorder signed coefficients and convert to unsigned integer */
void fwd_order(uint *ublock, const int *iblock, const uchar *perm, uint n)
{
    do
        *ublock++ = int2uint(iblock[*perm++]);
    while (--n);
}

typedef struct bitstream
{
    uint bits[1024];
    size_t pos;
} bitstream;

/* write single bit (must be 0 or 1) */
uint stream_write_bit(bitstream *s, uint bit)
{
    s->bits[s->pos++] = bit;

    return bit;
}

/* read single bit (0 or 1) */
uint stream_read_bit(bitstream *s)
{
    return s->bits[s->pos++];
}

uint64 stream_write_bits(bitstream *s, uint64 value, uint n)
{
    uint i;

    for (i = 0; i < n; i++, value >>= 1)
        stream_write_bit(s, value & 1);
}

uint64 stream_read_bits(bitstream *s, uint n)
{
    uint i;
    uint64 value;

    for (i = 0, value = 0; i < n; i++)
        value += (uint64)stream_read_bit(s) << i;

    return value;
}

/* append n zero-bits to stream */
void stream_pad(bitstream *stream, uint n)
{
    uint i;

    for (i = 0; i < n; i++)
        stream->bits[stream->pos++] = 0u;
};

/* compress sequence of size unsigned integers */
uint encode_ints(bitstream *stream, uint maxbits, uint maxprec, const uint *data, uint size)
{
    uint intprec = CHAR_BIT * (uint)sizeof(uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n;
    uint64 x;

    /* encode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        /* step 1: extract bit plane #k to x */
        x = 0;
        for (i = 0; i < size; i++)
            x += (uint64)((data[i] >> k) & 1u) << i;
        /* step 2: encode first n bits of bit plane */
        m = MIN(n, bits);
        bits -= m;
        x = stream_write_bits(stream, x, m);
        /* step 3: unary run-length encode remainder of bit plane */
        for (; n < size && bits && (bits--, stream_write_bit(stream, !!x)); x >>= 1, n++)
            for (; n < size - 1 && bits && (bits--, !stream_write_bit(stream, x & 1u)); x >>= 1, n++)
                ;
    }

    return maxbits - bits;
}

/* encode block of integers */
uint encode_block(bitstream *stream, int minbits, int maxbits, int maxprec, int *iblock)
{
    int bits;
    uint ublock[BLOCK_SIZE];

    /* perform decorrelating transform */
    fwd_xform(iblock);

    /* reorder signed coefficients and convert to unsigned integer */
    fwd_order(ublock, iblock, perm_2, BLOCK_SIZE); // for DIMS == 2

    printf("ublock:\n");
    for (int i = 0; i < BLOCK_SIZE; i++)
        printf("%u\t", ublock[i]);
    printf("\n");

    /* encode integer coefficients */
    //if (BLOCK_SIZE <= 64)
    bits = encode_ints(stream, maxbits, maxprec, ublock, BLOCK_SIZE);
    //else
    //    bits = encode_many_ints(/*stream,*/ maxbits, maxprec, ublock, BLOCK_SIZE);

    /* write at least minbits bits by padding with zeros */
    if (bits < minbits)
    {
        stream_pad(stream, minbits - bits);
        printf("all-zeroes, padding stream with %d\n", minbits - bits);
        bits = minbits;
    }
    return bits;
}

int main()
{
    uint i, j;
    uint offset;

    float fblock[BLOCK_SIZE];
    int iblock[BLOCK_SIZE];

    offset = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            fblock[offset++] = (i + 1) * (j + 1);

    printf("fblock:\n");
    for (i = 0; i < BLOCK_SIZE; i++)
        printf("%f\t", fblock[i]);
    printf("\n");

    double rate = 8.0;
    uint n = 1u << (2 * DIMS);
    uint bits = (uint)floor(n * rate + 0.5);
    bits = MAX(bits, 1 + 8u);

    int minbits = bits;
    int maxbits = bits;
    //int rate = maxbits / BLOCK_SIZE;
    int minexp = ZFP_MIN_EXP;

    printf("rate: %f, bits: %u, rate: %d\n", rate, bits, maxbits / BLOCK_SIZE);

    // BITSTREAM
    bitstream stream;
    memset(stream.bits, 0, 1024);
    stream.pos = 0;

    // --------------------------------------------------
    // ENCODER

    bits = 1;
    /* compute maximum exponent */
    int emax = exponent_block(fblock, BLOCK_SIZE);
    int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
    uint e = maxprec ? emax + EBIAS : 0;

    printf("emax: %d, maxprec: %d, e: %u\n", emax, maxprec, e);

    if (!e)
    {
        // all-zeroes, no need to encode, padding with minbits - bits

        /* write single zero-bit to indicate that all values are zero */
        stream_write_bit(&stream, 0);
        if (minbits > bits)
        {
            stream_pad(&stream, minbits - bits);
            bits = minbits;
        }
        printf("all-zeroes, padding stream with %d\n", minbits - bits);
    }
    else
    {
        // continue encoding the block

        /* encode common exponent; LSB indicates that exponent is nonzero */
        bits += EBITS;
        stream_write_bits(&stream, 2 * e + 1, bits);
        printf("stream_write: %u, bits: %u\n", 2 * e + 1, bits);

        /* perform forward block-floating-point transform */
        fwd_cast(iblock, fblock, BLOCK_SIZE, emax);

        printf("iblock:\n");
        for (i = 0; i < BLOCK_SIZE; i++)
            printf("%d\t", iblock[i]);
        printf("\n");

        bits += encode_block(&stream, minbits - bits, maxbits - bits, maxprec, iblock);
    }

    printf("emitted %u bits:\n", bits);

    for (i = 0; i < bits; i++)
        printf("%u ", stream.bits[i]);
    printf("\n");

    // rewind the bitstream / reset arrays ahead of decoding
    stream.pos = 0;

    for (i = 0; i < BLOCK_SIZE; i++)
    {
        fblock[i] = 0.0f;
        iblock[i] = 0;
    }

    // -------------------------------------------------
    // DECODER
    bits = 1;

    if (!stream_read_bit(&stream))
    {
        printf("setting all values to ZERO\n");

        // set all values to zero
        for (i = 0; i < BLOCK_SIZE; i++)
            fblock[i] = 0.0f;
    }
    else
    {
        int iblock[BLOCK_SIZE];
        int emax;

        /* decode common exponent */
        bits += EBITS;
        emax = (int)stream_read_bits(&stream, EBITS) - EBIAS;
        maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);

        printf("emax: %d, maxprec: %d\n", emax, maxprec);

        /* decode integer block */
        //bits += decode_block(&stream, minbits - bits, maxbits - bits, maxprec, iblock);

        /* perform inverse block-floating-point transform */
        //inv_cast(iblock, fblock, BLOCK_SIZE, emax);
    }

    printf("decoded %u bits:\n", bits);

    printf("fblock:\n");
    for (i = 0; i < BLOCK_SIZE; i++)
        printf("%f\t", fblock[i]);
    printf("\n");
}