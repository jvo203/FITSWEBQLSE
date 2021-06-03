#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <limits.h>
#include <math.h>

#include <stdint.h>
#include <inttypes.h>

#include <string.h>

#include "../src/zfp.h"

typedef unsigned char uchar;
typedef unsigned int uint;
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

#define index(j, i) ((i) + 4 * (j))

/* order coefficients (i, j) by i + j, then i^2 + j^2 */
static const uchar perm_f[16] = {
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

/* map integer x relative to exponent e to floating-point number */
float dequantize(int x, int e)
{
    return LDEXP((float)x, e - (CHAR_BIT * (int)sizeof(float) - 2));
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

/* inverse block-floating-point transform from signed integers */
void inv_cast(const int *iblock, float *fblock, uint n, int emax)
{
    /* compute power-of-two scale factor s */
    float s = dequantize(1, emax);
    /* compute p-bit float x = s*y where |y| <= 2^(p-2) - 1 */
    do
        *fblock++ = (float)(s * *iblock++);
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

/* private functions ------------------------------------------------------- */

/* inverse lifting transform of 4-vector */
void inv_lift(int *p, uint s)
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
  **       ( 4  6 -4 -1) (x)
  ** 1/4 * ( 4  2  4  5) (y)
  **       ( 4 -2  4 -5) (z)
  **       ( 4 -6 -4  1) (w)
  */
    y += w >> 1;
    w -= y >> 1;
    y += w;
    w <<= 1;
    w -= y;
    z += x;
    x <<= 1;
    x -= z;
    y += z;
    z <<= 1;
    z -= y;
    w += x;
    x <<= 1;
    x -= w;

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

/* map two's complement signed integer to negabinary unsigned integer */
int uint2int(uint x)
{
    return (int)((x ^ NBMASK) - NBMASK);
}

/* reorder signed coefficients and convert to unsigned integer */
void fwd_order(uint *ublock, const int *iblock, const uchar *perm, uint n)
{
    do
        *ublock++ = int2uint(iblock[*perm++]);
    while (--n);
}

/* reorder unsigned coefficients and convert to signed integer */
void inv_order(const uint *ublock, int *iblock, const uchar *perm, uint n)
{
    do
        iblock[*perm++] = uint2int(*ublock++);
    while (--n);
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

/* inverse decorrelating 2D transform */
void inv_xform(int *p)
{
    uint x, y;
    /* transform along y */
    for (x = 0; x < 4; x++)
        inv_lift(p + 1 * x, 4);
    /* transform along x */
    for (y = 0; y < 4; y++)
        inv_lift(p + 4 * y, 1);
}

typedef struct bitstream
{
    uint bits[1024];
    size_t pos;
} bitstream;

/* write single bit (must be 0 or 1) */
uint stream_write_bit(bitstream *s, uint bit)
{
    printf("|%u|", bit);

    s->bits[s->pos++] = bit;

    return bit;
}

/* read single bit (0 or 1) */
uint stream_read_bit(bitstream *s)
{
    return s->bits[s->pos++];
}

/* write 0 <= n <= 64 low bits of value and return remaining bits */
uint64 stream_write_bits(bitstream *s, uint64 value, uint n)
{
    uint i;

    for (i = 0; i < n; i++, value >>= 1)
        stream_write_bit(s, value & 1u);

    return value >> n;
}

/* read 0 <= n <= 64 bits */
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

/* skip over the next n bits (n >= 0) */
void stream_skip(bitstream *stream, uint n)
{
    stream->pos += n;
}

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

/* decompress sequence of size unsigned integers */
uint decode_ints(bitstream *stream, uint maxbits, uint maxprec, uint *data, uint size)
{
    uint intprec = CHAR_BIT * (uint)sizeof(uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n;
    uint64 x;

    /* initialize data array to all zeros */
    for (i = 0; i < size; i++)
        data[i] = 0;

    /* decode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        /* decode first n bits of bit plane #k */
        m = MIN(n, bits);
        bits -= m;
        x = stream_read_bits(stream, m);

        /* unary run-length decode remainder of bit plane */
        for (; n < size && bits && (bits--, stream_read_bit(stream)); x += (uint64)1 << n++)
            for (; n < size - 1 && bits && (bits--, !stream_read_bit(stream)); n++)
                ;

        /* deposit bit plane from x */
        for (i = 0; x; i++, x >>= 1)
            data[i] += (uint)(x & 1u) << k;
    }

    return maxbits - bits;
}

// Assumes little endian
void printBits(size_t const size, void const *const ptr)
{
    unsigned char *b = (unsigned char *)ptr;
    unsigned char byte;
    int i, j;

    for (i = size - 1; i >= 0; i--)
    {
        for (j = 7; j >= 0; j--)
        {
            byte = (b[i] >> j) & 1;
            printf("%u", byte);
        }
    }
    puts("");
}

/* compress sequence of size > 64 unsigned integers */
uint encode_many_ints(bitstream *stream, uint maxbits, uint maxprec, const uint *data, uint size)
{
    uint intprec = CHAR_BIT * (uint)sizeof(uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n, c;

    // visualise all the bits
    for (i = 0; i < size; i++)
        printBits(sizeof(float), &data[i]);

    /* encode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        printf("k:%u, n:%u, bits:%u\t", k, n, bits);
        /* step 1: encode first n bits of bit plane #k */
        m = MIN(n, bits);
        bits -= m;
        printf("m:%u, bits:%u\n", m, bits);
        for (i = 0; i < m; i++)
            stream_write_bit(stream, (data[i] >> k) & 1u);

        /* step 2: count remaining one-bits in bit plane */
        c = 0;
        for (i = m; i < size; i++)
            c += (data[i] >> k) & 1u;
        printf("c:%u\n", c);

        /* step 3: unary run-length encode remainder of bit plane */
        for (; n < size && bits && (--bits, stream_write_bit(stream, !!c)); c--, n++)
        {
            printf("outer loop;");
            for (; n < size - 1 && bits && (--bits, !stream_write_bit(stream, (data[n] >> k) & 1u)); n++)
                printf("inner loop;");
        }

        printf("\n");
    }

    return maxbits - bits;
}

/* decompress sequence of size > 64 unsigned integers */
uint decode_many_ints(bitstream *stream, uint maxbits, uint maxprec, uint *data, uint size)
{
    uint intprec = CHAR_BIT * (uint)sizeof(uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n;

    /* initialize data array to all zeros */
    for (i = 0; i < size; i++)
        data[i] = 0;

    /* decode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        /* decode first n bits of bit plane #k */
        m = MIN(n, bits);
        bits -= m;

        for (i = 0; i < m; i++)
            if (stream_read_bit(stream))
                data[i] += (uint)1 << k;

        /* unary run-length decode remainder of bit plane */
        for (; n < size && bits && (--bits, stream_read_bit(stream)); data[n] += (uint)1 << k, n++)
            for (; n < size - 1 && bits && (--bits, !stream_read_bit(stream)); n++)
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

    printf("iblock:\n");
    for (int i = 0; i < BLOCK_SIZE; i++)
        printf("%d\t", iblock[i]);
    printf("\n");

    /* reorder signed coefficients and convert to unsigned integer */
    fwd_order(ublock, iblock, perm_2, BLOCK_SIZE); // for DIMS == 2

    printf("ublock:\n");
    for (int i = 0; i < BLOCK_SIZE; i++)
        printf("%u\t", ublock[i]);
    printf("\n");

    /* encode integer coefficients */
    /*if (BLOCK_SIZE <= 64)
        bits = encode_ints(stream, maxbits, maxprec, ublock, BLOCK_SIZE);
    else*/
    bits = encode_many_ints(stream, maxbits, maxprec, ublock, BLOCK_SIZE);

    /* write at least minbits bits by padding with zeros */
    if (bits < minbits)
    {
        stream_pad(stream, minbits - bits);
        printf("all-zeroes, padding stream with %d bits\n", minbits - bits);
        bits = minbits;
    }

    return bits;
}
/* decode block of integers */
uint decode_block(bitstream *stream, int minbits, int maxbits, int maxprec, int *iblock)
{
    int bits;
    uint ublock[BLOCK_SIZE];

    /* decode integer coefficients */
    //if (BLOCK_SIZE <= 64)
    //bits = decode_ints(stream, maxbits, maxprec, ublock, BLOCK_SIZE);
    //else
    bits = decode_many_ints(stream, maxbits, maxprec, ublock, BLOCK_SIZE);

    /* read at least minbits bits */
    if (bits < minbits)
    {
        printf("stream_skip %d bits\n", minbits - bits);
        stream_skip(stream, minbits - bits);
        bits = minbits;
    }

    printf("ublock:\n");
    for (int i = 0; i < BLOCK_SIZE; i++)
        printf("%u\t", ublock[i]);
    printf("\n");

    /* reorder unsigned coefficients and convert to signed integer */
    inv_order(ublock, iblock, perm_2, BLOCK_SIZE);

    /* perform decorrelating transform */
    inv_xform(iblock);

    return bits;
}

int main()
{
    uint i, j, k;
    uint offset;

    float fblock[BLOCK_SIZE];
    int iblock[BLOCK_SIZE];

    offset = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            fblock[offset++] = (i + 1) * (j + 1);

    fblock[5] = 6.4f;

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
    memset(stream.bits, 0, 1024 * sizeof(uint));
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
        bits += decode_block(&stream, minbits - bits, maxbits - bits, maxprec, iblock);

        /* perform inverse block-floating-point transform */
        inv_cast(iblock, fblock, BLOCK_SIZE, emax);
    }

    printf("decoded %u bits\n", bits);

    printf("fblock:\n");
    for (i = 0; i < BLOCK_SIZE; i++)
        printf("%f\t", fblock[i]);
    printf("\n");

    printf("perm_2:\n");
    for (i = 0; i < 16; i++)
        printf("%u, ", perm_2[i]);
    printf("\n");

    printf("perm_f:\n");
    for (i = 0; i < 16; i++)
        printf("%u, ", perm_f[i]);
    printf("\n");

    printf("Vectorised ZFP ENCODE\n");

    offset = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            fblock[offset++] = (i + 1) * (j + 1);

    fblock[5] = 6.4f;
    //fblock[3] = 0.0 / 0.0f;

    uint bitstream[4]; // 8 bits per value, a total of 128 bits

    encode_array(fblock, bitstream, minbits, maxbits, minexp, 4, 4);

    // visualise all the bits
    for (i = 0; i < 4; i++)
        printBits(sizeof(uint), &bitstream[i]);

    // a custom array not divisible into 4x4 blocks
    int width = 117;
    int height = 47;
    size_t total_size = width * height;

    float data[total_size];

    for (i = 0; i < total_size; i++)
        data[i] = sinf(2.0f * 3.141529f / 27.0f * (float)i);

    int cn = width / 4;
    int cm = height / 4;

    // the input dimensions might not be divisible by 4
    if (width % 4 != 0)
        cn = cn + 1;

    if (height % 4 != 0)
        cm = cm + 1;

    size_t compressed_size = cn * cm * 4 * sizeof(uint);
    uint compressed[compressed_size];

    printf("width: %d, height: %d\tcn: %d, cm: %d\n", width, height, cn, cm);

    encode_array(data, compressed, minbits, maxbits, minexp, width, height);

    // print out the compressed bitstreams
    for (int j = 0; j < cm; j++)
        for (i = 0; i < cn; i++)
        {
            int bit_idx = j * cn + i;
            int bit_off = bit_idx * 4; // 4 x 32-bit integers per block (128 bits)

            //if (i == 2 && j == 9)
            {
                printf("cn: %d, cm: %d, bit_idx: %d, bit_off: %d\n", i, j, bit_idx, bit_off);

                // visualise all the bits
                for (k = 0; k < 4; k++)
                {
                    printf("%u\t", compressed[bit_off + k]);
                    printBits(sizeof(uint), &compressed[bit_off + k]);
                }
            }
        }

    // the decoder part (partial block decoding, not the whole array)
    int x1 = 14;
    int x2 = 41;
    int y1 = 5;
    int y2 = 40;

    int dimx = (x2 - x1) + 1;
    int dimy = (y2 - y1) + 1;
    size_t view_size = dimx * dimy;

    printf("dimx: %d, dimy: %d\n", dimx, dimy);

    float viewport[view_size];

    for (int k = 0; k < view_size; k++)
        viewport[k] = 0.0f;

    float spectrum = viewport_spectrum_rect(compressed, viewport, minbits, maxbits, minexp, width, height, x1, x2, y1, y2, 0, 1.0f);

    printf("rect. spectrum = %f\n", spectrum);
}