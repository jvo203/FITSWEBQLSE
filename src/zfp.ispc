typedef unsigned int8 uchar;

#define CHAR_BIT 8

#define MAX_BITS 128
#define ZFP_MAX_PREC 64
#define ZFP_MIN_EXP -1074

#define DIMS 2
#define BLOCK_SIZE (1 << (2 * DIMS)) /* values per block */

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */
#define NBMASK 0xaaaaaaaau             /* negabinary mask */

#define FREXP(x, e) frexp(x, e)
#define FABS(x) abs(x)
#define LDEXP(x, e) ldexp(x, e)

#define MIN(x, y) min(x, y)
#define MAX(x, y) max(x, y)

bool isinf(float tmp)
{
  unsigned int i = intbits(tmp) ;  
  return ( (i & 0x7fffffff) == 0x7f800000 ) || ( (i & 0xffffffff) == 0xff800000 ) ;
}

#define index(i, j) ((i) + 4 * (j))

/* order coefficients (i, j) by i + j, then i^2 + j^2 */
static const uniform uchar PERM[16] = {
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


inline uint precision(int maxexp, uint maxprec, int minexp, int dims)
{
    return MIN(maxprec, (uint)MAX(0, maxexp - minexp + 2 * (dims + 1)));
}

inline int exponent(float x)
{
    int e, val;
    FREXP(x, &e);

    /* clamp exponent in case x is denormal */
    val = (x > 0) ? MAX(e, 1 - EBIAS) : -EBIAS;
   
    return val;
}

inline int exponent_block(const varying float p[16])
{
    float max = 0;

    for(uniform int i=0; i<BLOCK_SIZE; i++)
    {
        float f = FABS(p[i]);
        max = (max < f) ? f : max;
    }

    return exponent(max);
}

inline float quantize(float x, int e)
{
    return LDEXP(x, (CHAR_BIT * (int)sizeof(uniform float) - 2) - e);
}

/* map integer x relative to exponent e to floating-point number */
inline float dequantize(int x, int e)
{
    return LDEXP((float)x, e - (CHAR_BIT * (int)sizeof(uniform float) - 2));
}

/* forward block-floating-point transform to signed integers */
inline void fwd_cast(varying int iblock[BLOCK_SIZE], const varying float fblock[BLOCK_SIZE], varying int emax)
{
    /* compute power-of-two scale factor s */
    float s = quantize(1, emax);

    /* compute p-bit int y = s*x where x is floating and |y| <= 2^(p-2) - 1 */
    for(uniform int i=0; i<BLOCK_SIZE; i++)
        iblock[i] = (int)(s * fblock[i]);
}

/* inverse block-floating-point transform from signed integers */
inline void inv_cast(const varying int iblock[BLOCK_SIZE], varying float fblock[BLOCK_SIZE], varying int emax)
{
    /* compute power-of-two scale factor s */
    float s = dequantize(1, emax);

    /* compute p-bit float x = s*y where |y| <= 2^(p-2) - 1 */
    for(uniform int i=0; i<BLOCK_SIZE; i++)
        fblock[i] = (float)(s * iblock[i]);
}

/* forward lifting transform of 4-vector */
/* efficient gather / scatter */
inline void fwd_lift(varying int p[BLOCK_SIZE], const uniform uint offset, const uniform uint s)
{
    varying int x, y, z, w;
    uniform uint idx = offset;

    x = p[idx];idx += s;
    y = p[idx];idx += s;
    z = p[idx];idx += s;
    w = p[idx];

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

    p[idx] = w;idx -= s;
    p[idx] = z;idx -= s;
    p[idx] = y;idx -= s;
    p[idx] = x;
}

/* inverse lifting transform of 4-vector */
/* efficient gather / scatter */
void inv_lift(varying int p[BLOCK_SIZE], const uniform uint offset, const uniform uint s)
{
    varying int x, y, z, w;
    uniform uint idx = offset;

    x = p[idx];idx += s;
    y = p[idx];idx += s;
    z = p[idx];idx += s;
    w = p[idx];

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

    p[idx] = w;idx -= s;
    p[idx] = z;idx -= s;
    p[idx] = y;idx -= s;
    p[idx] = x;
}

/* map two's complement signed integer to negabinary unsigned integer */
inline uint int2uint(int x)
{
    return ((uint)x + NBMASK) ^ NBMASK;
}

/* map two's complement signed integer to negabinary unsigned integer */
inline int uint2int(uint x)
{
    return (int)((x ^ NBMASK) - NBMASK);
}

/* reorder signed coefficients and convert to unsigned integer */
inline void fwd_order(varying uint ublock[BLOCK_SIZE], const varying int iblock[BLOCK_SIZE], const uniform uchar perm[BLOCK_SIZE])
{
    for(uniform int i=0; i<BLOCK_SIZE; i++)
        ublock[i] = int2uint(iblock[perm[i]]);
}

/* reorder unsigned coefficients and convert to signed integer */
inline void inv_order(const uint *ublock, int *iblock, const uchar *perm, uint n)
{
    do
        iblock[*perm++] = uint2int(*ublock++);
    while (--n);
}

/* forward decorrelating 2D transform */
void fwd_xform(varying int p[BLOCK_SIZE])
{
    uniform uint x, y;

    /* transform along x */
    for (y = 0; y < 4; y++)
        fwd_lift(p, 4 * y, 1);

    /* transform along y */
    for (x = 0; x < 4; x++)
        fwd_lift(p, 1 * x, 4);
}

/* inverse decorrelating 2D transform */
void inv_xform(varying int p[BLOCK_SIZE])
{
    uniform uint x, y;

    /* transform along y */
    for (x = 0; x < 4; x++)
        inv_lift(p, 1 * x, 4);

    /* transform along x */
    for (y = 0; y < 4; y++)
        inv_lift(p, 4 * y, 1);
}

void encode_integer_block(varying int iblock[BLOCK_SIZE], uniform int minbits, uniform int maxbits, varying int maxprec)
{
    uint ublock[BLOCK_SIZE];

    /* perform decorrelating transform */
    fwd_xform(iblock);

    print("iblock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", iblock[k]);
    print("\n");

    /* reorder signed coefficients and convert to unsigned integer */
    fwd_order(ublock, iblock, PERM); // for DIMS == 2

    print("ublock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", ublock[k]);
    print("\n");
}

void encode_float_block(const varying float fblock[BLOCK_SIZE], uniform int minbits, uniform int maxbits, uniform int minexp)
{
    /* compute maximum exponent */
    int emax = exponent_block(fblock);
    int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
    uint e = maxprec ? emax + EBIAS : 0;

    print("emax: %, maxprec: %, e: %\n", emax, maxprec, e);

    int iblock[BLOCK_SIZE];

    /* perform forward block-floating-point transform */
    fwd_cast(iblock, fblock, emax);

    print("iblock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", iblock[k]);
    print("\n");

    encode_integer_block(iblock, minbits, maxbits, maxprec);
}

/* write single bit (must be 0 or 1) */
uint stream_write_bit(varying uint stream[4], uint bit, int pos)
{
    int idx;

    idx = pos >> 3; // divide by 8 to get a byte index

    print("|%|, pos: %, idx: %", bit, pos, idx);

    // left-shift the selected byte only and append the next bit
    stream[idx] = (stream[idx] << 1) | bit;

    return bit;
}

/* write 0 <= n <= 32 low bits of value and return remaining bits */
uint stream_write_bits(varying uint stream[4], uint value, uint n, int pos)
{
    uint i;
    uint ret;

    for (i = 0; i < n; i++, value >>= 1)
        stream_write_bit(stream, value & 1u, pos++);

    return value >> n;
}

export void encode_array(uniform float src[], uniform uchar bitstream[], uniform int minbits, uniform int maxbits, uniform int minexp, uniform int width, uniform int height, uniform int stride)
{
    // process each block in parallel
    foreach(j=0 ... height, i=0 ... width)
    {
        // a private bitstream
        uint stream[4];
        int pos; // a current position in the bitstream

        uint bit, bitmask;
        
        // a private 4x4 block
        float fblock[BLOCK_SIZE];

        bitmask = 0u;

        // copy the source data
        for(int k=0; k<BLOCK_SIZE; k++)
        {
            float tmp = src[k];
            bool nan = isnan(tmp) || isinf(tmp);

            bit = nan ? 1u : 0u;
            bitmask = (bitmask << 1) | bit;

            fblock[k] = nan ? 0.0 : tmp;
        }

        print("bitmask: %\n", bitmask);

        pos = 0;

        cif(bitmask == 0u)
            stream_write_bit(stream, 0u, pos++);
        else
        {
            stream_write_bit(stream, 1u, pos++);

            // write the 16-bit mask too
            stream_write_bits(stream, bitmask, 16, pos);
            pos += 16;
        }

        print("\npos:%\n", pos);

        encode_float_block(fblock, minbits, maxbits, minexp);
    }
}