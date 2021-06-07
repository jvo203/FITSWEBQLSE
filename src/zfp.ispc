typedef unsigned int8 uchar;

#define CHAR_BIT 8

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
inline void inv_order(const varying uint ublock[BLOCK_SIZE], varying int iblock[BLOCK_SIZE], const uniform uchar perm[BLOCK_SIZE])
{
    for(uniform int i=0; i<BLOCK_SIZE; i++)
        iblock[perm[i]] = uint2int(ublock[i]);
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

/* write single bit (must be 0 or 1) */
// was varying uint stream[4]
uint stream_write_bit(uniform uint *stream, int bit_off, int pos, uint bit)
{
    int idx;

    idx = pos >> 5; // divide by 32 to get an int index

    // print("stream_write_bit|%|, pos: %, idx: %\n", bit, pos, idx);

    // left-shift the selected byte only and append the next bit
    stream[bit_off + idx] = (stream[bit_off + idx] << 1) | bit;

    return bit;
}


/* read single bit (must be 0 or 1) */
uint stream_read_bit(uniform uint *stream, int bit_off, int pos)
{
    uint bit;
    int idx;

    idx = pos >> 5; // divide by 32 to get an int index
    int shift = 31 - pos % 32; 

    // left-shift the selected byte only and append the next bit
    bit = stream[bit_off + idx] & (1u << shift);

    // print("stream_read_bit|%|, pos: %, idx: %, shift: %\n", bit, pos, idx, shift);

    return (bit > 0) ? 1u : 0u;
}

/* write 0 <= n <= 32 low bits of value and return remaining bits */
uint stream_write_bits(uniform uint *stream, int bit_off, int pos, uint value, uint n)
{
    uint i;
    uint ret;

    for (i = 0; i < n; i++, value >>= 1)
        stream_write_bit(stream, bit_off, pos++, value & 1u);

    return value >> n;
}

/* read 0 <= n <= 32 bits */
uint stream_read_bits(uniform uint *stream, int bit_off, int pos, uint n)
{
    uint i, value;
    
    for (i = 0, value = 0; i < n; i++)
        value += stream_read_bit(stream, bit_off, pos++) << i;

    return value;
}

/* append n zero-bits to stream */
void stream_pad(uniform uint *stream, int bit_off, int pos, uint n)
{
    uint i;

    for (i = 0; i < n; i++)
        stream_write_bit(stream, bit_off, pos++, 0u);
};


/* compress sequence of size > 64 unsigned integers */
int encode_many_ints(uniform uint *stream, int bit_off, int pos, uint maxbits, uint maxprec, varying const uint data[BLOCK_SIZE])
{
    uint intprec = CHAR_BIT * (uint)sizeof(uniform uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n, c;

    /* encode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        // print("k:%, n:%, bits:%\t", k, n, bits);
        /* step 1: encode first n bits of bit plane #k */
        m = MIN(n, bits);
        bits -= m;
        // print("m:%, bits:%\n", m, bits);
        for (i = 0; i < m; i++)
            stream_write_bit(stream, bit_off, pos++, (data[i] >> k) & 1u);

        /* step 2: count remaining one-bits in bit plane */
        c = 0;
        for (i = m; i < BLOCK_SIZE; i++)
            c += (data[i] >> k) & 1u;
        // print("c:%\n", c);

        /* step 3: unary run-length encode remainder of bit plane */
        for (; n < BLOCK_SIZE && bits && (--bits, stream_write_bit(stream, bit_off, pos++, !!c)); c--, n++)
            for (; n < BLOCK_SIZE - 1 && bits && (--bits, !stream_write_bit(stream, bit_off, pos++, (data[n] >> k) & 1u)); n++) ;
    }

    return pos;
}

/* decompress sequence of size > 64 unsigned integers */
void decode_many_ints(varying uint data[BLOCK_SIZE], uniform uint *stream, int bit_off, int pos, uint maxbits, uint maxprec)
{
    uint intprec = CHAR_BIT * (uint)sizeof(uniform uint);
    uint kmin = intprec > maxprec ? intprec - maxprec : 0;
    uint bits = maxbits;
    uint i, k, m, n;

    /* initialize data array to all zeros */
    for (i = 0; i < BLOCK_SIZE; i++)
        data[i] = 0;

    /* decode one bit plane at a time from MSB to LSB */
    for (k = intprec, n = 0; bits && k-- > kmin;)
    {
        // print("k:%, n:%, bits:%\t", k, n, bits);

        /* decode first n bits of bit plane #k */
        m = MIN(n, bits);
        bits -= m;

        for (i = 0; i < m; i++)
            if (stream_read_bit(stream, bit_off, pos++))
                data[i] += (uint)1 << k;

        /* unary run-length decode remainder of bit plane */
        for (; n < BLOCK_SIZE && bits && (--bits, stream_read_bit(stream, bit_off, pos++)); data[n] += (uint)1 << k, n++)
            for (; n < BLOCK_SIZE - 1 && bits && (--bits, !stream_read_bit(stream, bit_off, pos++)); n++)
                ;
    }

    /*print("data:\n");
    for (k = 0; k < BLOCK_SIZE; k++)
        print("%\t", data[k]);
    print("\n");*/
}

void encode_integer_block(varying int iblock[BLOCK_SIZE], uniform int minbits, uniform int maxbits, varying int maxprec, uniform uint *stream, int bit_off, int pos)
{
    uint ublock[BLOCK_SIZE];

    /* perform decorrelating transform */
    fwd_xform(iblock);

    /* print("iblock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", iblock[k]);
    print("\n"); */

    /* reorder signed coefficients and convert to unsigned integer */
    fwd_order(ublock, iblock, PERM); // for DIMS == 2

    /* print("ublock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", ublock[k]);
    print("\n"); */

    pos = encode_many_ints(stream, bit_off, pos, maxbits - pos, maxprec, ublock);

    // print("\npos:%, minbits:%\n", pos, minbits);

    /* write at least minbits bits by padding with zeros */
    cif (pos < minbits)
    {
        stream_pad(stream, bit_off, pos, minbits - pos);
        // print("all-zeroes, padding stream with % bits\n", minbits - pos);
    }
}

void decode_integer_block(varying int iblock[BLOCK_SIZE], uniform uint *stream, int bit_off, int pos, uniform int minbits, uniform int maxbits, varying int maxprec)
{
    uint ublock[BLOCK_SIZE];

    decode_many_ints(ublock, stream, bit_off, pos, maxbits - pos, maxprec);

    /*print("ublock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", ublock[k]);
    print("\n");*/

    /* reorder unsigned coefficients and convert to signed integer */
    inv_order(ublock, iblock, PERM);

    /* perform decorrelating transform */
    inv_xform(iblock);
}

void encode_float_block(const varying float fblock[BLOCK_SIZE], uniform int minbits, uniform int maxbits, uniform int minexp, uniform uint *stream, int bit_off, int pos)
{
    /* compute maximum exponent */
    int emax = exponent_block(fblock);
    int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
    uint e = maxprec ? emax + EBIAS : 0;

    // print("emax: %, maxprec: %, e: %\n", emax, maxprec, e);

    int iblock[BLOCK_SIZE];

    /* perform forward block-floating-point transform */
    fwd_cast(iblock, fblock, emax);

    /* print("iblock:\n");
    for (int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", iblock[k]);
    print("\n"); */

    // emit the 8-bit exponent
    //stream_write_bits(stream, bit_off, pos, 2 * e + 1, EBITS);
    stream_write_bits(stream, bit_off, pos, emax + EBIAS, EBITS);
    pos += EBITS;

    encode_integer_block(iblock, minbits, maxbits, maxprec, stream, bit_off, pos);
}

void decode_float_block(varying float fblock[BLOCK_SIZE], uniform uint *stream, int bit_off, int pos, uniform int minbits, uniform int maxbits, varying int maxprec, varying int emax)
{
    int iblock[BLOCK_SIZE];

    decode_integer_block(iblock, stream, bit_off, pos, minbits, maxbits, maxprec);
    
    /* perform inverse block-floating-point transform */
    inv_cast(iblock, fblock, emax);
}

export void encode_array(uniform float src[], uniform uint bitstream[], uniform int minbits, uniform int maxbits, uniform int minexp, uniform int width, uniform int height, uniform float ignrval, uniform float datamin, uniform float datamax)
{
    uniform int cn, cm;

    cn = width / 4;
    cm = height / 4;

    // the input dimensions might not be divisible by 4
    if (width % 4 != 0)
        cn = cn + 1;

    if (height % 4 != 0)
        cm = cm + 1;

    // print("ISPC:\tcn: %, cm: %\n", cn, cm);

    // process each block in parallel
    foreach(j=0 ... cm, i=0 ... cn)
    {
        // bitstream array destination index / offset
        int bit_idx = j * cn + i;
        int bit_off = bit_idx * 4; // 4 x 32-bit integers per block (128 bits)

        int pos; // a current position in the bitstream

        uint bit, bitmask;
        
        // a private 4x4 block
        float fblock[BLOCK_SIZE];

        bitmask = 0u;

        // 4x4 destination offset      
        uniform int dst_offset = 0;

        int src_x = i * 4;
        int src_y = j * 4;

        // print("src_x: %, src_y: %\n", src_x, src_y);

        // copy the source data
        for(uniform int iy=0;iy<4;iy++)
            for(uniform int ix=0;ix<4;ix++)
            {                
                float tmp;
                int src_offset = (src_y + iy) * width + (src_x + ix);

                // print("ix: %, iy: %, src_offset: %\n", ix, iy, src_offset);

                cif((src_x + ix) < width && (src_y + iy) < height)
                {                                        
                    tmp = src[src_offset];
                }
                else
                    tmp = 0.0; // non-existent edge values are filled with 0.0
            
                bool nan = isnan(tmp) || isinf(tmp) || (tmp <= ignrval) || (tmp < datamin) || (tmp > datamax) ;

                bit = nan ? 1u : 0u;
                bitmask = (bitmask << 1) | bit;

                fblock[dst_offset++] = nan ? 0.0 : tmp;
            }            

        /*for(int k=0; k<BLOCK_SIZE; k++)
        {
            float tmp = src[k];
            bool nan = isnan(tmp) || isinf(tmp);

            bit = nan ? 1u : 0u;
            bitmask = (bitmask << 1) | bit;

            fblock[k] = nan ? 0.0 : tmp;
        }*/
        
        // reset the stream position
        pos = 0;

        // zero-out the bitstream
        bitstream[bit_off + 0] = 0;
        bitstream[bit_off + 1] = 0;
        bitstream[bit_off + 2] = 0;
        bitstream[bit_off + 3] = 0;

        cif(bitmask == 0u)
            stream_write_bit(bitstream, bit_off, pos++, 0u);
        else
        {
            //print("cn: %, cm: %, bit_idx: %, bit_off: %, bitmask: %\n", i, j, bit_idx, bit_off, bitmask);

            stream_write_bit(bitstream, bit_off, pos++, 1u);

            // write the 16-bit mask too
            stream_write_bits(bitstream, bit_off, pos, bitmask, 16);
            pos += 16;
        }

        encode_float_block(fblock, minbits, maxbits, minexp, bitstream, bit_off, pos);

        //print("bitmask: %\n", bitmask);        
        //print("cn: %, cm: %, bit_idx: %, bit_off: %, bitmask: %\n", i, j, bit_idx, bit_off, bitmask);
        /*print("\t%\n", bitstream[bit_off + 0]);
        print("\t%\n", bitstream[bit_off + 1]);
        print("\t%\n", bitstream[bit_off + 2]);
        print("\t%\n", bitstream[bit_off + 3]);*/

    }
}

export uniform float viewport_spectrum_rect(uniform uint bitstream[], uniform float dst[], uniform int minbits, uniform int maxbits, uniform int minexp, uniform int width, uniform int height, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
{
    uniform int cn, cm;

    cn = width / 4;
    cm = height / 4;

    // the input dimensions might not be divisible by 4
    if (width % 4 != 0)
        cn = cn + 1;

    if (height % 4 != 0)
        cm = cm + 1;

    uniform int cn_start, cn_end;
    uniform int cm_start, cm_end;

    cn_start = x1 / 4;
    cn_end = x2 / 4;
    cm_start = y1 / 4;
    cm_end = y2 / 4;

    print("compressed block range: cn[%,%]; cm[%,%]\n", cn_start, cn_end, cm_start, cm_end);

    varying float partial_sum = 0.0f ;
    varying unsigned int64 partial_count = 0 ;

    // process each block in parallel
    foreach(j=cm_start ... cm_end + 1, i=cn_start ... cn_end + 1)
    {
        // bitstream array destination index / offset
        int bit_idx = j * cn + i;
        int bit_off = bit_idx * 4; // 4 x 32-bit integers per block (128 bits)

        int pos; // a current position in the bitstream

        uint bit, bitmask;
        
        // a private 4x4 block
        float fblock[BLOCK_SIZE];

        bitmask = 0u;
        
        // reset the stream position
        pos = 0;

        // read the first bit
        bit = stream_read_bit(bitstream, bit_off, pos++);

        if(bit)
        {
            // read the 16-bit mask
            bitmask = stream_read_bits(bitstream, bit_off, pos, 16);
            pos += 16;
        }

        print("cn: %, cm: %, bit_idx: %, bit_off: %, bitmask: %\n", i, j, bit_idx, bit_off, bitmask);

        int emax = (int)stream_read_bits(bitstream, bit_off, pos, EBITS) - EBIAS;
        int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
        pos += EBITS;

        // print("emax: %, maxprec: %, pos: %\n", emax, maxprec, pos);
       
        decode_float_block(fblock, bitstream, bit_off, pos, minbits, maxbits, maxprec, emax);

        /*print("fblock:\n");
        for (int k = 0; k < BLOCK_SIZE; k++)
            print("%\t", fblock[k]);
        print("\n");*/

        // 4x4 destination offset      
        uniform int offset = BLOCK_SIZE;

        int src_x = i * 4;
        int src_y = j * 4;

        // print("src_x: %, src_y: %\n", src_x, src_y);

        // go throught the 4x4 block
        for(uniform int iy=3;iy>=0;iy--)
            for(uniform int ix=3;ix>=0;ix--)
            {                
                float tmp;

                int pixel_x = src_x + ix;
                int pixel_y = src_y + iy;

                bit = bitmask & 1u;
                bitmask >>= 1;

                // not-a-NaN
                cif(!bit)
                {
                    tmp = fblock[--offset];

                    partial_sum += tmp ;
	                partial_count += 1 ;
                }
            }

    }

    uniform float sum = reduce_add(partial_sum) ;
    uniform int64 count = reduce_add(partial_count) ;

    if(count > 0)
    {
        if(average)
	        return sum / (float) count ;
        else
	        return sum * cdelt3 ;
    }
    else
        return 0.0f ;
}