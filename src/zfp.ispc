typedef unsigned int8 uchar;

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

#define index(i, j) ((i) + 4 * (j))

/* order coefficients (i, j) by i + j, then i^2 + j^2 */
static const uchar PERM[16] = {
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
    if (x > 0)
    {
        int e;
        FREXP(x, &e);
        /* clamp exponent in case x is denormal */
        return MAX(e, 1 - EBIAS);
    }

    return -EBIAS;
}

inline int exponent_block(const float *p, uint n)
{
    float max = 0;

    for(int i=0; i<n; i++)
    {
        float f = FABS(p[i]);

        if (max < f)
            max = f;
    }

    /*do
    {
        float f = FABS(*p++);

        if (max < f)
            max = f;
    } while (--n);*/

    return exponent(max);
}

export void vec_encode_block(uniform float fblock[], uniform int iblock[], uniform uint ublock[], uniform int minbits, uniform int maxbits, uniform int minexp, uniform int width, uniform int height)
{
    // process each block in parallel
    foreach(j=0 ... height, i=0 ... width)
    {
        /* compute maximum exponent */
        int emax = exponent_block(fblock, BLOCK_SIZE);
        int maxprec = precision(emax, ZFP_MAX_PREC, minexp, DIMS);
        uint e = maxprec ? emax + EBIAS : 0;

        print("block (%,%), emax: %, maxprec: %, e: %\n", i, j, emax, maxprec, e);
    }
}