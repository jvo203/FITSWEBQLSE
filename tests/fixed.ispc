#define BASE 4
#define DIM 16
#define BLOCK_SIZE (DIM * DIM) /* values per block */
#define significant_bits 7

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

struct fixed_block_t {
    int16 mask[DIM];
    int16 common_exp;
    int8 mantissa[BLOCK_SIZE];
};

inline int exponent(float x)
{
    int e, val;
    frexp(x, &e);

    /* clamp exponent in case x is denormal */
    val = (x > 0) ? max(e, 1 - EBIAS) : -EBIAS;
   
    return val;
}

inline uniform int exponent_block(const uniform float p[BLOCK_SIZE])
{
    float max = 0;

    foreach (i=0 ... BLOCK_SIZE)
    {
        float f = abs(p[i]);
        max = (max < f) ? f : max;
    }

    return reduce_max(exponent(max));
}

export void encode_float_block(const uniform float fblock[BLOCK_SIZE], uniform struct fixed_block_t compressed[])
{    
    print("DIM: %\n", DIM);
    print("BLOCK_SIZE: %\n", BLOCK_SIZE);

    print("fblock:\n");
    for (uniform int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", fblock[k]);
    print("\n");

    uniform int emax;

    // first the mask (zero for now)
    foreach (k = 0 ... DIM) // (k goes from 0 to DIM - 1)
        compressed[0].mask[k] = 0;

    emax = exponent_block(fblock);

    // get the maximum exponent
    compressed[0].common_exp = (int16)emax;
    
    //foreach (k = 0 ... BLOCK_SIZE)    
    //    compressed[0].mantissa[k] = (int)(s * fblock[k]);

    varying float scale = ldexp(1.0f, (uniform float)significant_bits - emax);
    print("scale factor = %\n", scale);

    foreach(k=0 ... BLOCK_SIZE)
    {
        // make sure the argument is not 0        
        // compressed[0].mantissa[k] = (fblock[k] != 0.0f) ? (int8)round(ldexp(fblock[k], scale)) : 0;
        compressed[0].mantissa[k] = (int8)round(scale * fblock[k]);
    }

    print("sizeof(fixed_block_t): %\n", sizeof(uniform fixed_block_t));
}

export uniform float reduce_fixed_block_ldexp(uniform struct fixed_block_t compressed[])
{
    varying float sum = 0.0f;
    uniform float reduced = 0.0f;

    uniform int emax = (int)compressed[0].common_exp;
    print("emax: %\n", emax);

    varying float scale = emax - significant_bits;
    print("scale factor = %\n", scale);

    foreach(k=0 ... BLOCK_SIZE)
    {
        varying int8 x = compressed[0].mantissa[k];
        varying float reconstructed = (x != 0) ? ldexp((float)x, scale) : 0.0f;
        sum += reconstructed;

        //print("x: %, scale: %\n", x, scale);
        //print("reconstructed: %\n", reconstructed);
    }

    reduced = reduce_add(sum);
    print("reduced: %\n", reduced);

    return reduced;
}

export uniform float reduce_fixed_block(uniform struct fixed_block_t compressed[])
{
    varying float sum = 0.0f;
    varying int count = 0;

    uniform float reduced = 0.0f;
    uniform int valid = 0;

    uniform int emax = (int)compressed[0].common_exp;
    print("emax: %\n", emax);
    
    varying float scale = ldexp(1.0f, (uniform float)(emax - significant_bits));
    print("scale factor = %\n", scale);

    foreach(k=0 ... BLOCK_SIZE)
    {
        varying float reconstructed = scale * (float)compressed[0].mantissa[k];
        sum += reconstructed;

        // find out which FORTRAN column the value is located
        varying int idx = k >> BASE;
        varying int16 mask = compressed[0].mask[idx];

        count += 1;

        print("reconstructed: %\n", reconstructed);
        print("idx: %\n", idx);
        // print("mask: %\n", mask);
    }

    reduced = reduce_add(sum);
    valid = reduce_add(count);

    print("reduced: %, valid count: %\n", reduced, valid);

    return reduced;
}