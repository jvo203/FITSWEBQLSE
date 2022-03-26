#define DIM 16
#define significant_bits 7

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

struct fixed_block_t {
    int16 mask[DIM];
    int8 common_exp;
    int8 mantissa[DIM][DIM];
};

inline int exponent(float x)
{
    int e, val;
    frexp(x, &e);

    /* clamp exponent in case x is denormal */
    val = (x > 0) ? max(e, 1 - EBIAS) : -EBIAS;
   
    return val;
}

inline uniform int exponent_block(const uniform float p[DIM][DIM])
{
    varying float max = 0;

    foreach_tiled (j = 0 ... DIM, i=0 ... DIM)
    {
        float f = abs(p[i][j]);
        max = (max < f) ? f : max;
    }

    return reduce_max(exponent(max));
}

export void encode_float_block(const uniform float fblock[DIM][DIM], uniform struct fixed_block_t compressed[])
{    
    print("DIM: %\n", DIM);    

    print("fblock:\n");
    for (uniform int j = 0; j < DIM; j++)
        for (uniform int i = 0; i < DIM; i++)
            print("%\t", fblock[i][j]);
    print("\n");

    uniform int emax;

    // first the mask (zero for now)
    foreach (k = 0 ... DIM) // (k goes from 0 to DIM - 1)
        compressed[0].mask[k] = 0;

    emax = exponent_block(fblock);

    // get the maximum exponent
    compressed[0].common_exp = (int8)(emax - 1);
    
    varying float scale = significant_bits - emax;
    print("scale factor = %\n", scale);

    foreach_tiled (j = 0 ... DIM, i=0 ... DIM)
        compressed[0].mantissa[i][j] = (int8)round(ldexp(fblock[i][j], scale));    
}