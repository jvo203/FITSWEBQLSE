#define DIM 16
#define BLOCK_SIZE (DIM * DIM) /* values per block */
#define significant_bits 7

#define EBITS 8                        /* single-precision floating-point */
#define EBIAS ((1 << (EBITS - 1)) - 1) /* exponent bias */

struct fixed_block_t {
    int16 mask[DIM];
    int8 common_exp;
    int8 mantissa[BLOCK_SIZE];
};

inline int exponent(float x)
{
    int e, val;
    frexp(x, &e);

    /* clamp exponent in case x is denormal */
    val = (x > 0) ? max(e, 1 - EBIAS) : -EBIAS;
   
    return val - 1; // subtract 1 to fit within 8 bits
}

inline uniform int8 exponent_block(const uniform float p[BLOCK_SIZE])
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

    // first the mask (zero for now)
    foreach (k = 0 ... DIM) // (k goes from 0 to DIM - 1)
        compressed[0].mask[k] = 0;

    // get the maximum exponent
    compressed[0].common_exp = exponent_block(fblock);
}