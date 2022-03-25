#define DIM 16
#define BLOCK_SIZE (DIM * DIM) /* values per block */
#define significant_bits 7

struct fixed_block_t {
    int16 mask[DIM];
    int8 common_exp;
    int8 mantissa[BLOCK_SIZE];
};

export void encode_float_block(const uniform float fblock[BLOCK_SIZE], uniform struct fixed_block_t compressed[])
{    
    print("DIM: %\n", DIM);
    print("BLOCK_SIZE: %\n", BLOCK_SIZE);

    print("fblock:\n");
    for (uniform int k = 0; k < BLOCK_SIZE; k++)
        print("%\t", fblock[k]);
    print("\n");
}