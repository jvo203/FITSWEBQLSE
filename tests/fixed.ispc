#define BASE 4
#define DIM (1 << (2 * BASE)) /* block dimensions */
#define BLOCK_SIZE (1 << (2 * DIM)) /* values per block */

export void encode_float_block()
{
    print("BASE: %, DIM: %\n", BASE, DIM);
}