#define BASE 4
#define DIM (1 << (2 * BASE)) /* block dimensions */
#define BLOCK_SIZE (1 << (2 * DIM)) /* values per block */

void encode_float_bloack()
{
    print("BASE: %, DIM: %\n", BASE, DIM);
}