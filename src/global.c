#include "global.h"

void init_hash_table()
{
    datasets = g_hash_table_new(g_str_hash, g_str_equal);
}

void delete_hash_table()
{
    g_hash_table_destroy(datasets);
}