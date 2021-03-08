#include "global.h"

void init_hash_table()
{
    g_mutex_init(datasets_mtx);
    datasets = g_hash_table_new(g_str_hash, g_str_equal);
}

void delete_hash_table()
{
    g_hash_table_destroy(datasets);
    g_mutex_clear(datasets_mtx);
}

void insert_item(const char *datasetid, void *item)
{
    g_mutex_lock(datasets_mtx);
    g_hash_table_insert(datasets, datasetid, item);
    g_mutex_unlock(datasets_mtx);
}

void insert_item_with_replace(const char *datasetid, void *item)
{
    g_mutex_lock(datasets_mtx);
    g_hash_table_replace(datasets, datasetid, item);
    g_mutex_unlock(datasets_mtx);
}