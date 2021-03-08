#include "hash_table.h"

static GMutex datasets_mtx;
static GHashTable *datasets;

void init_hash_table()
{
    g_mutex_init(&datasets_mtx);
    datasets = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, free_hash_data);
}

void delete_hash_table()
{
    g_hash_table_destroy(datasets);
    g_mutex_clear(&datasets_mtx);
}

void free_hash_data(gpointer item)
{
    if (item == NULL)
        return;

    // call Fortran to delete the dataset
    delete_dataset(item);
}

void insert_dataset(const char *datasetid, void *item)
{
    g_mutex_lock(&datasets_mtx);
    g_hash_table_insert(datasets, (gpointer)datasetid, item);
    g_mutex_unlock(&datasets_mtx);
}

void insert_dataset_with_replace(const char *datasetid, void *item)
{
    g_mutex_lock(&datasets_mtx);
    g_hash_table_replace(datasets, (gpointer)datasetid, item);
    g_mutex_unlock(&datasets_mtx);
}

bool dataset_exists(const char *datasetid)
{
    g_mutex_lock(&datasets_mtx);

    if (g_hash_table_contains(datasets, (gconstpointer)datasetid))
    {
        g_mutex_unlock(&datasets_mtx);
        return true;
    }
    else
    {
        g_mutex_unlock(&datasets_mtx);
        return false;
    }
}