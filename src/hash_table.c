#include <stdio.h>

#include "hash_table.h"

static GMutex datasets_mtx;
static GHashTable *datasets;

void init_hash_table()
{
    g_mutex_init(&datasets_mtx);
    datasets = g_hash_table_new_full(g_str_hash, g_str_equal, free, free_hash_data);
}

void delete_hash_table()
{
    g_hash_table_destroy(datasets);
    g_mutex_clear(&datasets_mtx);
}

void free_hash_data(gpointer item)
{
    // call Fortran to delete the dataset
    if (item != NULL)
        delete_dataset(item);
}

void insert_dataset(const char *datasetid, void *item)
{
    g_mutex_lock(&datasets_mtx);
    g_hash_table_replace(datasets, (gpointer)strdup(datasetid), item);
    g_mutex_unlock(&datasets_mtx);

    printf("[C] inserted %s into the hash table\n", datasetid);
}

bool insert_if_not_exists(const char *datasetid, void *item)
{
    bool exists;

    g_mutex_lock(&datasets_mtx);

    if (!g_hash_table_contains(datasets, (gconstpointer)datasetid))
    {
        exists = false;
        g_hash_table_replace(datasets, (gpointer)strdup(datasetid), item);
    }
    else
        exists = true;

    g_mutex_unlock(&datasets_mtx);

    return exists;
}

void *get_dataset(const char *datasetid)
{
    void *item;

    g_mutex_lock(&datasets_mtx);
    item = g_hash_table_lookup(datasets, (gconstpointer)datasetid);
    g_mutex_unlock(&datasets_mtx);

    return item;
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