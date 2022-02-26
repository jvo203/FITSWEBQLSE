#include <stdio.h>
#include <pthread.h>

#include "hash_table.h"

static GHashTable *datasets;
pthread_mutex_t datasets_mtx;

#include "http.h"
extern options_t options; // <options> is defined in main.c

void init_hash_table()
{
    if (pthread_mutex_init(&datasets_mtx, NULL) != 0)
    {
        perror("mutex_init error");
        exit(1);
    }

    datasets = g_hash_table_new_full(g_str_hash, g_str_equal, free, free_hash_data);
}

void delete_hash_table()
{
    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        g_hash_table_destroy(datasets);
        pthread_mutex_unlock(&datasets_mtx);
    }

    pthread_mutex_destroy(&datasets_mtx);
}

void free_hash_data(gpointer item)
{
    printf("[C] cache dir: %s\n", options.cache);

    // call Fortran to delete the dataset
    if (item != NULL)
        delete_dataset(item);
}

void insert_dataset(const char *datasetid, int len, void *item)
{
    char *id = strndup(datasetid, len);

    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        g_hash_table_replace(datasets, (gpointer)strdup(id), item);
        pthread_mutex_unlock(&datasets_mtx);

        printf("[C] inserted %s into the hash table\n", id);
    }

    free(id);
}

bool insert_if_not_exists(const char *datasetid, void *item)
{
    bool exists;

    if (pthread_mutex_lock(&datasets_mtx) != 0)
        return false;

    if (!g_hash_table_contains(datasets, (gconstpointer)datasetid))
    {
        exists = false;
        g_hash_table_replace(datasets, (gpointer)strdup(datasetid), item);
    }
    else
        exists = true;

    pthread_mutex_unlock(&datasets_mtx);

    return exists;
}

void *get_dataset(const char *datasetid)
{
    void *item;

    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        item = g_hash_table_lookup(datasets, (gconstpointer)datasetid);
        pthread_mutex_unlock(&datasets_mtx);
    }
    else
        item = NULL;

    return item;
}

bool dataset_exists(const char *datasetid)
{
    if (pthread_mutex_lock(&datasets_mtx) != 0)
        return false;

    if (g_hash_table_contains(datasets, (gconstpointer)datasetid))
    {
        pthread_mutex_unlock(&datasets_mtx);
        return true;
    }
    else
    {
        pthread_mutex_unlock(&datasets_mtx);
        return false;
    }
}