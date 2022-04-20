#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <pthread.h>
#include <string.h>

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
    // call Fortran to delete the dataset
    if (item != NULL)
        delete_dataset(item, options.cache, strlen(options.cache));
}

int mkcache(const char *dir)
{
    struct stat st = {0};

    // make a cache directory if it does not exist
    if (stat(dir, &st) == -1)
    {
        int stat = mkdir(dir, S_IRWXU | S_IRWXG | S_IRWXO);

        if (stat == -1)
        {
            perror(dir);

            // ignore an existing directory, other threads might have created it in the meantime
            if (errno == EEXIST)
                return 0;
            else
                printf("[C] mkcache errno = %d\n", errno);
        }

        return stat;
    }
    else
        return 0;
}

void rmcache(const char *dir)
{
    GString *cmd = g_string_new("rm -rf ");

    g_string_append_printf(cmd, "%s", dir);

    int ret = system(cmd->str);

    if (ret == 0)
        printf("[C] (non-empty?) directory '%s' deleted successfully.\n", dir);
    else
        printf("[C] Unable to delete directory '%s'.\n", dir);

    g_string_free(cmd, TRUE);
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
    else
        printf("[C] cannot lock datasets_mtx!\n");

    free(id);
}

bool insert_if_not_exists(const char *datasetid, void *item)
{
    bool exists;

    if (pthread_mutex_lock(&datasets_mtx) != 0)
    {
        printf("[C] cannot lock datasets_mtx!\n");
        return false;
    }

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
    {
        printf("[C] cannot lock datasets_mtx!\n");
        item = NULL;
    }

    return item;
}

bool dataset_exists(const char *datasetid)
{
    if (pthread_mutex_lock(&datasets_mtx) != 0)
    {
        printf("[C] cannot lock datasets_mtx!\n");
        return false;
    }

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