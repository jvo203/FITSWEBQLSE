#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
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
        perror("datasets mutex_init error");
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

void garbage_collect()
{
    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        g_hash_table_foreach(datasets, garbage_collect_hash_data, NULL);
        pthread_mutex_unlock(&datasets_mtx);
    }
}

void garbage_collect_hash_data(gpointer id, gpointer item, gpointer userdata)
{
    (void)userdata; // ignore user data

    // check if a dataset has exceeded the timeout
    if (dataset_timeout(item, options.timeout))
    {
        pthread_t tid;
        pthread_attr_t attr; // thread's attribute
        int rc;              // return code

        printf("[C] marking %s for garbage collection.\n", (char *)id);

        rc = pthread_attr_init(&attr);

        if (rc == 0)
        {
            rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

            if (rc == 0)
            {
                // strdup((char *)id), launch a 'delete' pthread in a detached state
                char *key = strdup((char *)id);

                rc = pthread_create(&tid, &attr, delete_hash_data, key);

                if (rc != 0)
                    free(key);
            }

            pthread_attr_destroy(&attr);
        }
    }
}

void *delete_hash_data(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    gpointer id = (gpointer)arg;

    void *item;
    gboolean steal = false;
    gpointer stolen_key = NULL;
    gpointer stolen_value = NULL;
    int timeout = 0;

    // lock the hash table
    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        // get the item from the hash table
        item = g_hash_table_lookup(datasets, (gconstpointer)id);

        if (item != NULL)
        {
            // re-confirm the timeout
            timeout = dataset_timeout(item, options.timeout);

            // remove (steal) the item from the hash table
            if (timeout)
                steal = g_hash_table_steal_extended(datasets, (gconstpointer)id, &stolen_key, &stolen_value);
        }

        // unlock the hash table
        pthread_mutex_unlock(&datasets_mtx);

        // destruct the item
        if (timeout && steal)
        {
            // notify the cluster nodes
            pthread_t tid;
            pthread_attr_t attr; // thread's attribute
            int rc;              // return code

            rc = pthread_attr_init(&attr);

            if (rc == 0)
            {
                rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

                if (rc == 0)
                {
                    // launch a 'delete' pthread in a detached state with the stolen_key
                    // stolen_key will be freeded inside the thread
                    rc = pthread_create(&tid, &attr, http_update_timestamp, stolen_key);

                    if (rc != 0)
                        free(stolen_key);
                }
                else
                    free(stolen_key);

                pthread_attr_destroy(&attr);
            }
            else
                free(stolen_key);

            free_hash_data(stolen_value);
        }
    }

    free(arg);
    pthread_exit(NULL);
}

void *delete_hash_data_no_timeout(void *arg)
{
    if (arg == NULL)
        pthread_exit(NULL);

    gpointer id = (gpointer)arg;

    void *item;
    gboolean steal = false;
    gpointer stolen_key = NULL;
    gpointer stolen_value = NULL;

    // lock the hash table
    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        // get the item from the hash table
        item = g_hash_table_lookup(datasets, (gconstpointer)id);

        if (item != NULL)
        {
            // remove (steal) the item from the hash table
            steal = g_hash_table_steal_extended(datasets, (gconstpointer)id, &stolen_key, &stolen_value);
        }

        // unlock the hash table
        pthread_mutex_unlock(&datasets_mtx);

        // destruct the item
        if (steal)
        {
            free(stolen_key);
            free_hash_data(stolen_value);
        }
    }

    free(arg);
    pthread_exit(NULL);
}

void free_hash_data(gpointer item)
{
    // call Fortran to delete the dataset
    if (item != NULL)
        delete_dataset(item, options.cache, strlen(options.cache), options.threshold);
}

int rdopen(const char *file)
{
    return open(file, O_RDONLY);
}

int wropen(const char *file)
{
    return open(file, O_WRONLY | O_APPEND | O_CREAT, (mode_t)0600);
}

int read_frame(int fd, void *dst, int pos, size_t frame_size)
{
    ssize_t bytes_read = pread(fd, dst, frame_size, pos * frame_size);

    if (bytes_read != (ssize_t)frame_size)
        return -1;
    else
        return 0;
}

int write_frame(int fd, void *src, size_t frame_size)
{
    ssize_t bytes_written = write(fd, src, frame_size);

    if (bytes_written != (ssize_t)frame_size)
        return -1;
    else
        return 0;
}

int write_array(const char *file, void *src, size_t frame_size)
{
    int fd = open(file, O_WRONLY | O_APPEND | O_CREAT, (mode_t)0600);
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
    printf("[C] insert_if_not_exists(%s,%p)\n", datasetid, item);

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
        printf("[C] '%s' does not exist.\n", datasetid);
        pthread_mutex_unlock(&datasets_mtx);
        return false;
    }
}