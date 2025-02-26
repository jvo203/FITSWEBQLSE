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

// FILE_CHUNK 256KB
#define FILE_CHUNK 262144

extern int get_error_status(void *item);
extern size_t chunked_write_with_chunk(int fd, const char *src, size_t n, size_t chunk); // defined in http.c
extern size_t chunked_read_with_chunk(int fd, const char *src, size_t n, size_t chunk);  // defined in http.c

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

    void *item = NULL;
    gboolean steal = false;
    gpointer stolen_key = NULL;
    gpointer stolen_value = NULL;
    int timeout = 0;
    int error = 0;

    // lock the hash table
    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        // get the item from the hash table
        item = g_hash_table_lookup(datasets, (gconstpointer)id);

        if (item != NULL)
        {
            // re-confirm the timeout
            timeout = dataset_timeout(item, options.timeout);
            error = get_error_status(item);

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

                    struct timeout_arg *arg = (struct timeout_arg *)malloc(sizeof(struct timeout_arg));

                    if (arg != NULL)
                    {
                        arg->ptr = stolen_key;
                        arg->error = error;

                        rc = pthread_create(&tid, &attr, http_propagate_timeout, arg);

                        if (rc != 0)
                        {
                            arg->ptr = NULL;
                            free(arg);

                            free(stolen_key);
                        }
                    }
                    else
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

    void *item = NULL;
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

int read_frame(int fd, void *dst, int frame, int plane, size_t frame_size)
{
    ssize_t bytes_read = pread(fd, dst, frame_size, (size_t)frame * (size_t)plane * frame_size);

    if (bytes_read != (ssize_t)frame_size)
        return -1; // signal an error
    else
        return 0;
}

int chunked_read_frame(int fd, void *dst, size_t frame_size)
{
    size_t bytes_read = chunked_read_with_chunk(fd, dst, frame_size, FILE_CHUNK);

    if (bytes_read != frame_size)
        return -1; // signal an error
    else
        return 0;
}

int write_frame(int fd, void *src, size_t frame_size)
{
    ssize_t bytes_written = write(fd, src, frame_size);

    if (bytes_written != (ssize_t)frame_size)
        return -1; // signal an error
    else
        return 0;
}

int chunked_write_frame(int fd, void *src, size_t frame_size)
{
    size_t bytes_written = chunked_write_with_chunk(fd, src, frame_size, FILE_CHUNK);

    if (bytes_written != frame_size)
        return -1; // signal an error
    else
        return 0;
}

int read_array(const char *file, void *dst, size_t frame_size)
{
    int fd = rdopen(file);

    if (fd < 0)
        return -1; // signal an error

    int stat = read_frame(fd, dst, 0, 0, frame_size);

    // switch to a chunked mode upon failure
    if (stat != 0)
    {
        printf("[C] Switching to a chunked read mode for '%s'.\n", file);

        // reposition the file offset to the beginning (just in case the fd had been used before and failed)
        lseek(fd, 0, SEEK_SET);

        // read the data in a chunked mode
        stat = chunked_read_frame(fd, dst, frame_size);
    }

    close(fd);

    return stat;
}

int write_array(const char *file, void *src, size_t frame_size)
{
    int fd = wropen(file);

    if (fd < 0)
        return -1; // signal an error

    int stat = write_frame(fd, src, frame_size);

    // switch to a chunked mode upon failure
    if (stat != 0)
    {
        printf("[C] Switching to a chunked write mode for '%s'.\n", file);

        // reposition the file offset to the beginning
        lseek(fd, 0, SEEK_SET);

        // write the data in a chunked mode
        stat = chunked_write_frame(fd, src, frame_size);
    }

    close(fd);

    // delete the file upon any write errors
    if (stat != 0)
        remove(file);

    return stat;
}

int mkcache(const char *dir)
{
    if (dir == NULL)
        return -1;

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
    if (dir == NULL)
        return;

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
    if (datasetid == NULL)
        return;

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
    if (datasetid == NULL)
        return false;

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
    void *item = NULL;

    if (datasetid == NULL)
        return NULL;

    if (pthread_mutex_lock(&datasets_mtx) == 0)
    {
        item = g_hash_table_lookup(datasets, (gconstpointer)datasetid);
        pthread_mutex_unlock(&datasets_mtx);
    }
    else
        printf("[C] cannot lock datasets_mtx!\n");

    return item;
}

bool dataset_exists(const char *datasetid)
{
    if (datasetid == NULL)
        return false;

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