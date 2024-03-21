#pragma once

#include <stdbool.h>

#include <glib.h>

struct timeout_arg
{
    void *ptr;
    int error;
};

void init_hash_table();
void delete_hash_table();
void garbage_collect();
void free_dataset(gpointer item);
void free_hash_data(gpointer item);
void *delete_hash_data(void *arg);
void *delete_hash_data_no_timeout(void *arg);
void garbage_collect_hash_data(gpointer id, gpointer item, gpointer userdata);
void insert_dataset(const char *datasetid, int len, void *item);
bool insert_if_not_exists(const char *datasetid, void *item);
bool dataset_exists(const char *datasetid);
void *get_dataset(const char *datasetid);
int mkcache(const char *dir);
void rmcache(const char *dir);

int rdopen(const char *file);
int wropen(const char *file);
int read_frame(int fd, void *dst, int pos, size_t frame_size);
int write_frame(int fd, void *src, size_t frame_size);
int read_array(const char *file, void *dst, size_t frame_size);
int write_array(const char *file, void *src, size_t frame_size);

// Fortran callbacks
extern void delete_dataset(void *ptr, char *dir, size_t len, int threshold);
extern int dataset_timeout(void *ptr, int timeout);
extern void get_channel_range_C(void *ptr, int progress, int *startindex, int *endindex, int *status);
extern void update_progress_C(void *ptr, int progress);
extern void calculate_global_statistics_C(void *ptr, float dmedian, float *sumP, int64_t *countP, float *sumN, int64_t *countN, int first, int last);