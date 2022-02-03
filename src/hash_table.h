#pragma once

#include <stdbool.h>

#include <glib.h>

void init_hash_table();
void delete_hash_table();
void free_hash_data(gpointer item);
void insert_dataset(const char *datasetid, int len, void *item);
bool insert_if_not_exists(const char *datasetid, void *item);
bool dataset_exists(const char *datasetid);
void *get_dataset(const char *datasetid);

// Fortran callbacks
extern void delete_dataset(void *ptr);
extern void get_channel_range_from_C(void *ptr, int *startindex, int *endindex, int *status);
