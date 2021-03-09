#include <stdbool.h>

#include <glib.h>

void init_hash_table();
void delete_hash_table();
void free_hash_data(gpointer item);
void insert_dataset(const char *datasetid, void *item);
bool insert_if_not_exists(const char *datasetid, void *item);
bool dataset_exists(const char *datasetid);
extern void delete_dataset(void *item);