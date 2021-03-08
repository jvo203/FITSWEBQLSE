#include <stdbool.h>
#include <glib.h>

static GMutex *datasets_mtx;
static GHashTable *datasets;

void init_hash_table();
void delete_hash_table();
void insert_item(const char *datasetid, void *item);
void insert_item_with_replace(const char *datasetid, void *item);
bool item_exists(const char *datasetid);