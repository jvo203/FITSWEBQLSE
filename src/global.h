#include <glib.h>

static GMutex *datasets_mtx;
static GHashTable *datasets;