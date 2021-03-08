#include <glib.h>

static GMutex *fits_mtx;
static GHashTable *datasets;