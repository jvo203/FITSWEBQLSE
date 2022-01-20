#pragma once

#include <glib.h>

static GSList *cluster = NULL;
static GMutex cluster_mtx;