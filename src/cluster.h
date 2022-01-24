#pragma once

#include <glib.h>

void init_cluster();
void delete_cluster();
int get_cluster_size();
void distributed_exit();