#pragma once

#include "mongoose.h"

void start_ws();
extern void submit_channel_range(void *ptr, int idx, int progress, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);