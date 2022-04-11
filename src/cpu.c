#include <stdio.h>

#include <libcpuid.h>

int get_physical_cores()
{
    struct cpu_raw_data_t raw;
    struct cpu_id_t data;

    cpuid_get_raw_data(&raw);
    cpu_identify(&raw, &data);

    // printf("[C] No. of Physical Core(s) : %d\n", data.num_cores);

    return data.num_cores;
}
