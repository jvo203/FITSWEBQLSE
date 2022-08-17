#include <stdio.h>

#if !defined(__APPLE__) || !defined(__MACH__)
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
#else
// on macOS
#include <sys/sysctl.h>

int get_physical_cores()
{
    int mib[4];
    int numCPU = 0;
    size_t len = sizeof(numCPU);

    // on the Apple M1 Ultra
    // sysctl -n hw.perflevel0.physicalcpu
    // 16

    // get the number of Apple Silicon performance cores
    if (sysctlbyname("hw.perflevel0.physicalcpu", &numCPU, &len, NULL, 0) == -1)
        perror("sysctlbyname");

    if (numCPU < 1)
    {
        /* set the mib for hw.ncpu */
        mib[0] = CTL_HW;
        mib[1] = HW_NCPU;
        sysctl(mib, 2, &numCPU, &len, NULL, 0);
        if (numCPU < 1)
            numCPU = 1;
    }

    // printf("[C] No. of Performance Core(s) : %d\n", numCPU);

    return numCPU;
}
#endif
