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
    int numCPU;
    size_t len = sizeof(numCPU);

    /* set the mib for hw.ncpu */
    // mib[0] = CTL_HW;
    //  mib[1] = HW_NCPU; // HW_AVAILCPU; // alternatively, try HW_NCPU;

    // sysctl -n hw.perflevel0.physicalcpu
    // 16

    /* Fill out the mib */
    len = 4;
    int status = sysctlnametomib("hw.perflevel0.physicalcpu", mib, &len);

    if (status != 0)
        perror("sysctlnametomib");
    else
    {
        printf("[C] sysctlnametomib status : %d\n", status);
        for (int i = 0; i < 4; i++)
            printf("mib[%d] : %d\n", i, mib[i]);
        printf("\n");
    }

    len = sizeof(numCPU);

    /* get the number of CPUs from the system */
    if (sysctl(mib, 2, &numCPU, &len, NULL, 0) == -1)
        perror("sysctl");
    else
        printf("[C] sysctl numCPU : %d\n", numCPU);

    if (numCPU < 1)
    {
        mib[0] = CTL_HW;
        mib[1] = HW_NCPU;
        sysctl(mib, 2, &numCPU, &len, NULL, 0);
        if (numCPU < 1)
            numCPU = 1;
    }

    printf("[C] No. of Physical Core(s) : %d\n", numCPU);

    return numCPU;
}
#endif
