#include <stdlib.h>
#include <stdio.h>

#include <Accelerate/Accelerate.h>

extern int get_physical_cores();
extern void resizeNearestSIMD(unsigned char *restrict pSrc, int srcWidth, int srcHeight, unsigned char *restrict pDest, int dstWidth, int dstHeight);

// entry functions from Fortran
extern void resizeLanczos(float *restrict pSrc, int srcWidth, int srcHeight, float *restrict pDest, int dstWidth, int dstHeight, int numLobes)
{
    struct vImage_Buffer src, dst;
    vImage_Flags flags;
    vImage_Error res;

    src.data = pSrc;
    src.height = srcHeight;
    src.width = srcWidth;
    src.rowBytes = srcWidth * sizeof(float);

    dst.data = pDest;
    dst.height = dstHeight;
    dst.width = dstWidth;
    dst.rowBytes = dstWidth * sizeof(float);

    if (numLobes == 5)
        flags = kvImageHighQualityResampling;
    else
        flags = kvImageNoFlags;

    res = vImageScale_PlanarF(&src, &dst, NULL, flags);
}

extern void resizeSuper(float *restrict pSrc, int srcWidth, int srcHeight, float *restrict pDest, int dstWidth, int dstHeight)
{
    // for now pass all calls to a high-quality Lanczos scaling function
    resizeLanczos(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight, 5);
}

/*extern void resizeNearest(unsigned char *restrict pSrc, int srcWidth, int srcHeight, unsigned char *restrict pDest, int dstWidth, int dstHeight)
{
    struct vImage_Buffer src, dst;
    vImage_Error res;

    src.data = pSrc;
    src.height = srcHeight;
    src.width = srcWidth;
    src.rowBytes = srcWidth * sizeof(unsigned char);

    dst.data = pDest;
    dst.height = dstHeight;
    dst.width = dstWidth;
    dst.rowBytes = dstWidth * sizeof(unsigned char);

    // WARNING: for now Lanczos is used instead of the Nearest Neighbour !!!
    res = vImageScale_Planar8(&src, &dst, NULL, kvImageNoFlags);
}*/

/*extern void resizeNearest(unsigned char *restrict pSrc, int srcWidth, int srcHeight, unsigned char *restrict pDest, int dstWidth, int dstHeight)
{
    int num_threads, max_threads;

    const int x_ratio = (int)((srcWidth << 16) / dstWidth) + 1;
    const int y_ratio = (int)((srcHeight << 16) / dstHeight) + 1;

    max_threads = get_physical_cores();

    // restrict the number of threads
    // take into account the fact that there are other parallel 'resizeNearest' calls
    num_threads = (max_threads >= 4) ? 2 : 1;

#pragma omp parallel for shared(x_ratio, y_ratio) num_threads(num_threads)
    for (int i = 0; i < dstHeight; i++)
    {
        const int idx = i * y_ratio;
        const int y2 = idx >> 16;
        const size_t offset_src = y2 * srcWidth;

        int jdx = 0;
        const size_t offset_dst = i * dstWidth;

#pragma GCC ivdep
        for (int j = 0; j < dstWidth; j++)
        {
            int x2 = jdx >> 16;
            jdx += x_ratio;

            pDest[offset_dst + j] = pSrc[offset_src + x2];
        }
    }
}*/

extern void resizeNearest(unsigned char *restrict pSrc, int srcWidth, int srcHeight, unsigned char *restrict pDest, int dstWidth, int dstHeight)
{
    resizeNearestSIMD(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight);
}