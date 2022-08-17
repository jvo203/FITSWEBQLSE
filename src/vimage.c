#include <stdlib.h>
#include <stdio.h>

#include <Accelerate/Accelerate.h>

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

    res = vImageScale_PlanarF(&src, &dst, NULL, kvImageNoFlags);
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

extern void resizeNearest(unsigned char *restrict pSrc, int srcWidth, int srcHeight, unsigned char *restrict pDest, int dstWidth, int dstHeight)
{
    int x_ratio = (int)((srcWidth << 16) / dstWidth) + 1;
    int y_ratio = (int)((srcHeight << 16) / dstHeight) + 1;

    for (int i = 0; i < dstHeight; i++)
    {
        size_t offset_i = i * dstWidth;
        int idx = i * y_ratio;

        for (int j = 0; j < dstWidth; j++)
        {
            int x2 = ((j * x_ratio) >> 16);
            int y2 = (idx >> 16);

            pDest[offset_i + j] = pSrc[(y2 * srcWidth) + x2];
        }
    }
}