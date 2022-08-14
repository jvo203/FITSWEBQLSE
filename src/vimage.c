#include <stdlib.h>
#include <stdio.h>

#include <Accelerate/Accelerate.h>

// entry functions from Fortran

extern void resizeLanczos(float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight, int numLobes)
{
    struct vImage_Buffer src, dst;
    vImage_Error res;

    src.data = pSrc;
    src.height = srcHeight;
    src.width = srcWidth;
    src.rowBytes = srcWidth * sizeof(float);

    dst.data = pDest;
    dst.height = dstHeight;
    dst.width = dstWidth;
    dst.rowBytes = dstWidth * sizeof(float);

    res = vImageScale_PlanarF(&src, &dst, NULL, kvImageHighQualityResampling);
}

extern void resizeSuper(float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight)
{
    // for now pass all calls to a high-quality Lanczos scaling function
    resizeLanczos(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight, 5);
}

extern void resizeNearest(unsigned char *pSrc, int srcWidth, int srcHeight, unsigned char *pDest, int dstWidth, int dstHeight)
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

    res = vImageScale_Planar8(&src, &dst, NULL, kvImageHighQualityResampling);
}