#include <stdlib.h>
#include <stdio.h>

#include <Accelerate/Accelerate.h>

// entry functions from Fortran

extern void resizeLanczos(float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight, int numLobes)
{
    struct vImage_Buffer srcBuffer, dstBuffer;

    srcBuffer.data = pSrc;
    srcBuffer.height = srcHeight;
    srcBuffer.width = srcWidth;
    srcBuffer.rowBytes = srcWidth * sizeof(float);

    dstBuffer.data = pDest;
    dstBuffer.height = dstHeight;
    dstBuffer.width = dstWidth;
    dstBuffer.rowBytes = dstWidth * sizeof(float);
}

extern void resizeSuper(float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight)
{
    // for now pass all calls to a high-quality Lanczos scaling function
    resizeLanczos(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight, 5);
}

extern void resizeNearest(unsigned char *pSrc, int srcWidth, int srcHeight, unsigned char *pDest, int dstWidth, int dstHeight)
{
}