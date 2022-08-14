#include <stdlib.h>
#include <stdio.h>

#include <Accelerate/Accelerate.h>

// entry functions from Fortran

extern void resizeLanczos(const float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight, int numLobes)
{
}

extern void resizeSuper(const float *pSrc, int srcWidth, int srcHeight, float *pDest, int dstWidth, int dstHeight)
{
}

extern void resizeNearest(const unsigned char *pSrc, int srcWidth, int srcHeight, unsigned char *pDest, int dstWidth, int dstHeight)
{
}