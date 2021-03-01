#include <stdlib.h>
#include <stdio.h>

#include <ipp.h>

IppStatus resizeNearest8u_C1R(Ipp8u *pSrc, IppiSize srcSize, Ipp32s srcStep,
                              Ipp8u *pDst, IppiSize dstSize, Ipp32s dstStep)
{
    IppiResizeSpec_32f *pSpec = 0;
    int specSize = 0, initSize = 0, bufSize = 0;
    Ipp8u *pBuffer = 0;
    Ipp32u numChannels = 1;
    IppiPoint dstOffset = {0, 0};
    IppiPoint srcOffset = {0, 0};
    IppStatus status = ippStsNoErr;
    IppStatus pStatus;

    /* Spec and init buffer sizes */
    status = ippiResizeGetSize_8u(srcSize, dstSize, ippNearest, 0, &specSize, &initSize);

    if (status != ippStsNoErr)
        return status;

    /* Memory allocation */
    pSpec = (IppiResizeSpec_32f *)ippsMalloc_8u(specSize);

    if (pSpec == NULL)
    {
        ippsFree(pSpec);
        return ippStsNoMemErr;
    }

    /* Filter initialization */
    status = ippiResizeNearestInit_8u(srcSize, dstSize, pSpec);

    if (status != ippStsNoErr)
    {
        ippsFree(pSpec);
        return status;
    }

    /* General transform function */
    ippiResizeGetBufferSize_8u(pSpec, dstSize, ippC1, &bufSize);

    pBuffer = ippsMalloc_8u(bufSize);

    if (pBuffer)
    {
        Ipp8u *pOneBuf;

        pStatus = ippiResizeGetSrcRoi_8u(pSpec, dstOffset, dstSize,
                                         &srcOffset, &srcSize);

        if (pStatus == ippStsNoErr)
        {
            pOneBuf = pBuffer;

            pStatus = ippiResizeNearest_8u_C1R(
                pSrc, srcStep * sizeof(Ipp32f), pDst, dstStep * sizeof(Ipp32f),
                dstOffset, dstSize, pSpec, pOneBuf);
        }
    }

    ippsFree(pSpec);

    if (pBuffer == NULL)
        return ippStsNoMemErr;

    ippsFree(pBuffer);

    /* Return bad status */
    if (pStatus != ippStsNoErr)
        return pStatus;

    return status;
}

IppStatus resizeLanczos32f_C1R(Ipp32f *pSrc, IppiSize srcSize, Ipp32s srcStep,
                               Ipp32f *pDst, IppiSize dstSize, Ipp32s dstStep,
                               int numLobes)
{
    IppiResizeSpec_32f *pSpec = 0;
    int specSize = 0, initSize = 0, bufSize = 0;
    Ipp8u *pBuffer = 0;
    Ipp8u *pInitBuf = 0;
    Ipp32u numChannels = 1;
    IppiPoint dstOffset = {0, 0};
    IppiPoint srcOffset = {0, 0};
    IppStatus status = ippStsNoErr;
    IppiBorderSize borderSize = {0, 0, 0, 0};
    IppiBorderType border = ippBorderRepl;
    IppStatus pStatus;

    /* Spec and init buffer sizes */
    status = ippiResizeGetSize_32f(srcSize, dstSize, ippLanczos, 0, &specSize, &initSize);

    if (status != ippStsNoErr)
        return status;

    /* Memory allocation */
    pInitBuf = ippsMalloc_8u(initSize);
    pSpec = (IppiResizeSpec_32f *)ippsMalloc_8u(specSize);

    if (pInitBuf == NULL || pSpec == NULL)
    {
        ippsFree(pInitBuf);
        ippsFree(pSpec);
        return ippStsNoMemErr;
    }

    /* Filter initialization */
    status = ippiResizeLanczosInit_32f(srcSize, dstSize, numLobes, pSpec, pInitBuf);
    ippsFree(pInitBuf);

    if (status != ippStsNoErr)
    {
        ippsFree(pSpec);
        return status;
    }

    status = ippiResizeGetBorderSize_32f(pSpec, &borderSize);
    if (status != ippStsNoErr)
    {
        ippsFree(pSpec);
        return status;
    }

    /* General transform function */
    ippiResizeGetBufferSize_32f(pSpec, dstSize, ippC1, &bufSize);

    pBuffer = ippsMalloc_8u(bufSize);

    if (pBuffer)
    {
        Ipp8u *pOneBuf;

        pStatus = ippiResizeGetSrcRoi_32f(pSpec, dstOffset, dstSize,
                                          &srcOffset, &srcSize);

        if (pStatus == ippStsNoErr)
        {
            pOneBuf = pBuffer;

            pStatus = ippiResizeLanczos_32f_C1R(
                pSrc, srcStep * sizeof(Ipp32f), pDst, dstStep * sizeof(Ipp32f),
                dstOffset, dstSize, border, 0, pSpec, pOneBuf);
        }
    }

    ippsFree(pSpec);

    if (pBuffer == NULL)
        return ippStsNoMemErr;

    ippsFree(pBuffer);

    /* Return bad status */
    if (pStatus != ippStsNoErr)
        return pStatus;

    return status;
}

extern void resizeLanczos(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight, int numLobes)
{
    IppiSize srcSize;
    srcSize.width = srcWidth;
    srcSize.height = srcHeight;
    Ipp32s srcStep = srcSize.width;

    IppiSize dstSize;
    dstSize.width = dstWidth;
    dstSize.height = dstHeight;
    Ipp32s dstStep = dstSize.width;

    IppStatus stat = resizeLanczos32f_C1R(pSrc, srcSize, srcStep, pDest, dstSize, dstStep, numLobes);

    printf("[C] resizeLanczos%d: %d, %s\n", numLobes, stat, ippGetStatusString(stat));
}