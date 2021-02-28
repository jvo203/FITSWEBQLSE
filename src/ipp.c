#include <stdlib.h>

#include <ipp.h>

IppStatus tileResize32f_C1R(Ipp32f *pSrc, IppiSize srcSize, Ipp32s srcStep,
                            Ipp32f *pDst, IppiSize dstSize, Ipp32s dstStep)
{

    // int MAX_NUM_THREADS = omp_get_max_threads();
    int max_threads = omp_get_max_threads();

    // a per-thread limit
    size_t max_work_size = 1024 * 1024 * 4;
    size_t plane_size = size_t(srcSize.width) * size_t(srcSize.height);
    size_t work_size = MIN(plane_size, max_work_size);
    int MAX_NUM_THREADS =
        MAX((int)roundf(float(plane_size) / float(work_size)), 1);
    //printf("tileResize32f_C1R::num_threads = %d\n", MAX_NUM_THREADS);

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
    int numThreads, slice, tail;
    int bufSize1, bufSize2;
    IppiSize dstTileSize, dstLastTileSize;
    IppStatus pStatus[MAX_NUM_THREADS];

    /* Spec and init buffer sizes */
    status = ippiResizeGetSize_32f(srcSize, dstSize, ippLanczos, 0, &specSize,
                                   &initSize);

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
    status = ippiResizeLanczosInit_32f(srcSize, dstSize, 3, pSpec, pInitBuf);
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
    /* Parallelized only by Y-direction here */
#pragma omp parallel num_threads(MAX_NUM_THREADS)
    {
#pragma omp master
        {
            numThreads = omp_get_num_threads();
            slice = dstSize.height / numThreads;
            tail = dstSize.height % numThreads;

            dstTileSize.width = dstLastTileSize.width = dstSize.width;
            dstTileSize.height = slice;
            dstLastTileSize.height = slice + tail;

            ippiResizeGetBufferSize_32f(pSpec, dstTileSize, ippC1, &bufSize1);
            ippiResizeGetBufferSize_32f(pSpec, dstLastTileSize, ippC1, &bufSize2);

            pBuffer = ippsMalloc_8u(bufSize1 * (numThreads - 1) + bufSize2);
        }

#pragma omp barrier
        {
            if (pBuffer)
            {
                int i;
                Ipp32f *pSrcT, *pDstT;
                Ipp8u *pOneBuf;
                IppiPoint srcOffset = {0, 0};
                IppiPoint dstOffset = {0, 0};
                IppiSize srcSizeT = srcSize;
                IppiSize dstSizeT = dstTileSize;

                i = omp_get_thread_num();
                dstSizeT.height = slice;
                dstOffset.y += i * slice;

                if (i == numThreads - 1)
                    dstSizeT = dstLastTileSize;

                pStatus[i] = ippiResizeGetSrcRoi_32f(pSpec, dstOffset, dstSizeT,
                                                     &srcOffset, &srcSizeT);

                if (pStatus[i] == ippStsNoErr)
                {
                    pSrcT = pSrc + srcOffset.y * srcStep;
                    if (!mirror)
                        pDstT = pDst + dstOffset.y * dstStep;
                    else
                    {
                        if (i == numThreads - 1)
                            pDstT = pDst;
                        else
                            pDstT = pDst + (dstSize.height - (i + 1) * slice) * dstStep;
                    }

                    pOneBuf = pBuffer + i * bufSize1;

                    pStatus[i] = ippiResizeLanczos_32f_C1R(
                        pSrcT, srcStep * sizeof(Ipp32f), pDstT, dstStep * sizeof(Ipp32f),
                        dstOffset, dstSizeT, border, 0, pSpec, pOneBuf);
                }
            }
        }
    }

    ippsFree(pSpec);

    if (pBuffer == NULL)
        return ippStsNoMemErr;

    ippsFree(pBuffer);

    for (int i = 0; i < numThreads; ++i)
    {
        /* Return bad status */
        if (pStatus[i] != ippStsNoErr)
            return pStatus[i];
    }

    return status;
}
