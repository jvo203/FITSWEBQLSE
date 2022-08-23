IppStatus resizeLanczos32f_C1R_LT(Ipp32f *pSrc, IppiSizeL srcSize, Ipp32s srcStep,
                                  Ipp32f *pDst, IppiSizeL dstSize, Ipp32s dstStep,
                                  int numLobes)
{
    int threads = get_physical_cores();
    omp_set_num_threads(threads);

    IppiResizeSpec_LT *pSpec = 0;
    // int specSize = 0, initSize = 0, bufSize = 0;
    IppSizeL specSize = 0;
    IppSizeL initSize = 0;
    IppDataType ippType = ipp32f;

    Ipp8u *pBuffer = 0;
    Ipp8u *pInitBuf = 0;
    IppiPointL dstOffset = {0, 0};
    IppiPointL srcOffset = {0, 0};
    IppStatus status = ippStsNoErr;
    IppiBorderSize borderSize = {0, 0, 0, 0};
    IppiBorderType border = ippBorderRepl;
    IppStatus pStatus;

    /* Spec and init buffer sizes */
    status = ippiResizeGetSize_LT(srcSize, dstSize, ippType, ippLanczos, 0, &specSize, &initSize);
    CHECK_STATUS_PRINT_BR(status, "ippiResizeGetSize_LT()", ippGetStatusString(status));

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
