/*******************************************************************************
* Copyright 2016-2021 Intel Corporation.
*
* This software and the related documents are Intel copyrighted  materials,  and
* your use of  them is  governed by the  express license  under which  they were
* provided to you (License).  Unless the License provides otherwise, you may not
* use, modify, copy, publish, distribute,  disclose or transmit this software or
* the related documents without Intel's prior written permission.
*
* This software and the related documents  are provided as  is,  with no express
* or implied  warranties,  other  than those  that are  expressly stated  in the
* License.
*******************************************************************************/

#if (defined _DEBUG && defined _WIN32)
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include <math.h>
#include <memory>

#include "base.h"
#include "base_image.h"
#include "base_renderer.h"
#include "base_ipp.h"

#include "ippcore_tl.h"
#include "ippi_tl.h"
#include "ipps.h"
#include "ippcore_tl.h"

#if defined USE_OMP

#include <omp.h>
#elif defined USE_TBB

#define TBB_PREVIEW_GLOBAL_CONTROL 1
#include "tbb/global_control.h"
#endif

static void printVersion(bool useTL)
{
    const IppLibraryVersion *pVer = ippsGetLibVersion();

    printf("\nIntel(R) IPP Threading Layers Example: Resize");
    printf("\nThis is a simple example of Intel(R) IPP resize functionality which provides comparison between Intel(R) IPP and Intel(R) IPP Threading Layer APIs.\n");
    printf("\nBased on:");
    if (useTL) {
        IppThreadingType thrType;
        ippGetThreadingType_LT (&thrType);
        printf("\nIntel(R) IPP Threading Layer (%s)", (thrType==OMP)? "OpenMP" : "TBB");
    }
    printf("\nIntel(R) IPP: %s %s %s", pVer->Name, pVer->Version, pVer->BuildDate);
    printf("\n");
}

static void printHelp(const cmd::OptDef pOptions[], char* argv[])
{
    printf("\nUsage: %s [-i] InputFile [[-o] OutputFile] [Options]\n", GetProgName(argv));
    printf("Options:\n");
    cmd::OptUsage(pOptions);
}

int main(int argc, char *argv[])
{
#ifdef _CRTDBG_MAP_ALLOC
    _CrtSetDbgFlag(_CRTDBG_LEAK_CHECK_DF | _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG));
#endif

    /*
    // Variables initialization
    */
    Status       status         = STS_OK;
    DString      sInputFile     = CheckTestDirs( BMP_GRAYSCALE_FILE );
    DString      sOutputFile;
    DString      sIppCpu;
    DString      sIType          = "tl";
    bool         useTL           = false;

    unsigned int  iDstSize[2]    = {0, 0};
    unsigned int  threads        = 0;
    bool          bNoAspect      = false;
    bool          bNoWindow      = false;
    bool          bPrintHelp     = false;

    Image srcData;
    Image dstData;

    // General timing
    vm_tick      tickStart   = 0;
    vm_tick      tickAcc     = 0;
    vm_tick      tickFreq    = vm_time_get_frequency()/1000;
    double       fTime       = 0;
    double       fTimeLimit  = 0;
    unsigned int iLoops      = 0;
    unsigned int iLoopsLimit = 0;

    /*
    // Cmd parsing
    */
    const cmd::OptDef cmdOpts[] = {
        { 'i', "", 1, cmd::KT_DSTRING,   cmd::KF_OPTIONAL,  &sInputFile,      "input file name" },
        { 'o', "", 1, cmd::KT_DSTRING,   cmd::KF_OPTIONAL,  &sOutputFile,     "output file name" },
        { 'm', "", 1, cmd::KT_DSTRING,   0,                 &sIType,          "interface mode: ipp, tl (default)" },
        { 'r', "", 2, cmd::KT_POSITIVE,  0,                 &iDstSize[0],     "destination resolution (width height)" },
        { 'k', "", 1, cmd::KT_BOOL,      0,                 &bNoAspect,       "do not keep aspect ratio" },
#if defined USE_OMP
        { 't', "", 1, cmd::KT_INTEGER,   0,                 &threads,         "number of threads for TL interface (OpenMP)" },
#elif defined USE_TBB
        { 't', "", 1, cmd::KT_INTEGER,   0,                 &threads,         "number of tasks for TL interface (TBB task_scheduler_init)" },
#endif
#if defined(ENABLE_RENDERING)
        { 's', "", 1, cmd::KT_BOOL,      0,                 &bNoWindow,       "suppress window output" },
#endif
        { 'w', "", 1, cmd::KT_DOUBLE,    0,                 &fTimeLimit,      "minimum test time in milliseconds" },
        { 'l', "", 1, cmd::KT_POSITIVE,  0,                 &iLoopsLimit,     "number of loops (overrides test time)" },
        { 'T', "", 1, cmd::KT_DSTRING,   0,                 &sIppCpu,         "target Intel IPP optimization (" IPP_OPT_LIST ")" },
        { 'h', "", 1, cmd::KT_BOOL,      0,                 &bPrintHelp,      "print help and exit" },
        {0}
    };

    if (cmd::OptParse(argc, argv, cmdOpts))
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("invalid input parameters");
        return 1;
    }

    InitPreferredCpu(sIppCpu.c_str());

    // Check default image availability
    if ( !strcmp(sInputFile.c_str(), BMP_GRAYSCALE_FILE) ) {
        bPrintHelp = ( -1 == vm_file_access(sInputFile.c_str(), 0) );
    }

    if(bPrintHelp)
    {
        printHelp(cmdOpts, argv);
        return 0;
    }

    if(!sInputFile.Size())
    {
        printHelp(cmdOpts, argv);
        PRINT_MESSAGE("Cannot open input file");
        return 1;
    }

    if(sIType == "ipp")
        useTL = false;
    else if(sIType == "tl")
        useTL = true;
    else
    {
        PRINT_MESSAGE("Improper interface type");
        return 1;
    }

    printVersion(useTL);

    for(;;)
    {
        IppStatus ippStatus;
        AutoBuffer<IppiResizeSpec>    ippResizeSpec;
        AutoBuffer<IppiResizeSpec_LT> ippResizeTLSpec;
        AutoBuffer<Ipp8u>             ippResizeBuffer;
        IppiSizeL   ippSrcSize;
        IppiSizeL   ippDstSize;
        IppDataType ippType;

        // Read from file
        printf("\nInput file: %s\n", sInputFile.c_str());
        status = srcData.Read(sInputFile, CF_GRAY, ST_8U);
        CHECK_STATUS_PRINT_BR(status, "Image::Read()", GetBaseStatusString(status));
        printf("Input info: %dx%d %s\n", (int)srcData.m_size.width, (int)srcData.m_size.height, colorFormatName[srcData.m_color]);

        // Prepare destination buffer
        if(!iDstSize[0])
            iDstSize[0] = (int)(srcData.m_size.width/2);
        if(!iDstSize[1])
            iDstSize[1] = (int)(srcData.m_size.height/2);

        if(!iDstSize[0] || !iDstSize[1])
        {
            PRINT_MESSAGE("Invalid output resolution");
            break;
        }

        dstData               = srcData;
        dstData.m_size.width  = iDstSize[0];
        dstData.m_size.height = iDstSize[1];

        if(!bNoAspect)
        {
            double fXRatio  = (double)dstData.m_size.width/srcData.m_size.width;
            dstData.m_size.height = (unsigned int)ROUND_NEAR(srcData.m_size.height*fXRatio);
            if(!dstData.m_size.height)
                dstData.m_size.height = 1;
        }

        if (!threads) threads = 1;

#if defined USE_OMP
        omp_set_num_threads (threads);
#elif defined USE_TBB
		tbb::global_control set_num_threads(tbb::global_control::max_allowed_parallelism, threads); // set_num_threads(threads)
#endif

        status = dstData.Alloc();
        CHECK_STATUS_PRINT_BR(status, "Image::Alloc()", GetBaseStatusString(status));

        printf("\nOutput file: %s\n", (sOutputFile.Size())?sOutputFile.c_str():"-");
        printf("Output info: %dx%d %s\n\n", (int)dstData.m_size.width, (int)dstData.m_size.height, colorFormatName[dstData.m_color]);

        ippSrcSize = ImageSizeToIpp(srcData.m_size);
        ippDstSize = ImageSizeToIpp(dstData.m_size);
        ippType    = ImageFormatToIpp(srcData.m_sampleFormat);

        if(useTL)
        {
            IppSizeL specSize = 0;
            IppSizeL tempSize = 0;

            ippStatus = ippiResizeGetSize_LT(ippSrcSize, ippDstSize, ippType, ippLinear, 0, &specSize, &tempSize);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeGetSize_LT()", ippGetStatusString(ippStatus));

            ippResizeTLSpec.Alloc(1, specSize);

            ippStatus = ippiResizeLinearInit_LT(ippSrcSize, ippDstSize, ippType, srcData.m_samples, ippResizeTLSpec);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeLinearInit_LT()", ippGetStatusString(ippStatus));

            ippStatus = ippiResizeGetBufferSize_LT(ippResizeTLSpec, &tempSize);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeGetBufferSize_LT()", ippGetStatusString(ippStatus));

            ippResizeBuffer.Alloc(tempSize);
        }
        else
        {
            IppSizeL specSize = 0;
            IppSizeL tempSize = 0;

            ippStatus = ippiResizeGetSize_L(ippSrcSize, ippDstSize, ippType, ippLinear, 0, &specSize, &tempSize);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeGetSize_L()", ippGetStatusString(ippStatus));

            ippResizeSpec.Alloc(1, specSize);

            ippStatus = ippiResizeLinearInit_L(ippSrcSize, ippDstSize, ippType, ippResizeSpec);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeLinearInit_L()", ippGetStatusString(ippStatus));

            ippStatus = ippiResizeGetBufferSize_L(ippResizeSpec, ippDstSize, srcData.m_samples, &tempSize);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeGetBufferSize_L()", ippGetStatusString(ippStatus));

            ippResizeBuffer.Alloc(tempSize);
        }

        printf("API:       %s\n",    sIType.c_str());
        printf("Threads:   %u\n",    threads);

        for(iLoops = 1, tickAcc = 0;; iLoops++)
        {
            tickStart = vm_time_get_tick();
            if(useTL)
            {
                ippStatus = ippiResizeLinear_8u_C1R_LT((const Ipp8u*)srcData.ptr(), srcData.m_step, (Ipp8u*)dstData.ptr(), dstData.m_step, ippBorderRepl, NULL, ippResizeTLSpec, ippResizeBuffer);
            }
            else
            {
                IppiPointL dstOffset = {0};
                ippStatus = ippiResizeLinear_8u_C1R_L((const Ipp8u*)srcData.ptr(), srcData.m_step, (Ipp8u*)dstData.ptr(), dstData.m_step, dstOffset, ippDstSize, ippBorderRepl, NULL, ippResizeSpec, ippResizeBuffer);
            }
            tickAcc += (vm_time_get_tick() - tickStart);
            CHECK_STATUS_PRINT_BR(ippStatus, "ippiResizeLinear_8u_C1R_X()", ippGetStatusString(ippStatus));

            fTime = (double)tickAcc/tickFreq;
            if(iLoopsLimit)
            {
                if(iLoops >= iLoopsLimit)
                    break;
            }
            else
            {
                if(fTime >= fTimeLimit)
                    break;
            }
        }
        if(status < 0) break;

        /*
        // Results output
        */
        printf("\nLoops:      %u\n", iLoops);
        printf("Time total: %0.3fms\n",  fTime);
        printf("Loop avg:   %0.3fms\n",  fTime/iLoops);

        if(sOutputFile.Size())
        {
            status = dstData.Write(sOutputFile.c_str());
            CHECK_STATUS_PRINT_BR(status, "Image::Write()", GetBaseStatusString(status));
        }

        // Rendering
        if(!bNoWindow)
        {
            WindowDraw draw("Intel(R) IPP Threading Layer Example", WF_FIT_TO_IMAGE);
            if(draw.IsInitialized())
            {
                printf("\nPress Space to cycle through stages:\n");
                printf("1 - result image\n");
                printf("2 - original image\n");
                printf("\nClose window to exit.\n");

                int  iIndex  = 0;
                bool bRedraw = true;
                while(!draw.IsClosed())
                {
                    vm_time_sleep(10);
                    if(draw.CheckKey() == KK_SPACE)
                    {
                        iIndex = (iIndex+1)%2;
                        bRedraw = true;
                    }
                    if(draw.IsInvalidated())
                        bRedraw = true;

                    if(bRedraw)
                    {
                        if(iIndex == 0)
                            draw.DrawImage(&dstData);
                        else if(iIndex == 1)
                            draw.DrawImage(&srcData);
                        bRedraw = false;
                    }
                }
            }
        }

        break;
    }

    if(status < 0)
        return status;
    return 0;
}
