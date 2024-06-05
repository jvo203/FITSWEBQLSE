#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

// Starlink AST
#include <ast.h>

// read a header from a string
AstFrameSet *ast_read_header(const char *header)
{
    AstFitsChan *fitschan = NULL;
    AstFrameSet *wcsinfo = NULL;

    /* Create a FitsChan and fill it with FITS header cards. */
    fitschan = astFitsChan(NULL, NULL, "");
    astPutCards(fitschan, header);

    /* Read WCS information from the FitsChan. */
    wcsinfo = astRead(fitschan);

    if (!astOK)
    {
        printf("<[C] an error occurred (a message will have been issued)>\n");
    }
    else if (wcsinfo == AST__NULL)
    {
        printf("[C] <there was no WCS information present>\n");
    }
    else if (strcmp(astGetC(wcsinfo, "Class"), "FrameSet"))
    {
        printf("[C] <something unexpected was read (i.e. not a FrameSet)>\n");
    }
    else
    {
        printf("[C] <WCS information was read OK>\n");
    }

    // astShow(wcsinfo);

    // annul the FitsChan
    astAnnul(fitschan);

    return wcsinfo;
}

void astPix2Sky(AstFrameSet *wcsinfo, float x, float y, double *ra, double *dec)
{
    double pixcrd[4] = {x, y, 0, 0};
    double coords[4] = {0, 0, 0, 0};

    if (wcsinfo == AST__NULL)
    {
        printf("[C] WCS info is NULL\n");

        *ra = NAN;
        *dec = NAN;
        return;
    }

    if (strcmp(astGetC(wcsinfo, "Class"), "FrameSet"))
    {
        printf("[C] WCS info is not a FrameSet\n");

        *ra = NAN;
        *dec = NAN;
        return;
    }

    astTranN(wcsinfo, 1, 4, 1, pixcrd, 1, 4, 1, coords);

    // ra + 360 and then take the modulo 360
    *ra = fmod(coords[0] * AST__DR2D + 360.0, 360.0);
    *dec = coords[1] * AST__DR2D;
}
