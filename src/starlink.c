#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
        printf("<an error occurred (a message will have been issued)>\n");
    }
    else if (wcsinfo == AST__NULL)
    {
        printf("<there was no WCS information present>\n");
    }
    else if (strcmp(astGetC(wcsinfo, "Class"), "FrameSet"))
    {
        printf("<something unexpected was read (i.e. not a FrameSet)>\n");
    }
    else
    {
        printf("<WCS information was read OK>\n");
    }

    astShow(wcsinfo);

    // annul the FitsChan
    astAnnul(fitschan);

    return wcsinfo;
}
