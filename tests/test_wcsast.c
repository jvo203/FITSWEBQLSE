#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// CFITSIO
#include <fitsio.h>

// Starlink AST
#include <ast.h>

/************************************************************************/
/* Read a FITS image and compress it with ZFP and OpenEXR               */
/************************************************************************/

/*--------------------------------------------------------------------------*/
void printerror(int status)
{
    /*****************************************************/
    /* Print out cfitsio error messages and exit program */
    /*****************************************************/

    if (status)
    {
        fits_report_error(stderr, status); /* print error report */
        // exit(status); /* terminate the program, returning error status */
    }
    return;
}

void test_wcs(const char *filename, const double x, const double y, const double ra, const double dec)
{
    fitsfile *fptr = NULL; /* pointer to the FITS file, defined in fitsio.h */
    AstFitsChan *fitschan = NULL;
    AstFrameSet *wcsinfo = NULL;

    int status = 0;

    // load a FITS file
    if (fits_open_file(&fptr, filename, READONLY, &status))
    {
        printerror(status);
        return;
    }

    // WCS test
    char *header = NULL;
    int nkeys;

    if (fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeys, &status))
    {
        printerror(status);
        return;
    }

    printf("header: %s\n", header);
    printf("nkeys: %d\n", nkeys);

    astBegin;

    /* Create a FitsChan and fill it with FITS header cards. */
    fitschan = astFitsChan(NULL, NULL, "");
    astPutCards(fitschan, header);

    // free the cfitsio memory
    status = 0;
    fits_free_memory(header, &status);

    status = 0;
    if (fits_close_file(fptr, &status))
        printerror(status);

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

    if (wcsinfo == NULL)
    {
        astEnd;

        printf("[AST] astRead failed!\n");
        return;
    }
    else
    {
        printf("[AST] astRead success!\n");
        astShow(wcsinfo);
        astSet(wcsinfo, "Report=1");
    }

    int nin, nout;

    nin = astGetI(wcsinfo, "Nin");
    nout = astGetI(wcsinfo, "Nout");
    printf("Nin: %d, Nout: %d\n", nin, nout);

    printf("====================================================\n");

    double pixcrd[4] = {0, 0, 0, 0}; // dummy pixel coordinates
    double world[4] = {0, 0, 0, 0};
    double coords[4] = {0, 0, 0, 0};

    printf("[ds9] x: %f, y: %f\n", x, y);
    printf("[ds9] ra: %f, dec: %f\n", ra, dec);
    printf("====================================================\n");

    // pix2sky
    pixcrd[0] = x;
    pixcrd[1] = y;

    printf("[ds9] pixcrd: %f, %f\n", pixcrd[0], pixcrd[1]);
    astTranN(wcsinfo, 1, 4, 1, pixcrd, 1, 4, 1, coords);
    printf("[AST] world: %f, %f\n", coords[0] * AST__DR2D, coords[1] * AST__DR2D);

    // sky2pix
    // WCS coordinates
    world[0] = coords[0];
    world[1] = coords[1];

    printf("====================================================\n");
    printf("[AST] world: %f, %f\n", world[0], world[1]);
    astTranN(wcsinfo, 1, 4, 1, world, 0, 4, 1, coords);
    printf("[AST] pixcrd: %f, %f\n", coords[0], coords[1]);

    // ds9 coordinates
    world[0] = ra * AST__DD2R;
    world[1] = dec * AST__DD2R;

    printf("====================================================\n");
    printf("[ds9] (ICRS) world: %f, %f\n", world[0] * AST__DR2D, world[1] * AST__DR2D);
    astTranN(wcsinfo, 1, 4, 1, world, 0, 4, 1, coords);
    printf("[AST] pixcrd: %f, %f\n", coords[0], coords[1]);

    // ds9 exported as FK5 then converted to ICRS using AST
    world[0] = 52.265612 * AST__DD2R;
    world[1] = 31.267705 * AST__DD2R;

    printf("====================================================\n");
    printf("[ds9] (FK5 --> ICRS) world: %f, %f\n", world[0] * AST__DR2D, world[1] * AST__DR2D);
    astTranN(wcsinfo, 1, 4, 1, world, 0, 4, 1, coords);
    printf("[AST] pixcrd: %f, %f\n", coords[0], coords[1]);

    printf("====================================================\n\n");

    astEnd;
}

int main()
{
    // passing the ra, dec obtained from SAO ds9 as FK5
    // test_wcs("/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022); // NG file

    // passing the ra, dec obtained from SAO ds9 as FK5 and converted to ICRS using AST
    // test_wcs("/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.265612, 31.267705); // NG file

    // passing the ra, dec obtained from SAO ds9 as ICRS
    test_wcs("/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656094, 31.2677078);

    // passing the ra, dec obtained from SAO ds9
    // test_wcs("/Users/chris/Downloads/ALMA01018218.fits", 856.49056, 438.4528, 261.2105354, -34.2435452); // OK file

    // passing the ra, dec obtained from SAO ds9
    // test_wcs("/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022);

    return 0;
}
