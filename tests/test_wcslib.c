#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// CFITSIO
#include <fitsio.h>

// WCSLIB
#include <wcshdr.h>
#include <wcs.h>

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

        exit(status); /* terminate the program, returning error status */
    }
    return;
}

void test_wcs(const char *filename, const double x, const double y, const double ra, const double dec)
{
    fitsfile *fptr = NULL; /* pointer to the FITS file, defined in fitsio.h */
    struct wcsprm *wcs = NULL;

    int status = 0;

    // load a FITS file
    if (fits_open_file(&fptr, filename, READONLY, &status))
        printerror(status);

    // WCS test
    char *header = NULL;
    int nkeys;

    int relax = WCSHDR_all, ctrl = 4; // 4 for a full telemetry report, 0 for nothing
    int nreject, nwcs, stat;

    if (fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeys, &status))
        printerror(status);

    printf("header: %s\n", header);
    printf("nkeys: %d\n", nkeys);

    stat = wcspih(header, nkeys, relax, ctrl, &nreject, &nwcs, &wcs);
    printf("[WCSLIB] stat: %d, nreject: %d, nwcs: %d\n", stat, nreject, nwcs);

    printf("====================================================\n");

    double imgcrd[2] = {0, 0};
    double phi[2] = {0, 0};
    double theta[2] = {0, 0};
    double pixcrd[2] = {0, 0};
    double world[2] = {0, 0};
    double coords[2] = {0, 0};

    printf("[ds9] x: %f, y: %f\n", x, y);
    printf("[ds9] ra: %f, dec: %f\n", ra, dec);
    printf("====================================================\n");

    // pix2sky
    pixcrd[0] = x;
    pixcrd[1] = y;

    printf("[WCSLIB] pixcrd: %f, %f\n", pixcrd[0], pixcrd[1]);
    wcsp2s(wcs, 1, 2, pixcrd, imgcrd, phi, theta, coords, &status);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
        printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
        printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);
    }
    else
        printf("[WCSLIB] Error: %d\n", status);

    // sky2pix
    // WCSLIB coordinates
    world[0] = coords[0];
    world[1] = coords[1];

    printf("====================================================\n");
    printf("[WCSLIB] world: %f, %f\n", world[0], world[1]);
    wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
        printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
        printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);
    }
    else
        printf("[WCSLIB] Error: %d\n", status);

    // ds9 coordinates
    world[0] = ra;
    world[1] = dec;

    printf("====================================================\n");
    printf("[ds9] world: %f, %f\n", world[0], world[1]);
    wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
        printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
        printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);
    }
    else
        printf("[WCSLIB] Error: %d\n", status);

    free(header);

    status = 0;
    if (fits_close_file(fptr, &status))
        printerror(status);

    wcsvfree(&nwcs, &wcs);
}

int main()
{
    // test_wcs("/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022);
    // test_wcs("/Users/chris/Downloads/ALMA01018218.fits", 856.49056, 438.4528, 261.2105354, -34.2435452);
    test_wcs("/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022);
    test_wcs("/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022);

    return 0;
}
