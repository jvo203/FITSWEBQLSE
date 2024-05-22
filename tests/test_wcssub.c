#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// CFITSIO
#include <fitsio.h>

// WCSTools
#include "wcsinit.h"

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
    fitsfile *fptr = NULL;        /* pointer to the FITS file, defined in fitsio.h */
    struct WorldCoor *wcs = NULL; // WCSTools

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

    wcs = wcsinit((const char *)header);
    if (wcs == NULL)
    {
        printf("[WCSTools] wcsinit failed!\n");

        status = 0;
        if (fits_close_file(fptr, &status))
            printerror(status);

        return;
    }
    else
    {
        printf("[WCSTools] wcsinit success!\n");
    }

    printf("====================================================\n");

    double pixcrd[2] = {0, 0};
    double world[2] = {0, 0};
    double coords[2] = {0, 0};

    printf("[ds9] x: %f, y: %f\n", x, y);
    printf("[ds9] ra: %f, dec: %f\n", ra, dec);
    printf("====================================================\n");

    // pix2sky
    pixcrd[0] = x;
    pixcrd[1] = y;

    printf("[WCSTools] pixcrd: %f, %f\n", pixcrd[0], pixcrd[1]);
    pix2wcs(wcs, x, y, &coords[0], &coords[1]);
    printf("[WCCTools] world: %f, %f\n", coords[0], coords[1]);

    // sky2pix
    // WCS coordinates
    world[0] = coords[0];
    world[1] = coords[1];
    int offscl = -1;

    printf("====================================================\n");
    printf("[WCSTools] world: %f, %f\n", world[0], world[1]);
    wcs2pix(wcs, world[0], world[1], &coords[0], &coords[1], &offscl);
    if (offscl == 0)
        printf("[WCSTools] pixcrd: %f, %f\n", coords[0], coords[1]);
    else
        printf("[WCSTools] Error: %d\n", offscl);

    // ds9 coordinates
    world[0] = ra;
    world[1] = dec;

    printf("====================================================\n");
    printf("[ds9] world: %f, %f\n", world[0], world[1]);
    wcs2pix(wcs, world[0], world[1], &coords[0], &coords[1], &offscl);
    if (offscl == 0)
        printf("[WCSTools] pixcrd: %f, %f\n", coords[0], coords[1]);
    else
        printf("[WCSTools] Error: %d\n", offscl);

    free(header);

    status = 0;
    if (fits_close_file(fptr, &status))
        printerror(status);

    // free the memory
    wcsfree(wcs);

    printf("====================================================\n\n");
}

int main()
{
    // passing the ra, dec obtained from SAO ds9
    test_wcs("/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022); // NG file

    // passing the ra, dec obtained from SAO ds9
    test_wcs("/Users/chris/Downloads/ALMA01018218.fits", 856.49056, 438.4528, 261.2105354, -34.2435452); // OK file
    test_wcs("/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits", 905.0, 880.0, 52.2656215, 31.2677022);

    return 0;
}
