#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// CFITSIO
#include <fitsio.h>

// WCSLIB
#include <wcs.h>
#include <wcshdr.h>

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
    int nkeyrec;

    if ((status = fits_hdr2str(fptr, 1, NULL, 0, &header, &nkeyrec, &status)))
        printerror(status);

    printf("header: %s\n", header);
    printf("nkeyrec: %d\n", nkeyrec);

    int relax = WCSHDR_all, ctrl = 0; // 4 for a full telemetry report, 0 for nothing
    int nreject, nwcs;

    status = wcspih(header, nkeyrec, relax, ctrl, &nreject, &nwcs, &wcs);
    printf("[WCSLIB] status: %d, nreject: %d, nwcs: %d\n", status, nreject, nwcs);

    if ((status = wcsset(wcs)))
    {
        fprintf(stderr, "wcsset ERROR %d: %s.\n", status, wcs_errmsg[status]);
        return;
    }

    printf("====================================================\n");

    double imgcrd[2] = {0, 0};
    double phi[1] = {0};
    double theta[1] = {0};
    double pixcrd[2] = {0, 0};
    double world[2] = {0, 0};
    double coords[2] = {0, 0};
    int stat[1] = {0};

    printf("[ds9] x: %f, y: %f\n", x, y);
    printf("[ds9] ra: %f, dec: %f\n", ra, dec);
    printf("====================================================\n");

    // pix2sky
    pixcrd[0] = x;
    pixcrd[1] = y;

    printf("[WCSLIB] pixcrd: %f, %f\n", pixcrd[0], pixcrd[1]);
    status = wcsp2s(wcs, 1, 2, pixcrd, imgcrd, phi, theta, coords, stat);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f\n", phi[0]);
        printf("[WCSLIB] theta: %f\n", theta[0]);
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
    status = wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f\n", phi[0]);
        printf("[WCSLIB] theta: %f\n", theta[0]);
        printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);
    }
    else
        printf("[WCSLIB] Error: %d\n", status);

    // ds9 coordinates
    world[0] = ra;
    world[1] = dec;

    printf("====================================================\n");
    printf("[ds9] world: %f, %f\n", world[0], world[1]);
    status = wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    if (status == 0)
    {
        printf("[WCSLIB] phi: %f\n", phi[0]);
        printf("[WCSLIB] theta: %f\n", theta[0]);
        printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);
    }
    else
        printf("[WCSLIB] Error: %d\n", status);

    status = 0;
    fits_free_memory(header, &status);

    status = 0;
    if (fits_close_file(fptr, &status))
        printerror(status);

    // Clean up the WCS params structure
    status = wcsvfree(&nwcs, &wcs);

    return;
}

int main()
{
    const char filename1[] = "/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits";
    const char filename2[] = "/Users/chris/Downloads/ALMA01018218.fits";

    // passing the ra, dec obtained from SAO ds9
    test_wcs(filename1, 905.0, 880.0, 52.2656215, 31.2677022);

    // passing the ra, dec obtained from SAO ds9
    test_wcs(filename2, 856.49056, 438.4528, 261.2105354, -34.2435452);

    // I know these are duplicates but the C compilers seem to be behaving strangely (CFITSIO::ffopen fails to open the file in the second test_wcs call,it seg. faults)
    const char filename3[] = "/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits";
    const char filename4[] = "/home/chris/ダウンロード/SVS13_13CO.clean.image.pbcor.fits";

    // passing the ra, dec obtained from SAO ds9
    // test_wcs(filename3, 905.0, 880.0, 52.2656215, 31.2677022);

    // passing the ra, dec obtained from SAO ds9
    // test_wcs(filename4, 905.0, 880.0, 52.2656215, 31.2677022);

    return 0;
}
