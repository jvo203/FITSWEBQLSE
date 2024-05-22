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

int main()
{
    // load a FITS file

    fitsfile *fptr = NULL; /* pointer to the FITS file, defined in fitsio.h */
    struct wcsprm *wcs = NULL;

    int status, nfound, anynull;
    long naxes[2], fpixel, npixels, ii;

    float *pixels = NULL;
    char *mask = NULL;

    // char filename[] = "/mnt/c/Documents and Settings/クリストファー/Downloads/ALMA00000085.fits";
    char filename[] = "/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits";

    status = 0;

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

    // allocate wcsprm struct
    wcs = (struct wcsprm *)malloc(sizeof(struct wcsprm));
    if (wcs == NULL)
    {
        printf("wcs is NULL\n");
        return 1;
    }

    stat = wcspih(header, nkeys, relax, ctrl, &nreject, &nwcs, &wcs);
    printf("[WCSLIB] stat: %d, nreject: %d, nwcs: %d\n", stat, nreject, nwcs);

    printf("====================================================\n");

    double imgcrd[2], phi[2], theta[2];
    double pixcrd[2];
    double world[2];
    double coords[2];

    const double x = 905.0, y = 880.0;              // exported from DS9 as 'physical' coordinates
    const double ra = 52.2656215, dec = 31.2677022; // exported from DS9 as 'fk5' WCS coordinates

    printf("[ds9] x: %f, y: %f\n", x, y);
    printf("[ds9] ra: %f, dec: %f\n", ra, dec);
    printf("====================================================\n");

    // pix2sky
    pixcrd[0] = x;
    pixcrd[1] = y;

    printf("[WCSLIB] pixcrd#1: %f, %f\n", pixcrd[0], pixcrd[1]);
    wcsp2s(wcs, 1, 2, pixcrd, imgcrd, phi, theta, coords, &status);
    printf("[WCSLIB] pixcrd#2: %f, %f\n", pixcrd[0], pixcrd[1]);
    printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
    printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
    printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);

    // sky2pix
    // WCSLIB coordinates
    world[0] = coords[0];
    world[1] = coords[1];

    printf("====================================================\n");
    printf("[WCSLIB] world#1: %f, %f\n", world[0], world[1]);
    wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    printf("[WCSLIB] world#2: %f, %f\n", world[0], world[1]);
    printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
    printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
    printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);

    // ds9 coordinates
    world[0] = ra;
    world[1] = dec;

    printf("====================================================\n");
    printf("[ds9] world#1: %f, %f\n", world[0], world[1]);
    wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, coords, &status);
    printf("[WCSLIB] world#2: %f, %f\n", world[0], world[1]);
    printf("[WCSLIB] phi: %f, %f\n", phi[0], phi[1]);
    printf("[WCSLIB] theta: %f, %f\n", theta[0], theta[1]);
    printf("[WCSLIB] coords: %f, %f\n", coords[0], coords[1]);

    free(header);

    /* read the NAXIS1 and NAXIS2 keyword to get image size */
    if (fits_read_keys_lng(fptr, "NAXIS", 1, 2, naxes, &nfound, &status))
        printerror(status);

    npixels = naxes[0] * naxes[1]; /* number of pixels in the image */
    fpixel = 1;
    float nullval = 0; /* don't check for null values in the image */

    pixels = (float *)malloc(npixels * sizeof(float));
    mask = (char *)malloc(npixels * sizeof(char));

    // it's a test program so don't bother with checking pixels and mask pointers for NULL, just assume they are properly allocated
    if (fits_read_imgnull(fptr, TFLOAT, fpixel, npixels, pixels, mask, &anynull, &status))
        printerror(status);

    // print anynull
    printf("anynull: %d\n", anynull);

    free(pixels);
    free(mask);

    if (fits_close_file(fptr, &status))
        printerror(status);

    free(wcs);

    return 0;
}
