#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <fitsio.h>

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

    fitsfile *fptr; /* pointer to the FITS file, defined in fitsio.h */
    int status, nfound, anynull;
    long naxes[2], fpixel, npixels, ii;

    float *pixels = NULL;
    char *mask = NULL;

    // char filename[] = "/mnt/c/Documents and Settings/クリストファー/Downloads/ALMA00000085.fits";
    char filename[] = "/Users/chris/Downloads/SVS13_13CO.clean.image.pbcor.fits";

    status = 0;

    if (fits_open_file(&fptr, filename, READONLY, &status))
        printerror(status);

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

    return 0;
}
