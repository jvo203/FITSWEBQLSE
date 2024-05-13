#pragma once

#include "fitsio.h"

// CFITSIO-based WCS
struct fitswcs
{
    double xrval;          /* O - X reference value           */
    double yrval;          /* O - Y reference value           */
    double xrpix;          /* O - X reference pixel           */
    double yrpix;          /* O - Y reference pixel           */
    double xinc;           /* O - X increment per pixel       */
    double yinc;           /* O - Y increment per pixel       */
    double rot;            /* O - rotation angle (degrees)    */
    char type[FLEN_VALUE]; /* O - type of projection ('-tan') */
};

int myffgics(char *header, int nkeyrec, /* I - FITS header pointer           */
             double *xrval,             /* O - X reference value           */
             double *yrval,             /* O - Y reference value           */
             double *xrpix,             /* O - X reference pixel           */
             double *yrpix,             /* O - Y reference pixel           */
             double *xinc,              /* O - X increment per pixel       */
             double *yinc,              /* O - Y increment per pixel       */
             double *rot,               /* O - rotation angle (degrees)    */
             char *type);               /* O - type of projection ('-tan') */
                                        // return              /* IO - error status               */