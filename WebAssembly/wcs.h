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

// return              /* IO - error status               */
int myffgics(char *header, int nkeyrec, /* I - FITS header pointer           */
             double *xrval,             /* O - X reference value           */
             double *yrval,             /* O - Y reference value           */
             double *xrpix,             /* O - X reference pixel           */
             double *yrpix,             /* O - Y reference pixel           */
             double *xinc,              /* O - X increment per pixel       */
             double *yinc,              /* O - Y increment per pixel       */
             double *rot,               /* O - rotation angle (degrees)    */
             char *type);               /* O - type of projection ('-tan') */

int ffwldp(double xpix, double ypix, double xref, double yref,
           double xrefpix, double yrefpix, double xinc, double yinc, double rot,
           char *type, double *xpos, double *ypos, int *status);

int myffxypx(double xpos, double ypos, double xref, double yref,
             double xrefpix, double yrefpix, double xinc, double yinc, double rot,
             char *type, double *xpix, double *ypix, int *status);