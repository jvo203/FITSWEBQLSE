#pragma once

#include <wcs.h>

/* set up a WCS structure from a FITS image header */
struct WorldCoor *wcsinit(const char *);

void pix2wcs(                       /* Convert pixel coordinates to World Coordinates */
             struct WorldCoor *wcs, /* World coordinate system structure */
             double xpix,           /* Image horizontal coordinate in pixels */
             double ypix,           /* Image vertical coordinate in pixels */
             double *xpos,          /* Longitude/Right Ascension in degrees (returned) */
             double *ypos);         /* Latitude/Declination in degrees (returned) */

void wcs2pix(                       /* Convert World Coordinates to pixel coordinates */
             struct WorldCoor *wcs, /* World coordinate system structure */
             double xpos,           /* Longitude/Right Ascension in degrees */
             double ypos,           /* Latitude/Declination in degrees */
             double *xpix,          /* Image horizontal coordinate in pixels (returned) */
             double *ypix,          /* Image vertical coordinate in pixels (returned) */
             int *offscl);          /* Return value: 0 if within bounds, else off scale */