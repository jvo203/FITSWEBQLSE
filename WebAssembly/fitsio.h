#pragma once

#define FLEN_CARD 81  /* length of a FITS header card */
#define FLEN_VALUE 71 /* max length of a keyword value string */

#define ANGLE_TOO_BIG 501  /* celestial angle too large for projection */
#define BAD_WCS_VAL 502    /* bad celestial coordinate or pixel value */
#define WCS_ERROR 503      /* error in celestial coordinate calculation */
#define BAD_WCS_PROJ 504   /* unsupported type of celestial projection */
#define NO_WCS_KEY 505     /* celestial coordinate keywords not found */
#define APPROX_WCS_KEY 506 /* approximate WCS keywords were calculated */