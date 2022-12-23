#include <stdio.h>
#include <math.h>

#include "conrec.h"

#define xsect(p1, p2) (h[p2] * xh[p1] - h[p1] * xh[p2]) / (h[p2] - h[p1])
#define ysect(p1, p2) (h[p2] * yh[p1] - h[p1] * yh[p2]) / (h[p2] - h[p1])
#define min(x, y) (x < y ? x : y)
#define max(x, y) (x > y ? x : y)

/*
Copyright (c) 1996-1997 Nicholas Yue

This software is copyrighted by Nicholas Yue. This code is base on the work of
Paul D. Bourke CONREC.F routine

The authors hereby grant permission to use, copy, and distribute this
software and its documentation for any purpose, provided that existing
copyright notices are retained in all copies and that this notice is included
verbatim in any distributions. Additionally, the authors grant permission to
modify this software and its documentation for any purpose, provided that
such modifications are not distributed without the explicit consent of the
authors and that existing copyright notices are retained in all copies. Some
of the algorithms implemented by this software are patented, observe all
applicable patent law.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR
DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES THEREOF,
EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS PROVIDED ON AN
"AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
*/

// set a single pixel on an RGBA canvas
void set_pixel(unsigned char *buffer, int width, int height, int x, int y, unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
    if (x < 0 || x >= width || y < 0 || y >= height)
        return;

    int index = (y * width + x) * 4;

    buffer[index + 0] = r;
    buffer[index + 1] = g;
    buffer[index + 2] = b;
    buffer[index + 3] = a;
}

// downloaded from: https://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#C
void line(unsigned char *buffer, int width, int height, int x0, int y0, int x1, int y1, unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{

    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = (dx > dy ? dx : -dy) / 2, e2;

    for (;;)
    {
        // setPixel(x0, y0);
        set_pixel(buffer, width, height, x0, y0, r, g, b, a);
        if (x0 == x1 && y0 == y1)
            break;
        e2 = err;
        if (e2 > -dx)
        {
            err -= dy;
            x0 += sx;
        }
        if (e2 < dy)
        {
            err += dx;
            y0 += sy;
        }
    }
}

// this is a simple line drawing algorithm that draws a line between two points
// downloaded from https://joshbeam.com/articles/simple_line_drawing/
void draw_line(unsigned char *buffer, int width, int height, float x1, float y1, float x2, float y2, unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
    float xdiff = (x2 - x1);
    float ydiff = (y2 - y1);

    if (xdiff == 0.0f && ydiff == 0.0f)
    {
        // SetPixel(x1, y1, color1);
        set_pixel(buffer, width, height, roundf(x1), roundf(y1), r, g, b, a);
        return;
    }

    if (fabs(xdiff) > fabs(ydiff))
    {
        float xmin, xmax;

        // set xmin to the lower x value given
        // and xmax to the higher value
        if (x1 < x2)
        {
            xmin = x1;
            xmax = x2;
        }
        else
        {
            xmin = x2;
            xmax = x1;
        }

        // draw line in terms of y slope
        float slope = ydiff / xdiff;
        for (float x = xmin; x <= xmax; x += 1.0f)
        {
            float y = y1 + ((x - x1) * slope);
            /*Color color = color1 + ((color2 - color1) * ((x - x1) / xdiff));
            SetPixel(x, y, color);*/
            set_pixel(buffer, width, height, roundf(x), roundf(y), r, g, b, a);
        }
    }
    else
    {
        float ymin, ymax;

        // set ymin to the lower y value given
        // and ymax to the higher value
        if (y1 < y2)
        {
            ymin = y1;
            ymax = y2;
        }
        else
        {
            ymin = y2;
            ymax = y1;
        }

        // draw line in terms of x slope
        float slope = xdiff / ydiff;
        for (float y = ymin; y <= ymax; y += 1.0f)
        {
            float x = x1 + ((y - y1) * slope);
            /*Color color = color1 + ((color2 - color1) * ((y - y1) / ydiff));
            SetPixel(x, y, color);*/
            set_pixel(buffer, width, height, roundf(x), roundf(y), r, g, b, a);
        }
    }
}

// this version of CONREC results in somewhat spurious lines !!!
// try nncntr.c instead ...
//=============================================================================
//
//     CONREC is a contouring subroutine for rectangularily spaced data.
//
//     It emits calls to a line drawing subroutine supplied by the user
//     which draws a contour map corresponding to real*4data on a randomly
//     spaced rectangular grid. The coordinates emitted are in the same
//     units given in the x() and y() arrays.
//
//     Any number of contour levels may be specified but they must be
//     in order of increasing value.
//
//     As this code is ported from FORTRAN-77, please be very careful of the
//     various indices like ilb,iub,jlb and jub, remeber that C/C++ indices
//     starts from zero (0)
//
//=============================================================================
int conrec(float **d,
           int ilb,
           int iub,
           int jlb,
           int jub,
           float *x,
           float *y,
           int nc,
           float *z,
           unsigned char *canvas,
           int img_width,
           int img_height)
// d               ! matrix of data to contour
// ilb,iub,jlb,jub ! index bounds of data matrix
// x               ! data matrix column coordinates
// y               ! data matrix row coordinates
// nc              ! number of contour levels
// z               ! contour levels in increasing order
{
    int m1, m2, m3, case_value;
    float dmin, dmax, x1, x2, y1, y2;
    register int i, j, k, m;
    float h[5];
    int sh[5];
    float xh[5], yh[5];
    //===========================================================================
    // The indexing of im and jm should be noted as it has to start from zero
    // unlike the fortran counter part
    //===========================================================================
    int im[4] = {0, 1, 1, 0}, jm[4] = {0, 0, 1, 1};
    //===========================================================================
    // Note that castab is arranged differently from the FORTRAN code because
    // Fortran and C/C++ arrays are transposed of each other, in this case
    // it is more tricky as castab is in 3 dimension
    //===========================================================================
    int castab[3][3][3] =
        {
            {{0, 0, 8}, {0, 2, 5}, {7, 6, 9}},
            {{0, 3, 4}, {1, 3, 1}, {4, 3, 0}},
            {{9, 6, 7}, {5, 2, 0}, {8, 0, 0}}};
    int line_count = 0;
    for (j = (jub - 1); j >= jlb; j--)
    {
        for (i = ilb; i <= iub - 1; i++)
        {
            float temp1, temp2;
            temp1 = min(d[i][j], d[i][j + 1]);
            temp2 = min(d[i + 1][j], d[i + 1][j + 1]);
            dmin = min(temp1, temp2);
            temp1 = max(d[i][j], d[i][j + 1]);
            temp2 = max(d[i + 1][j], d[i + 1][j + 1]);
            dmax = max(temp1, temp2);
            if (dmax >= z[0] && dmin <= z[nc - 1])
            {
                for (k = 0; k < nc; k++)
                {
                    if (z[k] >= dmin && z[k] <= dmax)
                    {
                        for (m = 4; m >= 0; m--)
                        {
                            if (m > 0)
                            {
                                //=============================================================
                                // The indexing of im and jm should be noted as it has to
                                // start from zero
                                //=============================================================
                                h[m] = d[i + im[m - 1]][j + jm[m - 1]] - z[k];
                                xh[m] = x[i + im[m - 1]];
                                yh[m] = y[j + jm[m - 1]];
                            }
                            else
                            {
                                h[0] = 0.25 * (h[1] + h[2] + h[3] + h[4]);
                                xh[0] = 0.5 * (x[i] + x[i + 1]);
                                yh[0] = 0.5 * (y[j] + y[j + 1]);
                            }
                            if (h[m] > 0.0)
                            {
                                sh[m] = 1;
                            }
                            else if (h[m] < 0.0)
                            {
                                sh[m] = -1;
                            }
                            else
                                sh[m] = 0;
                        }
                        //=================================================================
                        //
                        // Note: at this stage the relative heights of the corners and the
                        // centre are in the h array, and the corresponding coordinates are
                        // in the xh and yh arrays. The centre of the box is indexed by 0
                        // and the 4 corners by 1 to 4 as shown below.
                        // Each triangle is then indexed by the parameter m, and the 3
                        // vertices of each triangle are indexed by parameters m1,m2,and
                        // m3.
                        // It is assumed that the centre of the box is always vertex 2
                        // though this isimportant only when all 3 vertices lie exactly on
                        // the same contour level, in which case only the side of the box
                        // is drawn.
                        //
                        //
                        //      vertex 4 +-------------------+ vertex 3
                        //               | \               / |
                        //               |   \    m-3    /   |
                        //               |     \       /     |
                        //               |       \   /       |
                        //               |  m=2    X   m=2   |       the centre is vertex 0
                        //               |       /   \       |
                        //               |     /       \     |
                        //               |   /    m=1    \   |
                        //               | /               \ |
                        //      vertex 1 +-------------------+ vertex 2
                        //
                        //
                        //
                        //               Scan each triangle in the box
                        //
                        //=================================================================
                        for (m = 1; m <= 4; m++)
                        {
                            m1 = m;
                            m2 = 0;
                            if (m != 4)
                                m3 = m + 1;
                            else
                                m3 = 1;
                            case_value = castab[sh[m1] + 1][sh[m2] + 1][sh[m3] + 1];
                            if (case_value != 0)
                            {
                                switch (case_value)
                                {
                                    //===========================================================
                                    //     Case 1 - Line between vertices 1 and 2
                                    //===========================================================
                                case 1:
                                    x1 = xh[m1];
                                    y1 = yh[m1];
                                    x2 = xh[m2];
                                    y2 = yh[m2];
                                    break;
                                    //===========================================================
                                    //     Case 2 - Line between vertices 2 and 3
                                    //===========================================================
                                case 2:
                                    x1 = xh[m2];
                                    y1 = yh[m2];
                                    x2 = xh[m3];
                                    y2 = yh[m3];
                                    break;
                                    //===========================================================
                                    //     Case 3 - Line between vertices 3 and 1
                                    //===========================================================
                                case 3:
                                    x1 = xh[m3];
                                    y1 = yh[m3];
                                    x2 = xh[m1];
                                    y2 = yh[m1];
                                    break;
                                    //===========================================================
                                    //     Case 4 - Line between vertex 1 and side 2-3
                                    //===========================================================
                                case 4:
                                    x1 = xh[m1];
                                    y1 = yh[m1];
                                    x2 = xsect(m2, m3);
                                    y2 = ysect(m2, m3);
                                    break;
                                    //===========================================================
                                    //     Case 5 - Line between vertex 2 and side 3-1
                                    //===========================================================
                                case 5:
                                    x1 = xh[m2];
                                    y1 = yh[m2];
                                    x2 = xsect(m3, m1);
                                    y2 = ysect(m3, m1);
                                    break;
                                    //===========================================================
                                    //     Case 6 - Line between vertex 3 and side 1-2
                                    //===========================================================
                                case 6:
                                    x1 = xh[m3];
                                    y1 = yh[m3];
                                    x2 = xsect(m1, m2);
                                    y2 = ysect(m1, m2);
                                    break;
                                    //===========================================================
                                    //     Case 7 - Line between sides 1-2 and 2-3
                                    //===========================================================
                                case 7:
                                    x1 = xsect(m1, m2);
                                    y1 = ysect(m1, m2);
                                    x2 = xsect(m2, m3);
                                    y2 = ysect(m2, m3);
                                    break;
                                    //===========================================================
                                    //     Case 8 - Line between sides 2-3 and 3-1
                                    //===========================================================
                                case 8:
                                    x1 = xsect(m2, m3);
                                    y1 = ysect(m2, m3);
                                    x2 = xsect(m3, m1);
                                    y2 = ysect(m3, m1);
                                    break;
                                    //===========================================================
                                    //     Case 9 - Line between sides 3-1 and 1-2
                                    //===========================================================
                                case 9:
                                    x1 = xsect(m3, m1);
                                    y1 = ysect(m3, m1);
                                    x2 = xsect(m1, m2);
                                    y2 = ysect(m1, m2);
                                    break;
                                default:
                                    break;
                                }
                                //=============================================================
                                // Put your processing code here and comment out the printf
                                //=============================================================
                                line_count++;
                                // printf("%f %f %f %f %f\n", x1, y1, x2, y2, z[k]);
                                // printf("%f %f %f %f %d\n", x1, y1, x2, y2, k);
                                /*set_pixel(canvas, img_width, img_height, roundf(x1), roundf(y1), 255, 204, 0, 255);
                                set_pixel(canvas, img_width, img_height, roundf(x2), roundf(y2), 255, 204, 0, 255);*/
                                // draw_line(canvas, img_width, img_height, x1, y1, x2, y2, 255, 204, 0, 255);
                                line(canvas, img_width, img_height, roundf(x1), roundf(y1), round(x2), round(y2), 0, 0, 0, 128);
                            }
                        }
                    }
                }
            }
        }
    }
    printf("CONREC #lines: %d\n", line_count);
    return 0;
}
