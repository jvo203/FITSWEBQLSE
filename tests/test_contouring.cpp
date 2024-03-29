#include <stdlib.h>
#include <math.h>

#include <fstream>  // ifstream
#include <iostream> // cout, cerr
#include <sstream>  // stringstream
using namespace std;

#define xsect(p1, p2) (h[p2] * xh[p1] - h[p1] * xh[p2]) / (h[p2] - h[p1])
#define ysect(p1, p2) (h[p2] * yh[p1] - h[p1] * yh[p2]) / (h[p2] - h[p1])
#define min(x, y) (x < y ? x : y)
#define max(x, y) (x > y ? x : y)

// set a single pixel on an RGBA canvas
// was int x, int y but changed to int y, int x (CONREC uses a reversed order? FORTRAN?)
void set_pixel(unsigned char *buffer, int width, int height, int y, int x, unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
    if (x < 0 || x >= width || y < 0 || y >= height)
    {
        printf("set_pixel: out of bounds: %d, %d\n", x, y);
        return;
    }

    int index = (y * width + x) * 1; // was * 4

    buffer[index + 0] = 255;
    /*buffer[index + 1] = g;
    buffer[index + 2] = b;
    buffer[index + 3] = a;*/
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

int main()
{
    int x = 0, y = 0, width = 0, height = 0, maxval = 0;

    string inputLine = "";
    std::string filename = "tests/one.pgm";
    std::ifstream pgm_file(filename, std::ios::out | std::ios::binary);

    getline(pgm_file, inputLine);

    if (inputLine.compare("P5") != 0)
        cerr << "Version error" << endl;
    else
        cout << "Version : " << inputLine << endl;

    // Second line : comment
    getline(pgm_file, inputLine);
    cout << "Comment : " << inputLine << endl;

    pgm_file >> width >> height >> maxval;
    cout << width << " x " << height << " maxval: " << maxval << endl;

    size_t img_size = width * height;
    uint8_t array[img_size];

    pgm_file.read((char *)array, img_size);
    pgm_file.close();

    // the contouring canvas
    uint8_t canvas[img_size];

    // zero-out the canvas
    memset(canvas, 0, img_size);

    // make a copy of data (re-arrange) for the CONREC algorithm
    float **d = (float **)calloc(height, sizeof(float *));
    for (int i = 0; i < height; i++)
    {
        d[i] = (float *)calloc(width, sizeof(float));

        for (int j = 0; j < width; j++)
            d[i][j] = array[i * width + j];
    }

    // CONREC variables
    const int nc = 4;
    float xc[height];
    float yc[width];
    float zc[nc];

    const int ilb = 0;
    const int iub = height - 1;
    const int jlb = 0;
    const int jub = width - 1;

    for (int i = 0; i < height; i++)
        xc[i] = i;

    for (int i = 0; i < width; i++)
        yc[i] = i;

    zc[0] = 0.0f;
    zc[1] = 100.0f;
    zc[2] = 200.0f;
    zc[3] = 300.0f;

    // apply the contouring
    conrec(d, ilb, iub, jlb, jub, xc, yc, nc, zc, canvas, width, height);

    {
        // export the contour canvas to a PGM file for a cross-check
        std::string filename = "tests/canvas.pgm";
        std::fstream pgm_file(filename, std::ios::out | std::ios::binary);

        pgm_file << "P5" << std::endl;
        pgm_file << width << " " << height << " 255" << std::endl;
        pgm_file.write((const char *)canvas, img_size);
        pgm_file.close();
    }

    // free d
    for (int i = 0; i < height; i++)
        free(d[i]);

    free(d);
}