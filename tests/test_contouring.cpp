#include <stdlib.h>
#include <math.h>

#include <fstream>  // ifstream
#include <iostream> // cout, cerr
#include <sstream>  // stringstream
using namespace std;

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

    // contouring

    // copy the image to the canvas
    memcpy(canvas, array, img_size);

    {
        // export the contour canvas to a PGM file for a cross-check
        std::string filename = "tests/canvas.pgm";
        std::fstream pgm_file(filename, std::ios::out | std::ios::binary);

        pgm_file << "P5" << std::endl;
        pgm_file << width << " " << height << " 255" << std::endl;
        pgm_file.write((const char *)canvas, img_size);
        pgm_file.close();
    }
}