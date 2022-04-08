#pragma once

enum zoom_shape
{
    circle,
    square
};

enum intensity_mode
{
    mean,
    integrated
};

enum image_quality
{
    low,
    medium,
    high
};

struct image_spectrum_request
{
    // input
    int dx;
    bool image;
    enum image_quality quality;
    int x1;
    int y1;
    int x2;
    int y2;
    int width;
    int height;
    enum zoom_shape beam;
    enum intensity_mode intensity;
    double frame_start;
    double frame_end;
    double ref_freq;
    int seq_id;
    float timestamp;

    // output (the 'write' end of a Unix pipe)
    int fd;

    void *ptr;
};