     // RED
     float black_r = params_r.z;
     float white_r = params_r.w;

     // GREEN
     float black_g = params_g.z;
     float white_g = params_g.w;

     // BLUE
     float black_b = params_b.z;
     float white_b = params_b.w;
     
     // RED
     float slope_r = 1.0 / (white_r - black_r);
     float pixel_r = (x_r - black_r) * slope_r;

     // GREEN
     float slope_g = 1.0 / (white_g - black_g);
     float pixel_g = (x_g - black_g) * slope_g;

     // BLUE
     float slope_b = 1.0 / (white_b - black_b);
     float pixel_b = (x_b - black_b) * slope_b;

     // to be glued together with a separate colourmap shader