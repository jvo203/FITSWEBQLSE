     // RED
     float sensitivity_r = params_r.y;
     float black_r = params_r.z;

     // GREEN
     float sensitivity_g = params_g.y;
     float black_g = params_g.z;

     // BLUE
     float sensitivity_b = params_b.y;
     float black_b = params_b.z;
     
     // RED
     float pixel_r = 5.0 * (x_r - black_r) * sensitivity_r;
     pixel_r = (pixel_r > 0.0) ? pixel_r / (1.0 + pixel_r) : 0.0;

     // GREEN
     float pixel_g = 5.0 * (x_g - black_g) * sensitivity_g;
     pixel_g = (pixel_g > 0.0) ? pixel_g / (1.0 + pixel_g) : 0.0;

     // BLUE
     float pixel_b = 5.0 * (x_b - black_b) * sensitivity_b;
     pixel_b = (pixel_b > 0.0) ? pixel_b / (1.0 + pixel_b) : 0.0;

     // to be glued together with a separate colourmap shader