     // RED
     float median_r = params_r.x;
     float sensitivity_r = params_r.y;
     float black_r = params_r.z;
     float white_r = params_r.w;

     // GREEN
     float median_g = params_g.x;
     float sensitivity_g = params_g.y;
     float black_g = params_g.z;
     float white_g = params_g.w;

     // BLUE
     float median_b = params_b.x;
     float sensitivity_b = params_b.y;
     float black_b = params_b.z;
     float white_b = params_b.w;

     // asymmetric z-score calculation around the median

     // RED     
     float val_r = x_r - median_r;
     float pixel_r = (val_r < 0.0) ? val_r / (median_r - black_r) : val_r / (white_r - median_r);

     // GREEN
     float val_g = x_g - median_g;
     float pixel_g = (val_g < 0.0) ? val_g / (median_g - black_g) : val_g / (white_g - median_g);

     // BLUE
     float val_b = x_b - median_b;
     float pixel_b = (val_b < 0.0) ? val_b / (median_b - black_b) : val_b / (white_b - median_b);

     // finally convert to a 0-1 range for colour mapping

     // RED
     pixel_r = 2.326 * sensitivity_r * pixel_r / 6.0 + 0.5;     

     // GREEN
     pixel_g = 2.326 * sensitivity_g * pixel_g / 6.0 + 0.5;

     // BLUE
     pixel_b = 2.326 * sensitivity_b * pixel_b / 6.0 + 0.5;

     // to be glued together with a separate colourmap shader