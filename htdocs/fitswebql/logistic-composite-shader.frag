     // RED
     float median_r = params_r.x;
     float sensitivity_r = params_r.y;

     // GREEN
     float median_g = params_g.x;
     float sensitivity_g = params_g.y;

     // BLUE
     float median_b = params_b.x;
     float sensitivity_b = params_b.y;

     // RED
     float pixel_r = 1.0 / (1.0 + exp(-6.0 * (x_r - median_r) * sensitivity_r));

     // GREEN
     float pixel_g = 1.0 / (1.0 + exp(-6.0 * (x_g - median_g) * sensitivity_g));

     // BLUE
     float pixel_b = 1.0 / (1.0 + exp(-6.0 * (x_b - median_b) * sensitivity_b));

     // to be glued together with a separate colourmap shader