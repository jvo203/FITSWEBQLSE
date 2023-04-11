     // RED
     float pmin_r = params_r.x;
     float pmax_r = params_r.y;
     float lmin_r = params_r.z;
     float lmax_r = params_r.w;

    // GREEN
     float pmin_g = params_g.x;
     float pmax_g = params_g.y;
     float lmin_g = params_g.z;
     float lmax_g = params_g.w;

    // BLUE
     float pmin_b = params_b.x;
     float pmax_b = params_b.y;
     float lmin_b = params_b.z;
     float lmax_b = params_b.w;

    // RED
     float pixel_r = 0.5 + (x_r - pmin_r) / (pmax_r - pmin_r);
     pixel_r = (pixel_r > 0.0) ? (log(pixel_r) - lmin_r) / (lmax_r - lmin_r) : 0.0;

     // GREEN
     float pixel_g = 0.5 + (x_g - pmin_g) / (pmax_g - pmin_g);
     pixel_g = (pixel_g > 0.0) ? (log(pixel_g) - lmin_g) / (lmax_g - lmin_g) : 0.0;

    // BLUE
     float pixel_b = 0.5 + (x_b - pmin_b) / (pmax_b - pmin_b);
     pixel_b = (pixel_b > 0.0) ? (log(pixel_b) - lmin_b) / (lmax_b - lmin_b) : 0.0;

     // to be glued together with a separate colourmap shader