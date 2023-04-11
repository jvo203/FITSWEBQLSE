     float pmin = params.x;
     float pmax = params.y;
     float lmin = params.z;
     float lmax = params.w;

     float pixel = 0.5 + (x - pmin) / (pmax - pmin);
     pixel = (pixel > 0.0) ? (log(pixel) - lmin) / (lmax - lmin) : 0.0;

     // to be glued together with a separate colourmap shader