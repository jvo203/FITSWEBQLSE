     float pmin = params.x;
     float pmax = params.y;
     float lmin = params.z;
     float lmax = params.w;

     float pixel = 0.5 + (x - pmin) / (pmax - pmin);

     if (pixel > 0.0)
          pixel = (log(pixel) - lmin) / (lmax - lmin);
     else
          pixel = 0.0;

     // to be glued together with a separate colourmap shader