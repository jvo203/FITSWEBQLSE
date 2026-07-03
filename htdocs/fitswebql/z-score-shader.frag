     float median = params.x;
     float sensitivity = params.y;
     float madN = params.z;
     float madP = params.w;
     
     // asymmetric z-score calculation around the median, using the median absolute deviation (MAD) as a robust measure of scale
     float val = x - median;
     float pixel = (val < 0.0) ? 5.0 * val * sensitivity / madN : 5.0 * val * sensitivity / madP;

     // finally convert to a 0-1 range for colour mapping
     pixel = pixel / 6.0 + 0.5;     

     // to be glued together with a separate colourmap shader