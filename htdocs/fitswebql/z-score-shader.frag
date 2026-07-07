     float median = params.x;
     float sensitivity = params.y;
     float black = params.z;
     float white = params.w;
     
     // asymmetric z-score calculation around the median
     float val = x - median;
     float pixel = (val < 0.0) ? val / (median - black) : val / (white - median);

     // finally convert to a 0-1 range for colour mapping
     pixel = 2.326 * sensitivity * pixel / 6.0 + 0.5;     

     // to be glued together with a separate colourmap shader