     float black = params.z;
     float white = params.w;
     
     float slope = 1.0 / (white - black);
     float pixel = (x - black) * slope;

     // to be glued together with a separate colourmap shader