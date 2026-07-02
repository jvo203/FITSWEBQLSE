     float sensitivity = params.y;
     float black = params.z;
     
     float pixel = 5.0 * (x - black) * sensitivity;
     pixel = (pixel > 0.0) ? pixel / (1.0 + pixel) : 0.0;

     // to be glued together with a separate colourmap shader