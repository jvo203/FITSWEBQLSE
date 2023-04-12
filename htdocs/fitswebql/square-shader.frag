     float sensitivity = params.y;
     float black = params.z;
    
     float pixel = (x - black) * sensitivity;
     pixel = (pixel > 0.0) ? pixel * pixel : 0.0;

     // to be glued together with a separate colourmap shader