     float median = params.x;
     float sensitivity = params.y;
    
     float pixel = 1.0 / (1.0 + exp(-6.0 * (x - median) * sensitivity));

     // to be glued together with a separate colourmap shader