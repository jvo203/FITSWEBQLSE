     float sensitivity = params.y;
     float black = params.z;
     
     float pixel = 5.0 * (x - black) * sensitivity;

     if (pixel > 0.0)
          pixel = pixel / (1.0 + pixel) ;
     else
          pixel = 0.0;

     // to be glued together with a separate colourmap shader