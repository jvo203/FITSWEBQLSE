    // amber "rgba(255,204,0,1.0)"
     /*colour.r = pixel;
     colour.g = pixel*204.0/255.0;
     colour.b = 0.0;

     gl_FragColor = colour;*/

     gl_FragColor = colormap_amber(clamp(pixel, 0.0, 1.0), colour.a) ;
     gl_FragColor.rgb *= gl_FragColor.a;
}