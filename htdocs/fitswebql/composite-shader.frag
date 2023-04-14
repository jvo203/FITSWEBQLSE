    // composite
    colour.r = pixel_r;
    colour.g = pixel_g;
    colour.b = pixel_b;

     gl_FragColor = colour;     
     gl_FragColor.rgb *= gl_FragColor.a;
}