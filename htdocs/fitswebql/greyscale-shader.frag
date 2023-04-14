    // greyscale
    colour.r = pixel;
    colour.g = pixel;
    colour.b = pixel;

     gl_FragColor = colour;
     gl_FragColor.rgb *= gl_FragColor.a;
}