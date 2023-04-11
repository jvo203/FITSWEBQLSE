    // composite
     /*colour.r = pixel_r;
     colour.g = pixel_g;
     colour.b = pixel_b;

     gl_FragColor = colour;*/
     gl_FragColor = vec4(pixel_r, pixel_g, pixel_b, 1.0) ;
     gl_FragColor.rgb *= gl_FragColor.a;
}