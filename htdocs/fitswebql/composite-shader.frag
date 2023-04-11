    // composite
     colour.r = pixel_r;
     colour.g = pixel_g;
     colour.b = pixel_b;

     //gl_FragColor = colour;
     gl_FragColor = vec4(0.0,0.0,0.0,colour.a) ;
     gl_FragColor.rgb *= gl_FragColor.a;
}