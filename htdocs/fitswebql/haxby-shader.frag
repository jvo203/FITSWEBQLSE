    // haxby
    gl_FragColor = colormap_haxby(clamp(pixel, 0.0, 1.0), colour.a) ;
    gl_FragColor.rgb *= gl_FragColor.a;
}