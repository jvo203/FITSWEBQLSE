    // viridis
    gl_FragColor = colormap_viridis(clamp(pixel, 0.0, 1.0), colour.a) ;
    gl_FragColor.rgb *= gl_FragColor.a;
}