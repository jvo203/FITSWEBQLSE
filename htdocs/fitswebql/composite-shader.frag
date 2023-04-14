    // composite     
     gl_FragColor = vec4(pixel_r, pixel_g, pixel_b, alpha) ;
     gl_FragColor.rgb *= gl_FragColor.a;
}