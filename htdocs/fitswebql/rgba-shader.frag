precision mediump float;

varying vec4 v_texcoord;
uniform sampler2D u_texture;

void main() {
    vec4 colour = texture2D(u_texture, v_texcoord.xy);// the raw floating-point colour

    // clip the coordinates
    if(v_texcoord.x < 0.0 || v_texcoord.x > 1.0 || v_texcoord.y < 0.0 || v_texcoord.y > 1.0)
        colour = vec4(0.0, 0.0, 0.0, 0.0);
    
    // RGBA pass-through
    gl_FragColor = colour;
    gl_FragColor.rgb *= gl_FragColor.a;
}