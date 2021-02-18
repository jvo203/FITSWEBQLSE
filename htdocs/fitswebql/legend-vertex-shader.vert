precision mediump float;

// an attribute will receive data from a buffer
attribute vec4 a_position;

varying vec2 v_texcoord;
     
void main() {     
     gl_Position = a_position;

     // transform [-1, 1] to [0, 1]
     v_texcoord = 0.5 * a_position.xy + vec2(0.5, 0.5);
}