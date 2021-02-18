precision mediump float;

// an attribute will receive data from a buffer
attribute vec4 a_position;

uniform vec4 box;

varying vec4 v_texcoord;
     
void main() {
     float xmin = box.x;
     float ymin = box.y;
     float width = box.z;
     float height = box.w;

     gl_Position = a_position;
     //v_texcoord = 0.5 * a_position.xy + vec2(0.5, 0.5); // transform [-1, 1] to [0, 1]

     // apply an image bounding box
     vec2 a = 0.5 * vec2(width, height); 
     vec2 c = a + vec2(xmin, ymin);
     v_texcoord.xy = a * a_position.xy + c;

     // pass the original coordinates to the circular viewport
     v_texcoord.zw = a_position.xy;
}