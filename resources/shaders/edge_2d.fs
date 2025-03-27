#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float edge_fill;

uniform sampler2D u_color_ids_tex;
uniform vec4 u_color_ids_bypass[10];
uniform int u_color_ids_bypass_count;

uniform vec2 u_resolution;

// Output fragment color
out vec4 finalColor;

// #define NOISEBLUR_SECS u_time
// #define NOISEBLUR_GAUSSIAN_K 2.0
// #define BLUENOISE_TEXTURE u_noise
// #define BLUENOISE_TEXTURE_RESOLUTION u_noiseResolution

#include "lygia/sample/clamp2edge.glsl"
#define EDGE_SAMPLER_FNC(TEX, UV) sampleClamp2edge(TEX, UV).g
#include "lygia/filter/edge.glsl"

void main (void) {
    vec3 color = vec3(0.0);
    vec2 pixel = 1.0/u_resolution;
    vec2 st = fragTexCoord;
    vec4 texelColor = texture(texture0, fragTexCoord);
    vec4 color_id = texture(u_color_ids_tex, fragTexCoord);
    // Higher radius is like an out of focus effect.
    float radius = 1.0;

    float c = 1.0;

    bool trigger =
        st.x * st.y < sin(edge_fill * 0.87 * c) * cos(edge_fill * 0.23 * c) + sin(edge_fill * 0.37 * c) * cos(edge_fill * 0.53 * c)
        //st.x > sin(edge_fill * 0.24) * 1.0 && st.x < sin(edge_fill * 0.87 * c)
        //st.y > sin(edge_fill * 0.41 * c) * 0.4 && st.y < sin(edge_fill * 0.46 * c)
        ;

    bool is_bypassing = false;
    for (int i = 0; i < u_color_ids_bypass_count; i++) {
        if (abs(color_id.r - (u_color_ids_bypass[i]).r) < 0.001 &&
            abs(color_id.g - (u_color_ids_bypass[i]).g) < 0.001 &&
            abs(color_id.b - (u_color_ids_bypass[i]).b) < 0.001 &&
            abs(color_id.a - (u_color_ids_bypass[i]).a) < 0.001) {
            is_bypassing = true;
        }
    }

    trigger = trigger && !is_bypassing;

    float th = 0.0;

    if (trigger && texelColor.r > th && texelColor.g > th && texelColor.b > th) {
        // If we comment all 4 below, we have good effect as well.
        //color += edgePrewitt(texture0, st, pixel * radius) * vec3(0.9, 0.3, 0.4);
        //color += edgePrewitt(texture0, st, pixel * radius) * vec3(0.9, 0.7, 0.5) + 0.2;
        //color += edgePrewitt(texture0, st, pixel * radius) + (texelColor*colDiffuse*fragColor).xyz*0.2;

        // By adjusting the negative factor in the second term, we can adjust light intensity.
        color += edgePrewitt(texture0, st, pixel * radius) + (texelColor*colDiffuse*fragColor).xyz*-0.3;

        // More effect.
        //color += edgePrewitt(texture0, st, pixel * radius) * (texelColor*colDiffuse*fragColor).zyx*8.0;
        //color += edgePrewitt(texture0, st, pixel * radius) * (texelColor*colDiffuse*fragColor).zyx*-9.0;

        finalColor = vec4((vec3(0.9, 0.7, 0.5)- color), 1.0);
    } else {
        // Texel color fetching from texture sampler
        vec4 texelColor = texture(texture0, fragTexCoord);

        // NOTE: Implement here your fragment shader code

        finalColor = texelColor*colDiffuse*fragColor;
    }

    finalColor.a = texelColor.a;
}
