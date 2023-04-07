precision mediump float;

varying vec4 v_texcoord;
uniform sampler2D u_texture;

// single tone mapping
uniform vec4 params;

// RGB composite tone mappings
uniform vec4 params_r;
uniform vec4 params_g;
uniform vec4 params_b;

//IDL green, red, purple
      
float colormap_red(float x) {
    return 1.61361058036781E+00 * x - 1.55391688559828E+02;
}

float colormap_green(float x) {
    return 9.99817607003891E-01 * x + 1.01544260700389E+00;
}

float colormap_blue(float x) {
    return 3.44167852062589E+00 * x - 6.19885917496444E+02;
}

vec4 colormap_red_white_linear(float x, float alpha) {
    float t = x * 255.0;
    float r = clamp(colormap_red(t) / 255.0, 0.0, 1.0);
    float g = clamp(colormap_green(t) / 255.0, 0.0, 1.0);
    float b = clamp(colormap_blue(t) / 255.0, 0.0, 1.0);
    return vec4(g, r, b, alpha);
}

vec4 colormap_green_white_linear(float x, float alpha) {
    float t = x * 255.0;
    float r = clamp(colormap_red(t) / 255.0, 0.0, 1.0);
    float g = clamp(colormap_green(t) / 255.0, 0.0, 1.0);
    float b = clamp(colormap_blue(t) / 255.0, 0.0, 1.0);
    return vec4(r, g, b, alpha);
}

//IDL blue-white-linear

float colormap_red2(float x) {
    if (x < 1.0 / 3.0) {
        return 4.0 * x - 2.992156863;
    } else if (x < 2.0 / 3.0) {
        return 4.0 * x - 2.9882352941;
    } else if (x < 2.9843137255 / 3.0) {
        return 4.0 * x - 2.9843137255;
    } else {
        return x;
    }
}

float colormap_green2(float x) {
    return 1.602642681354730 * x - 5.948580022657070e-1;
}

float colormap_blue2(float x) {
    return 1.356416928785610 * x + 3.345982835050930e-3;
}

vec4 colormap_blue_white_linear(float x, float alpha) {
    float r = clamp(colormap_red2(x), 0.0, 1.0);
    float g = clamp(colormap_green2(x), 0.0, 1.0);
    float b = clamp(colormap_blue2(x), 0.0, 1.0);
    return vec4(r, g, b, alpha);
}

vec4 colormap_hot(float x, float alpha) {
    float r = clamp(8.0 / 3.0 * x, 0.0, 1.0);
    float g = clamp(8.0 / 3.0 * x - 1.0, 0.0, 1.0);
    float b = clamp(4.0 * x - 3.0, 0.0, 1.0);
    return vec4(r, g, b, alpha);
}

vec4 colormap_hsv2rgb(float h, float s, float v, float alpha) {
	float r = v;
	float g = v;
	float b = v;
	if (s > 0.0) {
		h *= 6.0;
		int i = int(h);
		float f = h - float(i);
		if (i == 1) {
			r *= 1.0 - s * f;
			b *= 1.0 - s;
		} else if (i == 2) {
			r *= 1.0 - s;
			b *= 1.0 - s * (1.0 - f);
		} else if (i == 3) {
			r *= 1.0 - s;
			g *= 1.0 - s * f;
		} else if (i == 4) {
			r *= 1.0 - s * (1.0 - f);
			g *= 1.0 - s;
		} else if (i == 5) {
			g *= 1.0 - s;
			b *= 1.0 - s * f;
		} else {
			g *= 1.0 - s * (1.0 - f);
			b *= 1.0 - s;
		}
	}
	return vec4(r, g, b, alpha);
}

vec4 colormap_rainbow(float x, float alpha) {
	if (x < 0.0) {
		return vec4(0.0, 0.0, 0.0, alpha);
	} else if (1.0 < x) {
		return vec4(0.0, 0.0, 0.0, alpha);
	} else {
		float h = clamp(-9.42274071356572E-01 * x + 8.74326827903982E-01, 0.0, 1.0);
		float s = 1.0;
		float v = clamp(4.90125513855204E+00 * x + 9.18879034690780E-03, 0.0, 1.0);
		return colormap_hsv2rgb(h, s, v, alpha);
	}
}

vec4 colormap_amber(float x, float alpha) {
    /*float k;
    float delta = 1.0 / 255.0;

    if (x < 0.0) {
        return vec4(0.0, 0.0, 0.0, 0.0);
    } else if (1.0 < x) {
        return vec4(0.0, 0.0, 0.0, 0.0);
    }

    k = 0.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.0, 0.0, 0.0, alpha);
        vec4 v2 = vec4(0.00027022, 0.00019212, 0.00020587, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 1.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00027022, 0.00019212, 0.00020587, alpha);
        vec4 v2 = vec4(0.00096304, 0.00065703, 0.00071188, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 2.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00096304, 0.00065703, 0.00071188, alpha);
        vec4 v2 = vec4(0.00204508, 0.00134366, 0.00147063, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 3.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00204508, 0.00134366, 0.00147063, alpha);
        vec4 v2 = vec4(0.00350856, 0.00222749, 0.00246041, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 4.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00350856, 0.00222749, 0.00246041, alpha);
        vec4 v2 = vec4(0.00535221, 0.00329214, 0.00366701, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 5.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00535221, 0.00329214, 0.00366701, alpha);
        vec4 v2 = vec4(0.00757776, 0.00452536, 0.00507988, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 6.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.00757776, 0.00452536, 0.00507988, alpha);
        vec4 v2 = vec4(0.01018856, 0.00591737, 0.00669055, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 7.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.01018856, 0.00591737, 0.00669055, alpha);
        vec4 v2 = vec4(0.01318900, 0.00746001, 0.00849191, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 8.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.01318900, 0.00746001, 0.00849191, alpha);
        vec4 v2 = vec4(0.01658415, 0.00914629, 0.01047778, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 9.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.01658415, 0.00914629, 0.01047778, alpha);
        vec4 v2 = vec4(0.02037961, 0.01097012, 0.01264259, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 10.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.02037961, 0.01097012, 0.01264259, alpha);
        vec4 v2 = vec4(0.02458134, 0.01292603, 0.01498127, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 11.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.02458134, 0.01292603, 0.01498127, alpha);
        vec4 v2 = vec4(0.02919561, 0.01500912, 0.01748909, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 12.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.02919561, 0.01500912, 0.01748909, alpha);
        vec4 v2 = vec4(0.03422895, 0.01721491, 0.02016157, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 13.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.03422895, 0.01721491, 0.02016157, alpha);
        vec4 v2 = vec4(0.03968806, 0.01953927, 0.02299445, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 14.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.03968806, 0.01953927, 0.02299445, alpha);
        vec4 v2 = vec4(0.04531868, 0.02197839, 0.02598358, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 15.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.04531868, 0.02197839, 0.02598358, alpha);
        vec4 v2 = vec4(0.05090566, 0.02452870, 0.02912494, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 16.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.05090566, 0.02452870, 0.02912494, alpha);
        vec4 v2 = vec4(0.05646220, 0.02718685, 0.03241458, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 17.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.05646220, 0.02718685, 0.03241458, alpha);
        vec4 v2 = vec4(0.06199227, 0.02994969, 0.03584857, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 18.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.06199227, 0.02994969, 0.03584857, alpha);
        vec4 v2 = vec4(0.06749930, 0.03281425, 0.03942305, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 19.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.06749930, 0.03281425, 0.03942305, alpha);
        vec4 v2 = vec4(0.07298632, 0.03577768, 0.04303969, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 20.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.07298632, 0.03577768, 0.04303969, alpha);
        vec4 v2 = vec4(0.07845598, 0.03883730, 0.04658955, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 21.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.07845598, 0.03883730, 0.04658955, alpha);
        vec4 v2 = vec4(0.08391079, 0.04194805, 0.05008449, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 22.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.08391079, 0.04194805, 0.05008449, alpha);
        vec4 v2 = vec4(0.08935272, 0.04500145, 0.05352652, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 23.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.08935272, 0.04500145, 0.05352652, alpha);
        vec4 v2 = vec4(0.09478364, 0.04800810, 0.05691734, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 24.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.09478364, 0.04800810, 0.05691734, alpha);
        vec4 v2 = vec4(0.10020524, 0.05097015, 0.06025847, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 25.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.10020524, 0.05097015, 0.06025847, alpha);
        vec4 v2 = vec4(0.10561922, 0.05388947, 0.06355116, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 26.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.10561922, 0.05388947, 0.06355116, alpha);
        vec4 v2 = vec4(0.11102684, 0.05676793, 0.06679660, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 27.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.11102684, 0.05676793, 0.06679660, alpha);
        vec4 v2 = vec4(0.11642930, 0.05960724, 0.06999581, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 28.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.11642930, 0.05960724, 0.06999581, alpha);
        vec4 v2 = vec4(0.12182792, 0.06240883, 0.07314960, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 29.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.12182792, 0.06240883, 0.07314960, alpha);
        vec4 v2 = vec4(0.12722366, 0.06517415, 0.07625875, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 30.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.12722366, 0.06517415, 0.07625875, alpha);
        vec4 v2 = vec4(0.13261737, 0.06790461, 0.07932394, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 31.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.13261737, 0.06790461, 0.07932394, alpha);
        vec4 v2 = vec4(0.13801023, 0.07060123, 0.08234560, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 32.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.13801023, 0.07060123, 0.08234560, alpha);
        vec4 v2 = vec4(0.14340273, 0.07326539, 0.08532433, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 33.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.14340273, 0.07326539, 0.08532433, alpha);
        vec4 v2 = vec4(0.14879582, 0.07589801, 0.08826042, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 34.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.14879582, 0.07589801, 0.08826042, alpha);
        vec4 v2 = vec4(0.15419005, 0.07850022, 0.09115424, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 35.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.15419005, 0.07850022, 0.09115424, alpha);
        vec4 v2 = vec4(0.15958614, 0.08107290, 0.09400597, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 36.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.15958614, 0.08107290, 0.09400597, alpha);
        vec4 v2 = vec4(0.16498455, 0.08361705, 0.09681588, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 37.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.16498455, 0.08361705, 0.09681588, alpha);
        vec4 v2 = vec4(0.17038597, 0.08613344, 0.09958399, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 38.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.17038597, 0.08613344, 0.09958399, alpha);
        vec4 v2 = vec4(0.17579064, 0.08862305, 0.10231051, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 39.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.17579064, 0.08862305, 0.10231051, alpha);
        vec4 v2 = vec4(0.18119931, 0.09108647, 0.10499530, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 40.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.18119931, 0.09108647, 0.10499530, alpha);
        vec4 v2 = vec4(0.18661211, 0.09352465, 0.10763847, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 41.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.18661211, 0.09352465, 0.10763847, alpha);
        vec4 v2 = vec4(0.19202947, 0.09593829, 0.11023995, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 42.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.19202947, 0.09593829, 0.11023995, alpha);
        vec4 v2 = vec4(0.19745187, 0.09832799, 0.11279954, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 43.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.19745187, 0.09832799, 0.11279954, alpha);
        vec4 v2 = vec4(0.20287943, 0.10069459, 0.11531721, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 44.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.20287943, 0.10069459, 0.11531721, alpha);
        vec4 v2 = vec4(0.20831240, 0.10303874, 0.11779278, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 45.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.20831240, 0.10303874, 0.11779278, alpha);
        vec4 v2 = vec4(0.21375116, 0.10536102, 0.12022597, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 46.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.21375116, 0.10536102, 0.12022597, alpha);
        vec4 v2 = vec4(0.21919587, 0.10766210, 0.12261658, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 47.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.21919587, 0.10766210, 0.12261658, alpha);
        vec4 v2 = vec4(0.22464664, 0.10994267, 0.12496437, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 48.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.22464664, 0.10994267, 0.12496437, alpha);
        vec4 v2 = vec4(0.23010363, 0.11220334, 0.12726904, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 49.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.23010363, 0.11220334, 0.12726904, alpha);
        vec4 v2 = vec4(0.23556699, 0.11444473, 0.12953028, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 50.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.23556699, 0.11444473, 0.12953028, alpha);
        vec4 v2 = vec4(0.24103680, 0.11666745, 0.13174774, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 51.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.24103680, 0.11666745, 0.13174774, alpha);
        vec4 v2 = vec4(0.24651314, 0.11887210, 0.13392105, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 52.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.24651314, 0.11887210, 0.13392105, alpha);
        vec4 v2 = vec4(0.25199607, 0.12105931, 0.13604983, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 53.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.25199607, 0.12105931, 0.13604983, alpha);
        vec4 v2 = vec4(0.25748561, 0.12322967, 0.13813367, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 54.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.25748561, 0.12322967, 0.13813367, alpha);
        vec4 v2 = vec4(0.26298174, 0.12538380, 0.14017214, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 55.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.26298174, 0.12538380, 0.14017214, alpha);
        vec4 v2 = vec4(0.26848445, 0.12752234, 0.14216482, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 56.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.26848445, 0.12752234, 0.14216482, alpha);
        vec4 v2 = vec4(0.27399366, 0.12964590, 0.14411126, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 57.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.27399366, 0.12964590, 0.14411126, alpha);
        vec4 v2 = vec4(0.27950930, 0.13175513, 0.14601098, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 58.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.27950930, 0.13175513, 0.14601098, alpha);
        vec4 v2 = vec4(0.28503126, 0.13385066, 0.14786352, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 59.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.28503126, 0.13385066, 0.14786352, alpha);
        vec4 v2 = vec4(0.29055939, 0.13593316, 0.14966839, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 60.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.29055939, 0.13593316, 0.14966839, alpha);
        vec4 v2 = vec4(0.29609353, 0.13800331, 0.15142512, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 61.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.29609353, 0.13800331, 0.15142512, alpha);
        vec4 v2 = vec4(0.30163360, 0.14006170, 0.15313309, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 62.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.30163360, 0.14006170, 0.15313309, alpha);
        vec4 v2 = vec4(0.30717934, 0.14210906, 0.15479185, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 63.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.30717934, 0.14210906, 0.15479185, alpha);
        vec4 v2 = vec4(0.31273046, 0.14414615, 0.15640093, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 64.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.31273046, 0.14414615, 0.15640093, alpha);
        vec4 v2 = vec4(0.31828674, 0.14617366, 0.15795977, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 65.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.31828674, 0.14617366, 0.15795977, alpha);
        vec4 v2 = vec4(0.32384799, 0.14819226, 0.15946776, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 66.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.32384799, 0.14819226, 0.15946776, alpha);
        vec4 v2 = vec4(0.32941373, 0.15020285, 0.16092455, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 67.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.32941373, 0.15020285, 0.16092455, alpha);
        vec4 v2 = vec4(0.33498380, 0.15220606, 0.16232943, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 68.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.33498380, 0.15220606, 0.16232943, alpha);
        vec4 v2 = vec4(0.34055769, 0.15420281, 0.16368203, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 69.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.34055769, 0.15420281, 0.16368203, alpha);
        vec4 v2 = vec4(0.34613510, 0.15619386, 0.16498174, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 70.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.34613510, 0.15619386, 0.16498174, alpha);
        vec4 v2 = vec4(0.35171555, 0.15818010, 0.16622814, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 71.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.35171555, 0.15818010, 0.16622814, alpha);
        vec4 v2 = vec4(0.35729867, 0.16016233, 0.16742062, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 72.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.35729867, 0.16016233, 0.16742062, alpha);
        vec4 v2 = vec4(0.36288386, 0.16214158, 0.16855888, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 73.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.36288386, 0.16214158, 0.16855888, alpha);
        vec4 v2 = vec4(0.36847077, 0.16411861, 0.16964223, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 74.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.36847077, 0.16411861, 0.16964223, alpha);
        vec4 v2 = vec4(0.37405875, 0.16609447, 0.17067038, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 75.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.37405875, 0.16609447, 0.17067038, alpha);
        vec4 v2 = vec4(0.37964722, 0.16807017, 0.17164291, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 76.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.37964722, 0.16807017, 0.17164291, alpha);
        vec4 v2 = vec4(0.38523568, 0.17004663, 0.17255932, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 77.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.38523568, 0.17004663, 0.17255932, alpha);
        vec4 v2 = vec4(0.39082346, 0.17202491, 0.17341924, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 78.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.39082346, 0.17202491, 0.17341924, alpha);
        vec4 v2 = vec4(0.39640989, 0.17400610, 0.17422240, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 79.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.39640989, 0.17400610, 0.17422240, alpha);
        vec4 v2 = vec4(0.40199427, 0.17599129, 0.17496847, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 80.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.40199427, 0.17599129, 0.17496847, alpha);
        vec4 v2 = vec4(0.40757589, 0.17798159, 0.17565714, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 81.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.40757589, 0.17798159, 0.17565714, alpha);
        vec4 v2 = vec4(0.41315403, 0.17997814, 0.17628817, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 82.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.41315403, 0.17997814, 0.17628817, alpha);
        vec4 v2 = vec4(0.41872790, 0.18198211, 0.17686136, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 83.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.41872790, 0.18198211, 0.17686136, alpha);
        vec4 v2 = vec4(0.42429670, 0.18399471, 0.17737654, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 84.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.42429670, 0.18399471, 0.17737654, alpha);
        vec4 v2 = vec4(0.42985959, 0.18601716, 0.17783360, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 85.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.42985959, 0.18601716, 0.17783360, alpha);
        vec4 v2 = vec4(0.43541572, 0.18805071, 0.17823249, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 86.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.43541572, 0.18805071, 0.17823249, alpha);
        vec4 v2 = vec4(0.44096419, 0.19009664, 0.17857321, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 87.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.44096419, 0.19009664, 0.17857321, alpha);
        vec4 v2 = vec4(0.44650409, 0.19215623, 0.17885582, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 88.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.44650409, 0.19215623, 0.17885582, alpha);
        vec4 v2 = vec4(0.45203447, 0.19423082, 0.17908043, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 89.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.45203447, 0.19423082, 0.17908043, alpha);
        vec4 v2 = vec4(0.45755437, 0.19632171, 0.17924723, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 90.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.45755437, 0.19632171, 0.17924723, alpha);
        vec4 v2 = vec4(0.46306280, 0.19843027, 0.17935646, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 91.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.46306280, 0.19843027, 0.17935646, alpha);
        vec4 v2 = vec4(0.46855878, 0.20055783, 0.17940843, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 92.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.46855878, 0.20055783, 0.17940843, alpha);
        vec4 v2 = vec4(0.47404130, 0.20270573, 0.17940344, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 93.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.47404130, 0.20270573, 0.17940344, alpha);
        vec4 v2 = vec4(0.47950929, 0.20487536, 0.17934205, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 94.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.47950929, 0.20487536, 0.17934205, alpha);
        vec4 v2 = vec4(0.48496170, 0.20706810, 0.17922481, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 95.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.48496170, 0.20706810, 0.17922481, alpha);
        vec4 v2 = vec4(0.49039751, 0.20928527, 0.17905222, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 96.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.49039751, 0.20928527, 0.17905222, alpha);
        vec4 v2 = vec4(0.49581562, 0.21152825, 0.17882502, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 97.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.49581562, 0.21152825, 0.17882502, alpha);
        vec4 v2 = vec4(0.50121496, 0.21379838, 0.17854403, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 98.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.50121496, 0.21379838, 0.17854403, alpha);
        vec4 v2 = vec4(0.50659449, 0.21609694, 0.17820994, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 99.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.50659449, 0.21609694, 0.17820994, alpha);
        vec4 v2 = vec4(0.51195309, 0.21842528, 0.17782382, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 100.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.51195309, 0.21842528, 0.17782382, alpha);
        vec4 v2 = vec4(0.51728973, 0.22078464, 0.17738660, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 101.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.51728973, 0.22078464, 0.17738660, alpha);
        vec4 v2 = vec4(0.52260336, 0.22317627, 0.17689933, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 102.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.52260336, 0.22317627, 0.17689933, alpha);
        vec4 v2 = vec4(0.52789290, 0.22560139, 0.17636323, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 103.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.52789290, 0.22560139, 0.17636323, alpha);
        vec4 v2 = vec4(0.53315736, 0.22806114, 0.17577943, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 104.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.53315736, 0.22806114, 0.17577943, alpha);
        vec4 v2 = vec4(0.53839571, 0.23055665, 0.17514930, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 105.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.53839571, 0.23055665, 0.17514930, alpha);
        vec4 v2 = vec4(0.54360697, 0.23308898, 0.17447413, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 106.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.54360697, 0.23308898, 0.17447413, alpha);
        vec4 v2 = vec4(0.54879018, 0.23565914, 0.17375540, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 107.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.54879018, 0.23565914, 0.17375540, alpha);
        vec4 v2 = vec4(0.55394442, 0.23826810, 0.17299454, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 108.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.55394442, 0.23826810, 0.17299454, alpha);
        vec4 v2 = vec4(0.55906877, 0.24091674, 0.17219313, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 109.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.55906877, 0.24091674, 0.17219313, alpha);
        vec4 v2 = vec4(0.56416240, 0.24360589, 0.17135272, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 110.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.56416240, 0.24360589, 0.17135272, alpha);
        vec4 v2 = vec4(0.56922447, 0.24633631, 0.17047496, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 111.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.56922447, 0.24633631, 0.17047496, alpha);
        vec4 v2 = vec4(0.57425419, 0.24910869, 0.16956153, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 112.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.57425419, 0.24910869, 0.16956153, alpha);
        vec4 v2 = vec4(0.57925084, 0.25192364, 0.16861412, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 113.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.57925084, 0.25192364, 0.16861412, alpha);
        vec4 v2 = vec4(0.58421372, 0.25478172, 0.16763450, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 114.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.58421372, 0.25478172, 0.16763450, alpha);
        vec4 v2 = vec4(0.58914218, 0.25768338, 0.16662440, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 115.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.58914218, 0.25768338, 0.16662440, alpha);
        vec4 v2 = vec4(0.59403562, 0.26062904, 0.16558565, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 116.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.59403562, 0.26062904, 0.16558565, alpha);
        vec4 v2 = vec4(0.59889351, 0.26361899, 0.16452001, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 117.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.59889351, 0.26361899, 0.16452001, alpha);
        vec4 v2 = vec4(0.60371532, 0.26665350, 0.16342934, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 118.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.60371532, 0.26665350, 0.16342934, alpha);
        vec4 v2 = vec4(0.60850062, 0.26973274, 0.16231538, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 119.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.60850062, 0.26973274, 0.16231538, alpha);
        vec4 v2 = vec4(0.61324900, 0.27285682, 0.16118009, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 120.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.61324900, 0.27285682, 0.16118009, alpha);
        vec4 v2 = vec4(0.61796012, 0.27602577, 0.16002512, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 121.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.61796012, 0.27602577, 0.16002512, alpha);
        vec4 v2 = vec4(0.62263366, 0.27923956, 0.15885242, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 122.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.62263366, 0.27923956, 0.15885242, alpha);
        vec4 v2 = vec4(0.62726938, 0.28249810, 0.15766372, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 123.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.62726938, 0.28249810, 0.15766372, alpha);
        vec4 v2 = vec4(0.63186707, 0.28580123, 0.15646078, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 124.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.63186707, 0.28580123, 0.15646078, alpha);
        vec4 v2 = vec4(0.63642657, 0.28914875, 0.15524553, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 125.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.63642657, 0.28914875, 0.15524553, alpha);
        vec4 v2 = vec4(0.64094774, 0.29254039, 0.15401956, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 126.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.64094774, 0.29254039, 0.15401956, alpha);
        vec4 v2 = vec4(0.64543053, 0.29597585, 0.15278463, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 127.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.64543053, 0.29597585, 0.15278463, alpha);
        vec4 v2 = vec4(0.64987489, 0.29945476, 0.15154265, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 128.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.64987489, 0.29945476, 0.15154265, alpha);
        vec4 v2 = vec4(0.65428082, 0.30297672, 0.15029515, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 129.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.65428082, 0.30297672, 0.15029515, alpha);
        vec4 v2 = vec4(0.65864836, 0.30654129, 0.14904382, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 130.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.65864836, 0.30654129, 0.14904382, alpha);
        vec4 v2 = vec4(0.66297757, 0.31014801, 0.14779035, alpha);
        return mix(v1, v2, x/delta-k);
    }

    k = 131.0;
    if(x < (k+1.0)*delta) {
        vec4 v1 = vec4(0.66297757, 0.31014801, 0.14779035, alpha);
        vec4 v2 = vec4(0.66726857, 0.31379636, 0.14653648, alpha);
        return mix(v1, v2, x/delta-k);
    }*/

    return vec4(x, x*204.0/255.0, 0.0, alpha);
    //return vec4(0.0, 0.5, 0.0, alpha);
}

vec4 colormap_parula(float x, float alpha) {
    if (x < 0.0) {
        return vec4(0.0, 0.0, 0.0, 0.0);
    } else if (1.0 < x) {
        return vec4(0.0, 0.0, 0.0, 0.0);
    }
    
    if (x < 3.1250000000000000e-02) {
        float dx = x - 1.5625000000000000e-02;
        return ((vec4(-1.4151576683620706e+02,  2.4271369358056621e+01,  4.5510373586485706e+01, alpha) * dx
               + vec4( 0.0000000000000000e+00,  0.0000000000000000e+00,  0.0000000000000000e+00, alpha)) * dx
               + vec4( 2.6007355728658488e-01,  1.4968553250962457e+00,  3.0913652594248364e+00, alpha)) * dx
               + vec4( 2.0810000000000001e-01,  1.6630000000000000e-01,  5.2920000000000000e-01, alpha);
    } ;

    if (x < 4.6875000000000000e-02) {
        float dx = x - 3.1250000000000000e-02;
        return ((vec4(-5.1390461057291191e+01,  1.2211762733842230e+01, -1.2843448884986955e+01, alpha) * dx
               + vec4(-6.6335515704472066e+00,  1.1377204386589042e+00,  2.1332987618665173e+00, alpha)) * dx
               + vec4( 1.5642431399834725e-01,  1.5146322069502911e+00,  3.1246980525790007e+00, alpha)) * dx
               + vec4( 2.1162380952380999e-01,  1.8978095238095199e-01,  5.7767619047619101e-01, alpha);
    } ;

    if (x < 6.2500000000000000e-02) {
        float dx = x - 4.6875000000000000e-02;
        return ((vec4(-1.4725107464858192e+02,  1.3014608277362621e+01,  5.8634219534912217e+00, alpha) * dx
               + vec4(-9.0424794325077311e+00,  1.7101468168077587e+00,  1.5312620953827538e+00, alpha)) * dx
               + vec4(-8.8513670422823654e-02,  1.5591301328169576e+00,  3.1819568159735203e+00, alpha)) * dx
               + vec4( 2.1225238095238100e-01,  2.1377142857142900e-01,  6.2697142857142896e-01, alpha);
    } ;

    if (x < 7.8125000000000000e-02) {
        float dx = x - 6.2500000000000000e-02;
        return ((vec4(-2.1469400225321081e+02, -1.4338005366630648e+01, -4.1817857976177763e+01, alpha) * dx
               + vec4(-1.5944873556660008e+01,  2.3202065798091316e+00,  1.8061099994526548e+00, alpha)) * dx
               + vec4(-4.7894106087856969e-01,  1.6221044046390967e+00,  3.2341032549553237e+00, alpha)) * dx
               + vec4( 2.0810000000000001e-01,  2.3860000000000001e-01,  6.7708571428571396e-01, alpha);
    } ;

    if (x < 9.3750000000000000e-02) {
        float dx = x - 7.8125000000000000e-02;
        return ((vec4(-2.8846495443400278e+02,  2.0037550842697090e+02,  1.1771734328417965e+02, alpha) * dx
               + vec4(-2.6008654912279265e+01,  1.6481125782483199e+00, -1.5410209318067788e-01, alpha)) * dx
               + vec4(-1.1344649432057459e+00,  1.6841093914837442e+00,  3.2599158784908235e+00, alpha)) * dx
               + vec4( 1.9590476190476200e-01,  2.6445714285714300e-01,  7.2789999999999999e-01, alpha);
    } ;

    if (x < 1.0937500000000000e-01) {
        float dx = x - 9.3750000000000000e-02;
        return ((vec4(-5.4509738001026233e+02,  5.1696771659011155e+01, -6.5374637230314454e+02, alpha) * dx
               + vec4(-3.9530449651373146e+01,  1.1040714535762580e+01,  5.3638983732652425e+00, alpha)) * dx
               + vec4(-2.1585134520128149e+00,  1.8823723151401646e+00,  3.3413189453671448e+00, alpha)) * dx
               + vec4( 1.7072857142857101e-01,  2.9193809523809500e-01,  7.7924761904761897e-01, alpha);
    } ;

    if (x < 1.2500000000000000e-01) {
        float dx = x - 1.0937500000000000e-01;
        return ((vec4( 2.3639968744743715e+03, -8.1036503315845437e+02, -8.1573269216733058e+02, alpha) * dx
               + vec4(-6.5081889339354191e+01,  1.3464000707278728e+01, -2.5280462828444659e+01, alpha)) * dx
               + vec4(-3.7930812487429293e+00,  2.2652584908126849e+00,  3.0301226257549660e+00, alpha)) * dx
               + vec4( 1.2527142857142901e-01,  3.2424285714285700e-01,  8.3027142857142899e-01, alpha);
    } ;

    if (x < 1.4062500000000000e-01) {
        float dx = x - 1.2500000000000000e-01;
        return ((vec4( 1.4125902630655582e+03,  2.5375056097507152e+02,  9.0826266478267496e+02, alpha) * dx
               + vec4( 4.5730464151631985e+01, -2.4521860222023822e+01, -6.3517932773788282e+01, alpha)) * dx
               + vec4(-4.0954472673010889e+00,  2.0924794358947931e+00,  1.6426476944700765e+00, alpha)) * dx
               + vec4( 5.9133333333333399e-02,  3.5983333333333301e-01,  8.6833333333333296e-01, alpha);
    } ;

    if (x < 1.5625000000000000e-01) {
        float dx = x - 1.4062500000000000e-01;
        return ((vec4(-1.9850459267366693e+03,  1.4738473211499172e+02,  2.4976683303608979e+02, alpha) * dx
               + vec4( 1.1194563273283002e+02, -1.2627302676317344e+01, -2.0943120362100398e+01, alpha)) * dx
               + vec4(-1.6317582534813697e+00,  1.5120237656082123e+00,  3.2294373922181602e-01, alpha)) * dx
               + vec4( 1.1695238095238101e-02,  3.8750952380952403e-01,  8.8195714285714299e-01, alpha);
    } ;

    if (x < 1.7187500000000000e-01) {
        float dx = x - 1.5625000000000000e-01;
        return ((vec4(-1.3211246088080517e+02,  6.1731462945951478e+01,  9.6199145930320853e+01, alpha) * dx
               + vec4( 1.8896604917048652e+01, -5.7186433584271068e+00, -9.2353000635336890e+00, alpha)) * dx
               + vec4( 4.1265170979798449e-01,  1.2253683588153301e+00, -1.4859407992871662e-01, alpha)) * dx
               + vec4( 5.9571428571428596e-03,  4.0861428571428599e-01,  8.8284285714285704e-01, alpha);
    } ;

    if (x < 1.8750000000000000e-01) {
        float dx = x - 1.7187500000000000e-01;
        return ((vec4(-2.4276114402580023e+02,  1.8878292291818184e+01,  5.4500811814199913e+01, alpha) * dx
               + vec4( 1.2703833313260910e+01, -2.8249810328356313e+00, -4.7259650980498993e+00, alpha)) * dx
               + vec4( 9.0640855714657143e-01,  1.0918742277018498e+00, -3.6673884807846019e-01, alpha)) * dx
               + vec4( 1.6514285714285700e-02,  4.2659999999999998e-01,  8.7863333333333304e-01, alpha);
    } ;

    if (x < 2.0312500000000000e-01) {
        float dx = x - 1.8750000000000000e-01;
        return ((vec4(-2.4875702015890445e+02,  2.7531596458333780e+01,  1.1605149669749400e+01, alpha) * dx
               + vec4( 1.3244046870515243e+00, -1.9400610816566539e+00, -2.1712395442592785e+00, alpha)) * dx
               + vec4( 1.1255997759014531e+00,  1.0174204446629080e+00, -4.7450767061454108e-01, alpha)) * dx
               + vec4( 3.2852380952381001e-02,  4.4304285714285702e-01,  8.7195714285714299e-01, alpha);
    } ;

    if (x < 2.1875000000000000e-01) {
        float dx = x - 2.0312500000000000e-01;
        return ((vec4( 6.6879357994795782e+01,  3.3156266362545779e+00,  3.1398894268734253e+01, alpha) * dx
               + vec4(-1.0336080632897122e+01, -6.4951749767225808e-01, -1.6272481534897754e+00, alpha)) * dx
               + vec4( 9.8479233924761567e-01,  9.7695827936089374e-01, -5.3385904089187008e-01, alpha)) * dx
               + vec4( 4.9814285714285700e-02,  4.5857142857142902e-01,  8.6405714285714297e-01, alpha);
    } ;

    if (x < 2.3437500000000000e-01) {
        float dx = x - 2.1875000000000000e-01;
        return ((vec4(-3.7807546774099214e+00,  2.9110963663947160e+01,  2.0085673255558202e+01, alpha) * dx
               + vec4(-7.2011107268910699e+00, -4.9409749909782474e-01, -1.5542498464285720e-01, alpha)) * dx
               + vec4( 7.1077372425092522e-01,  9.5908929503636120e-01, -5.6171330867519242e-01, alpha)) * dx
               + vec4( 6.2933333333333299e-02,  4.7369047619047600e-01,  8.5543809523809500e-01, alpha);
    } ;

    if (x < 2.5000000000000000e-01) {
        float dx = x - 2.3437500000000000e-01;
        return ((vec4(-1.8052110713761824e+01,  7.5676044216235097e+00,  2.6820241280346455e+01, alpha) * dx
               + vec4(-7.3783336023946600e+00,  8.7047892264969851e-01,  7.8609094921143352e-01, alpha)) * dx
               + vec4( 4.8296990660583561e-01,  9.6497025477935916e-01, -5.5185915297880839e-01, alpha)) * dx
               + vec4( 7.2266666666666701e-02,  4.8866666666666703e-01,  8.4670000000000001e-01, alpha);
    } ;

    if (x < 2.6562500000000000e-01) {
        float dx = x - 2.5000000000000000e-01;
        return ((vec4(-8.5042116753280467e+01,  3.9234694840689350e+01,  6.3623990194130904e+01, alpha) * dx
               + vec4(-8.2245262921022455e+00,  1.2252103799133005e+00,  2.0432897592276738e+00, alpha)) * dx
               + vec4( 2.3917522075432149e-01,  9.9771540013190607e-01, -5.0765007940944740e-01, alpha)) * dx
               + vec4( 7.7942857142857203e-02,  5.0398571428571404e-01,  8.3837142857142899e-01, alpha);
    } ;

    if (x < 2.8125000000000000e-01) {
        float dx = x - 2.6562500000000000e-01;
        return ((vec4(-4.4981860368289709e+01,  3.5222378119677195e+01,  1.8276940800992332e+01, alpha) * dx
               + vec4(-1.2210875514912267e+01,  3.0643367005706139e+00,  5.0256642995775600e+00, alpha)) * dx
               + vec4(-8.0127932480280273e-02,  1.0647395732644671e+00, -3.9719767224061564e-01, alpha)) * dx
               + vec4( 7.9347619047619000e-02,  5.2002380952381000e-01,  8.3118095238095202e-01, alpha);
    } ;

    if (x < 2.9687500000000000e-01) {
        float dx = x - 2.8125000000000000e-01;
        return ((vec4( 8.8958586797831074e+01, -6.4031864461777545e+01, -5.4343639113056135e+01, alpha) * dx
               + vec4(-1.4319400219675847e+01,  4.7153856749304826e+00,  5.8823958996240755e+00, alpha)) * dx
               + vec4(-4.9466349083321959e-01,  1.1862977353816719e+00, -2.2675923162809006e-01, alpha)) * dx
               + vec4( 7.4942857142857103e-02,  5.3754285714285699e-01,  8.2627142857142899e-01, alpha);
    } ;

    if (x < 3.1250000000000000e-01) {
        float dx = x - 2.9687500000000000e-01;
        return ((vec4( 2.3465669412937996e+02, -7.4943148843863256e+01, -1.7040059387215410e+02, alpha) * dx
               + vec4(-1.0149466463527515e+01,  1.7138920282846606e+00,  3.3350378161995691e+00, alpha)) * dx
               + vec4(-8.7698953275827207e-01,  1.2867551994944084e+00, -8.2736829818345611e-02, alpha)) * dx
               + vec4( 6.4057142857142799e-02,  5.5698571428571397e-01,  8.2395714285714305e-01, alpha);
    } ;

    if (x < 3.2812500000000000e-01) {
        float dx = x - 3.1250000000000000e-01;
        return ((vec4( 3.5054309382746595e+02, -7.5598816353949772e+01, -5.9224118732067950e+01, alpha) * dx
               + vec4( 8.5006607378717081e-01, -1.7990680737714295e+00, -4.6524900215576546e+00, alpha)) * dx
               + vec4(-1.0222926638479650e+00,  1.2854243237836778e+00, -1.0332202052706571e-01, alpha)) * dx
               + vec4( 4.8771428571428597e-02,  5.7722380952381003e-01,  8.2282857142857202e-01, alpha);
    } ;

    if (x < 3.4375000000000000e-01) {
        float dx = x - 3.2812500000000000e-01;
        return ((vec4(-1.3511844086782639e+02,  2.1571557117596814e+01,  6.5912402293741552e+00, alpha) * dx
               + vec4( 1.7281773596949638e+01, -5.3427625903628249e+00, -7.4286205871233397e+00, alpha)) * dx
               + vec4(-7.3898266899270237e-01,  1.1738332196565799e+00, -2.9208937378770627e-01, alpha)) * dx
               + vec4( 3.4342857142857203e-02,  5.9658095238095199e-01,  8.1985238095238100e-01, alpha);
    } ;

    if (x < 3.5937500000000000e-01) {
        float dx = x - 3.4375000000000000e-01;
        return ((vec4(-1.6458788273706924e+02,  1.0533768835542057e+01,  3.0362548290707878e+01, alpha) * dx
               + vec4( 1.0948096681270275e+01, -4.3315958504754741e+00, -7.1196562013714262e+00, alpha)) * dx
               + vec4(-2.9789094589551629e-01,  1.0226713690184817e+00, -5.1940619860793691e-01, alpha)) * dx
               + vec4( 2.6499999999999999e-02,  6.1370000000000002e-01,  8.1350000000000000e-01, alpha);
    } ;

    if (x < 3.7500000000000000e-01) {
        float dx = x - 3.5937500000000000e-01;
        return ((vec4(-1.0406115199344315e+02,  1.9929786587720105e+01,  3.6734795179105028e+01, alpha) * dx
               + vec4( 3.2330396779701545e+00, -3.8378254363094402e+00, -5.6964117502444944e+00, alpha)) * dx
               + vec4(-7.6310690282384588e-02,  8.9502416141246732e-01, -7.1965726035193567e-01, alpha)) * dx
               + vec4( 2.3890476190476202e-02,  6.2866190476190498e-01,  8.0376190476190501e-01, alpha);
    } ;

    if (x < 3.9062500000000000e-01) {
        float dx = x - 3.7500000000000000e-01;
        return ((vec4( 2.3255546213942225e+02,  1.8349599099637384e+01,  1.7433813849989207e+01, alpha) * dx
               + vec4(-1.6448268217224928e+00, -2.9036166900100602e+00, -3.9744682262239461e+00, alpha)) * dx
               + vec4(-5.1494864403514876e-02,  7.8968912818872505e-01, -8.7076475998425507e-01, alpha)) * dx
               + vec4( 2.3090476190476199e-02,  6.4178571428571396e-01,  7.9126666666666701e-01, alpha);
    } ;

    if (x < 4.0625000000000000e-01) {
        float dx = x - 3.9062500000000000e-01;
        return ((vec4( 1.5126193200717549e+02,  2.0267550346934740e+01,  2.0857035135376179e+01, alpha) * dx
               + vec4( 9.2562104660629245e+00, -2.0434792322145579e+00, -3.1572582020057021e+00, alpha)) * dx
               + vec4( 6.7433005039304356e-02,  7.1239075440396538e-01, -9.8219798542534331e-01, alpha)) * dx
               + vec4( 2.2771428571428599e-02,  6.5348571428571400e-01,  7.7675714285714303e-01, alpha);
    } ;

    if (x < 4.2187500000000000e-01) {
        float dx = x - 4.0625000000000000e-01;
        return ((vec4( 1.0861181935568159e+02, -5.7969433444380156e+00,  3.9956456082908054e+00, alpha) * dx
               + vec4( 1.6346613528899276e+01, -1.0934378097019919e+00, -2.1795846800349437e+00, alpha)) * dx
               + vec4( 4.6747712996058871e-01,  6.6337642562401933e-01, -1.0655861554572283e+00, alpha)) * dx
               + vec4( 2.6661904761904800e-02,  6.6419523809523795e-01,  7.6071904761904796e-01, alpha);
    } ;

    if (x < 4.3750000000000000e-01) {
        float dx = x - 4.2187500000000000e-01;
        return ((vec4(-3.0484063800132168e+02,  1.4154965887634640e+01, -3.1353889969814710e+00, alpha) * dx
               + vec4( 2.1437792561196851e+01, -1.3651695289725239e+00, -1.9922887921463122e+00, alpha)) * dx
               + vec4( 1.0578584751183406e+00,  6.2496068595722998e-01, -1.1307716784600605e+00, alpha)) * dx
               + vec4( 3.8371428571428598e-02,  6.7427142857142897e-01,  7.4355238095238096e-01, alpha);
    } ;

    if (x < 4.5312500000000000e-01) {
        float dx = x - 4.3750000000000000e-01;
        return ((vec4( 1.9732370744832981e+01, -3.3873392535419122e+00, -5.1854420010455629e+00, alpha) * dx
               + vec4( 7.1483876548848961e+00, -7.0165550298965007e-01, -2.1392601513798186e+00, alpha)) * dx
               + vec4( 1.5045175409946179e+00,  5.9266654483282100e-01, -1.1953271307026563e+00, alpha)) * dx
               + vec4( 5.8971428571428598e-02,  6.8375714285714295e-01,  7.2538571428571397e-01, alpha);
    } ;

    if (x < 4.6875000000000000e-01) {
        float dx = x - 4.5312500000000000e-01;
        return ((vec4(-5.2460806882781675e+01, -6.0560887320505685e-01,  1.3890718905419471e+01, alpha) * dx
               + vec4( 8.0733425335489422e+00, -8.6043703049942721e-01, -2.3823277451788294e+00, alpha)) * dx
               + vec4( 1.7423570751888966e+00,  5.6825884899705426e-01, -1.2659769415863851e+00, alpha)) * dx
               + vec4( 8.4300000000000000e-02,  6.9283333333333297e-01,  7.0616666666666705e-01, alpha);
    } ;

    if (x < 4.8437500000000000e-01) {
        float dx = x - 4.6875000000000000e-01;
        return ((vec4( 1.0354971072183483e+01,  5.8097747460711062e+00, -5.4384621916749820e+00, alpha) * dx
               + vec4( 5.6142422109185510e+00, -8.8882494643091425e-01, -1.7312002964872917e+00, alpha)) * dx
               + vec4( 1.9562255868212013e+00,  5.4092663060751767e-01, -1.3302508172374183e+00, alpha)) * dx
               + vec4( 1.1329523809523800e-01,  7.0150000000000001e-01,  6.8585714285714305e-01, alpha);
    } ;

    if (x < 5.0000000000000000e-01) {
        float dx = x - 4.8437500000000000e-01;
        return ((vec4(-1.3925172644537971e+01, -8.9021377300786071e+00, -4.6199177582688593e+00, alpha) * dx
               + vec4( 6.0996314799271518e+00, -6.1649175520883115e-01, -1.9861282117220564e+00, alpha)) * dx
               + vec4( 2.1392548632406654e+00,  5.1740605714439658e-01, -1.3883340751781894e+00, alpha)) * dx
               + vec4( 1.4527142857142900e-01,  7.0975714285714298e-01,  6.6462857142857201e-01, alpha);
    } ;

    if (x < 5.1562500000000000e-01) {
        float dx = x - 5.0000000000000000e-01;
        return ((vec4( 3.1614367125520630e+01, -1.1395280968671647e+01,  2.1421523701702025e+01, alpha) * dx
               + vec4( 5.4468890122144344e+00, -1.0337794613062659e+00, -2.2026868566409092e+00, alpha)) * dx
               + vec4( 2.3196692459303776e+00,  4.9162056938634824e-01, -1.4537843106213608e+00, alpha)) * dx
               + vec4( 1.8013333333333301e-01,  7.1765714285714299e-01,  6.4243333333333297e-01, alpha);
    } ;

    if (x < 5.3125000000000000e-01) {
        float dx = x - 5.1562500000000000e-01;
        return ((vec4(-3.7634010143333590e+01,  2.0544616050328934e+00,  1.3219372364175872e+00, alpha) * dx
               + vec4( 6.9288124712232140e+00, -1.5679332567127493e+00, -1.1985529331236269e+00, alpha)) * dx
               + vec4( 2.5130395816090907e+00,  4.5096880816730112e-01, -1.5069286823364316e+00, alpha)) * dx
               + vec4( 2.1782857142857101e-01,  7.2504285714285699e-01,  6.1926190476190501e-01, alpha);
    } ;

    if (x < 5.4687500000000000e-01) {
        float dx = x - 5.3125000000000000e-01;
        return ((vec4( 1.2815768685879013e+01, -1.4298832118473902e+01,  3.9450879734146490e+01, alpha) * dx
               + vec4( 5.1647182457544520e+00, -1.4716303689768324e+00, -1.1365871251665525e+00, alpha)) * dx
               + vec4( 2.7020009990618670e+00,  4.0347562651590141e-01, -1.5434152457472157e+00, alpha)) * dx
               + vec4( 2.5864285714285701e-01,  7.3171428571428598e-01,  5.9542857142857097e-01, alpha);
    } ;

    if (x < 5.6250000000000000e-01) {
        float dx = x - 5.4687500000000000e-01;
        return ((vec4(-7.8540912219456771e+01, -1.8509114083431125e+01,  3.3113477160250433e+01, alpha) * dx
               + vec4( 5.7654574029050307e+00, -2.1418881245302965e+00,  7.1267286237156402e-01, alpha)) * dx
               + vec4( 2.8727849935721714e+00,  3.4701440005485251e-01, -1.5500389061033872e+00, alpha)) * dx
               + vec4( 3.0217142857142898e-01,  7.3760476190476199e-01,  5.7118571428571396e-01, alpha);
    } ;

    if (x < 5.7812500000000000e-01) {
        float dx = x - 5.6250000000000000e-01;
        return ((vec4(-5.8163891236508938e+01,  9.6920884524980497e+00,  3.0320583052976861e+01, alpha) * dx
               + vec4( 2.0838521426179946e+00, -3.0095028471911305e+00,  2.2648671042583031e+00, alpha)) * dx
               + vec4( 2.9954304552209687e+00,  2.6652391612170523e-01, -1.5035148441247956e+00, alpha)) * dx
               + vec4( 3.4816666666666701e-01,  7.4243333333333295e-01,  5.4726666666666701e-01, alpha);
    } ;

    if (x < 5.9375000000000000e-01) {
        float dx = x - 5.7812500000000000e-01;
        return ((vec4(-6.4543256167712116e+01, -2.8636353652780144e-01,  2.8905906284068501e+00, alpha) * dx
               + vec4(-6.4258025909336181e-01, -2.5551862009802844e+00,  3.6861444348665935e+00, alpha)) * dx
               + vec4( 3.0179503284010409e+00,  1.7957564974402687e-01, -1.4105302888259692e+00, alpha)) * dx
               + vec4( 3.9525714285714297e-01,  7.4590000000000001e-01,  5.2444285714285699e-01, alpha);
    } ;

    if (x < 6.0937500000000000e-01) {
        float dx = x - 5.9375000000000000e-01;
        return ((vec4(-2.4450284092939786e+01,  1.3922851408411924e+01, -1.6916850328844372e+01, alpha) * dx
               + vec4(-3.6680453919548675e+00, -2.5686094917550251e+00,  3.8216408705731646e+00, alpha)) * dx
               + vec4( 2.9505968026034126e+00,  9.9516342045037676e-02, -1.2932211434284731e+00, alpha)) * dx
               + vec4( 4.4200952380952402e-01,  7.4808095238095196e-01,  5.0331428571428605e-01, alpha);
    } ;

    if (x < 6.2500000000000000e-01) {
        float dx = x - 6.0937500000000000e-01;
        return ((vec4( 1.2547821111311350e+01,  1.5748329330961459e+01, -1.7611303598786566e+01, alpha) * dx
               + vec4(-4.8141524588114200e+00, -1.9159758319857161e+00,  3.0286635114085847e+00, alpha)) * dx
               + vec4( 2.8180624611851890e+00,  2.9444696361588602e-02, -1.1861851374600081e+00, alpha)) * dx
               + vec4( 4.8712380952380901e-01,  7.4906190476190504e-01,  4.8397619047619100e-01, alpha);
    } ;

    if (x < 6.4062500000000000e-01) {
        float dx = x - 6.2500000000000000e-01;
        return ((vec4( 9.2115329809656430e+00, -3.2661877796437579e+00, -1.2675733711774058e+00, alpha) * dx
               + vec4(-4.2259733442187004e+00, -1.1777728945968977e+00,  2.2031336552154643e+00, alpha)) * dx
               + vec4( 2.6768104955128438e+00, -1.8895127491264742e-02, -1.1044383067315073e+00, alpha)) * dx
               + vec4( 5.3002857142857096e-01,  7.4911428571428595e-01,  4.6611428571428598e-01, alpha);
    } ;

    if (x < 6.5625000000000000e-01) {
        float dx = x - 6.4062500000000000e-01;
        return ((vec4( 1.4269589821681299e+01,  7.3028598827757278e+00, -8.5260219639800940e+00, alpha) * dx
               + vec4(-3.7941827357359359e+00, -1.3308754467676989e+00,  2.1437161534415234e+00, alpha)) * dx
               + vec4( 2.5514955567635522e+00, -5.8092757825086563e-02, -1.0365187784712420e+00, alpha)) * dx
               + vec4( 5.7085714285714295e-01,  7.4851904761904797e-01,  4.4939047619047601e-01, alpha);
    } ;

    if (x < 6.7187500000000000e-01) {
        float dx = x - 6.5625000000000000e-01;
        return ((vec4( 8.6083934467238432e+00,  2.6914824850885094e-01, -1.7057138772896455e+01, alpha) * dx
               + vec4(-3.1252957128446250e+00, -9.8855388976258662e-01,  1.7440588738799565e+00, alpha)) * dx
               + vec4( 2.4433787060044811e+00, -9.4333841208372265e-02, -9.7577229366934382e-01, alpha)) * dx
               + vec4( 6.0985238095238103e-01,  7.4731428571428604e-01,  4.3368571428571401e-01, alpha);
    } ;

    if (x < 6.8750000000000000e-01) {
        float dx = x - 6.7187500000000000e-01;
        return ((vec4( 8.7188554392023345e+00,  1.7834947123447904e+01, -1.8886229447019101e+00, alpha) * dx
               + vec4(-2.7217772700294449e+00, -9.7593756561373424e-01,  9.4450549390043514e-01, alpha)) * dx
               + vec4( 2.3520181906470738e+00, -1.2502902019862727e-01, -9.3376347542277516e-01, alpha)) * dx
               + vec4( 6.4729999999999999e-01,  7.4560000000000004e-01,  4.1880000000000001e-01, alpha);
    } ;

    if (x < 7.0312500000000000e-01) {
        float dx = x - 6.8750000000000000e-01;
        return ((vec4( 8.9449847961700044e+00, -2.1676746266635202e+01, -4.0993789718798466e+00, alpha) * dx
               + vec4(-2.3130809213168355e+00, -1.3992441920211368e-01,  8.5597629336753311e-01, alpha)) * dx
               + vec4( 2.2733485314072883e+00, -1.4246436371137491e-01, -9.0563094749671313e-01, alpha)) * dx
               + vec4( 6.8341904761904804e-01,  7.4347619047619096e-01,  4.0443333333333298e-01, alpha);
    } ;

    if (x < 7.1875000000000000e-01) {
        float dx = x - 7.0312500000000000e-01;
        return ((vec4( 1.1674919661892304e+01,  2.3933066515154213e+01, -1.1673175453308831e+01, alpha) * dx
               + vec4(-1.8937847589963666e+00, -1.1560219004506387e+00,  6.6381790406066532e-01, alpha)) * dx
               + vec4( 2.2076162551523946e+00, -1.6271352495594915e-01, -8.8188416316189755e-01, alpha)) * dx
               + vec4( 7.1840952380952405e-01,  7.4113333333333298e-01,  3.9047619047618998e-01, alpha);
    } ;

    if (x < 7.3437500000000000e-01) {
        float dx = x - 7.1875000000000000e-01;
        return ((vec4(-4.4641682053710623e+00,  2.0910706819426692e+00,  4.6048045942407727e+00, alpha) * dx
               + vec4(-1.3465228998451648e+00, -3.4159407552784897e-02,  1.1663780468681384e-01, alpha)) * dx
               + vec4( 2.1569864479829954e+00, -1.8131010789350266e-01, -8.6968954271271826e-01, alpha)) * dx
               + vec4( 7.5248571428571398e-01,  7.3839999999999995e-01,  3.7681428571428599e-01, alpha);
    } ;

    if (x < 7.5000000000000000e-01) {
        float dx = x - 7.3437500000000000e-01;
        return ((vec4( 1.2423276968973711e+01, -6.0829492432479162e+00, -2.1725700066572116e+01, alpha) * dx
               + vec4(-1.5557807844719334e+00,  6.3859530663277708e-02,  3.3248802004185007e-01, alpha)) * dx
               + vec4( 2.1116379529155407e+00, -1.8084604346990121e-01, -8.6267195170133282e-01, alpha)) * dx
               + vec4( 7.8584285714285695e-01,  7.3556666666666704e-01,  3.6327142857142902e-01, alpha);
    } ;

    if (x < 7.6562500000000000e-01) {
        float dx = x - 7.5000000000000000e-01;
        return ((vec4( 3.4549460436900552e+00,  2.2240726291601970e+01, -7.5799471847609725e+00, alpha) * dx
               + vec4(-9.7343967655129060e-01, -2.2127871511396835e-01, -6.8590417057871789e-01, alpha)) * dx
               + vec4( 2.0721188832120530e+00, -1.8330571822694325e-01, -8.6819407905347146e-01, alpha)) * dx
               + vec4( 8.1850476190476196e-01,  7.3273333333333301e-01,  3.4979047619047599e-01, alpha);
    } ;

    if (x < 7.8125000000000000e-01) {
        float dx = x - 7.6562500000000000e-01;
        return ((vec4( 8.7094721894791203e+00,  1.3239510743088688e+01, -2.2852796908624047e+01, alpha) * dx
               + vec4(-8.1148908075331927e-01,  8.2125532980487381e-01, -1.0412141948643885e+00, alpha)) * dx
               + vec4( 2.0442293713791684e+00, -1.7393108362239784e-01, -8.9518030351351996e-01, alpha)) * dx
               + vec4( 8.5065714285714300e-01,  7.2989999999999999e-01,  3.3602857142857101e-01, alpha);
    } ;

    if (x < 7.9687500000000000e-01) {
        float dx = x - 7.8125000000000000e-01;
        return ((vec4(-1.2078434801289291e+01,  4.3390183117236198e+01, -3.9570693752303733e+01, alpha) * dx
               + vec4(-4.0323257187148548e-01,  1.4418573958871561e+00, -2.1124390499561407e+00, alpha)) * dx
               + vec4( 2.0252493455569058e+00, -1.3856994728345987e-01, -9.4445613546384066e-01, alpha)) * dx
               + vec4( 8.8243333333333296e-01,  7.2743333333333304e-01,  3.2169999999999999e-01, alpha);
    } ;

    if (x < 8.1250000000000000e-01) {
        float dx = x - 7.9687500000000000e-01;
        return ((vec4(-1.2824532984374384e+01,  1.1653781393088177e+02, -1.1096774236821523e+02, alpha) * dx
               + vec4(-9.6940920318192092e-01,  3.4757722295076028e+00, -3.9673153195953783e+00, alpha)) * dx
               + vec4( 2.0038018178216963e+00, -6.1731984386666772e-02, -1.0394522974880831e+00, alpha)) * dx
               + vec4( 9.1393333333333304e-01,  7.2578571428571403e-01,  3.0627619047619098e-01, alpha);
    } ;

    if (x < 8.2812500000000000e-01) {
        float dx = x - 8.1250000000000000e-01;
        return ((vec4(-3.5855044278532131e+02,  2.7064903734930277e+02, -8.0792089155266083e+01, alpha) * dx
               + vec4(-1.5705591868244702e+00,  8.9384822575176859e+00, -9.1689282431054675e+00, alpha)) * dx
               + vec4( 1.9641148117278464e+00,  1.3224074197310332e-01, -1.2447061031552840e+00, alpha)) * dx
               + vec4( 9.4495714285714305e-01,  7.2611428571428605e-01,  2.8864285714285698e-01, alpha);
    } ;

    if (x < 8.4375000000000000e-01) {
        float dx = x - 8.2812500000000000e-01;
        return ((vec4(-3.8174017206443654e+02, -1.9549693475620506e+02,  4.4911575613188438e+02, alpha) * dx
               + vec4(-1.8377611192386407e+01,  2.1625155883266252e+01, -1.2956057422258565e+01, alpha)) * dx
               + vec4( 1.6524246495526764e+00,  6.0979758792285232e-01, -1.5904090041765968e+00, alpha)) * dx
               + vec4( 9.7389523809523804e-01,  7.3139523809523799e-01,  2.6664761904761902e-01, alpha);
    } ;

    if (x < 8.5937500000000000e-01) {
        float dx = x - 8.4375000000000000e-01;
        return ((vec4( 4.3248438818547703e+02, -2.7134838403902307e+02,  3.3204036056432756e+01, alpha) * dx
               + vec4(-3.6271681757906869e+01,  1.2461237066569140e+01,  8.0962436464235150e+00, alpha)) * dx
               + vec4( 7.9852944720434427e-01,  1.1423974777640304e+00, -1.6663435944240195e+00, alpha)) * dx
               + vec4( 9.9377142857142897e-01,  7.4545714285714304e-01,  2.4034761904761900e-01, alpha);
    } ;

    if (x < 8.7500000000000000e-01) {
        float dx = x - 8.5937500000000000e-01;
        return ((vec4( 1.7847934313241271e+02, -6.1117386114828536e+00, -1.0882439559595376e+02, alpha) * dx
               + vec4(-1.5998976061712632e+01, -2.5821843526006538e-01,  9.6526828365688004e+00, alpha)) * dx
               + vec4(-1.8199581227210410e-02,  1.3330696438782346e+00, -1.3890166181272647e+00, alpha)) * dx
               + vec4( 9.9904285714285701e-01,  7.6531428571428595e-01,  2.1641428571428600e-01, alpha);
    } ;

    if (x < 8.9062500000000000e-01) {
        float dx = x - 8.7500000000000000e-01;
        return ((vec4( 1.0065469642774150e+02,  1.1181852770679304e+01, -4.2302948910418884e+01, alpha) * dx
               + vec4(-7.6327568523807861e+00, -5.4470618267332416e-01,  4.5515392930084682e+00, alpha)) * dx
               + vec4(-3.8744540800992006e-01,  1.3205239467230254e+00, -1.1670756473526198e+00, alpha)) * dx
               + vec4( 9.9553333333333305e-01,  7.8605714285714301e-01,  1.9665238095238100e-01, alpha);
    } ;

    if (x < 9.0625000000000000e-01) {
        float dx = x - 8.9062500000000000e-01;
        return ((vec4( 5.1792385442186948e+01,  1.3813127528788970e+01, -4.7771351619749993e+01, alpha) * dx
               + vec4(-2.9145679573304033e+00, -2.0556834047731776e-02,  2.5685885628325829e+00, alpha)) * dx
               + vec4(-5.5224735816165738e-01,  1.3116917120867588e+00, -1.0558236496051034e+00, alpha)) * dx
               + vec4( 9.8799999999999999e-01,  8.0659999999999998e-01,  1.7936666666666701e-01, alpha);
    } ;

    if (x < 9.2187500000000000e-01) {
        float dx = x - 9.0625000000000000e-01;
        return ((vec4( 1.1035785704157649e+02,  5.2154589495154021e+01, -3.9990387467675163e+01, alpha) * dx
               + vec4(-4.8679988972789023e-01,  6.2693351886425119e-01,  3.2930645565680206e-01, alpha)) * dx
               + vec4(-6.0539373077194325e-01,  1.3211663477870170e+00, -1.0105440399412067e+00, alpha)) * dx
               + vec4( 9.7885714285714298e-01,  8.2714285714285696e-01,  1.6331428571428599e-01, alpha);
    } ;

    if (x < 9.3750000000000000e-01) {
        float dx = x - 9.2187500000000000e-01;
        return ((vec4( 4.6043843534396274e+01,  2.0987943062129727e+01, -2.3203479461840441e+01, alpha) * dx
               + vec4( 4.6862246590960082e+00,  3.0716799014495959e+00, -1.5452429568904713e+00, alpha)) * dx
               + vec4(-5.3977771875056635e-01,  1.3789571824794209e+00, -1.0295430477729828e+00, alpha)) * dx
               + vec4( 9.6970000000000001e-01,  8.4813809523809502e-01,  1.4745238095238100e-01, alpha);
    } ;

    if (x < 9.5312500000000000e-01) {
        float dx = x - 9.3750000000000000e-01;
        return ((vec4( 6.1233625963980650e+01,  2.8669866827404956e+01,  2.4201791029260814e+01, alpha) * dx
               + vec4( 6.8445298247708335e+00,  4.0554897324869268e+00, -2.6329060566642419e+00, alpha)) * dx
               + vec4(-3.5960967994014698e-01,  1.4903192080096790e+00, -1.0948266261097752e+00, alpha)) * dx
               + vec4( 9.6258571428571404e-01,  8.7051428571428602e-01,  1.3089999999999999e-01, alpha);
    } ;

    if (x < 9.6875000000000000e-01) {
        float dx = x - 9.5312500000000000e-01;
        return ((vec4( 4.1070719275903762e+01,  5.3910277236601019e+00,  2.0019172487757277e+01, alpha) * dx
               + vec4( 9.7148560418324266e+00,  5.3993897400215340e+00, -1.4984471021676413e+00, alpha)) * dx
               + vec4(-1.0086927577447102e-01,  1.6380516997676238e+00, -1.1593790192165234e+00, alpha)) * dx
               + vec4( 9.5887142857142904e-01,  8.9490000000000003e-01,  1.1324285714285701e-01, alpha);
    } ;

    if (x < 9.8437500000000000e-01) {
        float dx = x - 9.6875000000000000e-01;
        return ((vec4(-5.3250445924665847e+01, -1.6529749150400146e+01, -1.4422423336140781e+02, alpha) * dx
               + vec4( 1.1640046007890415e+01,  5.6520941645681013e+00, -5.6004839180401900e-01, alpha)) * dx
               + vec4( 2.3280106875244833e-01,  1.8107311357768368e+00, -1.1915430113098306e+00, alpha)) * dx
               + vec4( 9.5982380952380997e-01,  9.2183333333333295e-01,  9.4838095238095305e-02, alpha);
    } ;

    if (x < 1.0000000000000000e+00) {
        float dx = x - 9.8437500000000000e-01;
        return ((vec4(-1.9507053557699635e+02, -1.0404825969371934e+02,  1.5617193238656020e+02, alpha) * dx
               + vec4( 9.1439313551717039e+00,  4.8772621731430945e+00, -7.3205593306200099e+00, alpha)) * dx
               + vec4( 5.5755071505029385e-01,  1.9752523285535741e+00, -1.3146775069727061e+00, alpha)) * dx
               + vec4( 9.6609999999999996e-01,  9.5144285714285703e-01,  7.5533333333333300e-02, alpha);
    }

    {
        float dx = x - 1.0000000000000000e+00;
        return ((vec4( 0.0000000000000000e+00,  3.4202936336155174e+00,  3.0625241907655076e+00, alpha) * dx
               + vec4( 0.0000000000000000e+00,  0.0000000000000000e+00,  0.0000000000000000e+00, alpha)) * dx
               + vec4( 0.0000000000000000e+00,  0.0000000000000000e+00,  0.0000000000000000e+00, alpha)) * dx
               + vec4( 9.7629999999999995e-01,  9.8309999999999997e-01,  5.3800000000000001e-02, alpha);
    }
}

vec4 colormap_inferno (float x, float alpha) {
  const float e0 = 0.0;
  vec4 v0 = vec4(0,0,0.01568627450980392,alpha);
  const float e1 = 0.13;
  vec4 v1 = vec4(0.12156862745098039,0.047058823529411764,0.2823529411764706,alpha);
  const float e2 = 0.25;
  vec4 v2 = vec4(0.3333333333333333,0.058823529411764705,0.42745098039215684,alpha);
  const float e3 = 0.38;
  vec4 v3 = vec4(0.5333333333333333,0.13333333333333333,0.41568627450980394,alpha);
  const float e4 = 0.5;
  vec4 v4 = vec4(0.7294117647058823,0.21176470588235294,0.3333333333333333,alpha);
  const float e5 = 0.63;
  vec4 v5 = vec4(0.8901960784313725,0.34901960784313724,0.2,alpha);
  const float e6 = 0.75;
  vec4 v6 = vec4(0.9764705882352941,0.5490196078431373,0.0392156862745098,alpha);
  const float e7 = 0.88;
  vec4 v7 = vec4(0.9764705882352941,0.788235294117647,0.19607843137254902,alpha);
  const float e8 = 1.0;
  vec4 v8 = vec4(0.9882352941176471,1,0.6431372549019608,alpha);
  float a0 = smoothstep(e0,e1,x);
  float a1 = smoothstep(e1,e2,x);
  float a2 = smoothstep(e2,e3,x);
  float a3 = smoothstep(e3,e4,x);
  float a4 = smoothstep(e4,e5,x);
  float a5 = smoothstep(e5,e6,x);
  float a6 = smoothstep(e6,e7,x);
  float a7 = smoothstep(e7,e8,x);
  return max(mix(v0,v1,a0)*step(e0,x)*step(x,e1),
    max(mix(v1,v2,a1)*step(e1,x)*step(x,e2),
    max(mix(v2,v3,a2)*step(e2,x)*step(x,e3),
    max(mix(v3,v4,a3)*step(e3,x)*step(x,e4),
    max(mix(v4,v5,a4)*step(e4,x)*step(x,e5),
    max(mix(v5,v6,a5)*step(e5,x)*step(x,e6),
    max(mix(v6,v7,a6)*step(e6,x)*step(x,e7),mix(v7,v8,a7)*step(e7,x)*step(x,e8)
  )))))));
}

vec4 colormap_magma (float x, float alpha) {
  const float e0 = 0.0;
  vec4 v0 = vec4(0,0,0.01568627450980392,alpha);
  const float e1 = 0.13;
  vec4 v1 = vec4(0.10980392156862745,0.06274509803921569,0.26666666666666666,alpha);
  const float e2 = 0.25;
  vec4 v2 = vec4(0.30980392156862746,0.07058823529411765,0.4823529411764706,alpha);
  const float e3 = 0.38;
  vec4 v3 = vec4(0.5058823529411764,0.1450980392156863,0.5058823529411764,alpha);
  const float e4 = 0.5;
  vec4 v4 = vec4(0.7098039215686275,0.21176470588235294,0.47843137254901963,alpha);
  const float e5 = 0.63;
  vec4 v5 = vec4(0.8980392156862745,0.3137254901960784,0.39215686274509803,alpha);
  const float e6 = 0.75;
  vec4 v6 = vec4(0.984313725490196,0.5294117647058824,0.3803921568627451,alpha);
  const float e7 = 0.88;
  vec4 v7 = vec4(0.996078431372549,0.7607843137254902,0.5294117647058824,alpha);
  const float e8 = 1.0;
  vec4 v8 = vec4(0.9882352941176471,0.9921568627450981,0.7490196078431373,alpha);
  float a0 = smoothstep(e0,e1,x);
  float a1 = smoothstep(e1,e2,x);
  float a2 = smoothstep(e2,e3,x);
  float a3 = smoothstep(e3,e4,x);
  float a4 = smoothstep(e4,e5,x);
  float a5 = smoothstep(e5,e6,x);
  float a6 = smoothstep(e6,e7,x);
  float a7 = smoothstep(e7,e8,x);
  return max(mix(v0,v1,a0)*step(e0,x)*step(x,e1),
    max(mix(v1,v2,a1)*step(e1,x)*step(x,e2),
    max(mix(v2,v3,a2)*step(e2,x)*step(x,e3),
    max(mix(v3,v4,a3)*step(e3,x)*step(x,e4),
    max(mix(v4,v5,a4)*step(e4,x)*step(x,e5),
    max(mix(v5,v6,a5)*step(e5,x)*step(x,e6),
    max(mix(v6,v7,a6)*step(e6,x)*step(x,e7),mix(v7,v8,a7)*step(e7,x)*step(x,e8)
  )))))));
}

vec4 colormap_plasma (float x, float alpha) {
  const float e0 = 0.0;
  vec4 v0 = vec4(0.050980392156862744,0.03137254901960784,0.5294117647058824,alpha);
  const float e1 = 0.13;
  vec4 v1 = vec4(0.29411764705882354,0.011764705882352941,0.6313725490196078,alpha);
  const float e2 = 0.25;
  vec4 v2 = vec4(0.49019607843137253,0.011764705882352941,0.6588235294117647,alpha);
  const float e3 = 0.38;
  vec4 v3 = vec4(0.6588235294117647,0.13333333333333333,0.5882352941176471,alpha);
  const float e4 = 0.5;
  vec4 v4 = vec4(0.796078431372549,0.27450980392156865,0.4745098039215686,alpha);
  const float e5 = 0.63;
  vec4 v5 = vec4(0.8980392156862745,0.4196078431372549,0.36470588235294116,alpha);
  const float e6 = 0.75;
  vec4 v6 = vec4(0.9725490196078431,0.5803921568627451,0.2549019607843137,alpha);
  const float e7 = 0.88;
  vec4 v7 = vec4(0.9921568627450981,0.7647058823529411,0.1568627450980392,alpha);
  const float e8 = 1.0;
  vec4 v8 = vec4(0.9411764705882353,0.9764705882352941,0.12941176470588237,alpha);
  float a0 = smoothstep(e0,e1,x);
  float a1 = smoothstep(e1,e2,x);
  float a2 = smoothstep(e2,e3,x);
  float a3 = smoothstep(e3,e4,x);
  float a4 = smoothstep(e4,e5,x);
  float a5 = smoothstep(e5,e6,x);
  float a6 = smoothstep(e6,e7,x);
  float a7 = smoothstep(e7,e8,x);
  return max(mix(v0,v1,a0)*step(e0,x)*step(x,e1),
    max(mix(v1,v2,a1)*step(e1,x)*step(x,e2),
    max(mix(v2,v3,a2)*step(e2,x)*step(x,e3),
    max(mix(v3,v4,a3)*step(e3,x)*step(x,e4),
    max(mix(v4,v5,a4)*step(e4,x)*step(x,e5),
    max(mix(v5,v6,a5)*step(e5,x)*step(x,e6),
    max(mix(v6,v7,a6)*step(e6,x)*step(x,e7),mix(v7,v8,a7)*step(e7,x)*step(x,e8)
  )))))));
}

vec4 colormap_viridis (float x, float alpha) {
  const float e0 = 0.0;
  vec4 v0 = vec4(0.26666666666666666,0.00392156862745098,0.32941176470588235,alpha);
  const float e1 = 0.13;
  vec4 v1 = vec4(0.2784313725490196,0.17254901960784313,0.47843137254901963,alpha);
  const float e2 = 0.25;
  vec4 v2 = vec4(0.23137254901960785,0.3176470588235294,0.5450980392156862,alpha);
  const float e3 = 0.38;
  vec4 v3 = vec4(0.17254901960784313,0.44313725490196076,0.5568627450980392,alpha);
  const float e4 = 0.5;
  vec4 v4 = vec4(0.12941176470588237,0.5647058823529412,0.5529411764705883,alpha);
  const float e5 = 0.63;
  vec4 v5 = vec4(0.15294117647058825,0.6784313725490196,0.5058823529411764,alpha);
  const float e6 = 0.75;
  vec4 v6 = vec4(0.3607843137254902,0.7843137254901961,0.38823529411764707,alpha);
  const float e7 = 0.88;
  vec4 v7 = vec4(0.6666666666666666,0.8627450980392157,0.19607843137254902,alpha);
  const float e8 = 1.0;
  vec4 v8 = vec4(0.9921568627450981,0.9058823529411765,0.1450980392156863,alpha);
  float a0 = smoothstep(e0,e1,x);
  float a1 = smoothstep(e1,e2,x);
  float a2 = smoothstep(e2,e3,x);
  float a3 = smoothstep(e3,e4,x);
  float a4 = smoothstep(e4,e5,x);
  float a5 = smoothstep(e5,e6,x);
  float a6 = smoothstep(e6,e7,x);
  float a7 = smoothstep(e7,e8,x);
  return max(mix(v0,v1,a0)*step(e0,x)*step(x,e1),
    max(mix(v1,v2,a1)*step(e1,x)*step(x,e2),
    max(mix(v2,v3,a2)*step(e2,x)*step(x,e3),
    max(mix(v3,v4,a3)*step(e3,x)*step(x,e4),
    max(mix(v4,v5,a4)*step(e4,x)*step(x,e5),
    max(mix(v5,v6,a5)*step(e5,x)*step(x,e6),
    max(mix(v6,v7,a6)*step(e6,x)*step(x,e7),mix(v7,v8,a7)*step(e7,x)*step(x,e8)
  )))))));
}

vec4 colormap_jet_prev(float x, float alpha) {
  vec4 result;
  result.r = x < 0.89 ? ((x - 0.35) / 0.31) : (1.0 - (x - 0.89) / 0.11 * 0.5);
  result.g = x < 0.64 ? ((x - 0.125) * 4.0) : (1.0 - (x - 0.64) / 0.27);
  result.b = x < 0.34 ? (0.5 + x * 0.5 / 0.11) : (1.0 - (x - 0.34) / 0.31);
  result.a = alpha;
  return clamp(result, 0.0, 1.0);
}

vec4 colormap_jet(float x, float alpha) {
    vec4 result;
    result.r = (x < 0.7) ? (4.0 * x - 1.5) : (-4.0 * x + 4.5);
    result.g = (x < 0.5) ? (4.0 * x - 0.5) : (-4.0 * x + 3.5);
    result.b = (x < 0.3) ? (4.0 * x + 0.5) : (-4.0 * x + 2.5);
    result.a = alpha;
    return clamp(result, 0.0, 1.0);
}

/*
 * Adapted from http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/CubeHelix.m
 * which is licensed under http://unlicense.org/
 */
vec4 colormap_cubehelix(float x, float alpha) {
    float xclamp = clamp(x, 0.0, 1.0);
    float angle = 2.0 * 3.1415926 * (4.0 / 3.0 + xclamp);
    float amp = xclamp * (1.0 - xclamp) / 2.0;
    vec4 result;
    float cosangle = cos(angle);
    float sinangle = sin(angle);
    result.r = -0.14861 * cosangle + 1.78277 * sinangle;
    result.g = -0.29227 * cosangle + -0.90649 * sinangle;
    result.b = 1.97294 * cosangle;    
    result = clamp(xclamp + amp * result, 0.0, 1.0);
    result.a = alpha;
    return result;
}

vec4 colormap_haxby(float x, float alpha) {
    float dx = 1.0 / (64.0 - 1.0);

    if( x < 0.0)
        return vec4(0,0,0,0);

    if( x < 1.0 * dx) {
        vec4 x1 = vec4(0.1450980392156863, 0.2235294117647059, 0.6862745098039216, alpha);
        vec4 x2 = vec4(0.1469654528478058, 0.2671023965141612, 0.7335823218176158, alpha);
        float a = x / dx;
        return mix(x1, x2, a);
    }

    if( x < 2.0 * dx) {        
        vec4 x1 = vec4(0.1469654528478058, 0.2671023965141612, 0.7335823218176158, alpha);
        vec4 x2 = vec4(0.1488328664799253, 0.3106753812636166, 0.7808901338313103, alpha);
        float a = (x - 1.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 3.0 * dx) {                
        vec4 x1 = vec4(0.1488328664799253, 0.3106753812636166, 0.7808901338313103, alpha);
        vec4 x2 = vec4(0.1507002801120448, 0.3542483660130719, 0.8281979458450047, alpha);
        float a = (x - 2.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 4.0 * dx) {                        
        vec4 x1 = vec4(0.1507002801120448, 0.3542483660130719, 0.8281979458450047, alpha);
        vec4 x2 = vec4(0.1525676937441643, 0.3978213507625272, 0.875505757858699, alpha);
        float a = (x - 3.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 5.0 * dx) {                                
        vec4 x1 = vec4(0.1525676937441643, 0.3978213507625272, 0.875505757858699, alpha);
        vec4 x2 = vec4(0.1544351073762839, 0.4413943355119826, 0.9228135698723934, alpha);
        float a = (x - 4.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 6.0 * dx) {                                        
        vec4 x1 = vec4(0.1544351073762839, 0.4413943355119826, 0.9228135698723934, alpha);
        vec4 x2 = vec4(0.1563025210084034, 0.4849673202614379, 0.9701213818860878, alpha);
        float a = (x - 5.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 7.0 * dx) {                                                
        vec4 x1 = vec4(0.1563025210084034, 0.4849673202614379, 0.9701213818860878, alpha);
        vec4 x2 = vec4(0.1612200435729848, 0.5254901960784314, 0.9860566448801743, alpha);
        float a = (x - 6.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 8.0 * dx) {                                                        
        vec4 x1 = vec4(0.1612200435729848, 0.5254901960784314, 0.9860566448801743, alpha);
        vec4 x2 = vec4(0.1674447556800498, 0.5647058823529412, 0.9885465297230004, alpha);
        float a = (x - 7.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 9.0 * dx) {                                                                
        vec4 x1 = vec4(0.1674447556800498, 0.5647058823529412, 0.9885465297230004, alpha);
        vec4 x2 = vec4(0.1736694677871148, 0.6039215686274509, 0.9910364145658264, alpha);
        float a = (x - 8.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 10.0 * dx) {                                                                        
        vec4 x1 = vec4(0.1736694677871148, 0.6039215686274509, 0.9910364145658264, alpha);
        vec4 x2 = vec4(0.1798941798941799, 0.6431372549019608, 0.9935262994086523, alpha);
        float a = (x - 9.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 11.0 * dx) {                                                                                
        vec4 x1 = vec4(0.1798941798941799, 0.6431372549019608, 0.9935262994086523, alpha);
        vec4 x2 = vec4(0.186118892001245, 0.6823529411764706, 0.9960161842514784, alpha);
        float a = (x - 10.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 12.0 * dx) {                                                                                        
        vec4 x1 = vec4(0.186118892001245, 0.6823529411764706, 0.9960161842514784, alpha);
        vec4 x2 = vec4(0.19234360410831, 0.7215686274509804, 0.9985060690943044, alpha);
        float a = (x - 11.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 13.0 * dx) {                                                                                                
        vec4 x1 = vec4(0.19234360410831, 0.7215686274509804, 0.9985060690943044, alpha);
        vec4 x2 = vec4(0.2100217864923747, 0.7563025210084033, 1, alpha);
        float a = (x - 12.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 14.0 * dx) {                                                                                                        
        vec4 x1 = vec4(0.2100217864923747, 0.7563025210084033, 1, alpha);
        vec4 x2 = vec4(0.244880174291939, 0.7843137254901961, 1, alpha);
        float a = (x - 13.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 15.0 * dx) {                                                                                                                
        vec4 x1 = vec4(0.244880174291939, 0.7843137254901961, 1, alpha);
        vec4 x2 = vec4(0.2797385620915033, 0.8123249299719888, 1, alpha);
        float a = (x - 14.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 16.0 * dx) {
        vec4 x1 = vec4(0.2797385620915033, 0.8123249299719888, 1, alpha);
        vec4 x2 = vec4(0.3145969498910676, 0.8403361344537815, 1, alpha);
        float a = (x - 15.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 17.0 * dx) {        
        vec4 x1 = vec4(0.3145969498910676, 0.8403361344537815, 1, alpha);
        vec4 x2 = vec4(0.3494553376906318, 0.8683473389355743, 1, alpha);
        float a = (x - 16.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 18.0 * dx) {                
        vec4 x1 = vec4(0.3494553376906318, 0.8683473389355743, 1, alpha);
        vec4 x2 = vec4(0.3843137254901962, 0.896358543417367, 1, alpha);
        float a = (x - 17.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 19.0 * dx) {        
        vec4 x1 = vec4(0.3843137254901962, 0.896358543417367, 1, alpha);
        vec4 x2 = vec4(0.4176781823840648, 0.9216308745720511, 0.9949579831932772, alpha);
        float a = (x - 18.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 20.0 * dx) {                
        vec4 x1 = vec4(0.4176781823840648, 0.9216308745720511, 0.9949579831932772, alpha);
        vec4 x2 = vec4(0.4375972611266729, 0.9222533457827575, 0.9445378151260503, alpha);
        float a = (x - 19.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 21.0 * dx) {                        
        vec4 x1 = vec4(0.4375972611266729, 0.9222533457827575, 0.9445378151260503, alpha);
        vec4 x2 = vec4(0.4575163398692811, 0.9228758169934641, 0.8941176470588236, alpha);
        float a = (x - 20.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 22.0 * dx) {                                
        vec4 x1 = vec4(0.4575163398692811, 0.9228758169934641, 0.8941176470588236, alpha);
        vec4 x2 = vec4(0.4774354186118892, 0.9234982882041706, 0.8436974789915966, alpha);
        float a = (x - 21.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 23.0 * dx) {                                        
        vec4 x1 = vec4(0.4774354186118892, 0.9234982882041706, 0.8436974789915966, alpha);
        vec4 x2 = vec4(0.4973544973544974, 0.924120759414877, 0.7932773109243697, alpha);
        float a = (x - 22.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 24.0 * dx) {                                                
        vec4 x1 = vec4(0.4973544973544974, 0.924120759414877, 0.7932773109243697, alpha);
        vec4 x2 = vec4(0.5172735760971056, 0.9247432306255835, 0.7428571428571428, alpha);
        float a = (x - 23.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 25.0 * dx) {                                                        
        vec4 x1 = vec4(0.5172735760971056, 0.9247432306255835, 0.7428571428571428, alpha);
        vec4 x2 = vec4(0.5371926548397137, 0.9253657018362901, 0.6924369747899159, alpha);
        float a = (x - 24.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 26.0 * dx) {                                                                
        vec4 x1 = vec4(0.5371926548397137, 0.9253657018362901, 0.6924369747899159, alpha);
        vec4 x2 = vec4(0.5745409274821039, 0.9349517584811702, 0.6763772175536882, alpha);
        float a = (x - 25.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 27.0 * dx) {                                                                        
        vec4 x1 = vec4(0.5745409274821039, 0.9349517584811702, 0.6763772175536882, alpha);
        vec4 x2 = vec4(0.6162464985994398, 0.9467787114845938, 0.6689075630252102, alpha);
        float a = (x - 26.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 28.0 * dx) {                                                                                
        vec4 x1 = vec4(0.6162464985994398, 0.9467787114845938, 0.6689075630252102, alpha);
        vec4 x2 = vec4(0.6579520697167756, 0.9586056644880174, 0.661437908496732, alpha);
        float a = (x - 27.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 29.0 * dx) {                                                                                    
        vec4 x1 = vec4(0.6579520697167756, 0.9586056644880174, 0.661437908496732, alpha);
        vec4 x2 = vec4(0.6996576408341115, 0.9704326174914411, 0.653968253968254, alpha);
        float a = (x - 28.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 30.0 * dx) {                                                                                            
        vec4 x1 = vec4(0.6996576408341115, 0.9704326174914411, 0.653968253968254, alpha);
        vec4 x2 = vec4(0.7413632119514472, 0.9822595704948647, 0.6464985994397759, alpha);
        float a = (x - 29.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 31.0 * dx) {                                                                                                    
        vec4 x1 = vec4(0.7413632119514472, 0.9822595704948647, 0.6464985994397759, alpha);
        vec4 x2 = vec4(0.7830687830687831, 0.9940865234982882, 0.6390289449112979, alpha);
        float a = (x - 30.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 32.0 * dx) {                                                                                                            
        vec4 x1 = vec4(0.7830687830687831, 0.9940865234982882, 0.6390289449112979, alpha);
        vec4 x2 = vec4(0.8148148148148148, 0.9940865234982882, 0.6225334578275755, alpha);
        float a = (x - 31.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 33.0 * dx) {
        vec4 x1 = vec4(0.8148148148148148, 0.9940865234982882, 0.6225334578275755, alpha);
        vec4 x2 = vec4(0.8366013071895425, 0.9822595704948647, 0.5970121381886088, alpha);
        float a = (x - 32.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 34.0 * dx) {        
        vec4 x1 = vec4(0.8366013071895425, 0.9822595704948647, 0.5970121381886088, alpha);
        vec4 x2 = vec4(0.8583877995642701, 0.9704326174914411, 0.5714908185496421, alpha);
        float a = (x - 33.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 35.0 * dx) {                
        vec4 x1 = vec4(0.8583877995642701, 0.9704326174914411, 0.5714908185496421, alpha);
        vec4 x2 = vec4(0.8801742919389979, 0.9586056644880173, 0.5459694989106754, alpha);
        float a = (x - 34.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 36.0 * dx) {        
        vec4 x1 = vec4(0.8801742919389979, 0.9586056644880173, 0.5459694989106754, alpha);
        vec4 x2 = vec4(0.9019607843137255, 0.9467787114845938, 0.5204481792717086, alpha);
        float a = (x - 35.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 37.0 * dx) {        
        vec4 x1 = vec4(0.9019607843137255, 0.9467787114845938, 0.5204481792717086, alpha);
        vec4 x2 = vec4(0.9237472766884532, 0.9349517584811702, 0.4949268596327419, alpha);
        float a = (x - 36.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 38.0 * dx) {
        vec4 x1 = vec4(0.9237472766884532, 0.9349517584811702, 0.4949268596327419, alpha);
        vec4 x2 = vec4(0.9430438842203548, 0.9196389666977901, 0.4702769996887644, alpha);
        float a = (x - 37.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 39.0 * dx) {
        vec4 x1 = vec4(0.9430438842203548, 0.9196389666977901, 0.4702769996887644, alpha);
        vec4 x2 = vec4(0.9523809523809524, 0.8903828197945844, 0.4491129785247432, alpha);
        float a = (x - 38.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 40.0 * dx) {
        vec4 x1 = vec4(0.9523809523809524, 0.8903828197945844, 0.4491129785247432, alpha);
        vec4 x2 = vec4(0.96171802054155, 0.8611266728913787, 0.4279489573607221, alpha);
        float a = (x - 39.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 41.0 * dx) {        
        vec4 x1 = vec4(0.96171802054155, 0.8611266728913787, 0.4279489573607221, alpha);
        vec4 x2 = vec4(0.9710550887021475, 0.831870525988173, 0.4067849361967009, alpha);
        float a = (x - 40.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 42.0 * dx) {        
        vec4 x1 = vec4(0.9710550887021475, 0.831870525988173, 0.4067849361967009, alpha);
        vec4 x2 = vec4(0.9803921568627451, 0.8026143790849672, 0.3856209150326798, alpha);
        float a = (x - 41.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 43.0 * dx) {        
        vec4 x1 = vec4(0.9803921568627451, 0.8026143790849672, 0.3856209150326798, alpha);
        vec4 x2 = vec4(0.9897292250233427, 0.7733582321817616, 0.3644568938686586, alpha);
        float a = (x - 42.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 44.0 * dx) {        
        vec4 x1 = vec4(0.9897292250233427, 0.7733582321817616, 0.3644568938686586, alpha);
        vec4 x2 = vec4(0.9990662931839402, 0.7441020852785559, 0.3432928727046375, alpha);
        float a = (x - 43.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 45.0 * dx) {        
        vec4 x1 = vec4(0.9990662931839402, 0.7441020852785559, 0.3432928727046375, alpha);
        vec4 x2 = vec4(1, 0.7254901960784313, 0.3305322128851541, alpha);
        float a = (x - 44.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 46.0 * dx) {        
        vec4 x1 = vec4(1, 0.7254901960784313, 0.3305322128851541, alpha);
        vec4 x2 = vec4(1, 0.7080610021786493, 0.3187052598817304, alpha);
        float a = (x - 45.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 47.0 * dx) {        
        vec4 x1 = vec4(1, 0.7080610021786493, 0.3187052598817304, alpha);
        vec4 x2 = vec4(1, 0.6906318082788672, 0.3068783068783069, alpha);
        float a = (x - 46.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 48.0 * dx) {        
        vec4 x1 = vec4(1, 0.6906318082788672, 0.3068783068783069, alpha);
        vec4 x2 = vec4(1, 0.6732026143790849, 0.2950513538748833, alpha);
        float a = (x - 47.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 49.0 * dx) {        
        vec4 x1 = vec4(1, 0.6732026143790849, 0.2950513538748833, alpha);
        vec4 x2 = vec4(1, 0.6557734204793029, 0.2832244008714597, alpha);
        float a = (x - 48.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 50.0 * dx) {        
        vec4 x1 = vec4(1, 0.6557734204793029, 0.2832244008714597, alpha);
        vec4 x2 = vec4(1, 0.6383442265795207, 0.2713974478680361, alpha);
        float a = (x - 49.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 51.0 * dx) {        
        vec4 x1 = vec4(1, 0.6383442265795207, 0.2713974478680361, alpha);
        vec4 x2 = vec4(1, 0.6407096171802054, 0.2909430438842204, alpha);
        float a = (x - 50.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 52.0 * dx) {        
        vec4 x1 = vec4(1, 0.6407096171802054, 0.2909430438842204, alpha);
        vec4 x2 = vec4(1, 0.6562713974478681, 0.3314036725801432, alpha);
        float a = (x - 51.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 53.0 * dx) {        
        vec4 x1 = vec4(1, 0.6562713974478681, 0.3314036725801432, alpha);
        vec4 x2 = vec4(1, 0.6718331777155307, 0.3718643012760661, alpha);
        float a = (x - 52.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 54.0 * dx) {        
        vec4 x1 = vec4(1, 0.6718331777155307, 0.3718643012760661, alpha);
        vec4 x2 = vec4(1, 0.6873949579831934, 0.4123249299719889, alpha);
        float a = (x - 53.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 55.0 * dx) {        
        vec4 x1 = vec4(1, 0.6873949579831934, 0.4123249299719889, alpha);
        vec4 x2 = vec4(1, 0.7029567382508559, 0.4527855586679118, alpha);
        float a = (x - 54.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 56.0 * dx) {        
        vec4 x1 = vec4(1, 0.7029567382508559, 0.4527855586679118, alpha);
        vec4 x2 = vec4(1, 0.7185185185185186, 0.4932461873638346, alpha);
        float a = (x - 55.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 57.0 * dx) {        
        vec4 x1 = vec4(1, 0.7185185185185186, 0.4932461873638346, alpha);
        vec4 x2 = vec4(1, 0.7422969187675071, 0.5443510737628388, alpha);
        float a = (x - 56.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 58.0 * dx) {        
        vec4 x1 = vec4(1, 0.7422969187675071, 0.5443510737628388, alpha);
        vec4 x2 = vec4(1, 0.7852474323062559, 0.6202925614690323, alpha);
        float a = (x - 57.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 59.0 * dx) {        
        vec4 x1 = vec4(1, 0.7852474323062559, 0.6202925614690323, alpha);
        vec4 x2 = vec4(1, 0.8281979458450048, 0.6962340491752258, alpha);
        float a = (x - 58.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 60.0 * dx) {        
        vec4 x1 = vec4(1, 0.8281979458450048, 0.6962340491752258, alpha);
        vec4 x2 = vec4(1, 0.8711484593837536, 0.7721755368814194, alpha);
        float a = (x - 59.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 61.0 * dx) {        
        vec4 x1 = vec4(1, 0.8711484593837536, 0.7721755368814194, alpha);
        vec4 x2 = vec4(1, 0.9140989729225023, 0.8481170245876131, alpha);
        float a = (x - 60.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x < 62.0 * dx) {        
        vec4 x1 = vec4(1, 0.9140989729225023, 0.8481170245876131, alpha);
        vec4 x2 = vec4(1, 0.9570494864612512, 0.9240585122938065, alpha);
        float a = (x - 61.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    if( x <= 63.0 * dx) {        
        vec4 x1 = vec4(1, 0.9570494864612512, 0.9240585122938065, alpha);
        vec4 x2 = vec4(1, 1, 1, alpha);
        float a = (x - 62.0 * dx) / dx;
        return mix(x1, x2, a);
    }

    // the default (unimplemented colour)
    return vec4(0,0,0,0);
}

void main() {
    vec4 colour = texture2D(u_texture, v_texcoord.xy);// the raw floating-point colour

    // clip the coordinates
    if(v_texcoord.x < 0.0 || v_texcoord.x > 1.0 || v_texcoord.y < 0.0 || v_texcoord.y > 1.0)
        colour = vec4(0.0, 0.0, 0.0, 0.0);

    float x = colour.r;