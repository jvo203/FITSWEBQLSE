function get_js_version() {
    return "JS2025-01-20.0";
}

function uuidv4() {
    return ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, c =>
        (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16))
}

function get_worker_script() {
    return `self.addEventListener('message', function (e) {
        importScripts('https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isobands.min.js');
        importScripts('https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/marchingsquares-isocontours.min.js');
        var band = MarchingSquaresJS.isoBands(e.data.data, e.data.lowerBand, e.data.upperBand - e.data.lowerBand);
        self.postMessage(band);
        self.close();
    }, false);`
}

const wasm_supported = (() => {
    try {
        console.log("checking for WebAssembly support");
        if (typeof WebAssembly === "object"
            && typeof WebAssembly.instantiate === "function") {
            const module = new WebAssembly.Module(Uint8Array.of(0x0, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00));
            if (module instanceof WebAssembly.Module)
                return new WebAssembly.Instance(module) instanceof WebAssembly.Instance;
        }
    } catch (e) {
    }
    return false;
})();

console.log(wasm_supported ? "WebAssembly is supported" : "WebAssembly is not supported");

const limit = 100; // Atomic Spectra Database lines number display limit

String.prototype.insert_at = function (index, string) {
    return this.substr(0, index) + string + this.substr(index);
}

Array.prototype.rotate = function (n) {
    return this.slice(n, this.length).concat(this.slice(0, n));
}

function clamp(value, min, max) {
    return Math.min(Math.max(min, value), max)
}

function string2buffer(str) {
    var buffer = new ArrayBuffer(str.length);
    var view = new Uint8Array(buffer);
    for (var i = 0; i < str.length; i += 1) {
        view[i] = str.charCodeAt(i);
    }

    return buffer;
}

function pix2sky(wcs, x, y) {
    // call pix2lonlat_func
    /*const { lon, lat } = pix2lonlat_func(wcs.index, x + 0.5, y + 0.5);
    console.log("lon:", lon, "lat:", lat);*/

    // wcslib uses 1-indexing for pixel coordinates
    var world = Module.pix2sky(wcs.index, x + 0.5, y + 0.5);
    // console.log("world:", world);

    return [world[0], world[1]];
}

function sky2pix(wcs, ra, dec) {
    /*const { x, y } = lonlat2pix_func(wcs.index, ra, dec);
    console.log("x:", x, "y:", y);*/

    var pixcrd = Module.sky2pix(wcs.index, ra, dec);
    // console.log("pixcrd:", pixcrd);

    // wcslib uses 1-indexing for pixel coordinates
    return [pixcrd[0] - 0.5, pixcrd[1] - 0.5];
}

function round(value, precision, mode) {
    //  discuss at: http://locutus.io/php/round/
    // original by: Philip Peterson
    //  revised by: Onno Marsman (https://twitter.com/onnomarsman)
    //  revised by: T.Wild
    //  revised by: Rafał Kukawski (http://blog.kukawski.pl)
    //    input by: Greenseed
    //    input by: meo
    //    input by: William
    //    input by: Josep Sanz (http://www.ws3.es/)
    // bugfixed by: Brett Zamir (http://brett-zamir.me)
    //      note 1: Great work. Ideas for improvement:
    //      note 1: - code more compliant with developer guidelines
    //      note 1: - for implementing PHP constant arguments look at
    //      note 1: the pathinfo() function, it offers the greatest
    //      note 1: flexibility & compatibility possible
    //   example 1: round(1241757, -3)
    //   returns 1: 1242000
    //   example 2: round(3.6)
    //   returns 2: 4
    //   example 3: round(2.835, 2)
    //   returns 3: 2.84
    //   example 4: round(1.1749999999999, 2)
    //   returns 4: 1.17
    //   example 5: round(58551.799999999996, 2)
    //   returns 5: 58551.8

    var m, f, isHalf, sgn // helper variables
    // making sure precision is integer
    precision |= 0
    m = Math.pow(10, precision)
    value *= m
    // sign of the number
    sgn = (value > 0) | -(value < 0)
    isHalf = value % 1 === 0.5 * sgn
    f = Math.floor(value)

    if (isHalf) {
        switch (mode) {
            case 'PHP_ROUND_HALF_DOWN':
                // rounds .5 toward zero
                value = f + (sgn < 0)
                break
            case 'PHP_ROUND_HALF_EVEN':
                // rouds .5 towards the next even integer
                value = f + (f % 2 * sgn)
                break
            case 'PHP_ROUND_HALF_ODD':
                // rounds .5 towards the next odd integer
                value = f + !(f % 2)
                break
            default:
                // rounds .5 away from zero
                value = f + (sgn > 0)
        }
    }

    return (isHalf ? value : Math.round(value)) / m
}

// https://stackoverflow.com/questions/3407012/rounding-up-to-the-nearest-multiple-of-a-number
// works for positive numbers only
function roundUp(numToRound, multiple) {
    if (multiple == 0) {
        return numToRound;
    }

    let remainder = numToRound % multiple;

    if (remainder == 0) {
        return numToRound;
    }

    return numToRound + multiple - remainder;
}

function getUint64(dataview, byteOffset, littleEndian) {
    // 64ビット数を2つの32ビット (4バイト) の部分に分割する
    const left = dataview.getUint32(byteOffset, littleEndian);
    const right = dataview.getUint32(byteOffset + 4, littleEndian);

    // 2つの32ビットの値を結合する
    const combined = littleEndian ? left + 2 ** 32 * right : 2 ** 32 * left + right;

    if (!Number.isSafeInteger(combined))
        console.warn(combined, 'exceeds MAX_SAFE_INTEGER. Precision may be lost');

    return combined;
}

var colours = ["red", "green", "lightblue"];
var linedash = [[], [10, 5], [5, 5, 2, 2]];

function get_axes_range(width, height) {
    var xMin = /*0.005*width ;*/0.025 * width;
    var xMax = width - xMin - 1;

    var yMin = 0.05 * height;
    var yMax = height - yMin - 1;

    var range = {
        xMin: Math.round(xMin),
        xMax: Math.round(xMax),
        yMin: Math.round(yMin),
        yMax: Math.round(yMax)
    };

    return range;
}

function get_screen_scale(x) {
    //return Math.floor(0.925*x) ;
    return Math.floor(0.9 * x);
}

function get_image_scale_square(width, height, img_width, img_height) {
    var screen_dimension = get_screen_scale(Math.min(width, height));
    var image_dimension = Math.max(img_width, img_height);

    return screen_dimension / image_dimension;
}

function get_image_scale(width, height, img_width, img_height) {
    if (img_width == img_height)
        return get_image_scale_square(width, height, img_width, img_height);

    if (img_height < img_width) {
        var screen_dimension = 0.9 * height;
        var image_dimension = img_height;

        var scale = screen_dimension / image_dimension;

        var new_image_width = scale * img_width;

        if (new_image_width > 0.8 * width) {
            screen_dimension = 0.8 * width;
            image_dimension = img_width;
            scale = screen_dimension / image_dimension;
        }

        return scale;
    }

    if (img_width < img_height) {
        var screen_dimension = 0.8 * width;
        var image_dimension = img_width;

        var scale = screen_dimension / image_dimension;

        var new_image_height = scale * img_height;

        if (new_image_height > 0.9 * height) {
            screen_dimension = 0.9 * height;
            image_dimension = img_height;
            scale = screen_dimension / image_dimension;
        }

        return scale;
    }
}

function get_pv_screen_scale(x) {
    return Math.floor(0.95 * x);
}

function get_pv_image_scale_square(width, height, img_width, img_height) {
    var screen_dimension = get_pv_screen_scale(Math.min(width, height));
    var image_dimension = Math.max(img_width, img_height);

    return screen_dimension / image_dimension;
}

function get_pv_image_scale(width, height, img_width, img_height) {
    if (img_width == img_height)
        return get_pv_image_scale_square(width, height, img_width, img_height);

    if (img_height < img_width) {
        var screen_dimension = 0.95 * height;
        var image_dimension = img_height;

        var scale = screen_dimension / image_dimension;

        var new_image_width = scale * img_width;

        if (new_image_width > 0.95 * width) {
            screen_dimension = 0.95 * width;
            image_dimension = img_width;
            scale = screen_dimension / image_dimension;
        }

        return scale;
    }

    if (img_width < img_height) {
        var screen_dimension = 0.95 * width;
        var image_dimension = img_width;

        var scale = screen_dimension / image_dimension;

        var new_image_height = scale * img_height;

        if (new_image_height > 0.95 * height) {
            screen_dimension = 0.95 * height;
            image_dimension = img_height;
            scale = screen_dimension / image_dimension;
        }

        return scale;
    }
}

function get_spectrum_direction(fitsData) {
    var reverse = false;

    if (has_velocity_info) {
        var has_frequency = false;

        if (va_count <= 1) {
            if (RESTFRQ > 0.0)
                has_frequency = true;
        };

        if (!has_frequency) {
            if (fitsData.CDELT3 > 0.0)
                reverse = false;
            else
                reverse = true;
        }
        else {
            if (fitsData.CDELT3 < 0.0)
                reverse = false;
            else
                reverse = true;
        }
    }
    else {
        //ALMAWebQLv2 behaviour
        if (fitsData.CDELT3 > 0.0)
            reverse = false;
        else
            reverse = true;
    };

    return reverse;
}

function get_freq2vel_bounds(freq_start, freq_end, fitsData) {
    if (fitsData.RESTFRQ <= 0.0 && RESTFRQ <= 0.0)
        return { frame_start: 0, frame_end: fitsData.depth - 1 };

    let c = 299792458;//speed of light [m/s]

    var fRatio, v1, v2, x1, x2;

    fRatio = freq_start / RESTFRQ;
    v1 = (1.0 - fRatio * fRatio) / (1.0 + fRatio * fRatio) * c;

    fRatio = freq_end / RESTFRQ;
    v2 = (1.0 - fRatio * fRatio) / (1.0 + fRatio * fRatio) * c;

    x1 = fitsData.CRPIX3 + (v1 - fitsData.CRVAL3) / fitsData.CDELT3 - 1.0;
    x2 = fitsData.CRPIX3 + (v2 - fitsData.CRVAL3) / fitsData.CDELT3 - 1.0;

    var _frame_start = Math.round(x1);
    var _frame_end = Math.round(x2);

    if (_frame_end < _frame_start) {
        let tmp = _frame_start;
        _frame_start = _frame_end;
        _frame_end = tmp;
    };

    _frame_start = Math.max(_frame_start, 0);
    _frame_start = Math.min(_frame_start, fitsData.depth - 1);

    _frame_end = Math.max(_frame_end, 0);
    _frame_end = Math.min(_frame_end, fitsData.depth - 1);

    return { frame_start: _frame_start, frame_end: _frame_end };
}

function get_frequency_bounds(freq_start, freq_end, fitsData) {
    //console.log("get_frequency_bounds(" + freq_start + "," + freq_end + ")");

    var f1, f2, band_lo, band_hi;

    f1 = fitsData.CRVAL3 + fitsData.CDELT3 * (1.0 - fitsData.CRPIX3);
    f2 = fitsData.CRVAL3 + fitsData.CDELT3 * (fitsData.depth - fitsData.CRPIX3);

    band_lo = Math.min(f1, f2);
    band_hi = Math.max(f1, f2);

    //console.log("band_lo:", band_lo, "band_hi:", band_hi);

    var _frame_start, _frame_end;

    if (fitsData.CDELT3 > 0.0) {
        _frame_start =
            Math.round((freq_start - band_lo) / (band_hi - band_lo) * (fitsData.depth - 1));
        _frame_end =
            Math.round((freq_end - band_lo) / (band_hi - band_lo) * (fitsData.depth - 1));
    }
    else {
        _frame_start =
            Math.round((band_hi - freq_start) / (band_hi - band_lo) * (fitsData.depth - 1));
        _frame_end =
            Math.round((band_hi - freq_end) / (band_hi - band_lo) * (fitsData.depth - 1));
    }

    if (_frame_end < _frame_start) {
        let tmp = _frame_start;
        _frame_start = _frame_end;
        _frame_end = tmp;
    };

    _frame_start = Math.max(_frame_start, 0);
    _frame_start = Math.min(_frame_start, fitsData.depth - 1);

    _frame_end = Math.max(_frame_end, 0);
    _frame_end = Math.min(_frame_end, fitsData.depth - 1);

    return { frame_start: _frame_start, frame_end: _frame_end };
}

function get_velocity_bounds(vel_start, vel_end, fitsData) {
    //console.log("get_velocity_bounds(" + vel_start + "," + vel_end + ")");

    var v1, v2, vel_lo, vel_hi;

    v1 = fitsData.CRVAL3 + fitsData.CDELT3 * (1.0 - fitsData.CRPIX3);
    v2 = fitsData.CRVAL3 + fitsData.CDELT3 * (fitsData.depth - fitsData.CRPIX3);

    vel_lo = Math.min(v1, v2);
    vel_hi = Math.max(v1, v2);

    //console.log("vel_lo:", vel_lo, "vel_hi:", vel_hi);

    var _frame_start, _frame_end;

    if (fitsData.CDELT3 > 0.0) {
        _frame_start = Math.round((vel_start - vel_lo) / (vel_hi - vel_lo) * (fitsData.depth - 1));
        _frame_end = Math.round((vel_end - vel_lo) / (vel_hi - vel_lo) * (fitsData.depth - 1));
    }
    else {
        _frame_start = Math.round((vel_hi - vel_start) / (vel_hi - vel_lo) * (fitsData.depth - 1));
        _frame_end = Math.round((vel_hi - vel_end) / (vel_hi - vel_lo) * (fitsData.depth - 1));
    };

    if (_frame_end < _frame_start) {
        let tmp = _frame_start;
        _frame_start = _frame_end;
        _frame_end = tmp;
    };

    _frame_start = Math.max(_frame_start, 0);
    _frame_start = Math.min(_frame_start, fitsData.depth - 1);

    _frame_end = Math.max(_frame_end, 0);
    _frame_end = Math.min(_frame_end, fitsData.depth - 1);

    return { frame_start: _frame_start, frame_end: _frame_end };
}

function get_frame_bounds(lo, hi, index) {
    //ref_freq = RESTFRQ
    let fitsData = fitsContainer[index];

    if (fitsData == null)
        return { frame_start: 0, frame_end: 0 };

    if (fitsData.depth <= 1)
        return { frame_start: 0, frame_end: 0 };

    if (has_velocity_info && RESTFRQ > 0.0)
        return get_freq2vel_bounds(lo, hi, fitsData);

    if (has_frequency_info)
        return get_frequency_bounds(lo, hi, fitsData);

    if (has_velocity_info)
        return get_velocity_bounds(lo, hi, fitsData);
}

function spectrum_binning(data, factor) {
    if (factor <= 1)
        return data;

    var len = data.length;
    var new_len = Math.floor(len / factor);

    if (new_len <= 1)
        return data;

    var new_data = new Array(new_len);

    for (var i = 0; i < new_len; i++) {
        var sum = 0.0;

        for (var j = 0; j < factor; j++) {
            let idx = i * factor + j;
            sum += data[idx];

            if (idx >= len)
                break;

        };

        new_data[i] = sum / factor;
    };

    return new_data;
}

function largestTriangleThreeBuckets(data, threshold) {

    var floor = Math.floor,
        abs = Math.abs;

    var dataLength = data.length;
    if (threshold >= dataLength || threshold === 0) {
        return data; // Nothing to do
    }

    //console.log("applying 'largestTriangleThreeBuckets'");

    var sampled = [],
        sampledIndex = 0;

    // Bucket size. Leave room for start and end data points
    var every = (dataLength - 2) / (threshold - 2);

    var a = 0,  // Initially a is the first point in the triangle
        maxAreaPoint,
        maxArea,
        area,
        nextA;

    sampled[sampledIndex++] = data[a]; // Always add the first point

    for (var i = 0; i < threshold - 2; i++) {

        // Calculate point average for next bucket (containing c)
        var avgX = 0,
            avgY = 0,
            avgRangeStart = floor((i + 1) * every) + 1,
            avgRangeEnd = floor((i + 2) * every) + 1;
        avgRangeEnd = avgRangeEnd < dataLength ? avgRangeEnd : dataLength;

        var avgRangeLength = avgRangeEnd - avgRangeStart;

        for (; avgRangeStart < avgRangeEnd; avgRangeStart++) {
            avgX += avgRangeStart;//data[ avgRangeStart ][ xAccessor ] * 1; // * 1 enforces Number (value may be Date)
            avgY += data[avgRangeStart];
        }
        avgX /= avgRangeLength;
        avgY /= avgRangeLength;

        // Get the range for this bucket
        var rangeOffs = floor((i + 0) * every) + 1,
            rangeTo = floor((i + 1) * every) + 1;

        // Point a
        var pointAX = a,//data[ a ][ xAccessor ] * 1, // enforce Number (value may be Date)
            pointAY = data[a];

        maxArea = area = -1;

        for (; rangeOffs < rangeTo; rangeOffs++) {
            // Calculate triangle area over three buckets
            area = abs((pointAX - avgX) * (data[rangeOffs] - pointAY) -
                (pointAX - rangeOffs) * (avgY - pointAY)
            ) * 0.5;
            if (area > maxArea) {
                maxArea = area;
                maxAreaPoint = data[rangeOffs];
                nextA = rangeOffs; // Next a is this b
            }
        }

        sampled[sampledIndex++] = maxAreaPoint; // Pick this point from the bucket
        a = nextA; // This a is the next a (chosen b)
    }

    sampled[sampledIndex++] = data[dataLength - 1]; // Always add last

    return sampled;
}

function getShadowStyle() {
    if (!composite_view) {
        if (theme == 'bright')
            return "black";// purple
        else
            //return "yellow";//was red
            return "rgba(255,204,0,1.0)"; // Amber
    }
    else {
        //return "yellow";
        return "rgba(255,204,0,1.0)"; // Amber
        //return "white";
    };
}

function getStrokeStyle() {
    var style = "rgba(0,0,0,1.0)";

    //style = "rgba(255,204,0,0.9)" ;//yellowish ALMAWebQL v2
    style = "rgba(255,255,255,1.0)";//white
    //style = "rgba(153, 102, 153, 0.9)" ;//violet

    if (theme == 'bright') {
        //style = "rgba(0,0,0,1.0)";//black
        style = "rgba(127,127,127,1.0)";// grey

        if (colourmap == "greyscale")
            style = "rgba(255,204,0,1.0)";//yellowish ALMAWebQL v2
    }


    if (theme == 'dark') {
        if (colourmap == "green")
            //style = "rgba(255,127,80,0.9)";//orange
            //style = "rgba(238,130,238,0.9)" ;
            //style = "rgba(204,204,204,0.9)";//grey
            style = "rgba(255,204,0,1.0)";//yellowish ALMAWebQL v2
        //style = "rgba(204,204,204,1.0)";//grey

        if (colourmap == "red")
            style = "rgba(0,191,255,1.0)";//deepskyblue

        if (colourmap == "blue")
            style = "rgba(255,215,0,1.0)";//gold

        if (colourmap == "hot")
            style = "rgba(0,191,255,1.0)";//deepskyblue

        //if(document.getElementById('colourmap').value == "rainbow")// || document.getElementById('colourmap').value == "parula" || document.getElementById('colourmap').value == "viridis")
        //	style = "rgba(204,204,204,0.9)" ;
    }

    return style;
}

// a rotation transform of (px, py) by a theta angle around the point (alpha, beta)
function rotate_point(px, py, alpha, beta, theta) {
    const qx = Math.cos(theta) * (px - alpha) - Math.sin(theta) * (py - beta) + alpha;
    const qy = Math.sin(theta) * (px - alpha) + Math.cos(theta) * (py - beta) + beta;

    return { x: qx, y: qy };
}

// convert from FITS to image coordinates
function fits2image(fitsData, image, elem, orig_x, orig_y) {
    var image_bounding_dims = image.image_bounding_dims;

    const elem_width = parseFloat(elem.getAttribute("width"));
    const elem_height = parseFloat(elem.getAttribute("height"));
    const elem_x = 0;//parseFloat(elem.getAttribute("x"));
    const elem_y = 0;//parseFloat(elem.getAttribute("y"));

    var x = orig_x * (image.width - 1) / (fitsData.width - 1);
    var y = orig_y * (image.height - 1) / (fitsData.height - 1);

    var ax = (image_bounding_dims.width - 1) / (elem_width - 0);
    var ay = (image_bounding_dims.height - 1) / (elem_height - 0);

    x = elem_x + (x - image_bounding_dims.x1) / ax;
    y = elem_y + ((image_bounding_dims.y1 + image_bounding_dims.height - 1) - y) / ay;

    return { x: x, y: y };
}

function plot_hds_crosshair(x0, y0, theta) {
    // console.log("plot_hds_crosshair:", x0, y0, theta);

    if (mousedown)
        return;

    // check if theta is nan
    if (isNaN(theta))
        return;

    var fitsData = fitsContainer[va_count - 1];
    if (fitsData == null)
        return;

    var image = imageContainer[va_count - 1];
    if (image == null)
        return;

    var elem = document.getElementById("image_rectangle");
    if (elem == null)
        return;

    var img_width = parseFloat(elem.getAttribute("width"));
    var img_height = parseFloat(elem.getAttribute("height"));
    var img_x = parseFloat(elem.getAttribute("x"));
    var img_y = parseFloat(elem.getAttribute("y"));
    console.log("img_width:", img_width, "img_height:", img_height, "img_x:", img_x, "img_y:", img_y);

    var x1, x2, y1, y2;
    var aLine;

    // the spectrum X-Y starting point offsets
    var xmin = 0;
    var xmax = img_width;
    var ymin = 0;
    var ymax = img_height;

    // take the largest dimension
    var dim = Math.max(fitsData.width, fitsData.height);

    // the X-axis
    x1 = x0 - dim;
    y1 = y0;

    x2 = x0 + dim;
    y2 = y0;

    // rotate the cross-hair
    var p1 = rotate_point(x1, y1, x0, y0, -theta);
    var p2 = rotate_point(x2, y2, x0, y0, -theta);
    // console.log("#xline p1:", p1, "p2:", p2);

    // convert the rotated cross-hair to image coordinates
    p1 = fits2image(fitsData, image, elem, p1.x, p1.y);
    p2 = fits2image(fitsData, image, elem, p2.x, p2.y);

    // update the xline
    d3.select("#xline").attr("x1", p1.x).attr("y1", p1.y).attr("x2", p2.x).attr("y2", p2.y).attr("opacity", 1.0);
    aLine = Line.create([p1.x, p1.y], [p2.x - p1.x, p2.y - p1.y]);

    let edgeT = Line.create([0, 0], [img_width, 0]);
    let edgeB = Line.create([0, img_height], [img_width, 0]);

    let intersectT = aLine.intersectionWith(edgeT);
    let intersectB = aLine.intersectionWith(edgeB);

    if (intersectT != null && intersectB != null) {
        console.log("intersections:", intersectT.elements, intersectB.elements);

        // calculate the spectrum horizontal boundaries
        // vector elements begin at 1

        if (theta < 0.0) {
            xmin = Math.max(0, intersectB.e(1));
            xmax = Math.min(img_width, intersectT.e(1));
        } else {
            xmin = Math.max(0, intersectT.e(1));
            xmax = Math.min(img_width, intersectB.e(1));
        }

        console.log("xmin:", xmin, "xmax:", xmax);
    }

    // the Y-axis
    x1 = x0;
    y1 = y0 - dim;

    x2 = x0;
    y2 = y0 + dim;

    // rotate the cross-hair
    p1 = rotate_point(x1, y1, x0, y0, -theta);
    p2 = rotate_point(x2, y2, x0, y0, -theta);
    // console.log("#yline p1:", p1, "p2:", p2);

    // convert the rotated cross-hair to image coordinates
    p1 = fits2image(fitsData, image, elem, p1.x, p1.y);
    p2 = fits2image(fitsData, image, elem, p2.x, p2.y);
    console.log("p1:", p1, "p2:", p2);

    // update the yline
    d3.select("#yline").attr("x1", p1.x).attr("y1", p1.y).attr("x2", p2.x).attr("y2", p2.y).attr("opacity", 1.0);
    aLine = Line.create([p1.x, p1.y], [p2.x - p1.x, p2.y - p1.y]);

    let edgeL = Line.create([0, 0], [0, img_height]);
    let edgeR = Line.create([img_width, 0], [0, img_height]);

    let intersectL = aLine.intersectionWith(edgeL);
    let intersectR = aLine.intersectionWith(edgeR);

    if (intersectL != null && intersectR != null) {
        console.log("intersections:", intersectL.elements, intersectR.elements);

        // calculate the spectrum vertical boundaries
        // vector elements begin at 1

        if (theta < 0.0) {
            ymin = Math.max(0, intersectL.e(2));
            ymax = Math.min(img_height, intersectR.e(2));
        } else {
            ymin = Math.max(0, intersectR.e(2));
            ymax = Math.min(img_height, intersectL.e(2));
        }

        console.log("ymin:", ymin, "ymax:", ymax);
    }

    // set the HDS svg opacity
    d3.select("#hds_svg").attr("opacity", 1);

    // returns the spectrum horizontal & vertical boundaries
    return { xmin: xmin, xmax: xmax, ymin: ymin, ymax: ymax };
}

function plot_hds_spectrum(data, mask, bmin, bmax, index) {
    // console.log("plot_hds_spectrum:", data, mask, index);

    if (mousedown)
        return;

    let len = data.length;

    if (len < 2) // at least two points are needed to plot a line
        return;

    var elem = document.getElementById("SpectrumCanvas");
    if (displaySpectrum) {
        elem.style.display = "block";
        d3.select("#yaxis").attr("opacity", 1);
        d3.select("#ylabel").attr("opacity", 1);
    }
    else {
        elem.style.display = "none";
        d3.select("#yaxis").attr("opacity", 0);
        d3.select("#ylabel").attr("opacity", 0);
    }

    var canvas = document.getElementById("SpectrumCanvas");
    var ctx = canvas.getContext('2d');

    var width = canvas.width;
    var height = canvas.height;

    let dmin = 0.0;
    let dmax = d3.max(data);

    var elem = document.getElementById("image_rectangle");
    if (elem == null)
        return;

    var img_width = parseFloat(elem.getAttribute("width"));
    var img_height = parseFloat(elem.getAttribute("height"));
    var img_x = parseFloat(elem.getAttribute("x"));
    var img_y = parseFloat(elem.getAttribute("y"));

    var dx = bmax - bmin;//img_width;
    var dy = bmax - bmin;//img_height;

    var range = get_axes_range(width, height);
    //var dx = range.xMax - range.xMin;
    //var dy = range.yMax - range.yMin;

    var chart_height = (height - 1 - range.yMax - 1);

    // the X-axis
    if (index == 0) {
        var incrx = dx / (len - 1);
        var offset = img_x + bmin;//range.xMin;
    } else {
        var incrx = dy / (len - 1);
        var offset = img_y + bmin + dy;//range.yMax;
    }

    var offsetx = 0;
    var is_nan = false;

    // find the first valid data point
    while (mask[offsetx] == 0 || data[offsetx] > dmax) {
        if (index == 0) {
            offset += incrx;
        } else {
            offset -= incrx;
        }

        offsetx++;

        if (offsetx >= len)
            break;
    }

    let datum = data[offsetx];
    var y = (datum - dmin) / (dmax - dmin) * chart_height;

    ctx.save();
    ctx.beginPath();

    if (index == 0) {
        ctx.moveTo(offset, range.yMax + chart_height - y);
    } else {
        ctx.moveTo(img_x + img_width + emStrokeWidth + chart_height - y, offset);
    }

    if (index == 0) {
        offset += incrx;
    } else {
        offset -= incrx;
    }

    for (var x = offsetx | 0; x < data.length; x = (x + 1) | 0) {
        //let datum = clamp(data[x], dmin, dmax);
        //let datum = data[x];
        let datum = (mask[x] == 0) || (data[x] > dmax) ? NaN : data[x];

        // check for NaN
        if (!isNaN(datum)) {
            y = (datum - dmin) / (dmax - dmin) * chart_height;

            if (is_nan) {
                if (index == 0) {
                    ctx.moveTo(offset, range.yMax + chart_height - y);
                } else {
                    ctx.moveTo(img_x + img_width + emStrokeWidth + chart_height - y, offset);
                }
            } else {
                if (index == 0) {
                    ctx.lineTo(offset, range.yMax + chart_height - y);
                } else {
                    ctx.lineTo(img_x + img_width + emStrokeWidth + chart_height - y, offset);
                }
            }

            is_nan = false;

        } else {
            is_nan = true;
        }

        if (index == 0) {
            offset += incrx;
        } else {
            offset -= incrx;
        }
    };

    ctx.lineWidth = 1;
    ctx.strokeWidth = emStrokeWidth;
    ctx.strokeStyle = getStrokeStyle();

    ctx.stroke();
    ctx.closePath();
    ctx.restore();

}

function plot_spectrum(dataArray) {
    /*try {
      if (d3.select("#pvline").attr("opacity") > 0.0)
        return;
    } catch (e) { };*/

    if (mousedown)
        return;

    let len = dataArray.length;

    if (len < 1)
        return;

    let fitsData = fitsContainer[len - 1];

    if (fitsData.depth <= 1 || optical_view) {
        return;
    }

    var elem = document.getElementById("SpectrumCanvas");
    if (displaySpectrum) {
        elem.style.display = "block";
        d3.select("#yaxis").attr("opacity", 1);
        d3.select("#ylabel").attr("opacity", 1);
    }
    else {
        elem.style.display = "none";
        d3.select("#yaxis").attr("opacity", 0);
        d3.select("#ylabel").attr("opacity", 0);
    }

    var canvas = document.getElementById("SpectrumCanvas");
    var ctx = canvas.getContext('2d');

    var width = canvas.width;
    var height = canvas.height;

    var dmin = 0;
    var dmax = 0;

    tmp_data_min = Number.MAX_VALUE;
    tmp_data_max = - Number.MAX_VALUE;

    for (let index = 0; index < len; index++) {
        let data = dataArray[index];
        let scale = spectrum_scale[index];

        tmp_data_min = Math.min(tmp_data_min, scale * d3.min(data));
        tmp_data_max = Math.max(tmp_data_max, scale * d3.max(data));
    }

    if (autoscale) {
        dmin = tmp_data_min;
        dmax = tmp_data_max;
    }
    else {
        if ((user_data_min != null) && (user_data_max != null)) {
            dmin = user_data_min;
            dmax = user_data_max;
        }
        else {
            dmin = data_min;
            dmax = data_max;
        }
    };

    if (windowLeft) {
        dmin = data_min;
        dmax = data_max;
    }

    if (dmin == dmax) {
        if (dmin == 0.0 && dmax == 0.0) {
            dmin = -1.0;
            dmax = 1.0;
        } else {
            if (dmin > 0.0) {
                dmin *= 0.99;
                dmax *= 1.01;
            };

            if (dmax < 0.0) {
                dmax *= 0.99;
                dmin *= 1.01;
            }
        }
    }

    var range = get_axes_range(width, height);

    var dx = range.xMax - range.xMin;
    var dy = range.yMax - range.yMin;

    var interval = dmax - dmin;
    dmin -= get_spectrum_margin() * interval;
    dmax += get_spectrum_margin() * interval;

    ctx.clearRect(0, 0, width, height);

    //iterate through all spectral lines
    for (let index = 0; index < len; index++) {
        let data = dataArray[index];
        let scale = spectrum_scale[index];

        data = largestTriangleThreeBuckets(data, dx / 2);

        // binning
        data = spectrum_binning(data, binning);

        var incrx = dx / (data.length - 1);
        var offset = range.xMin;

        //get display direction
        var reverse = get_spectrum_direction(fitsData);

        var y = 0;

        if (reverse)
            y = (scale * data[data.length - 1] - dmin) / (dmax - dmin) * dy;
        else
            y = (scale * data[0] - dmin) / (dmax - dmin) * dy;

        ctx.save();
        ctx.beginPath();

        ctx.moveTo(offset, range.yMax - y);
        offset += incrx;

        for (var x = 1 | 0; x < data.length; x = (x + 1) | 0) {
            if (reverse)
                y = (scale * data[data.length - 1 - x] - dmin) / (dmax - dmin) * dy;
            else
                y = (scale * data[x] - dmin) / (dmax - dmin) * dy;

            ctx.lineTo(offset, range.yMax - y);
            offset += incrx;
        };

        ctx.shadowColor = getShadowStyle();
        ctx.shadowBlur = 5;//20
        //ctx.shadowOffsetX = 10;
        //ctx.shadowOffsetY = 10;

        ctx.strokeStyle = getStrokeStyle();

        if (len > 1) {
            //ctx.strokeStyle = colours[index % colours.length] ;
            ctx.setLineDash(linedash[index % linedash.length]);
        }

        ctx.lineWidth = 1;// 0
        ctx.strokeWidth = emStrokeWidth;

        ctx.stroke();
        ctx.closePath();
        ctx.restore();
    }

    //plot a zero line
    if (va_count == 1)
        if (dmin <= 0 && dmax >= 0) {
            ctx.save();
            ctx.beginPath();

            ctx.shadowColor = getShadowStyle();
            ctx.shadowBlur = 20;
            //ctx.shadowOffsetX = 10;
            //ctx.shadowOffsetY = 10;
            ctx.strokeStyle = getStrokeStyle();

            //ctx.setLineDash([5, 3]);
            ctx.setLineDash([10, 10]);
            ctx.lineWidth = 1;
            ctx.strokeWidth = emStrokeWidth;

            y = (0 - dmin) / (dmax - dmin) * dy;
            ctx.moveTo(range.xMin, range.yMax - y + emStrokeWidth / 2);
            ctx.lineTo(range.xMax, range.yMax - y + emStrokeWidth / 2);

            ctx.stroke();
            ctx.closePath();
            ctx.restore();
        }
}

function replot_y_axis() {
    if (!displaySpectrum || optical_view)
        return;

    var svg = d3.select("#BackSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var dmin = 0.0;
    var dmax = 0.0;

    if (autoscale) {
        dmin = tmp_data_min;
        dmax = tmp_data_max;
    }
    else {
        if ((user_data_min != null) && (user_data_max != null)) {
            dmin = user_data_min;
            dmax = user_data_max;
        }
        else {
            dmin = data_min;
            dmax = data_max;
        }
    };

    if (windowLeft) {
        dmin = data_min;
        dmax = data_max;
    }

    if (dmin == dmax) {
        if (dmin == 0.0 && dmax == 0.0) {
            dmin = -1.0;
            dmax = 1.0;
        } else {
            if (dmin > 0.0) {
                dmin *= 0.99;
                dmax *= 1.01;
            };

            if (dmax < 0.0) {
                dmax *= 0.99;
                dmin *= 1.01;
            }
        }
    }

    var interval = dmax - dmin;

    var range = get_axes_range(width, height);

    var yR = d3.scaleLinear()
        .range([range.yMax, range.yMin])
        .domain([dmin - get_spectrum_margin() * interval, dmax + get_spectrum_margin() * interval]);

    var yAxis = d3.axisRight(yR)
        .tickSizeOuter([3])
        //.tickFormat(function(d) { return d.toPrecision(3) ; }) ;
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 1000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    d3.select("#yaxis").remove();
    svg = d3.select("#axes");

    // Add the Y Axis
    svg.append("g")
        .attr("class", "axis")
        .attr("id", "yaxis")
        .style("fill", axisColour)
        .style("stroke", axisColour)
        //.style("stroke-width", emStrokeWidth)
        .attr("transform", "translate(" + (0.75 * range.xMin - 1) + ",0)")
        .call(yAxis);

    //y-axis label
    var yLabel = "Integrated";

    if (intensity_mode == "mean")
        yLabel = "Mean";

    let fitsData = fitsContainer[va_count - 1];

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (intensity_mode == "integrated" && has_velocity_info)
            bunit += '•km/s';

        bunit = "[" + bunit + "]";
    }

    d3.select("#ylabel").text(yLabel + ' ' + fitsData.BTYPE.trim() + " " + bunit);
}

// Inverse Error Function
function erfinv(x) {
    // maximum relative error = .00013
    const a = 0.147;

    if (0 == x) { return 0 };

    const b = 2 / (Math.PI * a) + Math.log(1 - x ** 2) / 2;
    const sqrt1 = Math.sqrt(b ** 2 - Math.log(1 - x ** 2) / a);
    const sqrt2 = Math.sqrt(sqrt1 - b);
    return sqrt2 * Math.sign(x);
}

function pv_axes(left, top, width, height, vmin, vmax, pmin, pmax, pmean, pstd, x1, y1, x2, y2) {
    d3.select("#PVContourSVG").remove();
    d3.select("#PVSVGX").remove();
    d3.select("#PVSVGY").remove();
    d3.select("#PVLABELSVG").remove();
    d3.select("#PVTITLESVG").remove();
    d3.select("#PVSCALESVG").remove();
    d3.select("#PVAXISLINE").remove();
    d3.select("#velocityline").remove();
    d3.select("#angularline").remove();

    let svg_left = 10 + left;
    let svg_top = 10 + top;

    let svg_width = width;
    let svg_height = height;

    var div = d3.select("#PVDiagram");

    if (document.getElementById('PVSVGX') === null) {
        // console.log("pv_axes: PVSVGX is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSVGX")
            .attr("width", (svg_width + 4 * emFontSize))
            .attr("height", 5 * emFontSize)
            .attr('style', `position: fixed; left: ${svg_left - 2 * emFontSize}px; top: ${svg_top + svg_height}px; cursor: default`);
    }

    if (document.getElementById('PVSVGY') === null) {
        // console.log("pv_axes: PVSVGY is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSVGY")
            .attr("width", 10 * emFontSize)
            .attr("height", (svg_height + 2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left - 10 * emFontSize}px; top: ${svg_top - emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVLABELSVG') === null) {
        // console.log("pv_axes: PVLABELSVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVLABELSVG")
            .attr("width", 20 * emFontSize)
            .attr("height", (2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left - 5 * emFontSize}px; top: ${svg_top - 1.75 * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVTITLESVG') === null) {
        // console.log("pv_axes: PVTITLESVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVTITLESVG")
            .attr("width", svg_width)
            .attr("height", (2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left}px; top: ${svg_top - 2 * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVSCALESVG') === null) {
        // console.log("pv_axes: PVSCALESVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSCALESVG")
            .attr("width", (svg_width - 4 * emFontSize))
            .attr("height", 3 * emFontSize)
            .attr('style', `position: fixed; left: ${svg_left + 2 * emFontSize}px; top: ${svg_top - 4.5 * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVAXISLINE') === null) {
        // console.log("pv_axes: PVAXISLINE is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVAXISLINE")
            .attr("width", svg_width)
            .attr("height", svg_height)
            .attr("top", svg_top)
            .attr("left", svg_left)
            .attr("opacity", 0.0)
            .attr('style', `position: fixed; left: ${svg_left}px; top: ${svg_top}px; cursor: default`);
    }

    var xsvg = d3.select("#PVSVGX");
    var ysvg = d3.select("#PVSVGY");
    var labelsvg = d3.select("#PVLABELSVG");
    var titlesvg = d3.select("#PVTITLESVG");
    var scalesvg = d3.select("#PVSCALESVG");
    var axissvg = d3.select("#PVAXISLINE");

    // always use a dark theme for the PV diagram
    let _axisColour = "rgba(255,204,0,0.8)"; // axisColour

    var midx = (x1 + x2) / 2;
    var midy = (y1 + y2) / 2;

    let fitsData = fitsContainer[va_count - 1];

    // find the radec for the mid point
    let world = pix2sky(fitsData, midx, midy);
    let midra = world[0] / toDegrees;
    let middec = world[1] / toDegrees;
    console.log("midx:", midx, "midy:", midy, "midra:", midra, "middec:", middec);

    // find the dmin and dmax
    world = pix2sky(fitsData, x1, y1);
    let ra1 = world[0] / toDegrees;
    let dec1 = world[1] / toDegrees;

    world = pix2sky(fitsData, x2, y2);
    let ra2 = world[0] / toDegrees;
    let dec2 = world[1] / toDegrees;

    let dmin = HaversineDistance(midra, middec, ra1, dec1);
    let dmax = HaversineDistance(midra, middec, ra2, dec2);

    // convert dmin and dmax from radians to arcsec
    dmin *= 206264.80624709635516;
    dmax *= 206264.80624709635516;

    console.log("P-V Line offset [arcsec] dmin:", -Math.abs(dmin), "dmax:", Math.abs(dmax));

    var xR = d3.scaleLinear()
        .range([2 * emFontSize, 2 * emFontSize + svg_width])
        .domain([-Math.abs(dmin), Math.abs(dmax)]);

    pvxR = xR;

    div.append("div")
        .attr("id", "pvtooltip")
        .style("position", "absolute")
        .style("z-index", "10")
        .style("visibility", "hidden")
        .style("background", "rgba(0,0,0,0.7)")
        .style("color", "white")
        .html("&nbsp;<span id='angulartooltip'></span>&nbsp;<br/>&nbsp;<span id='velocitytooltip'></span>&nbsp;<br/>&nbsp;<span id='intensitytooltip'></span>&nbsp;"); // '<br/>' or ',&nbsp;'

    var xAxis = d3.axisBottom(xR)
        .tickSizeOuter([3])
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 10000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    // Add the X Axis
    xsvg.append("g")
        .attr("class", "axis")
        .attr("id", "pvxaxis")
        .style("fill", _axisColour)
        .style("stroke", _axisColour)
        .attr("transform", "translate(0,1)")
        .call(xAxis);

    var yR = d3.scaleLinear()
        .range([emFontSize + svg_height, emFontSize])
        .domain([vmin, vmax]);

    pvyR = yR;

    var yAxis = d3.axisLeft(yR)
        .tickSizeOuter([3])
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 10000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    // Add the Y Axis
    ysvg.append("g")
        .attr("class", "axis")
        .attr("id", "pvyaxis")
        .style("fill", _axisColour)
        .style("stroke", _axisColour)
        .attr("transform", "translate(" + (10 * emFontSize - 1) + ",0)")
        .call(yAxis);

    // the horizontal velocity line
    if (velocityline_position == -1) {
        var velocity;

        // check if there is a zero value between vmin and vmax
        if (vmin <= 0 && vmax >= 0) {
            velocity = 0;
        }
        else {
            velocity = (vmin + vmax) / 2;
        }

        // invert yR to get the position
        velocityline_position = yR(velocity) - emFontSize;

        // update the velocitytooltip html
        d3.select("#velocitytooltip")
            .html(velocity.toFixed(2) + " km/s");

        console.log("velocity:", velocity, "position:", velocityline_position);
    }

    // the vertical angular line
    if (angularline_position == -1) {
        // invert xR to get the position
        let angular = 0.0;

        angularline_position = xR(angular) - 2 * emFontSize;

        // update the angulartooltip html
        d3.select("#angulartooltip")
            .html(angular.toFixed(2) + " arcsec");

        console.log("angular position:", angularline_position);
    }

    // add a horizontal "velocityline"
    axissvg.append("line")
        .attr("id", "velocityline")
        .attr("x1", 0)
        .attr("y1", velocityline_position)
        .attr("x2", svg_width)
        .attr("y2", velocityline_position)
        .attr("stroke-width", emStrokeWidth)
        .attr("opacity", 0.5)
        .attr("stroke", "black")
        .attr("stroke-dasharray", "5, 3")
        .attr("pointer-events", "none");

    // add a vertical "angularline" at the mid-point
    axissvg.append("line")
        .attr("id", "angularline")
        .attr("x1", angularline_position)
        .attr("y1", 0)
        .attr("x2", angularline_position)
        .attr("y2", svg_height)
        .attr("stroke-width", emStrokeWidth)
        .attr("opacity", 0.5)
        .attr("stroke", "black")
        .attr("stroke-dasharray", "5, 3")
        .attr("pointer-events", "none");

    // svg pv rectangle for moving the crosshair
    axissvg.append("rect")
        .attr("id", "pv_rectangle")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", svg_width)
        .attr("height", svg_height)
        .attr("opacity", 0.0)
        .attr("pointer-events", "auto")
        .style("fill", "transparent")
        .style("stroke", "transparent")
        .on("mouseenter", function (event) {
            if (displayPVCrosshair) {
                crosshair_move(event); // first move the crosshair into place
                d3.select('#PVAXISLINE').attr("opacity", 1.0);
                d3.select("#pv_rectangle").style('cursor', 'none'); // was 'move'
            } else {
                d3.select("#pv_rectangle").style('cursor', 'default');
                d3.select("#pvtooltip").style("visibility", "hidden");
            }
        })
        .on("mouseleave", function (event) {
            d3.select("#pv_rectangle").style('cursor', 'default');
            d3.select('#PVAXISLINE').attr("opacity", 0.0);
            d3.select("#pvtooltip").style("visibility", "hidden");
        })
        .on("mousemove", function (event) {
            if (displayPVCrosshair) {
                crosshair_move(event);
            }
        });

    d3.select("#pv_rectangle").moveToFront();

    // labels

    // attach the statistics to the fitsData object
    fitsData.pvmin = pmin;
    fitsData.pvmax = pmax;
    fitsData.pvmean = pmean;
    fitsData.pvstd = pstd;

    xsvg.append("text")
        .attr("id", "pvxlabel")
        .attr("x", (svg_width + 4 * emFontSize) / 2)
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "middle")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("offset [arcsec]");

    xsvg.append("text")
        .attr("id", "pvpoint1")
        .attr("x", (2 * emFontSize))
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "start")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("① ...");

    xsvg.append("text")
        .attr("id", "pvpoint2")
        .attr("x", (svg_width + 2 * emFontSize))
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "end")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("... ②");

    var strYLabel = "";

    if (fitsData.SPECSYS.trim() != "")
        strYLabel = "<I>V<SUB>" + fitsData.SPECSYS.trim() + "</SUB></I> [km/s]";
    else
        strYLabel = "<I>V<SUB>" + 'LSRK' + "</SUB></I> [km/s]";

    labelsvg.append("foreignObject")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", 20 * emFontSize)
        .attr("height", 2 * emFontSize)
        .append("xhtml:div")
        .attr("id", "pvylabel")
        .style("display", "inline-block")
        .attr("class", "pv-label")
        .html(strYLabel);

    // a colour scale for the PV diagram
    const math_r = [66, 59, 100, 246, 255, 244, 233, 219, 255];
    const math_g = [91, 127, 168, 250, 255, 244, 202, 143, 107];
    const math_b = [227, 216, 203, 252, 255, 229, 118, 2, 0];

    // make an array of RGB colour strings
    let math_x = [0.0, 0.166667, 0.333333, 0.499999, 0.5, 0.500001, 0.666667, 0.833333, 1.0];
    let math_rgb = [];

    for (let i = 0; i < math_r.length; i++) {
        math_rgb.push("rgb(" + math_r[i] + "," + math_g[i] + "," + math_b[i] + ")");
    }

    var wolfram = d3.scaleLinear()
        .domain(math_x)
        .range(math_rgb);

    // replace the endings of the colour scale with the actual values
    let p1 = (pmin - pmean) / pstd;
    p1 = p1 / 6.0 + 0.5;
    console.log("lower (pmin,p1): ", pmin, p1);
    math_x[0] = Math.max(p1, 0.0);

    let p2 = (pmax - pmean) / pstd;
    p2 = p2 / 6.0 + 0.5;
    console.log("upper (pmax,p2): ", pmax, p2);
    math_x[math_x.length - 1] = Math.min(p2, 1.0);

    // adjust the endings of the colour scale to avoid infinities
    math_rgb[0] = wolfram(math_x[0]);
    console.log("lower (Wolfram RGB): ", pmin, math_x[0], math_rgb[0]);

    math_rgb[math_rgb.length - 1] = wolfram(math_x[math_x.length - 1]);
    console.log("upper (Wolfram RGB): ", pmax, math_x[math_x.length - 1], math_rgb[math_rgb.length - 1]);

    // invert the scale
    console.log("math_x: ", math_x);
    for (let i = 0; i < math_x.length; i++) {
        // convert from (0, 1) range to the underlying (truncated) range (pmin, pmax)
        math_x[i] = 6.0 * (math_x[i] - 0.5);
        math_x[i] = math_x[i] * pstd + pmean;
    }

    console.log("math_x: ", math_x);

    var linear = d3.scaleLinear()
        .domain(math_x)
        .range(math_rgb);

    scalesvg.append("g")
        .attr("class", "legendLinear");
    //.attr("transform", "translate(20,20)");

    var legendLinear = d3.legendColor()
        .shapeWidth((svg_width - 5.5 * emFontSize) / 10)
        .orient('horizontal')
        .cells(10)
        .scale(linear)
        //.labelFormat(d3.format(".2e")); // .1e or .2f
        .labelFormat(d3.format(".2g")); // .1e or .2f

    /*console.log(d3.format(".2g")(4.2));
    console.log(d3.format(".2g")(42000));
    console.log(d3.format(".2g")(0.0000008767));*/

    scalesvg.select(".legendLinear")
        .call(legendLinear);

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (fitsData.depth > 1 && has_velocity_info)
            bunit += '•km/s';

        bunit = "integrated intensity [" + bunit + "]";
    }

    titlesvg.append("text")
        .attr("id", "pvtitle")
        .attr("x", (svg_width) / 2)
        .attr("y", 1.5 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "middle")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text(bunit);
}

function composite_pv_axes(left, top, width, height, vmin, vmax, pmin, pmax, pmean, pstd, x1, y1, x2, y2) {
    d3.select("#PVContourSVG").remove();
    d3.select("#PVSVGX").remove();
    d3.select("#PVSVGY").remove();
    d3.select("#PVLABELSVG").remove();
    d3.select("#PVTITLESVG").remove();
    d3.select("#PVSCALESVG").remove();
    d3.select("#PVAXISLINE").remove();
    d3.select("#velocityline").remove();

    let svg_left = 10 + left;
    let svg_top = 10 + top;

    let svg_width = width;
    let svg_height = height;

    var div = d3.select("#PVDiagram");

    if (document.getElementById('PVSVGX') === null) {
        // console.log("pv_axes: PVSVGX is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSVGX")
            .attr("width", (svg_width + 4 * emFontSize))
            .attr("height", 5 * emFontSize)
            .attr('style', `position: fixed; left: ${svg_left - 2 * emFontSize}px; top: ${svg_top + svg_height}px; cursor: default`);
    }

    if (document.getElementById('PVSVGY') === null) {
        // console.log("pv_axes: PVSVGY is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSVGY")
            .attr("width", 10 * emFontSize)
            .attr("height", (svg_height + 2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left - 10 * emFontSize}px; top: ${svg_top - emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVLABELSVG') === null) {
        // console.log("pv_axes: PVLABELSVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVLABELSVG")
            .attr("width", 20 * emFontSize)
            .attr("height", (2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left - 5 * emFontSize}px; top: ${svg_top - 1.75 * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVTITLESVG') === null) {
        // console.log("pv_axes: PVTITLESVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVTITLESVG")
            .attr("width", svg_width)
            .attr("height", (2 * emFontSize))
            .attr('style', `position: fixed; left: ${svg_left}px; top: ${svg_top - 2 * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVSCALESVG') === null) {
        // console.log("pv_axes: PVSCALESVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVSCALESVG")
            .attr("width", (svg_width - 4 * emFontSize))
            .attr("height", 3 * emFontSize * va_count)
            .attr('style', `position: fixed; left: ${svg_left + 2 * emFontSize}px; top: ${svg_top - (1.0 + 3 * va_count) * emFontSize}px; cursor: default`);
    }

    if (document.getElementById('PVAXISLINE') === null) {
        // console.log("pv_axes: PVAXISLINE is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVAXISLINE")
            .attr("width", svg_width)
            .attr("height", svg_height)
            .attr("top", svg_top)
            .attr('style', `position: fixed; left: ${svg_left}px; top: ${svg_top}px; cursor: default`);
    }

    var xsvg = d3.select("#PVSVGX");
    var ysvg = d3.select("#PVSVGY");
    var labelsvg = d3.select("#PVLABELSVG");
    var titlesvg = d3.select("#PVTITLESVG");
    var scalesvg = d3.select("#PVSCALESVG");
    var axissvg = d3.select("#PVAXISLINE");

    // always use a dark theme for the PV diagram
    let _axisColour = "rgba(255,204,0,0.8)"; // axisColour

    var midx = (x1 + x2) / 2;
    var midy = (y1 + y2) / 2;

    let fitsData = fitsContainer[va_count - 1];

    // find the radec for the mid point
    let world = pix2sky(fitsData, midx, midy);
    let midra = world[0] / toDegrees;
    let middec = world[1] / toDegrees;
    console.log("midx:", midx, "midy:", midy, "midra:", midra, "middec:", middec);

    // find the dmin and dmax
    world = pix2sky(fitsData, x1, y1);
    let ra1 = world[0] / toDegrees;
    let dec1 = world[1] / toDegrees;

    world = pix2sky(fitsData, x2, y2);
    let ra2 = world[0] / toDegrees;
    let dec2 = world[1] / toDegrees;

    let dmin = HaversineDistance(midra, middec, ra1, dec1);
    let dmax = HaversineDistance(midra, middec, ra2, dec2);

    // convert dmin and dmax from radians to arcsec
    dmin *= 206264.80624709635516;
    dmax *= 206264.80624709635516;

    var xR = d3.scaleLinear()
        .range([2 * emFontSize, 2 * emFontSize + svg_width - 1])
        .domain([-Math.abs(dmin), Math.abs(dmax)]);

    var xAxis = d3.axisBottom(xR)
        .tickSizeOuter([3])
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 10000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    // Add the X Axis
    xsvg.append("g")
        .attr("class", "axis")
        .attr("id", "pvxaxis")
        .style("fill", _axisColour)
        .style("stroke", _axisColour)
        .attr("transform", "translate(0,1)")
        .call(xAxis);

    var yR = d3.scaleLinear()
        .range([emFontSize + svg_height - 1, emFontSize])
        .domain([vmin, vmax]);

    var yAxis = d3.axisLeft(yR)
        .tickSizeOuter([3])
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 10000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    // Add the Y Axis
    ysvg.append("g")
        .attr("class", "axis")
        .attr("id", "pvyaxis")
        .style("fill", _axisColour)
        .style("stroke", _axisColour)
        .attr("transform", "translate(" + (10 * emFontSize - 1) + ",0)")
        .call(yAxis);

    // the horizontal velocity line
    if (velocityline_position == -1) {
        var velocity;

        // check if there is a zero value between vmin and vmax
        if (vmin <= 0 && vmax >= 0) {
            velocity = 0;
        }
        else {
            velocity = (vmin + vmax) / 2;
        }

        // invert yR to get the position
        velocityline_position = yR(velocity) - emFontSize;

        console.log("velocity:", velocity, "position:", velocityline_position);
    }

    // add a horizontal "velocityline"
    axissvg.append("line")
        .attr("id", "velocityline")
        .attr("x1", 0)
        .attr("y1", velocityline_position)
        .attr("x2", svg_width)
        .attr("y2", velocityline_position)
        .attr("stroke-width", 2 * emStrokeWidth)
        .attr("opacity", 0.5)
        .attr("stroke", "red")
        .attr("stroke-dasharray", "5, 3");

    // labels
    xsvg.append("text")
        .attr("id", "pvxlabel")
        .attr("x", (svg_width + 4 * emFontSize) / 2)
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "middle")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("offset [arcsec]");

    xsvg.append("text")
        .attr("id", "pvpoint1")
        .attr("x", (2 * emFontSize))
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "start")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("① ...");

    xsvg.append("text")
        .attr("id", "pvpoint2")
        .attr("x", (svg_width + 2 * emFontSize))
        .attr("y", 3.0 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "end")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text("... ②");

    var strYLabel = "";

    if (fitsData.SPECSYS.trim() != "")
        strYLabel = "<I>V<SUB>" + fitsData.SPECSYS.trim() + "</SUB></I> [km/s]";
    else
        strYLabel = "<I>V<SUB>" + 'LSRK' + "</SUB></I> [km/s]";

    labelsvg.append("foreignObject")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", 20 * emFontSize)
        .attr("height", 2 * emFontSize)
        .append("xhtml:div")
        .attr("id", "pvylabel")
        .style("display", "inline-block")
        .attr("class", "pv-label")
        .html(strYLabel);

    for (let channel = 0; channel < va_count; channel++) {
        // make an array of RGB colour strings
        let math_x = [0.0, 0.166667, 0.333333, 0.499999, 0.5, 0.500001, 0.666667, 0.833333, 1.0];
        let math_rgb = [];

        for (let i = 0; i < math_x.length; i++) {
            // R
            if (channel == 0)
                math_rgb.push("rgb(" + Math.round(255 * math_x[i]) + ",0,0)");

            // G
            if (channel == 1)
                math_rgb.push("rgb(0," + Math.round(255 * math_x[i]) + ",0)");

            // B
            if (channel == 2)
                math_rgb.push("rgb(0,0," + Math.round(255 * math_x[i]) + ")");
        }

        var wolfram = d3.scaleLinear()
            .domain(math_x)
            .range(math_rgb);

        // replace the endings of the colour scale with the actual values
        let p1 = (pmin[channel] - pmean[channel]) / pstd[channel];
        p1 = p1 / 6.0 + 0.5;
        console.log("lower (pmin,p1): ", pmin[channel], p1);
        math_x[0] = Math.max(p1, 0.0);

        let p2 = (pmax[channel] - pmean[channel]) / pstd[channel];
        p2 = p2 / 6.0 + 0.5;
        console.log("upper (pmax,p2): ", pmax[channel], p2);
        math_x[math_x.length - 1] = Math.min(p2, 1.0);

        // adjust the endings of the colour scale to avoid infinities
        math_rgb[0] = wolfram(math_x[0]);
        console.log("lower (Wolfram RGB): ", pmin[channel], math_x[0], math_rgb[0]);

        math_rgb[math_rgb.length - 1] = wolfram(math_x[math_x.length - 1]);
        console.log("upper (Wolfram RGB): ", pmax[channel], math_x[math_x.length - 1], math_rgb[math_rgb.length - 1]);

        // invert the scale
        console.log("math_x: ", math_x);
        for (let i = 0; i < math_x.length; i++) {
            // convert from (0, 1) range to the underlying (truncated) range (pmin, pmax)
            math_x[i] = 6.0 * (math_x[i] - 0.5);
            math_x[i] = math_x[i] * pstd[channel] + pmean[channel];
        }

        console.log("math_x: ", math_x);

        let linear = d3.scaleLinear()
            .domain(math_x)
            .range(math_rgb);

        scalesvg.append("g")
            .attr("class", "legendLinear")
            .attr("id", "legendLinear" + channel)
            .attr("transform", "translate(0," + 3.0 * emFontSize * channel + ")");

        let legendLinear = d3.legendColor()
            .shapeWidth((svg_width - 5.5 * emFontSize) / 10)
            .shapeHeight(0.75 * emFontSize)
            .orient('horizontal')
            .cells(10)
            .scale(linear)
            .labelFormat(d3.format(".2g")); // .1e or .2f

        scalesvg.select("#legendLinear" + channel)
            .call(legendLinear);
    }

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (fitsData.depth > 1 && has_velocity_info)
            bunit += '•km/s';

        bunit = "integrated intensity [" + bunit + "]";
    }

    titlesvg.append("text")
        .attr("id", "pvtitle")
        .attr("x", (svg_width) / 2)
        .attr("y", 1.5 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", "1.25em")
        .attr("text-anchor", "middle")
        .style("fill", "lightgray")
        .attr("stroke", "none")
        .text(bunit);
}

function crosshair_move(event) {
    event.preventDefault = true;

    // get the dimensions of the SVG "PVAXISLINE"
    let _svg_top = parseFloat(d3.select("#PVAXISLINE").attr("top"));
    let _svg_left = parseFloat(d3.select("#PVAXISLINE").attr("left"));
    let _svg_width = parseFloat(d3.select("#PVAXISLINE").attr("width"));
    let _svg_height = parseFloat(d3.select("#PVAXISLINE").attr("height"));

    var offset = d3.pointer(event);

    // get the x,y coordinates of the mouse pointer within the PV region
    let x = offset[0];
    let y = offset[1];

    // make sure the line lies FIRMLY within the SVG
    x = Math.min(Math.max(x, 1), _svg_width - 1);
    y = Math.min(Math.max(y, 1), _svg_height - 1);

    // move the crosshair lines
    d3.select("#angularline")
        .attr("x1", x)
        .attr("x2", x);

    d3.select("#velocityline")
        .attr("y1", y)
        .attr("y2", y);

    let tooltipX = x + _svg_left;
    let tooltipY = y + _svg_top;

    // update the pvtooltip
    d3.select("#pvtooltip")
        .style("left", (tooltipX + 10) + "px")
        .style("top", (tooltipY + 10) + "px")
        .style("opacity", 0.7)
        .style("visibility", "visible");

    // given x, invert pvxR to get the angular offset
    let angular = pvxR.invert(2 * emFontSize + x);

    // given y, invert pvyR to get the velocity
    let velocity = pvyR.invert(emFontSize + y);

    // get the FITS data container
    let fitsData = fitsContainer[va_count - 1];

    // get the 'PVCanvas2'
    let canvas = document.getElementById("PVCanvas2");

    // apply a 10-pixel correction to x and y
    let coordX = tooltipX - 10;
    let coordY = tooltipY - 10;
    let pixel = canvas.getContext('2d').getImageData(coordX, coordY, 1, 1).data;

    // read the original value from the alpha channel
    let alpha = pixel[3];

    let intensity = 6.0 * (alpha / 255 - 0.5); // [0, 1]-- > [-3, 3]
    intensity = intensity * fitsData.pvstd + fitsData.pvmean;
    intensity = clamp(intensity, fitsData.pvmin, fitsData.pvmax);

    // update the angulartooltip html
    d3.select("#angulartooltip")
        .html(angular.toFixed(2) + " arcsec");

    // update the velocitytooltip html
    d3.select("#velocitytooltip")
        .html(velocity.toFixed(2) + " km/s");

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (fitsData.depth > 1 && has_velocity_info)
            bunit += '•km/s';
    }

    // update the intensitytooltip html
    d3.select("#intensitytooltip")
        .html(intensity.toFixed(2) + " " + bunit);
}

/** ---------------------------------------------------------------------
 * Create and compile an individual shader.
 * @param gl WebGLRenderingContext The WebGL context.
 * @param type Number The type of shader, either gl.VERTEX_SHADER or gl.FRAGMENT_SHADER
 * @param source String The code/text of the shader
 * @returns WebGLShader A WebGL shader program object.
 */
function createAndCompileShader(gl, type, source) {
    var typeName;
    switch (type) {
        case gl.VERTEX_SHADER:
            typeName = "Vertex Shader";
            break;
        case gl.FRAGMENT_SHADER:
            typeName = "Fragment Shader";
            break;
        default:
            console.error("Invalid type of shader in createAndCompileShader()");
            return null;
    }

    // Create shader object
    var shader = gl.createShader(type);
    if (!shader) {
        console.error("Fatal error: gl could not create a shader object.");
        return null;
    }

    // Put the source code into the gl shader object
    gl.shaderSource(shader, source);

    // Compile the shader code
    gl.compileShader(shader);

    // Check for any compiler errors
    var compiled = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
    if (!compiled && !gl.isContextLost()) {
        // There are errors, so display them
        var errors = gl.getShaderInfoLog(shader);
        console.error('Failed to compile ' + typeName + ' with these errors:' + errors);

        gl.deleteShader(shader);
        return null;
    }

    return shader;
};

/** ---------------------------------------------------------------------
 * Given two shader programs, create a complete rendering program.
 * @param gl WebGLRenderingContext The WebGL context.
 * @param vertexShaderCode String Code for a vertex shader.
 * @param fragmentShaderCode String Code for a fragment shader.
 * @returns WebGLProgram A WebGL shader program object.
 */
//
function createProgram(gl, vertexShaderCode, fragmentShaderCode) {
    // Create the 2 required shaders
    var vertexShader = createAndCompileShader(gl, gl.VERTEX_SHADER, vertexShaderCode);
    var fragmentShader = createAndCompileShader(gl, gl.FRAGMENT_SHADER, fragmentShaderCode);
    if (!vertexShader || !fragmentShader) {
        return null;
    }

    // Create a WebGLProgram object
    var program = gl.createProgram();
    if (!program) {
        console.error('Fatal error: Failed to create a program object');
        return null;
    }

    // Attach the shader objects
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);

    // Link the WebGLProgram object
    gl.linkProgram(program);

    // Check for success
    var linked = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (!linked && !gl.isContextLost()) {
        // There were errors, so get the errors and display them.
        var error = gl.getProgramInfoLog(program);
        console.error('Fatal error: Failed to link program: ' + error);
        gl.deleteProgram(program);
        gl.deleteShader(fragmentShader);
        gl.deleteShader(vertexShader);
        return null;
    }

    // Remember the shaders. This allows for them to be cleanly deleted.
    program.vShader = vertexShader;
    program.fShader = fragmentShader;

    return program;
};

function update_webgl_video_texture(index) {
    let image = videoFrame[index - 1];
    if (image == null) {
        console.log("update_webgl_video_texture: null video");
        return;
    }

    var gl = image.gl;
    if (gl == null) {
        console.log("update_webgl_video_texture: null gl");
        return;
    }

    gl.bindTexture(gl.TEXTURE_2D, image.tex);

    if (webgl2) {
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.rgba);
    }
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);
}

function update_webgl_video_viewport_texture(index) {
    let image = videoFrame[index - 1];
    if (image == null) {
        console.log("update_webgl_video_texture: null video");
        return;
    }

    var frame = image.zoom;

    var gl = frame.gl;
    if (gl == null) {
        console.log("update_webgl_video_viewport_texture: null gl");
        return;
    }

    gl.bindTexture(gl.TEXTURE_2D, frame.tex);

    if (webgl2) {
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.rgba);
    }
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);
}

function webgl_video_viewport_renderer(index, gl, width, height) {
    let image = videoFrame[index - 1];

    if (image == null) {
        console.log("webgl_video_renderer: null video");
        return;
    }

    var frame = image.zoom;
    var image_bounding_dims = image.image_bounding_dims;

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("rgba-shader").text;

    // grey-out pixels for alpha = 0.0
    var pos = fragmentShaderCode.lastIndexOf("}");
    fragmentShaderCode = fragmentShaderCode.insert_at(pos, "if (gl_FragColor.a == 0.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.3);\n");

    if (zoom_shape == "circle") {
        pos = fragmentShaderCode.lastIndexOf("}");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "float r_x = v_texcoord.z;\n float r_y = v_texcoord.w;\n if (r_x * r_x + r_y * r_y > 1.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.0);\n");
    }

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    frame.program = program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(frame.program, "a_position");

    // Create a position buffer
    frame.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, frame.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    frame.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, frame.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2) {
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.rgba);
    }
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    //WebGL how to convert from clip space to pixels
    //let px = viewport_zoom_settings.px;
    //let py = viewport_zoom_settings.py;

    // force the upper location, overriding the current <zoom_location>
    let px = emStrokeWidth;
    let py = emStrokeWidth;

    let viewport_size = viewport_zoom_settings.zoomed_size;
    py = height - py - viewport_size;
    gl.viewport(Math.round(px), Math.round(py), Math.round(viewport_size) - 0, Math.round(viewport_size) - 0);

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // the image bounding box
    var locationOfBox = gl.getUniformLocation(frame.program, "box");

    // drawRegion (execute the GLSL program)
    // Tell WebGL to use our shader program pair
    gl.useProgram(frame.program);

    let xmin = (viewport_zoom_settings.x - viewport_zoom_settings.clipSize - 1) / (image.width - 1);
    let ymin = (viewport_zoom_settings.y - viewport_zoom_settings.clipSize - 1) / (image.height - 1);

    let xmax = (viewport_zoom_settings.x + viewport_zoom_settings.clipSize + 1) / (image.width - 1);
    let ymax = (viewport_zoom_settings.y + viewport_zoom_settings.clipSize + 1) / (image.height - 1);

    let _width = xmax - xmin;
    let _height = ymax - ymin;

    //console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
    gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

    // Setup the attributes to pull data from our buffers
    gl.enableVertexAttribArray(positionLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, frame.positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);
}

function webgl_video_renderer(index, gl, width, height) {
    let image = videoFrame[index - 1];

    if (image == null) {
        console.log("webgl_video_renderer: null video");
        return;
    }

    var image_bounding_dims = image.image_bounding_dims;

    if (zoom_dims != null)
        if (zoom_dims.view != null) {
            image_bounding_dims = zoom_dims.view;
        }

    var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);

    if (va_count > 1 && !composite_view) {
        if (va_count == 2)
            scale = 0.8 * scale;
        else if (va_count == 4)
            scale = 0.6 * scale;
        else if (va_count == 5)
            scale = 0.5 * scale;
        else if (va_count == 6)
            scale = 0.45 * scale;
        else if (va_count == 7)
            scale = 0.45 * scale;
        else
            scale = 2 * scale / va_count;
    }

    var img_width = Math.floor(scale * image_bounding_dims.width);
    var img_height = Math.floor(scale * image_bounding_dims.height);
    // console.log("scaling by", scale, "new width:", img_width, "new height:", img_height, "orig. width:", image.image_bounding_dims.width, "orig. height:", image.image_bounding_dims.height);

    var image_position = get_image_position(index, width, height);
    var posx = image_position.posx;
    var posy = height - image_position.posy;
    console.log("index:", index, "image_position:", image_position);

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("rgba-shader").text;

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    image.program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(image.program, "a_position");

    // Create a position buffer
    image.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, image.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    image.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, image.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2) {
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.rgba);
    }
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    //WebGL how to convert from clip space to pixels
    if (va_count == 1 || composite_view)
        gl.viewport(Math.round((width - img_width) / 2), Math.round((height - img_height) / 2), Math.round(img_width) - 0, Math.round(img_height) - 0);
    else
        gl.viewport(Math.round(posx - img_width / 2), Math.round(posy - img_height / 2), Math.round(img_width) - 0, Math.round(img_height) - 0);

    console.log("gl.viewport:", gl.getParameter(gl.VIEWPORT));

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // the image bounding box
    var locationOfBox = gl.getUniformLocation(image.program, "box");

    // drawRegion (execute the GLSL program)
    // Tell WebGL to use our shader program pair
    gl.useProgram(image.program);

    // by default show the whole image
    var xmin = image.image_bounding_dims.x1 / (image.width - 0);// was - 1
    var ymin = image.image_bounding_dims.y1 / (image.height - 0);// was - 1
    var _width = image.image_bounding_dims.width / image.width;
    var _height = image.image_bounding_dims.height / image.height;

    if (zoom_dims != null)
        if (zoom_dims.view != null) {
            let view = zoom_dims.view;
            // console.log("view:", view);

            // handle the zoom view
            xmin = view.x1 / (image.width - 0);// was - 1
            ymin = view.y1 / (image.height - 0);// was - 1
            _width = view.width / image.width;
            _height = view.height / image.height;
        }

    console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
    gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

    // Setup the attributes to pull data from our buffers
    gl.enableVertexAttribArray(positionLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, image.positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);
}

function webgl_viewport_renderer(gl, container, height) {
    let image = imageContainer[va_count - 1];

    if (image == null) {
        console.log("webgl_viewport_renderer: null image");
        return;
    }

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(image.tone_mapping.flux + "-shader").text;

    if (webgl2)
        fragmentShaderCode = fragmentShaderCode + "\ncolour.a = colour.g;\n";

    fragmentShaderCode += document.getElementById(colourmap + "-shader").text;

    // grey-out pixels for alpha = 0.0
    var pos = fragmentShaderCode.lastIndexOf("}");
    fragmentShaderCode = fragmentShaderCode.insert_at(pos, "if (gl_FragColor.a == 0.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.3);\n");

    if (zoom_shape == "circle") {
        pos = fragmentShaderCode.lastIndexOf("}");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "float r_x = v_texcoord.z;\n float r_y = v_texcoord.w;\n if (r_x * r_x + r_y * r_y > 1.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.0);\n");
    }

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    var program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(program, "a_position");

    // Create a position buffer
    var positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    var tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RG32F, container.width, container.height, 0, gl.RG, gl.FLOAT, container.texture);
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE_ALPHA, container.width, container.height, 0, gl.LUMINANCE_ALPHA, gl.FLOAT, container.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    let index = va_count;

    //WebGL how to convert from clip space to pixels
    let px = viewport_zoom_settings.px;
    let py = viewport_zoom_settings.py;
    let viewport_size = viewport_zoom_settings.zoomed_size;
    py = height - py - viewport_size;
    gl.viewport(Math.round(px), Math.round(py), Math.round(viewport_size) - 0, Math.round(viewport_size) - 0);

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // the image bounding box
    var locationOfBox = gl.getUniformLocation(program, "box");

    // image tone mapping
    var locationOfParams = gl.getUniformLocation(program, "params");

    // drawRegion (execute the GLSL program)
    // Tell WebGL to use our shader program pair
    gl.useProgram(program);

    // show the entire viewport texture
    let xmin = 0.0;
    let ymin = 0.0;
    let _width = 1.0;
    let _height = 1.0;

    gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

    // get the multiplier
    var noise_sensitivity = document.getElementById('sensitivity' + index).value;
    var multiplier = get_noise_sensitivity(noise_sensitivity);

    if (image.tone_mapping.flux == "legacy") {
        var params = [image.tone_mapping.black, image.tone_mapping.white, image.tone_mapping.lmin, image.tone_mapping.lmax];
        gl.uniform4fv(locationOfParams, params);
    } else {
        if (image.tone_mapping.flux == "ratio")
            var params = [image.tone_mapping.median, multiplier * image.tone_mapping.ratio_sensitivity, image.tone_mapping.black, image.tone_mapping.white];
        else
            var params = [image.tone_mapping.median, multiplier * image.tone_mapping.sensitivity, image.tone_mapping.black, image.tone_mapping.white];

        gl.uniform4fv(locationOfParams, params);
    }

    // Setup the attributes to pull data from our buffers
    gl.enableVertexAttribArray(positionLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);

    invalidateViewport = true;

    // clean-up WebGL buffers etc.

    // position buffer
    if (positionBuffer != undefined)
        gl.deleteBuffer(positionBuffer);

    // texture
    if (tex != undefined)
        gl.deleteTexture(tex);

    // program
    if (program != undefined) {
        gl.deleteShader(program.vShader);
        gl.deleteShader(program.fShader);
        gl.deleteProgram(program);
    }
}

function webgl_composite_viewport_renderer(gl, container, height) {
    let image = compositeImage;

    if (image == null) {
        console.log("webgl_composite_viewport_renderer: null image");
        return;
    }

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(image.tone_mapping.flux + "-composite-shader").text;

    fragmentShaderCode += document.getElementById("composite-shader").text;

    // grey-out pixels for alpha = 0.0
    var pos = fragmentShaderCode.lastIndexOf("}");
    fragmentShaderCode = fragmentShaderCode.insert_at(pos, "if (gl_FragColor.a == 0.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.3);\n");

    if (zoom_shape == "circle") {
        pos = fragmentShaderCode.lastIndexOf("}");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "float r_x = v_texcoord.z;\n float r_y = v_texcoord.w;\n if (r_x * r_x + r_y * r_y > 1.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.0);\n");
    }

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    var program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(program, "a_position");

    // Create a position buffer
    var positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    var tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, container.width, container.height, 0, gl.RGBA, gl.FLOAT, container.texture);
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, container.width, container.height, 0, gl.RGBA, gl.FLOAT, container.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    //WebGL how to convert from clip space to pixels
    let px = viewport_zoom_settings.px;
    let py = viewport_zoom_settings.py;
    let viewport_size = viewport_zoom_settings.zoomed_size;
    py = height - py - viewport_size;
    gl.viewport(Math.round(px), Math.round(py), Math.round(viewport_size) - 0, Math.round(viewport_size) - 0);

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // the image bounding box
    var locationOfBox = gl.getUniformLocation(program, "box");

    // image tone mapping
    var locationOfParamsR = gl.getUniformLocation(program, "params_r");
    var locationOfParamsG = gl.getUniformLocation(program, "params_g");
    var locationOfParamsB = gl.getUniformLocation(program, "params_b");

    // create an array with parameter locations
    var locationOfParams = [locationOfParamsR, locationOfParamsG, locationOfParamsB];

    // drawRegion (execute the GLSL program)
    // Tell WebGL to use our shader program pair
    gl.useProgram(program);

    // show the entire viewport texture
    let xmin = 0.0;
    let ymin = 0.0;
    let _width = 1.0;
    let _height = 1.0;

    gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

    for (index = 1; index <= va_count; index++) {
        let tone_mapping = imageContainer[index - 1].tone_mapping;

        // get the multiplier
        let noise_sensitivity = document.getElementById('sensitivity' + index).value;
        let multiplier = get_noise_sensitivity(noise_sensitivity);

        if (tone_mapping.flux == "legacy") {
            let params = [tone_mapping.black, tone_mapping.white, tone_mapping.lmin, tone_mapping.lmax];
            gl.uniform4fv(locationOfParams[index - 1], params);
        } else if (tone_mapping.flux == "ratio") {
            let params = [tone_mapping.median, multiplier * tone_mapping.ratio_sensitivity, tone_mapping.black, tone_mapping.white];
            gl.uniform4fv(locationOfParams[index - 1], params);
        }
        else {
            let params = [tone_mapping.median, multiplier * tone_mapping.sensitivity, tone_mapping.black, tone_mapping.white];
            gl.uniform4fv(locationOfParams[index - 1], params);
        }
    }

    // Setup the attributes to pull data from our buffers
    gl.enableVertexAttribArray(positionLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // unbind the texture
    gl.bindTexture(gl.TEXTURE_2D, null);

    invalidateViewport = true;

    // clean-up WebGL buffers etc.

    // position buffer
    if (positionBuffer != undefined)
        gl.deleteBuffer(positionBuffer);

    // texture
    if (tex != undefined)
        gl.deleteTexture(tex);

    // program
    if (program != undefined) {
        gl.deleteShader(program.vShader);
        gl.deleteShader(program.fShader);
        gl.deleteProgram(program);
    }
}

function webgl_zoom_renderer(gl, height) {
    let image = imageContainer[va_count - 1];

    if (image == null) {
        console.log("webgl_zoom_renderer: null image");
        return;
    }

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(image.tone_mapping.flux + "-shader").text;

    if (webgl2)
        fragmentShaderCode = fragmentShaderCode + "\ncolour.a = colour.g;\n";

    fragmentShaderCode += document.getElementById(colourmap + "-shader").text;

    // grey-out pixels for alpha = 0.0
    var pos = fragmentShaderCode.lastIndexOf("}");
    fragmentShaderCode = fragmentShaderCode.insert_at(pos, "if (gl_FragColor.a == 0.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.3);\n");

    if (zoom_shape == "circle") {
        pos = fragmentShaderCode.lastIndexOf("}");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "float r_x = v_texcoord.z;\n float r_y = v_texcoord.w;\n if (r_x * r_x + r_y * r_y > 1.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.0);\n");
    }

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    viewport.program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(viewport.program, "a_position");

    // Create a position buffer
    viewport.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, viewport.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    viewport.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, viewport.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RG32F, image.width, image.height, 0, gl.RG, gl.FLOAT, image.texture);
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE_ALPHA, image.width, image.height, 0, gl.LUMINANCE_ALPHA, gl.FLOAT, image.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    var last_viewport_loop = 0;
    viewport.refresh = true;

    // shoud be done in an animation loop
    function zoom_rendering_loop(_timestamp) {
        if (viewport_zoom_settings == null) {
            // console.log("webgl_zoom_renderer: null viewport_zoom_settings");
            viewport.loopId = requestAnimationFrame(zoom_rendering_loop);
            return;
        }

        let now = performance.now();

        // limit the FPS
        let _fps = 30;
        if ((now - last_viewport_loop) < (1000 / _fps)) {
            viewport.loopId = requestAnimationFrame(zoom_rendering_loop);
            return;
        } else {
            last_viewport_loop = now;
        }

        if (viewport.gl === undefined || viewport.gl == null) {
            return;
        }

        if (!viewport.refresh) {
            viewport.loopId = requestAnimationFrame(zoom_rendering_loop);
            return;
        } else
            viewport.refresh = false;

        if (invalidateViewport) {
            clear_webgl_viewport();
            invalidateViewport = false;
        }

        let index = va_count;

        //WebGL how to convert from clip space to pixels
        let px = viewport_zoom_settings.px;
        let py = viewport_zoom_settings.py;
        let viewport_size = viewport_zoom_settings.zoomed_size;
        py = height - py - viewport_size;
        gl.viewport(Math.round(px), Math.round(py), Math.round(viewport_size) - 0, Math.round(viewport_size) - 0);

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);

        // the image bounding box
        var locationOfBox = gl.getUniformLocation(viewport.program, "box");

        // image tone mapping
        var locationOfParams = gl.getUniformLocation(viewport.program, "params");

        // drawRegion (execute the GLSL program)
        // Tell WebGL to use our shader program pair
        gl.useProgram(viewport.program);

        let xmin = (viewport_zoom_settings.x - viewport_zoom_settings.clipSize - 1) / (image.width - 1);
        let ymin = (viewport_zoom_settings.y - viewport_zoom_settings.clipSize - 1) / (image.height - 1);

        let xmax = (viewport_zoom_settings.x + viewport_zoom_settings.clipSize + 1) / (image.width - 1);
        let ymax = (viewport_zoom_settings.y + viewport_zoom_settings.clipSize + 1) / (image.height - 1);

        let _width = xmax - xmin;
        let _height = ymax - ymin;

        //console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
        gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

        // get the multiplier
        var noise_sensitivity = document.getElementById('sensitivity' + index).value;
        var multiplier = get_noise_sensitivity(noise_sensitivity);

        if (image.tone_mapping.flux == "legacy") {
            var params = [image.tone_mapping.black, image.tone_mapping.white, image.tone_mapping.lmin, image.tone_mapping.lmax];
            gl.uniform4fv(locationOfParams, params);
        } else {
            if (image.tone_mapping.flux == "ratio")
                var params = [image.tone_mapping.median, multiplier * image.tone_mapping.ratio_sensitivity, image.tone_mapping.black, image.tone_mapping.white];
            else
                var params = [image.tone_mapping.median, multiplier * image.tone_mapping.sensitivity, image.tone_mapping.black, image.tone_mapping.white];

            gl.uniform4fv(locationOfParams, params);
        }

        // Setup the attributes to pull data from our buffers
        gl.enableVertexAttribArray(positionLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, viewport.positionBuffer);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

        // execute the GLSL program
        // draw the quad (2 triangles, 6 vertices)
        gl.drawArrays(gl.TRIANGLES, 0, 6);

        viewport.loopId = requestAnimationFrame(zoom_rendering_loop);
    };

    viewport.loopId = requestAnimationFrame(zoom_rendering_loop);
}

function webgl_composite_zoom_renderer(gl, height) {
    let image = compositeImage;

    if (image == null) {
        console.log("webgl_composite_zoom_renderer: null image");
        return;
    }

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(image.tone_mapping.flux + "-composite-shader").text;

    fragmentShaderCode += document.getElementById("composite-shader").text;

    // grey-out pixels for alpha = 0.0
    var pos = fragmentShaderCode.lastIndexOf("}");
    fragmentShaderCode = fragmentShaderCode.insert_at(pos, "if (gl_FragColor.a == 0.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.3);\n");

    if (zoom_shape == "circle") {
        pos = fragmentShaderCode.lastIndexOf("}");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "float r_x = v_texcoord.z;\n float r_y = v_texcoord.w;\n if (r_x * r_x + r_y * r_y > 1.0) gl_FragColor.rgba = vec4(0.0, 0.0, 0.0, 0.0);\n");
    }

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    viewport.program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(viewport.program, "a_position");

    // Create a position buffer
    viewport.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, viewport.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    viewport.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, viewport.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, image.width, image.height, 0, gl.RGBA, gl.FLOAT, image.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    var last_viewport_loop = 0;
    viewport.refresh = true;

    // shoud be done in an animation loop
    function composite_zoom_rendering_loop(_timestamp) {
        if (viewport_zoom_settings == null) {
            // console.log("webgl_zoom_renderer: null viewport_zoom_settings");
            viewport.loopId = requestAnimationFrame(composite_zoom_rendering_loop);
            return;
        }

        let now = performance.now();

        // limit the FPS
        let _fps = 30;
        if ((now - last_viewport_loop) < (1000 / _fps)) {
            viewport.loopId = requestAnimationFrame(composite_zoom_rendering_loop);
            return;
        } else {
            last_viewport_loop = now;
        }

        if (viewport.gl === undefined || viewport.gl == null) {
            return;
        }

        if (!viewport.refresh) {
            viewport.loopId = requestAnimationFrame(composite_zoom_rendering_loop);
            return;
        } else
            viewport.refresh = false;

        if (invalidateViewport) {
            clear_webgl_viewport();
            invalidateViewport = false;
        }

        //WebGL how to convert from clip space to pixels
        let px = viewport_zoom_settings.px;
        let py = viewport_zoom_settings.py;
        let viewport_size = viewport_zoom_settings.zoomed_size;
        py = height - py - viewport_size;
        gl.viewport(Math.round(px), Math.round(py), Math.round(viewport_size) - 0, Math.round(viewport_size) - 0);

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);

        // the image bounding box
        var locationOfBox = gl.getUniformLocation(viewport.program, "box");

        // image tone mapping
        var locationOfParamsR = gl.getUniformLocation(viewport.program, "params_r");
        var locationOfParamsG = gl.getUniformLocation(viewport.program, "params_g");
        var locationOfParamsB = gl.getUniformLocation(viewport.program, "params_b");

        // create an array with parameter locations
        var locationOfParams = [locationOfParamsR, locationOfParamsG, locationOfParamsB];

        // drawRegion (execute the GLSL program)
        // Tell WebGL to use our shader program pair
        gl.useProgram(viewport.program);

        let xmin = (viewport_zoom_settings.x - viewport_zoom_settings.clipSize - 1) / (image.width - 1);
        let ymin = (viewport_zoom_settings.y - viewport_zoom_settings.clipSize - 1) / (image.height - 1);

        let xmax = (viewport_zoom_settings.x + viewport_zoom_settings.clipSize + 1) / (image.width - 1);
        let ymax = (viewport_zoom_settings.y + viewport_zoom_settings.clipSize + 1) / (image.height - 1);

        let _width = xmax - xmin;
        let _height = ymax - ymin;

        //console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
        gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

        for (index = 1; index <= va_count; index++) {
            let tone_mapping = imageContainer[index - 1].tone_mapping;

            // get the multiplier
            let noise_sensitivity = document.getElementById('sensitivity' + index).value;
            let multiplier = get_noise_sensitivity(noise_sensitivity);

            if (tone_mapping.flux == "legacy") {
                let params = [tone_mapping.black, tone_mapping.white, tone_mapping.lmin, tone_mapping.lmax];
                gl.uniform4fv(locationOfParams[index - 1], params);
            } else if (tone_mapping.flux == "ratio") {
                let params = [tone_mapping.median, multiplier * tone_mapping.ratio_sensitivity, tone_mapping.black, tone_mapping.white];
                gl.uniform4fv(locationOfParams[index - 1], params);
            }
            else {
                let params = [tone_mapping.median, multiplier * tone_mapping.sensitivity, tone_mapping.black, tone_mapping.white];
                gl.uniform4fv(locationOfParams[index - 1], params);
            }
        }

        // Setup the attributes to pull data from our buffers
        gl.enableVertexAttribArray(positionLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, viewport.positionBuffer);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

        // execute the GLSL program
        // draw the quad (2 triangles, 6 vertices)
        gl.drawArrays(gl.TRIANGLES, 0, 6);

        viewport.loopId = requestAnimationFrame(composite_zoom_rendering_loop);
    };

    viewport.loopId = requestAnimationFrame(composite_zoom_rendering_loop);
}

function init_webgl_zoom_buffers() {
    // place the viewport onto the zoom canvas
    var canvas = document.getElementById('ZOOMCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            cancelAnimationFrame(viewport.loopId);
            console.error("ZOOMCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("ZOOMCanvas: webglcontextrestored");
                init_webgl_zoom_buffers();
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2");
        viewport.gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_zoom_renderer(ctx, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl");
        viewport.gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_zoom_renderer(ctx, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function init_webgl_composite_zoom_buffers() {
    // place the viewport onto the zoom canvas
    var canvas = document.getElementById('ZOOMCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            cancelAnimationFrame(viewport.loopId);
            console.error("ZOOMCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("ZOOMCanvas: webglcontextrestored");
                init_webgl_composite_zoom_buffers();
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2");
        viewport.gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_composite_zoom_renderer(ctx, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl");
        viewport.gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_composite_zoom_renderer(ctx, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function clear_webgl_zoom_buffers() {
    // cancel the animation loop
    cancelAnimationFrame(viewport.loopId);

    var gl = viewport.gl;

    if (gl === undefined || gl == null)
        return;

    // position buffer
    if (viewport.positionBuffer != undefined)
        gl.deleteBuffer(viewport.positionBuffer);

    // texture
    if (viewport.tex != undefined)
        gl.deleteTexture(viewport.tex);

    // program
    if (viewport.program != undefined && viewport.program != null) {
        gl.deleteShader(viewport.program.vShader);
        gl.deleteShader(viewport.program.fShader);
        gl.deleteProgram(viewport.program);
        viewport.program = null;
    }

    viewport.gl = null;
}

function init_webgl_viewport_buffers(container) {
    // place the viewport onto the zoom canvas
    var canvas = document.getElementById('ViewportCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            console.error("ViewportCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("ViewportCanvas: webglcontextrestored");
                init_webgl_viewport_buffers(container);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2");
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_viewport_renderer(ctx, container, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl");
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_viewport_renderer(ctx, container, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function init_webgl_composite_viewport_buffers(container) {
    // place the viewport onto the zoom canvas
    var canvas = document.getElementById('ViewportCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            console.error("ViewportCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("ViewportCanvas: webglcontextrestored");
                init_webgl_composite_viewport_buffers(container);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2");
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_composite_viewport_renderer(ctx, container, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl");
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_composite_viewport_renderer(ctx, container, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function clear_webgl_viewport() {
    var canvas = document.getElementById('ViewportCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari

    if (webgl2) {
        var gl = canvas.getContext("webgl2");

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);
    } else if (webgl1) {
        var gl = canvas.getContext("webgl");

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);
    }
}

function init_webgl_composite_image_buffers() {
    //place the image onto the main canvas
    var canvas = document.getElementById('HTMLCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari
    var width = canvas.width;
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            cancelAnimationFrame(compositeImage.loopId);
            console.error("HTMLCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("HTMLCanvas: webglcontextrestored");
                init_webgl_composite_image_buffers();
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2", { preserveDrawingBuffer: true });
        compositeImage.gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_composite_image_renderer(ctx, width, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl", { preserveDrawingBuffer: true });
        compositeImage.gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_composite_image_renderer(ctx, width, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function init_webgl_image_buffers(index) {
    //place the image onto the main canvas
    if (va_count == 1)
        var canvas = document.getElementById('HTMLCanvas');
    else
        var canvas = document.getElementById('HTMLCanvas' + index);

    canvas.style.display = "block";// a hack needed by Apple Safari
    var width = canvas.width;
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();

            var image = imageContainer[index - 1];
            cancelAnimationFrame(image.loopId);
            console.error("HTMLCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("HTMLCanvas: webglcontextrestored");
                init_webgl_image_buffers(index);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2", { preserveDrawingBuffer: true });
        imageContainer[index - 1].gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_image_renderer(index, ctx, width, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl", { preserveDrawingBuffer: true });
        imageContainer[index - 1].gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_image_renderer(index, ctx, width, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function init_webgl_video_buffers(index) {
    //place the image onto the main canvas
    if (va_count == 1 || composite_view)
        var canvas = document.getElementById('HTMLCanvas');
    else
        var canvas = document.getElementById('HTMLCanvas' + index);

    canvas.style.display = "block";// a hack needed by Apple Safari
    var width = canvas.width;
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();
            console.error("HTMLCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("HTMLCanvas: webglcontextrestored");
                init_webgl_video_buffers(index);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2", { preserveDrawingBuffer: true });
        videoFrame[index - 1].gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_video_renderer(index, ctx, width, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl", { preserveDrawingBuffer: true });
        videoFrame[index - 1].gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_video_renderer(index, ctx, width, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function init_webgl_video_viewport_buffers(index) {
    //place the image onto the main canvas
    if (va_count > 1 && !composite_view)
        return;

    var canvas = document.getElementById('ViewportCanvas');

    canvas.style.display = "block";// a hack needed by Apple Safari
    var width = canvas.width;
    var height = canvas.height;

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();
            console.error("HTMLCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("HTMLCanvas: webglcontextrestored");
                init_webgl_video_viewport_buffers(index);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2", { preserveDrawingBuffer: true });
        videoFrame[index - 1].zoom.gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_video_viewport_renderer(index, ctx, width, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl", { preserveDrawingBuffer: true });
        videoFrame[index - 1].zoom.gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_video_viewport_renderer(index, ctx, width, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function clear_webgl_composite_image_buffers() {
    clear_webgl_internal_buffers(compositeImage);
}

function clear_webgl_image_buffers(index) {
    clear_webgl_internal_buffers(imageContainer[index - 1]);
}

function clear_webgl_video_buffers(index) {
    var video = videoFrame[index - 1];

    clear_webgl_internal_buffers(video);

    if (videoFrame[index - 1].zoom != null) {
        var gl = videoFrame[index - 1].zoom.gl;

        if (gl !== undefined && gl != null) {
            // Clear the canvas
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT);
        }

        // also hide the viewport cursor of the image rectangle
        var zoom_element = d3.select("#zoom");
        var zoom_cross = d3.select("#zoomCross");

        zoom_element.attr("opacity", 0.0);
        zoom_cross.attr("opacity", 0.0);

        // extra cleanup
        d3.select("#" + zoom_location).style("stroke", "transparent");
        d3.select("#" + zoom_location + "Cross").attr("opacity", 0.0);
        d3.select("#" + zoom_location + "Beam").attr("opacity", 0.0);
        d3.select("#pixel").text("").attr("opacity", 0.0);

        viewport_zoom_settings = null;

        clear_webgl_internal_buffers(video.zoom);
    }
}

function clear_webgl_internal_buffers(image) {
    if (image.first)
        return;

    // cancel the animation loop
    cancelAnimationFrame(image.loopId);

    var gl = image.gl;

    if (gl === undefined || gl == null)
        return;

    // position buffer
    if (image.positionBuffer !== undefined)
        gl.deleteBuffer(image.positionBuffer);

    // texture
    gl.deleteTexture(image.tex);

    // program
    if (image.program !== undefined && image.program != null) {
        gl.deleteShader(image.program.vShader);
        gl.deleteShader(image.program.fShader);
        gl.deleteProgram(image.program);
        image.program = null;
    }

    image.gl = null;
}

function process_hdr_viewport(img_width, img_height, pixels, alpha, index) {
    // console.log("process_hdr_viewport: #" + index);
    if (streaming || moving || dragging || windowLeft)
        return;

    // combine pixels with a mask
    let len = pixels.length | 0;
    var texture = new Float32Array(2 * len);
    let offset = 0 | 0;

    for (let i = 0 | 0; i < len; i = (i + 1) | 0) {
        texture[offset] = pixels[i];
        offset = (offset + 1) | 0;

        texture[offset] = (alpha[i] > 0) ? 1.0 : 0.0;
        offset = (offset + 1) | 0;
    }

    //next project the viewport
    if (va_count == 1) {

        let viewportContainer = { width: img_width, height: img_height, pixels: pixels, alpha: alpha, texture: texture };

        if (viewport != null) {
            // Clear the ZOOM Canvas
            //console.log("clearing the ZOOM Canvas");
            var gl = viewport.gl;

            if (gl !== undefined && gl != null) {
                gl.clearColor(0, 0, 0, 0);
                gl.clear(gl.COLOR_BUFFER_BIT);
            }
        }

        init_webgl_viewport_buffers(viewportContainer);
    } else {
        if (composite_view) {
            // create a composite viewport texture
            if (compositeViewportTexture == null) {
                compositeViewportTexture = new Float32Array(4 * len).fill(0.0); // RGBA
            }

            // add a single channel to the RGBA composite viewport texture
            let channel = (index - 1) | 0;
            let offset = 0 | 0;

            // console.log("viewport channel: " + channel + ", offset: " + offset + ", len: " + len + ", cross-check: " + (img_width * img_height));

            for (let i = 0 | 0; i < len; i = (i + 1) | 0) {
                compositeViewportTexture[(offset + channel) | 0] = pixels[i]; // RGB channels
                compositeViewportTexture[offset + 3] |= (alpha[i] > 0) ? 1.0 : 0.0; // alpha channel
                offset = (offset + 4) | 0;
            }

        } else {
            let viewportContainer = { width: img_width, height: img_height, pixels: pixels, alpha: alpha, texture: texture };

            if (imageContainer[index - 1] != null) {
                clear_webgl_image_buffers(index);

                // attach the viewportContainer to the imageContainer
                imageContainer[index - 1].viewportContainer = viewportContainer;

                // display the viewport as an image
                init_webgl_image_buffers(index);
            }
        }
    }

    viewport_count++;

    if (viewport_count == va_count && composite_view) {
        let viewportContainer = { width: img_width, height: img_height, texture: compositeViewportTexture };
        // console.log("process_hdr_viewport: all viewports loaded", viewportContainer);

        if (viewport != null) {
            // Clear the ZOOM Canvas
            //console.log("clearing the ZOOM Canvas");
            var gl = viewport.gl;

            if (gl !== undefined && gl != null) {
                gl.clearColor(0, 0, 0, 0);
                gl.clear(gl.COLOR_BUFFER_BIT);
            }
        }

        // display the composite viewport
        init_webgl_composite_viewport_buffers(viewportContainer);
    }
}

function process_hdr_image(img_width, img_height, pixels, alpha, tone_mapping, index) {
    // console.log("process_hdr_image: #" + index);
    var image_bounding_dims = true_image_dimensions(alpha, img_width, img_height);
    var pixel_range = image_pixel_range(pixels, alpha, img_width, img_height);
    console.log(image_bounding_dims, pixel_range);

    // combine pixels with a mask
    let len = pixels.length | 0;
    var texture = new Float32Array(2 * len);
    let offset = 0 | 0;

    for (let i = 0 | 0; i < len; i = (i + 1) | 0) {
        texture[offset] = pixels[i];
        offset = (offset + 1) | 0;

        texture[offset] = (alpha[i] > 0) ? 1.0 : 0.0;
        offset = (offset + 1) | 0;
    }

    if (imageContainer[index - 1] != null) {
        if (!streaming) {
            clear_webgl_image_buffers(index);
        }

        // re-use the existing tone mapping settings if possible
        tone_mapping = imageContainer[index - 1].tone_mapping;
    }

    imageContainer[index - 1] = { width: img_width, height: img_height, pixels: pixels, alpha: alpha, texture: texture, image_bounding_dims: image_bounding_dims, pixel_range: pixel_range, tone_mapping: tone_mapping, viewportContainer: null, first: false };

    //next display the image
    if (va_count == 1) {
        if (!streaming) {
            init_webgl_image_buffers(va_count);
        }

        setup_image_selection();

        try {
            display_scale_info();
        }
        catch (err) {
        };

        has_image = true;

        setup_viewports();

        hide_hourglass();
    } else {
        if (composite_view) {
            // create a composite image texture
            if (compositeImageTexture == null) {
                compositeImageTexture = new Float32Array(4 * len).fill(0.0); // RGBA
            }

            // add a single channel to the RGBA composite image texture
            let channel = (index - 1) | 0;
            let offset = 0 | 0;

            // console.log("image channel: " + channel + ", offset: " + offset + ", len: " + len + ", cross-check: " + (img_width * img_height));

            for (let i = 0 | 0; i < len; i = (i + 1) | 0) {
                compositeImageTexture[(offset + channel) | 0] = pixels[i]; // RGB channels
                compositeImageTexture[offset + 3] |= (alpha[i] > 0) ? 1.0 : 0.0; // alpha channel
                offset = (offset + 4) | 0;
            }
        } else {
            if (zoom_dims != null)
                if (zoom_dims.view != null)
                    image_bounding_dims = zoom_dims.view;

            var c = document.getElementById('HTMLCanvas' + index);
            var width = c.width;
            var height = c.height;

            var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);

            if (va_count > 1) {
                if (va_count == 2)
                    scale = 0.8 * scale;
                else if (va_count == 4)
                    scale = 0.6 * scale;
                else if (va_count == 5)
                    scale = 0.5 * scale;
                else if (va_count == 6)
                    scale = 0.45 * scale;
                else if (va_count == 7)
                    scale = 0.45 * scale;
                else
                    scale = 2 * scale / va_count;
            }

            var img_width = Math.floor(scale * image_bounding_dims.width);
            var img_height = Math.floor(scale * image_bounding_dims.height);
            // console.log("scaling by", scale, "new width:", img_width, "new height:", img_height, "orig. width:", image.image_bounding_dims.width, "orig. height:", image.image_bounding_dims.height);

            var image_position = get_image_position(index, width, height);
            var posx = image_position.posx;
            var posy = /*height -*/ image_position.posy;
            console.log("index:", index, "image_position:", image_position);

            if (!streaming) {
                init_webgl_image_buffers(index);
            }

            setup_image_selection_index(index, posx - img_width / 2, posy - img_height / 2, img_width, img_height);

            //trigger a tileTimeout
            /*if (zoom_dims != null)
                if (zoom_dims.view != null)
                    tileTimeout(true);*/
        }
    };

    image_count++;

    if (image_count == va_count) {
        if (composite_view) {
            // find the common bounding box for all images
            // start with the first image
            let _x1 = imageContainer[0].image_bounding_dims.x1;
            let _y1 = imageContainer[0].image_bounding_dims.y1;
            let _width = imageContainer[0].image_bounding_dims.width;
            let _height = imageContainer[0].image_bounding_dims.height;

            // iterate through the remaining images
            for (let i = 1; i < va_count; i = (i + 1)) {
                let image_bounding_dims = imageContainer[i].image_bounding_dims;
                _x1 = Math.min(_x1, image_bounding_dims.x1);
                _y1 = Math.min(_y1, image_bounding_dims.y1);
                _width = Math.max(_width, image_bounding_dims.width);
                _height = Math.max(_height, image_bounding_dims.height);
            }

            // make the new bounding box
            var new_image_bounding_dims = {
                x1: _x1,
                y1: _y1,
                width: _width,
                height: _height
            };

            // borrow the tone mapping flux from the first image
            var new_tone_mapping = { flux: imageContainer[0].tone_mapping.flux };

            compositeImage = { width: img_width, height: img_height, texture: compositeImageTexture, image_bounding_dims: new_image_bounding_dims, tone_mapping: new_tone_mapping };
            // console.log("process_hdr_image: all images loaded", compositeImage);

            if (!streaming) {
                // clear the composite image buffers
                clear_webgl_composite_image_buffers();

                //display the composite image
                init_webgl_composite_image_buffers();
            }

            setup_image_selection();

            try {
                display_scale_info();
            }
            catch (err) {
            };

            setup_viewports();
        }

        has_image = true;

        hide_hourglass();
    }
}

async function process_hds_spectrum(img_width, img_height, pixels, alpha, div) {
    var bounds = true_image_dimensions(alpha, img_width, img_height);
    console.log("image_bounding_dims:", bounds);

    //bounds.x1 = 0;
    //bounds.x2 = img_width;

    // print spectrum dimensions
    console.log("HDS spectrum dimensions:", img_width, img_height);

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    let titleStr = fitsData.OBJECT.replace(/_/g, " ").trim();

    var dateobs = fitsData.DATEOBS;

    if (dateobs == '')
        dateobs = '';//'DATEOBS N/A' ;
    else {
        var pos = dateobs.indexOf('.');

        if (pos >= 0)
            dateobs = dateobs.substr(0, pos);

        dateobs = dateobs.replace(/T/g, " ") + ' ' + fitsData.TIMESYS;
    }

    let raText = '';

    // check if fitsData.RA is not empty
    if (fitsData.RA != '') {
        raText = 'RA: ' + fitsData.RA;
    }

    let decText = '';

    // check if fitsData.DEC is not empty
    if (fitsData.DEC != '') {
        decText = 'DEC: ' + fitsData.DEC;
    }

    var svg = d3.select("#FrontSVG");
    var div_width = parseFloat(svg.attr("width"));
    var div_height = 0.95 * parseFloat(svg.attr("height"));

    // override the div height if img_height > 1
    if (img_height > 1) {
        // set overflow-y:scroll for the 'SpectrumDiv' div
        d3.select("#" + div).style("overflow-y", "scroll").style("height", div_height + "px");

        //div_width *= 0.9;
        div_height /= 2;
    }

    // iterate through fitsData.watArray, discarding the keywords and keeping the values
    let watArray = fitsData.watArray;
    let watValues = [];

    for (let i = 0; i < watArray.length; i++) {
        let wat = watArray[i];
        let pos = wat.indexOf('=');

        if (pos >= 0) {
            let value = wat.substr(pos + 1).trim();
            // strip the enclosing quotes
            value = value.replace(/'/g, "");
            watValues.push(value);
        }
    }

    // concatenate the WAT values into a single string
    let watStr = watValues.join("");

    // loop through img_height
    var offset = 0;
    for (let k = 0; k < img_height; k++) {
        let specStr = '';
        // extract 'spec' + (k+1) from the WAT string (the value is enclosed in double quotes)
        let spec = 'spec' + (k + 1);
        let pos = watStr.indexOf(spec);

        if (pos > -1) {
            let pos1 = watStr.indexOf('"', pos);
            let pos2 = watStr.indexOf('"', pos1 + 1);

            if (pos1 > -1 || pos2 > -1) {
                specStr = watStr.substr(pos1 + 1, pos2 - pos1 - 1);
            }
        }

        // if specStr is not empty, split it into an array of numbers
        let specValues = [];
        if (specStr != '') {
            let specArray = specStr.split(" ");

            for (let i = 0; i < specArray.length; i++) {
                let value = parseFloat(specArray[i]);

                if (!isNaN(value))
                    specValues.push(value);
            }
        }

        let a = NaN;
        let b = NaN;

        if (specValues.length == 9) {
            x0 = specValues[2];
            y0 = specValues[3];
            a = specValues[4];
            b = y0 - a * x0;
        }

        // prepare data for Plotly.js
        let x = [];
        let y = [];

        let mean = 0.0;
        let std = 0.0;
        let count = 0;

        // first pass
        for (let i = 0; i < img_width; i++) {
            let world = pix2sky(fitsData, i + 0.5, 0);// '-' --> '+' for the HDS spectrum

            // if a and b are not NaN, apply a linear transformation
            if (!isNaN(a) && !isNaN(b)) { world[0] = a * i + b; }

            x.push(world[0]);

            // push NaN for the masked pixels
            if (alpha[offset + i] > 0) {
                y.push(pixels[offset + i]);
                mean += pixels[offset + i];
                count += 1;
            }
            else
                y.push(NaN);
        }

        if (count > 0)
            mean /= count;

        // second pass
        for (let i = 0; i < y.length; i++) {
            if (alpha[offset + i] > 0)
                std += Math.pow(y[i] - mean, 2);
        }

        offset += img_width;

        if (count > 0)
            std = Math.sqrt(std / count);

        // console.log("Spectrum #", (k + 1), "HDS spectrum mean:", mean, "std:", std, "count:", count);

        let divId = "SpectrumDiv#" + (k + 1);

        // create a new div element by appending it to the "SpectrumDiv" div
        d3.select("#" + div).append("div")
            .attr("id", divId);

        let title = '';
        let yoffset = 1.05;

        if (img_height > 1) {
            title = "Spectrum #" + (k + 1) + "/" + img_height + ': ';
            yoffset = 1.15;
        }

        hds_divs.push({ id: divId, offset: yoffset });

        let res = plot_time_series(x, y, mean, std, divId, div_width, div_height, yoffset, title + titleStr, dateobs, raText, decText);

        if (k < 2)
            await res;
    }

    has_image = true;

    hide_hourglass();
}

function romanize(num) {
    var lookup = { M: 1000, CM: 900, D: 500, CD: 400, C: 100, XC: 90, L: 50, XL: 40, X: 10, IX: 9, V: 5, IV: 4, I: 1 }, roman = '', i;

    for (i in lookup) {
        while (num >= lookup[i]) {
            roman += i;
            num -= lookup[i];
        }
    }

    return roman;
}

function insert_atomic_spectra(div, data) {
    // delete the existing database first
    // (it might be empty or contain outdated data)
    {
        let deleteRequest = indexedDB.deleteDatabase(div);

        deleteRequest.onsuccess = function () {
            // console.log("Database deleted successfully", div);
        };

        deleteRequest.onerror = function () {
            console.error("Error deleting database", div);
        };
    }

    let openRequest = indexedDB.open(div, 1);

    openRequest.onupgradeneeded = function (event) {
        // triggers if the client had no database
        // ...perform initialization...
        const db = openRequest.result;

        if (event.oldVersion == 0) {
            console.log("Populating the atomic spectra database", div);

            // create an object store for the atomic spectra
            const objectStore = db.createObjectStore("lines", { autoIncrement: true });

            // create a wavelength index
            objectStore.createIndex("obs_wl", "obs_wl", { unique: false });

            // Use transaction oncomplete to make sure the objectStore creation is
            // finished before adding data into it.
            objectStore.transaction.oncomplete = (event) => {
                // Store values in the newly created objectStore.
                const lines = db
                    .transaction("lines", "readwrite")
                    .objectStore("lines");

                data.forEach((atom) => {
                    lines.add(atom);
                });
            };
        }
    };

    openRequest.onerror = function () {
        console.error("Error", openRequest.error);
    };

    /*openRequest.onsuccess = function () {
        const db = openRequest.result;
        console.log("Success", db);

        const lines = db.transaction("lines", "readonly").objectStore("lines");

        // list all atomic spectra
        let cursor = lines.openCursor();

        cursor.onsuccess = function (event) {
            let cursor = event.target.result;

            if (cursor) {
                console.log("Atomic spectrum:", cursor.value);
                cursor.continue();
            }
        };

        // close the database
        db.close();
    }*/
}

async function plot_time_series(x, y, mean, std, div, width, height, yoffset, title, date, ra, dec) {
    var paper_bgcolor, plot_bgcolor, line_color, hover_bgcolor, gridcolor, font_color;

    let wmin = d3.min(x);
    let wmax = d3.max(x);
    // console.log("wmin:", wmin, "wmax:", wmax, "Å");

    fetch_atomic_spectra(wmin, wmax).then(spectra => {
        console.log("fetch_atomic_spectra: #" + spectra.length);

        if (theme == 'bright') {
            paper_bgcolor = 'rgba(255, 255, 255, 1.0)';
            plot_bgcolor = 'rgba(255, 255, 255, 1.0)';
            line_color = '#636EFA'; // default Plotly.js muted blue
            hover_bgcolor = '#FFFFFF';
            gridcolor = 'rgba(0, 0, 0, 0.5)';
            font_color = 'rgba(0, 0, 0, 1.0)'
        } else {
            paper_bgcolor = 'rgba(0, 0, 0, 1.0)';
            plot_bgcolor = 'rgba(0, 0, 0, 1.0)';
            line_color = 'rgba(255,204,0,1.0)' // Amber
            hover_bgcolor = '#000000';
            gridcolor = 'rgba(255, 255, 255, 0.5)';
            font_color = 'rgba(255, 255, 255, 1.0)'
        }

        var data = [{
            x: x,
            y: y,
            type: 'scatter',
            line: { color: line_color }
        }];

        var layout = {
            paper_bgcolor: paper_bgcolor,
            plot_bgcolor: plot_bgcolor,
            hoverlabel: { bgcolor: hover_bgcolor },
            title: {
                text: title + ' (' + date + ')' + ' ' + ra + ' ' + dec, font: { color: font_color }
            },
            autosize: false,
            width: width,
            height: height,
            xaxis: {
                wrange: [wmin, wmax], // store the original wavelength range
                autorange: true,
                rangeslider: { visible: true },
                type: 'linear',
                title: { text: 'Wavelength [Å]', font: { color: font_color } },
                gridcolor: gridcolor
            },
            yaxis: {
                fixedrange: false,
                type: 'linear',
                rangemode: 'tozero',
                range: [0.0, mean + 5.0 * std],
                title: { text: 'Normalised intensity [arb. unit]', font: { color: font_color } },
                gridcolor: gridcolor
            }
        };

        let noatoms = spectra.length;

        if (noatoms > 0 && noatoms <= limit) {
            var shapes = [];
            var annotations = [];

            // add a vertical line for each atomic spectrum
            for (let i = 0; i < noatoms; i++) {
                let atom = spectra[i];

                let element = atom.element;
                let number = atom.sp_num;
                let wavelength = atom.obs_wl;

                let line = {
                    type: 'line',
                    x0: wavelength,
                    y0: 0,
                    x1: wavelength,
                    y1: 1.0,
                    xref: 'x',
                    yref: 'paper',
                    line: {
                        color: 'grey',
                        width: 1.0,
                        dash: 'dot'
                    }
                };

                let label = {
                    font: { style: "normal" },
                    x: wavelength,
                    y: yoffset,
                    xref: 'x',
                    yref: 'paper',
                    text: element + ' ' + romanize(number),
                    textangle: -45,
                    xanchor: 'center',
                    yanchor: 'center',
                    showarrow: false,
                    arrowhead: 1
                    /*ax: 0,
                    ay: -40*/
                };

                shapes.push(line);
                annotations.push(label);
            }

            layout.shapes = shapes;
            layout.annotations = annotations;
        } else {
            const text = noatoms > 0 ? 'The number of atomic spectra found (' + noatoms + ') exceeds the limit (' + limit + '). Zoom-in to see the atomic spectra.' : 'No atomic spectra found.';

            const annotations = [{
                x: 0.5,
                y: yoffset,
                xref: 'paper',
                yref: 'paper',
                text: text,
                showarrow: false
            }];

            layout.annotations = annotations;
        }

        Plotly.newPlot(div, data, layout);

        var myPlot = document.getElementById(div);

        /*myPlot.on('plotly_afterplot', function (event) {
            var range = myPlot.layout.xaxis.range;
            var d2p = myPlot._fullLayout.xaxis.d2p;
            var l2p = myPlot._fullLayout.xaxis.l2p;

            let x0 = d2p(wmin);
            let x1 = d2p(wmax);
            let dx = x1 - x0;

            console.log("d2p(" + wmin + ") = ", x0);
            console.log("d2p(" + wmax + ") = ", x1);
            console.log("xaxis range dx = ", dx);

            d3.selectAll(".annotation-text-g").selectAll('text')
                .call(d3.drag()
                    .on("drag", dragMolecule)
                    .on("end", dropMolecule));
            //.on("click", function (d) {
            //   // d.target.__data__.x ???
            //  console.log("annotation-text @" + d.x);
            //});
        });*/

        // removed the limit check as it was interfering with re-layout event when redshift != 0
        /*if (noatoms > limit)*/ {
            //add relayout event function to graph
            myPlot.on('plotly_relayout', function (event) {
                clearTimeout(idleResize);

                idleResize = setTimeout(function () {
                    plotlyRelayoutEventFunction(event, div, yoffset);
                }, 250);
            });

            insert_atomic_spectra(datasetId + '/' + div, spectra); // only needed if there is re-layout event
        }
    });
}

async function refresh_hds_spectral_lines(item, _) {
    let div = item.id;
    let yoffset = item.offset;

    var myPlot = document.getElementById(div);
    var range = myPlot.layout.xaxis.range;

    console.log("refresh_hds_spectral_lines: ", div, yoffset, range, "z:", redshift);

    let wmin = range[0];
    let wmax = range[1];
    let z = redshift;
    // console.log("wmin:", wmin, "wmax:", wmax, "Å");

    fetch_atomic_spectra(wmin / (1 + z), wmax / (1 + z)).then(spectra => {
        console.log(div, "fetch_atomic_spectra: #" + spectra.length);

        let noatoms = spectra.length;

        var shapes = [];
        var annotations = [];

        if (noatoms > 0 && noatoms <= limit) {
            // add a vertical line for each atomic spectrum
            for (let i = 0; i < noatoms; i++) {
                let atom = spectra[i];

                let element = atom.element;
                let number = atom.sp_num;
                let wavelength = atom.obs_wl * (1 + z);

                let line = {
                    type: 'line',
                    x0: wavelength,
                    y0: 0,
                    x1: wavelength,
                    y1: 1.0,
                    xref: 'x',
                    yref: 'paper',
                    line: {
                        color: 'grey',
                        width: 1.0,
                        dash: 'dot'
                    }
                };

                let label = {
                    font: { style: "normal" },
                    x: wavelength,
                    y: yoffset,
                    xref: 'x',
                    yref: 'paper',
                    text: element + ' ' + romanize(number),
                    textangle: -45,
                    xanchor: 'center',
                    yanchor: 'center',
                    showarrow: false,
                    arrowhead: 1
                    /*ax: 0,
                    ay: -40*/
                };

                shapes.push(line);
                annotations.push(label);
            }
        } else {
            const text = noatoms > 0 ? 'The number of atomic spectra found (' + noatoms + ') exceeds the limit (' + limit + '). Zoom-in to see the atomic spectra.' : 'No atomic spectra found.';

            annotations = [{
                x: 0.5,
                y: yoffset,
                xref: 'paper',
                yref: 'paper',
                text: text,
                showarrow: false
            }];
        }

        Plotly.relayout(div, { annotations: annotations, shapes: shapes });
    });
}

function dropMolecule(event) {
    event.preventDefault = true;

    // re-layout the graph
}

function dragMolecule(event) {
    event.preventDefault = true;

    console.log("dragMolecule: ", event, event.dx);

    // re-calculate the redshift

    // shift all the molecules by dx
    //var elems = d3.selectAll(".annotation-text-g").selectAll('text');//.attr("transform", "translate(" + event.dx + ",0)");
    //console.log("dragMolecule: ", elems);
    //elems.attr("transform", "translate(" + 10 + ",100)");
}

function plotlyRelayoutEventFunction(event, id, yoffset) {
    const dbName = datasetId + '/' + id;

    var elem = document.getElementById(id);
    var layout = elem.layout;

    console.log("plotly_relayout event:", event, "id:", id);

    var xmin, xmax;

    try {
        xmin = event['xaxis.range[0]'];
        xmax = event['xaxis.range[1]'];

        console.log("re-sizing the plotly graph", xmin, xmax);
    } catch (err) {
        console.log("plotly_relayout event: no rangeslider event!!");
        return;
    }

    // if ymax is undefined, return
    if (xmin == undefined || xmax == undefined) {
        // check if xaxis.range is an array
        if (Array.isArray(event['xaxis.range'])) {
            xmin = event['xaxis.range'][0];
            xmax = event['xaxis.range'][1];
        } else if (event['xaxis.autorange'] == true) {
            // xaxis.autorange is true, get the original range
            xmin = layout.xaxis.wrange[0];
            xmax = layout.xaxis.wrange[1];
        }
        else {
            console.log("plotly_relayout event: no x-axis range!!");
            return;
        }
    }

    try {
        console.log("plotly_relayout event: updating the atomic spectra", xmin, xmax);

        if (Math.abs(redshift) == 0) {
            let openRequest = indexedDB.open(dbName, 1);

            openRequest.onsuccess = function () {
                const db = openRequest.result;

                const lines = db.transaction("lines", "readonly").objectStore("lines");
                const index = lines.index("obs_wl");

                let request = index.getAll(IDBKeyRange.bound(xmin, xmax));

                request.onsuccess = function () {
                    if (request.result !== undefined) {
                        spectra = request.result;
                        console.log("plotly_relayout event: indexedDB  atomic spectra found", spectra.length, "in the range", xmin, xmax, spectra);

                        let noatoms = spectra.length;

                        var shapes = [];
                        var annotations = [];

                        if (noatoms > 0 && noatoms <= limit) {
                            for (let i = 0; i < noatoms; i++) {
                                let atom = spectra[i];

                                let element = atom.element;
                                let number = atom.sp_num;
                                let wavelength = atom.obs_wl;

                                let line = {
                                    type: 'line',
                                    x0: wavelength,
                                    y0: 0,
                                    x1: wavelength,
                                    y1: 1.0,
                                    xref: 'x',
                                    yref: 'paper',
                                    line: {
                                        color: 'grey',
                                        width: 1.0,
                                        dash: 'dot'
                                    }
                                };

                                let label = {
                                    font: { style: "normal" },
                                    x: wavelength,
                                    y: yoffset,
                                    xref: 'x',
                                    yref: 'paper',
                                    text: element + ' ' + romanize(number),
                                    textangle: -45,
                                    xanchor: 'center',
                                    yanchor: 'center',
                                    showarrow: false
                                };

                                shapes.push(line);
                                annotations.push(label);
                            }
                        } else {
                            const text = noatoms > 0 ? 'The number of atomic spectra found (' + noatoms + ') exceeds the limit (' + limit + '). Zoom-in to see the atomic spectra.' : 'No atomic spectra found.';

                            annotations = [{
                                x: 0.5,
                                y: yoffset,
                                xref: 'paper',
                                yref: 'paper',
                                text: text,
                                showarrow: false
                            }];
                        };

                        Plotly.relayout(id, { annotations: annotations, shapes: shapes });
                    } else {
                        console.log("No spectra found in the range", xmin, xmax);
                    }
                };

                // close the database
                db.close();
            }
        } else {
            // make an item and call refresh_hds_spectral_lines
            var item = { id: id, offset: yoffset };

            refresh_hds_spectral_lines(item, null);
        }
    } catch (err) {
        console.log("plotly_relayout event: no annotations!!");
        return;
    }
}

function webgl_composite_image_renderer(gl, width, height) {
    var scale = get_image_scale(width, height, compositeImage.image_bounding_dims.width, compositeImage.image_bounding_dims.height);
    var img_width = Math.floor(scale * compositeImage.image_bounding_dims.width);
    var img_height = Math.floor(scale * compositeImage.image_bounding_dims.height);
    //console.log("scaling by", scale, "new width:", img_width, "new height:", img_height, "orig. width:", compositeImage.image_bounding_dims.width, "orig. height:", compositeImage.image_bounding_dims.height);

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    try {
        var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(compositeImage.tone_mapping.flux + "-composite-shader").text;
        console.log("webgl_composite_image_renderer: using a common tone mapping", compositeImage.tone_mapping.flux);
    } catch (_) {
        // this will be triggered only for datasets where the tone mapping has not been set (i.e. the mask is null etc...)
        var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById("legacy-composite-shader").text;
    }

    fragmentShaderCode += document.getElementById("composite-shader").text;

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    compositeImage.program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(compositeImage.program, "a_position");

    // Create a position buffer
    compositeImage.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, compositeImage.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    compositeImage.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, compositeImage.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    if (webgl2)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, compositeImage.width, compositeImage.height, 0, gl.RGBA, gl.FLOAT, compositeImage.texture);
    else
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, compositeImage.width, compositeImage.height, 0, gl.RGBA, gl.FLOAT, compositeImage.texture);

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    compositeImage.refresh = true;
    compositeImage.first = true;

    // shoud be done in an animation loop
    function composite_image_rendering_loop(_timestamp) {
        // set a flag
        compositeImage.first = false;

        if (compositeImage.gl === undefined || compositeImage.gl == null) {
            return;
        }

        if (!compositeImage.refresh) {
            compositeImage.loopId = requestAnimationFrame(composite_image_rendering_loop);
            return;
        } else
            compositeImage.refresh = false;

        //WebGL how to convert from clip space to pixels
        gl.viewport(Math.round((width - img_width) / 2), Math.round((height - img_height) / 2), Math.round(img_width) - 0, Math.round(img_height) - 0);
        // console.log("gl.viewport:", (width - img_width) / 2, (height - img_height) / 2, img_width, img_height);
        // console.log("gl.viewport:", gl.getParameter(gl.VIEWPORT));
        // set the global variable
        image_gl_viewport = gl.getParameter(gl.VIEWPORT);

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);

        // the image bounding box
        var locationOfBox = gl.getUniformLocation(compositeImage.program, "box");

        // image tone mapping
        var locationOfParamsR = gl.getUniformLocation(compositeImage.program, "params_r");
        var locationOfParamsG = gl.getUniformLocation(compositeImage.program, "params_g");
        var locationOfParamsB = gl.getUniformLocation(compositeImage.program, "params_b");

        // create an array with parameter locations
        var locationOfParams = [locationOfParamsR, locationOfParamsG, locationOfParamsB];

        // drawRegion (execute the GLSL program)
        // Tell WebGL to use our shader program pair
        gl.useProgram(compositeImage.program);

        let xmin = compositeImage.image_bounding_dims.x1 / (compositeImage.width - 0);// was - 1
        let ymin = compositeImage.image_bounding_dims.y1 / (compositeImage.height - 0);// was - 1
        let _width = compositeImage.image_bounding_dims.width / compositeImage.width;
        let _height = compositeImage.image_bounding_dims.height / compositeImage.height;

        // console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
        gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

        for (index = 1; index <= va_count; index++) {
            let tone_mapping = imageContainer[index - 1].tone_mapping;

            // get the multiplier
            let noise_sensitivity = document.getElementById('sensitivity' + index).value;
            let multiplier = get_noise_sensitivity(noise_sensitivity);

            if (tone_mapping.flux == "legacy") {
                let params = [tone_mapping.black, tone_mapping.white, tone_mapping.lmin, tone_mapping.lmax];
                gl.uniform4fv(locationOfParams[index - 1], params);
            } else if (tone_mapping.flux == "ratio") {
                let params = [tone_mapping.median, multiplier * tone_mapping.ratio_sensitivity, tone_mapping.black, tone_mapping.white];
                gl.uniform4fv(locationOfParams[index - 1], params);
            }
            else {
                let params = [tone_mapping.median, multiplier * tone_mapping.sensitivity, tone_mapping.black, tone_mapping.white];
                gl.uniform4fv(locationOfParams[index - 1], params);
            }
        }

        // Setup the attributes to pull data from our buffers
        gl.enableVertexAttribArray(positionLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, compositeImage.positionBuffer);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

        // execute the GLSL program
        // draw the quad (2 triangles, 6 vertices)
        gl.drawArrays(gl.TRIANGLES, 0, 6);

        compositeImage.loopId = requestAnimationFrame(composite_image_rendering_loop);
    };

    compositeImage.loopId = requestAnimationFrame(composite_image_rendering_loop);
}

function webgl_image_renderer(index, gl, width, height) {
    var image = imageContainer[index - 1];
    var image_bounding_dims = image.image_bounding_dims;

    if (zoom_dims != null)
        if (zoom_dims.view != null) {
            image_bounding_dims = zoom_dims.view;
        }

    var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);

    if (va_count > 1) {
        if (va_count == 2)
            scale = 0.8 * scale;
        else if (va_count == 4)
            scale = 0.6 * scale;
        else if (va_count == 5)
            scale = 0.5 * scale;
        else if (va_count == 6)
            scale = 0.45 * scale;
        else if (va_count == 7)
            scale = 0.45 * scale;
        else
            scale = 2 * scale / va_count;
    }

    var img_width = Math.floor(scale * image_bounding_dims.width);
    var img_height = Math.floor(scale * image_bounding_dims.height);
    // console.log("scaling by", scale, "new width:", img_width, "new height:", img_height, "orig. width:", image.image_bounding_dims.width, "orig. height:", image.image_bounding_dims.height);

    var image_position = get_image_position(index, width, height);
    var posx = image_position.posx;
    var posy = height - image_position.posy;
    // console.log("index:", index, "image_position:", image_position);

    // setup GLSL program
    var vertexShaderCode = document.getElementById("vertex-shader").text;
    try {
        var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById(image.tone_mapping.flux + "-shader").text;
    } catch (_) {
        // this will be triggered only for datasets where the tone mapping has not been set (i.e. the mask is null etc...)
        var fragmentShaderCode = document.getElementById("common-shader").text + document.getElementById("legacy-shader").text;
    }

    if (webgl2)
        fragmentShaderCode = fragmentShaderCode + "\ncolour.a = colour.g;\n";

    fragmentShaderCode += document.getElementById(colourmap + "-shader").text;

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    image.program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(image.program, "a_position");

    // Create a position buffer
    image.positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, image.positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // load a texture
    image.tex = gl.createTexture();

    gl.bindTexture(gl.TEXTURE_2D, image.tex);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    /*gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);*/
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    var container = image.viewportContainer;

    if (container == null) {
        if (webgl2)
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RG32F, image.width, image.height, 0, gl.RG, gl.FLOAT, image.texture);
        else
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE_ALPHA, image.width, image.height, 0, gl.LUMINANCE_ALPHA, gl.FLOAT, image.texture);
    } else {
        if (webgl2)
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RG32F, container.width, container.height, 0, gl.RG, gl.FLOAT, container.texture);
        else
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE_ALPHA, container.width, container.height, 0, gl.LUMINANCE_ALPHA, gl.FLOAT, container.texture);
    }

    var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    if (status != gl.FRAMEBUFFER_COMPLETE) {
        console.error(status);
    }

    image.refresh = true;
    image.first = true;

    // shoud be done in an animation loop
    function image_rendering_loop(_timestamp) {
        // set a flag
        image.first = false;

        if (image.gl === undefined || image.gl == null) {
            return;
        }

        if (!image.refresh) {
            image.loopId = requestAnimationFrame(image_rendering_loop);
            return;
        } else
            image.refresh = false;

        //WebGL how to convert from clip space to pixels
        if (va_count == 1)
            gl.viewport(Math.round((width - img_width) / 2), Math.round((height - img_height) / 2), Math.round(img_width) - 0, Math.round(img_height) - 0);
        else
            gl.viewport(Math.round(posx - img_width / 2), Math.round(posy - img_height / 2), Math.round(img_width) - 0, Math.round(img_height) - 0);

        //console.log("gl.viewport:", (width - img_width) / 2, (height - img_height) / 2, img_width, img_height);
        //console.log("gl.viewport:", gl.getParameter(gl.VIEWPORT));
        // set the global variable
        image_gl_viewport = gl.getParameter(gl.VIEWPORT);

        // Clear the canvas
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);

        // the image bounding box
        var locationOfBox = gl.getUniformLocation(image.program, "box");

        // image tone mapping
        var locationOfParams = gl.getUniformLocation(image.program, "params");

        // drawRegion (execute the GLSL program)
        // Tell WebGL to use our shader program pair
        gl.useProgram(image.program);

        // by default show the whole image
        var xmin = image.image_bounding_dims.x1 / (image.width - 0);// was - 1
        var ymin = image.image_bounding_dims.y1 / (image.height - 0);// was - 1
        var _width = image.image_bounding_dims.width / image.width;
        var _height = image.image_bounding_dims.height / image.height;

        if (zoom_dims != null)
            if (zoom_dims.view != null) {
                let view = zoom_dims.view;
                // console.log("view:", view);

                // handle the zoom view
                xmin = view.x1 / (image.width - 0);// was - 1
                ymin = view.y1 / (image.height - 0);// was - 1
                _width = view.width / image.width;
                _height = view.height / image.height;
            }

        if (image.viewportContainer != null) {
            xmin = 0;
            ymin = 0;
            _width = 1;
            _height = 1;
        }

        // console.log("xmin:", xmin, "ymin:", ymin, "_width:", _width, "_height:", _height);
        gl.uniform4fv(locationOfBox, [xmin, ymin, _width, _height]);

        // get the multiplier
        var noise_sensitivity = document.getElementById('sensitivity' + index).value;
        var multiplier = get_noise_sensitivity(noise_sensitivity);

        if (image.tone_mapping.flux == "legacy") {
            var params = [image.tone_mapping.black, image.tone_mapping.white, image.tone_mapping.lmin, image.tone_mapping.lmax];
            gl.uniform4fv(locationOfParams, params);
        } else {
            if (image.tone_mapping.flux == "ratio")
                var params = [image.tone_mapping.median, multiplier * image.tone_mapping.ratio_sensitivity, image.tone_mapping.black, image.tone_mapping.white];
            else
                var params = [image.tone_mapping.median, multiplier * image.tone_mapping.sensitivity, image.tone_mapping.black, image.tone_mapping.white];

            gl.uniform4fv(locationOfParams, params);
        }

        // Setup the attributes to pull data from our buffers
        gl.enableVertexAttribArray(positionLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, image.positionBuffer);
        gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

        // execute the GLSL program
        // draw the quad (2 triangles, 6 vertices)
        gl.drawArrays(gl.TRIANGLES, 0, 6);

        image.loopId = requestAnimationFrame(image_rendering_loop);
    };

    image.loopId = requestAnimationFrame(image_rendering_loop);
}

function process_hdr_video(index) {
    if (!streaming || videoFrame[index - 1] == null || videoFrame[index - 1].rgba == null)
        return;

    if (videoFrame[index - 1].first) {
        // check the first value of rgba for NaN
        if (isNaN(videoFrame[index - 1].rgba[0])) {
            console.log("process_hdr_video: NaN detected, skipping the frame");
            return;
        }

        //clear the VideoCanvas
        {
            if (va_count > 1 && !composite_view) {
                for (let index = 0; index < va_count; index++) {
                    /*var gl = imageContainer[index].gl;

                    if (gl !== undefined && gl != null) {
                        gl.clearColor(0, 0, 0, 0);
                        gl.clear(gl.COLOR_BUFFER_BIT);
                    }*/

                    clear_webgl_image_buffers(index + 1);
                }
            } else {
                /*let gl = null;

                if (!composite_view)
                    gl = imageContainer[va_count - 1].gl;
                else
                    gl = compositeImage.gl;

                if (gl !== undefined && gl != null) {
                    gl.clearColor(0, 0, 0, 0);
                    gl.clear(gl.COLOR_BUFFER_BIT);
                }*/

                if (!composite_view)
                    clear_webgl_image_buffers(va_count);
                else
                    clear_webgl_composite_image_buffers();
            }
        }

        // init the video WebGL renderer
        init_webgl_video_buffers(index);

        if (viewport_zoom_settings != null) {
            // init the video WebGL video viewport (zoom) renderer
            init_webgl_video_viewport_buffers(index);

            videoFrame[index - 1].zoom.first = false;
        }

        videoFrame[index - 1].first = false;
    } else {
        // update the video WebGL texture
        update_webgl_video_texture(index);

        if (viewport_zoom_settings != null) {
            // update the video WebGL video viewport (zoom) texture
            update_webgl_video_viewport_texture(index);
        }
    }

}

function process_progress_event(data, index) {
    if (data != null) {
        var progress = data.progress;
        var elapsed = data.elapsed;

        if (progress > 0) {
            notifications_received[index - 1] = Math.max(progress, notifications_received[index - 1]);

            /*if(running > 0)
            PROGRESS_VARIABLE = running/total ;
            else*/
            var PROGRESS_VARIABLE = progress;

            if (PROGRESS_VARIABLE != previous_progress[index - 1]) {
                previous_progress[index - 1] = PROGRESS_VARIABLE;

                PROGRESS_INFO = "&nbsp;" + numeral(PROGRESS_VARIABLE / 100.0).format('0.0%');

                if (!isNaN(elapsed)) {
                    var speed = notifications_received[index - 1] / elapsed;
                    var remaining_time = (100.0 - notifications_received[index - 1]) / speed;//[s]

                    //console.log("speed:", speed, "remaining:", remaining_time);
                    if (remaining_time > 1)
                        PROGRESS_INFO += ", " + numeral(remaining_time).format('00:00:00');
                }

                d3.select("#progress-bar" + index)
                    .attr("aria-valuenow", (PROGRESS_VARIABLE))
                    .style("width", (PROGRESS_VARIABLE) + "%")
                    .html(PROGRESS_INFO);
            }

            if (progress >= 100.0)
                document.getElementById('welcome').style.display = "none";
        }
        /*else {
          notifications_completed++;

          if (notifications_completed == va_count)
            document.getElementById('welcome').style.display = "none";
        }*/
    }
}

function getEndianness() {
    var a = new ArrayBuffer(4);
    var b = new Uint8Array(a);
    var c = new Uint32Array(a);
    b[0] = 0xa1;
    b[1] = 0xb2;
    b[2] = 0xc3;
    b[3] = 0xd4;
    if (c[0] === 0xd4c3b2a1) {
        return true;//BlobReader.ENDIANNESS.LITTLE_ENDIAN;
    }
    if (c[0] === 0xa1b2c3d4) {
        return false;//BlobReader.ENDIANNESS.BIG_ENDIAN;
    } else {
        throw new Error('Unrecognized endianness');
    }
}

function send_ping() {
    if (wsConn[va_count - 1] != null) {
        t = performance.now();

        try {
            wsConn[va_count - 1].send('[heartbeat] ' + t);
        } catch (e) {
            // console.log(e);
            setTimeout(send_ping, 1000 + ping_latency);
        }
    }
}

function poll_heartbeat() {
    var xmlhttp = new XMLHttpRequest();
    var url = 'heartbeat/' + performance.now();

    xmlhttp.onreadystatechange = function () {
        var RRT = 0;

        if (xmlhttp.readyState == 4 && xmlhttp.status == 404) { };

        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            var data = xmlhttp.response;

            try {
                var previous_t = parseFloat(data);
                ping_latency = (performance.now() - previous_t);

                if (ping_latency > 0) {
                    if (realtime_spectrum) {
                        fps = 1000 / ping_latency;
                        fps = Math.min(30, fps);
                        fps = Math.max(10, fps);
                    }
                    else
                        fps = 30;

                    fpsInterval = 1000 / fps;
                }
            }
            catch (e) { };

            d3.select("#heartbeat")
                .attr("fill", "grey")
                .attr("opacity", 1.0)
                //.text('\ue143');// an empty heart
                //.text('\ue005');// a full heart
                .text('\uf004');// heart
            //.text('📡');
            //.text('📶');

            setTimeout(function () {
                d3.select("#heartbeat")
                    .attr("fill", "grey")
                    .attr("opacity", 1.0)
                    //.text('\ue144');// link
                    //.text('\uf004');// handshake
                    // .text('\uf00c');// check
                    .text('\uf21e');// heartbeat

                setTimeout(function () {
                    d3.select("#heartbeat")
                        .attr("opacity", 0.0);

                    setTimeout(poll_heartbeat, 1000 + RRT);
                }, 500);
            }, 500);
        };

        if (xmlhttp.readyState == 4 && xmlhttp.status == 0) {
            // display an error
            d3.select("#heartbeat")
                .attr("fill", "red")
                .attr("opacity", 1.0)
                .html("&#x274c;");// Cross Mark

            setTimeout(poll_heartbeat, 10000 + RRT);
        }
    }

    xmlhttp.open("POST", url, true);
    xmlhttp.responseType = 'text';
    xmlhttp.timeout = 0;
    xmlhttp.send();
}

function poll_cluster() {
    var xmlhttp = new XMLHttpRequest();
    var url = 'cluster/' + performance.now();

    xmlhttp.onreadystatechange = function () {
        var RRT = 0;

        if (xmlhttp.readyState == 4 && xmlhttp.status == 404) { };

        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            var data = xmlhttp.response;

            try {
                var jsonData = JSON.parse(data);
            } catch (e) {
                console.log(e);
            };

            RTT = performance.now() - jsonData.timestamp;

            // only display cluster information if there is more than one node
            if (jsonData.nodes.length > 1) {
                var clusterStr = "";

                for (var i = 0; i < jsonData.nodes.length; i++) {
                    if (i > 0)
                        clusterStr += "<span>&nbsp;</span>";

                    // ■ or ● or ⎯
                    if (jsonData.nodes[i].status) {
                        // clusterStr += '<span style="color:green;">' + jsonData.nodes[i].node + '</span>';
                        clusterStr += '<span style="color:green;">■</span>';
                    } else {
                        // clusterStr += '<span style="color:red;">' + jsonData.nodes[i].node + '</span>';
                        clusterStr += '<span style="color:red;">■</span>';
                    };
                }

                d3.select("#cluster").html(clusterStr.trim());
            };

            setTimeout(poll_cluster, 1000 + RRT);

            if (xmlhttp.readyState == 4 && xmlhttp.status == 0) {
                setTimeout(poll_cluster, 10000 + RRT);
            }
        }
    }

    xmlhttp.open("POST", url, true);
    xmlhttp.responseType = 'text'; // was json
    xmlhttp.timeout = 0;
    xmlhttp.send();
}

async function poll_progress(datasetId, index) {
    var xmlhttp = new XMLHttpRequest();
    var url = 'progress/' + encodeURIComponent(datasetId);

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 404) {
            console.log("poll_progress: dataset not found.");
        };

        if (xmlhttp.readyState == 4 && xmlhttp.status == 202) {
            console.log("Server not ready, long-polling progress again after 250 ms.");
            setTimeout(function () {
                poll_progress(datasetId, index);
            }, 250);
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            var data = xmlhttp.response;
            // console.log("poll_progress: data:", data, "index:", index);

            try {
                process_progress_event(data, index);

                // if (data.total == 0 || data.running != data.total)
                if (data.progress == 0 || data.progress < 100.0)
                    setTimeout(function () {
                        poll_progress(datasetId, index);
                    }, 250);
            } catch (e) {
                console.log(e, data);
                setTimeout(function () {
                    poll_progress(datasetId, index);
                }, 250);
            }
        }
    }

    xmlhttp.open("POST", url, true);
    xmlhttp.responseType = 'json';
    xmlhttp.timeout = 0;
    xmlhttp.send();
}

function setup_window_timeout() {
    window.clearTimeout(idleWindow); // cancel any previous timeouts

    let timeout = 60 * 60 * 1000; // 1h
    idleWindow = window.setTimeout(show_timeout, timeout);
};

function close_websocket_connections() {
    for (let index = 1; index <= va_count; index++) {
        if (wsConn[index - 1] != null) {
            try {
                wsConn[index - 1].close();
            } catch (_) {
            };

            wsConn[index - 1] = null;
        }
    }

    // clear all timeouts (poll_progress in particular)
    var id = window.setTimeout(function () { }, 0);

    while (id--) {
        window.clearTimeout(id); // will do nothing if no timeout with id is present
    }
}

async function open_websocket_connection(_datasetId, index) {
    if ("WebSocket" in window) {
        // make a unique session id
        var session_id = uuidv4();

        // Let us open a web socket
        var loc = window.location, ws_uri;
        var prot = loc.protocol;

        if (prot !== "https:") {
            ws_uri = "ws://" + loc.hostname + ':' + WS_PORT;
        } else {
            ws_uri = "wss://" + loc.hostname;
        }

        // a JVO override (a special exception)
        if (loc.hostname.indexOf("jvo.") != -1 || loc.hostname.indexOf("jvo-dev.") != -1) {
            ws_uri = "wss://" + loc.hostname;
        }

        ws_uri += ROOT_PATH + "websocket/" + encodeURIComponent(_datasetId) + "/" + session_id;

        //d3.select("#welcome").append("p").text("ws_uri: " + ws_uri) ;

        {
            d3.select("#ping")
                .attr("fill", "orange")
                .attr("opacity", 0.8);

            var ALMAWS = new ReconnectingWebSocket(ws_uri, "", { binaryType: 'arraybuffer' });
            ALMAWS.binaryType = 'arraybuffer';

            ALMAWS.addEventListener("open", function (evt) {
                d3.select("#ping")
                    .attr("fill", "green")
                    .attr("opacity", 0.8);

                ALMAWS.binaryType = 'arraybuffer';

                //let log = wasm_supported ? "WebAssembly is supported" : "WebAssembly is not supported";
                //ALMAWS.send('[debug] ' + log);

                /*var rect = document.getElementById('mainDiv').getBoundingClientRect();
                var width = rect.width - 20;
                var height = rect.height - 20;
                ALMAWS.send('image/' + width + '/' + height);*/

                if (index == va_count) {
                    send_ping();
                }
            });

            ALMAWS.addEventListener("error", function (evt) {

                d3.select("#ping")
                    .attr("fill", "red")
                    .attr("opacity", 0.8);

                d3.select("#latency").text('websocket conn. error');
            });

            ALMAWS.addEventListener("close", function (evt) {
                console.log("websocket conn. closed");

                d3.select("#ping")
                    .attr("fill", "red")
                    .attr("opacity", 0.8);

                d3.select("#latency").text('websocket conn. closed');
            });

            ALMAWS.addEventListener("message", function (evt) {
                var t = performance.now();
                var received_msg = evt.data;

                if (evt.data instanceof ArrayBuffer) {
                    var dv = new DataView(received_msg);

                    latency = performance.now() - dv.getFloat32(0, endianness);
                    // console.log("[ws] latency = " + latency.toFixed(1) + " [ms]");
                    recv_seq_id = dv.getUint32(4, endianness);
                    var type = dv.getUint32(8, endianness);

                    //spectrum
                    if (type == 0) {
                        hide_cursor();
                        computed = dv.getFloat32(12, endianness);

                        var offset = 16;
                        var spectrum_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var frame = new Uint8Array(received_msg, offset);
                        // console.log("computed:", computed, "spectrum length:", spectrum_len, "frame.length:", frame.length);

                        waitForModuleReady().then(() => {
                            // ZFP decoder part
                            Module.ready
                                .then(_ => {
                                    let start = performance.now();
                                    // var spectrum = Module.decompressZFPspectrum(spectrum_len, frame).map((x) => x); // clone an array
                                    var res = Module.decompressZFPspectrum(spectrum_len, frame);
                                    const spectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);
                                    let elapsed = Math.round(performance.now() - start);

                                    // console.log("spectrum size: ", spectrum.length, "elapsed: ", elapsed, "[ms]");

                                    if (spectrum.length > 0) {
                                        if (!windowLeft) {
                                            spectrum_stack[index - 1].push({ spectrum: spectrum, id: recv_seq_id });
                                            //console.log("index:", index, "spectrum_stack length:", spectrum_stack[index - 1].length);
                                        };
                                    }

                                })
                                .catch(e => console.error(e));
                        }).catch(e => console.error(e));

                        //console.log("[ws] computed = " + computed.toFixed(1) + " [ms]" + " length: " + length + " spectrum length:" + spectrum.length + " spectrum: " + spectrum);

                        return;
                    }

                    //viewport
                    if (type == 1) {
                        hide_cursor();
                        var offset = 16;
                        var view_width = dv.getUint32(offset, endianness);
                        offset += 4;

                        var view_height = dv.getUint32(offset, endianness);
                        offset += 4;

                        var pixels_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        //console.log('pixels length:', pixels_length);

                        var frame_pixels = new Uint8Array(received_msg, offset, pixels_length);
                        offset += pixels_length;

                        var mask_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        //console.log('mask length:', mask_length);

                        var frame_mask = new Uint8Array(received_msg, offset, mask_length);
                        offset += mask_length;

                        // WASM decoder part
                        /*Module.ready
                          .then(_ => {*/
                        {
                            //console.log("processing an HDR viewport");
                            let start = performance.now();

                            // decompressZFP returns std::vector<float>
                            // decompressZFPimage returns Float32Array but emscripten::typed_memory_view is buggy
                            var res = Module.decompressZFPimage(view_width, view_height, frame_pixels);
                            const pixels = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                            var res = Module.decompressLZ4mask(view_width, view_height, frame_mask);
                            const alpha = Module.HEAPU8.slice(res[0], res[0] + res[1]);

                            let elapsed = Math.round(performance.now() - start);

                            // console.log("viewport width: ", view_width, "height: ", view_height, "elapsed: ", elapsed, "[ms]");

                            process_hdr_viewport(view_width, view_height, pixels, alpha, index);
                        }
                        /*})
                        .catch(e => console.error(e));*/

                        return;
                    }

                    //image
                    if (type == 2) {
                        var tone_mapping = new Object();
                        let p = 0.5;
                        tone_mapping.lmin = Math.log(p);
                        tone_mapping.lmax = Math.log(p + 1.0);

                        var offset = 12;
                        var str_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        let flux = new Uint8Array(received_msg, offset, str_length);
                        tone_mapping.flux = (new TextDecoder("utf-8").decode(flux)).trim();
                        offset += str_length;

                        tone_mapping.min = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.max = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.median = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.sensitivity = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.ratio_sensitivity = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.white = dv.getFloat32(offset, endianness);
                        offset += 4;

                        tone_mapping.black = dv.getFloat32(offset, endianness);
                        offset += 4;

                        if (tone_mapping.flux == "legacy") {
                            tone_mapping.black = tone_mapping.min;
                            tone_mapping.white = tone_mapping.max;
                        }

                        // console.log(tone_mapping);

                        let currentFlux = document.getElementById('flux' + index).value;

                        if (currentFlux != tone_mapping.flux) {
                            document.getElementById('flux' + index).value = tone_mapping.flux;
                        }

                        if (imageContainer[index - 1] != null) {
                            // re-set the existing tone mapping settings
                            imageContainer[index - 1].tone_mapping = tone_mapping;
                        }

                        // update the tone mapping
                        fitsContainer[index - 1].min = tone_mapping.min;
                        fitsContainer[index - 1].max = tone_mapping.max;
                        fitsContainer[index - 1].median = tone_mapping.median;
                        fitsContainer[index - 1].sensitivity = tone_mapping.sensitivity;
                        fitsContainer[index - 1].ratio_sensitivity = tone_mapping.ratio_sensitivity;
                        fitsContainer[index - 1].black = tone_mapping.black;
                        fitsContainer[index - 1].white = tone_mapping.white;

                        // next receive/process the 32-bit floating-point image frame
                        var img_width = dv.getUint32(offset, endianness);
                        offset += 4;

                        var img_height = dv.getUint32(offset, endianness);
                        offset += 4;

                        //console.log('img_width:', img_width, 'img_height:', img_height);

                        var pixels_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        //console.log('pixels length:', pixels_length);

                        var frame_pixels = new Uint8Array(received_msg, offset, pixels_length);
                        offset += pixels_length;

                        var mask_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        //console.log('mask length:', mask_length);

                        var frame_mask = new Uint8Array(received_msg, offset, mask_length);
                        offset += mask_length;

                        {
                            //console.log("processing an HDR image");
                            let start = performance.now();

                            // decompressZFP returns std::vector<float>
                            // decompressZFPimage returns Float32Array but emscripten::typed_memory_view is buggy
                            var res = Module.decompressZFPimage(img_width, img_height, frame_pixels);
                            const pixels = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                            var res = Module.decompressLZ4mask(img_width, img_height, frame_mask);
                            const alpha = Module.HEAPU8.slice(res[0], res[0] + res[1]);

                            let elapsed = Math.round(performance.now() - start);

                            // console.log("image width: ", img_width, "height: ", img_height, "elapsed: ", elapsed, "[ms]");

                            process_hdr_image(img_width, img_height, pixels, alpha, tone_mapping, index);

                            if (displayContours)
                                update_contours();
                        }

                        let padding = 4 - offset % 4;
                        //console.log("histogram offset:", offset, "padding:", padding);
                        offset += padding;

                        // next the histogram length + bins
                        var nbins = dv.getUint32(offset, endianness);
                        offset += 4;

                        var histogram = new Int32Array(received_msg, offset, nbins);
                        offset += nbins * 4;

                        fitsContainer[index - 1].histogram = histogram;

                        //refresh the histogram
                        redraw_histogram(index);

                        // refresh tone mapping
                        change_tone_mapping(index, true);

                        if (composite_view) {
                            if (image_count == va_count)
                                display_rgb_legend();
                        } else {
                            display_legend();
                        }

                        return;

                        //clear the Video Canvas
                        /*var c = document.getElementById('VideoCanvas') ;
                        var ctx = c.getContext("2d");

                        var width = c.width ;
                        var height = c.height ;

                        ctx.clearRect(0, 0, width, height);*/
                    }

                    //full spectrum refresh
                    if (type == 3) {
                        hide_cursor();
                        hide_hourglass();

                        var offset = 12;
                        var spectrum_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var frame = new Uint8Array(received_msg, offset);

                        waitForModuleReady().then(() => {
                            // ZFP decoder part
                            Module.ready
                                .then(_ => {
                                    // var spectrum = Module.decompressZFPspectrum(spectrum_len, frame).map((x) => x); // clone an array
                                    var res = Module.decompressZFPspectrum(spectrum_len, frame);
                                    const spectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                                    // console.log("spectrum size: ", spectrum.length, spectrum, "elapsed: ", elapsed, "[ms]");

                                    if (spectrum.length > 0) {
                                        // attach the spectrum as either "mean" or "integrated"
                                        //insert a spectrum object to the spectrumContainer at <index-1>

                                        fitsContainer[index - 1].depth = spectrum.length;

                                        if (intensity_mode == "mean") {
                                            fitsContainer[index - 1].mean_spectrum = spectrum;
                                            mean_spectrumContainer[index - 1] = spectrum;
                                        }

                                        if (intensity_mode == "integrated") {
                                            fitsContainer[index - 1].integrated_spectrum = spectrum;
                                            integrated_spectrumContainer[index - 1] = spectrum;
                                        }

                                        spectrum_count++;

                                        if (va_count == 1) {
                                            setup_axes();

                                            plot_spectrum([spectrum]);
                                        }
                                        else {
                                            if (spectrum_count == va_count) {
                                                setup_axes();

                                                if (intensity_mode == "mean")
                                                    plot_spectrum(mean_spectrumContainer);

                                                if (intensity_mode == "integrated")
                                                    plot_spectrum(integrated_spectrumContainer);
                                            }
                                        }
                                    }

                                })
                                .catch(e => console.error(e));
                        }).catch(e => console.error(e));

                        return;
                    }

                    //histogram refresh
                    if (type == 4) {
                        hide_cursor();
                        var min = dv.getFloat32(12, endianness);
                        var max = dv.getFloat32(16, endianness);
                        var black = dv.getFloat32(20, endianness);
                        var white = dv.getFloat32(24, endianness);
                        var median = dv.getFloat32(28, endianness);
                        var sensitivity = dv.getFloat32(32, endianness);
                        var ratio_sensitivity = dv.getFloat32(36, endianness);

                        //console.log("histogram refresh", min, max, median, sensitivity, ratio_sensitivity, black, white);

                        let fitsData = fitsContainer[index - 1];
                        //console.log("min: ", fitsData.min, "-->", min);
                        //console.log("max: ", fitsData.max, "-->", max);
                        //console.log("median: ", fitsData.median, "-->", median);
                        //console.log("sensitivity: ", fitsData.sensitivity, "-->", sensitivity);
                        //console.log("ratio sensitivity: ", fitsData.ratio_sensitivity, "-->", ratio_sensitivity);
                        //console.log("black: ", fitsData.black, "-->", black);
                        //console.log("white: ", fitsData.white, "-->", white);

                        fitsContainer[index - 1].min = min;
                        fitsContainer[index - 1].max = max;
                        fitsContainer[index - 1].median = median;
                        fitsContainer[index - 1].sensitivity = sensitivity;
                        fitsContainer[index - 1].ratio_sensitivity = ratio_sensitivity;
                        fitsContainer[index - 1].black = black;
                        fitsContainer[index - 1].white = white;

                        var nbins = dv.getUint32(40, endianness);
                        var histogram = new Int32Array(received_msg, 44, nbins);
                        fitsContainer[index - 1].histogram = histogram;

                        //console.log("NBINS:", nbins, histogram);

                        //refresh the histogram
                        redraw_histogram(index);

                        return;
                    }

                    //video
                    if (type == 5) {
                        computed = dv.getFloat32(12, endianness);

                        var frame = new Uint8Array(received_msg, 16);

                        var latency = performance.now() - dv.getFloat32(0, endianness);
                        var transfer = (latency - computed) / 1000;//[s]

                        if (transfer > 0) {
                            var bandwidth = (received_msg.byteLength * 8 / 1000) / transfer;//[kilobits per s]

                            //bitrate tracking (variance-tracking Kalman Filter)
                            //eta = (variance - bitrate*bitrate) / (1 + Math.cosh(bitrate));
                            bitrate = (1 - eta) * bitrate + eta * bandwidth;
                            //variance = (1 - eta)*variance + eta * bandwidth*bandwidth;
                            target_bitrate = 0.8 * bitrate;
                        }

                        // console.log("[ws] computed = " + computed.toFixed(1) + " [ms], latency = " + latency.toFixed(1) + "[ms], n/w transfer time = " + (1000 * transfer).toFixed(1) + " [ms], n/w bandwidth = " + Math.round(bandwidth) + " [kbps], frame length:" + frame.length + " bytes, received_msg.byteLength = " + received_msg.byteLength + " bytes");

                        //call the wasm decoder
                        {
                            let start = performance.now();

                            if (streaming && videoFrame[index - 1] != null) {
                                var img = videoFrame[index - 1].img;
                                var data, fill;

                                if (theme == "dark")
                                    fill = 0.0;
                                else
                                    fill = 1.0;

                                try {
                                    // contouring
                                    var contours = 0;

                                    if (displayContours)
                                        contours = parseInt(document.getElementById('contour_lines').value) + 1;

                                    //HEVC
                                    var _colourmap;

                                    if (va_count > 1 && composite_view) {
                                        _colourmap = 'composite';
                                    } else {
                                        _colourmap = colourmap;
                                    }

                                    var res = Module.hevc_decode_frame(videoFrame[index - 1].width, videoFrame[index - 1].height, frame, index - 1, _colourmap, fill, contours);
                                    // data = new Uint8ClampedArray(Module.HEAPU8.subarray(res[0], res[0] + res[1])); // it's OK to use .subarray() instead of .slice() as a copy is made in "new Uint8ClampedArray()"
                                    var rgba = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]); // receive RGBA texture from the WASM module

                                    videoFrame[index - 1].rgba = rgba;

                                    requestAnimationFrame(function () {
                                        process_hdr_video(index)
                                    });
                                } catch (e) {
                                    console.log(e);
                                };

                            }
                            else {
                                try {
                                    //HEVC, ignore the decompressed output, just purge the HEVC buffers
                                    Module.hevc_decode_frame(0, 0, frame, index - 1, 'greyscale', fill, 0);
                                } catch (e) {
                                    // console.log(e);
                                };
                            }

                            let delta = performance.now() - start;

                            //console.log('total decoding/processing/rendering time: ' + delta.toFixed() + ' [ms]');

                            let log = 'video frame length ' + frame.length + ' bytes, decoding/processing/rendering time: ' + delta.toFixed() + ' [ms], bandwidth: ' + Math.round(bandwidth) + " [kbps], request latency: " + latency.toFixed() + ' [ms]';

                            if (video_fps_control == 'auto') {
                                //latency > computed or delta, take the greater
                                if (Math.max(latency, delta) > 0.8 * vidInterval) {
                                    //reduce the video FPS
                                    vidFPS = 0.8 * vidFPS;
                                    vidFPS = Math.max(1, vidFPS);
                                }
                                else {
                                    //increase the video FPS
                                    vidFPS = 1.2 * vidFPS;
                                    vidFPS = Math.min(30, vidFPS);
                                }
                            }

                            log += ' vidFPS = ' + Math.round(vidFPS);

                            if (videoFrame[index - 1] != null)
                                d3.select("#fps").text('video: ' + Math.round(vidFPS) + ' fps, bitrate: ' + Math.round(bitrate) + ' kbps');//, η: ' + eta.toFixed(4) + ' var: ' + variance
                        }

                        return;
                    }

                    // CSV spectrum
                    if (type == 6) {
                        hide_cursor();
                        hide_hourglass();

                        var csv_len = dv.getUint32(12, endianness);
                        var csv_frame = new Uint8Array(received_msg, 16);

                        // decompress CSV
                        var LZ4 = require('lz4');

                        var uncompressed = new Uint8Array(csv_len);
                        uncompressedSize = LZ4.decodeBlock(csv_frame, uncompressed);
                        uncompressed = uncompressed.slice(0, uncompressedSize);

                        try {
                            var csv = new TextDecoder().decode(uncompressed);

                            // prepend the UTF-8 Byte Order Mark (BOM) 0xEF,0xBB,0xBF
                            var blob = new Blob([new Uint8Array([0xEF, 0xBB, 0xBF]), csv], { type: "data:text/csv;charset=utf-8" });

                            var filename;

                            if (va_count == 1) {
                                filename = datasetId + ".csv";
                            } else {
                                filename = datasetId[index - 1] + ".csv";
                            };

                            saveAs(blob, filename.replace('/', '_'));
                        }
                        catch (err) {
                            console.error(err);
                        };

                        return;
                    }

                    // P-V Diagram
                    if (type == 7) {
                        d3.select("#hourglassPVDiagram").remove();

                        d3.select("#pvline")
                            .attr("x1", 0)
                            .attr("y1", 0)
                            .attr("x2", 0)
                            .attr("y2", 0)
                            .attr("opacity", 0.0);

                        d3.select("#pvmid")
                            .attr("x1", 0)
                            .attr("y1", 0)
                            .attr("x2", 0)
                            .attr("y2", 0)
                            .attr("opacity", 0.0);

                        pv_latency = latency;

                        var offset = 12;
                        var str_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        let str = new Uint8Array(received_msg, offset, str_length);
                        let id = (new TextDecoder("utf-8").decode(str)).trim();
                        offset += str_length;

                        let pmin = new Float32Array(received_msg, offset, va_count);
                        offset += 4 * va_count;

                        let pmax = new Float32Array(received_msg, offset, va_count);
                        offset += 4 * va_count;

                        let pmean = new Float32Array(received_msg, offset, va_count);
                        offset += 4 * va_count;

                        let pstd = new Float32Array(received_msg, offset, va_count);
                        offset += 4 * va_count;

                        let vmin = dv.getFloat64(offset, endianness);
                        offset += 8;

                        let vmax = dv.getFloat64(offset, endianness);
                        offset += 8;

                        let pvx1 = dv.getUint32(offset, endianness) - 1; // -1 to convert from 1-based to 0-based
                        offset += 4;

                        let pvy1 = dv.getUint32(offset, endianness) - 1; // -1 to convert from 1-based to 0-based
                        offset += 4;

                        let pvx2 = dv.getUint32(offset, endianness) - 1; // -1 to convert from 1-based to 0-based
                        offset += 4;

                        let pvy2 = dv.getUint32(offset, endianness) - 1; // -1 to convert from 1-based to 0-based
                        offset += 4;

                        console.log("P-V Diagram: ", id, pmin, pmax, pmean, pstd, vmin, vmax, pvx1, pvy1, pvx2, pvy2);

                        var pv_width = dv.getUint32(offset, endianness);
                        offset += 4;

                        var pv_height = dv.getUint32(offset, endianness);
                        offset += 4;

                        var pv_length = dv.getUint32(offset, endianness);
                        offset += 4;

                        console.log("P-V Diagram: ", pv_width, pv_height, pv_length);

                        var frame_pv = new Uint8Array(received_msg, offset, pv_length);
                        offset += pixels_length;

                        console.log("P-V Diagram: frame_pv.length = ", frame_pv.length);

                        if (id == "ZFP") {
                            // decompress ZFP
                            if (va_count == 1) {
                                var res = Module.decompressPVdiagram(pv_width, pv_height, frame_pv);
                            } else {
                                var res = Module.decompressCompositePVdiagram(pv_width, pv_height, va_count, frame_pv);
                            }

                            const pv = new Uint8ClampedArray(Module.HEAPU8.subarray(res[0], res[0] + res[1])); // it's OK to use .subarray() instead of .slice() as a copy is made in "new Uint8ClampedArray()"

                            var pvData = new ImageData(pv, pv_width, pv_height);

                            let pvCanvas = document.createElement('canvas');
                            pvCanvas.style.visibility = "hidden";
                            var context = pvCanvas.getContext('2d');

                            pvCanvas.width = pv_width;
                            pvCanvas.height = pv_height;
                            context.putImageData(pvData, 0, 0);

                            //place the image onto the PV canvas
                            var c = document.getElementById('PVCanvas');
                            var c2 = document.getElementById('PVCanvas2');
                            var dst_width = c.width / 2;
                            var dst_height = c.height;
                            var offset = 0.05 * c.height;

                            // scale with preserving the aspect ratio
                            /*var scale = get_pv_image_scale(dst_width, dst_height, pv_width, pv_height);
                            var img_width = scale * pv_width;
                            var img_height = scale * pv_height;*/

                            // stretch the image to fill the right half of the canvas (do not preserve the aspect ratio)
                            var img_width = 0.8 * dst_width;
                            var img_height = 0.8 * dst_height;

                            var ctx = c.getContext("2d");
                            ctx.webkitImageSmoothingEnabled = false;
                            ctx.msImageSmoothingEnabled = false;
                            ctx.imageSmoothingEnabled = false;

                            var ctx2 = c2.getContext("2d");
                            ctx2.webkitImageSmoothingEnabled = false;
                            ctx2.msImageSmoothingEnabled = false;
                            ctx2.imageSmoothingEnabled = false;

                            // clear the right half of the PV canvas
                            ctx.clearRect(c.width / 2, 0, c.width, c.height);
                            ctx2.clearRect(c2.width / 2, 0, c2.width, c2.height);

                            // save the current transformation matrix
                            ctx.save();
                            ctx2.save();

                            var flipY = false;

                            if (vmin < vmax) {
                                // flip both the X and Y axes
                                ctx.translate(c.width, c.height + offset);
                                ctx.scale(-1, -1); // X was -1

                                ctx2.translate(c.width, c.height + offset);
                                ctx2.scale(-1, -1); // X was -1

                                flipY = true;
                            } else {
                                // flip the X axis only
                                ctx.translate(c.width, offset);
                                ctx.scale(-1, 1); // X was -1

                                ctx2.translate(c.width, offset);
                                ctx2.scale(-1, 1); // X was -1

                                // swap vmin and vmax
                                let tmp = vmin;
                                vmin = vmax;
                                vmax = tmp;
                            }

                            // then place the new PV diagram onto the ctx canvas
                            // ctx.drawImage(pvCanvas, 0, 0, pv_width, pv_height, 3 * dst_width / 2 - img_width / 2, (dst_height - img_height) / 2, img_width, img_height); // normal X axis
                            ctx2.drawImage(pvCanvas, 0, 0, pv_width, pv_height, (dst_width - img_width) / 2, (dst_height - img_height) / 2, img_width, img_height); // the X axis has been reversed

                            // set the alpha channel to 255 for pvData
                            var data = pvData.data;

                            for (var i = 0; i < data.length; i += 4) {
                                data[i + 3] = 255;
                            }

                            context.putImageData(pvData, 0, 0);

                            // finally draw the PV diagram onto the visible PVCanvas
                            ctx.drawImage(pvCanvas, 0, 0, pv_width, pv_height, (dst_width - img_width) / 2, (dst_height - img_height) / 2, img_width, img_height); // the X axis has been reversed

                            // restore the transformation matrix
                            ctx.restore();
                            ctx2.restore();

                            if (va_count == 1) {
                                pv_axes(3 * dst_width / 2 - img_width / 2, offset + (dst_height - img_height) / 2, img_width, img_height, vmin, vmax, pmin[0], pmax[0], pmean[0], pstd[0], pvx1, pvy1, pvx2, pvy2);
                            } else {
                                composite_pv_axes(3 * dst_width / 2 - img_width / 2, offset + (dst_height - img_height) / 2, img_width, img_height, vmin, vmax, pmin, pmax, pmean, pstd, pvx1, pvy1, pvx2, pvy2);
                            }

                            // cancel idlePV timer and set a new one
                            window.clearTimeout(idlePV);
                            idlePV = window.setTimeout(function () {
                                pv_contour(3 * dst_width / 2 - img_width / 2, offset + (dst_height - img_height) / 2, img_width, img_height, pvCanvas, flipY, pv_width, pv_height, frame_pv);
                            }, 250);
                        }

                        return;
                    }

                    // HDS X-Y spectra
                    if (type == 8) {
                        hide_cursor();
                        computed = dv.getFloat32(12, endianness);

                        var offset = 16;
                        var xlen = dv.getUint32(offset, endianness);
                        offset += 4;

                        var xcomp_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var xcomp = new Uint8Array(received_msg, offset, xcomp_len);
                        offset += xcomp_len;

                        var xmask_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var xframe = new Uint8Array(received_msg, offset, xmask_len);
                        offset += xmask_len;

                        var ylen = dv.getUint32(offset, endianness);
                        offset += 4;

                        var ycomp_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var ycomp = new Uint8Array(received_msg, offset, ycomp_len);
                        offset += ycomp_len;

                        var ymask_len = dv.getUint32(offset, endianness);
                        offset += 4;

                        var yframe = new Uint8Array(received_msg, offset, ymask_len);
                        offset += ymask_len;

                        // X : uint32
                        // Y : uint32
                        // THETA : float [rad]
                        var X = dv.getUint32(offset, endianness);
                        offset += 4;

                        var Y = dv.getUint32(offset, endianness);
                        offset += 4;

                        var THETA = dv.getFloat32(offset, endianness);
                        offset += 4;

                        /*console.log("HDS X-Y spectra: xlen:", xlen, "ylen:", ylen);
                        console.log("HDS X-Y spectra: xcomp_len:", xcomp_len, "xmask_len:", xmask_len, "ycomp_len:", ycomp_len, "ymask_len:", ymask_len);
                        console.log("HDS X-Y spectra: X:", X, "Y:", Y, "THETA:", THETA);*/

                        waitForModuleReady().then(() => {
                            // ZFP decoder part
                            Module.ready
                                .then(_ => {
                                    //console.log("processing HDS X-Y spectra");
                                    let start = performance.now();

                                    var res = Module.decompressZFPspectrum(xlen, xcomp);
                                    const xspectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                                    var res = Module.decompressLZ4mask(xlen, 1, xframe);
                                    const xmask = Module.HEAPU8.slice(res[0], res[0] + res[1]);

                                    // console.log("HDS X-Y spectra: xspectrum:", xspectrum, "xmask:", xmask);

                                    var res = Module.decompressZFPspectrum(ylen, ycomp);
                                    const yspectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                                    var res = Module.decompressLZ4mask(ylen, 1, yframe);
                                    const ymask = Module.HEAPU8.slice(res[0], res[0] + res[1]);

                                    // console.log("HDS X-Y spectra: yspectrum:", yspectrum, "ymask:", ymask);

                                    let elapsed = Math.round(performance.now() - start);
                                    // console.log("HDS X-Y spectra: elapsed: ", elapsed, "[ms]");

                                    if (!windowLeft) {
                                        spectrum_stack[index - 1].push({ xspectrum: xspectrum, xmask: xmask, yspectrum: yspectrum, ymask: ymask, x0: X - 1, y0: Y - 1, angle: THETA, id: recv_seq_id }); // -1 to convert from 1-based to 0-based
                                    };
                                })
                                .catch(e => console.error(e));
                        }).catch(e => console.error(e));

                        return;
                    }

                }

                if (typeof evt.data === "string") {
                    var cmd = "[close]";
                    var pos = received_msg.indexOf(cmd);

                    if (pos >= 0) {
                        if (ALMAWS != null)
                            ALMAWS.close();

                        d3.select("#ping")
                            .attr("fill", "red")
                            .attr("opacity", 0.8);

                        d3.select("#latency").text('60 min. inactive session time-out');

                        show_timeout();

                        return;
                    }
                }

                if (typeof evt.data === "string") {
                    var cmd = "[heartbeat]";
                    var pos = received_msg.indexOf(cmd);

                    if (pos >= 0) {
                        setTimeout(send_ping, 1000 + ping_latency);

                        var previous_t = parseFloat(received_msg.substring(pos + cmd.length));

                        ping_latency = (t - previous_t);

                        if (ping_latency > 0) {
                            if (realtime_spectrum) {
                                fps = 1000 / ping_latency;
                                fps = Math.min(30, fps);
                                fps = Math.max(10, fps);
                            }
                            else
                                fps = 30;

                            fpsInterval = 1000 / fps;
                        }

                        //console.log("ping latency = " + ping_latency.toFixed(1) + " [ms]" + ' fps: ' + fps.toFixed()) ;

                        /*d3.select("#ping")
                          .attr("fill", "green")
                          .attr("opacity", 1.0);*/
                        /*.transition()
                        .duration(250)
                        .attr("opacity", 0.0);*/

                        if (ping_latency >= 1)
                            d3.select("#latency").text(`${ping_latency.toFixed()} ms ${fps.toFixed()} fps`);
                        else
                            d3.select("#latency").text(`${ping_latency.toFixed(1)} ms ${fps.toFixed()} fps`);

                        return;
                    }

                    try {
                        var data = JSON.parse(received_msg);

                        if (data.type == "progress")
                            process_progress_event(data, index);

                        if (data.type == "caching") {
                            console.log(data);
                            show_cursor();
                        }

                        if (data.type == "cached") {
                            console.log(data);
                            hide_cursor();
                        }

                        if (data.type == "init_video") {
                            //console.log(data);

                            var width = data.width;
                            var height = data.height;

                            if (videoFrame[index - 1] == null) {
                                let imageFrame = imageContainer[va_count - 1];

                                if (imageFrame != null) {
                                    let tmp = imageFrame.image_bounding_dims;
                                    let dims = { x1: tmp.x1, y1: tmp.y1, width: tmp.width, height: tmp.height };

                                    videoFrame[index - 1] = {
                                        first: true,
                                        width: width,
                                        height: height,
                                        image_bounding_dims: dims,
                                        zoom: { first: true },
                                        //image_bounding_dims: imageFrame.image_bounding_dims,
                                        //image_bounding_dims: {x1: 0, y1: 0, width: width, height: height},
                                    }
                                }
                            }

                            try {
                                //init the HEVC decoder
                                Module.hevc_init_frame(va_count, width, height);
                            } catch (e) {
                                //console.log(e);
                            };

                            // hide the contour plot
                            if (displayContours)
                                document.getElementById("ContourSVG").style.display = "none";
                        }

                        return;
                    }
                    catch (e) {
                        console.error(received_msg, e);
                    }
                }
            })

            wsConn[index - 1] = ALMAWS;
        }
    }
    else {
        d3.select("#welcome").append("p").text("LOADING IMAGE...");

        // The browser doesn't support WebSocket
        alert("WebSocket NOT supported by your Browser, progress updates disabled.");
    }
}

function image_pixel_range(pixels, mask, width, height) {
    var min_pixel = Number.MAX_VALUE;
    var max_pixel = -Number.MAX_VALUE;

    let plane_size = width * height;

    for (let i = 0; i < plane_size; i++) {
        if (mask[i] > 0) {
            let pixel = pixels[i]; // Float32Array
            // let pixel = pixels.get(i); // Float()

            if (pixel > max_pixel)
                max_pixel = pixel;

            if (pixel < min_pixel)
                min_pixel = pixel;
        }
    };

    return { min_pixel: min_pixel, max_pixel: max_pixel };
}

function true_image_dimensions(alpha, width, height) {
    var width = width | 0;
    var height = height | 0;
    var linesize = width | 0;
    var length = (width * height) | 0;

    var x, y, offset;
    var found_data;

    var y1 = 0 | 0;
    var y2 = 0 | 0;
    var x1 = 0 | 0;
    var x2 = 0 | 0;

    //find y1
    for (var i = 0 | 0; i < length; i = (i + 1) | 0) {
        if (alpha[i] > 0) {
            y1 = (i / linesize) | 0;
            break;
        }
    }

    //find y2
    for (var i = length - 1; i >= 0; i = (i - 1) | 0) {
        if (alpha[i] > 0) {
            y2 = (i / linesize) | 0;
            break;
        }
    }

    //find x1
    found_data = false;
    for (var x = 0 | 0; x < width; x = (x + 1) | 0) {
        for (var y = y1; y <= y2; y = (y + 1) | 0) {
            if (alpha[y * linesize + x] > 0) {
                x1 = x | 0;
                found_data = true;
                break;
            }
        }

        if (found_data)
            break;
    }

    //find x2
    found_data = false;
    for (var x = (width - 1) | 0; x >= 0; x = (x - 1) | 0) {
        for (var y = y1; y <= y2; y = (y + 1) | 0) {
            if (alpha[y * linesize + x] > 0) {
                x2 = x | 0;
                found_data = true;
                break;
            }
        }

        if (found_data)
            break;
    }

    //console.log("image bounding box: y1 =", y1, "y2 =", y2, "x1 =", x1, "x2 =", x2);

    return {
        x1: x1,
        y1: y1,
        x2: x2,
        y2: ((height - 1) - y2), // was 'y1', with WebGL swap y1 with y2 due to a vertical mirror flip
        width: Math.abs(x2 - x1) + 1,
        height: Math.abs(y2 - y1) + 1
    }
}

function display_hourglass() {
    var c = document.getElementById('HTMLCanvas');
    var width = c.width;
    var height = c.height;

    //hourglass
    /*var img_width = 200 ;
    var img_height = 200 ;*/

    //squares
    var img_width = 128;
    var img_height = 128;

    d3.select('#FrontSVG').append("svg:image")
        .attr("id", "hourglass")
        .attr("x", (width - img_width) / 2)
        .attr("y", (height - img_height) / 2)
        //.attr("xlink:href", ROOT_PATH + "loading.gif")
        .attr("xlink:href", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/loading.gif")
        .attr("width", img_width)
        .attr("height", img_height)
        .attr("opacity", 1.0);
}

function hide_hourglass() {
    try {
        d3.selectAll('#hourglass').remove();
    }
    catch (e) { };
}

function copy_coordinates(e) {
    var textToPutOnClipboard = d3.select("#ra").text() + " " + d3.select("#dec").text();

    navigator.clipboard.writeText(textToPutOnClipboard).then(function () {
        console.log('Async: Copying to clipboard was successful!');
    }, function (err) {
        console.error('Async: Could not copy text: ', err);
    });

    e.preventDefault();
}

// Returns the inverse of matrix `M`.
function matrix_invert(M) {
    // I use Guassian Elimination to calculate the inverse:
    // (1) 'augment' the matrix (left) by the identity (on the right)
    // (2) Turn the matrix on the left into the identity by elemetry row ops
    // (3) The matrix on the right is the inverse (was the identity matrix)
    // There are 3 elemtary row ops: (I combine b and c in my code)
    // (a) Swap 2 rows
    // (b) Multiply a row by a scalar
    // (c) Add 2 rows

    //if the matrix isn't square: exit (error)
    if (M.length !== M[0].length) { return; }

    //create the identity matrix (I), and a copy (C) of the original
    var i = 0, ii = 0, j = 0, dim = M.length, e = 0, t = 0;
    var I = [], C = [];
    for (i = 0; i < dim; i += 1) {
        // Create the row
        I[I.length] = [];
        C[C.length] = [];
        for (j = 0; j < dim; j += 1) {

            //if we're on the diagonal, put a 1 (for identity)
            if (i == j) { I[i][j] = 1; }
            else { I[i][j] = 0; }

            // Also, make the copy of the original
            C[i][j] = M[i][j];
        }
    }

    // Perform elementary row operations
    for (i = 0; i < dim; i += 1) {
        // get the element e on the diagonal
        e = C[i][i];

        // if we have a 0 on the diagonal (we'll need to swap with a lower row)
        if (e == 0) {
            //look through every row below the i'th row
            for (ii = i + 1; ii < dim; ii += 1) {
                //if the ii'th row has a non-0 in the i'th col
                if (C[ii][i] != 0) {
                    //it would make the diagonal have a non-0 so swap it
                    for (j = 0; j < dim; j++) {
                        e = C[i][j];       //temp store i'th row
                        C[i][j] = C[ii][j];//replace i'th row by ii'th
                        C[ii][j] = e;      //repace ii'th by temp
                        e = I[i][j];       //temp store i'th row
                        I[i][j] = I[ii][j];//replace i'th row by ii'th
                        I[ii][j] = e;      //repace ii'th by temp
                    }
                    //don't bother checking other rows since we've swapped
                    break;
                }
            }
            //get the new diagonal
            e = C[i][i];
            //if it's still 0, not invertable (error)
            if (e == 0) { return }
        }

        // Scale this row down by e (so we have a 1 on the diagonal)
        for (j = 0; j < dim; j++) {
            C[i][j] = C[i][j] / e; //apply to original matrix
            I[i][j] = I[i][j] / e; //apply to identity
        }

        // Subtract this row (scaled appropriately for each row) from ALL of
        // the other rows so that there will be 0's in this column in the
        // rows above and below this one
        for (ii = 0; ii < dim; ii++) {
            // Only apply to other rows (we want a 1 on the diagonal)
            if (ii == i) { continue; }

            // We want to change this element to 0
            e = C[ii][i];

            // Subtract (the row above(or below) scaled by e) from (the
            // current row) but start at the i'th column and assume all the
            // stuff left of diagonal is 0 (which it should be if we made this
            // algorithm correctly)
            for (j = 0; j < dim; j++) {
                C[ii][j] -= e * C[i][j]; //apply to original matrix
                I[ii][j] -= e * I[i][j]; //apply to identity
            }
        }
    }

    //we've done all operations, C should be the identity
    //matrix I should be the inverse:
    return I;
}

function inverse_CD_matrix(arcx, arcy) {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    //convert from arc seconds to radians
    var dx = (arcx / 86400.0) * 2 * pi;//[s]
    var dy = (arcy / 3600.0) / toDegrees;//["]

    //convert to radians
    var CRVAL1 = fitsData.CRVAL1 / toDegrees;
    var CRVAL2 = fitsData.CRVAL2 / toDegrees;

    var RA = CRVAL1 + dx;
    var DEC = CRVAL2 + dy;

    //console.log(RadiansPrintHMS(CRVAL1), RadiansPrintHMS(RA)) ;
    //console.log(RadiansPrintDMS(CRVAL2), RadiansPrintDMS(DEC)) ;

    var y = (1 - Math.tan(CRVAL2) * Math.cos(dx) / Math.tan(DEC)) / (Math.tan(CRVAL2) + Math.cos(dx) / Math.tan(DEC));
    var x = Math.tan(dx) * Math.cos(CRVAL2) * (1 - y * Math.tan(CRVAL2));

    //convert from radians to degrees
    x = x * toDegrees;
    y = y * toDegrees;

    //console.log("inverse: x = ", x, "y = ", y);

    var CD1_1 = fitsData.CD1_1;
    var CD1_2 = fitsData.CD1_2;
    var CD2_1 = fitsData.CD2_1;
    var CD2_2 = fitsData.CD2_2;

    //convert the North/East rotation angle from radians to degrees
    var theta = Math.atan(CD1_2 / CD1_1) * toDegrees;

    var M = [[CD1_1, CD1_2], [CD2_1, CD2_2]];
    var invM = matrix_invert(M);

    var DC1_1 = invM[0][0];
    var DC1_2 = invM[0][1];
    var DC2_1 = invM[1][0];
    var DC2_2 = invM[1][1];

    var DX = DC1_1 * x + DC1_2 * y;
    var DY = DC2_1 * x + DC2_2 * y;

    //DX: assume no change in y
    DX = DC1_1 * x;
    //DY: assume no change in x
    DY = DC2_2 * y;

    var gridScale = new Array(DX / fitsData.width, Math.sign(CD2_2) * Math.abs(DY) / fitsData.height, theta);

    return gridScale;
}

function spoint_vector3d(lng, lat) {
    var x = Math.cos(lat) * Math.cos(lng);
    var y = Math.cos(lat) * Math.sin(lng);
    var z = Math.sin(lat);

    //return new Array(x, y, z);
    return { x: x, y: y, z: z };
}

function vector3d_cross(v1, v2) {
    var x = v1.y * v2.z - v1.z * v2.y;
    var y = v1.z * v2.x - v1.x * v2.z;
    var z = v1.x * v2.y - v1.y * v2.x;

    //return new Array(x, y, z);
    return { x: x, y: y, z: z };
}

function vector3d_length(v) {
    return Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
}

function FPeq(a, b) {
    return Math.abs(a - b) < 1e-6;
}

function AngularDistance(p1lng, p1lat, p2lng, p2lat) {
    var dl = p1lng - p2lng;
    var f = ((Math.sin(p1lat) * Math.sin(p2lat) + Math.cos(p1lat) * Math.cos(p2lat) * Math.cos(dl)));
    console.log("f = ", f);

    if (FPeq(f, 1.0)) {
        // for small distances
        console.log("small angular distance");
        v1 = spoint_vector3d(p1lng, p1lat);
        v2 = spoint_vector3d(p2lng, p2lat);
        v3 = vector3d_cross(v1, v2);
        return vector3d_length(v3);
    }
    else {
        // for normal distances
        console.log("normal angular distance");
        return Math.acos(f);
    }
}

function HaversineDistance(ra1, dec1, ra2, dec2) {
    var dRA = ra2 - ra1;
    var dDEC = dec2 - dec1;

    console.log("dRA=", dRA, "dDEC=", dDEC);

    var a = Math.sin(dDEC / 2) * Math.sin(dDEC / 2) + Math.cos(dec1) * Math.cos(dec2) * Math.sin(dRA / 2) * Math.sin(dRA / 2);
    var b = 2 * Math.asin(Math.sqrt(a)); // works well for small angles (where a is near 0)
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a)); // suggested by the GitHub Copilot

    var d = AngularDistance(ra1, dec1, ra2, dec2);

    console.log("a=", a, "b=", b, "c=", c, "d=", d);

    return d;
}

function display_scale_info() {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    if (fitsData.is_spectrum)
        return;

    // add the markers anyway (they are needed by the P-V diagram)
    var svg = d3.select("#BackgroundSVG");
    var width = parseFloat(svg.attr("width"));
    var defs = svg.append("defs");

    defs.append("marker")
        .attr("id", "head")
        .attr("orient", "auto")
        .attr("markerWidth", (emStrokeWidth))
        .attr("markerHeight", (0.5 * emFontSize))
        .attr("refX", 0)
        .attr("refY", (0.5 * emFontSize / 2))
        .append("path")
        .style("stroke-width", 1)
        .attr("d", "M0,0 V" + 0.5 * emFontSize);

    defs.append("marker")
        .attr("id", "arrow")
        .attr("viewBox", "0 -5 10 10")
        .attr("refX", 5)
        .attr("refY", 0)
        .attr("markerWidth", 0.67 * emFontSize)
        .attr("markerHeight", 0.67 * emFontSize)
        .attr("orient", "auto")
        .append("path")
        .style("stroke-width", 1)
        .style("fill", "none")
        .attr("d", "M-5,-5 L5,0 L-5,5");

    if (fitsData.depth > 1)
        return;

    var elem = document.getElementById("image_rectangle");
    if (elem == null)
        return;

    var img_width = parseFloat(elem.getAttribute("width"));
    var img_height = parseFloat(elem.getAttribute("height"));
    var img_x = parseFloat(elem.getAttribute("x"));
    var img_y = parseFloat(elem.getAttribute("y"));

    var image = imageContainer[va_count - 1];
    var image_bounding_dims = image.image_bounding_dims;
    var scale = image.height / image_bounding_dims.height;

    //scale
    var arcmins = 60;
    var gridScale = inverse_CD_matrix(arcmins, arcmins);

    for (let i = 0; i < gridScale.length; i++)
        if (isNaN(gridScale[i]))
            throw "NaN gridScale";

    if (Math.abs(gridScale[1]) * scale > 1) {
        //reduce the scale
        //console.log("Vertical height:", Math.abs(gridScale[1]) * scale);

        arcmins = 10;
        gridScale = inverse_CD_matrix(arcmins, arcmins);

        for (let i = 0; i < gridScale.length; i++)
            if (isNaN(gridScale[i]))
                throw "NaN gridScale";

        //console.log("Reduced vertical height:", Math.abs(gridScale[1]) * scale);
    }

    //vertical scale
    var L = Math.abs(gridScale[1]) * scale * img_height;
    var X = 1 * emFontSize;
    if (composite_view)
        X += img_x + img_width;
    //var Y = L + img_y;//1.75 * emFontSize;
    var Y = img_y + img_height;

    var vert = svg.append("g")
        .attr("id", "verticalScale");

    vert.append("path")
        .attr("marker-end", "url(#head)")
        .attr("marker-start", "url(#head)")
        .style("stroke-width", (emStrokeWidth))
        .style("fill", "none")
        .attr("d", "M" + X + "," + Y + " L" + X + "," + (Y - L));

    vert.append("text")
        .attr("x", (X + emFontSize))
        .attr("y", (Y - L / 2 + emFontSize / 3))
        .attr("font-family", "Monospace")
        .attr("font-size", "1.0em")
        .attr("text-anchor", "middle")
        .attr("stroke", "none")
        .text(arcmins + "\"");

    //N-E compass
    var L = 3 * emFontSize;//*Math.sign(gridScale[0]) ;
    var X = 0.02 * width + L + 1.5 * emFontSize;
    var Y = Y - L / 2;
    if (composite_view)
        X += img_x + img_width;
    //var Y = 0.01*width + L + emFontSize;
    //var Y = L + img_y;//Math.max(Y - 1.5 * emFontSize, 0.01 * width + L + emFontSize);

    //rotation
    var compass = svg.append("g")
        .attr("id", "compass")
        .attr("transform", 'rotate(' + gridScale[2] * Math.sign(gridScale[0]) + ' ' + X + ' ' + Y + ')');

    var east = compass.append("g")
        .attr("id", "east");

    east.append("path")
        .attr("marker-end", "url(#arrow)")
        .style("stroke-width", (emStrokeWidth))
        .style("fill", "none")
        .attr("d", "M" + X + "," + Y + " L" + (X + L * Math.sign(gridScale[0])) + "," + Y);

    east.append("text")
        .attr("x", (X + L * Math.sign(gridScale[0]) + Math.sign(gridScale[0]) * emFontSize / 2))
        .attr("y", (Y + emFontSize / 2.5))
        .attr("font-family", "Monospace")
        .attr("font-size", "1.0em")
        .attr("text-anchor", "middle")
        .attr("stroke", "none")
        .text("E");

    var north = compass.append("g")
        .attr("id", "north");

    L *= Math.sign(gridScale[1]);

    north.append("path")
        .attr("marker-end", "url(#arrow)")
        .style("stroke-width", (emStrokeWidth))
        .style("fill", "none")
        .attr("d", "M" + X + "," + Y + " L" + X + "," + (Y - L));

    if (L > 0)
        north.append("text")
            .attr("x", (X))
            .attr("y", (Y - L - emFontSize / 4))
            .attr("font-family", "Monospace")
            .attr("font-size", "1.1em")
            .attr("text-anchor", "middle")
            .attr("stroke", "none")
            .text("N");
    else
        north.append("text")
            .attr("x", (X))
            .attr("y", (Y - L + emFontSize))
            .attr("font-family", "Monospace")
            .attr("font-size", "1.0em")
            .attr("text-anchor", "middle")
            .attr("stroke", "none")
            .text("N");
}


function display_gridlines() {
    if (va_count > 1 && !composite_view)
        return;

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    if (!has_image)
        return;

    try {
        d3.select("#gridlines").remove();
    }
    catch (e) {
    }

    var elem = d3.select("#image_rectangle");
    var width = parseFloat(elem.attr("width"));
    var height = parseFloat(elem.attr("height"));

    var x_offset = parseFloat(elem.attr("x"));
    var y_offset = parseFloat(elem.attr("y"));

    var x = d3.scaleLinear()
        .range([x_offset, x_offset + width])
        .domain([0, 1]);

    var y = d3.scaleLinear()
        .range([y_offset + height, y_offset])
        .domain([0, 1]);

    var svg = d3.select("#BackgroundSVG");

    svg = svg.append("g")
        .attr("id", "gridlines")
        .attr("opacity", 1.0);

    let fillColour = 'white';
    let strokeColour = 'white';

    if (theme == 'bright') {
        fillColour = 'gray';
        strokeColour = 'gray';
    }

    if (colourmap == "greyscale" || colourmap == "negative") {
        fillColour = "#C4A000";
        strokeColour = fillColour;
    }

    // Add the X Axis
    if (fitsData.depth > 1) {
        var xAxis = d3.axisBottom(x)
            .tickSize(height)
            .tickFormat(function (d) {
                if (d == 0.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var tmp, orig_x, orig_y;

                tmp = image_bounding_dims.x1 + d * (image_bounding_dims.width - 1);
                orig_x = tmp * fitsData.width / image.width;
                tmp = image_bounding_dims.y1 + (image_bounding_dims.width - 1);
                orig_y = tmp * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        return RadiansPrintDMS(radec[0]);
                    else
                        return RadiansPrintHMS(radec[0]);
                } else {
                    return RadiansPrintDMS(radec[0]);
                }
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "ra_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", "translate(0," + (y_offset) + ")")
            .call(xAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            .attr("dx", "-1.0em")
            .attr("dy", "1.0em")
            .attr("transform", "rotate(-45)")
            .style("text-anchor", "middle");
    }
    else {
        var xAxis = d3.axisTop(x)
            .tickSize(height)
            .tickFormat(function (d) {
                if (d == 0.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var tmp = image_bounding_dims.x1 + d * (image_bounding_dims.width - 1);
                var orig_x = tmp * fitsData.width / image.width;
                var orig_y = image_bounding_dims.y1 * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        return RadiansPrintDMS(radec[0]);
                    else
                        return RadiansPrintHMS(radec[0]);
                } else {
                    return RadiansPrintDMS(radec[0]);
                }
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "ra_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", "translate(0," + (height + y_offset) + ")")
            .call(xAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            //.attr("dx", ".35em")
            .attr("dy", ".35em")
            .attr("transform", "rotate(-45)")
            .style("text-anchor", "middle");
    }

    // Add the Y Axis
    {
        var yAxis = d3.axisLeft(y)
            .tickSize(width)
            .tickFormat(function (d) {
                if (d == 0.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var tmp, orig_x, orig_y;

                tmp = image_bounding_dims.x1 + (image_bounding_dims.width - 1);
                orig_x = tmp * fitsData.width / image.width;
                tmp = image_bounding_dims.y1 + d * (image_bounding_dims.height - 1);
                orig_y = tmp * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];
                return RadiansPrintDMS(radec[1]);
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "dec_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", "translate(" + (width - 0 + x_offset) + ",0)")
            .call(yAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            .attr("dx", ".35em")
            //.attr("dy", "-0.35em")
            //.attr("transform", "rotate(-45)")
            .style("text-anchor", "start");//was end, dx -.35, dy 0
    }

    if (va_count == 1 || composite_view) {
        var htmlStr = displayGridlines ? '<span class="fas fa-check-square"></span> lon/lat grid lines' : '<span class="far fa-square"></span> lon/lat grid lines';
        d3.select("#displayGridlines").html(htmlStr);

        var elem = d3.select("#gridlines");
        if (displayGridlines)
            elem.attr("opacity", 1);
        else
            elem.attr("opacity", 0);
    }
}

function display_cd_gridlines() {
    if (va_count > 1 && !composite_view)
        return;

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    //scale
    var gridScale = inverse_CD_matrix
    var angle = gridScale[2] * Math.sign(gridScale[0]);

    var label_angle = -45;

    if (Math.sign(angle) != 0)
        label_angle *= Math.sign(angle);

    for (let i = 0; i < gridScale.length; i++)
        if (isNaN(gridScale[i]))
            throw "CD matrix is not available";

    if (!has_image)
        return;

    try {
        d3.select("#gridlines").remove();
    }
    catch (e) {
    }

    var elem = d3.select("#image_rectangle");
    var width = parseFloat(elem.attr("width"));
    var height = parseFloat(elem.attr("height"));

    var x_offset = parseFloat(elem.attr("x"));
    var y_offset = parseFloat(elem.attr("y"));

    var x = d3.scaleLinear()
        .range([0, width])
        .domain([-1, 1]);

    var y = d3.scaleLinear()
        .range([height, 0])
        .domain([-1, 1]);

    var svg = d3.select("#BackgroundSVG");

    svg = svg.append("g")
        .attr("id", "gridlines")
        .attr("opacity", 1.0);

    let fillColour = 'white';
    let strokeColour = 'white';

    if (theme == 'bright') {
        fillColour = 'gray';
        strokeColour = 'gray';
    }

    if (colourmap == "greyscale" || colourmap == "negative") {
        fillColour = "#C4A000";
        strokeColour = fillColour;
    }

    // Add the X Axis
    if (fitsData.depth > 1) {
        var xAxis = d3.axisBottom(x)
            .tickSize(height)
            .tickFormat(function (d) {
                if (d == -1.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var dx = d * Math.cos(angle / toDegrees);
                var dy = d * Math.sin(angle / toDegrees);

                //convert dx, dy to a 0 .. 1 range
                var tmpx = image_bounding_dims.x1 + (dx + 1) / 2 * (image_bounding_dims.width - 1);
                var tmpy = image_bounding_dims.y1 + (dy + 1) / 2 * (image_bounding_dims.height - 1);

                var orig_x = tmpx * fitsData.width / image.width;
                var orig_y = tmpy * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        return RadiansPrintDMS(radec[0]);
                    else
                        return RadiansPrintHMS(radec[0]);
                } else {
                    return RadiansPrintDMS(radec[0]);
                }
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "ra_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", "translate(" + (x_offset) + "," + (y_offset) + ")" + ' rotate(' + angle + ' ' + (width / 2) + ' ' + (height / 2) + ')')
            .call(xAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            //.attr("dx", "-1.0em")
            //.attr("dy", "1.0em")
            .attr("transform", "rotate(" + label_angle + ")")
            .style("text-anchor", "middle");
    }
    else {
        var xAxis = d3.axisTop(x)
            .tickSize(height)
            .tickFormat(function (d) {
                if (d == -1.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var dx = d * Math.cos(angle / toDegrees);
                var dy = d * Math.sin(angle / toDegrees);

                //convert dx, dy to a 0 .. 1 range
                var tmpx = image_bounding_dims.x1 + (dx + 1) / 2 * (image_bounding_dims.width - 1);
                var tmpy = image_bounding_dims.y1 + (dy + 1) / 2 * (image_bounding_dims.height - 1);

                var orig_x = tmpx * fitsData.width / image.width;
                var orig_y = tmpy * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        return RadiansPrintDMS(radec[0]);
                    else
                        return RadiansPrintHMS(radec[0]);
                } else {
                    return RadiansPrintDMS(radec[0]);
                }
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "ra_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", "translate(" + (x_offset) + "," + (height + y_offset) + ")" + ' rotate(' + angle + ' ' + (width / 2) + ' ' + (- height / 2) + ')')
            .call(xAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            //.attr("dx", ".35em")
            //.attr("dy", ".35em")
            .attr("transform", "rotate(" + label_angle + ")")
            .style("text-anchor", "middle");
    }

    // Add the Y Axis
    {
        var yAxis = d3.axisLeft(y)
            .tickSize(width)
            .tickFormat(function (d) {
                if (d == -1.0 || d == 1.0)
                    return "";

                var image = imageContainer[va_count - 1];
                var image_bounding_dims = image.image_bounding_dims;

                var dx = d * Math.sin(angle / toDegrees);
                var dy = d * Math.cos(angle / toDegrees);

                //convert dx, dy to a 0 .. 1 range
                var tmpx = image_bounding_dims.x1 + (dx + 1) / 2 * (image_bounding_dims.width - 1);
                var tmpy = image_bounding_dims.y1 + (dy + 1) / 2 * (image_bounding_dims.height - 1);

                var orig_x = tmpx * fitsData.width / image.width;
                var orig_y = tmpy * fitsData.height / image.height;

                let world = pix2sky(fitsData, orig_x, orig_y);
                let radec = [world[0] / toDegrees, world[1] / toDegrees];
                return RadiansPrintDMS(radec[1]);
            });

        svg.append("g")
            .attr("class", "gridlines")
            .attr("id", "dec_axis")
            .style("fill", fillColour)
            .style("stroke", strokeColour)
            .style("stroke-width", 1.0)
            .attr("opacity", 1.0)
            .attr("transform", " translate(" + (width - 0 + x_offset) + "," + (y_offset) + ")" + ' rotate(' + angle + ' ' + (- width / 2) + ' ' + (height / 2) + ')')
            .call(yAxis)
            .selectAll("text")
            .attr("y", 0)
            .attr("x", 0)
            .style("fill", fillColour)
            .attr("dx", ".35em")
            //.attr("dy", "-0.35em")
            //.attr("transform", "rotate(-45)")
            .style("text-anchor", "start");//was end, dx -.35, dy 0
    }

    if (va_count == 1 || composite_view) {
        var htmlStr = displayGridlines ? '<span class="fas fa-check-square"></span> lon/lat grid lines' : '<span class="far fa-square"></span> lon/lat grid lines';
        d3.select("#displayGridlines").html(htmlStr);

        var elem = d3.select("#gridlines");
        if (displayGridlines)
            elem.attr("opacity", 1);
        else
            elem.attr("opacity", 0);
    }
}

function display_beam() {
    if (va_count > 1 && !composite_view)
        return;

    if (optical_view)
        return;

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    if (!has_image)
        return;

    try {
        d3.select("#beam").remove();
    }
    catch (e) {
    }

    var elem = d3.select("#image_rectangle");
    var img_width = parseFloat(elem.attr("width"));
    var img_height = parseFloat(elem.attr("height"));

    var image = imageContainer[va_count - 1];
    var image_bounding_dims = image.image_bounding_dims;
    var scale = (image.width / fitsData.width) * (img_width / image_bounding_dims.width);

    //display telescope beam
    if (fitsData.BMIN > 0.0 && fitsData.BMAJ > 0.0) {
        var svg = d3.select("#BackgroundSVG");
        var width = parseFloat(svg.attr("width"));
        var height = parseFloat(svg.attr("height"));

        svg = svg.append("g")
            .attr("id", "beam")
            .attr("opacity", 1.0);

        var rx = 0.5 * scale * fitsData.BMAJ / Math.abs(fitsData.CDELT1);
        var ry = 0.5 * scale * fitsData.BMIN / Math.abs(fitsData.CDELT2);
        var max_dim = Math.max(rx, ry);

        var beam_multiplier = 1.0;
        var beam_reduce = false;
        var beam_enlarge = false;
        var upper_threshold = 3 * emFontSize;
        var lower_threshold = 0.25 * emFontSize;

        while (max_dim > upper_threshold) {
            beam_reduce = true;
            beam_multiplier *= 2;
            max_dim /= beam_multiplier;

            //console.log("beam_reduce scale multiplier = ", beam_multiplier);
        }

        if (!beam_reduce) {
            while (max_dim < lower_threshold) {
                beam_enlarge = true;
                beam_multiplier *= 2;
                max_dim *= beam_multiplier;

                //console.log("beam_enlarge multiplier = ", beam_multiplier);
            }
        }

        if (beam_reduce) {
            rx /= beam_multiplier;
            ry /= beam_multiplier;
        }

        if (beam_enlarge) {
            rx *= beam_multiplier;
            ry *= beam_multiplier;
        }

        var rectSize = 2.5 * Math.max(rx, ry);

        //console.log("rx:", rx, "ry:", ry);

        var cx = (width + img_width) / 2 - rectSize - 0.025 * img_width;
        var cy = (height - img_height) / 2 + rectSize + 0.025 * img_height;

        let fillColour = 'white';
        let strokeColour = 'white';

        if (theme == 'bright') {
            fillColour = 'black';
            strokeColour = 'black';
        }

        svg.append("ellipse")
            .attr("cx", cx)
            .attr("cy", cy)
            .attr("rx", rx)
            .attr("ry", ry)
            .attr("transform", "rotate(" + (-90 - fitsData.BPA) + ',' + cx + ',' + cy + ")")
            .attr("fill", fillColour)
            .attr("opacity", 1.0);

        rectSize = Math.max(rectSize, 0.0075 * width);//10

        svg.append("rect")
            .attr("x", cx - rectSize)
            .attr("y", cy - rectSize)
            .attr("width", 2 * rectSize)
            .attr("height", 2 * rectSize)
            .attr("fill", "none")
            .style("stroke", strokeColour)
            .attr("opacity", 1.0);

        if (beam_reduce) {
            svg.append("text")
                .attr("x", cx - rectSize + 0.5 * emFontSize)
                .attr("y", cy + rectSize - 0.5 * emFontSize)
                .attr("font-family", "Inconsolata")
                .attr("font-style", "italic")
                .attr("text-anchor", "start")
                .attr("stroke", "none")
                .text("reduced 1:" + beam_multiplier);
        }

        if (beam_enlarge) {
            svg.append("text")
                .attr("x", cx + rectSize + 0.0 * emFontSize)
                .attr("y", cy + rectSize + 1.0 * emFontSize)
                .attr("font-family", "Inconsolata")
                .attr("font-style", "italic")
                .attr("text-anchor", "end")
                .attr("stroke", "none")
                .text("enlarged " + beam_multiplier + ":1");
        }

        svg.moveToBack();

        displayBeam = true;
        var htmlStr = displayBeam ? '<span class="fas fa-check-square"></span> telescope beam' : '<span class="far fa-square"></span> telescope beam';
        d3.select("#displayBeam").html(htmlStr);
    }
}

function zoom_beam() {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    if (!has_image)
        return;

    try {
        d3.select("#zoomBeam").remove();
    }
    catch (e) { };

    if (fitsData.BMIN > 0.0 && fitsData.BMAJ > 0.0) {
        var svg = d3.select("#BackSVG");

        let opacity = displayBeam ? 1 : 0;

        svg = svg.append("g")
            .attr("id", "zoomBeam")
            .attr("opacity", opacity);

        var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;
        var clipSize = Math.min(image_bounding_dims.width - 1, image_bounding_dims.height - 1) / zoom_scale;

        var elem = d3.select("#image_rectangle");
        var canvas_width = parseFloat(elem.attr("width"));
        var fitsSize = clipSize * fitsData.width / canvas_width;

        var rx = 0.5 * fitsData.BMAJ / Math.abs(fitsData.CDELT1);
        var ry = 0.5 * fitsData.BMIN / Math.abs(fitsData.CDELT2);

        let strokeColour = 'white';

        if (theme == 'bright')
            strokeColour = 'black';

        //first handle the circular viewport
        if (zoom_shape == "circle") {
            var tmp, cx, cy, cr, scale;

            tmp = d3.select("#upper");
            cx = parseFloat(tmp.attr("cx"));
            cy = parseFloat(tmp.attr("cy"));
            cr = parseFloat(tmp.attr("r"));
            scale = cr / fitsSize;

            cx -= cr / 2;
            cy += cr / 2;

            if (Math.max(rx * scale, ry * scale) > cr)
                strokeColour = 'none';

            svg.append("ellipse")
                .attr("id", "upperBeam")
                .attr("cx", cx)
                .attr("cy", cy)
                .attr("rx", rx * scale)
                .attr("ry", ry * scale)
                .attr("transform", "rotate(" + (-90 - fitsData.BPA) + ',' + cx + ',' + cy + ")")
                .attr("fill", "none")
                .attr("stroke", strokeColour)
                .style("stroke-dasharray", ("5, 1, 5"))
                .attr("opacity", 0.0);

            tmp = d3.select("#lower");
            cx = parseFloat(tmp.attr("cx"));
            cy = parseFloat(tmp.attr("cy"));

            cx -= cr / 2;
            cy += cr / 2;

            svg.append("ellipse")
                .attr("id", "lowerBeam")
                .attr("cx", cx)
                .attr("cy", cy)
                .attr("rx", rx * scale)
                .attr("ry", ry * scale)
                .attr("transform", "rotate(" + (-90 - fitsData.BPA) + ',' + cx + ',' + cy + ")")
                .attr("fill", "none")
                .attr("stroke", strokeColour)
                .style("stroke-dasharray", ("5, 1, 5"))
                .attr("opacity", 0.0);
        }

        //next a square viewport
        if (zoom_shape == "square") {
            var tmp, cx, cy, cr, scale;
            var x, y, sizeX, sizeY;

            tmp = d3.select("#upper");
            x = parseFloat(tmp.attr("x"));
            y = parseFloat(tmp.attr("y"));
            sizeX = parseFloat(tmp.attr("width"));
            sizeY = parseFloat(tmp.attr("height"));

            cx = x + sizeX / 2;
            cy = y + sizeY / 2;

            cr = sizeX / 2;
            scale = cr / fitsSize;

            cx = cx - cr + cr / 4;
            cy = cy + cr - cr / 4;

            if (Math.max(rx * scale, ry * scale) > (sizeX / 2))
                strokeColour = 'none';

            svg.append("ellipse")
                .attr("id", "upperBeam")
                .attr("cx", cx)
                .attr("cy", cy)
                .attr("rx", rx * scale)
                .attr("ry", ry * scale)
                .attr("transform", "rotate(" + (-90 - fitsData.BPA) + ',' + cx + ',' + cy + ")")
                .attr("fill", "none")
                .attr("stroke", strokeColour)
                .style("stroke-dasharray", ("5, 1, 5"))
                .attr("opacity", 0.0);

            tmp = d3.select("#lower");
            x = parseFloat(tmp.attr("x"));
            y = parseFloat(tmp.attr("y"));
            sizeX = parseFloat(tmp.attr("width"));
            sizeY = parseFloat(tmp.attr("height"));

            cx = x + sizeX / 2;
            cy = y + sizeY / 2;

            cx = cx - cr + cr / 4;
            cy = cy + cr - cr / 4;

            svg.append("ellipse")
                .attr("id", "lowerBeam")
                .attr("cx", cx)
                .attr("cy", cy)
                .attr("rx", rx * scale)
                .attr("ry", ry * scale)
                .attr("transform", "rotate(" + (-90 - fitsData.BPA) + ',' + cx + ',' + cy + ")")
                .attr("fill", "none")
                .attr("stroke", strokeColour)
                .style("stroke-dasharray", ("5, 1, 5"))
                .attr("opacity", 0.0);
        }
    }
}

function frame_reference_unit(index) {
    let fitsData = fitsContainer[index - 1];

    if (fitsData.CUNIT3.toLowerCase() === "Hz".toLowerCase()) {
        has_frequency_info = true;
        frame_multiplier = 1;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "kHz".toLowerCase()) {
        has_frequency_info = true;
        frame_multiplier = 1e3;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "MHz".toLowerCase()) {
        has_frequency_info = true;
        frame_multiplier = 1e6;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "GHz".toLowerCase()) {
        has_frequency_info = true;
        frame_multiplier = 1e9;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "THz".toLowerCase()) {
        has_frequency_info = true;
        frame_multiplier = 1e12;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "m/s".toLowerCase()) {
        has_velocity_info = true;
        frame_multiplier = 1;
        return;
    }

    if (fitsData.CUNIT3.toLowerCase() === "km/s".toLowerCase()) {
        has_velocity_info = true;
        frame_multiplier = 1e3;
        return;
    }
}

function frame_reference_type(index) {
    let fitsData = fitsContainer[index - 1];

    if (fitsData.CTYPE3.toLowerCase().includes("f")) {
        has_frequency_info = true;
        return;
    }

    if (fitsData.CTYPE3.toLowerCase().includes("v")) {
        has_velocity_info = true;
        return;
    }
}

function display_dataset_info() {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    var svg = d3.select("#FrontSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    xradec = new Array(null, null);

    let orig_x = fitsData.width / 2;
    let orig_y = fitsData.height / 2;
    let world = pix2sky(fitsData, orig_x, orig_y);

    xradec[0] = world[0] / toDegrees;
    xradec[1] = world[1] / toDegrees;

    try {
        d3.select("#information").remove();
    }
    catch (e) {
    }

    var group = svg.append("g")
        .attr("id", "information");

    var object = fitsData.OBJECT;
    var filter = fitsData.FILTER.trim().toUpperCase();

	/*if (object == '')
		object = 'OBJECT N/A';
	else*/ {
        //object = object.replace('_' + filter, '');//filter names are inconsistent!!!

        if (filter != "") {
            var pos = object.lastIndexOf('_');

            if (pos >= 0)
                object = object.substr(0, pos);
        }
    }

    var line = '';

    /*if(va_count == 1)
    {
    line = fitsData.LINE.trim() ;

    if(line != "")
      line = ' (' + line + ')' ;
    }*/

    group.append("text")
        .attr("id", "object")
        .attr("x", width)
        .attr("y", 4.5 * emFontSize)//7*
        .attr("font-family", "Helvetica")//Arial
        .attr("font-weight", "normal")
        .attr("font-size", "2.5em")
        .attr("text-anchor", "end")
        .attr("stroke", "none")
        .text(object.replace(/_/g, " ") + line)
        .append("svg:title")
        .text("object name");

    let titleStr = fitsData.OBJECT.replace(/_/g, " ").trim();

    if (titleStr != "") {
        document.title = titleStr;
    };

    var dateobs = fitsData.DATEOBS;

    if (dateobs == '')
        dateobs = '';//'DATEOBS N/A' ;
    else {
        var pos = dateobs.indexOf('.');

        if (pos >= 0)
            dateobs = dateobs.substr(0, pos);

        dateobs = dateobs.replace(/T/g, " ") + ' ' + fitsData.TIMESYS;
    }

    group.append("text")
        .attr("id", "dateobs")
        .attr("x", width)
        .attr("y", 6.5 * emFontSize)//6.5 6.0
        .attr("font-family", "Helvetica")
        .attr("font-size", "1.3em")//1.75
        .attr("text-anchor", "end")
        .attr("stroke", "none")
        .attr("opacity", 0.75)
        .text(dateobs)
        .append("svg:title")
        .text("observation date");

    let raText = 'RA N/A';

    // check if fitsData.RA is not empty
    if (fitsData.RA != '') {
        raText = 'RA: ' + fitsData.RA;
    }

    if (fitsData.CTYPE1.indexOf("RA") > -1) {
        if (coordsFmt == 'DMS')
            raText = 'α: ' + RadiansPrintDMS(xradec[0]);
        else
            raText = 'α: ' + RadiansPrintHMS(xradec[0]);
    }

    if (fitsData.CTYPE1.indexOf("GLON") > -1)
        raText = 'l: ' + RadiansPrintDMS(xradec[0]);

    if (fitsData.CTYPE1.indexOf("ELON") > -1)
        raText = 'λ: ' + RadiansPrintDMS(xradec[0]);

    group.append("text")
        .attr("id", "ra")
        .attr("x", width)
        .attr("y", 8.5 * emFontSize)//8.5 7.5
        .attr("font-family", "Inconsolata")
        //.attr("font-style", "italic")
        .attr("font-size", "1.5em")
        .attr("text-anchor", "end")
        .attr("stroke", "none")
        .text(raText);
    /*.append("svg:title")
    .text(fitsData.CTYPE1.trim());*/

    let decText = 'DEC N/A';

    // check if fitsData.DEC is not empty
    if (fitsData.DEC != '') {
        decText = 'DEC: ' + fitsData.DEC;
    }

    if (fitsData.CTYPE2.indexOf("DEC") > -1)
        decText = 'δ: ' + RadiansPrintDMS(xradec[1]);

    if (fitsData.CTYPE2.indexOf("GLAT") > -1)
        decText = 'b: ' + RadiansPrintDMS(xradec[1]);

    if (fitsData.CTYPE2.indexOf("ELAT") > -1)
        decText = 'β: ' + RadiansPrintDMS(xradec[1]);

    group.append("text")
        .attr("id", "dec")
        .attr("x", width)
        .attr("y", 10 * emFontSize)//10 8.75
        .attr("font-family", "Inconsolata")
        //.attr("font-style", "italic")
        .attr("font-size", "1.5em")
        .attr("text-anchor", "end")
        .attr("stroke", "none")
        .text(decText);
    /*.append("svg:title")
    .text(fitsData.CTYPE2.trim());*/

    group.append("text")
        .attr("id", "pixel")
        .attr("x", width)
        .attr("y", 11.5 * emFontSize)//11.5 10
        .attr("font-family", "Inconsolata")
        //.attr("font-style", "italic")
        .attr("font-size", "1.5em")
        .attr("text-anchor", "end")
        .attr("stroke", "none")
        .attr("opacity", 0.0)
        .text("")
        .append("svg:title")
        .text("pixel value (intensity)");

    var val1 = fitsData.CRVAL3 + fitsData.CDELT3 * (1 - fitsData.CRPIX3);
    var val2 = fitsData.CRVAL3 + fitsData.CDELT3 * (fitsData.depth - fitsData.CRPIX3);

    data_band_lo = Math.min(val1, val2);
    data_band_hi = Math.max(val1, val2);
    RESTFRQ = fitsData.RESTFRQ;

    //disable frequency display in multiple-view mode
    if (va_count > 1)
        RESTFRQ = 0;
    else {
        if (RESTFRQ > 0.0)
            has_frequency_info = true;
    }

    if (has_velocity_info && has_frequency_info) {
        var c = 299792.458;//speed of light [km/s]

        var v1 = fitsData.CRVAL3 + fitsData.CDELT3 * (1 - fitsData.CRPIX3);
        v1 /= 1000;//[km/s]

        var v2 = fitsData.CRVAL3 + fitsData.CDELT3 * (fitsData.depth - fitsData.CRPIX3);
        v2 /= 1000;//[km/s]

        var f1 = RESTFRQ * Math.sqrt((1 - v1 / c) / (1 + v1 / c));
        var f2 = RESTFRQ * Math.sqrt((1 - v2 / c) / (1 + v2 / c));

        data_band_lo = Math.min(f1, f2);
        data_band_hi = Math.max(f1, f2);

        //console.log("v1:", v1, "v2:", v2);
        //console.log("f1:", f1, "f2:", f2);
    }
    else if (has_frequency_info) {
        /*if(fitsData.CTYPE3 != "")
        {
          //an override due to ALMA data errors: use the no middle-point
          RESTFRQ = (val1+val2)/2 ;//expects [Hz]
        }*/

        RESTFRQ = (RESTFRQ / 1.0e9).toPrecision(7) * 1.0e9;//slightly rounded, expected unit is [Hz]

        data_band_lo = Math.min(val1, val2);
        data_band_hi = Math.max(val1, val2);
    }

    //console.log("CTYPE3 = ", fitsData.CTYPE3, "has_freq:", has_frequency_info, "has_vel:", has_velocity_info);

    if (has_frequency_info > 0.0 && va_count == 1) {

        var bandStr = '';

        if (fitsData.depth > 1)
            bandStr = '<span style="float:left; font-weight:bold">REF FRQ</span><br><input type="number" id="frequencyInput" min="0" step="0.1" style="width: 6em; color: black; background-color: lightgray; font-size: 1.0em" value="' + (RESTFRQ / 1.0e9).toPrecision(7) + '"> GHz';
        else
            bandStr = '<span style="float:left; font-weight:bold">REF FRQ</span><br><input type="number" id="frequencyInput" min="0" step="0.1" style="width: 6em; color: black; background-color: lightgray; font-size: 1.0em" value="' + (RESTFRQ / 1.0e9).toPrecision(7) + '" disabled> GHz';

        group.append("g")
            .attr("id", "foreignBandG")
            .style("opacity", 0.25)
            .append("foreignObject")
            .attr("id", "foreignBand")
            .attr("x", (width - 20 * emFontSize))
            .attr("y", 12.5 * emFontSize)//12.5
            .attr("width", 20 * emFontSize)
            .attr("height", 7 * emFontSize)
            .on("mouseenter", function () {
                d3.select("#foreignBandG").style("opacity", 1.0);
            })
            .on("mouseleave", function () {
                d3.select("#foreignBandG").style("opacity", 0.25);
            })
            .append("xhtml:div")
            .attr("id", "bandDiv")
            .attr("class", "container-fluid input")
            .style("float", "right")
            .style("padding", "2.5%")
            .append("span")
            .attr("id", "band")
            .html(bandStr);

        var elem = document.getElementById('frequencyInput');
        elem.onblur = submit_corrections;
        elem.onmouseleave = submit_corrections;
        elem.onkeyup = function (e) {
            var event = e || window.event;
            var charCode = event.which || event.keyCode;

            if (charCode == '13') {
                //console.log('REF FRQ ENTER');
                // Enter pressed
                submit_corrections();
                return false;
            }
        }
    }

    if (fitsData.depth > 1 && (/*has_velocity_info ||*/ has_frequency_info)) {
        var velStr = '<span id="redshift" class="redshift" style="float:left; font-weight:bold">SRC&nbsp;</span><input type="radio" id="velV" name="velocity" value="v" style="vertical-align: middle; margin: 0px;" onclick="javascript:toggle_redshift_input_source(this);"> V&nbsp;<input type="radio" id="velZ" name="velocity" value="z" style="vertical-align: middle; margin: 0px;" onclick="javascript:toggle_redshift_input_source(this);"> z&nbsp;<br><span><input type="number" id="velocityInput" step="0.1" style="width: 4.5em; color: black; background-color: lightgray; font-size: 1.0em" value="' + USER_DELTAV + '"></span> <span id="unit">km/s</span><br>';

        if (has_frequency_info)
            velStr += '<label class="small" style="cursor: pointer; font-weight:bold"><input type="checkbox" value="" class="control-label" style="cursor: pointer" id="restcheckbox" onmouseenter="javascript:this.focus();" onchange="javascript:toggle_rest_frequency();">&nbsp;<I>F<SUB>REST</SUB></I></label>'

        var yoffset = 21 * emFontSize;

        if (composite_view)
            yoffset += 1 * emFontSize;

        group.append("g")
            .attr("id", "foreignVelG")
            .style("opacity", 0.25)
            .append("foreignObject")
            .attr("id", "foreignVel")
            .attr("x", (width - 20 * emFontSize))
            .attr("y", yoffset)//(17.5*emFontSize)//19
            .attr("width", 20 * emFontSize)
            .attr("height", 10.0 * emFontSize)
            .on("mouseenter", function () {
                d3.select("#foreignVelG").style("opacity", 1.0);
            })
            .on("mouseleave", function () {
                d3.select("#foreignVelG").style("opacity", 0.25);
            })
            .append("xhtml:div")
            .attr("id", "velDiv")
            .attr("class", "container-fluid input")
            .style("float", "right")
            .style("padding", "2.5%")
            .append("span")
            .attr("id", "vel")
            .html(velStr);

        if (has_frequency_info) {
            var checkbox = document.getElementById('restcheckbox');

            if (sessionStorage.getItem("rest") === null)
                checkbox.checked = false;
            else {
                var checked = sessionStorage.getItem("rest");

                if (checked == "true")
                    checkbox.checked = true;
                else
                    checkbox.checked = false;
            }
        }

        if (sessionStorage.getItem("redshift") === null)
            document.getElementById('velV').setAttribute("checked", "");
        else {
            var value = sessionStorage.getItem("redshift");

            if (value == "z") {
                document.getElementById('velZ').setAttribute("checked", "");
                document.getElementById('unit').style.opacity = "0.0";
            }
            else
                document.getElementById('velV').setAttribute("checked", "");
        }

        //add onblur
        var m = document.getElementById('velocityInput');
        m.onblur = submit_delta_v;
        m.onmouseleave = submit_delta_v;
        m.onkeyup = function (e) {
            var event = e || window.event;
            var charCode = event.which || event.keyCode;

            if (charCode == '13') {
                // Enter pressed
                submit_delta_v();
                return false;
            }
        }

    }

    //add video playback control
    if (fitsData.depth > 1) {
        var yoffset = 32 * emFontSize;

        if (composite_view)
            yoffset += 1 * emFontSize;

        var videoStr = '<span id="videoPlay" class="fas fa-play" style="display:inline-block; cursor: pointer"></span><span id="videoPause" class="fas fa-pause" style="display:none; cursor: pointer"></span>&nbsp; <span id="videoStop" class="fas fa-stop" style="cursor: pointer"></span>&nbsp; <span id="videoForward" class="fas fa-forward" style="cursor: pointer"></span>&nbsp; <span id="videoFastForward" class="fas fa-fast-forward" style="cursor: pointer"></span>';

        group.append("g")
            .attr("id", "videoControlG")
            .style("opacity", 0.25)
            .append("foreignObject")
            .attr("id", "videoControl")
            .attr("x", (width - 20 * emFontSize))
            .attr("y", yoffset)//(17.5*emFontSize)//19
            .attr("width", 20 * emFontSize)
            .attr("height", 5.0 * emFontSize)
            .on("mouseenter", function () {
                d3.select("#videoControlG").style("opacity", 1.0);

                //hide the molecular list (spectral lines) so that it does not obscure the controls!
                displayMolecules_bak = displayMolecules;
                displayMolecules = false;
                document.getElementById('molecularlist').style.display = "none";
            })
            .on("mouseleave", function () {
                d3.select("#videoControlG").style("opacity", 0.25);

                displayMolecules = displayMolecules_bak;

                video_playback = false;
                clearTimeout(video_timeout);
                video_timeout = -1;

                document.getElementById('videoPlay').style.display = "inline-block";
                document.getElementById('videoPause').style.display = "none";

                if (streaming)
                    x_axis_mouseleave();
            })
            .append("xhtml:div")
            .attr("id", "videoDiv")
            .attr("class", "container-fluid input")
            .style("float", "right")
            .style("padding", "2.5%")
            .append("span")
            .attr("id", "vel")
            .html(videoStr);

        document.getElementById('videoPlay').onclick = function () {
            video_playback = true;
            video_period = 10.0;

            if (video_offset == null)
                video_offset = [parseFloat(d3.select("#frequency").attr("x")), parseFloat(d3.select("#frequency").attr("y"))];

            document.getElementById('videoPlay').style.display = "none";
            document.getElementById('videoPause').style.display = "inline-block";

            if (!streaming)
                x_axis_mouseenter(video_offset);

            if (video_timeout < 0)
                replay_video();
        };

        document.getElementById('videoPause').onclick = function () {
            video_playback = false;
            clearTimeout(video_timeout);
            video_timeout = -1;

            document.getElementById('videoPlay').style.display = "inline-block";
            document.getElementById('videoPause').style.display = "none";
        };

        document.getElementById('videoStop').onclick = function () {
            video_playback = false;
            video_offset = null;
            clearTimeout(video_timeout);
            video_timeout = -1;

            document.getElementById('videoPlay').style.display = "inline-block";
            document.getElementById('videoPause').style.display = "none";

            if (streaming)
                x_axis_mouseleave();
        };

        document.getElementById('videoForward').onclick = function () {
            video_playback = true;
            video_period = 5.0;

            if (video_offset == null)
                video_offset = [parseFloat(d3.select("#frequency").attr("x")), parseFloat(d3.select("#frequency").attr("y"))];

            document.getElementById('videoPlay').style.display = "none";
            document.getElementById('videoPause').style.display = "inline-block";

            if (!streaming)
                x_axis_mouseenter(video_offset);

            if (video_timeout < 0)
                replay_video();
        };

        document.getElementById('videoFastForward').onclick = function () {
            video_playback = true;
            video_period = 2.5;

            if (video_offset == null)
                video_offset = [parseFloat(d3.select("#frequency").attr("x")), parseFloat(d3.select("#frequency").attr("y"))];

            document.getElementById('videoPlay').style.display = "none";
            document.getElementById('videoPause').style.display = "inline-block";

            if (!streaming)
                x_axis_mouseenter(video_offset);

            if (video_timeout < 0)
                replay_video();
        };
    }

    var range = get_axes_range(width, height);

    svg.append("text")
        .attr("x", emFontSize / 4 /*width / 2*/)
        //.attr("y", 0.67 * range.yMin)
        .attr("y", 0.70 * range.yMin)
        .attr("font-family", "Helvetica")
        .attr("font-weight", "normal")
        //.attr("font-style", "italic")
        .attr("font-size", 0.75 * range.yMin)
        //.attr("text-anchor", "middle")
        .attr("stroke", "none")
        .attr("opacity", 0.5)//0.25
        //.text("☰ SETTINGS");
        //.text("⚙");
        .text("☰");

    let strokeColour = 'white';

    if (theme == 'bright')
        strokeColour = 'black';

    //add a menu activation area
    svg.append("rect")
        .attr("id", "menu_activation_area")
        .attr("x", 0/*emStrokeWidth*/)
        .attr("y", emStrokeWidth - 1)
        //.attr("width", (width - 2 * emStrokeWidth))
        .attr("width", (width))
        .attr("height", (range.yMin - 2 * emStrokeWidth))
        .attr("fill", "transparent")
        .attr("opacity", 0.1)//was 0.7
        .attr("stroke", strokeColour)//strokeColour or "transparent"
        .style("stroke-dasharray", ("1, 5"))
        .on("mouseenter", function () {
            d3.select(this).attr("opacity", 0);
            document.getElementById('menu').style.display = "block";
        });
}

function toggle_rest_frequency() {
    var checkbox = document.getElementById('restcheckbox');

    var freq_start = data_band_lo;
    var freq_end = data_band_hi;

    if (checkbox.checked) {
        sessionStorage.setItem("rest", "true");

        freq_start = relativistic_rest_frequency(freq_start);
        freq_end = relativistic_rest_frequency(freq_end);
    }
    else
        sessionStorage.setItem("rest", "false");

    //refresh spectral lines
    fetch_spectral_lines(datasetId, freq_start, freq_end);

    //refresh axes
    setup_axes();
}

function toggle_redshift(selection) {
    var unit = document.getElementById('unit');
    var elem = document.getElementById('redshift');

    sessionStorage.setItem("redshift", selection.value);

    if (selection.value == "v") {
        unit.innerHTML = "km/s";

        // set the elem "min", "max" and "step" attributes
        elem.setAttribute("min", -299792);
        elem.setAttribute("max", 299792);
        elem.setAttribute("step", 1);
    }

    if (selection.value == "z") {
        unit.innerHTML = "(z > -1)";

        // set the elem "min" and "step" attributes
        elem.setAttribute("min", -0.9);
        elem.setAttribute("step", 0.0001);
    }

    var m = document.getElementById('redshift');
    m.value = 0.0.toFixed(1);
    m.focus();

    validate_redshift();
}

function toggle_redshift_input_source(selection) {
    var unit = document.getElementById('unit');

    sessionStorage.setItem("redshift", selection.value);

    if (selection.value == "v")
        unit.style.opacity = "1.0";

    if (selection.value == "z")
        unit.style.opacity = "0.0";

    var m = document.getElementById('velocityInput');
    m.value = "0";
    m.focus();

    submit_delta_v();

    var m = document.getElementById('velocityInput');
    m.focus();

    //console.log("toggled redshift input source");
}

function submit_delta_v() {
    //do we need to refresh molecules?
    if (has_frequency_info) {
        var checkbox = document.getElementById('restcheckbox');

        if (checkbox.checked) {
            var freq_start = relativistic_rest_frequency(data_band_lo);
            var freq_end = relativistic_rest_frequency(data_band_hi);

            //refresh spectral lines
            fetch_spectral_lines(datasetId, freq_start, freq_end);
        }
    }

    var strV = document.getElementById('velocityInput').value;

    if (strV.trim() == '')
        document.getElementById('velocityInput').value = 0;

    var tmp = 0.0;

    try {
        tmp = document.getElementById('velocityInput').valueAsNumber;
    }
    catch (e) {
        console.error(e);
    }

    //range checks
    var c = 299792.458;//speed of light [km/s]

    var value = sessionStorage.getItem("redshift");

    if (value == "z") {
        if (tmp <= -1) {
            document.getElementById('velocityInput').value = 0;
            invalid_range();
        }
        else
            USER_DELTAV = tmp;
    }
    else {
        if (tmp <= -c) {
            document.getElementById('velocityInput').value = 0;
            invalid_range();
        }
        else
            if (tmp >= c) {
                document.getElementById('velocityInput').value = 0;
                invalid_range();
            }
            else
                USER_DELTAV = tmp;

    };

    setup_axes();

    //re-attach lost event handlers
    {
        var m = document.getElementById('velocityInput');
        m.onblur = submit_delta_v;
        m.onmouseleave = submit_delta_v;
        m.onkeyup = function (e) {
            var event = e || window.event;
            var charCode = event.which || event.keyCode;

            if (charCode == '13') {
                // Enter pressed
                submit_delta_v();
                return false;
            }
        }
    }
}

function submit_corrections() {
    var referenceFrequency = document.getElementById('frequencyInput').valueAsNumber * 1e9;

    //console.log("user referenceFrequency:", referenceFrequency);

    if (referenceFrequency > 0.0) {
        USER_SELFRQ = referenceFrequency;

        let fitsData = fitsContainer[va_count - 1];

        if (fitsData.RESTFRQ <= 0.0) {
            fitsContainer[va_count - 1].RESTFRQ = USER_SELFRQ;

            //has_frequency_info = false ;//force the re-creation of band ranges

            display_dataset_info();

            toggle_rest_frequency();
        }

        if (has_velocity_info && has_frequency_info) {
            fitsContainer[va_count - 1].RESTFRQ = USER_SELFRQ;

            display_dataset_info();

            toggle_rest_frequency();
        }
    }
    else
        USER_SELFRQ = RESTFRQ;

    set_user_restfrq();

    //re-attach lost event handlers
    {
        var elem = document.getElementById('frequencyInput');
        elem.onblur = submit_corrections;
        elem.onmouseleave = submit_corrections;
        elem.onkeyup = function (e) {
            var event = e || window.event;
            var charCode = event.which || event.keyCode;

            if (charCode == '13') {
                //console.log('REF FRQ ENTER');
                // Enter pressed
                submit_corrections();
                return false;
            }
        }
    }
};

function validate_redshift() {
    var toggle = sessionStorage.getItem("redshift");
    var value = document.getElementById('redshift').valueAsNumber;

    if (isNaN(value))
        //document.getElementById('redshift').value = previous_redshift;
        document.getElementById('redshift').value = 0.0.toFixed(1);

    // range checks for z
    if (toggle == 'z') {
        if (value <= -1)
            document.getElementById('redshift').value = -0.99999;

        if (value > 6.0)
            document.getElementById('redshift').value = 6.0.toFixed(1);

        // no need to convert anything, use the value 'as-is'
        redshift = document.getElementById('redshift').valueAsNumber;
    }

    // range checks for v
    if (toggle == 'v') {
        if (value < -299792)
            document.getElementById('redshift').value = -299792;

        if (value > 299792)
            document.getElementById('redshift').value = 299792;

        // convert z redshift to velocity
        let c = 299792.458;//speed of light [km/s]
        let vel = document.getElementById('redshift').valueAsNumber;

        let beta = vel / c;
        redshift = Math.sqrt((1.0 + beta) / (1.0 - beta)) - 1.0;
    }

    if (redshift != previous_redshift) {
        previous_redshift = redshift;

        // refresh the HDS spectral lines
        hds_divs.forEach(refresh_hds_spectral_lines);
    }

    //re-attach lost event handlers
    {
        var elem = document.getElementById('redshift');
        elem.onblur = validate_redshift;
        elem.onmouseleave = validate_redshift;
        elem.onkeyup = function (e) {
            var event = e || window.event;

            if (event.key === 'Enter') {
                // Enter pressed
                validate_redshift();
                return false;
            }
        }
        elem.onchange = function () {
            clearTimeout(idleResize);

            idleResize = setTimeout(function () {
                validate_redshift();
            }, 250);
        };
    }
}

function validate_contour_lines() {
    var value = document.getElementById('contour_lines').valueAsNumber;

    if (isNaN(value))
        document.getElementById('contour_lines').value = previous_contour_lines;

    if (value < 2)
        document.getElementById('contour_lines').value = 2;

    if (value > 10)
        document.getElementById('contour_lines').value = 10;

    value = document.getElementById('contour_lines').valueAsNumber;

    if (value != previous_contour_lines) {
        previous_contour_lines = value;
        update_contours();
    }
}

function validate_pv_contour_lines() {
    var value = document.getElementById('pv_contour_lines').valueAsNumber;

    if (isNaN(value))
        document.getElementById('pv_contour_lines').value = previous_pv_contour_lines;

    if (value < 2)
        document.getElementById('pv_contour_lines').value = 2;

    if (value > 10)
        document.getElementById('pv_contour_lines').value = 10;

    value = document.getElementById('pv_contour_lines').valueAsNumber;

    if (value != previous_pv_contour_lines) {
        previous_pv_contour_lines = value;

        if (displayPVContours) {
            resubmit_pv_line();
        }
    }
}

function set_user_restfrq() {
    RESTFRQ = USER_SELFRQ;

    var bandStr = bandStr = '<span style="float:left; font-weight:bold">REF FRQ</span><br><input type="number" id="frequencyInput" min="0" step="0.1" style="width: 6em; color: black; background-color: lightgray; font-size: 1.0em" value="' + (RESTFRQ / 1.0e9).toPrecision(7) + '"> GHz';

    d3.select("#band").html(bandStr);

    setup_axes();
};

function invalid_range() {
    $("#rangevalidation").modal("show");

    var modal = document.getElementById('rangevalidation');
    var span = document.getElementById('rangevalidationclose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $("#rangevalidation").modal("hide");
    }
    // When the user clicks anywhere outside of the modal, close it
    window.onclick = function (event) {
        if (event.target == modal) {
            $("#rangevalidation").modal("hide");
        }
    }
}

function change_spectrum_scale(index) {
    var value = document.getElementById("scale" + index).value;

    spectrum_scale[index - 1] = parseFloat(value);

    change_intensity_mode();
}

function change_tone_mapping(index, recursive) {
    var display;

    if (document.getElementById('flux' + index).value == 'linear' || document.getElementById('flux' + index).value == 'square')
        display = "none";
    else
        display = "block";

    d3.select("#sensitivitySlider" + index)
        .style("display", display);

    noise_sensitivity = 50;
    document.getElementById('sensitivity' + index).value = noise_sensitivity;
    document.getElementById('sensitivityInput' + index).innerHTML = get_noise_sensitivity_string(noise_sensitivity, 2);

    setup_histogram_interaction(index);

    // set a new tone mapping function
    if (imageContainer[index - 1] != null) {
        let image = imageContainer[index - 1];

        image.tone_mapping.flux = document.getElementById('flux' + index).value;

        // reset the legacy settings
        let p = 0.5;
        image.tone_mapping.lmin = Math.log(p);
        image.tone_mapping.lmax = Math.log(p + 1.0);

        if (image.tone_mapping.flux == "legacy") {
            image.tone_mapping.black = image.tone_mapping.min;
            image.tone_mapping.white = image.tone_mapping.max;
        }

        if (composite_view) {
            clear_webgl_composite_image_buffers();
        } else {
            clear_webgl_image_buffers(index);
        }

    }

    if (va_count == 1) {
        update_legend();
    } else {
        draw_rbg_legend(index);
    }

    if (!composite_view) {
        init_webgl_image_buffers(index);
    }

    //change other datasets too
    if (va_count > 1 && recursive) {
        for (let i = 1; i <= va_count; i++)
            if (i != index) {
                document.getElementById('flux' + i).value = document.getElementById('flux' + index).value
                change_tone_mapping(i, false);
            }

        // refresh an image
        if (composite_view) {
            compositeImage.tone_mapping.flux = document.getElementById('flux' + index).value;
            init_webgl_composite_image_buffers();
        }
    }
}

function cube_refresh(index) {
    try {
        d3.selectAll('#contourPlot').remove();
    }
    catch (e) { };

    enable_autoscale();

    var elem = document.getElementById("SpectrumCanvas");
    var range = get_axes_range(elem.width, elem.height);
    var dx = range.xMax - range.xMin;

    var rect = document.getElementById('mainDiv').getBoundingClientRect();
    var width = Math.round(rect.width - 20);
    var height = Math.round(rect.height - 20);

    var request = {
        type: "image",
        dx: dx,
        width: width,
        height: height,
        quality: image_quality,
        intensity: intensity_mode,
        frame_start: data_band_lo,
        frame_end: data_band_hi,
        ref_freq: RESTFRQ,
        timestamp: performance.now()
    };

    //send an [image] request to the server
    if (wsConn[index - 1].readyState == 1)
        wsConn[index - 1].send(JSON.stringify(request));


    setup_window_timeout();
}

function display_scale_range_ui(called_from_menu = false) {
    d3.select("#yaxis")
        .style("fill", 'white')
        .style("stroke", 'white')
        .transition()
        .duration(500)
        .style("fill", axisColour)
        .style("stroke", axisColour);

    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "scalingHelp")
        .attr("class", "modal")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var content = div.append("div")
        .attr("class", "modal-content");

    var header = content.append("div")
        .attr("class", "modal-header");

    header.append("span")
        .attr("id", "scalingHeaderClose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    header.append("h3")
        .text("How to Scale the Y-Axis");

    var body = content.append("div")
        .attr("class", "modal-body");

    body.append("p")
        .html("move mouse cursor over to the Y-Axis whilst holding the 「Shift」 key");

    body.append("p")
        .html("drag the mouse over the Y-Axis to <i>shift</i> it <em>UP</em> and <em>DOWN</em>");

    body.append("p")
        .html("use the mouse <i>scroll wheel</i> or a two-finger <i>touch gesture</i> to <i>re-scale</i> the Y-Axis range");

    var footer = content.append("div")
        .attr("class", "modal-footer");

    footer.append("p")
        .style("color", "#a94442")
        .html("you can disable showing this dialog via the <i>Preferences</i> menu, <i>display pop-up help</i> checkbox");

    if (called_from_menu)
        $('#scalingHelp').addClass("modal-center");

    if (displayScalingHelp) {
        show_scaling_help();
        $('#scalingHelp').modal('show');
    }

    /*var svg = d3.select("#FrontSVG") ;
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    d3.select("#yaxis")
    .attr("data-toggle", "popover")
    .attr("data-trigger", "hover")
    .attr("title", "fixed scale")
    .attr("data-content", "hold 's' and move mouse over the Y-Axis, then use mouse drag/scroll-wheel to adjust the Y-Axis scale");

    $(document).ready(function(){
    $('[data-toggle="popover"]').popover();
    });*/

}

function set_autoscale_range(called_from_menu = false) {
    autoscale = false;
    var htmlStr = autoscale ? '<span class="fas fa-check-square"></span> autoscale y-axis' : '<span class="far fa-square"></span> autoscale y-axis';
    d3.select("#autoscale").html(htmlStr);

    user_data_min = tmp_data_min;
    user_data_max = tmp_data_max;

    plot_spectrum(last_spectrum);
    replot_y_axis();

    display_scale_range_ui(called_from_menu);
};

function enable_autoscale() {
    autoscale = true;
    var htmlStr = autoscale ? '<span class="fas fa-check-square"></span> autoscale y-axis' : '<span class="far fa-square"></span> autoscale y-axis';
    d3.select("#autoscale").html(htmlStr);

    user_data_min = null;
    user_data_max = null;
};

function change_binning() {
    try {
        binning = parseInt(document.getElementById('binning').value);
    }
    catch (e) { };

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData != null) {
        if (fitsData.depth > 1) {
            if (va_count == 1) {
                if (intensity_mode == "mean") {
                    plot_spectrum([fitsData.mean_spectrum]);
                    replot_y_axis();
                }

                if (intensity_mode == "integrated") {
                    plot_spectrum([fitsData.integrated_spectrum]);
                    replot_y_axis();
                }
            }
            else {
                if (intensity_mode == "mean") {
                    plot_spectrum(mean_spectrumContainer);
                    replot_y_axis();
                }

                if (intensity_mode == "integrated") {
                    plot_spectrum(integrated_spectrumContainer);
                    replot_y_axis();
                }
            }
        }
    }
}

function change_video_fps_control() {
    video_fps_control = document.getElementById('video_fps_control').value;

    if (video_fps_control == 'auto')
        vidFPS = 5;//10
    else
        vidFPS = parseInt(video_fps_control);

    localStorage.setItem("video_fps_control", video_fps_control);
}

function change_zoom_shape() {
    zoom_shape = document.getElementById('zoom_shape').value;
    localStorage.setItem("zoom_shape", zoom_shape);

    setup_image_selection();
    setup_viewports();
}

function change_intensity_mode() {
    intensity_mode = document.getElementById('intensity_mode').value;
    localStorage.setItem("intensity_mode", intensity_mode);

    //console.log("new intensity mode:", intensity_mode);

    display_hourglass();

    image_count = 0;
    viewport_count = 0;
    spectrum_count = 0;

    for (let index = 1; index <= va_count; index++)
        cube_refresh(index);
}

function change_coords_fmt() {
    coordsFmt = document.getElementById('coords_fmt').value;
    localStorage.setItem("coordsFmt", coordsFmt);

    if (xradec != null) {
        let fitsData = fitsContainer[va_count - 1];

        if (fitsData.CTYPE1.indexOf("RA") > -1) {
            let raText = 'RA N/A';

            if (coordsFmt == 'DMS')
                raText = 'α: ' + RadiansPrintDMS(xradec[0]);
            else
                raText = 'α: ' + RadiansPrintHMS(xradec[0]);

            d3.select("#ra").text(raText);

            try {
                display_cd_gridlines();
            }
            catch (err) {
                display_gridlines();
            };
        }
    }
}

function change_ui_theme() {
    theme = document.getElementById('ui_theme').value;
    localStorage.setItem("ui_theme", theme);

    if (theme == 'bright')
        colourmap = "haxby";
    else
        colourmap = "green";

    localStorage.setItem("v5_colourmap", colourmap);

    location.reload(); // was reload(false)
    //resizeMe() ;
}

function change_colourmap(index, recursive) {
    colourmap = document.getElementById('colourmap' + index).value;
    localStorage.setItem("v5_colourmap", colourmap);

    console.log("change_colourmap:", colourmap, "index:", index, "recursive:", recursive);

    if (imageContainer[index - 1] != null) {
        clear_webgl_image_buffers(index);
        clear_webgl_legend_buffers(index);
    }

    init_webgl_image_buffers(index);

    if (va_count == 1) {
        display_legend();
    }

    let fitsData = fitsContainer[index - 1];

    if (fitsData != null) {
        if (fitsData.depth > 1) {
            if (va_count == 1) {
                if (intensity_mode == "mean") {
                    plot_spectrum([fitsData.mean_spectrum]);
                    replot_y_axis();
                }

                if (intensity_mode == "integrated") {
                    plot_spectrum([fitsData.integrated_spectrum]);
                    replot_y_axis();
                }
            }
            else {
                if (intensity_mode == "mean") {
                    plot_spectrum(mean_spectrumContainer);
                    replot_y_axis();
                }

                if (intensity_mode == "integrated") {
                    plot_spectrum(integrated_spectrumContainer);
                    replot_y_axis();
                }
            }
        }
    }

    //change other datasets too
    if (va_count > 1 && recursive) {
        for (let i = 1; i <= va_count; i++)
            if (i != index) {
                document.getElementById('colourmap' + i).value = colourmap;
                change_colourmap(i, false);
            }
    }
}

function add_histogram_line(g, pos, width, height, offset, info, position, addLine, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;

    //slider size
    var side = 0.67 * emFontSize;
    //slider translation
    var d = [{ x: 0, width: width }];

    var min = fitsData.min;
    var max = fitsData.max;
    var x = (pos - min) / (max - min) * width;

    //console.log(info, pos, width);

    var flux_elem = d3.select("#flux_path" + index);
    flux_elem.attr(info, pos);

    function dropGroup(event, d) {
    }

    function dragGroup(event, d) {
        event.preventDefault = true;

        d.x += event.dx;
        d.x = Math.max(-x, d.x);
        d.x = Math.min(width - x - 1, d.x);

        var black = (parseFloat(flux_elem.attr("black")) - min) / (max - min) * width;
        var white = (parseFloat(flux_elem.attr("white")) - min) / (max - min) * width;

        if (document.getElementById('flux' + index).value != "logistic" && document.getElementById('flux' + index).value != "ratio") {
            switch (info) {
                case 'black':
                    d.x = Math.min(white - x - 1, d.x);
                    break;

                case 'white':
                    d.x = Math.max(black - x + 1, d.x);
                    break;

                default:
                    break;
            }
        }

        d3.select(this).attr("transform", "translate(" + d.x + "," + offset + ")");
        flux_elem.attr(info, ((x + d.x) / width * (max - min) + min));//transformed from pixels into server units

        var black, white, median;

        try {
            black = parseFloat(flux_elem.attr("black"));
        }
        catch (e) {
        };

        try {
            white = parseFloat(flux_elem.attr("white"));
        }
        catch (e) {
        };

        try {
            median = parseFloat(flux_elem.attr("median"));
        }
        catch (e) {
        };

        // set image tone mapping
        let image = imageContainer[index - 1];

        if (document.getElementById('flux' + index).value == "linear") {
            image.tone_mapping.black = black;
            image.tone_mapping.white = white;
        }

        if (document.getElementById('flux' + index).value == "legacy") {
            image.tone_mapping.black = black;
            image.tone_mapping.white = white;
        }

        if (document.getElementById('flux' + index).value == "logistic") {
            image.tone_mapping.median = median;
        }

        if (document.getElementById('flux' + index).value == "ratio") {
            image.tone_mapping.black = black;
        }

        if (document.getElementById('flux' + index).value == "square") {
            image.tone_mapping.black = black;
            image.tone_mapping.sensitivity = 1 / (white - black);
        }

        var multiplier = get_noise_sensitivity(noise_sensitivity);
        var path = get_flux_path(width, height, document.getElementById('flux' + index).value, black, white, median, multiplier, index);
        flux_elem.attr("d", path);

        if (composite_view) {
            image = compositeImage;
        }

        image.refresh = true;

        if (va_count == 1) {
            update_legend();
        } else {
            draw_rbg_legend(index);
        }
    }

    var group = g.data(d).append("g")
        .attr("id", info + "Group" + index)
        .style('cursor', 'move')
        .attr("transform", function (d) { return "translate(" + d.x + "," + offset + ")"; })
        .call(d3.drag()
            .on("drag", dragGroup)
            .on("end", dropGroup));

    if (addLine) {
        group.append("line")
            .attr("id", info + "Line" + index)
            .attr("x1", x)
            .attr("y1", (height - 1))
            .attr("x2", x)
            .attr("y2", 0)
            .style("stroke", "red")
            .style("stroke-width", emStrokeWidth)
            .style("stroke-dasharray", ("1, 5, 1"))
            .attr("opacity", 0.5);
    }

    var tHeight = (height - 1 - Math.sqrt(3) * side * 0.67);
    var points = "0,0 " + (2 * side) + ",0 " + (side) + "," + (-Math.sqrt(3) * side);

    if (position.includes('top')) {
        tHeight = (-Math.sqrt(3) * side * 0.33);
        points = "0," + (-Math.sqrt(3) * side) + " " + (2 * side) + "," + (-Math.sqrt(3) * side) + " " + (side) + ",0";
    }

    group.append("svg")
        .attr("id", info + "Slider" + index)
        .attr("x", (x - side))
        .attr("y", tHeight)
        .attr("viewBox", "0 " + (-Math.sqrt(3) * side) + " " + (2 * side) + " " + (Math.sqrt(3) * side))
        .attr("width", 2 * side)
        .attr("height", 2 * side)
        .append("polygon")
        .style("stroke", "none")
        .style("fill", "red")
        .attr("points", points);

    var txtHeight = (height - 1 - side);

    if (position.includes('top'))
        txtHeight = "1.25em";//0.75em

    if (position.includes('right')) {
        group.append("text")
            .attr("id", info + "Text" + index)
            .attr("x", (x + 0.5 * emFontSize))
            .attr("y", txtHeight)
            .attr("font-family", "Inconsolata")
            .attr("font-weight", "normal")
            .attr("font-style", "italic")
            .attr("font-size", "1em")
            .attr("text-anchor", "start")
            .attr("fill", "red")
            .attr("stroke", "none")
            .attr("opacity", 1.0)
            .text(info);
    }

    if (position.includes('left')) {
        group.append("text")
            .attr("id", info + "Text" + index)
            .attr("x", (x - 0.5 * emFontSize))
            .attr("y", txtHeight)
            .attr("font-family", "Inconsolata")
            .attr("font-weight", "normal")
            .attr("font-style", "italic")
            .attr("font-size", "1em")
            .attr("text-anchor", "end")
            .attr("fill", "red")
            .attr("stroke", "none")
            .attr("opacity", 1.0)
            .text(info);
    }
}

function get_tone_mapping(value, flux, black, white, median, multiplier, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    let sensitivity = multiplier * fitsData.sensitivity;
    let ratio_sensitivity = multiplier * fitsData.ratio_sensitivity;
    let lmin = fitsData.lmin;
    let lmax = fitsData.lmax;

    switch (flux) {
        case 'linear':
            return get_tone_mapping_linear(value, black, white);
        case 'legacy':
            return get_tone_mapping_legacy(value, black, white, lmin, lmax);
        case 'logistic':
            return get_tone_mapping_logistic(value, median, sensitivity);
        case 'ratio':
            return get_tone_mapping_value_ratio(value, black, ratio_sensitivity);
        case 'square':
            return get_tone_mapping_value_square(value, black, sensitivity);
        default:
            return NaN;
    }
}

function get_tone_mapping_value_square(value, black, sensitivity) {
    var pixel = (value - black) * sensitivity;

    if (pixel > 0.0)
        pixel = pixel * pixel;
    else
        pixel = 0.0;

    return clamp(255 * pixel, 0, 255);
}

function get_tone_mapping_logistic(value, median, sensitivity) {
    var pixel = 1.0 / (1.0 + Math.exp(-6.0 * (value - median) * sensitivity));

    return clamp(255 * pixel, 0, 255);
}

function get_tone_mapping_legacy(value, black, white, lmin, lmax) {
    var pixel = 0.5 + (value - black) / (white - black);

    if (pixel > 0.0)
        pixel = (Math.log(pixel) - lmin) / (lmax - lmin);
    else
        pixel = 0.0;

    return clamp(255 * pixel, 0, 255);
}

function get_tone_mapping_linear(value, black, white) {
    var slope = 1.0 / (white - black);
    var pixel = (value - black) * slope;

    return clamp(255 * pixel, 0, 255);
}

function get_tone_mapping_value_ratio(value, black, sensitivity) {
    var pixel = 5.0 * (value - black) * sensitivity;

    if (pixel > 0.0)
        pixel = pixel / (1.0 + pixel);
    else
        pixel = 0.0;

    return clamp(255 * pixel, 0, 255);
}

function get_pixel_flux(pixel, index) {
    var black, white, median, multiplier, flux;

    var flux_elem = d3.select("#flux_path" + index);

    try {
        flux = document.getElementById('flux' + index).value
    }
    catch (e) {
        console.log('flux not available yet');
        return NaN;
    };

    try {
        black = parseFloat(flux_elem.attr("black"));
    }
    catch (e) {
        console.log('black not available yet');
        return NaN;
    };

    try {
        white = parseFloat(flux_elem.attr("white"));
    }
    catch (e) {
        console.log('white not available yet');
        return NaN;
    };

    try {
        median = parseFloat(flux_elem.attr("median"));
    }
    catch (e) {
        console.log('median not available yet');
        return NaN;
    };

    multiplier = get_noise_sensitivity(noise_sensitivity);

    return get_flux(pixel, flux, black, white, median, multiplier, index);
}

function get_flux_value_legacy(value, black, white, multiplier) {
    var p = get_slope_from_multiplier(multiplier);
    var lmin = Math.log(p);
    var lmax = Math.log(p + 1.0);

    var tmp = Math.exp(lmin + value * (lmax - lmin)) - p;
    return black + tmp * (white - black);
}

function get_flux_value_log(value, black, white) {
    return black + (Math.exp(value) - 1) / (Math.E - 1) * (white - black);
}

function get_flux_value_linear(value, black, white) {
    return black + value * (white - black);
}

function get_flux_value_logistic(value, min, max, median, sensitivity) {
    if (value == 0)
        return min;

    if (value == 1)
        return max;

    return median - Math.log(1 / value - 1) / (6 * sensitivity);
}

function get_flux_value_ratio(value, max, black, sensitivity) {
    if (value == 1)
        return max;

    return black + value / (5 * sensitivity * (1 - value));
}

function get_flux_value_square(value, black, white) {
    return black + Math.sqrt(value) * (white - black);
}

function get_flux(value, flux, black, white, median, multiplier, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    let sensitivity = multiplier * fitsData.sensitivity;
    let ratio_sensitivity = multiplier * fitsData.ratio_sensitivity;
    var min = fitsData.min;
    var max = fitsData.max;

    switch (flux) {
        case 'linear':
            return get_flux_value_linear(value, black, white);
            break;
        case 'legacy':
            return get_flux_value_legacy(value, black, white, multiplier);
            break;
        case 'log':
            return get_flux_value_log(value, black, white);
            break;
        case 'logistic':
            return get_flux_value_logistic(value, min, max, median, sensitivity);
            break;
        case 'ratio':
            return get_flux_value_ratio(value, max, black, ratio_sensitivity);
            break;
        case 'square':
            return get_flux_value_square(value, black, white);
            break;
        default:
            return NaN;
            break;
    }
}

function get_flux_path_square(width, height, min, max, black, white, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    var lower = min + black / width * (max - min);
    var upper = min + white / width * (max - min);

    var sensitivity = 1 / (upper - lower);
    var multiplier = sensitivity / fitsData.sensitivity;
    noise_sensitivity = get_noise_sensitivity_from_multiplier(multiplier);

    var path = "M0 " + (emStrokeWidth + height - 1) + " L" + black + " " + (emStrokeWidth + height - 1);

    var segments = 100;
    var dx = (white - black) / segments;

    for (var x = black; x < white + dx; x += dx) {
        var y = height - 1;
        var tmp = min + x / width * (max - min);
        tmp = (tmp - lower) * sensitivity;

        var pixel = tmp * tmp;
        pixel = Math.max(0.0, Math.min(1.0, pixel));

        y *= (1.0 - pixel);
        path += " L" + x + " " + (emStrokeWidth + y);
    }

    path += " L" + width + " " + emStrokeWidth;

    return path;
}

function get_flux_path_ratio(width, height, min, max, black, multiplier, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    var sensitivity = multiplier * fitsData.ratio_sensitivity;
    var threshold = min + black / width * (max - min);

    var path = "M0 " + (emStrokeWidth + height - 1) + " L" + black + " " + (emStrokeWidth + height - 1);

    var segments = 100;
    var dx = (width - black) / segments;

    for (var x = black; x < width + dx; x += dx) {
        var y = height - 1;
        var tmp = min + x / width * (max - min);
        tmp = 5 * (tmp - threshold) * sensitivity;

        var pixel = tmp / (1 + tmp);
        pixel = Math.max(0.0, Math.min(1.0, pixel));

        y *= (1.0 - pixel);
        path += " L" + x + " " + (emStrokeWidth + y);
    }

    return path;
}

function get_flux_path_logistic(width, height, min, max, median, multiplier, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    var sensitivity = multiplier * fitsData.sensitivity;
    var threshold = min + median / width * (max - min);

    var tmp = (min - threshold) * sensitivity;
    var pixel = 1.0 / (1.0 + Math.exp(-6 * tmp));
    var path = "M0 " + (emStrokeWidth + (height - 1) * (1.0 - pixel));

    var segments = 100;
    var dx = width / segments;

    for (var x = dx; x < width + dx; x += dx) {
        var y = height - 1;
        var tmp = min + x / width * (max - min);
        tmp = (tmp - threshold) * sensitivity;

        var pixel = 1.0 / (1.0 + Math.exp(-6 * tmp));
        pixel = Math.max(0.0, Math.min(1.0, pixel));

        y *= (1.0 - pixel);
        path += " L" + x + " " + (emStrokeWidth + y);
    }

    return path;
}

function get_flux_path_log(width, height, min, max, black, white, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    var lower = min + black / width * (max - min);
    var upper = min + white / width * (max - min);

    var sensitivity = 0.5 * (Math.E - 1) / (upper - lower);
    var multiplier = sensitivity / fitsData.sensitivity;
    noise_sensitivity = get_noise_sensitivity_from_multiplier(multiplier);

    var path = "M0 " + (emStrokeWidth + height - 1) + " L" + black + " " + (emStrokeWidth + height - 1);

    var segments = 100;
    var dx = (white - black) / segments;

    for (var x = black; x < white + dx; x += dx) {
        var y = height - 1;
        var tmp = min + x / width * (max - min);
        tmp = (tmp - lower) * sensitivity;

        var pixel = (tmp > -0.5) ? Math.log(2.0 * tmp + 1.0) : 0.0;
        pixel = Math.max(0.0, Math.min(1.0, pixel));

        y *= (1.0 - pixel);
        path += " L" + x + " " + (emStrokeWidth + y);
    }

    path += " L" + width + " " + emStrokeWidth;

    return path;
}

function get_flux_path_legacy(width, height, black, white, multiplier) {
    var path = "M0 " + (emStrokeWidth + height - 1) + " L" + black + " " + (emStrokeWidth + height - 1);

    var segments = 100;
    var dx = (white - black) / segments;

    var p = get_slope_from_multiplier(multiplier);
    var lmin = Math.log(p);
    var lmax = Math.log(p + 1.0);//Math.log(1.5) ;

    //console.log("multiplier = ", multiplier, "p = ", p) ;

    for (var x = black; x < white + dx; x += dx) {
        var y = height - 1;
        var tmp = (x - black) / (white - black);

        //console.log("tmp = ", tmp) ;

        var pixel = (Math.log(p + tmp) - lmin) / (lmax - lmin);
        pixel = Math.max(0.0, Math.min(1.0, pixel));

        y *= (1.0 - pixel);
        path += " L" + x + " " + (emStrokeWidth + y);
    }

    path += " L" + width + " " + emStrokeWidth;

    return path;
}

function get_flux_path_linear(width, height, black, white) {
    var path = "M0 " + (emStrokeWidth + height - 1) + " L" + black + " " + (emStrokeWidth + height - 1);
    path += " L" + white + " " + emStrokeWidth;//0
    path += " L" + width + " " + emStrokeWidth;//0

    return path;
}

function get_flux_path(width, height, flux, black, white, median, multiplier, index) {
    let fitsData = imageContainer[index - 1].tone_mapping;
    var min = fitsData.min;
    var max = fitsData.max;

    var black = (black - min) / (max - min) * width;
    var white = (white - min) / (max - min) * width;
    var median = (median - min) / (max - min) * width;

    switch (flux) {
        case 'legacy':
            return get_flux_path_legacy(width, height, black, white, multiplier);
            break;

        case 'linear':
            return get_flux_path_linear(width, height, black, white);
            break;

        case 'log':
            return get_flux_path_log(width, height, min, max, black, white, index);
            break;

        case 'logistic':
            return get_flux_path_logistic(width, height, min, max, median, multiplier, index);
            break;

        case 'ratio':
            return get_flux_path_ratio(width, height, min, max, black, multiplier, index);
            break;

        case 'square':
            return get_flux_path_square(width, height, min, max, black, white, index);
            break;

        default:
            return "";
            break;
    };
}

function setup_histogram_interaction(index) {
    try {
        d3.select("#interaction" + index).remove();
    }
    catch (e) { };

    var c = document.getElementById("HistogramCanvas" + index);
    var svg = d3.select("#HistogramSVG" + index);

    var width = c.width;
    var height = c.height;
    var offset = parseFloat(svg.attr("offset"));

    var group = svg.append("g")
        .attr("id", "interaction" + index);

    let fitsData = imageContainer[index - 1].tone_mapping;

    //console.log("min:", fitsData.min, "max:", fitsData.max, "median:", fitsData.median, "black:", fitsData.black, "white:", fitsData.white);

    var flux = document.getElementById('flux' + index).value;
    var min = fitsData.min;
    var max = fitsData.max;
    var black = fitsData.black;
    var white = fitsData.white;
    var median = fitsData.median;
    var multiplier = get_noise_sensitivity(noise_sensitivity);

    if (flux == 'legacy') {
        black = min;
        white = max;

        noise_sensitivity = 100;
        document.getElementById('sensitivity' + index).value = noise_sensitivity;
        document.getElementById('sensitivityInput' + index).innerHTML = get_noise_sensitivity_string(noise_sensitivity, 2);
        multiplier = get_noise_sensitivity(noise_sensitivity);
    }
    else {
        black = fitsData.black;
        white = fitsData.white;
    };

    var path = get_flux_path(width, height, flux, black, white, median, multiplier, index);

    group.append("path")
        .attr("id", "flux_path" + index)
        .attr("black", black)
        .attr("white", white)
        .attr("median", median)
        .attr("width", width)
        .attr("height", height)
        .attr("transform", "translate(0, " + offset + ")")
        .style("stroke", "red")
        .style("stroke-width", emStrokeWidth)
        .style("stroke-dasharray", ("3, 3, 1, 3"))
        .style("fill", "none")
        .attr("d", path);

    switch (flux) {
        case 'legacy':
            add_histogram_line(group, black, width, height, offset, 'black', 'right', false, index);
            add_histogram_line(group, white, width, height, offset, 'white', 'top left', true, index);
            break;

        case 'linear':
            add_histogram_line(group, black, width, height, offset, 'black', 'right', false, index);
            add_histogram_line(group, white, width, height, offset, 'white', 'top right', true, index);
            break;

        case 'log':
            add_histogram_line(group, black, width, height, offset, 'black', 'right', false, index);
            add_histogram_line(group, white, width, height, offset, 'white', 'top right', true, index);
            break;

        case 'ratio':
            add_histogram_line(group, black, width, height, offset, 'black', 'right', false, index);
            break;

        case 'square':
            add_histogram_line(group, black, width, height, offset, 'black', 'right', false, index);
            add_histogram_line(group, white, width, height, offset, 'white', 'top right', true, index);
            break;

        default:
            add_histogram_line(group, median, width, height, offset, 'median', 'right', true, index);
            break;
    };
}

function update_contours() {
    display_hourglass();

    setTimeout(function () {
        contour_surface();
        /*hide_hourglass() ;*/
    }, 0);//50, 100
}

function redraw_histogram(index) {
    let fitsData = fitsContainer[index - 1];
    var histogram = fitsData.histogram;

    if (histogram === undefined) {
        console.error("an undefined histogram was found");
        return;
    }

    var c = document.getElementById("HistogramCanvas" + index);
    var ctx = c.getContext("2d");

    var width = c.width;
    var height = c.height;

    ctx.clearRect(0, 0, width, height);
    ctx.fillStyle = "rgba(0, 0, 0, 0.8)";

    var nbins = histogram.length;
    var dx = width / nbins;

    var dmin = d3.min(histogram);
    var dmax = d3.max(histogram);

    if (dmin > 0.0)
        dmin = Math.log(dmin);

    if (dmax > 0.0)
        dmax = Math.log(dmax);

    var binH;
    var binV;

    for (var i = 0; i < nbins; i++) {
        binV = histogram[i] > 0.0 ? Math.log(histogram[i]) : 0.0;
        binH = (binV - dmin) / (dmax - dmin) * (height - 1);
        ctx.fillRect(i * dx, height - 1, dx, -binH);

        if (histogram[i] > 0.0)
            ctx.fillRect(i * dx, height - 1, dx, height - 1);
    };

    setup_histogram_interaction(index);
}

function display_preferences(index) {
    if (has_preferences)
        return;

    let fitsData = fitsContainer[index - 1];//va_count

    if (fitsData == null) {
        console.log("display_preferences: NULL fitsData.");
        return;
    }
    else
        console.log("display_preferences: fitsData OK.");

    var svg = d3.select("#BackSVG");
    var svgWidth = parseFloat(svg.attr("width"));
    var svgHeight = parseFloat(svg.attr("height"));
    var offset = 2.0 * emFontSize;

    //show ping
    var group = svg.append("g")
        .attr("id", "pingGroup");

    group.append("text")
        .attr("id", "heartbeat")
        .attr('class', 'fas')
        .attr("x", emFontSize / 4)
        //.attr("y", offset)//"0.75em")
        .attr("y", (svgHeight - 0.67 * offset))
        .attr("font-family", "Helvetica")//Helvetica
        //.attr("font-size", "0.75em")
        .attr("font-size", "1.5em")
        .attr("text-anchor", "start")
        .attr("fill", "grey")
        .attr("stroke", "none")
        .attr("opacity", 0.0)
        .text("");

    let fillColour = 'yellow';

    if (theme == 'bright')
        fillColour = 'black';

    let bottomY = svgHeight - offset / 4;

    group.append("text")
        .attr("id", "latency")
        .attr("x", (0 * emFontSize / 4 + 0 * 1.75 * emFontSize))
        //.attr("y", offset)//"0.85em")
        .attr("y", bottomY)
        .attr("font-family", "Inconsolata")
        //.attr("font-weight", "bold")
        .attr("font-size", "0.75em")//0.75 Helvetica
        .attr("text-anchor", "start")
        .attr("fill", fillColour)
        .attr("stroke", "none")
        .attr("opacity", 0.75)
        .text("");


    // show cluster nodes
    /*svg.append("text")
      .attr("id", "cluster")
      .attr("x", svgWidth)
      .attr("y", "0.85em")
      .attr("font-family", "Inconsolata")
      .attr("font-weight", "normal")
      .attr("font-size", "0.75em")
      .attr("text-anchor", "end")
      .attr("fill", fillColour)
      .attr("stroke", "none")
      .attr("opacity", 0.75)
      .text("");*/

    // a cluster element
    {
        var svg = d3.select("#ClusterSVG");
        var svgWidth = parseFloat(svg.attr("width"));
        var svgHeight = parseFloat(svg.attr("height"));

        svg.append("g")
            .append("foreignObject")
            .attr("x", 0)
            .attr("y", 0)
            .attr("width", svgWidth)
            // .attr("height", 1.0 * emFontSize)
            .attr("height", 1.0 * svgHeight)
            .attr("fill", fillColour)
            .attr("stroke", "none")
            .append("xhtml:div")
            .attr("id", "cluster")
            .attr("class", "cluster")
            .html("");
    }

    var range = get_axes_range(svgWidth, svgHeight);

    group.append("text")
        .attr("id", "fps")
        .attr("x", range.xMax - 0.25 * emFontSize)
        //.attr("y", offset)
        .attr("y", bottomY)
        .attr("font-family", "Inconsolata")
        //.attr("font-weight", "bold")
        .attr("font-size", "0.75em")//0.75 Helvetica
        .attr("text-anchor", "end")
        .attr("fill", fillColour)
        .attr("stroke", "none")
        .attr("opacity", 0.75)
        .text("");

    var prefDropdown = d3.select("#prefDropdown");

    var htmlStr = autoscale ? '<span class="fas fa-check-square"></span> autoscale y-axis' : '<span class="far fa-square"></span> autoscale y-axis';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "autoscale")
        .style('cursor', 'pointer')
        .on("click", function () {
            autoscale = !autoscale;
            //localStorage_write_boolean("autoscale", autoscale) ;

            d3.select("#yaxis")
                .style("fill", "white")
                .style("stroke", "white")
                .transition()
                .duration(500)
                .style("fill", axisColour)
                .style("stroke", axisColour);

            if (!autoscale)
                set_autoscale_range(true);
            else
                enable_autoscale();
        })
        .html(htmlStr);

    var htmlStr = displayDownloadConfirmation ? '<span class="fas fa-check-square"></span> download confirmation' : '<span class="far fa-square"></span> download confirmation';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "download_confirmation")
        .style('cursor', 'pointer')
        .on("click", function () {
            displayDownloadConfirmation = !displayDownloadConfirmation;
            localStorage_write_boolean("displayDownloadConfirmation", displayDownloadConfirmation);
            var htmlStr = displayDownloadConfirmation ? '<span class="fas fa-check-square"></span> download confirmation' : '<span class="far fa-square"></span> download confirmation';
            d3.select(this).html(htmlStr);
        })
        .html(htmlStr);

    var htmlStr = displayScalingHelp ? '<span class="fas fa-check-square"></span> display pop-up help' : '<span class="far fa-square"></span> display pop-up help';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "display_scaling_help")
        .style('cursor', 'pointer')
        .on("click", function () {
            displayScalingHelp = !displayScalingHelp;
            localStorage_write_boolean("displayScalingHelp", displayScalingHelp);
            var htmlStr = displayScalingHelp ? '<span class="fas fa-check-square"></span> display pop-up help' : '<span class="far fa-square"></span> display pop-up help';
            d3.select(this).html(htmlStr);
        })
        .html(htmlStr);

    var htmlStr = peak_tracking ? '<span class="fas fa-check-square"></span> automatic peak tracking' : '<span class="far fa-square"></span> automatic peak tracking';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "peak_tracking")
        .style('cursor', 'pointer')
        .on("click", function () {
            peak_tracking = !peak_tracking;
            localStorage_write_boolean("peak_tracking", peak_tracking);
            var htmlStr = peak_tracking ? '<span class="fas fa-check-square"></span> automatic peak tracking' : '<span class="far fa-square"></span> automatic peak tracking';
            d3.select(this).html(htmlStr);
        })
        .html(htmlStr);

    var htmlStr = realtime_spectrum ? '<span class="fas fa-check-square"></span> realtime spectrum updates' : '<span class="far fa-square"></span> realtime spectrum updates';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "realtime_spectrum")
        .style('cursor', 'pointer')
        .on("click", function () {
            realtime_spectrum = !realtime_spectrum;
            localStorage_write_boolean("realtime_spectrum", realtime_spectrum);
            var htmlStr = realtime_spectrum ? '<span class="fas fa-check-square"></span> realtime spectrum updates' : '<span class="far fa-square"></span> realtime spectrum updates';
            d3.select(this).html(htmlStr);
        })
        .html(htmlStr);

    var htmlStr = realtime_video ? '<span class="fas fa-check-square"></span> realtime video updates' : '<span class="far fa-square"></span> realtime video updates';
    prefDropdown.append("li")
        .append("a")
        .attr("id", "realtime_video")
        .style('cursor', 'pointer')
        .on("click", function () {
            realtime_video = !realtime_video;
            localStorage_write_boolean("realtime_video", realtime_video);
            var htmlStr = realtime_video ? '<span class="fas fa-check-square"></span> realtime video updates' : '<span class="far fa-square"></span> realtime video updates';
            d3.select(this).html(htmlStr);

            if (realtime_video) {
                d3.select('#video_fps_control_li').style("display", "block");
            }
            else {
                d3.select('#video_fps_control_li').style("display", "none");
            }
        })
        .html(htmlStr);

    //----------------------------------------
    var tmpA;

    tmpA = prefDropdown.append("li")
        .attr("id", "image_quality_li")
        //.style("background-color", "#FFF")
        .append("a")
        .style("class", "form-group")
        .attr("class", "form-horizontal");

    tmpA.append("label")
        .attr("for", "image_quality")
        .attr("class", "control-label")
        .html("image quality:&nbsp; ");

    tmpA.append("select")
        .attr("id", "image_quality")
        .attr("onchange", "javascript:change_image_quality();")
        .html("<option>high</option><option>medium</option><option>low</option>");

    document.getElementById('image_quality').value = image_quality;

    if (realtime_video) {
        d3.select('#video_fps_control_li').style("display", "block");
    }
    else {
        d3.select('#video_fps_control_li').style("display", "none");
    }

    //----------------------------------------
    tmpA = prefDropdown.append("li")
        .attr("id", "binning_li")
        //.style("background-color", "#FFF")
        .append("a")
        .style("class", "form-group")
        .attr("class", "form-horizontal");

    tmpA.append("label")
        .attr("for", "binning")
        .attr("class", "control-label")
        .html("spectrum binning:&nbsp; ");

    tmpA.append("select")
        .attr("id", "binning")
        .attr("onchange", "javascript:change_binning();")
        .html("<option value='1'>1:1</option><option value='2'>1:2</option><option value='4'>1:4</option><option value='8'>1:8</option><option value='16'>1:16</option>");

    document.getElementById('binning').value = binning.toString();


    //----------------------------------------
    tmpA = prefDropdown.append("li")
        .attr("id", "video_fps_control_li")
        //.style("background-color", "#FFF")
        .append("a")
        .style("class", "form-group")
        .attr("class", "form-horizontal");

    tmpA.append("label")
        .attr("for", "video_fps_control")
        .attr("class", "control-label")
        .html("video fps control:&nbsp; ");

    tmpA.append("select")
        .attr("id", "video_fps_control")
        .attr("onchange", "javascript:change_video_fps_control();")
        .html("<option value='auto'>auto</option><option value='5'>5 fps</option><option value='10'>10 fps</option><option value='20'>20 fps</option><option value='30'>30 fps</option>");

    document.getElementById('video_fps_control').value = video_fps_control;

    if (realtime_video) {
        d3.select('#video_fps_control_li').style("display", "block");
    }
    else {
        d3.select('#video_fps_control_li').style("display", "none");
    }

    //ui_theme
    {
        tmpA = prefDropdown.append("li")
            //.style("background-color", "#FFF")
            .append("a")
            .style("class", "form-group")
            .attr("class", "form-horizontal");

        tmpA.append("label")
            .attr("for", "ui_theme")
            .attr("class", "control-label")
            .html("ui theme:&nbsp; ");

        tmpA.append("select")
            //.attr("class", "form-control")
            .attr("id", "ui_theme")
            .attr("onchange", "javascript:change_ui_theme();")
            .html("<option>dark</option><option>bright</option>");

        document.getElementById('ui_theme').value = theme;
    }

    //coords_fmt
    {
        tmpA = prefDropdown.append("li")
            //.style("background-color", "#FFF")
            .append("a")
            .attr("id", "coords_fmt_li")
            .style("class", "form-group")
            .attr("class", "form-horizontal");

        tmpA.append("label")
            .attr("for", "coords_fmt")
            .attr("class", "control-label")
            .html("RA (<i>α</i>) display:&nbsp; ");

        tmpA.append("select")
            //.attr("class", "form-control")
            .attr("id", "coords_fmt")
            .attr("onchange", "javascript:change_coords_fmt();")
            .html("<option>HMS</option><option>DMS</option>");

        document.getElementById('coords_fmt').value = coordsFmt;
    }

    tmpA = prefDropdown.append("li")
        .attr("id", "contour_control_li")
        //.style("background-color", "#FFF")
        .append("a")
        .style("class", "form-group")
        .attr("class", "form-horizontal");

    tmpA.append("label")
        .attr("for", "contour_lines")
        .attr("class", "control-label")
        .html("#contour levels:&nbsp; ");

    previous_contour_lines = 5;

    tmpA.append("input")
        //.attr("class", "form-control")
        .attr("id", "contour_lines")
        .attr("type", "number")
        .style("width", "3em")
        .attr("min", 1)
        .attr("step", 1)
        .attr("value", previous_contour_lines);
    //.attr("onchange", "javascript:update_contours();");

    var elem = document.getElementById('contour_lines');
    elem.onblur = validate_contour_lines;
    elem.onmouseleave = validate_contour_lines;
    elem.onkeyup = function (e) {
        var event = e || window.event;
        var charCode = event.which || event.keyCode;

        if (charCode == '13') {
            // Enter pressed
            validate_contour_lines();
            return false;
        }
    }

    if (displayContours) {
        d3.select('#contour_control_li').style("display", "block");
    }
    else {
        d3.select('#contour_control_li').style("display", "none");
    }

    //----------------------------------------
    if (fitsData.depth > 1) {
        tmpA = prefDropdown.append("li")
            //.style("background-color", "#FFF")
            .append("a")
            .attr("id", "intensity_mode_li")
            .style("class", "form-group")
            .attr("class", "form-horizontal");

        tmpA.append("label")
            .attr("for", "intensity_mode")
            .attr("class", "control-label")
            .html("intensity mode:&nbsp; ");

        tmpA.append("select")
            .attr("id", "intensity_mode")
            .attr("onchange", "javascript:change_intensity_mode();")
            .html("<option>mean</option><option>integrated</option>");

        document.getElementById('intensity_mode').value = intensity_mode;
    }

    tmpA = prefDropdown.append("li")
        //.style("background-color", "#FFF")
        .append("a")
        .attr("id", "zoom_shape_li")
        .style("class", "form-group")
        .attr("class", "form-horizontal");

    tmpA.append("label")
        .attr("for", "zoom_shape")
        .attr("class", "control-label")
        .html("zoom shape:&nbsp; ");

    tmpA.append("select")
        //.attr("class", "form-control")
        .attr("id", "zoom_shape")
        .attr("onchange", "javascript:change_zoom_shape();")
        .html("<option>circle</option><option>square</option>");

    document.getElementById('zoom_shape').value = zoom_shape;
    //----------------------------------------

    has_preferences = true;
}

function display_histogram(index) {
    let fitsData = fitsContainer[index - 1];
    let imageData = imageContainer[index - 1];

    if (fitsData == null) {
        console.log("[display_histogram] NULL fitsData.");
        return;
    }
    else
        console.log("[display_histogram] fitsData OK.");

    if (imageData == null) {
        console.log("[display_histogram] NULL imageData.");
        return;
    }
    else
        console.log("[display_histogram] imageData OK.");

    var imageDropdown = d3.select("#imageDropdown");

    //add multiple panes
    if (va_count > 1) {
        if ($("#imageDropdown li").length == 0) {
            var ul = imageDropdown
                .append("li")
                .append("ul")
                .attr("class", "nav nav-tabs")
                .style("background-color", "#FFF");

            for (let index = 1; index <= va_count; index++) {
                let classStr = '';

                if (index == 1)
                    classStr = 'active';

                var li = ul.append("li")
                    .attr("class", classStr);

                var a = li.append("a")
                    .attr("id", "imageTag#" + index)
                    .attr("data-toggle", "tab")
                    .attr("href", "#image" + index)
                    .style("font-weight", "bold")
                    .html(datasetId[index - 1]);
            }

            var div = imageDropdown.append("li")
                .append("div")
                .attr("class", "tab-content form-group");

            for (let index = 1; index <= va_count; index++) {
                let classStr = 'tab-pane fade';

                if (index == 1)
                    classStr += ' in active';

                var tab = div.append("div")
                    .attr("id", "image" + index)
                    .attr("class", classStr);
            }
        }

        imageDropdown = d3.select("#image" + index)
            .append("ul")
            .style("list-style", "none")
            .style("list-style-type", "none")
            .style("list-style-position", "outside")
            .style("list-style-image", "none")
            .style("text-align", "left")
            .style("padding-left", "0");
    }

    var colourmap_string = "<option>amber</option><option>red</option><option>green</option><option>blue</option><option>greyscale</option><option>negative</option><option disabled>---</option><option>cubehelix</option><option>haxby</option><option>hot</option><option>parula</option><option>rainbow</option><option disabled>---</option><option>inferno</option><option>magma</option><option>plasma</option><option>viridis</option>";

    tmpA = imageDropdown.append("li")
        //.style("background-color", "#FFF")
        .append("a")
        .style("class", "form-group")
        .attr("class", "form-horizontal custom");

    if (!composite_view) {
        tmpA.append("label")
            .attr("for", "colourmap" + index)
            .attr("class", "control-label")
            .html("colourmap:&nbsp; ");

        tmpA.append("select")
            .attr("id", "colourmap" + index)
            .attr("onchange", "javascript:change_colourmap(" + index + ",true);")
            .html(colourmap_string);

        document.getElementById('colourmap' + index).value = colourmap;
    }

    tmpA = imageDropdown.append("li")
        //.style("background-color", "#FFF")
        .append("a")
        .attr("class", "form-group")
        .attr("class", "form-horizontal custom");

    tmpA.append("label")
        .attr("for", "flux" + index)
        .attr("class", "control-label")
        .html("tone mapping:&nbsp; ");

    tmpA.append("select")
        //.attr("class", "form-control")
        .attr("id", "flux" + index)
        .attr("onchange", "javascript:image_count=0;change_tone_mapping(" + index + ",true);")
        .html("<option value='linear'>linear</option><option value='legacy'>logarithmic</option><option value='logistic'>logistic</option><option value='ratio'>ratio</option><option value='square'>square</option>");//<option value='log'>log</option>

    document.getElementById('flux' + index).value = imageData.tone_mapping.flux;
    //document.querySelectorAll('[value="' + fitsData.flux + '"]')[0].text = fitsData.flux + ' (default)' ;

    var display;
    var tone_mapping = imageData.tone_mapping;

    if (tone_mapping.flux == 'linear' || tone_mapping.flux == 'square')
        display = "none";
    else
        display = "block";

    tmpA = imageDropdown.append("li")
        //.style("background-color", "#FFF")
        .append("a")
        .attr("id", "sensitivitySlider" + index)
        .attr("class", "form-group")
        .style("display", display)
        .attr("class", "form-horizontal custom");

    tmpA.append("label")
        .attr("for", "sensitivity" + index)
        .attr("class", "control-label")
        .html('image noise sensitivity:&nbsp; <span id="sensitivityInput' + index + '">' + get_noise_sensitivity_string(noise_sensitivity, 2) + "</span>");

    tmpA.append("input")
        //.attr("class", "form-control")
        .attr("id", "sensitivity" + index)
        .attr("class", "slider")
        .attr("type", "range")
        .attr("min", "0")
        .attr("max", "100")
        .attr("step", "1")
        .attr("value", noise_sensitivity)
        .attr("onmousemove", "javascript:change_noise_sensitivity(" + index + ");")
        .attr("onchange", "javascript:change_noise_sensitivity(" + index + ");");// was true

    var mainRect = document.getElementById('mainDiv').getBoundingClientRect();
    var width = 0.33 * mainRect.width;
    var height = width / (1 + Math.sqrt(2));

    //histogram part
    var histLI = imageDropdown.append("li");

    var histDiv = histLI.append("div")
        .attr("id", "histogram" + index)
        .attr('style', 'width:' + (width + 2 * emFontSize) + 'px; height:' + (height) + 'px;');
    //.style("width", (width+2*emFontSize))
    //.style("height", height);
    //.attr('style', 'position: fixed');

    var histWidth = width - 2 * emFontSize;//0.75
    var histHeight = height - 2 * emFontSize;
    var svgWidth = histWidth;
    var svgHeight = height;

    histDiv.append("canvas")
        .attr("id", "HistogramCanvas" + index)
        .attr("width", histWidth)
        .attr("height", histHeight)
        .attr('style', 'position: relative; left: 1em; top: 1em;');
    //.style("background-color", "#FFF")
    //.style("background-color", "rgba(0,0,0,0.4)");

    histDiv.append("svg")
        .attr("id", "HistogramSVG" + index)
        .attr("width", (svgWidth + 2 * emFontSize))
        .attr("height", svgHeight)
        .attr("offset", 0.75 * emFontSize)
        //.attr('style', 'position: relative; left: 1em; top: 0em; pointer-events: auto')
        .attr('style', 'position: relative; left: 1em; top: ' + (-histHeight + 0 * emFontSize) + 'px; pointer-events: auto')
        //.style('top', (-histHeight + 0*emFontSize));
        .on("dblclick", function () {
            if (isLocal)
                return;

            d3.select(this)
                .attr("opacity", 0.0)
                .transition()
                .duration(250)
                .attr("opacity", 1.0);

            var strRequest = '[vote] datasetId=' + datasetId + '&flux=' + document.getElementById('flux' + index).value;
            wsConn[index - 1].send(strRequest);
        });

    if (va_count > 1) {
        var imageTag = document.getElementById('imageTag#' + index);

        let line = fitsData.LINE.trim();
        let filter = fitsData.FILTER.trim();

        if (line != "")
            imageTag.innerHTML = plain2chem(line, true);

        if (filter != "")
            imageTag.innerHTML = filter;
    }

    redraw_histogram(index);
}

function Einstein_velocity_addition(v1, v2) {
    var c = 299792.458;//speed of light [km/s]

    return (v1 + v2) / (1 + v1 * v2 / (c * c));
}

function Einstein_relative_velocity(f, f0) {
    var c = 299792.458;//speed of light [km/s]

    var deltaV = 0.0;

    try {
        deltaV = document.getElementById('velocityInput').valueAsNumber;//[km/s]
    }
    catch (e) {
        console.log("USER_DELTAV = ", USER_DELTAV);
    }

    //convert redshift z to V
    var value = sessionStorage.getItem("redshift");

    if (value == "z") {
        var tmp = - (1.0 - (1.0 + deltaV) * (1.0 + deltaV)) / (1.0 + (1.0 + deltaV) * (1.0 + deltaV));

        deltaV = tmp * c;
    };

    var fRatio = f / f0;
    var v = (1.0 - fRatio * fRatio) / (1.0 + fRatio * fRatio) * c;

    return Einstein_velocity_addition(v, deltaV);
}

function relativistic_rest_frequency(f) {
    var c = 299792.458;//speed of light [km/s]

    var v = document.getElementById('velocityInput').valueAsNumber;//[km/s]

    var beta = v / c;

    //convert redshift z to V
    var value = sessionStorage.getItem("redshift");

    if (value == "z")
        beta = - (1.0 - (1.0 + v) * (1.0 + v)) / (1.0 + (1.0 + v) * (1.0 + v));

    var tmp = Math.sqrt((1.0 + beta) / (1.0 - beta));

    return f * tmp;
};

function get_spectrum_margin() {
    return 0.1;
}

function dragstart(event) {
    freqdrag = true;
    event.preventDefault = true;

    var offset = d3.pointer(event);
    freq_mouse_start = offset[0];

    var frequency = get_mouse_frequency(offset);

    if (optical_view)
        session_freq_start = frequency;

    if (has_frequency_info) {
        if (frequency > 0.0)
            session_freq_start = frequency;
    }
    else
        if (has_velocity_info)
            session_freq_start = frequency;

    session_freq_end = session_freq_start;//added by Chris on 2018/12/04

    //session_frame_start = get_mouse_frame(offset) ;

    if (has_frequency_info)
        console.log("drag started", freqdrag, freq_mouse_start, (frequency / 1e9).toPrecision(7), "GHz");
    else
        if (has_velocity_info)
            console.log("drag started", freqdrag, freq_mouse_start, (frequency / 1e3).toPrecision(5), "km/s");

    d3.select("#fregion").moveToFront();
}

function dragend() {
    console.log("drag ended");
    freqdrag = false;

    d3.select("#fregion").attr("opacity", 0.0);
    freq_mouse_start = 0;

    d3.select("#fregion").moveToBack();

    d3.select("#freq_bar").attr("opacity", 0.0);

    var freq_start = session_freq_start;
    var freq_end = session_freq_end;
    var tmp = freq_start;

    if (freq_start == freq_end) {
        console.log("ignoring a single-channel region selection!");

        freq_mouse_start = 0;
        freqdrag = false;
        session_freq_start = 0;
        session_freq_end = 0;

        shortcut.remove("f");
        shortcut.remove("Left");
        shortcut.remove("Right");
        shortcut.remove("Enter");
        mol_pos = -1;

        return;
    }

    if (freq_end < freq_start) {
        freq_start = freq_end;
        freq_end = tmp;
    };

    data_band_lo = freq_start;
    data_band_hi = freq_end;

    frame_start = session_frame_start;
    frame_end = session_frame_end;
    //recalculate {data_band_lo,data_band_hi} based on {frame_start,frame_end}

    //if((freq_start > 0.0) && (freq_end > 0.0))
    // if((frame_start >= 0) && (frame_end >= 0))
    {
        display_hourglass();

        image_count = 0;
        viewport_count = 0;
        spectrum_count = 0;

        for (let index = 1; index <= va_count; index++)
            cube_refresh(index);

        display_molecules();
    }

    freq_mouse_start = 0;
    freqdrag = false;
    session_freq_start = 0;
    session_freq_end = 0;

    shortcut.remove("f");
    shortcut.remove("Left");
    shortcut.remove("Right");
    shortcut.remove("Enter");
    mol_pos = -1;
}

function dragmove(event) {
    var offset = d3.pointer(event);
    var frequency = get_mouse_frequency(offset);

    var freq = d3.select("#frequency");
    var offsetx = parseFloat(freq.attr("x"));

    //console.log("dragmove", frequency.toPrecision(7)) ;

    var x1 = offsetx;
    var x2 = offsetx + parseFloat(freq.attr("width"));
    var x = offset[0];

    if (x < x1) x = x1;
    if (x > x2) x = x2;

    d3.select("#freq_bar").attr("x1", x).attr("x2", x).attr("opacity", 1.0);

    var fregion = d3.select("#fregion");
    var mouseBegin = freq_mouse_start;
    var mouseEnd = offset[0];

    if (mouseEnd < mouseBegin) {
        var mouseTmp = mouseBegin;
        mouseBegin = mouseEnd;

        if (mouseBegin < x1)
            mouseBegin = x1;

        mouseEnd = mouseTmp;
    };

    if (mouseBegin < x1)
        mouseBegin = x1;

    if (mouseEnd > x2)
        mouseEnd = x2;

    var mouseWidth = mouseEnd - mouseBegin;

    fregion.attr("x", mouseBegin).attr("width", mouseWidth).attr("opacity", 0.25);

    if (optical_view)
        session_freq_end = frequency;

    if (has_frequency_info) {
        if (frequency > 0.0)
            session_freq_end = frequency;
    }
    else
        if (has_velocity_info)
            session_freq_end = frequency;

    session_frame_end = get_mouse_frame(offset);

    var freq_start = session_freq_start;
    var freq_end = session_freq_end;
    var tmp = freq_start;

    if (freq_end < freq_start) {
        freq_start = freq_end;
        freq_end = tmp;
    };

    if (has_frequency_info)
        console.log((freq_start / 1e9).toPrecision(7) + " - " + (freq_end / 1e9).toPrecision(7) + " GHz");
    else
        if (has_velocity_info)
            console.log((freq_start / 1e3).toPrecision(5) + " - " + (freq_end / 1e3).toPrecision(5) + " km/s");

    var checkbox = document.getElementById('restcheckbox');

    try {
        if (checkbox.checked)
            frequency = relativistic_rest_frequency(frequency);
    }
    catch (e) {
        if (has_velocity_info)
            d3.select("#jvoText").text((frequency / 1.0e3).toFixed(getVelocityPrecision()) + " km/s");
    };

    if (optical_view)
        d3.select("#jvoText").text(Math.round(frequency));

    if (has_frequency_info) {
        var relvel = Einstein_relative_velocity(frequency, RESTFRQ);

        d3.select("#jvoText").text((frequency / 1.0e9).toPrecision(7) + " " + 'GHz' + ", " + relvel.toFixed(getVelocityPrecision()) + " km/s");
    }
}

function decimalPlaces(num) {
    var match = ('' + num).match(/(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/);
    if (!match) { return 0; }
    return Math.max(
        0,
        // Number of digits right of decimal point.
        (match[1] ? match[1].length : 0)
        // Adjust for scientific notation.
        - (match[2] ? +match[2] : 0));
}

function getFrequencyPrecision() {
    let fitsData = fitsContainer[va_count - 1];

    if (has_velocity_info) {
        return 7;
    }

    if (has_frequency_info) {
        let dF = fitsData.CDELT3 / 1e9;//[GHz]
        //console.log("dF = ", dF, "decimal = ", decimalPlaces(dF));

        return decimalPlaces(dF);
    };

    return 7;
}

function getVelocityPrecision() {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return 1;

    if (has_velocity_info) {
        let dV = fitsData.CDELT3 / 1000;//[km/s]
        //console.log("dV = ", dV, "decimal = ", decimalPlaces(dV)) ;

        let dec = decimalPlaces(dV);
        //return dec ;

        //add an override, some Star Formation FITS files use too many decimal places in CDELT3
        if (dec > 10)
            return 2;
        else
            return dec;
    };

    if (has_frequency_info) {
        /*let dF = fitsData.CDELT3 ;
        let dV = Einstein_relative_velocity(RESTFRQ+dF, RESTFRQ) ;//[km/s]
        console.log("dV = ", dV, "decimal = ", decimalPlaces(dV)) ;*/

        return 2;
    };

    return 1;
}

function getMinMaxVel(fitsData) {
    var vMin = null;
    var vMax = null;

    if (has_frequency_info) {
        vMin = Einstein_relative_velocity(data_band_lo, RESTFRQ);
        vMax = Einstein_relative_velocity(data_band_hi, RESTFRQ);
        //console.log("setup_axes:", "vMin = ", vMin, "vMax = ", vMax);
    }
    else {
        if (has_velocity_info) {
            vMin = data_band_lo / 1000;//[km/s]
            vMax = data_band_hi / 1000;//[km/s]
        }
        else {
            vMin = fitsData.CRVAL3 + fitsData.CDELT3 * (1 - fitsData.CRPIX3);
            vMin /= 1000;//[km/s]

            vMax = fitsData.CRVAL3 + fitsData.CDELT3 * (fitsData.depth - fitsData.CRPIX3);
            vMax /= 1000;//[km/s]
        }
    }

    return { vMin: vMin, vMax: vMax };
}

function composite_data_min_max() {
    data_min = Number.MAX_VALUE;
    data_max = - Number.MAX_VALUE;

    for (let i = 0; i < va_count; i++) {
        let spectrum = null;
        let fitsData = fitsContainer[i];
        let scale = spectrum_scale[i];

        if (intensity_mode == "mean")
            spectrum = fitsData.mean_spectrum;

        if (intensity_mode == "integrated")
            spectrum = fitsData.integrated_spectrum;

        data_min = Math.min(data_min, scale * d3.min(spectrum));
        data_max = Math.max(data_max, scale * d3.max(spectrum));
    }
}

function setup_csv_export() {
    var elem = document.getElementById('exportCSV');

    if (elem == null)
        return;

    elem.onclick = function () {
        console.log("export spectrum to CSV.");

        var c = 299792.458;//speed of light [km/s]

        var deltaV = 0.0;

        try {
            deltaV = document.getElementById('velocityInput').valueAsNumber;//[km/s]
        }
        catch (e) {
            console.log(e);
            console.log("USER_DELTAV = ", USER_DELTAV);
        }

        //convert redshift z to V
        var value = sessionStorage.getItem("redshift");

        if (value == "z") {
            var tmp = - (1.0 - (1.0 + deltaV) * (1.0 + deltaV)) / (1.0 + (1.0 + deltaV) * (1.0 + deltaV));

            deltaV = tmp * c;
        };

        var checkbox = document.getElementById('restcheckbox');
        var rest = false;

        try {
            rest = checkbox.checked;
        } catch (e) {
            console.log(e);
        }

        display_hourglass();

        for (let index = 0; index < va_count; index++) {
            // a CSV websocket request
            var request = {
                type: "spectrum",
                ra: d3.select("#ra").text().toString(),
                dec: d3.select("#dec").text().toString(),
                intensity: intensity_mode,
                frame_start: data_band_lo,
                frame_end: data_band_hi,
                ref_freq: RESTFRQ,
                deltaV: 1000.0 * deltaV, // [m/s]
                rest: rest,
                seq_id: sent_seq_id,
                timestamp: performance.now(),
            };

            if (wsConn[index].readyState == 1)
                wsConn[index].send(JSON.stringify(request));
        }

        setup_window_timeout();
    };
}

function setup_axes() {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData.depth <= 1)
        return;

    try {
        d3.select("#axes").remove();
        d3.select("#foreignCSV").remove();
    }
    catch (e) {
    }

    var svg = d3.select("#BackSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    svg = svg.append("g").attr("id", "axes");

    var velInfo = getMinMaxVel(fitsData);
    var vMin = velInfo.vMin;
    var vMax = velInfo.vMax;

    if (va_count == 1) {
        var spectrum = null;

        if (intensity_mode == "mean")
            spectrum = fitsData.mean_spectrum;

        if (intensity_mode == "integrated")
            spectrum = fitsData.integrated_spectrum;

        data_min = d3.min(spectrum);
        data_max = d3.max(spectrum);
    }
    else
        composite_data_min_max();

    var dmin = data_min;//d3.min(spectrum) ;
    var dmax = data_max;//d3.max(spectrum) ;

    if (dmin == dmax) {
        if (dmin == 0.0 && dmax == 0.0) {
            dmin = -1.0;
            dmax = 1.0;
        } else {
            if (dmin > 0.0) {
                dmin *= 0.99;
                dmax *= 1.01;
            };

            if (dmax < 0.0) {
                dmax *= 0.99;
                dmin *= 1.01;
            }
        }
    }

    var interval = dmax - dmin;

    var range = get_axes_range(width, height);

    var iR = d3.scaleLinear()
        .range([range.xMin, range.xMax])
        .domain([data_band_lo, data_band_hi]);

    var xR = d3.scaleLinear()
        .range([range.xMin, range.xMax])
        .domain([data_band_lo / 1e9, data_band_hi / 1e9]);

    var vR = d3.scaleLinear()
        .range([range.xMin, range.xMax])
        .domain([vMin, vMax]);

    var yR = d3.scaleLinear()
        .range([range.yMax, range.yMin])
        .domain([dmin - get_spectrum_margin() * interval, dmax + get_spectrum_margin() * interval]);

    var checkbox = document.getElementById('restcheckbox');

    try {
        if (checkbox.checked) {
            xR.domain([relativistic_rest_frequency(data_band_lo) / 1e9, relativistic_rest_frequency(data_band_hi) / 1e9]);

            vR.domain([Einstein_relative_velocity(relativistic_rest_frequency(data_band_lo), RESTFRQ), Einstein_relative_velocity(relativistic_rest_frequency(data_band_hi), RESTFRQ)]);
        }
    }
    catch (e) { };

    var iAxis = d3.axisTop(iR)
        .tickSizeOuter([3])
        .ticks(7);

    var xAxis = d3.axisTop(xR)
        .tickSizeOuter([3])
        .ticks(7);
    /*.tickFormat(function(d) {
      //limit the number of decimal digits shown
      return parseFloat(d.toPrecision(7)) ;
    });*/
    /*.tickFormat(function(d) {var n ;
           if(fitsData.CDELT3 > 0)
             n = d * (fitsData.depth-1) + 1 ;
           else
             n = (1-d) * (fitsData.depth-1) + 1 ;

           var freq = fitsData.CRVAL3+fitsData.CDELT3*(n-fitsData.CRPIX3) ;
           freq /= 1e9 ;//convert from Hz to GHz
           return freq.toPrecision(6) ;
    });*/

    var vAxis = d3.axisBottom(vR)
        .tickSizeOuter([3]);
    /*.tickFormat(function(d) {var freq = data_band_lo + d * (data_band_hi - data_band_lo) ;
           var vel = Einstein_relative_velocity(freq, RESTFRQ) ;
           return vel.toPrecision(6) ;
    });*/

    var yAxis = d3.axisRight(yR)
        .tickSizeOuter([3])
        .tickFormat(function (d) {
            var number;

            if (Math.abs(d) <= 0.001 || Math.abs(d) >= 1000)
                number = d.toExponential();
            else
                number = d;

            if (Math.abs(d) == 0)
                number = d;

            return number;
        });

    if (optical_view) {
        //i-axis label
        var strILabel = "cube frames";

        svg.append("foreignObject")
            .attr("x", (2 * range.xMin + 1.5 * emFontSize))
            .attr("y", (height - 3.5 * emFontSize))
            .attr("width", 20 * emFontSize)
            .attr("height", 2 * emFontSize)
            .append("xhtml:div")
            .attr("id", "frequency_display")
            .style("display", "inline-block")
            .attr("class", "axis-label")
            .html(strILabel);

        // Add the X Axis
        svg.append("g")
            .attr("class", "axis")
            .attr("id", "iaxis")
            .style("fill", axisColour)
            .style("stroke", axisColour)
            //.style("stroke-width", emStrokeWidth)
            .attr("transform", "translate(0," + (height - 1) + ")")
            .call(iAxis);
    }

    if (has_frequency_info) {
        //x-axis label
        var strXLabel = "";

        try {
            if (!checkbox.checked)
                strXLabel = '<I>F<SUB>' + fitsData.SPECSYS.trim() + '</SUB></I> [GHz]';
            else
                strXLabel = '<I>F<SUB>REST</SUB></I> [GHz]';
        }
        catch (e) {
            strXLabel = '<I>F<SUB>' + 'LSRK' + '</SUB></I> [GHz]';
        };

        svg.append("foreignObject")
            .attr("x", (2 * range.xMin + 1.5 * emFontSize))
            .attr("y", (height - 3.5 * emFontSize))
            .attr("width", 20 * emFontSize)
            .attr("height", 2 * emFontSize)
            .append("xhtml:div")
            .attr("id", "frequency_display")
            .style("display", "inline-block")
            .attr("class", "axis-label")
            .html(strXLabel);

        // Add the X Axis
        svg.append("g")
            .attr("class", "axis")
            .attr("id", "xaxis")
            .style("fill", axisColour)
            .style("stroke", axisColour)
            //.style("stroke-width", emStrokeWidth)
            .attr("transform", "translate(0," + (height - 1) + ")")
            .call(xAxis);
    }

    if (!optical_view) {
        //y-axis label
        var yLabel = "Integrated";

        if (intensity_mode == "mean")
            yLabel = "Mean";

        var bunit = '';
        if (fitsData.BUNIT != '') {
            bunit = fitsData.BUNIT.trim();

            if (intensity_mode == "integrated" && has_velocity_info)
                bunit += '•km/s';

            bunit = "[" + bunit + "]";
        }

        svg.append("text")
            .attr("id", "ylabel")
            .attr("x", (-height + 2 * range.xMin + 1.5 * emFontSize)/*-0.75*height*/)
            .attr("y", 1.25 * emFontSize + 0 * range.xMin)
            .attr("font-family", "Inconsolata")
            .attr("font-size", "1.25em")
            .attr("text-anchor", "start")
            .style("fill", "darkgray")
            //.style("opacity", 0.7)
            .attr("stroke", "none")
            .attr("transform", "rotate(-90)")
            .text(yLabel + ' ' + fitsData.BTYPE.trim() + " " + bunit);

        // Add the Y Axis
        svg.append("g")
            .attr("class", "axis")
            .attr("id", "yaxis")
            .style("fill", axisColour)
            .style("stroke", axisColour)
            //.style("stroke-width", emStrokeWidth)
            .attr("transform", "translate(" + (0.75 * range.xMin - 1) + ",0)")
            .call(yAxis);

        // Add a CSV export link
        if (has_velocity_info || has_frequency_info) {
            var front_svg = d3.select("#FrontSVG");
            var width = parseFloat(front_svg.attr("width"));
            var height = parseFloat(front_svg.attr("height"));

            strCSV = '<span id="exportCSV" class="fas fa-file-csv" style="display:inline-block; cursor: pointer" title="click to export spectrum to a local file"></span>'

            var colour_style = "csv-dark";
            if (theme == 'bright')
                colour_style = "csv-light";

            let x1 = range.xMax + 0.75 * emFontSize;
            let x2 = (range.xMax + width) / 2.0 - 0.5 * emFontSize;

            front_svg.append("foreignObject")
                .attr("id", "foreignCSV")
                .attr("x", Math.min(x1, x2))
                .attr("y", (height - 2.0 * emFontSize))
                .attr("width", 2 * emFontSize)
                .attr("height", 2 * emFontSize)
                .append("xhtml:div")
                .attr("id", "csv")
                .attr("class", colour_style)
                .attr("pointer-events", "auto")
                .html(strCSV);

            setup_csv_export();

            d3.select("#csv").moveToFront();
        };
    }

    //if(fitsData.CTYPE3 == "FREQ")
    if (vMin != null && vMax != null && !optical_view) {
        var vpos = 0;

        if (!has_frequency_info) {
            vpos = height - 1;

            vAxis = d3.axisTop(vR)
                .tickSizeOuter([3]);
        }

        // Add the V Axis
        svg.append("g")
            .attr("class", "axis")
            .attr("id", "vaxis")
            .style("fill", axisColour)
            .style("stroke", axisColour)
            //.style("stroke-width", emStrokeWidth)
            .attr("transform", "translate(0," + vpos + ")")
            .call(vAxis);

        var strZLabel = "";

        if (fitsData.SPECSYS.trim() != "")
            strZLabel = "<I>V<SUB>" + fitsData.SPECSYS.trim() + "</SUB></I> [km/s]";
        else
            strZLabel = "<I>V<SUB>" + 'LSRK' + "</SUB></I> [km/s]";

        var ypos = 2.0 * emFontSize;

        if (!has_frequency_info)
            ypos = height - 3.5 * emFontSize;

        //z-axis label
        svg.append("foreignObject")
            .attr("x", (2 * range.xMin + 1.5 * emFontSize))
            //.attr("y", (0.02*height+1.5*emFontSize))
            .attr("y", ypos)
            .attr("width", 20 * emFontSize)
            .attr("height", 2 * emFontSize)
            .append("xhtml:div")
            .attr("id", "velocity_display")
            .style("display", "inline-block")
            .attr("class", "axis-label")
            .html(strZLabel);
    }

    {
        svg.append("line")
            .attr("id", "freq_bar")
            .attr("x1", range.xMin)
            .attr("y1", 0)
            .attr("x2", range.xMin)
            .attr("y2", height - 1)
            .style("stroke", "white")
            //.style("stroke-dasharray", ("5, 5, 1, 5"))
            .style("stroke-width", 2 * emStrokeWidth)
            .attr("opacity", 0.0);
    }

    //add the x-axis frequency range selection shadow rectangle
    svg.append("rect")
        .attr("id", "fregion")
        .attr("x", range.xMin)
        .attr("y", 0)
        .attr("width", (range.xMax - range.xMin))
        .attr("height", height - 1)
        .attr("fill", "gray")//"gray"
        .style("stroke-dasharray", ("1, 5, 1, 1"))
        .style("mix-blend-mode", "difference")
        .attr("opacity", 0.0)
        .moveToBack();

    try {
        d3.select("#axes_selection").remove();
    }
    catch (e) {
    }

    var svg = d3.select("#FrontSVG");

    var group = svg.append("g").attr("id", "axes_selection");

    var patternScale = Math.ceil(((range.xMax - range.xMin) / 200 / 4));

    var patternPath = 'M' + (-1 * patternScale) + ',' + (1 * patternScale) + ' l' + (2 * patternScale) + ',' + (-2 * patternScale) + ' M0,' + (4 * patternScale) + ' l' + (4 * patternScale) + ',' + (-4 * patternScale) + ' M' + (3 * patternScale) + ',' + (5 * patternScale) + ' l' + (2 * patternScale) + ',' + (-2 * patternScale);

    svg.append("pattern")
        .attr("id", "diagonalHatch")
        .attr("patternUnits", "userSpaceOnUse")
        .attr("width", patternScale * 4)
        .attr("height", patternScale * 4)
        .append("path")
        //.attr("d", "M-1,1 l2,-2 M0,4 l4,-4 M3,5 l2,-2")
        .attr("d", patternPath)
        .style("stroke", "gray")
        .style("stroke-width", 1);

    if (has_frequency_info || has_velocity_info || optical_view)
        group.append("rect")
            .attr("id", "frequency")
            .attr("x", range.xMin)
            .attr("y", range.yMax + 1)
            .attr("width", (range.xMax - range.xMin))
            .attr("height", (height - 1 - range.yMax - 1))
            .attr("fill", "url(#diagonalHatch)")
            //.attr("stroke", "white")
            //.style("stroke-dasharray", ("1, 5"))
            .attr("opacity", 0.0)
            .style('cursor', 'pointer')
            .on("mouseleave", (event) => {
                x_axis_mouseleave();
            })
            .on("mouseenter", (event) => {
                var offset = d3.pointer(event);
                x_axis_mouseenter(offset);

            })
            .on("mousemove", (event) => {
                var offset = d3.pointer(event);

                if (offset[0] >= 0) {
                    x_axis_mousemove(offset);
                };
            })
            .call(d3.drag()
                .on("start", dragstart)
                .on("drag", dragmove)
                .on("end", dragend));

    //shift/zoom Y-Axis
    group = svg.append("g").attr("id", "y_axis_stretching");

    prev_scale = 1.0;

    group.append("rect")
        .attr("id", "scaling")
        .attr("x", 0)
        .attr("y", range.yMin)
        .attr("width", 2 * 0.75 * range.xMin)
        .attr("height", (range.yMax - range.yMin))
        .attr("fill", "url(#diagonalHatch)")
        .attr("opacity", 0.0)
        .call(d3.drag().on("drag", shifted))
        .call(d3.zoom().scaleExtent([0.1, 10]).on("zoom", scaled))
        .on("mouseleave", function (event) {
            d3.select(this)
                .style('cursor', '')
                .attr("opacity", 0.0);

            /*d3.select("#yaxis")
            .style("fill", axisColour)
            .style("stroke", axisColour);*/
        })
        .on("mouseenter", function (event) {
            if (autoscale)
                return;

            if (windowLeft)
                return;

            hide_navigation_bar();

            d3.select(this)
                .style('cursor', 'ns-resize')
                .attr("opacity", 0.5);

            let fillColour = 'white';

            if (theme == 'bright')
                fillColour = 'black';

            d3.select("#yaxis")
                .style("fill", fillColour)
                .style("stroke", fillColour);
        });
}

function x_axis_mouseenter(offset) {
    if (streaming) {
        console.log("streaming is active, calling 'x_axis_mouseleave()' to reset video streaming.");
        x_axis_mouseleave();
    }

    //send an init_video command via WebSockets
    streaming = true;
    video_stack = new Array(va_count);

    if (viewport_zoom_settings != null) {
        d3.select("#upper").style("stroke", "Gray");
        d3.select("#upperCross").attr("opacity", 0.75);
        d3.select("#upperBeam").attr("opacity", 0.75);
    }

    d3.select("#lower").attr("pointer-events", "none");

    // clear the legend
    if (va_count == 1) {
        var elem = d3.select("#legend"); elem.attr("opacity", 0);

        // Clear the legend canvas
        var image = imageContainer[va_count - 1];
        var gl = image.legend_gl;

        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);
    }
    else {
        for (let index = 1; index <= va_count; index++) {
            var elem = d3.select("#legend" + index);
            elem.attr("opacity", 0);
        }
    }

    //if (videoFrame == null)
    if (wasm_supported) {
        var freq = get_mouse_frequency(offset);

        sent_vid_id++;

        var rect = document.getElementById('mainDiv').getBoundingClientRect();
        var width = Math.round(rect.width - 20);
        var height = Math.round(rect.height - 20);

        if (composite_view) {
            var request = {
                type: "init_video",
                frame: freq,
                view: "composite",
                ref_freq: RESTFRQ,
                fps: vidFPS,
                seq_id: sent_vid_id,
                bitrate: Math.round(target_bitrate),
                width: width,
                height: height,
                flux: document.getElementById('flux' + 1).value,
                timestamp: performance.now()
            };

            if (wsConn[0].readyState == 1)
                wsConn[0].send(JSON.stringify(request));
            video_stack[0] = [];
        } else for (let index = 0; index < va_count; index++) {
            var request = {
                type: "init_video",
                frame: freq,
                view: "tile",
                ref_freq: RESTFRQ,
                fps: vidFPS,
                seq_id: sent_vid_id,
                bitrate: Math.round(target_bitrate),
                width: width,
                height: height,
                flux: document.getElementById('flux' + (index + 1)).value,
                timestamp: performance.now()
            };

            if (wsConn[index].readyState == 1)
                wsConn[index].send(JSON.stringify(request));
            video_stack[index] = [];
        };
    }

    hide_navigation_bar();

    d3.select("#scaling")
        .style('cursor', '')
        .attr("opacity", 0.0);

    d3.select("#yaxis")
        .style("fill", axisColour)
        .style("stroke", axisColour);

    d3.select("#frequency").attr("opacity", 0.5);

    let fillColour = 'white';

    if (theme == 'bright')
        fillColour = 'black';

    d3.select("#xaxis")
        .style("fill", fillColour)
        .style("stroke", fillColour)
        .attr("opacity", 1.0);

    d3.select("#vaxis")
        .style("fill", fillColour)
        .style("stroke", fillColour)
        .attr("opacity", 1.0);

    fillColour = 'white';
    let strokeColour = 'black';

    if (theme == 'bright') {
        fillColour = 'black';
        strokeColour = 'white';
    }

    var svg = d3.select("#BackSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    jvoText = d3.select("#FrontSVG").append("text")
        .attr("id", "jvoText")
        .attr("x", width / 2)
        .attr("y", height / 2)
        //.attr("font-family", "Arial")
        .attr("font-family", "Inconsolata")
        .attr("font-weight", "regular")
        .attr("font-size", "5em")
        .attr("text-anchor", "middle")
        .attr("fill", fillColour)
        .attr("stroke", strokeColour)
        .attr("pointer-events", "none")
        .attr("opacity", 1.0);

    shortcut.add("f", set_user_restfrq);
    shortcut.add("Left", x_axis_left);
    shortcut.add("Right", x_axis_right);
    shortcut.add("Enter", go_to_splatalogue);

    setup_window_timeout();
}

function x_axis_mouseleave() {
    streaming = false;
    video_stack = new Array(va_count);

    //clear the VideoCanvas and reset the Zoom Viewport
    d3.select("#upper").style("stroke", "transparent");
    d3.select("#upperCross").attr("opacity", 0.0);
    d3.select("#upperBeam").attr("opacity", 0.0);
    d3.select("#lower").attr("pointer-events", "auto");

    requestAnimationFrame(function () {
        if (va_count > 1 && !composite_view) {
            for (let index = 0; index < va_count; index++) {
                // re-display the image
                init_webgl_image_buffers(index + 1);
            }
        } else {
            // re-display the image
            if (!composite_view)
                init_webgl_image_buffers(va_count);
            else
                init_webgl_composite_image_buffers();
        }
    });

    if (va_count == 1) {
        var elem = d3.select("#legend");

        if (displayLegend)
            elem.attr("opacity", 1);
        else
            elem.attr("opacity", 0);
    }
    else {
        for (let index = 1; index <= va_count; index++) {
            var elem = d3.select("#legend" + index);

            if (displayLegend)
                elem.attr("opacity", 1);
            else
                elem.attr("opacity", 0);
        }
    }

    d3.select("#fps").text("");

    // show the contour plot
    if (displayContours) {
        document.getElementById("ContourSVG").style.display = "block";

        if (!has_contours)
            update_contours();
    }

    //send an end_video command via WebSockets
    var request = {
        type: "end_video"
    };

    if (videoFrame[0] != null) {
        if (composite_view) {
            if (!videoFrame[0].first) {
                clear_webgl_video_buffers(1);
            }

            videoFrame[0].rgba = null;
            videoFrame[0] = null;

            if (wsConn[0].readyState == 1)
                wsConn[0].send(JSON.stringify(request));

            video_stack[0] = [];
        } else for (let index = 0; index < va_count; index++) {
            if (!videoFrame[index].first) {
                clear_webgl_video_buffers(index + 1);
            }

            videoFrame[index].rgba = null;
            videoFrame[index] = null;

            if (wsConn[index].readyState == 1)
                wsConn[index].send(JSON.stringify(request));

            video_stack[index] = [];

            // not needed anymore (handled above)
            /*if (va_count > 1)
                refresh_tiles(index + 1);*/
        }

        try {
            Module.hevc_destroy_frame(va_count);
        } catch (e) {
            //console.log(e);
        };
    }

    shortcut.remove("f");
    shortcut.remove("Left");
    shortcut.remove("Right");
    shortcut.remove("Enter");

    d3.select("#frequency").attr("opacity", 0.0);
    d3.select("#freq_bar").attr("opacity", 0.0);

    d3.select("#xaxis")
        .style("fill", axisColour)
        .style("stroke", axisColour);

    /*d3.select("#yaxis")
      .style("fill", axisColour)
      .style("stroke", axisColour);*/

    d3.select("#vaxis")
        .style("fill", axisColour)
        .style("stroke", axisColour);

    //d3.select("#freq_bar").attr("opacity", 0.0);

    d3.select("#jvoText").remove();

    mol_pos = -1;
    var modal = document.getElementById('molecularlist');
    modal.style.display = "none";

    display_legend();

    setup_window_timeout();
}

function x_axis_mousemove(offset) {
    mol_pos = -1;

    x_axis_move(offset);
}

function x_axis_left() {
    var freq = round(get_line_frequency(), 10);

    //console.log("current line frequency = ", freq, "\tmol_pos = ", mol_pos) ;

    //find the next line to the left

    var m = document.getElementsByClassName("molecularp");

    if (m.length <= 0)
        return;

    if (mol_pos < 0) {
        mol_pos = 0;

        for (var i = 0; i < m.length; i++) {
            var tmp = round(parseFloat(m[i].getAttribute("freq")), 10);

            if (tmp >= freq)
                break;

            mol_pos = i;
        };
    }
    else {
        if (mol_pos - 1 >= 0)
            mol_pos--;
    };

    var offset = [parseFloat(m[mol_pos].getAttribute("x")), 0];

    x_axis_move(offset);
};

function x_axis_right() {
    var freq = round(get_line_frequency(), 10);

    //console.log("current line frequency = ", freq, "\tmol_pos = ", mol_pos) ;

    //find the next line to the left

    var m = document.getElementsByClassName("molecularp");

    if (m.length <= 0)
        return;

    if (mol_pos < 0) {
        mol_pos = m.length - 1;

        for (var i = m.length - 1; i >= 0; i--) {
            var tmp = round(parseFloat(m[i].getAttribute("freq")), 10);

            if (tmp <= freq)
                break;

            mol_pos = i;
        };
    }
    else {
        if (mol_pos + 1 <= m.length - 1)
            mol_pos++;
    };


    var offset = [parseFloat(m[mol_pos].getAttribute("x")), 0];

    x_axis_move(offset);
};

function replay_video() {
    if (!video_playback)
        return;

    x_axis_mousemove(video_offset);

    //simulate a mouse advance
    var width = parseFloat(d3.select("#frequency").attr("width"));
    var offsetx = parseFloat(d3.select("#frequency").attr("x"));

    let fps = 30;
    let no_frames = fps * video_period;

    let dx = width / no_frames;
    let dt = 1000.0 / fps;

    var new_video_offset = video_offset[0] + dx;
    if (new_video_offset > offsetx + width)
        new_video_offset = offsetx;

    video_offset[0] = new_video_offset;
    //var dt = video_period / width;

    video_timeout = setTimeout(replay_video, dt);
}

function x_axis_move(offset) {
    clearTimeout(idleVideo);

    let strokeColour = 'white';

    if (theme == 'bright')
        strokeColour = 'black';

    d3.select("#freq_bar")
        .attr("x1", offset[0])
        .attr("x2", offset[0])
        .attr("opacity", 1.0)
        .style("stroke", strokeColour);

    var dx = parseFloat(d3.select("#frequency").attr("width"));
    var offsetx = parseFloat(d3.select("#frequency").attr("x"));

    var band_lo = data_band_lo;
    var band_hi = data_band_hi;

    var freq = band_lo + (offset[0] - offsetx) / dx * (band_hi - band_lo);
    USER_SELFRQ = freq;

    var checkbox = document.getElementById('restcheckbox');

    try {
        if (checkbox.checked) {
            freq = relativistic_rest_frequency(freq);

            USER_SELFRQ = relativistic_rest_frequency(USER_SELFRQ);
        }
    }
    catch (e) {
        if (has_velocity_info)
            d3.select("#jvoText").text((freq / 1.0e3).toFixed(getVelocityPrecision()) + " km/s");

        //return ;//commented out by Chris on 2018/08/03
    };

    //console.log("RESTFRQ:", RESTFRQ);

    var relvel = Einstein_relative_velocity(freq, RESTFRQ);

    if (optical_view)
        d3.select("#jvoText").text(Math.round(freq));

    if (has_frequency_info)
        d3.select("#jvoText").text((freq / 1.0e9).toPrecision(7) + " " + 'GHz' + ", " + relvel.toFixed(getVelocityPrecision()) + " km/s");

    var modal = document.getElementById('molecularlist');

    if ((offset[0] - offsetx) >= 0.5 * dx) {
        modal.style.right = null;
        modal.style.left = "2.5%";
    }
    else {
        modal.style.right = "2.5%";
        modal.style.left = null;
    };

    let mol_freq = freq;

    if (!freqdrag && wasm_supported) {
        //initially assume 10 frames per second for a video
        //later on use a Kalman Filter to predict the next frame position and request it
        vidInterval = 1000 / vidFPS;

        now = performance.now();
        elapsed = performance.now() - then;

        var freq = get_mouse_frequency(offset);

        if (elapsed > vidInterval) {
            then = now - (elapsed % vidInterval);

            //for each dataset request a video frame via WebSockets
            sent_vid_id++;

            video_count = 0;

            if (realtime_video) {
                var fill;

                if (theme == "dark")
                    fill = 0;
                else
                    fill = 255;

                if (composite_view) {
                    var request = {
                        type: "composite_video",
                        frame: freq,
                        key: false,
                        fill: fill,
                        ref_freq: RESTFRQ,
                        fps: vidFPS,
                        seq_id: sent_vid_id,
                        bitrate: Math.round(target_bitrate),
                        timestamp: performance.now()
                    };

                    if (wsConn[0].readyState == 1)
                        wsConn[0].send(JSON.stringify(request));
                } else for (let index = 0; index < va_count; index++) {
                    var request = {
                        type: "video",
                        frame: freq,
                        key: false,
                        fill: fill,
                        ref_freq: RESTFRQ,
                        fps: vidFPS,
                        seq_id: sent_vid_id,
                        bitrate: Math.round(target_bitrate),
                        timestamp: performance.now()
                    };

                    if (wsConn[index].readyState == 1)
                        wsConn[index].send(JSON.stringify(request));
                }
            };
        };

        if (videoFrame[0] != null)
            idleVideo = setTimeout(videoTimeout, 250, freq);
    };

    zoom_molecules(mol_freq);

    setup_window_timeout();
}

function zoom_molecules(freq) {
    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return;

    if (fitsData.depth <= 1 || molecules.length <= 0)
        return;

    var pos = -1;
    var minDist = 10 * freq;

    var modal = document.getElementById('molecularlist');
    var scroller = zenscroll.createScroller(modal);
    var m = document.getElementsByClassName("molecularp");

    for (var i = 0; i < m.length; i++) {
        m[i].style.color = "inherit";
        m[i].style.fontSize = "100%";
        m[i].style.fontWeight = "normal";

        var tmp = parseFloat(m[i].getAttribute("freq"));
        var dist = Math.abs(freq - tmp);

        if (dist < minDist) {
            minDist = dist;
            pos = i;
        };
    };

    if (mol_pos >= 0)
        pos = mol_pos;

    if (pos > -1) {
        m[pos].style.color = "yellow";
        m[pos].style.fontSize = "130%";
        m[pos].style.fontWeight = "bold";

        pos = Math.max(0, pos - 5);

        //m[pos].scrollIntoView({ block: "start", behavior: "smooth" }); // does not work correctly in Safari
        scroller.to(m[pos], 0); // 'center' or 'to'
    };

    if (m.length > 0 && displayMolecules)
        modal.style.display = "block";
    else
        modal.style.display = "none";
}

function get_mouse_frame(offset) {
    var freq = d3.select("#frequency");
    var dx = parseFloat(freq.attr("width"));
    var offsetx = parseFloat(freq.attr("x"));

    var band_lo = frame_start;
    var band_hi = frame_end;

    var frame = Math.max(frame_start, Math.min(frame_end, Math.round(frame_start + (offset[0] - offsetx) / dx * (frame_end - frame_start))));

    return frame;
}

function get_mouse_frequency(offset) {
    var freq = d3.select("#frequency");
    var dx = parseFloat(freq.attr("width"));
    var offsetx = parseFloat(freq.attr("x"));

    var band_lo = data_band_lo;
    var band_hi = data_band_hi;

    var frequency = Math.max(band_lo, Math.min(band_hi, band_lo + (offset[0] - offsetx) / dx * (band_hi - band_lo)));

    return frequency;
};

function get_line_frequency() {
    var x = parseFloat(d3.select("#freq_bar").attr("x1"));

    var offset = [x, 0];

    var freq = get_mouse_frequency(offset);

    var checkbox = document.getElementById('restcheckbox');

    if (checkbox.checked)
        freq = relativistic_rest_frequency(freq);

    return freq;
};

function go_to_splatalogue() {
    var freq = round(get_line_frequency() / 1e9, 10);//[GHz]
    var offset = 0.01;//10 MHz [GHz]

    var fmin = freq - offset;//[GHz]
    var fmax = freq + offset;//[GHz]

    var url = "http://www.cv.nrao.edu/php/splat/sp_basic.php?el1=el1&el2=el2&ls1=ls1&ls5=ls5&displayRecomb=displayRecomb&displayLovas=displayLovas&displaySLAIM=displaySLAIM&displayJPL=displayJPL&displayCDMS=displayCDMS&displayToyaMA=displayToyaMA&displayOSU=displayOSU&displayLisa=displayLisa&displayRFI=displayRFI&data_version=v3.0&no_atmospheric=no_atmospheric&no_potential=no_potential&no_probable=no_probable&include_only_nrao=include_only_nrao&show_orderedfreq_only=show_orderedfreq_only&chemical_name=&band=any&z=&energy_range_from=&energy_range_to=&energy_range_type=el_cm1&frequency_units=GHz&from=" + fmin + "&to=" + fmax + "&submit=Search";

    var win = window.open(url, '_blank');

    if (win) {
        //Browser has allowed it to be opened
        win.focus();
    } else {
        //Browser has blocked it
        alert('Please allow popups for this website');
    }
};

function getMousePos(e) {
    return { x: e.clientX, y: e.clientY };
}

function get_zoomed_size(width, height, img_width, img_height) {
    var zoomed_size = Math.max(width / 2, height / 2) / golden_ratio;

    if (zoom_shape == "square")
        return Math.round(zoomed_size);

    if (zoom_shape == "circle")
        return Math.round(1.2 * zoomed_size);
}

d3.selection.prototype.moveToFront = function () {
    return this.each(function () {
        this.parentNode.appendChild(this);
    });
};

d3.selection.prototype.moveToBack = function () {
    return this.each(function () {
        var firstChild = this.parentNode.firstChild;
        if (firstChild) {
            this.parentNode.insertBefore(this, firstChild);
        }
    });
};

function setup_viewports() {
    //delete previous instances
    try {
        d3.select("#upper").remove();
        d3.select("#lower").remove();
        d3.select("#upperCross").remove();
        d3.select("#lowerCross").remove();
    }
    catch (e) { };

    var svg = d3.select("#FrontSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var elem = d3.select("#image_rectangle");
    var img_width = parseFloat(elem.attr("width"));
    var img_height = parseFloat(elem.attr("height"));
    var zoomed_size = get_zoomed_size(width, height, img_width, img_height);

    if (zoom_shape == "square") {
        //upper zoom
        svg.append("rect")
            .attr("id", "upper")
            .attr("x", (emStrokeWidth))
            .attr("y", (emStrokeWidth))
            .attr("width", zoomed_size)
            .attr("height", zoomed_size)
            .attr("fill", "transparent")
            .style("stroke", "transparent")
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", emStrokeWidth / 2)
            .attr("opacity", 1.0)
            .on("mouseover", function () { /*if(windowLeft) return; else swap_viewports();*/ zoom_location = "lower"; var elem = d3.select(this); elem.style("stroke", "transparent"); elem.moveToBack(); d3.select("#lower").moveToFront(); });

        //lower zoom
        svg.append("rect")
            .attr("id", "lower")
            .attr("x", (width - 1 - emStrokeWidth - zoomed_size))
            .attr("y", (height - 1 - emStrokeWidth - zoomed_size))
            .attr("width", zoomed_size)
            .attr("height", zoomed_size)
            .attr("fill", "transparent")
            .style("stroke", "transparent")
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", emStrokeWidth / 2)
            .attr("opacity", 1.0)
            .on("mouseover", function () { /*if(windowLeft) return; else swap_viewports();*/ zoom_location = "upper"; var elem = d3.select(this); elem.style("stroke", "transparent"); elem.moveToBack(); d3.select("#upper").moveToFront(); });
    };

    if (zoom_shape == "circle") {
        //upper zoom
        svg.append("circle")
            .attr("id", "upper")
            .attr("cx", (emStrokeWidth + zoomed_size / 2))
            .attr("cy", (emStrokeWidth + zoomed_size / 2))
            .attr("r", zoomed_size / 2)
            .attr("fill", "transparent")
            .style("stroke", "transparent")
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", emStrokeWidth / 2)
            .attr("opacity", 1.0)
            .on("mouseover", function () { /*if(windowLeft) return; else swap_viewports();*/ zoom_location = "lower"; var elem = d3.select(this); elem.style("stroke", "transparent"); elem.moveToBack(); d3.select("#lower").moveToFront(); });

        //lower zoom
        svg.append("circle")
            .attr("id", "lower")
            .attr("cx", (width - 1 - emStrokeWidth - zoomed_size / 2))
            .attr("cy", (height - 1 - emStrokeWidth - zoomed_size / 2))
            .attr("r", zoomed_size / 2)
            .attr("fill", "transparent")
            .style("stroke", "transparent")
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", emStrokeWidth / 2)
            .attr("opacity", 1.0)
            .on("mouseover", function () { /*if(windowLeft) return; else swap_viewports();*/ zoom_location = "upper"; var elem = d3.select(this); elem.style("stroke", "transparent"); elem.moveToBack(); d3.select("#upper").moveToFront(); });
    };

    var crossSize = 2.0 * emFontSize;

    //upper cross-hair
    svg.append("svg:image")
        .attr("id", "upperCross")
        .attr("x", (emStrokeWidth + (zoomed_size - crossSize) / 2))
        .attr("y", (emStrokeWidth + (zoomed_size - crossSize) / 2))
        //.attr("xlink:href", ROOT_PATH + "plainicon.com-crosshair_white.svg")
        .attr("xlink:href", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/plainicon.com-crosshair_white.svg")
        .attr("width", crossSize)
        .attr("height", crossSize)
        .attr("opacity", 0.0);

    //lower cross-hair
    svg.append("svg:image")
        .attr("id", "lowerCross")
        .attr("x", (width - 1 - emStrokeWidth - (zoomed_size + crossSize) / 2))
        .attr("y", (height - 1 - emStrokeWidth - (zoomed_size + crossSize) / 2))
        //.attr("xlink:href", ROOT_PATH + "plainicon.com-crosshair_white.svg")
        .attr("xlink:href", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/plainicon.com-crosshair_white.svg")
        .attr("width", crossSize)
        .attr("height", crossSize)
        .attr("opacity", 0.0);
}

function swap_viewports() {
    // swap the zoomed viewports
    if (viewport != null) {
        // Clear the ZOOM Canvas
        //console.log("clearing the ZOOM Canvas");
        var gl = viewport.gl;

        if (gl !== undefined && gl != null) {
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT);
        }

        viewport.refresh = true;
    }

    clear_webgl_viewport();

    d3.select("#" + zoom_location + "Cross").attr("opacity", 0.0);
    d3.select("#" + zoom_location + "Beam").attr("opacity", 0.0);

    var elem = d3.select('#' + zoom_location);
    elem.style("stroke", "transparent");
    elem.attr("pointer-events", "none");
    elem.moveToBack();

    if (zoom_location == "upper") {
        d3.select("#lower")
            .attr("pointer-events", "auto")
            .moveToFront();

        zoom_location = "lower";
        return;
    }

    if (zoom_location == "lower") {
        d3.select("#upper")
            .attr("pointer-events", "auto")
            .moveToFront();

        zoom_location = "upper";
        return;
    }
}

function pv_event(event) {
    // console.log("pv_event");

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData != null) {
        if (fitsData.depth == 1) {
            console.log("P-V Diagram not available for 2D data");
            return;
        }
    }

    try {
        mouse_click_end = !mouse_click_end;
    } catch (_) {
        mouse_click_end = false;
    }

    if (mouse_click_end) {
        // finalise the P-V line
        let x1 = line_x;
        let y1 = line_y;

        var offset = d3.pointer(event);
        let x2 = offset[0]; let y2 = offset[1];

        // disable the dashes, set the end marker
        d3.select("#pvline")
            .attr("x1", x1)
            .attr("y1", y1)
            .attr("x2", x2)
            .attr("y2", y2)
            .attr("marker-end", "url(#head)")
            .style("stroke-dasharray", (""))
            .attr("opacity", 1.0);

        let dx = x2 - x1;
        let dy = y2 - y1;

        if (Math.abs(dy) > 0) {
            let _m = - dx / dy; // a perpendicular line to the P-V line
            let _s = emFontSize / Math.sqrt(1 + _m * _m) / golden_ratio;

            let _mx = _s;
            let _my = _m * _s;

            let _x = (x1 + x2) / 2; // midpoint of the P-V line
            let _y = (y1 + y2) / 2; // midpoint of the P-V line

            let _x1 = _x - _mx;
            let _y1 = _y - _my;
            let _x2 = _x + _mx;
            let _y2 = _y + _my;

            d3.select("#pvmid")
                .attr("x1", _x1)
                .attr("y1", _y1)
                .attr("x2", _x2)
                .attr("y2", _y2)
                .attr("opacity", 1.0);
        }

        // submit the P-V line to the server
        if (va_count == 1 || composite_view) {
            // create a new canvas
            var div = d3.select("body").append("div")
                .attr("id", "PVDiagram")
                .attr("class", "threejs");

            var rect = document.getElementById('mainDiv').getBoundingClientRect();
            var width = Math.round(rect.width - 20);
            var height = Math.round(rect.height - 20);

            div.append("canvas")
                .attr("id", "PVCanvas2")
                .attr("width", width)
                .attr("height", height)
                .attr('style', 'position: fixed; left: 10px; top: 10px; visibility: hidden;');

            div.append("canvas")
                .attr("id", "PVCanvas")
                .attr("width", width)
                .attr("height", height)
                .attr('style', 'position: fixed; left: 10px; top: 10px;');

            div.append("span")
                .attr("id", "closePVDiagram")
                .attr("class", "close myclose")
                .on("click", function () {
                    d3.select("#PVDiagram").remove();
                })
                .text("×");

            var pvDiv = div.append("div")
                .attr('style', 'position: fixed; left: 10px; top: 10px;');

            pvDiv.append("span")
                .attr("class", "pv-label")
                .html("show:&nbsp;");

            var htmlStr = displayPVCrosshair ? '<span class="fas fa-check-square"></span> crosshair&nbsp;' : '<span class="far fa-square"></span> crosshair&nbsp;';
            pvDiv.append("span")
                .attr("id", "displayPVCrosshair")
                .attr("class", "pv-label")
                .style('cursor', 'pointer')
                .on("click", function () {
                    displayPVCrosshair = !displayPVCrosshair;
                    localStorage_write_boolean("displayPVCrosshair", displayPVCrosshair);
                    var htmlStr = displayPVCrosshair ? '<span class="fas fa-check-square"></span> crosshair&nbsp;' : '<span class="far fa-square"></span> crosshair&nbsp;';
                    d3.select(this).html(htmlStr);
                })
                .html(htmlStr);

            htmlStr = displayPVContours ? '<span class="fas fa-check-square"></span> contours' : '<span class="far fa-square"></span> contours';
            pvDiv.append("span")
                .attr("id", "displayPVContours")
                .attr("class", "pv-label")
                .style('cursor', 'pointer')
                .on("click", function () {
                    displayPVContours = !displayPVContours;
                    localStorage_write_boolean("displayPVContours", displayPVContours);
                    var htmlStr = displayPVContours ? '<span class="fas fa-check-square"></span> contours' : '<span class="far fa-square"></span> contours';
                    d3.select(this).html(htmlStr);

                    if (displayPVContours) {
                        d3.select('#pv_contour_control').style("display", "inline-block");
                    }
                    else {
                        d3.select('#pv_contour_control').style("display", "none");
                    }

                    if (displayPVContours) {
                        resubmit_pv_line();
                    } else {
                        try {
                            d3.select("#PVContourSVG").remove();
                        } catch (_) { }
                    }
                })
                .html(htmlStr);

            var pvContourLines = pvDiv.append("span")
                .attr("id", "pv_contour_control")
                .style("class", "form-group")
                .attr("class", "form-horizontal");

            pvContourLines.append("label")
                .attr("for", "pv_contour_lines")
                .attr("class", "pv-label")
                .html("&nbsp;&nbsp;#contour levels:&nbsp; ");

            previous_pv_contour_lines = 5;

            pvContourLines.append("input")
                .attr("id", "pv_contour_lines")
                .attr("type", "number")
                .style("width", "3em")
                .style("color", "#383838")
                .attr("min", 2)
                .attr("step", 1)
                .attr("value", previous_pv_contour_lines);

            var elem = document.getElementById('pv_contour_lines');
            elem.onblur = validate_pv_contour_lines;
            elem.onmouseleave = validate_pv_contour_lines;
            elem.onkeyup = function (e) {
                var event = e || window.event;
                var charCode = event.which || event.keyCode;

                if (charCode == '13') {
                    // Enter pressed
                    validate_pv_contour_lines();
                    return false;
                }
            }

            if (displayPVContours) {
                d3.select('#pv_contour_control').style("display", "inline-block");
            }
            else {
                d3.select('#pv_contour_control').style("display", "none");
            }

            pvContourLines.append("label")
                .attr("class", "pv-label")
                .html("&nbsp;&nbsp;scaling:&nbsp; ");

            pvContourLines.append("input")
                .attr("id", "pv_linear")
                .attr("type", "radio")
                .attr("name", "pv_scaling")
                .attr("value", "linear");

            pvContourLines.append("label")
                .attr("for", "pv_linear")
                .attr("class", "pv-label")
                .html("&nbsp;linear&nbsp; ");

            // set the default scaling to linear
            d3.select("#pv_linear").property("checked", true);

            pvContourLines.append("input")
                .attr("id", "pv_sqrt")
                .attr("type", "radio")
                .attr("name", "pv_scaling")
                .attr("value", "sqrt");

            pvContourLines.append("label")
                .attr("for", "pv_sqrt")
                .attr("class", "pv-label")
                .html("&nbsp;sqrt&nbsp; ");

            pvContourLines.append("input")
                .attr("id", "pv_log")
                .attr("type", "radio")
                .attr("name", "pv_scaling")
                .attr("value", "log");

            pvContourLines.append("label")
                .attr("for", "pv_log")
                .attr("class", "pv-label")
                .html("&nbsp;logarithmic&nbsp; ");

            // set the onchange handler
            d3.selectAll("input[name='pv_scaling']").on("change", function () {
                resubmit_pv_line();
            });

            div.append("img")
                .attr("id", "hourglassPVDiagram")
                .attr("class", "hourglass")
                .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/loading.gif")
                .attr("alt", "hourglass")
                .style("width", 200)
                .style("height", 200);

            d3.select("#zoom").attr("opacity", 0.0);
            d3.select("#zoomCross").attr("opacity", 0.0);

            velocityline_position = -1;
            angularline_position = -1;
            pv_loop = -1;
            const res = submit_pv_line(x1, y1, x2, y2);
            console.log(res);

            // copy the image from WebGL to the new canvas
            var elem = document.getElementById("image_rectangle");

            if (elem != null) {
                /*var src_width = parseFloat(elem.getAttribute("width"));
                var src_height = parseFloat(elem.getAttribute("height"));
                var src_x = parseFloat(elem.getAttribute("x"));
                var src_y = parseFloat(elem.getAttribute("y"));*/

                var src_x = image_gl_viewport[0];
                var src_y = image_gl_viewport[1];
                var src_width = image_gl_viewport[2];
                var src_height = image_gl_viewport[3];

                // get the image source canvas
                var image_canvas = document.getElementById('HTMLCanvas');

                var canvas = document.getElementById('PVCanvas');
                var dst_width = canvas.width / 2;
                var dst_height = canvas.height;

                var scale = get_pv_image_scale(dst_width, dst_height, src_width, src_height);
                var img_width = scale * src_width;
                var img_height = scale * src_height;

                var context = canvas.getContext('2d');
                /*context.webkitImageSmoothingEnabled = false;
                context.msImageSmoothingEnabled = false;
                context.imageSmoothingEnabled = false;*/

                // place the image on the left-hand side
                context.drawImage(image_canvas, src_x, src_y, src_width, src_height, (dst_width - img_width) / 2, (dst_height - img_height) / 2, img_width, img_height);

                pvsvg_left = 10 + (dst_width - img_width) / 2;
                pvsvg_top = 10 + (dst_height - img_height) / 2;
                pvsvg_width = img_width;
                pvsvg_height = img_height;

                var svg = div.append("svg")
                    .attr("id", "PVSVG")
                    .attr("width", img_width)
                    .attr("height", img_height)
                    .attr('style', `position: fixed; left: ${pvsvg_left}px; top: ${pvsvg_top}px; cursor: default`);

                let fillColour = 'white';

                if (theme == 'bright')
                    fillColour = 'black';

                //pv line selection
                svg.append("line")
                    .attr("id", "pvline2")
                    .attr("x1", res.x1 * img_width)
                    .attr("y1", res.y1 * img_height)
                    .attr("x2", res.x2 * img_width)
                    .attr("y2", res.y2 * img_height)
                    .style("stroke", fillColour)
                    .style("stroke-dasharray", ("1, 2"))
                    .style("stroke-width", emStrokeWidth)
                    .attr("pointer-events", "none")
                    .attr("opacity", 0.5);

                const dx = res.x2 - res.x1;
                const dy = res.y2 - res.y1;

                const _x = 0.5 * (res.x1 + res.x2) * img_width; // midpoint of the P-V line
                const _y = 0.5 * (res.y1 + res.y2) * img_height; // midpoint of the P-V line

                var _m, _s, _mx, _my, _x1, _x2, _y1, _y2;

                if (Math.abs(dx) > 0) {
                    _m = dy / dx; // a line parallel to the P-V line
                    _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                    _mx = _s;
                    _my = _m * _s;

                    _x1 = _x - _mx;
                    _y1 = _y - _my;
                    _x2 = _x + _mx;
                    _y2 = _y + _my;

                    svg.append("line")
                        .attr("id", "pvmid3")
                        .attr("x1", _x1)
                        .attr("y1", _y1)
                        .attr("x2", _x2)
                        .attr("y2", _y2)
                        .style("stroke", fillColour)
                        .style("stroke-width", emStrokeWidth)
                        .attr("opacity", 1.0);
                }

                if (Math.abs(dy) > 0) {
                    _m = - dx / dy; // a perpendicular line to the P-V line
                    _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                    _mx = _s;
                    _my = _m * _s;

                    _x1 = _x - _mx;
                    _y1 = _y - _my;
                    _x2 = _x + _mx;
                    _y2 = _y + _my;

                    svg.append("line")
                        .attr("id", "pvmid2")
                        .attr("x1", _x1)
                        .attr("y1", _y1)
                        .attr("x2", _x2)
                        .attr("y2", _y2)
                        .style("stroke", fillColour)
                        .style("stroke-width", emStrokeWidth)
                        .attr("opacity", 1.0);
                }

                // add a starting point 'A'
                svg.append("text")
                    .attr("id", "pvpointA")
                    .attr("x", res.x1 * img_width)
                    .attr("y", res.y1 * img_height)
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "central")
                    .attr("font-size", emFontSize)
                    .attr("fill", fillColour)
                    .attr("pointer-events", "none")
                    .attr("opacity", 1.0)
                    .text("1");

                // add a circle at the start of the line
                svg.append("circle")
                    .attr("id", "pvline2_start")
                    .attr("cx", res.x1 * img_width)
                    .attr("cy", res.y1 * img_height)
                    .attr("r", emFontSize / 2)
                    .style("fill", "transparent")
                    .style("stroke", fillColour)
                    .style("stroke-width", emStrokeWidth)
                    .attr("pointer-events", "auto")
                    .style('cursor', 'move')
                    .call(d3.drag()
                        .on("start", function (event) {
                            event.preventDefault = true;

                            // start the pv event loop
                            pv_loop = setTimeout(loop_pv_line, pv_latency);
                        })
                        .on("drag", function (event) {
                            event.preventDefault = true;

                            var offset = d3.pointer(event);
                            let x = offset[0] - pvsvg_left; // the SVG offset
                            let y = offset[1] - pvsvg_top; // the SVG offset

                            // check if the point is within the image
                            x = Math.min(Math.max(x, 0), pvsvg_width - 1);
                            y = Math.min(Math.max(y, 0), pvsvg_height - 1);

                            d3.select("#pvpointA")
                                .attr("x", x)
                                .attr("y", y);

                            d3.select("#pvline2_start")
                                .attr("cx", x)
                                .attr("cy", y);

                            d3.select("#pvline2")
                                .attr("x1", x)
                                .attr("y1", y);

                            var line = d3.select("#pvline2");
                            var x1 = parseFloat(line.attr("x1"));
                            var y1 = parseFloat(line.attr("y1"));
                            var x2 = parseFloat(line.attr("x2"));
                            var y2 = parseFloat(line.attr("y2"));

                            // re-centre the mid point
                            d3.select("#pvline2_mid")
                                .attr("cx", (x1 + x2) / 2)
                                .attr("cy", (y1 + y2) / 2);

                            const dx = x2 - x1;
                            const dy = y2 - y1;

                            const _x = (x1 + x2) / 2; // midpoint of the P-V line
                            const _y = (y1 + y2) / 2; // midpoint of the P-V line

                            var _m, _s, _mx, _my, _x1, _x2, _y1, _y2;

                            if (Math.abs(dx) > 0) {
                                _m = dy / dx; // a line parallel to the P-V line
                                _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                                _mx = _s;
                                _my = _m * _s;

                                _x1 = _x - _mx;
                                _y1 = _y - _my;
                                _x2 = _x + _mx;
                                _y2 = _y + _my;

                                d3.select("#pvmid3")
                                    .attr("x1", _x1)
                                    .attr("y1", _y1)
                                    .attr("x2", _x2)
                                    .attr("y2", _y2)
                                    .attr("opacity", 1.0);
                            }

                            if (Math.abs(dy) > 0) {
                                _m = - dx / dy; // a perpendicular line to the P-V line
                                _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                                _mx = _s;
                                _my = _m * _s;

                                _x1 = _x - _mx;
                                _y1 = _y - _my;
                                _x2 = _x + _mx;
                                _y2 = _y + _my;

                                d3.select("#pvmid2")
                                    .attr("x1", _x1)
                                    .attr("y1", _y1)
                                    .attr("x2", _x2)
                                    .attr("y2", _y2)
                                    .attr("opacity", 1.0);
                            }
                        })
                        .on("end", function (event) {
                            event.preventDefault = true;

                            clearTimeout(pv_loop);
                            resubmit_pv_line();
                        }))
                    .attr("opacity", 1.0);

                // add a circle in the middle of the line
                svg.append("circle")
                    .attr("id", "pvline2_mid")
                    .attr("cx", 0.5 * (res.x1 + res.x2) * img_width)
                    .attr("cy", 0.5 * (res.y1 + res.y2) * img_height)
                    .attr("r", emFontSize / 2)
                    .style("fill", "transparent")
                    .style("stroke", fillColour)
                    .style("stroke-width", emStrokeWidth)
                    .attr("pointer-events", "auto")
                    .style('cursor', 'move')
                    .call(d3.drag()
                        .on("start", function (event) {
                            event.preventDefault = true;

                            // start the pv event loop
                            pv_loop = setTimeout(loop_pv_line, pv_latency);
                        })
                        .on("drag", dragMid)
                        .on("end", function (event) {
                            event.preventDefault = true;

                            clearTimeout(pv_loop);
                            resubmit_pv_line();
                        }))
                    .attr("opacity", 1.0);

                // add an ending point 'B'
                svg.append("text")
                    .attr("id", "pvpointB")
                    .attr("x", res.x2 * img_width)
                    .attr("y", res.y2 * img_height)
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "central")
                    .attr("font-size", emFontSize)
                    .attr("fill", fillColour)
                    .attr("pointer-events", "none")
                    .attr("opacity", 1.0)
                    .text("2");

                // add a circle at the end of the line
                svg.append("circle")
                    .attr("id", "pvline2_end")
                    .attr("cx", res.x2 * img_width)
                    .attr("cy", res.y2 * img_height)
                    .attr("r", emFontSize / 2)
                    .style("fill", "transparent")
                    .style("stroke", fillColour)
                    .style("stroke-width", emStrokeWidth)
                    .attr("pointer-events", "auto")
                    .style('cursor', 'move')
                    .call(d3.drag()
                        .on("start", function (event) {
                            event.preventDefault = true;

                            // start the pv event loop
                            pv_loop = setTimeout(loop_pv_line, pv_latency);
                        })
                        .on("drag", function (event) {
                            event.preventDefault = true;

                            var offset = d3.pointer(event);
                            let x = offset[0] - pvsvg_left; // the SVG offset
                            let y = offset[1] - pvsvg_top; // the SVG offset

                            // check if the point is within the image
                            x = Math.min(Math.max(x, 0), pvsvg_width - 1);
                            y = Math.min(Math.max(y, 0), pvsvg_height - 1);

                            d3.select("#pvpointB")
                                .attr("x", x)
                                .attr("y", y);

                            d3.select("#pvline2_end")
                                .attr("cx", x)
                                .attr("cy", y);

                            d3.select("#pvline2")
                                .attr("x2", x)
                                .attr("y2", y);

                            var line = d3.select("#pvline2");
                            var x1 = parseFloat(line.attr("x1"));
                            var y1 = parseFloat(line.attr("y1"));
                            var x2 = parseFloat(line.attr("x2"));
                            var y2 = parseFloat(line.attr("y2"));

                            // re-centre the mid point
                            d3.select("#pvline2_mid")
                                .attr("cx", (x1 + x2) / 2)
                                .attr("cy", (y1 + y2) / 2);

                            const dx = x2 - x1;
                            const dy = y2 - y1;

                            const _x = (x1 + x2) / 2; // midpoint of the P-V line
                            const _y = (y1 + y2) / 2; // midpoint of the P-V line

                            var _m, _s, _mx, _my, _x1, _x2, _y1, _y2;

                            if (Math.abs(dx) > 0) {
                                _m = dy / dx; // a line parallel to the P-V line
                                _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                                _mx = _s;
                                _my = _m * _s;

                                _x1 = _x - _mx;
                                _y1 = _y - _my;
                                _x2 = _x + _mx;
                                _y2 = _y + _my;

                                d3.select("#pvmid3")
                                    .attr("x1", _x1)
                                    .attr("y1", _y1)
                                    .attr("x2", _x2)
                                    .attr("y2", _y2)
                                    .attr("opacity", 1.0);
                            }

                            if (Math.abs(dy) > 0) {
                                _m = - dx / dy; // a perpendicular line to the P-V line
                                _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

                                _mx = _s;
                                _my = _m * _s;

                                _x1 = _x - _mx;
                                _y1 = _y - _my;
                                _x2 = _x + _mx;
                                _y2 = _y + _my;

                                d3.select("#pvmid2")
                                    .attr("x1", _x1)
                                    .attr("y1", _y1)
                                    .attr("x2", _x2)
                                    .attr("y2", _y2)
                                    .attr("opacity", 1.0);
                            }
                        })
                        .on("end", function (event) {
                            event.preventDefault = true;

                            clearTimeout(pv_loop);
                            resubmit_pv_line();
                        }))
                    .attr("opacity", 1.0);
            }

        } else {
            d3.select("#pvline").attr("opacity", 0.0);
            d3.select("#pvmid").attr("opacity", 0.0);
        }
    } else {
        // start a new P-V line
        var offset = d3.pointer(event);
        line_x = offset[0];
        line_y = offset[1];

        // disabled in order to show the viewport circle and the zoom cross
        /*d3.select("#zoom").attr("opacity", 0.0);
        d3.select("#zoomCross").attr("opacity", 0.0);*/

        d3.select(this).style('cursor', 'crosshair');

        d3.select("#pixel").text("").attr("opacity", 0.0);
        d3.select("#ra").text("");
        d3.select("#dec").text("");

        // disable the end marker and make the line visible
        d3.select("#pvline").attr("marker-end", "").attr("opacity", 1.0);
        d3.select("#pvmid").attr("opacity", 1.0);
    }
}

function dragMid(event) {
    event.preventDefault = true;

    var offset = d3.pointer(event);
    let x = offset[0] - pvsvg_left; // the SVG offset
    let y = offset[1] - pvsvg_top; // the SVG offset

    // check if the point is within the image
    x = Math.min(Math.max(x, 0), pvsvg_width - 1);
    y = Math.min(Math.max(y, 0), pvsvg_height - 1);

    var mid = d3.select("#pvline2_mid");
    let cx = parseFloat(mid.attr("cx"));
    let cy = parseFloat(mid.attr("cy"));

    mid.attr("cx", x);
    mid.attr("cy", y);

    let dx = x - cx;
    let dy = y - cy;

    var line = d3.select("#pvline2");
    var x1 = parseFloat(line.attr("x1")) + dx;
    var y1 = parseFloat(line.attr("y1")) + dy;
    var x2 = parseFloat(line.attr("x2")) + dx;
    var y2 = parseFloat(line.attr("y2")) + dy;

    // re-set the PV line ends
    line.attr("x1", x1);
    line.attr("y1", y1);
    line.attr("x2", x2);
    line.attr("y2", y2);

    d3.select("#pvpointA")
        .attr("x", x1)
        .attr("y", y1);

    d3.select("#pvline2_start")
        .attr("cx", x1)
        .attr("cy", y1);

    d3.select("#pvpointB")
        .attr("x", x2)
        .attr("y", y2);

    d3.select("#pvline2_end")
        .attr("cx", x2)
        .attr("cy", y2);

    dx = x2 - x1;
    dy = y2 - y1;

    const _x = (x1 + x2) / 2; // midpoint of the P-V line
    const _y = (y1 + y2) / 2; // midpoint of the P-V line

    var _m, _s, _mx, _my, _x1, _x2, _y1, _y2;

    if (Math.abs(dx) > 0) {
        _m = dy / dx; // a line parallel to the P-V line
        _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

        _mx = _s;
        _my = _m * _s;

        _x1 = _x - _mx;
        _y1 = _y - _my;
        _x2 = _x + _mx;
        _y2 = _y + _my;

        d3.select("#pvmid3")
            .attr("x1", _x1)
            .attr("y1", _y1)
            .attr("x2", _x2)
            .attr("y2", _y2)
            .attr("opacity", 1.0);
    }

    if (Math.abs(dy) > 0) {
        _m = - dx / dy; // a perpendicular line to the P-V line
        _s = emFontSize / Math.sqrt(1 + _m * _m) / 2;

        _mx = _s;
        _my = _m * _s;

        _x1 = _x - _mx;
        _y1 = _y - _my;
        _x2 = _x + _mx;
        _y2 = _y + _my;

        d3.select("#pvmid2")
            .attr("x1", _x1)
            .attr("y1", _y1)
            .attr("x2", _x2)
            .attr("y2", _y2)
            .attr("opacity", 1.0);
    }
}

function fits_subregion_start(event) {
    if (freqdrag) return;
    if (optical_view) return;

    clearTimeout(idleMouse);
    moving = true;
    windowLeft = false;

    d3.select("#" + zoom_location).style("stroke", "transparent");
    d3.select("#" + zoom_location + "Cross").attr("opacity", 0.0);
    d3.select("#" + zoom_location + "Beam").attr("opacity", 0.0);

    d3.select("#pixel").text("").attr("opacity", 0.0);
    d3.select("#ra").text("");
    d3.select("#dec").text("");

    if (viewport != null) {
        // Clear the ZOOM Canvas
        //console.log("clearing the ZOOM Canvas");
        var gl = viewport.gl;

        if (gl !== undefined && gl != null) {
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT);
        }
    }

    clear_webgl_viewport();

    {
        var c = document.getElementById("SpectrumCanvas");
        var ctx = c.getContext("2d");
        var width = c.width;
        var height = c.height;
        ctx.clearRect(0, 0, width, height);
    }

    var offset = d3.pointer(event);
    begin_x = offset[0];
    begin_y = offset[1];

    // console.log("fits_subregion_start", offset);
    fits_subregion_drag_event = false;

    mousedown = true;
    d3.select("#zoom").attr("opacity", 0.0);
    d3.select("#zoomCross").attr("opacity", 0.0);
}

function fits_subregion_drag(event) {
    if (freqdrag) return;
    if (optical_view) return;
    if (d3.select("#pvline").attr("opacity") > 0.0) return;

    fits_subregion_drag_event = true;
    // console.log("fits_subregion_drag");

    d3.select("#zoom").attr("opacity", 0.0);
    d3.select("#zoomCross").attr("opacity", 0.0);
    d3.select(this).style('cursor', 'crosshair');

    d3.select("#pixel").text("").attr("opacity", 0.0);
    d3.select("#ra").text("");
    d3.select("#dec").text("");

    try {
        if (viewport != null) {
            // Clear the ZOOM Canvas
            //console.log("clearing the ZOOM Canvas");
            var gl = viewport.gl;

            if (gl !== undefined && gl != null) {
                gl.clearColor(0, 0, 0, 0);
                gl.clear(gl.COLOR_BUFFER_BIT);
            }
        }

        clear_webgl_viewport();

        {
            var c = document.getElementById("SpectrumCanvas");
            var ctx = c.getContext("2d");
            var width = c.width;
            var height = c.height;
            ctx.clearRect(0, 0, width, height);
        }
    } catch (_) { }

    if (mousedown) {
        let x1 = begin_x;
        let y1 = begin_y;

        var offset = d3.pointer(event);

        let x2 = offset[0]; let y2 = offset[1];

        if (x2 < x1) { x2 = x1; x1 = offset[0]; };
        if (y2 < y1) { y2 = y1; y1 = offset[1]; };

        let dx = x2 - x1; let dy = y2 - y1;

        // apply a 10-pixel correction to dx and dy
        if (dx > 10) dx = dx - 10;
        if (dy > 10) dy = dy - 10;

        d3.select("#region").attr("x", x1).attr("y", y1).attr("width", dx).attr("height", dy).attr("opacity", 1.0);//.5
    }
}

function fits_subregion_end(event) {
    if (freqdrag) return;
    if (optical_view) return;

    var offset = d3.pointer(event);
    end_x = offset[0];
    end_y = offset[1];

    // console.log("fits_subregion_end", offset);

    mousedown = false;
    d3.select("#zoom").attr("opacity", 1.0);
    d3.select("#region").attr("opacity", 0.0);

    if (!fits_subregion_drag_event) {
        // console.log("a single click detected, cancelling the subregion selection");
        return;
    }

    if (end_x == begin_x || end_y == begin_y) {
        console.log("an invalid partial download region");
        return cancel_download();
    }

    if (displayDownloadConfirmation) {
        var partialSize = partial_fits_size();

        download_confirmation(partialSize);
    }
    else
        partial_fits_download();

    /*var evt = new MouseEvent("mousemove");
    d3.select('#image_rectangle').node().dispatchEvent(evt);*/

    /*var event = document.createEvent('SVGEvents');
    event.initEvent('mousemove', true, true);
    d3.select('#image_rectangle').node().dispatchEvent(event);*/
}

function get_diagonal_image_position(index, width, height) {
    let basex = width / 2;
    let basey = height / 2;
    let t = index / (va_count + 1);
    t = 2 * t - 1;
    let posx = basex + t * 0.5 * width;//0.5 - overlap, 0.6 - no overlap
    let posy = basey + t * height / 4;

    var image_position = { posx: posx, posy: posy };

    return image_position;
}

function get_square_image_position_4(index, width, height) {
    let offset_x = 0, offset_y = 0;

    if (width >= height)
        offset_x = 0.025 * width;
    else
        offset_y = 0.025 * height;

    if (index == 1)
        return { posx: width / 4 - offset_x, posy: height / 4 - offset_y };

    if (index == 2)
        return { posx: width - width / 4 - offset_x, posy: height / 4 + offset_y };

    if (index == 3)
        return { posx: width / 4 + offset_x, posy: height - height / 4 - offset_y };

    if (index == 4)
        return { posx: width - width / 4 + offset_x, posy: height - height / 4 + offset_y };

    return { posx: width / 2, posy: height / 2 };
}

function get_diagonal_image_position_4(index, width, height) {
    //the diagonal line to the left
    if (index < 3) {
        let basex = width / 4;
        let basey = height / 2;

        let t = index / (va_count - 3 + 1);
        t = 2 * t - 1;
        let posx = basex + t * 0.5 * width / 2;
        let posy = basey + t * height / 2;

        return { posx: posx, posy: posy };
    }

    //the diagonal line to the right
    let basex = width - width / 4;
    let basey = height / 2;

    let t = (index - 2) / (va_count - 3 + 1);
    t = 2 * t - 1;
    let posx = basex + t * 0.5 * width / 2;
    let posy = basey + t * height / 2;

    return { posx: posx, posy: posy };
}

function get_image_position_5(index, width, height) {
    if (index < 5)
        return get_square_image_position_4(index, width, height);
    else
        return { posx: width / 2, posy: height / 2 };
}

function get_horizontal_image_position_6(index, width, height) {
    let offset_x = 0, offset_y = 0;

    offset_x = 0.025 * width;
    offset_y = 0.025 * height;

    if (index == 1)
        return { posx: width / 4 - offset_x, posy: height / 4 };

    if (index == 2)
        return { posx: width / 2 - offset_x, posy: height / 4 + offset_y };

    if (index == 3)
        return { posx: 3 * width / 4 - offset_x, posy: height / 4 };

    if (index == 4)
        return { posx: width / 4 + offset_x, posy: height - height / 4 };

    if (index == 5)
        return { posx: width / 2 + offset_x, posy: height - height / 4 - offset_y };

    if (index == 6)
        return { posx: 3 * width / 4 + offset_x, posy: height - height / 4 };

    return { posx: width / 2, posy: height / 2 };
}


function get_vertical_image_position_6(index, width, height) {
    let offset_x = 0, offset_y = 0;

    offset_x = 0.025 * width;
    offset_y = 0.025 * height;

    if (index == 1)
        return { posx: width / 4, posy: height / 4 - offset_y };

    if (index == 2)
        return { posx: width - width / 4, posy: height / 4 + offset_y };

    if (index == 3)
        return { posx: width / 4 + offset_x, posy: height / 2 - offset_y };

    if (index == 4)
        return { posx: width - width / 4 - offset_x, posy: height / 2 + offset_y };

    if (index == 5)
        return { posx: width / 4, posy: height - height / 4 - offset_y };

    if (index == 6)
        return { posx: width - width / 4, posy: height - height / 4 + offset_y };

    return { posx: width / 2, posy: height / 2 };
}

function get_image_position_6(index, width, height) {
    if (width >= height)
        return get_horizontal_image_position_6(index, width, height);
    else
        return get_vertical_image_position_6(index, width, height);
}

function get_diagonal_image_position_7(index, width, height) {
    //the middle diagonal
    if (index <= 3) {
        let basex = width / 2;
        let basey = height / 2;
        let t = index / 4;
        t = 2 * t - 1;
        let posx = basex + t * width / 3;
        let posy = basey - t * height / 2;

        return { posx: posx, posy: posy };
    }

    //the left diagonal
    if (index <= 5) {
        let basex = width / 3.5;
        let basey = height / 3.5;
        let t = (index - 3) / 4;
        t = 2 * t - 1;
        let posx = basex + t * width / 3;
        let posy = basey - t * height / 2;

        return { posx: posx, posy: posy };
    }

    //the right diagonal
    if (index <= 7) {
        let basex = width - width / 8;
        let basey = height - height / 2;
        let t = (index - 5) / 4;
        t = 2 * t - 1;
        let posx = basex + t * width / 3;
        let posy = basey - t * height / 2;

        return { posx: posx, posy: posy };
    }

    return { posx: width / 2, posy: height / 2 };
}

function get_image_position(index, width, height) {
    if (va_count <= 4)
        return get_diagonal_image_position(index, width, height);

    if (va_count == 5)
        return get_image_position_5(index, width, height);

    if (va_count == 6)
        return get_image_position_6(index, width, height);

    if (va_count == 7)
        return get_diagonal_image_position_7(index, width, height);

    return get_diagonal_image_position(index, width, height);
}

function isNumeric(obj) {
    return !isNaN(obj - parseFloat(obj));
}

var isotopes = ["1H", "2H", "3H", "3He", "4He", "6Li", "7Li", "9Be", "10B", "11B", "12C", "13C", "14C", "14N", "15N", "16O", "17O", "18O", "19F", "20Ne", "21Ne", "22Ne", "23Na", "24Mg", "25Mg", "26Mg", "27Al", "28Si", "29Si", "30Si", "31P", "32S", "33S", "34S", "36S", "35Cl", "37Cl", "36Ar", "38Ar", "40Ar", "39K", "40K", "41K", "40Ca", "42Ca", "43Ca", "44Ca", "46Ca", "48Ca", "45Sc", "46Ti", "47Ti", "48Ti", "49Ti", "50Ti", "50V", "51V", "50Cr", "52Cr", "53Cr", "54Cr", "55Mn", "54Fe", "56Fe", "57Fe", "58Fe", "59Co", "58Ni", "60Ni", "61Ni", "62Ni", "64Ni", "63Cu", "65Cu", "64Zn", "66Zn", "67Zn", "68Zn", "70Zn", "69Ga", "71Ga", "70Ge", "72Ge", "73Ge", "74Ge", "76Ge", "75As", "74Se", "76Se", "77Se", "78Se", "80Se", "82Se", "79Br", "81Br", "78Kr", "80Kr", "82Kr", "83Kr", "84Kr", "86Kr", "85Rb", "87Rb", "84Sr", "86Sr", "87Sr", "88Sr", "89Y", "90Zr", "91Zr", "92Zr", "94Zr", "93Nb", "92Mo", "94Mo", "95Mo", "96Mo", "97Mo", "98Mo", "100Mo", "98Tc", "96Ru", "98Ru", "99Ru", "100Ru", "101Ru", "102Ru", "104Ru", "103Rh", "102Pd", "104Pd", "105Pd", "106Pd", "108Pd", "110Pd", "107Ag", "109Ag", "106Cd", "108Cd", "110Cd", "111Cd", "112Cd", "113Cd", "114Cd", "116Cd", "113In", "115In", "112Sn", "114Sn", "115Sn", "116Sn", "117Sn", "118Sn", "119Sn", "120Sn", "122Sn", "124Sn", "121Sb", "123Sb", "120Te", "122Te", "123Te", "124Te", "125Te", "126Te", "128Te", "130Te", "127I", "124Xe", "126Xe", "128Xe", "129Xe", "130Xe", "131Xe", "132Xe", "134Xe", "133Cs", "130Ba", "132Ba", "134Ba", "135Ba", "136Ba", "137Ba", "138Ba", "138La", "139La", "136Ce", "138Ce", "140Ce", "142Ce", "141Pr", "142Nd", "143Nd", "144Nd", "145Nd", "146Nd", "148Nd", "150Nd", "145Pm", "144Sm", "147Sm", "148Sm", "149Sm", "150Sm", "152Sm", "154Sm", "151Eu", "153Eu", "152Gd", "154Gd", "155Gd", "156Gd", "157Gd", "158Gd", "160Gd", "159Tb", "156Dy", "158Dy", "160Dy", "161Dy", "162Dy", "163Dy", "154Dy", "165Ho", "162Er", "164Er", "166Er", "167Er", "168Er", "170Er", "169Tm", "168Yb", "170Yb", "171Yb", "172Yb", "173Yb", "174Yb", "176Yb", "175Lu", "176Lu", "174Hf", "176Hf", "177Hf", "178Hf", "179Hf", "180Hf", "180Ta", "181Ta", "180W", "182W", "183W", "184W", "186W", "185Re", "187Re", "184Os", "186Os", "187Os", "188Os", "189Os", "190Os", "192Os", "191Ir", "193Ir", "190Pt", "192Pt", "194Pt", "195Pt", "196Pt", "198Pt", "197Au", "196Hg", "198Hg", "199Hg", "200Hg", "201Hg", "202Hg", "204Hg", "203Tl", "205Tl", "204Pb", "206Pb", "207Pb", "208Pb", "209Bi", "209Po", "210At", "222Rn", "223Fr", "226Ra", "227Ac", "232Th", "231Pa", "234U", "235U", "238U", "237Np", "244Pu", "243Am", "247Cm", "247Bk", "251Cf", "252Es", "257Fm", "258Md", "259No", "262Lr", "263Rf", "262Db", "266Sg", "264Bh", "269Hs", "268Mt", "272Uun", "272Uuu", "277Uub", "289Uuq", "289Uuh", "292Uuo"];

function chemical_isotopes(line, baseline) {
    var i, j;

    var source = line;
    var dest = '';

    var pos = -1;

    for (i = 0; i < isotopes.length; i++) {
        pos = source.indexOf(isotopes[i]);

        if (pos > -1) {
            console.log("found " + isotopes[i] + " at pos " + pos);

            dest = source.substring(0, pos);

            if (baseline)
                dest += '<SUP style="font-size: smaller; vertical-align:baseline">';
            else
                dest += '<SUP style="font-size: smaller;">';

            var len = isotopes[i].length;

            for (j = 0; j < len; j++) {
                if (isNumeric(isotopes[i].charAt(j))) {
                    dest += isotopes[i].charAt(j);
                }
                else {
                    dest += "</SUP>" + isotopes[i].substring(j);
                    break;
                };
            }

            //append the remaining formula
            dest += source.substring(pos + len);

            //overwrite the source with a revised version
            source = dest;
        };
    };

    return source;
}

function plain2chem(line, baseline) {
    return chemical_isotopes(line, baseline);
}

function add_line_label(index) {
    if (va_count == 1)
        return;

    let fitsData = fitsContainer[index - 1];

    if (fitsData == null)
        return;

    let line = fitsData.LINE.trim();
    let filter = fitsData.FILTER.trim();

    if (line == "")
        //line = "line #" + index ;
        line = datasetId[index - 1];

    //console.log("SPECTRAL LINE:", line, "FILTER:", filter);

    var label;

    if (filter == "")
        label = plain2chem(line, false);
    else
        label = filter;

    if (imageContainer[index - 1] == null)
        return;

    let image_bounding_dims = imageContainer[index - 1].image_bounding_dims;

    let c = document.getElementById('HTMLCanvas' + index);
    let width = c.width;
    let height = c.height;

    let scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);

    if (va_count == 2)
        scale = 0.8 * scale;
    else if (va_count == 4)
        scale = 0.6 * scale;
    else if (va_count == 5)
        scale = 0.5 * scale;
    else if (va_count == 6)
        scale = 0.45 * scale;
    else if (va_count == 7)
        scale = 0.45 * scale;
    else
        scale = 2 * scale / va_count;

    let img_width = Math.floor(scale * image_bounding_dims.width);
    let img_height = Math.floor(scale * image_bounding_dims.height);

    let image_position = get_image_position(index, width, height);
    let posx = image_position.posx;
    let posy = image_position.posy;

    var svg = d3.select("#BackSVG");

    let fontColour = 'gray';//white

    if (theme == 'bright')
        fontColour = 'gray';

    if (colourmap == "greyscale" || colourmap == "negative")
        fontColour = "#C4A000";

    svg.append("foreignObject")
        .attr("x", (posx - img_width / 2))
        .attr("y", (posy - img_height / 2 - 1.75 * emFontSize))
        .attr("width", img_width)
        .attr("height", 2 * emFontSize)
        .append("xhtml:div")
        .attr("id", "line_display")
        .html('<p style="text-align: center; font-size:1.5em; font-family: Inconsolata; font-weight: bold; color:' + fontColour + '">' + label + '</p>');
};

function display_composite_legend() {
    var svg = d3.select("#FrontSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var group = d3.select("#information");

    //var strLegend = '<div class="container-fluid" style="color: lightgray;">' ;
    var strLegend = '<div class="container-fluid">';

    for (let index = 0; index < va_count; index++) {
        if (index >= 3)
            break;

        let fitsData = fitsContainer[index];
        let line = fitsData.LINE.trim();
        let filter = fitsData.FILTER.trim();

        if (filter != "")
            line = filter;
        else {
            if (line == "")
                line = "line-" + (index + 1);
        }

        //<canvas id="LEG' + line + '" style="width:2em;height:1em;display:inline-block"></canvas>

        //style="height:0.7em;display:inline-block;"
        //scale:&nbsp;
        var strSelect = '<br><div style="font-size:100%; font-family:Inconsolata;float:right;"><label for="scale' + (index + 1) + '" class="control-label"></label><select onchange="javascript:change_spectrum_scale(' + (index + 1) + ')" class="no-form-control" style="max-width:4em;max-height:1.5em;color:black;" id="scale' + (index + 1) + '"><option class="custom-option" value="1">x1</option><option class="custom-option" value="2">x2</option><option class="custom-option" value="5">x5</option><option class="custom-option" value="10">x10</option></select></div>';

        if (index != va_count - 1)
            //strSelect += '<br><hr width="66%">' ;
            strSelect += '<br><br>';

        strLegend += '<div><div id="DIV' + line + '" style="width:5em;height:1em;display:inline-block"><img id="IMG' + line + '" src="" alt="linedash" width="100%" height="100%"></div><span style="font-size:100%; font-family:Inconsolata; color:' + colours[index] + ';">&nbsp;■&nbsp;</span><span style="font-size:100%; font-family:Helvetica; font-weight:bold; nocolor:' + colours[index] + ';">' + plain2chem(line, false) + '</span>&nbsp;' + strSelect + '</div>';
    }

    strLegend += '</div>';

    group.append("g")
        .attr("id", "foreignRGBGroup")
        .style("opacity", 1.0)
        .append("foreignObject")
        .attr("id", "foreignRGB")
        .attr("x", (width - 25 * emFontSize))
        .attr("y", 12.5 * emFontSize)//12.5em 10.5em
        .attr("width", 25 * emFontSize)
        .attr("height", 25 * emFontSize)//10*
        /*.on("mouseenter", function () {
          d3.select("#foreignRGBGroup").style("opacity", 1.0) ;
          })
          .on("mouseleave", function () {
          d3.select("#foreignRGBGroup").style("opacity", 0.25) ;
          })*/
        .append("xhtml:div")
        .attr("id", "rgbDiv")
        .attr("class", "container-fluid input")
        .style("float", "right")
        .style("padding", "2.5%")
        .append("span")
        .html(strLegend);

    for (let index = 0; index < va_count; index++) {
        let fitsData = fitsContainer[index];
        let line = fitsData.LINE.trim();
        let filter = fitsData.FILTER.trim();

        if (filter != "")
            line = filter;
        else {
            if (line == "")
                line = "line-" + (index + 1);
        }

        let lineCanvas = document.createElement('canvas');
        lineCanvas.style.visibility = "hidden";

        let width = 3 * emFontSize;
        let height = emFontSize;

        lineCanvas.width = width;
        lineCanvas.height = height;

        var ctx = lineCanvas.getContext('2d');

        ctx.save();
        ctx.beginPath();

        ctx.strokeStyle = getStrokeStyle();
        ctx.setLineDash(linedash[index % linedash.length]);
        ctx.lineWidth = 1;
        ctx.strokeWidth = emStrokeWidth;

        ctx.moveTo(0, height / 2);
        ctx.lineTo(width, height / 2);

        ctx.stroke();
        ctx.closePath();
        ctx.restore();

        var src = lineCanvas.toDataURL();

        d3.select("#IMG" + line)
            .attr("src", src);
    }

    try {
        d3.select("#videoControlG").moveToFront();
    } catch (err) { };
}

function setup_image_selection_index(index, topx, topy, img_width, img_height) {
    //delete previous instances
    try {
        d3.select("#region").remove();
        d3.select("#zoom").remove();
        d3.select("#zoomCross").remove();
        d3.select("#image_rectangle" + index).remove();
    }
    catch (e) { };

    var zoom = d3.zoom()
        .scaleExtent([1, 40])
        .on("start", tiles_zoomstarted)
        .on("zoom", tiles_zoom)
        .on("end", tiles_zoomended);

    var drag = d3.drag()
        .on("start", tiles_dragstarted)
        .on("drag", tiles_dragmove)
        .on("end", tiles_dragended);

    now = performance.now();
    then = now;

    //set up the spectrum rendering loop
    function update_spectrum() {

        if (!windowLeft)
            requestAnimationFrame(update_spectrum);

        //spectrum
        try {
            let go_ahead = true;
            let new_seq_id = 0;

            for (let index = 0; index < va_count; index++) {
                let len = spectrum_stack[index].length;

                if (len > 0) {
                    let id = spectrum_stack[index][len - 1].id;

                    if (id <= last_seq_id)
                        go_ahead = false;
                    else
                        new_seq_id = Math.max(new_seq_id, id);
                }
                else
                    go_ahead = false;
            }

            if (go_ahead) {
                last_seq_id = new_seq_id;
                //console.log("last_seq_id:", last_seq_id);

                //pop all <va_count> spectrum stacks
                var data = [];

                for (let index = 0; index < va_count; index++) {
                    data.push(spectrum_stack[index].pop().spectrum);
                    spectrum_stack[index] = [];
                }

                plot_spectrum(data);
                replot_y_axis();

                last_spectrum = data;
            }

        }
        catch (e) {
            console.log(e);
        }
    }

    // a fix for Safari
    d3.select(document.body)
        .on('wheel.body', e => { });

    var svg = d3.select("#FrontSVG");

    //add a bounding box
    if (theme == 'bright')
        var strokeStyle = "white";
    else
        var strokeStyle = "black";

    //svg image rectangle for zooming-in
    var rect = svg.append("rect")
        .attr("id", "image_rectangle" + index)
        .attr("class", "image_rectangle")
        .attr("x", Math.round(topx))
        .attr("y", Math.round(topy))
        .attr("width", Math.round(img_width))
        .attr("height", Math.round(img_height))
        .style('cursor', 'pointer')//'crosshair')//'none' to mask Chrome latency
        .style("fill", "transparent")
        .style("stroke", strokeStyle)
        .style("stroke-width", 2)
        .attr("opacity", (index == va_count) ? 0.5 : 0.0)
        .call(drag)
        .call(zoom)
        .on("click", function () {
            if (isLocal) {
                //parse window.location to get the value of filename<index>
                let params = window.location.search.split("&");
                //console.log('URL PARAMS:', params);

                let search = 'filename' + index;

                for (let i = 0; i < params.length; i++) {
                    if (params[i].indexOf(search) > -1) {
                        //console.log("found a parameter", params[i]);
                        let values = params[i].split("=");

                        if (values.length > 1) {
                            let val = values[values.length - 1];
                            //console.log('VALUE:', val);

                            window.location = window.location + '&filename=' + val;

                            //window.location = window.location + '&filename=' + encodeURIComponent(datasetId[index-1]) ;
                        }
                    }
                }
            }
            else {
                //parse window.location to get the value of datasetId<index>
                let params = window.location.search.split("&");
                //console.log('URL PARAMS:', params);

                let search = 'datasetId' + index;

                for (let i = 0; i < params.length; i++) {
                    if (params[i].indexOf(search) > -1) {
                        //console.log("found a parameter", params[i]);
                        let values = params[i].split("=");

                        if (values.length > 1) {
                            let val = values[values.length - 1];
                            //console.log('VALUE:', val);

                            window.location = window.location + '&datasetId=' + val;

                            //window.location = window.location + '&datasetId=' + encodeURIComponent(datasetId[index-1]) ;
                        }
                    }
                }
            }
        })
        .on("mouseenter", function (event) {
            hide_navigation_bar();
            console.log("switching active view to", d3.select(this).attr("id"));

            for (let i = 0; i < va_count; i++)
                d3.select("#image_rectangle" + (i + 1)).attr("opacity", 0.0);
            d3.select(this).attr("opacity", 0.5);

            d3.select(this).moveToFront();
            dragging = false;

            windowLeft = false;

            spectrum_stack = new Array(va_count);
            for (let i = 0; i < va_count; i++)
                spectrum_stack[i] = [];

            requestAnimationFrame(update_spectrum);

            var imageElements = document.getElementsByClassName("image_rectangle");

            for (let i = 0; i < imageElements.length; i++) {
                let element = imageElements[i];

                let attr = element.getAttribute("id");
                let idx = attr.substring(15);

                d3.select("#HTMLCanvas" + idx).style('z-index', i + 1);
            }

            var fitsData = fitsContainer[index - 1];

            if (fitsData == null)
                return;

            if (imageContainer[index - 1] == null)
                return;

            let image_bounding_dims = imageContainer[index - 1].image_bounding_dims;

            if (zoom_dims == null) {
                zoom_dims = {
                    scale: 1.0,
                    dims: image_bounding_dims,
                    x1: image_bounding_dims.x1, y1: image_bounding_dims.y1, width: image_bounding_dims.width, height: image_bounding_dims.height, x0: image_bounding_dims.x1 + 0.5 * (image_bounding_dims.width - 1), y0: image_bounding_dims.y1 + 0.5 * (image_bounding_dims.height - 1),
                    dx: 0,
                    dy: 0,
                    prev_x0: -1,
                    prev_y0: -1,
                    view: null,
                    prev_view: null,
                    rect: null,
                    mouse_position: null
                };
            }
        })
        .on("mouseleave", function (event) {
            windowLeft = true;

            spectrum_stack = new Array(va_count);
            for (let i = 0; i < va_count; i++)
                spectrum_stack[i] = [];

            if (xradec != null) {
                let fitsData = fitsContainer[va_count - 1];

                let raText = 'RA N/A';
                let decText = 'DEC N/A';

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        raText = 'α: ' + RadiansPrintDMS(xradec[0]);
                    else
                        raText = 'α: ' + RadiansPrintHMS(xradec[0]);
                }

                if (fitsData.CTYPE1.indexOf("GLON") > -1)
                    raText = 'l: ' + RadiansPrintDMS(xradec[0]);

                if (fitsData.CTYPE1.indexOf("ELON") > -1)
                    raText = 'λ: ' + RadiansPrintDMS(xradec[0]);

                if (fitsData.CTYPE2.indexOf("DEC") > -1)
                    decText = 'δ: ' + RadiansPrintDMS(xradec[1]);

                if (fitsData.CTYPE2.indexOf("GLAT") > -1)
                    decText = 'b: ' + RadiansPrintDMS(xradec[1]);

                if (fitsData.CTYPE2.indexOf("ELAT") > -1)
                    decText = 'β: ' + RadiansPrintDMS(xradec[1]);

                //console.log(raText, decText);
                d3.select("#ra").text(raText);
                d3.select("#dec").text(decText);
            }
        })
        .on("mousemove", function (event) {
            //moving = true;
            windowLeft = false;

            if (zoom_dims == null)
                return;

            var offset;

            try {
                offset = d3.pointer(event);
            }
            catch (e) {
                console.log(e);
                return;
            }

            if (isNaN(offset[0]) || isNaN(offset[1]))
                return;

            mouse_position = { x: offset[0], y: offset[1] };

            var fitsData = fitsContainer[index - 1];

            if (fitsData == null)
                return;

            if (imageContainer[index - 1] == null)
                return;

            var image = imageContainer[index - 1];
            var image_bounding_dims = image.image_bounding_dims;

            if (zoom_dims.view != null)
                image_bounding_dims = zoom_dims.view;

            let rect = event.currentTarget;

            var ax = (image_bounding_dims.width - 1) / (rect.getAttribute("width") - 0);
            var x = image_bounding_dims.x1 + ax * (mouse_position.x - rect.getAttribute("x"));

            var ay = (image_bounding_dims.height - 1) / (rect.getAttribute("height") - 0);
            var y = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (mouse_position.y - rect.getAttribute("y"));

            x = clamp(x, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
            y = clamp(y, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

            zoom_dims.x0 = Math.round(x);
            zoom_dims.y0 = Math.round(y);
            zoom_dims.rect = rect;
            zoom_dims.mouse_position = mouse_position;

            var orig_x = x * (fitsData.width - 1) / (image.width - 1);
            var orig_y = y * (fitsData.height - 1) / (image.height - 1);
            // console.log("orig_x:", orig_x, "orig_y:", orig_y);

            let world = pix2sky(fitsData, orig_x, orig_y);
            // if either world value is NaN throw an error
            if (isNaN(world[0]) || isNaN(world[1]))
                throw new Error("NaN WCS");

            let radec = [world[0] / toDegrees, world[1] / toDegrees];
            // console.log("world:", world, "radec:", radec);

            let raText = 'RA N/A';
            let decText = 'DEC N/A';

            if (fitsData.CTYPE1.indexOf("RA") > -1) {
                if (coordsFmt == 'DMS')
                    raText = 'α: ' + RadiansPrintDMS(radec[0]);
                else
                    raText = 'α: ' + RadiansPrintHMS(radec[0]);
            }

            if (fitsData.CTYPE1.indexOf("GLON") > -1)
                raText = 'l: ' + RadiansPrintDMS(radec[0]);

            if (fitsData.CTYPE1.indexOf("ELON") > -1)
                raText = 'λ: ' + RadiansPrintDMS(radec[0]);

            if (fitsData.CTYPE2.indexOf("DEC") > -1)
                decText = 'δ: ' + RadiansPrintDMS(radec[1]);

            if (fitsData.CTYPE2.indexOf("GLAT") > -1)
                decText = 'b: ' + RadiansPrintDMS(radec[1]);

            if (fitsData.CTYPE2.indexOf("ELAT") > -1)
                decText = 'β: ' + RadiansPrintDMS(radec[1]);

            d3.select("#ra").text(raText);
            d3.select("#dec").text(decText);
        });

    zoom.scaleTo(rect, zoom_scale);
}

function show_cursor() {
    if (va_count > 1)
        return;

    d3.select("#image_rectangle")
        .style('cursor', 'wait');
}

function hide_cursor() {
    if (va_count > 1)
        return;

    d3.select("#image_rectangle")
        .style('cursor', 'none');
}

function setup_image_selection() {
    //delete previous instances
    try {
        d3.select("#region").remove();
        d3.select("#zoom").remove();
        d3.select("#zoomCross").remove();
        d3.select("#image_rectangle").remove();
    }
    catch (e) { };

    var svg = d3.select("#FrontSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;
    var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);
    var img_width = Math.floor(scale * image_bounding_dims.width);
    var img_height = Math.floor(scale * image_bounding_dims.height);

    let fillColour = 'white';

    if (theme == 'bright')
        fillColour = 'black';

    if (colourmap == "greyscale" || colourmap == "negative")
        fillColour = "#C4A000";

    //svg image rectangle for zooming-in
    var svg2 = svg.append("svg")
        .attr("id", "hds_svg")
        .attr("x", Math.round((width - img_width) / 2))
        .attr("y", Math.round((height - img_height) / 2))
        .attr("width", Math.round(img_width))
        .attr("height", Math.round(img_height))
        .attr("opacity", 0.0);

    //sub-region selection rectangle
    svg.append("rect")
        .attr("id", "region")
        .attr("x", 0)
        .attr("y", 0)
        .attr("width", 0)
        .attr("height", 0)
        .attr("fill", "none")
        .style("stroke", fillColour)
        .style("stroke-dasharray", ("1, 5, 1"))
        .style("stroke-width", emStrokeWidth)
        .attr("opacity", 0.0);

    //pv line selection
    svg.append("line")
        .attr("id", "pvline")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", 0)
        .attr("y2", 0)
        .attr("marker-start", "url(#head)")
        .attr("marker-end", "url(#head)")
        .style("stroke", fillColour)
        .style("stroke-dasharray", ("1, 5, 1"))
        .style("stroke-width", emStrokeWidth)
        .attr("opacity", 0.0);

    svg.append("line")
        .attr("id", "pvmid")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", 0)
        .attr("y2", 0)
        .style("stroke", fillColour)
        .style("stroke-width", emStrokeWidth)
        .attr("opacity", 0.0);

    // Subaru HDS X-Y spectrum cross-hair
    svg2.append("line")
        .attr("id", "xline")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", 0)
        .attr("y2", 0)
        .attr("marker-start", "url(#head)")
        .attr("marker-end", "url(#head)")
        .style("stroke", fillColour)
        .style("stroke-width", emStrokeWidth)
        .attr("opacity", 0.0);

    svg2.append("line")
        .attr("id", "yline")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", 0)
        .attr("y2", 0)
        .attr("marker-start", "url(#head)")
        .attr("marker-end", "url(#head)")
        .style("stroke", fillColour)
        .style("stroke-width", emStrokeWidth)
        .attr("opacity", 0.0);

    if (zoom_shape == "square") {
        //zoom selection rectangle
        svg.append("rect")
            .attr("id", "zoom")
            .attr("x", 0)
            .attr("y", 0)
            .attr("width", 0)
            .attr("height", 0)
            .attr("fill", "none")
            .attr("pointer-events", "none")
            .style("stroke", fillColour)
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", 3 * emStrokeWidth)
            .attr("opacity", 0.0);
    };

    if (zoom_shape == "circle") {
        //zoom selection circle
        svg.append("circle")
            .attr("id", "zoom")
            .attr("cx", 0)
            .attr("cy", 0)
            .attr("r", 0)
            .attr("fill", "none")
            .attr("pointer-events", "none")
            .style("stroke", fillColour)
            //.style("stroke-dasharray", ("1, 5, 1"))
            .style("stroke-width", 3 * emStrokeWidth)
            .attr("opacity", 0.0);
    };

    var crossSize = 1.0 * emFontSize;

    //zoom cross-hair
    svg.append("svg:image")
        .attr("id", "zoomCross")
        .attr("x", 0)
        .attr("y", 0)
        //.attr("xlink:href", ROOT_PATH + "plainicon.com-crosshair_white.svg")
        .attr("xlink:href", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/plainicon.com-crosshair_white.svg")
        .attr("width", crossSize)
        .attr("height", crossSize)
        .attr("opacity", 0.0);

    var zoom_element = d3.select("#zoom");
    var zoom_cross = d3.select("#zoomCross");

    var zoom = d3.zoom()
        .scaleExtent([5, 200])//was [10, 200]
        .on("zoom", zoomed);

    now = performance.now();
    then = now;

    spec_now = performance.now();
    spec_then = spec_now;

    //set up the spectrum rendering loop
    function update_spectrum() {
        spec_now = performance.now();
        spec_elapsed = spec_now - spec_then;

        //if (spec_elapsed > fpsInterval)
        {
            spec_then = spec_now - (spec_elapsed % fpsInterval);
            // console.log("spectrum interval: " + spec_elapsed.toFixed(3) + " [ms]", "fps = ", Math.round(1000 / spec_elapsed));

            //spectrum
            try {
                let go_ahead = true;
                let new_seq_id = 0;

                for (let index = 0; index < va_count; index++) {
                    let len = spectrum_stack[index].length;

                    if (len > 0) {
                        let id = spectrum_stack[index][len - 1].id;

                        if (id <= last_seq_id)
                            go_ahead = false;
                        else
                            new_seq_id = Math.max(new_seq_id, id);
                    }
                    else
                        go_ahead = false;
                }

                if (go_ahead) {
                    last_seq_id = new_seq_id;
                    //console.log("last_seq_id:", last_seq_id);

                    let fitsData = fitsContainer[va_count - 1];

                    if (!fitsData.is_spectrum) {
                        // pop all <va_count> spectrum stacks
                        var data = [];

                        for (let index = 0; index < va_count; index++) {
                            data.push(spectrum_stack[index].pop().spectrum);
                            spectrum_stack[index] = [];
                        }

                        plot_spectrum(data);
                        replot_y_axis();

                        last_spectrum = data;
                    } else {
                        // pop X-Y HDS spectra
                        let data = spectrum_stack[va_count - 1].pop();

                        var canvas = document.getElementById("SpectrumCanvas");
                        var ctx = canvas.getContext('2d');
                        var width = canvas.width;
                        var height = canvas.height;
                        ctx.clearRect(0, 0, width, height);

                        // update the cross-hair
                        var bounds = plot_hds_crosshair(data.x0, data.y0, data.angle);

                        // X direction
                        plot_hds_spectrum(data.xspectrum, data.xmask, bounds.xmin, bounds.xmax, 0);

                        // Y direction
                        plot_hds_spectrum(data.yspectrum, data.ymask, bounds.ymin, bounds.ymax, 1);
                    }
                }

            }
            catch (e) {
                console.log(e);
            }

        }

        if (!windowLeft)
            requestAnimationFrame(update_spectrum);
    }

    // a fix for Safari
    d3.select(document.body)
        .on('wheel.body', e => { });

    //svg image rectangle for zooming-in
    var rect = svg.append("rect")
        .attr("id", "image_rectangle")
        .attr("x", Math.round((width - img_width) / 2))
        .attr("y", Math.round((height - img_height) / 2))
        .attr("width", Math.round(img_width))
        .attr("height", Math.round(img_height))
        /*.attr("x", (width - img_width) / 2)
        .attr("y", (height - img_height) / 2)
        .attr("width", img_width)
        .attr("height", img_height)*/
        .style('cursor', 'none')//'crosshair')//'none' to mask Chrome latency
        /*.style('cursor', 'crosshair')//'crosshair')*/
        /*.style("fill", "transparent")
          .style("stroke", "yellow")
          .style("stroke-width", emStrokeWidth)
          .style("stroke-dasharray", ("1, 5, 1"))*/
        .attr("opacity", 0.0)
        .call(d3.drag()
            .on("start", fits_subregion_start)
            .on("drag", fits_subregion_drag)
            .on("end", fits_subregion_end)
        )
        .call(zoom)
        .on("mouseenter", (event) => {
            hide_navigation_bar();

            // cancel the image animation loop
            if (va_count == 1) {
                clear_webgl_image_buffers(va_count);
            } else {
                if (composite_view) {
                    clear_webgl_composite_image_buffers();
                }
            }

            try {
                zoom_beam();
            }
            catch (e) {
                console.log('NON-CRITICAL:', e);
            }

            if (d3.select("#pvline").attr("opacity") < 1.0) {
                // commented out on 2023/10/20
                // these two lines were interfering with the ds9 region import
                // anyway, the "mousemove" event will take care of the cross-hair
                /*zoom_element.attr("opacity", 1.0);
                zoom_cross.attr("opacity", 0.75);*/
            }

            d3.select("#pixel").text("").attr("opacity", 0.0);

            document.addEventListener('copy', copy_coordinates);
            shortcut.add("s", function () {
                set_autoscale_range(false);
            });
            shortcut.add("Meta+C", copy_coordinates);

            windowLeft = false;

            spectrum_stack = new Array(va_count);
            for (let i = 0; i < va_count; i++)
                spectrum_stack[i] = [];

            image_stack = [];
            viewport_zoom_settings = null;
            prev_mouse_position = { x: -1, y: -1 };

            requestAnimationFrame(update_spectrum);

            var offset;

            try {
                offset = d3.pointer(event);
            }
            catch (e) {
                console.log(e);
                return;
            }

            mouse_position = { x: offset[0], y: offset[1] };

            if (!initKalmanFilter)
                initKalman();

            resetKalman();

            if (va_count == 1) {
                init_webgl_zoom_buffers();
            } else {
                if (composite_view) {
                    init_webgl_composite_zoom_buffers();
                }
            }

            // send a "Kalman Filter reset" WebSocket message in order to reset the server-side Kalman Filter
            var msg = {
                type: "kalman_reset",
                seq_id: ++sent_seq_id
            };

            for (let index = 0; index < va_count; index++) {
                if (wsConn[index].readyState == 1)
                    wsConn[index].send(JSON.stringify(msg));
            }

            setup_window_timeout();
        })
        .on("mouseleave", (event) => {
            clearTimeout(idleMouse);

            // send a "Kalman Filter reset" WebSocket message in order to reset the server-side Kalman Filter
            var msg = {
                type: "kalman_reset",
                seq_id: ++sent_seq_id
            };

            for (let index = 0; index < va_count; index++) {
                if (wsConn[index].readyState == 1)
                    wsConn[index].send(JSON.stringify(msg));
            }

            setup_window_timeout();

            // cancel the P-V line & re-set the mouse click flag
            d3.select("#pvline")
                .attr("x1", 0)
                .attr("y1", 0)
                .attr("x2", 0)
                .attr("y2", 0)
                .style("stroke-dasharray", ("1, 5, 1"))
                .attr("opacity", 0.0);

            d3.select("#pvmid")
                .attr("x1", 0)
                .attr("y1", 0)
                .attr("x2", 0)
                .attr("y2", 0)
                .attr("opacity", 0.0);

            mouse_click_end = true;

            // cancel the X-Y HDS spectrum cross-hair
            d3.select("#hds_svg").attr("opacity", 0);

            d3.select("#xline")
                .attr("x1", 0)
                .attr("y1", 0)
                .attr("x2", 0)
                .attr("y2", 0)
                .attr("opacity", 0.0);

            d3.select("#yline")
                .attr("x1", 0)
                .attr("y1", 0)
                .attr("x2", 0)
                .attr("y2", 0)
                .attr("opacity", 0.0);

            // clear the ViewportCanvas in WebGL
            if (viewport != null) {
                // Clear the Viewport Canvas
                //console.log("clearing the Viewport Canvas");
                var gl = viewport.gl;

                if (gl !== undefined && gl != null) {
                    gl.clearColor(0, 0, 0, 0);
                    gl.clear(gl.COLOR_BUFFER_BIT);
                };

                clear_webgl_zoom_buffers();
            }

            // Clear the ZOOMCanvas
            clear_webgl_viewport();

            if (!event.shiftKey)
                windowLeft = true;

            spectrum_stack = new Array(va_count);
            for (let i = 0; i < va_count; i++)
                spectrum_stack[i] = [];

            image_stack = [];

            if (!event.shiftKey) {
                viewport_zoom_settings = null;
                zoom_element.attr("opacity", 0.0);
                zoom_cross.attr("opacity", 0.0);
            };

            d3.select("#" + zoom_location).style("stroke", "transparent");
            d3.select("#" + zoom_location + "Cross").attr("opacity", 0.0);
            d3.select("#" + zoom_location + "Beam").attr("opacity", 0.0);
            d3.select("#pixel").text("").attr("opacity", 0.0);

            document.removeEventListener('copy', copy_coordinates);
            shortcut.remove("Meta+C");
            shortcut.remove("s");

            if (event.shiftKey)
                return;

            setup_csv_export();

            if (xradec != null && d3.select("#pvline").attr("opacity") < 1.0) {
                let fitsData = fitsContainer[va_count - 1];

                let raText = 'RA N/A';
                let decText = 'DEC N/A';

                // check if fitsData.RA is not empty
                if (fitsData.RA != '') {
                    raText = 'RA: ' + fitsData.RA;
                }

                // check if fitsData.DEC is not empty
                if (fitsData.DEC != '') {
                    decText = 'DEC: ' + fitsData.DEC;
                }

                if (fitsData.CTYPE1.indexOf("RA") > -1) {
                    if (coordsFmt == 'DMS')
                        raText = 'α: ' + RadiansPrintDMS(xradec[0]);
                    else
                        raText = 'α: ' + RadiansPrintHMS(xradec[0]);
                }

                if (fitsData.CTYPE1.indexOf("GLON") > -1)
                    raText = 'l: ' + RadiansPrintDMS(xradec[0]);

                if (fitsData.CTYPE1.indexOf("ELON") > -1)
                    raText = 'λ: ' + RadiansPrintDMS(xradec[0]);

                if (fitsData.CTYPE2.indexOf("DEC") > -1)
                    decText = 'δ: ' + RadiansPrintDMS(xradec[1]);

                if (fitsData.CTYPE2.indexOf("GLAT") > -1)
                    decText = 'b: ' + RadiansPrintDMS(xradec[1]);

                if (fitsData.CTYPE2.indexOf("ELAT") > -1)
                    decText = 'β: ' + RadiansPrintDMS(xradec[1]);

                d3.select("#ra").text(raText);
                d3.select("#dec").text(decText);
            }

            if (mousedown)
                return;

            let fitsData = fitsContainer[va_count - 1];

            if (fitsData != null) {
                // restore the main image spectrum
                if (fitsData.depth > 1) {
                    if (va_count == 1) {
                        if (intensity_mode == "mean") {
                            plot_spectrum([fitsData.mean_spectrum]);
                            replot_y_axis();
                        }

                        if (intensity_mode == "integrated") {
                            plot_spectrum([fitsData.integrated_spectrum]);
                            replot_y_axis();
                        }
                    }
                    else {
                        if (intensity_mode == "mean") {
                            plot_spectrum(mean_spectrumContainer);
                            replot_y_axis();
                        }

                        if (intensity_mode == "integrated") {
                            plot_spectrum(integrated_spectrumContainer);
                            replot_y_axis();
                        }
                    }
                }

                // clear the HDS X-Y spectra
                if (fitsData.is_spectrum) {
                    let _canvas = document.getElementById("SpectrumCanvas");
                    let _ctx = _canvas.getContext('2d');
                    let _width = _canvas.width;
                    let _height = _canvas.height;
                    _ctx.clearRect(0, 0, _width, _height);
                }
            }

            if (va_count == 1) {
                clear_webgl_image_buffers(va_count);
                init_webgl_image_buffers(va_count);
            } else {
                if (composite_view) {
                    clear_webgl_composite_image_buffers();
                    init_webgl_composite_image_buffers();
                }
            }
        })
        .on("mousemove", (event) => {
            // cancel the image animation loop
            if (va_count == 1) {
                clear_webgl_image_buffers(va_count);
            } else {
                if (composite_view) {
                    clear_webgl_composite_image_buffers();
                }
            }

            if (!autoscale && event.shiftKey) {
                d3.select("#scaling")
                    .style('cursor', 'ns-resize')
                    .attr("opacity", 0.5);

                let fillColour = 'white';

                if (theme == 'bright')
                    fillColour = 'black';

                d3.select("#yaxis")
                    .style("fill", fillColour)
                    .style("stroke", fillColour);
            }
            else {
                d3.select("#scaling")
                    .style('cursor', '')
                    .attr("opacity", 0.0);

                d3.select("#yaxis")
                    .style("fill", axisColour)
                    .style("stroke", axisColour);
            }

            if (freqdrag || event.shiftKey) {
                var node = event.currentTarget;
                node.style.cursor = 'pointer';
                return;
            }

            try {
                // check the opacity attribute of the P-V line
                if (d3.select("#pvline").attr("opacity") > 0.0 && !mouse_click_end) {
                    // update the P-V line

                    let x1 = line_x;
                    let y1 = line_y;

                    let mpos = d3.pointer(event);
                    let x2 = mpos[0]; let y2 = mpos[1];

                    d3.select("#pvline")
                        .style("stroke-dasharray", ("1, 5, 1"))
                        .attr("x1", x1)
                        .attr("y1", y1)
                        .attr("x2", x2)
                        .attr("y2", y2);

                    let dx = x2 - x1;
                    let dy = y2 - y1;

                    if (Math.abs(dy) > 0) {
                        let _m = - dx / dy; // a perpendicular line to the P-V line
                        let _s = emFontSize / Math.sqrt(1 + _m * _m) / golden_ratio;

                        let _mx = _s;
                        let _my = _m * _s;

                        let _x = (x1 + x2) / 2; // midpoint of the P-V line
                        let _y = (y1 + y2) / 2; // midpoint of the P-V line

                        let _x1 = _x - _mx;
                        let _y1 = _y - _my;
                        let _x2 = _x + _mx;
                        let _y2 = _y + _my;

                        d3.select("#pvmid").attr("x1", _x1).attr("y1", _y1).attr("x2", _x2).attr("y2", _y2);
                    }

                    // return; // disabled to test zoom-in for the P-V line end point
                }

                if (d3.select("#pvline").attr("opacity") > 0.0 && mouse_click_end) {
                    // reset and hide the P-V line
                    d3.select("#pvline")
                        .attr("x1", 0)
                        .attr("y1", 0)
                        .attr("x2", 0)
                        .attr("y2", 0)
                        .attr("opacity", 0.0);

                    d3.select("#pvmid").attr("opacity", 0.0);
                }
            } catch (_) { }

            // commented out so that the caching 'wait' cursor remains visible
            //d3.select(this).style('cursor', 'none');

            event.preventDefault = true;
            if (!has_image) return;

            let fitsData = fitsContainer[va_count - 1];

            if (fitsData == null)
                return;

            var elem = document.getElementById("SpectrumCanvas");
            elem.getContext('2d').globalAlpha = 1.0;
            var width = elem.width;
            var height = elem.height;

            moving = true;
            clearTimeout(idleMouse);
            windowLeft = false;

            d3.select("#" + zoom_location).style("stroke", "Gray");
            d3.select("#" + zoom_location + "Cross").attr("opacity", 0.75);
            d3.select("#" + zoom_location + "Beam").attr("opacity", 0.75);

            try {
                var offset = d3.pointer(event);

                // there seems to be a bug in d3.js !? offset coordinates go negative !?
                if ((offset[0] < 0) || (offset[1] < 0)) {
                    offset[0] = mouse_position.x;
                    offset[1] = mouse_position.y;
                }
            }
            catch (e) {
                // return if for example <mouse_position> is undefined
                // hide the beam (just in case it gets displayed)
                d3.select("#" + zoom_location + "Beam").attr("opacity", 0.0);
                return;
            }

            if (isNaN(offset[0]) || isNaN(offset[1]))
                return;

            if ((offset[0] >= 0) && (offset[1] >= 0)) {
                mouse_position = { x: offset[0], y: offset[1] };
            };

            // console.log("mouse position:", mouse_position);

            var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;
            var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);

            var clipSize = Math.min(image_bounding_dims.width - 1, image_bounding_dims.height - 1) / zoom_scale;
            var sel_width = Math.floor(clipSize * scale);
            var sel_height = Math.floor(clipSize * scale);

            if (!mousedown) {
                let mx = mouse_position.x;
                let my = mouse_position.y;

                if (zoom_shape == "square")
                    zoom_element.attr("x", mx - sel_width).attr("y", my - sel_height).attr("width", 2 * sel_width).attr("height", 2 * sel_height).attr("opacity", 1.0);

                if (zoom_shape == "circle")
                    zoom_element.attr("cx", mx).attr("cy", my).attr("r", Math.round(sel_width)).attr("opacity", 1.0);

                var crossSize = 1.0 * emFontSize;
                zoom_cross.attr("x", mx - crossSize / 2).attr("y", my - crossSize / 2).attr("width", crossSize).attr("height", crossSize).attr("opacity", 0.75);
            }

            let rect = event.currentTarget;

            var ax = (image_bounding_dims.width - 1) / (rect.getAttribute("width") - 0);
            var x = image_bounding_dims.x1 + ax * (mouse_position.x - rect.getAttribute("x"));

            var ay = (image_bounding_dims.height - 1) / (rect.getAttribute("height") - 0);
            var y = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (mouse_position.y - rect.getAttribute("y"));

            x = clamp(x, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
            y = clamp(y, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

            var orig_x = x * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
            var orig_y = y * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);
            // console.log("scale:", scale, "ax:", ax, "ay:", ay, "x:", x, "y:", y, "orig_x:", orig_x, "orig_y:", orig_y);

            let world = pix2sky(fitsData, orig_x, orig_y);

            // if either world value is NaN throw an error
            if (isNaN(world[0]) || isNaN(world[1]))
                throw new Error("NaN WCS");

            let radec = [world[0] / toDegrees, world[1] / toDegrees];

            let raText = 'RA N/A';
            let decText = 'DEC N/A';

            if (fitsData.CTYPE1.indexOf("RA") > -1) {
                if (coordsFmt == 'DMS')
                    raText = 'α: ' + RadiansPrintDMS(radec[0]);
                else
                    raText = 'α: ' + RadiansPrintHMS(radec[0]);
            }

            if (fitsData.CTYPE1.indexOf("GLON") > -1)
                raText = 'l: ' + RadiansPrintDMS(radec[0]);

            if (fitsData.CTYPE1.indexOf("ELON") > -1)
                raText = 'λ: ' + RadiansPrintDMS(radec[0]);

            if (fitsData.CTYPE2.indexOf("DEC") > -1)
                decText = 'δ: ' + RadiansPrintDMS(radec[1]);

            if (fitsData.CTYPE2.indexOf("GLAT") > -1)
                decText = 'b: ' + RadiansPrintDMS(radec[1]);

            if (fitsData.CTYPE2.indexOf("ELAT") > -1)
                decText = 'β: ' + RadiansPrintDMS(radec[1]);

            if (spectrum_view) {
                raText = 'X: ' + clamp(Math.round(1 + orig_x), 1, fitsData.width);
                decText = 'Y: ' + clamp(Math.round(1 + orig_y), 1, fitsData.height);
            }

            d3.select("#ra").text(raText);
            d3.select("#dec").text(decText);

            //for each image
            var pixelText = '';
            var displayPixel = true;
            var PR = ["R:", "G:", "B:"];
            for (let index = 1; index <= va_count; index++) {
                var imageFrame = imageContainer[index - 1];

                var pixel_coord = Math.round(y) * imageFrame.width + Math.round(x);

                var pixel = imageFrame.pixels[pixel_coord];
                var alpha = imageFrame.alpha[pixel_coord];

                let bunit = fitsData.BUNIT.trim();
                if (fitsData.depth > 1 && has_velocity_info)
                    bunit += '•km/s';

                if (alpha > 0 && !isNaN(pixel)) {
                    //d3.select("#pixel").text(prefix + pixelVal.toPrecision(3) + " " + bunit).attr("opacity", 1.0) ;
                    if (va_count > 1)
                        pixelText += PR[index - 1 % PR.length];
                    pixelText += pixel.toPrecision(3) + " ";
                    displayPixel = displayPixel && true;
                }
                else {
                    //d3.select("#pixel").text("").attr("opacity", 0.0) ;
                    displayPixel = displayPixel && false;
                }

                if (index == va_count && displayPixel) {
                    pixelText += bunit;
                    d3.select("#pixel").text(pixelText).attr("opacity", 1.0);
                }
                else
                    d3.select("#pixel").text("").attr("opacity", 0.0);
            }

            //viewport collision detection
            {
                var collision_detected = false;

                if (zoom_shape == "square") {
                    let w1 = parseFloat(zoom_element.attr("width"));
                    let h1 = parseFloat(zoom_element.attr("height"));

                    let tmp = d3.select("#" + zoom_location);
                    let x2 = parseFloat(tmp.attr("x"));
                    let y2 = parseFloat(tmp.attr("y"));
                    let w2 = parseFloat(tmp.attr("width"));
                    let h2 = parseFloat(tmp.attr("height"));

                    if (zoom_location == "upper")
                        if (((offset[0] - w1 / 2) < (x2 + w2)) && (offset[1] - h1 / 2) < (y2 + h2)) {
                            collision_detected = true;
                        }

                    if (zoom_location == "lower")
                        if (((offset[0] + w1 / 2) > x2) && (offset[1] + h1 / 2) > y2)
                            collision_detected = true;
                }

                if (zoom_shape == "circle") {
                    let r1 = parseFloat(zoom_element.attr("r"));

                    let tmp = d3.select("#" + zoom_location);

                    let _x = parseFloat(tmp.attr("cx"));
                    let _y = parseFloat(tmp.attr("cy"));
                    let r2 = parseFloat(tmp.attr("r"));

                    let dx = offset[0] - _x;
                    let dy = offset[1] - _y;
                    let rSq = dx * dx + dy * dy;

                    if (rSq < (r1 + r2) * (r1 + r2))
                        collision_detected = true;
                }

                if (collision_detected/* && zoom_scale > 10*/) {
                    //ctx.clearRect(0, 0, c.width, c.height);
                    swap_viewports();
                }
            }

            // update image updates
            if (!mousedown || d3.select("#pvline").attr("opacity") == 1.0) {
                var px, py;

                var zoomed_size = Math.round(get_zoomed_size(width, height, img_width, img_height));

                if (zoom_location == "upper") {
                    px = emStrokeWidth;
                    py = emStrokeWidth;
                }
                else {
                    px = width - 1 - emStrokeWidth - zoomed_size;
                    py = height - 1 - emStrokeWidth - zoomed_size;
                }

                zoomed_size = Math.round(zoomed_size);
                px = Math.round(px);
                py = Math.round(py);

                //image_stack.push({ x: x, y: y, clipSize: clipSize, px: px, py: py, zoomed_size: zoomed_size });
                viewport_zoom_settings = { x: x, y: y, clipSize: clipSize, px: px, py: py, zoomed_size: zoomed_size };

                if ((mouse_position.x != prev_mouse_position.x) || (mouse_position.y != prev_mouse_position.y)) {
                    prev_mouse_position = mouse_position;
                    viewport.refresh = true;
                }
            }

            now = performance.now();
            elapsed = performance.now() - then;

            // predict future mouse positions, send spectrum update requests
            if (elapsed > fpsInterval + computed + processed && (!mousedown || d3.select("#pvline").attr("opacity") == 1.0))//+ latency, computed, processed
            {
                then = now - (elapsed % fpsInterval);
                //ALMAWS.send('[mouse] t=' + now + ' x=' + offset[0] + ' y=' + offset[1]);

                //console.log("refresh interval: " + elapsed.toFixed(3) + " [ms]", "fps = ", Math.round(1000 / elapsed));

                if (!initKalmanFilter)
                    initKalman();

                updateKalman();

                var pred_mouse_x = Math.round(mouse_position.x + last_x.elements[2] * latency);
                var pred_mouse_y = Math.round(mouse_position.y + last_x.elements[3] * latency);
                //var pred_mouse_x = Math.round(mouse_position.x + last_x.elements[0] * latency + 0.5 * last_x.elements[2] * latency * latency) ;
                //var pred_mouse_y = Math.round(mouse_position.y + last_x.elements[1] * latency + 0.5 * last_x.elements[3] * latency * latency) ;

                //console.log("latency = ", latency.toFixed(1), "[ms]", "mx = ", mouse_position.x, "px = ", pred_mouse_x, "my = ", mouse_position.y, "py = ", pred_mouse_y);
                /*var pred_x = image_bounding_dims.x1 + (pred_mouse_x - d3.select(this).attr("x")) / (d3.select(this).attr("width") - 1) * (image_bounding_dims.width - 1);
                var pred_y = image_bounding_dims.y2 + (pred_mouse_y - d3.select(this).attr("y")) / (d3.select(this).attr("height") - 1) * (image_bounding_dims.height - 1);*/

                let rect = event.currentTarget;

                var ax = (image_bounding_dims.width - 1) / (rect.getAttribute("width") - 0);
                var pred_x = image_bounding_dims.x1 + ax * (pred_mouse_x - rect.getAttribute("x"));

                var ay = (image_bounding_dims.height - 1) / (rect.getAttribute("height") - 0);
                var pred_y = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (pred_mouse_y - rect.getAttribute("y"));

                pred_x = clamp(pred_x, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
                pred_y = clamp(pred_y, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

                var fitsX = Math.round(pred_x * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1));//x or pred_x
                var fitsY = Math.round(pred_y * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1));//y or pred_y
                var fitsSize = clipSize * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);

                //console.log('active', 'x = ', x, 'y = ', y, 'clipSize = ', clipSize, 'fitsX = ', fitsX, 'fitsY = ', fitsY, 'fitsSize = ', fitsSize) ;
                //let strLog = 'active x = ' + x + ' y = '+ y + ' clipSize = ' + clipSize + ' fitsX = ' + fitsX + ' fitsY = ' + fitsY + ' fitsSize = ' + fitsSize + ' pred_x = ' + pred_x + ' pred_y = ' + pred_y + ' pred_mouse_x = ' + pred_mouse_x + ' pred_mouse_y = ' + pred_mouse_y ;

                //send a spectrum request to the server
                var x1 = Math.round(fitsX - fitsSize);
                var y1 = Math.round(fitsY - fitsSize);
                var x2 = Math.round(fitsX + fitsSize);
                var y2 = Math.round(fitsY + fitsSize);

                if (realtime_spectrum /*&& fitsData.depth > 1 && !optical_view*/) {
                    sent_seq_id++;
                    var range = get_axes_range(width, height);
                    var dx = range.xMax - range.xMin;
                    var dy = range.yMax - range.yMin;

                    for (let index = 0; index < va_count; index++) {
                        // a real-time websocket request

                        if (viewport_zoom_settings != null) {
                            let _width = viewport_zoom_settings.zoomed_size;
                            let _height = viewport_zoom_settings.zoomed_size;

                            var request;

                            if (fitsData.is_spectrum) {
                                request = {
                                    type: "realtime_image_spectrum",
                                    dx: dx,
                                    dy: dy,
                                    image: false,
                                    quality: image_quality,
                                    x1: x1 + 1,
                                    y1: y1 + 1,
                                    x2: x2 + 1,
                                    y2: y2 + 1,
                                    width: _width,
                                    height: _height,
                                    beam: zoom_shape,
                                    x: fitsX + 1,
                                    y: fitsY + 1,
                                    tracking: peak_tracking,
                                    seq_id: sent_seq_id,
                                    timestamp: performance.now()
                                };
                            } else {
                                request = {
                                    type: "realtime_image_spectrum",
                                    dx: dx,
                                    image: false,
                                    quality: image_quality,
                                    x1: x1 + 1,
                                    y1: y1 + 1,
                                    x2: x2 + 1,
                                    y2: y2 + 1,
                                    width: _width,
                                    height: _height,
                                    beam: zoom_shape,
                                    intensity: intensity_mode,
                                    frame_start: data_band_lo,
                                    frame_end: data_band_hi,
                                    ref_freq: RESTFRQ,
                                    seq_id: sent_seq_id,
                                    timestamp: performance.now()
                                };
                            }

                            if (wsConn[index].readyState == 1)
                                wsConn[index].send(JSON.stringify(request));
                        }
                    }
                }

                setup_window_timeout();
            }

            idleMouse = setTimeout(imageTimeout, 250);//was 250ms + latency
        });

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData != null) {
        // the P-V Diagram has been disabled in the composite mode
        if ((fitsData.depth > 1) && (va_count == 1)) {
            rect.on("click", pv_event);
        }
    }

    zoom.scaleTo(rect, zoom_scale);
}

function stripHTML(html) {
    try {
        return $("<p>" + html + "</p>").text(); // jQuery does the heavy lifting
    } catch (_) {
        return html;
    }
}

function screen_molecule(molecule, search) {
    if (search != '') {
        if (molecule.text.indexOf(search) == -1)
            return false;
    }

    var intensity = parseFloat(molecule.cdms);

    if (intensity < displayIntensity)
        return false;

    if (molecule.linelist == "CDMS")
        return displayCDMS;

    if (molecule.linelist == "JPL")
        return displayJPL;

    if (molecule.linelist == "Recomb")
        return displayRecomb;

    if (molecule.linelist == "SLAIM")
        return displaySLAIM;

    if (molecule.linelist == "TopModel")
        return displayTopModel;

    if (molecule.linelist == "OSU")
        return displayOSU;

    if (molecule.linelist == "Lovas")
        return displayLovas;

    if (molecule.linelist == "ToyaMA")
        return displayToyaMA;

    return true;
}

function index_molecules() {
    if (molecules.length <= 0)
        return;

    for (var i = 0; i < molecules.length; i++) {
        var molecule = molecules[i];

        // strip any HTML from the name and species (like <sup>, etc.)
        let name = stripHTML(molecule.name.toLowerCase()).trim();
        let species = stripHTML(molecule.species.toLowerCase()).trim();

        molecule.text = name + " " + species;
    }
}

function display_molecules() {
    if (molecules.length <= 0)
        return;

    if (data_band_lo <= 0 || data_band_hi <= 0)
        return;

    var band_lo = data_band_lo;//[Hz]
    var band_hi = data_band_hi;//[Hz]

    // get the search term (if any)
    var searchTerm = stripHTML(document.getElementById('searchInput').value.toLowerCase()).trim();

    var checkbox = document.getElementById('restcheckbox');

    if (checkbox.checked) {
        band_lo = relativistic_rest_frequency(band_lo);
        band_hi = relativistic_rest_frequency(band_hi);
    };

    var svg = d3.select("#BackSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));
    var range = get_axes_range(width, height);

    try {
        d3.select("#molecules").remove();
    }
    catch (e) {
    }

    var group = svg.append("g")
        .attr("id", "molecules")
        .attr("opacity", 0.0);

    // filter the molecules
    var mol_list = [];
    for (var i = 0; i < molecules.length; i++) {
        let molecule = molecules[i];

        if (!screen_molecule(molecule, searchTerm))
            continue;

        let f = molecule.frequency * 1e9;

        if ((f >= band_lo) && (f <= band_hi))
            mol_list.push(molecule);
    };

    var num = mol_list.length;

    if (num > displayLimit) {
        console.log("Too many spectral lines to display:", num, ", applying a hard limit of", displayLimit, ". Please refine your search.");

        // randomly select a subset of the molecules
        mol_list = mol_list.sort(() => Math.random() - Math.random()).slice(0, displayLimit);
        num = mol_list.length;
    }

    var fontStyle = Math.round(0.67 * emFontSize) + "px";// Helvetica";
    var strokeStyle = "#FFCC00";

    if (theme == 'bright')
        strokeStyle = 'black';

    /*if(colourmap == "rainbow" || colourmap == "hot")
    strokeStyle = "white";*/

    //and adjust (reduce) the font size if there are too many molecules to display
    if (num > 20)
        fontStyle = Math.max(8, Math.round(0.67 * emFontSize * .25)) + "px";// Helvetica";

    //console.log("valid molecules:", num);

    var dx = range.xMax - range.xMin;
    var offsety = height - 1;

    var div_molecules = d3.select("#molecularlist");
    div_molecules.selectAll("*").remove();

    for (var i = 0; i < mol_list.length; i++) {
        let molecule = mol_list[i];
        let f = molecule.frequency * 1e9;

        var x = range.xMin + dx * (f - band_lo) / (band_hi - band_lo);

        var moleculeG = group.append("g")
            .attr("id", "molecule_group")
            .attr("x", x);

        moleculeG.append("line")
            .attr("id", "molecule_line")
            .attr("x1", x)
            .attr("y1", offsety)
            .attr("x2", x)
            .attr("y2", offsety - 1.25 * emFontSize)
            .style("stroke", strokeStyle)
            .style("stroke-width", 1)
            .attr("opacity", 1.0);

        var text;

        if (molecule.species.indexOf("Unidentified") > -1)
            text = "";
        else
            text = molecule.species;

        moleculeG.append("foreignObject")
            .attr("x", (x - 0.5 * emFontSize))
            .attr("y", (offsety - 2.0 * emFontSize))
            .attr("width", (20 * emFontSize))
            .attr("height", (2 * emFontSize))
            .attr("transform", 'rotate(-45 ' + (x - 0.5 * emFontSize) + ',' + (offsety - 2.0 * emFontSize) + ')')
            .attr("opacity", 1.0)
            .append("xhtml:div")
            .style("font-size", fontStyle)
            .style("font-family", "Inconsolata")
            .style("color", strokeStyle)
            .style("display", "inline-block")
            //.append("p")
            .html(text.trim());

        //console.log("spectral line @ x = ",x, (f/1e9).toPrecision(7), text.trim()) ;

        try {
            var htmlStr = molecule.name.trim() + ' ' + text.trim() + ' ' + molecule.qn.trim() + ' <span style="font-size: 80%">(' + molecule.linelist + ')</span>';
        } catch (e) {
            console.log(molecule);
            console.error(e);
        }

        if (htmlStr.indexOf("Unidentified") > -1)
            htmlStr = molecule.name;

        div_molecules.append("p")
            .attr("class", "molecularp")
            .attr("freq", f)
            .attr("x", x)
            .html((f / 1e9).toPrecision(7) + ' GHz' + ' ' + htmlStr);
    }

    group.moveToBack();

    var elem = d3.select("#molecules");
    if (displayMolecules)
        elem.attr("opacity", 1);
    else
        elem.attr("opacity", 0);
}

async function fetch_atomic_spectra(wmin, wmax) {
    // wmin, wmax [Å]
    var url = 'get_atomic_spectra?wmin=' + wmin + '&wmax=' + wmax + '&' + encodeURIComponent(get_js_version());
    console.log("fetch_atomic_spectra:", url);

    let response = await fetch(url);

    var lines = [];

    if (response.ok) {
        await response.json().then(json => {
            lines = json.lines; // an array of atomic spectral lines
        }).catch(error => {
            console.error(error);
        });
    }

    return lines;
}

async function fetch_spectral_lines(datasetId, freq_start, freq_end) {
    var xmlhttp = new XMLHttpRequest();

    //freq_start, freq_end [Hz]
    var url = 'get_splatalogue?datasetId=' + encodeURIComponent(datasetId) + '&freq_start=' + freq_start + '&freq_end=' + freq_end + '&' + encodeURIComponent(get_js_version());
    console.log("fetch_spectral_lines:", url);

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 404) {
            console.log("No spectral lines found.");
            return;
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 502) {
            console.log("Connection error, re-fetching molecules after 1 second.");
            setTimeout(function () {
                fetch_spectral_lines(datasetId, freq_start, freq_end);
            }, 1000);
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 202) {
            console.log("Server not ready, long-polling molecules again after 250 ms.");
            setTimeout(function () {
                fetch_spectral_lines(datasetId, freq_start, freq_end);
            }, 250);
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            molecules = []; // a global variable

            try {
                var response = JSON.parse(xmlhttp.responseText);
                molecules = response.molecules;
            } catch (err) {
                var received_msg = xmlhttp.response;

                if (received_msg instanceof ArrayBuffer) {

                    try {
                        // bzip2 decoder
                        var bytes = new Uint8Array(received_msg);
                        jsonData = bzip2.simple(bzip2.array(bytes));

                        var response = JSON.parse(jsonData);
                        molecules = response.molecules;
                    } catch (e) {
                        console.log(e);
                    };
                };
            };

            index_molecules();
            // console.log("#SPLATALOGUE molecules: ", molecules.length);

            let fitsData = fitsContainer[va_count - 1];

            if (fitsData != null) {
                if (fitsData.depth > 1)
                    display_molecules();
            }
        }
    }

    xmlhttp.open("GET", url, true);

    sv = votable.getAttribute('data-server-version');

    if (sv.charAt(0) == 'J') {
        xmlhttp.responseType = 'arraybuffer';
    }

    xmlhttp.timeout = 0;
    xmlhttp.send();
};

async function fetch_image_spectrum(_datasetId, index, fetch_data, add_timestamp) {
    var rect = document.getElementById('mainDiv').getBoundingClientRect();
    var width = Math.round(rect.width - 20);
    var height = Math.round(rect.height - 20);

    var xmlhttp = new XMLHttpRequest();

    var url = 'image_spectrum?datasetId=' + encodeURIComponent(_datasetId) + '&width=' + width + '&height=' + height + '&quality=' + image_quality;

    if (fetch_data)
        url += '&fetch_data=true';

    url += '&' + encodeURIComponent(get_js_version());

    if (add_timestamp)
        url += '&timestamp=' + Date.now();

    xmlhttp.onreadystatechange = function () {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 404) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            hide_hourglass();
            show_not_found();
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 415) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            hide_hourglass();
            show_unsupported_media_type();
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 500) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            hide_hourglass();
            //show_critical_error();
            show_not_found();
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 500) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            console.log("Connection error:", xmlhttp.status, ", re-fetching image after 1 second.");
            setTimeout(function () {
                fetch_image_spectrum(_datasetId, index, fetch_data, true);
            }, 1000);
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 202) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            console.log("Server not ready, long-polling image again after 250ms.");
            setTimeout(function () {
                fetch_image_spectrum(_datasetId, index, fetch_data, false);
            }, 250);
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 204) {
            console.log("Server not ready / No Content, long-polling the server again after 100ms.");
            setTimeout(function () {
                // read the counter value from the URL location &counter=...
                var loc = window.location.href;
                var counter = parseInt(loc.split('&counter=')[1]);

                // if the counter is NAN set it to 1 else increment it
                if (isNaN(counter)) {
                    counter = 1;
                }
                else {
                    // delete the counter from the URL loc
                    loc = loc.split('&counter=')[0];
                    counter++;
                }

                // stop after 5 attempts
                if (counter > 5) {
                    hide_hourglass();
                    show_not_found();
                    return;
                }

                window.location.replace(loc + '&counter=' + counter);
                //window.location.reload();
            }, 100);

            if (dataset_timeout == -1) {
                dataset_timeout = setTimeout(function () {
                    hide_hourglass();
                    show_not_found();
                }, 10000);
            }
        }

        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            if (dataset_timeout != -1) {
                window.clearTimeout(dataset_timeout);
                dataset_timeout = -1;
            }

            setup_window_timeout();

            Promise.all([waitForModuleReady(), resGLSL]).then(async _ => {
                // wait for WebAssembly to get compiled
                Module.ready
                    .then(async _ => {
                        document.getElementById('welcome').style.display = "none";
                        //console.log('hiding the loading progress, style =', document.getElementById('welcome').style.display);

                        var received_msg = xmlhttp.response;

                        if (received_msg.byteLength == 0) {
                            hide_hourglass();
                            show_not_found();
                            return;
                        }

                        if (received_msg instanceof ArrayBuffer) {
                            var fitsHeader, mean_spectrum, integrated_spectrum;

                            var dv = new DataView(received_msg);
                            //console.log("FITSImage dataview byte length: ", dv.byteLength);

                            var tone_mapping = new Object();
                            let p = 0.5;
                            tone_mapping.lmin = Math.log(p);
                            tone_mapping.lmax = Math.log(p + 1.0);

                            var offset = 0;
                            var str_length = dv.getUint32(offset, endianness);
                            offset += 4;

                            let flux = new Uint8Array(received_msg, offset, str_length);
                            tone_mapping.flux = (new TextDecoder("utf-8").decode(flux)).trim();
                            offset += str_length;

                            tone_mapping.min = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.max = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.median = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.sensitivity = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.ratio_sensitivity = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.white = dv.getFloat32(offset, endianness);
                            offset += 4;

                            tone_mapping.black = dv.getFloat32(offset, endianness);
                            offset += 4;

                            //console.log(tone_mapping);

                            var img_width = dv.getUint32(offset, endianness);
                            offset += 4;

                            var img_height = dv.getUint32(offset, endianness);
                            offset += 4;

                            //console.log('img_width:', img_width, 'img_height:', img_height);

                            var pixels_length = dv.getUint32(offset, endianness);
                            offset += 4;

                            //console.log('pixels length:', pixels_length);

                            var frame_pixels = new Uint8Array(received_msg, offset, pixels_length);
                            offset += pixels_length;

                            var mask_length = dv.getUint32(offset, endianness);
                            offset += 4;

                            //console.log('mask length:', mask_length);

                            var frame_mask = new Uint8Array(received_msg, offset, mask_length);
                            offset += mask_length;

                            if (tone_mapping.flux == "legacy") {
                                tone_mapping.black = tone_mapping.min;
                                tone_mapping.white = tone_mapping.max;
                            }

                            //console.log(tone_mapping);

                            var has_json = true;

                            try {
                                var json_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var json = new Uint8Array(received_msg, offset, buffer_len);
                                offset += buffer_len;
                                //console.log("FITS json length:", json_len);
                            } catch (err) {
                                has_json = false;
                            }

                            var has_header = true;

                            try {
                                var header_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var header = new Uint8Array(received_msg, offset, buffer_len);
                                offset += buffer_len;
                                //console.log("FITS header length:", header_len);
                            } catch (err) {
                                has_header = false;
                            }

                            var mean_spectrum;
                            var has_mean_spectrum = true;

                            try {
                                var spectrum_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer = new Uint8Array(received_msg, offset, buffer_len);
                                offset += buffer_len;
                                //console.log("FITS mean spectrum length:", spectrum_len);

                                // ZFP decoder part
                                /*Module.ready
                                  .then(_ => {*/
                                let start = performance.now();
                                // mean_spectrum = Module.decompressZFPspectrum(spectrum_len, buffer).map((x) => x); // clone an array since there is only one underlying wasm memory buffer
                                var res = Module.decompressZFPspectrum(spectrum_len, buffer);
                                mean_spectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);
                                let elapsed = Math.round(performance.now() - start);

                                //console.log("vector size: ", vec.size(), "elapsed: ", elapsed, "[ms]");
                                /*})
                                .catch(e => {
                                  console.error(e);
                                  has_mean_spectrum = false;
                                });*/
                            } catch (err) {
                                has_mean_spectrum = false;
                            }

                            var integrated_spectrum;
                            var has_integrated_spectrum = true;

                            try {
                                var spectrum_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer_len = dv.getUint32(offset, endianness);
                                offset += 4;

                                var buffer = new Uint8Array(received_msg, offset, buffer_len);
                                offset += buffer_len;
                                //console.log("FITS integrated spectrum length:", spectrum_len);

                                // FPZIP decoder part
                                /*Module.ready
                                  .then(_ => {*/
                                let start = performance.now();
                                // integrated_spectrum = Module.decompressZFPspectrum(spectrum_len, buffer).map((x) => x); // clone an array since there is only one underlying wasm memory buffer
                                var res = Module.decompressZFPspectrum(spectrum_len, buffer);
                                integrated_spectrum = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);
                                let elapsed = Math.round(performance.now() - start);

                                //console.log("vector size: ", vec.size(), "elapsed: ", elapsed, "[ms]");
                                /*})
                                .catch(e => {
                                  console.error(e);
                                  has_integrated_spectrum = false;
                                });*/
                            } catch (err) {
                                has_integrated_spectrum = false;
                            }

                            if (has_header) {
                                // decompress the FITS data etc.
                                var LZ4 = require('lz4');

                                var uncompressed = new Uint8Array(header_len);
                                uncompressedSize = LZ4.decodeBlock(header, uncompressed);
                                uncompressed = uncompressed.slice(0, uncompressedSize);

                                try {
                                    fitsHeader = new TextDecoder().decode(uncompressed);
                                }
                                catch (err) {
                                    fitsHeader = '';
                                    for (var i = 0; i < uncompressed.length; i++)
                                        fitsHeader += String.fromCharCode(uncompressed[i]);
                                };

                                // console.log(fitsHeader);
                            }

                            if (has_json) {
                                // decompress the FITS data etc.
                                var LZ4 = require('lz4');

                                var uncompressed = new Uint8Array(json_len);
                                uncompressedSize = LZ4.decodeBlock(json, uncompressed);
                                uncompressed = uncompressed.slice(0, uncompressedSize);

                                var fitsData;

                                try {
                                    fitsData = new TextDecoder().decode(uncompressed);
                                }
                                catch (err) {
                                    fitsData = '';
                                    for (var i = 0; i < uncompressed.length; i++)
                                        fitsData += String.fromCharCode(uncompressed[i]);
                                };

                                //console.log(fitsData);
                                fitsData = JSON.parse(fitsData);

                                // replace the dummy FITS header
                                if (has_header) {
                                    fitsData.HEADER = fitsHeader;
                                } else {
                                    fitsData.HEADER = 'N/A';
                                };

                                // replace the dummy mean spectrum
                                if (has_mean_spectrum) {
                                    fitsData.mean_spectrum = mean_spectrum;
                                }

                                // replace the dummy integrated spectrum
                                if (has_integrated_spectrum) {
                                    fitsData.integrated_spectrum = integrated_spectrum;
                                }

                                // console.log(fitsData);

                                // handle the fitsData part
                                fitsContainer[index - 1] = fitsData;
                                optical_view = fitsData.is_optical;
                                spectrum_view = fitsData.is_spectrum;

                                if (!spectrum_view) {
                                    d3.select("#atomicMenu").remove();
                                }

                                if (optical_view) {
                                    d3.select("#splatMenu").remove();
                                }

                                if (!isLocal) {
                                    let filesize = fitsData.filesize;
                                    let strFileSize = numeral(filesize).format('0.0ib');
                                    d3.select("#FITS").html("full download (" + strFileSize + ")");
                                }

                                {
                                    frame_reference_unit(index);

                                    //rescale CRVAL3 and CDELT3
                                    fitsData.CRVAL3 *= frame_multiplier;
                                    fitsData.CDELT3 *= frame_multiplier;

                                    frame_reference_type(index);

                                    //console.log("has_freq:", has_frequency_info, "has_vel:", has_velocity_info);
                                }

                                let res = display_FITS_header(index);

                                display_preferences(index);

                                await res;

                                if (index == va_count)
                                    display_dataset_info();

                                if (va_count == 1 || composite_view) {
                                    try {
                                        if (index == va_count) {
                                            display_scale_info();
                                        }
                                    }
                                    catch (err) {
                                    };
                                };

                                if (!composite_view)
                                    add_line_label(index);

                                frame_start = 0;
                                frame_end = fitsData.depth - 1;

                                if (fitsData.depth > 1) {
                                    //insert a spectrum object to the spectrumContainer at <index-1>
                                    mean_spectrumContainer[index - 1] = fitsData.mean_spectrum;
                                    integrated_spectrumContainer[index - 1] = fitsData.integrated_spectrum;

                                    spectrum_count++;

                                    if (va_count == 1) {
                                        setup_axes();

                                        if (intensity_mode == "mean")
                                            plot_spectrum([fitsData.mean_spectrum]);

                                        if (intensity_mode == "integrated")
                                            plot_spectrum([fitsData.integrated_spectrum]);

                                        if (molecules.length > 0)
                                            display_molecules();
                                    }
                                    else {
                                        if (spectrum_count == va_count) {
                                            //console.log("mean spectrumContainer:", mean_spectrumContainer);
                                            //console.log("integrated spectrumContainer:", integrated_spectrumContainer);

                                            //display an RGB legend in place of REF FRQ
                                            display_composite_legend();

                                            setup_axes();

                                            if (intensity_mode == "mean")
                                                plot_spectrum(mean_spectrumContainer);

                                            if (intensity_mode == "integrated")
                                                plot_spectrum(integrated_spectrumContainer);

                                            if (molecules.length > 0)
                                                display_molecules();
                                        }
                                    }
                                }
                                else {
                                    spectrum_count++;
                                }
                            }

                            // WASM decoder part
                            /*Module.ready
                              .then(_ => {*/
                            {
                                // console.log("processing an HDR image");
                                let start = performance.now();

                                // decompressZFP returns std::vector<float>
                                // decompressZFPimage returns Float32Array but emscripten::typed_memory_view is buggy
                                var res = Module.decompressZFPimage(img_width, img_height, frame_pixels);
                                const pixels = Module.HEAPF32.slice(res[0] / 4, res[0] / 4 + res[1]);

                                var res = Module.decompressLZ4mask(img_width, img_height, frame_mask);
                                const alpha = Module.HEAPU8.slice(res[0], res[0] + res[1]);

                                let elapsed = Math.round(performance.now() - start);

                                // console.log("image width: ", img_width, "height: ", img_height, "elapsed: ", elapsed, "[ms]");

                                if (!spectrum_view) {
                                    d3.select("#peak_tracking").remove();

                                    process_hdr_image(img_width, img_height, pixels, alpha, tone_mapping, index);

                                    if (has_json) {
                                        display_histogram(index);

                                        try {
                                            display_cd_gridlines();
                                        }
                                        catch (err) {
                                            display_gridlines();
                                        };

                                        display_beam();
                                    }

                                    if (composite_view) {
                                        if (spectrum_count == va_count)
                                            display_rgb_legend();
                                    } else {
                                        display_legend();
                                    }
                                } else {
                                    console.log("spectrum_view with dimensions: ", img_width, img_height);

                                    d3.select("#regionLabel").remove();
                                    d3.select("#autoscale").remove();
                                    d3.select("#download_confirmation").remove();
                                    d3.select("#display_scaling_help").remove();
                                    d3.select("#realtime_video").remove();
                                    d3.select("#binning_li").remove();
                                    d3.select("#video_fps_control_li").remove();
                                    d3.select("#coords_fmt_li").remove();
                                    d3.select("#intensity_mode_li").remove();
                                    d3.select("#zoom_shape_li").remove();

                                    let fitsData = fitsContainer[va_count - 1];

                                    // if img_height > 1 and CTYPE1 is 'PIXEL' then it is a 2D image spectrum
                                    if (img_height > 1 && fitsData.CTYPE1.trim().toUpperCase() == 'PIXEL') {
                                        console.log("2D image spectrum with dimensions: ", img_width, img_height);

                                        d3.select("#atomicMenu").remove();
                                        d3.select("#displayGridlines").remove();
                                        d3.select("#displayMolecules").remove();
                                        d3.select("#displayBeam").remove();

                                        process_hdr_image(img_width, img_height, pixels, alpha, tone_mapping, index);

                                        if (has_json)
                                            display_histogram(index);

                                        display_legend();
                                    } else {
                                        // remove the unrelated menus
                                        d3.select("#imageMenu").remove();
                                        d3.select("#splatMenu").remove();
                                        d3.select("#viewMenu").remove();
                                        d3.select("#peak_tracking").remove();
                                        d3.select("#realtime_spectrum").remove();
                                        d3.select("#image_quality_li").remove();

                                        // remove an HTML element with id "SpectrumCanvas"
                                        d3.select("#SpectrumCanvas").remove();

                                        var svg = d3.select("#menu_activation_area");
                                        var y = parseFloat(svg.attr("y"));
                                        var height = parseFloat(svg.attr("height"));

                                        let top = y + height + 10 + emFontSize; // an extra spacing
                                        console.log("y:", y, "height:", height, "top:", top);

                                        // add an HTML element with id "SpectrumDiv" to the "mainDiv"
                                        d3.select("#mainDiv").append("div")
                                            .attr("id", "SpectrumDiv")
                                            .on("mouseenter", hide_navigation_bar)
                                            .attr('style', 'position: fixed; left: 10px; top: ' + top + 'px; z-index: 60');

                                        try {
                                            process_hds_spectrum(img_width, img_height, pixels, alpha, "SpectrumDiv");

                                            // remove object, dateobs, ra and dec elements
                                            d3.select("#object").remove();
                                            d3.select("#dateobs").remove();
                                            d3.select("#ra").remove();
                                            d3.select("#dec").remove();

                                            // remove the jvo logo
                                            d3.select("#jvoLogo").remove();

                                        } catch (err) {
                                            console.error(err);
                                        }
                                    }
                                }
                            }
                            /*})
                            .catch(e => console.error(e));*/
                        }

                    })
                    .catch(e => console.error(e));
            }).catch(e => console.error(e));
        }
    }

    xmlhttp.open("GET", url, true);//or "POST" to disable caching
    xmlhttp.responseType = 'arraybuffer';
    xmlhttp.timeout = 0;
    xmlhttp.send();
}

function refresh_tiles(index) {
    if (zoom_scale < 1)
        return;

    if (imageContainer[index - 1] == null) {
        return;
    }
    else {
        if (imageContainer[index - 1].viewportContainer != null) {
            clear_webgl_image_buffers(index);

            // remove a viewport from the imageContainer
            imageContainer[index - 1].viewportContainer = null;

            // re-display the image
            init_webgl_image_buffers(index);
        } else {
            // refresh the image
            imageContainer[index - 1].refresh = true;
        }
    }
}

function tiles_dragstarted(event) {
    console.log("drag started");

    if (zoom_dims == null)
        return;

    // backup the zoom_dims.view x1 and y1
    if (zoom_dims.view != null) {
        zoom_dims.prev_x1 = zoom_dims.view.x1;
        zoom_dims.prev_y1 = zoom_dims.view.y1;
    }

    // set the cursor for each "image_rectangle" + index
    for (let i = 1; i <= va_count; i++) {
        d3.select("#image_rectangle" + i).style('cursor', 'move');
    }

    dragging = true;
}

function tiles_dragended(event) {
    console.log("drag ended");

    if (zoom_dims == null)
        return;

    var offset;

    try {
        offset = d3.pointer(event);
    }
    catch (e) {
        console.log(e);
        return;
    }

    if (isNaN(offset[0]) || isNaN(offset[1]))
        return;

    mouse_position = { x: offset[0], y: offset[1] };
    zoom_dims.mouse_position = mouse_position;

    // track the changes in zoom_dims.view x1 and y1
    let dx = zoom_dims.view.x1 - zoom_dims.prev_x1;
    let dy = zoom_dims.view.y1 - zoom_dims.prev_y1;

    // then adjust zoom_dims.dims x1 and y1 (adjusted for the scale)
    zoom_dims.dims.x1 += zoom_dims.scale * dx;
    zoom_dims.dims.y1 += zoom_dims.scale * dy;

    // set the cursor for each "image_rectangle" + index
    for (let i = 1; i <= va_count; i++) {
        d3.select("#image_rectangle" + i).style('cursor', 'pointer');
    }

    dragging = false;

    // do not wait, call tileTimeout immediately
    tileTimeout();
}

function tiles_dragmove(event) {
    console.log("drag move");

    if (zoom_dims == null)
        return;

    var offset;

    try {
        offset = d3.pointer(event);
    }
    catch (e) {
        console.log(e);
        return;
    }

    if (isNaN(offset[0]) || isNaN(offset[1]))
        return;

    mouse_position = { x: offset[0], y: offset[1] };

    // track the changes
    let dx = mouse_position.x - zoom_dims.mouse_position.x;
    let dy = mouse_position.y - zoom_dims.mouse_position.y;

    // adjust the zoom_dims.view x1 and y1
    zoom_dims.view.x1 = clamp(zoom_dims.prev_x1 - dx / zoom_dims.scale, 0, zoom_dims.width - zoom_dims.view.width);
    zoom_dims.view.y1 = clamp(zoom_dims.prev_y1 - (-dy) / zoom_dims.scale, 0, zoom_dims.height - zoom_dims.view.height); // invert the Y-axis

    for (let i = 1; i <= va_count; i++) {
        refresh_tiles(i);
    }
}
function tiles_zoomstarted(event) {
    console.log("zoom start");

    if (zoom_dims == null)
        return;

    if (zoom_dims.prev_x0 != -1) {
        zoom_dims.dx = zoom_dims.x0 - zoom_dims.prev_x0;
    }

    if (zoom_dims.prev_y0 != -1) {
        zoom_dims.dy = zoom_dims.y0 - zoom_dims.prev_y0;
    }

    zoom_dims.prev_x0 = zoom_dims.x0;
    zoom_dims.prev_y0 = zoom_dims.y0;

    {
        zoom_dims.dims.x1 += (1.0 - zoom_dims.scale) * zoom_dims.dx;
        zoom_dims.dx = 0;
    }

    {
        zoom_dims.dims.y1 += (1.0 - zoom_dims.scale) * zoom_dims.dy;
        zoom_dims.dy = 0;
    }

    dragging = true;
}

function tiles_zoomended(event) {
    console.log("zoom end");

    dragging = false;

    // do not wait, call tileTimeout immediately
    tileTimeout();
}

function tiles_zoom(event) {
    console.log("scale: " + event.transform.k);
    zoom_scale = event.transform.k;
    moving = true;

    if (zoom_dims == null)
        return;

    zoom_dims.scale = zoom_scale;

    //rescale the image
    let width = zoom_dims.width;
    let height = zoom_dims.height;

    let new_width = (width - 0) / zoom_scale;
    let new_height = (height - 0) / zoom_scale;

    let x0 = zoom_dims.x0;
    let y0 = zoom_dims.y0;
    let x1 = zoom_dims.dims.x1;
    let y1 = zoom_dims.dims.y1;

    let _x1 = x0 - (x0 - x1) / zoom_scale;
    let _y1 = y0 - (y0 - y1) / zoom_scale;

    let new_x1 = clamp(_x1, 0, zoom_dims.width - new_width);
    let new_y1 = clamp(_y1, 0, zoom_dims.height - new_height);
    let dx1 = new_x1 - _x1;
    let dy1 = new_y1 - _y1;

    if (dx1 != 0 || dy1 != 0) {
        console.log("CORRECTION: new_x1:", new_x1, "new_y1:", new_y1, "dx1:", dx1, "dy1:", dy1);
    }

    zoom_dims.x1 = new_x1;
    zoom_dims.y1 = new_y1;

    zoom_dims.view = { x1: new_x1, y1: new_y1, width: new_width, height: new_height };
    console.log("zoom_dims:", zoom_dims, "view:", zoom_dims.view);

    // x0, y0 cross-check
    var cross_x0 = x0;
    var cross_y0 = y0;
    if (zoom_dims.rect != null && zoom_dims.mouse_position != null) {
        let image_bounding_dims = zoom_dims.view;
        let rect = zoom_dims.rect;
        let mouse_position = zoom_dims.mouse_position;

        var ax = (image_bounding_dims.width - 1) / (rect.getAttribute("width") - 0);
        var x = image_bounding_dims.x1 + ax * (mouse_position.x - rect.getAttribute("x"));

        var ay = (image_bounding_dims.height - 1) / (rect.getAttribute("height") - 0);
        var y = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (mouse_position.y - rect.getAttribute("y"));

        x = clamp(x, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
        y = clamp(y, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

        cross_x0 = Math.round(x);
        cross_y0 = Math.round(y);

        if (dx1 != 0) {
            zoom_dims.x0 = cross_x0;
            zoom_dims.prev_x0 = zoom_dims.x0;
            zoom_dims.dims.x1 += zoom_dims.scale * dx1;
            console.log("X0 CORRECTION: x0:", x0, "new_x0:", zoom_dims.x0, "dims.x1:", zoom_dims.dims.x1);
        }

        if (dy1 != 0) {
            zoom_dims.y0 = cross_y0;
            zoom_dims.prev_y0 = zoom_dims.y0;
            zoom_dims.dims.y1 += zoom_dims.scale * dy1;
            console.log("Y0 CORRECTION: y0:", y0, "new_y0:", zoom_dims.y0, "dims.y1:", zoom_dims.dims.y1);
        }
    }

    // RE-SET the zoom_dims
    if (zoom_scale == 1.0) {
        // round to the nearest integer (remove small numerical errors)
        zoom_dims.x1 = Math.round(zoom_dims.dims.x1);
        zoom_dims.y1 = Math.round(zoom_dims.dims.y1);
    }

    for (let i = 1; i <= va_count; i++) {
        refresh_tiles(i);

        //keep zoom scale in sync across all images
        try {
            var elem = d3.select("#image_rectangle" + i);
            elem.node().__zoom.k = zoom_scale;
        } catch (e) { };
    }
}

function zoomed(event) {
    if (d3.select("#pvline").attr("opacity") > 0.0) return;

    console.log("scale: " + event.transform.k);
    zoom_scale = event.transform.k;

    console.log("windowLeft:", windowLeft);

    if (!windowLeft) {
        try {
            zoom_beam();
        }
        catch (e) {
            console.log('NON-CRITICAL:', e);
        }

        var evt = new MouseEvent("mousemove");
        d3.select('#image_rectangle').node().dispatchEvent(evt);

        viewport.refresh = true;
    }
}

function shifted(event) {
    if (autoscale)
        return;

    if (last_spectrum == null)
        return;

    console.log("y-axis shift:", event.dy);

    var height = parseFloat(d3.select("#scaling").attr("height"));
    var interval = user_data_max - user_data_min;
    var shift = event.dy * interval / height;

    user_data_max += shift;
    user_data_min += shift;

    plot_spectrum(last_spectrum);
    replot_y_axis();
}

function scaled(event) {
    if (autoscale)
        return;

    if (last_spectrum == null)
        return;

    console.log("y-axis scale:", event.transform.k, "previous:", prev_scale);

    var factor = event.transform.k;

    if (event.transform.k > prev_scale)
        factor = 1.2;

    if (event.transform.k < prev_scale)
        factor = 0.8;

    prev_scale = event.transform.k;

    /*var interval = factor * (tmp_data_max - tmp_data_min) ;
    var middle = (tmp_data_max + tmp_data_min) / 2 ;*/

    var interval = factor * (user_data_max - user_data_min);
    var middle = (user_data_max + user_data_min) / 2;

    user_data_max = middle + interval / 2;
    user_data_min = middle - interval / 2;

    console.log("AFTER:", user_data_min, user_data_max);

    plot_spectrum(last_spectrum);
    replot_y_axis();
}

function videoTimeout(freq) {
    if (!streaming)
        return;

    console.log("video inactive event");

    sent_vid_id++;

    video_count = 0;

    var fill;

    if (theme == "dark")
        fill = 0;
    else
        fill = 255;

    if (composite_view) {
        var request = {
            type: "composite_video",
            frame: freq,
            key: true,
            fill: fill,
            ref_freq: RESTFRQ,
            fps: vidFPS,
            seq_id: sent_vid_id,
            bitrate: Math.round(target_bitrate),
            timestamp: performance.now()
        };

        if (wsConn[0].readyState == 1)
            wsConn[0].send(JSON.stringify(request));
    } else for (let index = 0; index < va_count; index++) {
        var request = {
            type: "video",
            frame: freq,
            key: true,
            fill: fill,
            ref_freq: RESTFRQ,
            fps: vidFPS,
            seq_id: sent_vid_id,
            bitrate: Math.round(target_bitrate),
            timestamp: performance.now()
        };

        if (wsConn[index].readyState == 1)
            wsConn[index].send(JSON.stringify(request));
    }

    setup_window_timeout();
}

function blink() {
    if (stop_blinking)
        return;

    d3.select("#ping")
        .transition()
        .duration(250)
        .attr("opacity", 0.0)
        .transition()
        .duration(250)
        .attr("opacity", 1.0)
        .on("end", blink);
}

function end_blink() {
    stop_blinking = true;
}

function tileTimeout(force = false) {
    console.log("tile inactive event");

    moving = false;

    if (zoom_dims == null) {
        console.log("tileTimeout: zoom_dims == null");
        return;
    }

    if (zoom_dims.view == null) {
        console.log("tileTimeout: zoom_dims.view == null");
        return;
    }

    let image_bounding_dims = zoom_dims.view;

    if (mousedown || streaming || dragging) {
        console.log("tileTimeout: mousedown:", mousedown, "streaming:", streaming, "dragging:", dragging);
        return;
    }

    //do nothing if the view has not changed
    if (!force && zoom_dims.prev_view != null) {
        let previous = zoom_dims.prev_view;

        if (image_bounding_dims.x1 == previous.x1 && image_bounding_dims.y1 == previous.y1 && image_bounding_dims.width == previous.width && image_bounding_dims.height == previous.height) {
            console.log("tileTimeout: zoom_dims.view == zoom_dims.prev_view");
            console.log("previous:", previous, "view:", image_bounding_dims);
            return;
        }
    }

    zoom_dims.prev_view = { x1: image_bounding_dims.x1, y1: image_bounding_dims.y1, width: image_bounding_dims.width, height: image_bounding_dims.height };

    viewport_count = 0;
    sent_seq_id++;

    var request_images = true;

    var canvas = document.getElementById("SpectrumCanvas");
    var width = canvas.width;
    var height = canvas.height;
    var range = get_axes_range(width, height);
    var dx = range.xMax - range.xMin;

    for (let index = 0; index < va_count; index++) {
        let img_width = image_bounding_dims.width;
        let img_height = image_bounding_dims.height;

        let view_width = 0, view_height = 0;
        try {
            let elem = d3.select("#image_rectangle" + (index + 1));
            view_width = parseFloat(elem.attr("width"));
            view_height = parseFloat(elem.attr("height"));
        } catch (err) {
            continue;
        }

        let view_pixels = view_width * view_height;
        let img_pixels = img_width * img_height;

        console.log("viewport: " + view_width + "x" + view_height + " image: " + img_width + "x" + img_height + "view pixels: " + view_pixels + " img pixels: " + img_pixels);

        let image = true;
        let beam = "square";

        if (img_pixels >= view_pixels)
            image = false;

        request_images = request_images && image;

        //convert zoom_dims.view into real FITS coordinates of each dataset
        var fitsData = fitsContainer[index];

        if (fitsData == null)
            continue;

        if (imageContainer[index] == null)
            continue;

        var canvas = imageContainer[index];

        let x1 = image_bounding_dims.x1 * (fitsData.width - 1) / (canvas.width - 1);
        let y1 = image_bounding_dims.y1 * (fitsData.height - 1) / (canvas.height - 1);
        let x2 = (image_bounding_dims.x1 + image_bounding_dims.width - 1) * (fitsData.width - 1) / (canvas.width - 1);
        let y2 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) * (fitsData.height - 1) / (canvas.height - 1);

        var request = {
            type: "realtime_image_spectrum",
            dx: dx,
            image: image,
            quality: image_quality,
            x1: clamp(Math.round(x1 + 1), 1, fitsData.width),
            y1: clamp(Math.round(y1 + 1), 1, fitsData.height),
            x2: clamp(Math.round(x2 + 1), 1, fitsData.width),
            y2: clamp(Math.round(y2 + 1), 1, fitsData.height),
            width: view_width,
            height: view_height,
            beam: beam,
            intensity: intensity_mode,
            frame_start: data_band_lo,
            frame_end: data_band_hi,
            ref_freq: RESTFRQ,
            seq_id: sent_seq_id,
            timestamp: performance.now()
        };

        if (wsConn[index].readyState == 1)
            wsConn[index].send(JSON.stringify(request));
    }

    if (request_images) {
        //display_hourglass();
        stop_blinking = false;
        blink();
    }

    setup_window_timeout();
}

function imageTimeout() {
    //console.log("image inactive event");

    if ((mousedown && d3.select("#pvline").attr("opacity") < 1.0) || streaming)
        return;

    moving = false;

    //d3.select("#image_rectangle").style('cursor','crosshair');

    // console.log("idle mouse position: ", mouse_position);

    var svg = d3.select("#FrontSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    let fitsData = fitsContainer[va_count - 1];
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;
    var scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);
    var img_width = Math.floor(scale * image_bounding_dims.width);
    var img_height = Math.floor(scale * image_bounding_dims.height);

    //var _y1 = imageContainer[va_count - 1].height - image_bounding_dims.height - image_bounding_dims.y1;

    var rect_elem = d3.select("#image_rectangle");

    var ax = (image_bounding_dims.width - 1) / (rect_elem.attr("width") - 0);
    var x = image_bounding_dims.x1 + ax * (mouse_position.x - rect_elem.attr("x"));

    var ay = (image_bounding_dims.height - 1) / (rect_elem.attr("height") - 0);
    var y = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (mouse_position.y - rect_elem.attr("y"));

    x = clamp(x, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y = clamp(y, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var clipSize = Math.min(image_bounding_dims.width - 1, image_bounding_dims.height - 1) / zoom_scale;
    var sel_width = Math.floor(clipSize * scale);
    var sel_height = Math.floor(clipSize * scale);

    var fitsX = Math.round(x * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1));
    var fitsY = Math.round(y * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1));
    var fitsSize = clipSize * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);

    var image_update = true;

    if (Math.round(fitsSize) > Math.round(clipSize))
        image_update = true;
    else
        image_update = false;

    //console.log('idle', 'x = ', x, 'y = ', y, 'clipSize = ', clipSize, 'fitsX = ', fitsX, 'fitsY = ', fitsY, 'fitsSize = ', fitsSize, 'image_update:', image_update);

    //send an image/spectrum request to the server
    var x1 = Math.round(fitsX - fitsSize);
    var y1 = Math.round(fitsY - fitsSize);
    var x2 = Math.round(fitsX + fitsSize);
    var y2 = Math.round(fitsY + fitsSize);

    var dimx = Math.abs(x2 - x1) + 1;
    var dimy = Math.abs(y2 - y1) + 1;

    if (dimx != dimy)
        console.log("unequal dimensions:", dimx, dimy, "fitsX =", fitsX, "fitsY =", fitsY, "fitsSize =", fitsSize);

    var zoomed_size = get_zoomed_size(width, height, img_width, img_height);

    //console.log("zoomed_size:", zoomed_size);

    if (moving || streaming)
        return;

    compositeViewportTexture = null;
    viewport_count = 0;

    sent_seq_id++;

    // attach a CSV export handler
    if (has_velocity_info || has_frequency_info) {
        _x1 = x1; _x2 = x2; _y1 = y1; _y2 = y2; // global variables

        var elem = document.getElementById('exportCSV');

        if (elem != null) {
            elem.onclick = function () {
                console.log("export viewport to CSV.");

                var c = 299792.458;//speed of light [km/s]

                var deltaV = 0.0;

                try {
                    deltaV = document.getElementById('velocityInput').valueAsNumber;//[km/s]
                }
                catch (e) {
                    console.log(e);
                    console.log("USER_DELTAV = ", USER_DELTAV);
                }

                //convert redshift z to V
                var value = sessionStorage.getItem("redshift");

                if (value == "z") {
                    var tmp = - (1.0 - (1.0 + deltaV) * (1.0 + deltaV)) / (1.0 + (1.0 + deltaV) * (1.0 + deltaV));

                    deltaV = tmp * c;
                };

                var checkbox = document.getElementById('restcheckbox');
                var rest = false;

                try {
                    rest = checkbox.checked;
                } catch (e) {
                    console.log(e);
                }

                display_hourglass();

                for (let index = 0; index < va_count; index++) {
                    // a CSV websocket request
                    var request = {
                        type: "spectrum",
                        ra: d3.select("#ra").text().toString(),
                        dec: d3.select("#dec").text().toString(),
                        x1: _x1 + 1,
                        y1: _y1 + 1,
                        x2: _x2 + 1,
                        y2: _y2 + 1,
                        beam: zoom_shape,
                        intensity: intensity_mode,
                        frame_start: data_band_lo,
                        frame_end: data_band_hi,
                        ref_freq: RESTFRQ,
                        deltaV: 1000.0 * deltaV, // [m/s]
                        rest: rest,
                        seq_id: sent_seq_id,
                        timestamp: performance.now(),
                    };

                    if (wsConn[index].readyState == 1)
                        wsConn[index].send(JSON.stringify(request));
                }

                setup_window_timeout();
            };
        }
    }

    for (let index = 0; index < va_count; index++) {
        // a real-time websocket request
        var range = get_axes_range(width, height);
        var dx = range.xMax - range.xMin;
        var dy = range.yMax - range.yMin;

        if (viewport_zoom_settings != null) {
            let _width = viewport_zoom_settings.zoomed_size;
            let _height = viewport_zoom_settings.zoomed_size;

            var request;

            if (fitsData.is_spectrum) {
                request = {
                    type: "realtime_image_spectrum",
                    dx: dx,
                    dy: dy,
                    image: image_update,
                    quality: image_quality,
                    x1: x1 + 1,
                    y1: y1 + 1,
                    x2: x2 + 1,
                    y2: y2 + 1,
                    width: _width,
                    height: _height,
                    beam: zoom_shape,
                    x: fitsX + 1,
                    y: fitsY + 1,
                    tracking: peak_tracking,
                    seq_id: sent_seq_id,
                    timestamp: performance.now()
                };
            } else {
                request = {
                    type: "realtime_image_spectrum",
                    dx: dx,
                    image: image_update,
                    quality: image_quality,
                    x1: x1 + 1,
                    y1: y1 + 1,
                    x2: x2 + 1,
                    y2: y2 + 1,
                    width: _width,
                    height: _height,
                    beam: zoom_shape,
                    intensity: intensity_mode,
                    frame_start: data_band_lo,
                    frame_end: data_band_hi,
                    ref_freq: RESTFRQ,
                    seq_id: sent_seq_id,
                    timestamp: performance.now()
                };
            }

            if (wsConn[index].readyState == 1)
                wsConn[index].send(JSON.stringify(request));
        }
    }

    setup_window_timeout();

    if (moving || streaming)
        return;

    var zoom_element = d3.select("#zoom");
    var zoom_cross = d3.select("#zoomCross");

    //in the meantime repaint the selection element and the zoom canvas
    let mx = mouse_position.x;
    let my = mouse_position.y;

    if (zoom_shape == "square")
        zoom_element.attr("x", mx - sel_width).attr("y", my - sel_height).attr("width", 2 * sel_width).attr("height", 2 * sel_height).attr("opacity", 1.0);

    if (zoom_shape == "circle")
        zoom_element.attr("cx", mx).attr("cy", my).attr("r", Math.round(sel_width)).attr("opacity", 1.0);

    var crossSize = 1.0 * emFontSize;
    zoom_cross.attr("x", mx - crossSize / 2).attr("y", my - crossSize / 2).attr("width", crossSize).attr("height", crossSize).attr("opacity", 0.75);

    var px, py;

    if (zoom_location == "upper") {
        px = emStrokeWidth;
        py = emStrokeWidth;
    }
    else {
        px = width - 1 - emStrokeWidth - zoomed_size;
        py = height - 1 - emStrokeWidth - zoomed_size;
    }

    zoomed_size = Math.round(zoomed_size);
    px = Math.round(px);
    py = Math.round(py);

    //ctx.clearRect(px, py, zoomed_size, zoomed_size);

    //console.log("imageTimeout::END");
}

function resetKalman() {
    last_x = $V([mouse_position.x, mouse_position.y, 0, 0]);
    //last_x = $V([0, 0, 0, 0]);
    last_velX = 0;
    last_velY = 0;
    last_xPos = mouse_position.x;
    last_yPos = mouse_position.y;
    last_t = performance.now();
}

function initKalman() {
    A = $M([
        [1, 0, 1, 0],
        [0, 1, 0, 1],
        [0, 0, 1, 0],
        [0, 0, 0, 1]
    ]);

    B = $M([
        [1, 0, 0, 0],
        [0, 1, 0, 0],
        [0, 0, 1, 0],
        [0, 0, 0, 1]
    ]);

    H = $M([
        [1, 0, 1, 0],
        [0, 1, 0, 1],
        [0, 0, 0, 0],
        [0, 0, 0, 0]
    ]);

    Q = $M([
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0.1, 0],
        [0, 0, 0, 0.1]
    ]);

    R = $M([
        [100, 0, 0, 0],
        [0, 100, 0, 0],
        [0, 0, 1000, 0],
        [0, 0, 0, 1000]
    ]);

    resetKalman();

    last_P = $M([
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]
    ]);

    initKalmanFilter = true;
}

function updateKalman() {
    cur_xPos = mouse_position.x;
    cur_yPos = mouse_position.y;

    var now = performance.now();
    var dt = now - last_t;

    if (dt == 0)
        return;

    last_t = now;

    //update A and H to take into account dt
    A.elements[0][2] = dt;
    A.elements[1][3] = dt;

    /*** KALMAN FILTER CODE ***/
    var velX = (cur_xPos - last_x.elements[0]) / dt;
    var velY = (cur_yPos - last_x.elements[1]) / dt;

    /*var velX = (cur_xPos - last_xPos)/dt;
    var velY = (cur_yPos - last_yPos)/dt;
    var accX = (velX - last_velX)/dt;
    var accY = (velY - last_velY)/dt;

    last_xPos = cur_xPos ;
    last_yPos = cur_yPos ;
    last_velX = velX ;
    last_velY = velY ;*/

    var measurement = $V([cur_xPos, cur_yPos, velX, velY]);
    //var measurement = $V([velX, velY, accX, accY]);
    var control = $V([0, 0, 0, 0]); // TODO - adjust

    // prediction
    var x = (A.multiply(last_x)).add(B.multiply(control));
    var P = ((A.multiply(last_P)).multiply(A.transpose())).add(Q);

    // correction
    var S = ((H.multiply(P)).multiply(H.transpose())).add(R);
    var K = (P.multiply(H.transpose())).multiply(S.inverse());
    var y = measurement.subtract(H.multiply(x));

    var cur_x = x.add(K.multiply(y));
    var cur_P = ((Matrix.I(4)).subtract(K.multiply(H))).multiply(P);

    last_x = cur_x;
    last_P = cur_P;
    /**************************/

    //return ;

    //console.log("mouse_position: x=", mouse_position.x, "y=", mouse_position.y) ;
    //console.log("K:", K) ;
    //console.log("Kalman Filter X=", cur_x.elements[0], "Y=",cur_x.elements[1], "Vx=", cur_x.elements[2], "Vy=",cur_x.elements[3]) ;
    //console.log("Kalman Filter Vx=", cur_x.elements[0], "Vy=",cur_x.elements[1], "Ax=", cur_x.elements[2], "Ay=",cur_x.elements[3]) ;

    return;

    /*mouse_position.x = cur_x.elements[0];
    mouse_position.y = cur_x.elements[1];

    return;

    //extrapolation
    var predX = last_x;
    var count = 5;//how many frames ahead

    for (var i = 0; i < count; i++)
      predX = (A.multiply(predX)).add(B.multiply(control));

    console.log("extrapolation: x=", predX.elements[0], "y=", predX.elements[1]);

    mouse_position.x = predX.elements[0];
    mouse_position.y = predX.elements[1];*/
}

function change_noise_sensitivity(index) {
    noise_sensitivity = document.getElementById('sensitivity' + index).value;
    var multiplier = get_noise_sensitivity(noise_sensitivity);
    document.getElementById('sensitivityInput' + index).innerHTML = get_noise_sensitivity_string(noise_sensitivity, 2);

    var c = document.getElementById("HistogramCanvas" + index);
    var svg = d3.select("#HistogramSVG" + index);
    var width = c.width;
    var height = c.height;

    var flux_elem = d3.select("#flux_path" + index);

    var black, white, median;

    try {
        black = parseFloat(flux_elem.attr("black"));
    }
    catch (e) {
    };

    try {
        white = parseFloat(flux_elem.attr("white"));
    }
    catch (e) {
    };

    try {
        median = parseFloat(flux_elem.attr("median"));
    }
    catch (e) {
    };

    var path = get_flux_path(width, height, document.getElementById('flux' + index).value, black, white, median, multiplier, index);

    flux_elem.attr("d", path);

    // set image tone mapping
    var image = imageContainer[index - 1];
    /*let fitsData = fitsContainer[index - 1];

    if (image.tone_mapping.flux == "ratio")
      image.tone_mapping.ratio_sensitivity = multiplier * fitsData.ratio_sensitivity;
    else
      image.tone_mapping.sensitivity = multiplier * fitsData.sensitivity;*/

    if (image.tone_mapping.flux == "legacy") {
        let p = get_slope_from_multiplier(multiplier);
        image.tone_mapping.lmin = Math.log(p);
        image.tone_mapping.lmax = Math.log(p + 1.0);
    }

    if (composite_view) {
        image = compositeImage;
    }

    image.refresh = true;

    if (va_count == 1) {
        update_legend();
    } else {
        draw_rbg_legend(index);
    }
}

function partial_fits_size() {
    let frame_bounds = get_frame_bounds(data_band_lo, data_band_hi, va_count - 1);
    let len = Math.abs(frame_bounds.frame_end - frame_bounds.frame_start) + 1;

    //console.log("frame_bounds:", frame_bounds, "#frames:", len);

    var offsetx = d3.select("#image_rectangle").attr("x");
    var offsety = d3.select("#image_rectangle").attr("y");

    let fitsData = fitsContainer[va_count - 1];
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;
    //console.log("BITPIX = ", fitsData.BITPIX);

    var ax = (image_bounding_dims.width - 1) / (d3.select("#image_rectangle").attr("width") - 0);
    var ay = (image_bounding_dims.height - 1) / (d3.select("#image_rectangle").attr("height") - 0);

    var x1 = image_bounding_dims.x1 + ax * (begin_x - offsetx);
    var y1 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (begin_y - offsety);

    x1 = clamp(x1, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y1 = clamp(y1, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x1 = x1 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y1 = y1 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    var x2 = image_bounding_dims.x1 + ax * (end_x - offsetx);
    var y2 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (end_y - offsety);

    x2 = clamp(x2, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y2 = clamp(y2, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x2 = x2 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y2 = y2 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    let dimx = Math.abs(orig_x2 - orig_x1) + 1;
    let dimy = Math.abs(orig_y2 - orig_y1) + 1;

    let fitsHeader = fitsData.HEADER;

    let partial_size = roundUp(fitsHeader.length, 2880) + len * dimx * dimy * Math.round(Math.abs(fitsData.BITPIX) / 8);

    // FITS header/data units come in multiples of 2880 bytes
    return va_count * roundUp(partial_size, 2880);
}

function pv_contour(left, top, width, height, pvCanvas, flipY, pv_width, pv_height, frame_pv) {
    console.log("calling pv_contour with canvas: ", pvCanvas, "flipY: ", flipY);

    d3.select("#PVContourSVG").remove();

    let svg_left = 10 + left;
    let svg_top = 10 + top;

    let svg_width = width;
    let svg_height = height;

    var div = d3.select("#PVDiagram");

    if (document.getElementById('PVContourSVG') === null) {
        // console.log("pv_axes: PVContourSVG is null, creating a new one.");

        div.append("svg")
            .attr("id", "PVContourSVG")
            .attr("width", svg_width)
            .attr("height", svg_height)
            .attr('style', `position: fixed; left: ${svg_left}px; top: ${svg_top}px; cursor: default`);

        d3.select("#PVAXISLINE").moveToFront();
    }

    if (!displayPVContours) return;

    if (va_count == 1) {
        var pv_pixels = Module.decompressZFP2D(pv_width, pv_height, frame_pv);

        // get minimum and maximum floating point value
        var min_value = Number.MAX_VALUE
        var max_value = -Number.MAX_VALUE;

        var data = [];
        var z;

        for (let h = pv_height - 1; h >= 0; h--) {
            let row = [];
            let pixel = h * pv_width;

            for (let w = 0; w < pv_width; w++) {
                z = pv_pixels.get(pixel);
                pixel += 1;

                if (z < min_value)
                    min_value = z;

                if (z > max_value)
                    max_value = z;

                row.push(z);
            }

            data.push(row);
        }
    } else {
        let imageData = pvCanvas.getContext('2d').getImageData(0, 0, pvCanvas.width, pvCanvas.height).data;

        var min_value = 255;
        var max_value = 0;

        var data = [];
        var r, g, b, z;

        for (let h = pvCanvas.height - 1; h >= 0; h--) {
            let row = [];
            let pixel = 4 * (h * pvCanvas.width);

            for (let w = 0; w < pvCanvas.width; w++) {
                r = imageData[pixel];
                g = imageData[pixel + 1];
                b = imageData[pixel + 2];
                z = (r + g + b) / va_count;
                pixel += 4;

                if (z < min_value)
                    min_value = z;

                if (z > max_value)
                    max_value = z;

                row.push(z);
            }

            data.push(row);
        }

    }

    let pmin = min_value;
    let pmax = max_value;

    console.log("min_value = ", min_value, " max_value = ", max_value);

    var pv_linear = document.getElementById('pv_linear');
    var pv_sqrt = document.getElementById('pv_sqrt');
    var pv_log = document.getElementById('pv_log');

    var contours = parseInt(document.getElementById('pv_contour_lines').value);
    var zs = new Array(contours);

    if (pv_linear.checked) {
        var step = (max_value - min_value) / (contours + 1);

        for (var i = 0; i < contours; i++)
            zs[i] = min_value + (i + 1) * step;
    } else if (pv_sqrt.checked) {
        min_value = 0.0;
        max_value = Math.sqrt(pmax - pmin);
        var step = (max_value - min_value) / (contours + 1);

        for (var i = 0; i < contours; i++) {
            zs[i] = min_value + (i + 1) * step;
            zs[i] = zs[i] * zs[i] + pmin;
        }
    } else if (pv_log.checked) {
        min_value = Math.log(Math.E);
        max_value = Math.log(pmax - pmin + Math.E);
        var step = (max_value - min_value) / (contours + 1);

        for (var i = 0; i < contours; i++) {
            zs[i] = min_value + (i + 1) * step;
            zs[i] = Math.exp(zs[i]) + pmin - Math.E;
        }
    } else {
        console.log("pv_contour: unknown contour type.");
        return;
    }

    console.log("zs:", zs);

    var completed_levels = 0;
    //parallel isoBands
    for (var i = 1; i < zs.length; i++) {
        var lowerBand = zs[i - 1];
        var upperBand = zs[i];

        var CRWORKER = new Worker('data:text/html;base64,' + window.btoa(get_worker_script()));

        CRWORKER.addEventListener('message', function (e) {
            //console.log('Worker said: ', e.data);
            completed_levels++;

            var isoBands = [];
            isoBands.push({ "coords": e.data, "level": i, "val": zs[i] });

            //plot the isoBands
            var elem = d3.select("#PVContourSVG");
            var width = parseFloat(elem.attr("width"));
            var height = parseFloat(elem.attr("height"));

            console.log("PVContourSVG width = ", width, " height = ", height);

            var x = d3.scaleLinear()
                .range([width, 0]) // flip the x-axis
                .domain([0, data[0].length - 1]);

            if (flipY) {
                var y = d3.scaleLinear()
                    .range([height, 0])
                    .domain([0, data.length - 1]);
            } else {
                var y = d3.scaleLinear()
                    .range([0, height])
                    .domain([0, data.length - 1]);
            }

            if (va_count == 1) {
                var strokeColour = "black";
            } else {
                //var strokeColour = "rgb(255,204,0)";
                if (va_count == 2) {
                    var strokeColour = "blue";
                } else {
                    var strokeColour = "Lightgreen";
                }
            }

            d3.select("#PVContourSVG").append("svg")
                .attr("id", "PVContourPlot")
                .attr("x", parseFloat(elem.attr("x")))
                .attr("y", parseFloat(elem.attr("y")))
                .attr("width", width)
                .attr("height", height)
                .selectAll("path")
                .data(isoBands)
                .enter().append("path")
                .style("fill", "none")
                .style("stroke", strokeColour)
                .attr("opacity", 0.25) // or black with 0.25
                .attr("d", function (d) {
                    var p = "";
                    d.coords.forEach(function (aa, i) {
                        p += (d3.line()
                            .x(function (dat) { return x(dat[0]); })
                            .y(function (dat) { return ((height - 1) - y(dat[1])); })
                            .curve(d3.curveLinear)
                        )(aa) + "Z";
                    });
                    return p;
                });

            if (completed_levels == zs.length - 1)
                console.log("completed_levels = ", completed_levels, " zs.length = ", zs.length)
        }, false);

        CRWORKER.postMessage({ data: data, level: i, lowerBand: lowerBand, upperBand: upperBand });
    };

    d3.select("#velocityline").moveToFront();
}

function send_pv_request(x1, y1, x2, y2) {
    var c = 299792.458;//speed of light [km/s]

    var deltaV = 0.0;

    try {
        deltaV = document.getElementById('velocityInput').valueAsNumber;//[km/s]
    }
    catch (e) {
        console.log(e);
        console.log("USER_DELTAV = ", USER_DELTAV);
    }

    //convert redshift z to V
    var value = sessionStorage.getItem("redshift");

    if (value == "z") {
        var tmp = - (1.0 - (1.0 + deltaV) * (1.0 + deltaV)) / (1.0 + (1.0 + deltaV) * (1.0 + deltaV));

        deltaV = tmp * c;
    };

    var checkbox = document.getElementById('restcheckbox');
    var rest = false;

    try {
        rest = checkbox.checked;
    } catch (e) {
        console.log(e);
    }

    var c = document.getElementById('PVCanvas');
    var width = c.width / 2;
    var height = c.height;

    sent_seq_id++;

    var request = {
        type: composite_view ? "composite_pv" : "pv",
        x1: Math.round(x1),
        y1: Math.round(y1),
        x2: Math.round(x2),
        y2: Math.round(y2),
        width: width,
        height: height,
        frame_start: data_band_lo,
        frame_end: data_band_hi,
        ref_freq: RESTFRQ,
        deltaV: 1000.0 * deltaV, // [m/s]
        rest: rest,
        seq_id: sent_seq_id,
        timestamp: performance.now()
    };

    //send a P-V diagram request to the server
    if (wsConn[0].readyState == 1)
        wsConn[0].send(JSON.stringify(request));

    setup_window_timeout();
}

function submit_pv_line(line_x1, line_y1, line_x2, line_y2) {
    mousedown = false;

    var offsetx = d3.select("#image_rectangle").attr("x");
    var offsety = d3.select("#image_rectangle").attr("y");

    let fitsData = fitsContainer[va_count - 1];
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;

    var ax = (image_bounding_dims.width - 1) / (d3.select("#image_rectangle").attr("width") - 0);
    var ay = (image_bounding_dims.height - 1) / (d3.select("#image_rectangle").attr("height") - 0);

    var x1 = image_bounding_dims.x1 + ax * (line_x1 - offsetx);
    var y1 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (line_y1 - offsety);

    x1 = clamp(x1, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y1 = clamp(y1, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x1 = 1 + x1 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y1 = 1 + y1 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    var x2 = image_bounding_dims.x1 + ax * (line_x2 - offsetx);
    var y2 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (line_y2 - offsety);

    x2 = clamp(x2, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y2 = clamp(y2, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x2 = 1 + x2 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y2 = 1 + y2 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    // console.log("orig_x1:", orig_x1, "orig_y1:", orig_y1, "orig_x2:", orig_x2, "orig_y2:", orig_y2);

    send_pv_request(orig_x1, orig_y1, orig_x2, orig_y2);

    return {
        x1: (line_x1 - offsetx) / d3.select("#image_rectangle").attr("width"),
        y1: (line_y1 - offsety) / d3.select("#image_rectangle").attr("height"),
        x2: (line_x2 - offsetx) / d3.select("#image_rectangle").attr("width"),
        y2: (line_y2 - offsety) / d3.select("#image_rectangle").attr("height")
    }
}

function loop_pv_line() {
    resubmit_pv_line();
    pv_loop = setTimeout(loop_pv_line, pv_latency);
}

function resubmit_pv_line() {
    var line = d3.select("#pvline2");
    var line_x1 = parseFloat(line.attr("x1")) / pvsvg_width;
    var line_y1 = parseFloat(line.attr("y1")) / pvsvg_height;
    var line_x2 = parseFloat(line.attr("x2")) / pvsvg_width;
    var line_y2 = parseFloat(line.attr("y2")) / pvsvg_height;

    let fitsData = fitsContainer[va_count - 1];
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;

    var x1 = image_bounding_dims.x1 + image_bounding_dims.width * line_x1;
    var y1 = (image_bounding_dims.y1 + image_bounding_dims.height - 0) - image_bounding_dims.height * line_y1;

    var orig_x1 = 1 + x1 * fitsData.width / imageContainer[va_count - 1].width;
    var orig_y1 = 1 + y1 * fitsData.height / imageContainer[va_count - 1].height;

    var x2 = image_bounding_dims.x1 + image_bounding_dims.width * line_x2;
    var y2 = (image_bounding_dims.y1 + image_bounding_dims.height - 0) - image_bounding_dims.height * line_y2;

    var orig_x2 = 1 + x2 * (fitsData.width - 0) / (imageContainer[va_count - 1].width - 0);
    var orig_y2 = 1 + y2 * (fitsData.height - 0) / (imageContainer[va_count - 1].height - 0);

    // console.log("orig_x1:", orig_x1, "orig_y1:", orig_y1, "orig_x2:", orig_x2, "orig_y2:", orig_y2);

    send_pv_request(orig_x1, orig_y1, orig_x2, orig_y2);
}

function partial_fits_download() {
    mousedown = false;
    d3.select("#region").attr("opacity", 0.0);

    var offsetx = d3.select("#image_rectangle").attr("x");
    var offsety = d3.select("#image_rectangle").attr("y");

    let fitsData = fitsContainer[va_count - 1];
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;

    var ax = (image_bounding_dims.width - 1) / (d3.select("#image_rectangle").attr("width") - 0);
    var ay = (image_bounding_dims.height - 1) / (d3.select("#image_rectangle").attr("height") - 0);

    var x1 = image_bounding_dims.x1 + ax * (begin_x - offsetx);
    var y1 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (begin_y - offsety);

    x1 = clamp(x1, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y1 = clamp(y1, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x1 = 1 + x1 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y1 = 1 + y1 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    var x2 = image_bounding_dims.x1 + ax * (end_x - offsetx);
    var y2 = (image_bounding_dims.y1 + image_bounding_dims.height - 1) - ay * (end_y - offsety);

    x2 = clamp(x2, image_bounding_dims.x1, image_bounding_dims.x1 + image_bounding_dims.width - 1);
    y2 = clamp(y2, image_bounding_dims.y1, image_bounding_dims.y1 + image_bounding_dims.height - 1);

    var orig_x2 = 1 + x2 * (fitsData.width - 1) / (imageContainer[va_count - 1].width - 1);
    var orig_y2 = 1 + y2 * (fitsData.height - 1) / (imageContainer[va_count - 1].height - 1);

    var url = "get_fits?";

    if (va_count == 1)
        url += "datasetId=" + encodeURIComponent(datasetId) + "&";
    else {
        for (let index = 1; index <= va_count; index++)
            url += "datasetId" + index + "=" + encodeURIComponent(datasetId[index - 1]) + "&";
    }

    url += "x1=" + Math.round(orig_x1) + "&y1=" + Math.round(orig_y1) + "&x2=" + Math.round(orig_x2) + "&y2=" + Math.round(orig_y2) + "&frame_start=" + data_band_lo + "&frame_end=" + data_band_hi + "&ref_freq=" + RESTFRQ;

    //console.log(url) ;
    //window.location.assign(url);
    window.open(url, '_blank');
    window.focus();
}

function ok_download() {
    $('#downloadConfirmation').modal('hide');
    d3.select("#downloadConfirmation").remove();

    partial_fits_download();
};

function cancel_download() {
    mousedown = false;
    d3.select("#region").attr("opacity", 0.0);

    $('#downloadConfirmation').modal('hide');
    d3.select("#downloadConfirmation").remove();
};

function show_download_confirmation() {
    var modal = document.getElementById('downloadConfirmation');
    var span = document.getElementById('downloadconfirmationclose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $('#downloadConfirmation').modal('hide');
        d3.select("#downloadConfirmation").remove();

        mousedown = false;
        d3.select("#region").attr("opacity", 0.0);
    }
    // When the user clicks a mouse, close it
    window.onclick = function (event) {
        if (event.target == modal) {
            $('#downloadConfirmation').modal('hide');
            d3.select("#downloadConfirmation").remove();

            mousedown = false;
            d3.select("#region").attr("opacity", 0.0);
        }
    }
}

function show_scaling_help() {
    var modal = document.getElementById('scalingHelp');
    var span = document.getElementById('scalingHeaderClose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $('#scalingHelp').modal('hide');
        d3.select("#scalingHelp").remove();
    }
    // When the user moves a mouse, close it
    window.onmousemove = function (event) {
        if (event.target == modal) {
            $('#scalingHelp').modal('hide');
            d3.select("#scalingHelp").remove();
        }
    }
}

function load_region() {
    console.log("loading a ds9 region file");

    // a user-selected ds9 region file
    const file = document.querySelector("#regionFile").files[0];

    if (!file) return;

    // file name
    let file_name = file.name;

    // file MIME type
    let file_type = file.type;

    // file size in bytes
    let file_size = file.size;

    console.log("file_name:", file_name, "file_type:", file_type, "file_size:", file_size);

    // append the file name to the label text "regionLabel"
    d3.select("#regionLabel").html("ds9 region: " + file_name + '<input type="file" accept=".reg, .REG" id="regionFile" style="display:none;" onclick="javascript:hide_navigation_bar();" onchange="javascript:load_region();"/>');

    let reader = new FileReader();

    reader.addEventListener('error', function (e) {
        let err = e.target.error;
        console.error(err);
        d3.select("#regionLabel").html("error loading " + file_name + " (" + err + ")" + '<input type="file" accept=".reg, .REG" id="regionFile" style="display:none;" onclick="javascript:hide_navigation_bar();" onchange="javascript:load_region();"/>');

        displayRegion = false;
        d3.select("#ds9region").attr("opacity", 0);
        document.getElementById("displayRegion").style.display = "none";
    });

    reader.addEventListener('load', async function (e) {
        let region = e.target.result;
        console.log(region);

        // split region into lines
        let lines = region.split(/\r?\n/);

        var coordinate_system = "unknown";
        var points = [];

        // iterate through lines
        for (let i = 0; i < lines.length; i++) {
            let line = lines[i];

            // skip empty lines
            if (line.length == 0) continue;

            // skip lines starting with '#'
            if (line.startsWith('#')) continue;

            // check if the line contains a coordinate system like "physical", "image", "fk5" or "icrs"
            if (line.startsWith("physical")) {
                coordinate_system = "physical";
                continue;
            }

            if (line.startsWith("image")) {
                coordinate_system = "image";
                continue;
            }

            // unsupported, the dec from AST is a bit off
            /*if (line.startsWith("fk4")) {
                coordinate_system = "fk4";
                continue;
            }*/

            if (line.startsWith("fk5")) {
                coordinate_system = "fk5";
                continue;
            }

            if (line.startsWith("icrs")) {
                coordinate_system = "icrs";
                continue;
            }

            // seems to work fine but disabled for now
            /*if (line.startsWith("galactic")) {
                coordinate_system = "galactic";
                continue;
            }*/

            // unsupported, the ra,dec from AST is way off
            /*if (line.startsWith("ecliptic")) {
                coordinate_system = "ecliptic";
                continue;
            }*/

            // extract the coordinates x,y from "circle(x,y,r)" or "point(x,y)"
            let match = line.match(/circle\((.*),(.*),.*\)/);
            let shape = "circle";

            if (match == null) {
                match = line.match(/point\((.*),(.*)\)/);
                shape = "point";
            }

            if (match != null) {
                let x = parseFloat(match[1]);
                let y = parseFloat(match[2]);

                // push the coordinates to the points array
                points.push({ x: x, y: y, shape: shape });
            }
        }

        console.log("coordinate_system:", coordinate_system);

        // fk5 is unsupported at the moment
        /*if (coordinate_system == "fk5") {
            alert("WCS (fk5) coordinate system is not supported at the moment. Use a physical or image coordinate system.");
            coordinate_system = "unknown";
        }*/

        if (coordinate_system == "unknown") {
            alert("Unknown / unsupported coordinate system. Supported ds9 coordinate systems: physical, image, fk5 and icrs.");
            d3.select("#regionLabel").html(file_name + ": (unsupported coordinate system)" + '<input type="file" accept=".reg, .REG" id="regionFile" style="display:none;" onclick="javascript:hide_navigation_bar();" onchange="javascript:load_region();"/>');
            return;
        }

        // check if there are any points
        if (points.length == 0) {
            alert("No points found in the region file. Supported ds9 shapes: Circle and Point.");
            d3.select("#regionLabel").html(file_name + ": (no points found)" + '<input type="file" accept=".reg, .REG" id="regionFile" style="display:none;" onclick="javascript:hide_navigation_bar();" onchange="javascript:load_region();"/>');

            displayRegion = false;
            d3.select("#ds9region").attr("opacity", 0);
            document.getElementById("displayRegion").style.display = "none";

            return;
        } else {
            // coordinate system transformation
            if (coordinate_system == "fk4" || coordinate_system == "fk5" || coordinate_system == "icrs" || coordinate_system == "galactic" || coordinate_system == "ecliptic") {
                // convert from world (sky) to pixel coordinates
                let fitsData = fitsContainer[va_count - 1];

                console.log("FITS RADESYS:", fitsData.RADESYS);

                for (let i = 0; i < points.length; i++) {
                    let point = points[i];

                    var ra = point.x;
                    var dec = point.y;

                    // optionally convert the coordinate systems to the one specified in the FITS header
                    // If fitsData.RADESYS is not empty and the lowercase value does not match coordinate_system
                    // request a coordinate conversion from the server using HTTP GET
                    if (fitsData.RADESYS != "" && fitsData.RADESYS.trim().toLowerCase() != coordinate_system) {
                        let url = "wcs?ra=" + ra + "&dec=" + dec + "&from=" + coordinate_system + "&to=" + fitsData.RADESYS.trim().toLowerCase();
                        console.log("WCS url:", url);

                        let response = await fetch(url);

                        if (response.ok) {
                            await response.json().then(json => {
                                ra = json.ra;
                                dec = json.dec;
                            }).catch(error => {
                                console.error(error);
                            });
                        }
                    }

                    // convert from world (sky) to pixel coordinates
                    let pixcrd = sky2pix(fitsData, ra, dec);
                    point.x = pixcrd[0];
                    point.y = pixcrd[1];

                    // console.log("ra:", ra, "dec:", dec, "x:", point.x, "y:", point.y);
                }
            }

            if (coordinate_system == "physical" || coordinate_system == "image") {
                // convert physical/image coordinates to 0-based pixels (subtract 1)
                for (let i = 0; i < points.length; i++) {
                    let point = points[i];

                    // ds9: the lower left pixel of the array will have the image coordinates (1,1). The lower left corner of the array will have coordinates (0.5,0.5)
                    point.x -= 0.5;
                    point.y -= 0.5;

                    // console.log("x:", point.x, "y:", point.y);
                }
            }

            let stat = display_points(points);

            if (stat != points.length)
                alert("ds9 import: some points are outside the image boundaries. Is the region file correct?");
        }
    });

    reader.readAsText(file);
}

function display_points(points) {
    console.log("points:", points);

    let fitsData = fitsContainer[va_count - 1];

    if (fitsData == null)
        return 0;

    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;

    var svg = d3.select("#BackSVG");

    // remove the existing "ds9region" group
    try {
        d3.select("#ds9region").remove();
    }
    catch (e) {
    }

    // override the prior displayRegion value
    displayRegion = true;

    let opacity = displayRegion ? 1 : 0;
    var group = svg.append("g")
        .attr("id", "ds9region")
        .attr("opacity", opacity);

    // get the element with id = "image_rectangle"
    let rect = d3.select("#image_rectangle");

    var ax = (image_bounding_dims.width - 1) / (parseFloat(rect.attr("width")) - 0);
    var ay = (image_bounding_dims.height - 1) / (parseFloat(rect.attr("height")) - 0);
    console.log("ax:", ax, "ay:", ay);

    let no_points = 0;

    for (let i = 0; i < points.length; i++) {
        let point = points[i];

        let orig_x = point.x;
        let orig_y = point.y;
        let shape = point.shape;
        console.log("orig_x:", orig_x, "orig_y:", orig_y, "shape:", shape);

        if (orig_x < 0 || orig_x > fitsData.width || orig_y < 0 || orig_y > fitsData.height)
            continue;

        // go from orig_x, orig_y to the mouse coordinates
        let x = orig_x * (imageContainer[va_count - 1].width - 1) / (fitsData.width - 1);
        let y = orig_y * (imageContainer[va_count - 1].height - 1) / (fitsData.height - 1);
        console.log("x:", x, "y:", y);

        let mx = parseFloat(rect.attr("x")) + (x - image_bounding_dims.x1) / ax;
        let my = parseFloat(rect.attr("y")) - (y - image_bounding_dims.y1 - image_bounding_dims.height + 1) / ay;
        console.log("orig_x:", orig_x, "orig_y:", orig_y, "mx:", mx, "my:", my);

        // place a circle at the mouse coordinates
        if (shape == "circle") {
            group.append("circle")
                .attr("cx", mx)
                .attr("cy", my)
                .attr("r", emFontSize / 4)
                .attr("fill", "none")
                .attr("stroke", "red")
                .attr("stroke-width", 2)
                .attr("opacity", 0.5)
                .attr("pointer-events", "none");
        }

        // make an 'X' mark with the centre at mx, my
        if (shape == "point") {
            group.append("line")
                .attr("x1", mx - emFontSize / 4)
                .attr("y1", my - emFontSize / 4)
                .attr("x2", mx + emFontSize / 4)
                .attr("y2", my + emFontSize / 4)
                .attr("stroke", "red")
                .attr("stroke-width", 2)
                .attr("opacity", 0.5)
                .attr("pointer-events", "none");

            group.append("line")
                .attr("x1", mx - emFontSize / 4)
                .attr("y1", my + emFontSize / 4)
                .attr("x2", mx + emFontSize / 4)
                .attr("y2", my - emFontSize / 4)
                .attr("stroke", "red")
                .attr("stroke-width", 2)
                .attr("opacity", 0.5)
                .attr("pointer-events", "none");
        }

        no_points++;
    }

    document.getElementById("displayRegion").style.display = "block";
    var htmlStr = displayRegion ? '<span class="fas fa-check-square"></span> ds9 region' : '<span class="far fa-square"></span> ds9 region';
    d3.select("#displayRegion").html(htmlStr);

    return no_points;
}

function show_fits_header() {
    hide_navigation_bar();

    $("#fitsHeader").modal("show");

    var modal = document.getElementById('fitsHeader');
    var span = document.getElementById('fitsHeaderClose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $("#fitsHeader").modal("hide");
    }
    // When the user clicks anywhere outside of the modal, close it
    window.onclick = function (event) {
        if (event.target == modal) {
            $("#fitsHeader").modal("hide");
        }
    }
}

function change_image_quality() {
    image_quality = document.getElementById('image_quality').value;
    localStorage.setItem("image_quality", image_quality);

    display_hourglass();

    if (va_count == 1) {
        fetch_image_spectrum(datasetId, 1, false, false);
    } else {
        for (let index = 1; index <= va_count; index++)
            fetch_image_spectrum(datasetId[index - 1], index, false, false);
    }
}

function change_intensity_threshold(refresh) {
    displayIntensity = parseFloat(document.getElementById('intensity').value);

    var htmlStr = displayIntensity.toFixed(1);

    if (displayIntensity == 0)
        htmlStr = "-" + htmlStr;

    d3.select("#intVal").html(htmlStr);

    if (refresh) {
        console.log("displayIntensity:", displayIntensity);
        localStorage.setItem("displayIntensity", displayIntensity);
        display_molecules();
    }
}

function hide_navigation_bar() {
    try {
        // d3 select all elements with class "dropdown-menu" and set their display to "none"
        d3.selectAll(".dropdown-menu").style("display", "none");
    } catch (e) { }

    try {
        document.getElementById('menu').style.display = "none";
        d3.select("#menu_activation_area").attr("opacity", 0.1);//was 0.7
    } catch (e) { }
}

function display_menu() {
    var div = d3.select("body").append("div")
        .attr("id", "menu")
        .attr("class", "menu");

    var nav = div.append("nav").attr("class", "navbar navbar-inverse navbar-fixed-top fixed-top navbar-expand-sm navbar-dark");

    var main = nav.append("div")
        .attr("class", "container-fluid");

    var header = main.append("div")
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
        })
        .attr("class", "navbar-header");

    header.append("a")
        .attr("href", "https://www.nao.ac.jp/")
        .append("img")
        .attr("class", "navbar-left")
        .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/logo_naoj_nothing_s.png")
        .attr("alt", "NAOJ")
        .attr("max-height", "100%")
        .attr("height", 50);//2.5*emFontSize);//50

    var mainUL = main.append("ul")
        .attr("class", "nav navbar-nav");

    //FITS
    var fitsMenu = mainUL.append("li")
        .attr("class", "dropdown");

    fitsMenu.append("a")
        .attr("class", "dropdown-toggle")
        .attr("data-toggle", "dropdown")
        .style('cursor', 'pointer')
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
            d3.select('#fitsDropdown').style("display", "block");
        })
        .html('FITS <span class="fas fa-folder-open"></span> <span class="caret"></span>');

    var fitsDropdown = fitsMenu.append("ul")
        .attr("id", "fitsDropdown")
        .attr("class", "dropdown-menu");

    fitsDropdown.append("li")
        .append("a")
        .style('cursor', 'pointer')
        .on("click", show_fits_header)
        .html('display header');

    fitsDropdown.append("li")
        .append("a")
        .html('<label id="regionLabel" style="cursor:pointer;font-weight:normal">import ds9 region<input type="file" accept=".reg, .REG" id="regionFile" style="display:none;" onclick="javascript:hide_navigation_bar();" onchange="javascript:load_region();"/></label>');

    if (!isLocal && va_count == 1 && (window.location.search.indexOf('ALMA') > 0 || window.location.search.indexOf('ALMB') > 0 || window.location.search.indexOf('FGN') > 0 || window.location.search.indexOf('CMG') > 0 || window.location.search.indexOf('SFP') > 0 || window.location.search.indexOf('NROA') > 0)) {
        var url = "";

        if (window.location.search.indexOf('ALMA') > 0 || window.location.search.indexOf('ALMB') > 0) {
            // ALMA
            if (datasetId.localeCompare("ALMA01000000") < 0)
                url = "http://jvo.nao.ac.jp/portal/alma/sv.do?action=download.fits&dataId=";
            else
                url = "http://jvo.nao.ac.jp/portal/alma/archive.do?action=download.fits&dataId=";

            url += datasetId + '_00_00_00';
        } else if (window.location.search.indexOf('FGN') > 0) {
            // FUGIN
            url = "http://jvo.nao.ac.jp/portal/nobeyama/fugin/download.do?action=download.fits&dataId=" + datasetId;
        } else if (window.location.search.indexOf('CMG') > 0) {
            // COMING
            url = "http://jvo.nao.ac.jp/portal/nobeyama/coming/download.do?action=download.fits&dataId=" + datasetId;
        } else if (window.location.search.indexOf('SFP') > 0) {
            // SFP
            url = "http://jvo.nao.ac.jp/portal/nobeyama/sfp/download.do?action=download.fits&dataId=" + datasetId;
        } else if (window.location.search.indexOf('NROA') > 0) {
            // NRO45M
            url = "http://jvo.nao.ac.jp/portal/nobeyama/archive/download.do?action=download.fits&dataId=" + datasetId;
        }

        fitsDropdown.append("li")
            .append("a")
            .attr("id", "FITS")
            .attr("href", url)
            .html('full FITS download <span class="fas fa-save"></span>');
    } else {
        let filename = datasetId;

        console.log("filename:", filename);

        // check if filename is a string (not an array)
        if (typeof filename === 'string' || filename instanceof String) {
            // check if a filename already ends with ".fits"; if not then append ".fits"
            if (!filename.endsWith(".fits"))
                filename += ".fits";
        }

        var _url = "get_fits?";

        if (va_count == 1) {
            _url += "datasetId=" + encodeURIComponent(datasetId);
            _url += "&filename=" + encodeURIComponent(filename);
        } else {
            for (let index = 1; index <= va_count; index++)
                _url += "datasetId" + index + "=" + encodeURIComponent(datasetId[index - 1]) + "&";
        }

        fitsDropdown.append("li")
            .append("a")
            .attr("id", "FITS")
            .attr("href", _url)
            .attr("target", "_blank")
            .attr('download', '')
            .html('full FITS download <span class="fas fa-save"></span>');
    }

    //IMAGE
    var imageMenu = mainUL.append("li")
        .attr("id", "imageMenu")
        .attr("class", "dropdown");

    imageMenu.append("a")
        .attr("class", "dropdown-toggle")
        .attr("data-toggle", "dropdown")
        .style('cursor', 'pointer')
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
            d3.select('#imageDropdown').style("display", "block");
        })
        .html('Image <span class="caret"></span>');

    var imageDropdown = imageMenu.append("ul")
        .attr("id", "imageDropdown")
        .attr("class", "dropdown-menu");
    //.style("background-color", "rgba(0,0,0,0.4)");

    //PREFERENCES
    var prefMenu = mainUL.append("li")
        .attr("class", "dropdown");

    prefMenu.append("a")
        .attr("class", "dropdown-toggle")
        .attr("data-toggle", "dropdown")
        .style('cursor', 'pointer')
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
            d3.select('#prefDropdown').style("display", "block");
        })
        .html('Preferences <span class="caret"></span>');

    var prefDropdown = prefMenu.append("ul")
        .attr("id", "prefDropdown")
        .attr("class", "dropdown-menu");

    //SPLATALOGUE
    if (!optical_view) {
        var splatMenu = mainUL.append("li")
            .attr("id", "splatMenu")
            .attr("class", "dropdown");

        splatMenu.append("a")
            .attr("class", "dropdown-toggle")
            .attr("data-toggle", "dropdown")
            .style('cursor', 'pointer')
            .on("mouseenter", function () {
                // d3 select all elements with class "dropdown-menu" and set their display to "none"
                d3.selectAll(".dropdown-menu").style("display", "none");
                d3.select('#splatDropdown').style("display", "block");
            })
            .html('Splatalogue <span class="caret"></span>');

        var splatDropdown = splatMenu.append("ul")
            .attr("id", "splatDropdown")
            .attr("class", "dropdown-menu");

        splatDropdown.append("li")
            .append("a")
            .html('<label>intensity cutoff < <span id="intVal">' + displayIntensity.toFixed(1) + '</span> <input id="intensity" class="slider" type="range" min="-10" max="0" step="0.1" value="' + displayIntensity + '" onmousemove="javascript:change_intensity_threshold(false);" onchange="javascript:change_intensity_threshold(true);"/></label>');

        splatDropdown.append("li")
            .append("a")
            .html('<label>&nbsp;search for:&nbsp;<input class="form-control search" type="text" id="searchInput" value="" placeholder="water, H2O, CH3 ..." onmouseenter="javascript:this.focus();"/></label>');

        //add onblur
        var m = document.getElementById('searchInput');
        m.onblur = display_molecules;
        m.onmouseleave = display_molecules;
        m.onkeyup = function (e) {
            var event = e || window.event;
            var charCode = event.which || event.keyCode;

            if (charCode == '13') {
                // Enter pressed
                clearTimeout(idleSearch);
                display_molecules();
                return false;
            } else {
                clearTimeout(idleSearch);
                idleSearch = setTimeout(display_molecules, 250);
            }
        }

        var htmlStr;

        htmlStr = displayCDMS ? '<span class="fas fa-check-square"></span> CDMS' : '<span class="far fa-square"></span> CDMS';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayCDMS = !displayCDMS;
                localStorage_write_boolean("displayCDMS", displayCDMS);
                var htmlStr = displayCDMS ? '<span class="fas fa-check-square"></span> CDMS' : '<span class="far fa-square"></span> CDMS';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayJPL ? '<span class="fas fa-check-square"></span> JPL' : '<span class="far fa-square"></span> JPL';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayJPL = !displayJPL;
                localStorage_write_boolean("displayJPL", displayJPL);
                var htmlStr = displayJPL ? '<span class="fas fa-check-square"></span> JPL' : '<span class="far fa-square"></span> JPL';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayLovas ? '<span class="fas fa-check-square"></span> Lovas' : '<span class="far fa-square"></span> Lovas';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayLovas = !displayLovas;
                localStorage_write_boolean("displayLovas", displayLovas);
                var htmlStr = displayLovas ? '<span class="fas fa-check-square"></span> Lovas' : '<span class="far fa-square"></span> Lovas';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayOSU ? '<span class="fas fa-check-square"></span> OSU' : '<span class="far fa-square"></span> OSU';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayOSU = !displayOSU;
                localStorage_write_boolean("displayOSU", displayOSU);
                var htmlStr = displayOSU ? '<span class="fas fa-check-square"></span> OSU' : '<span class="far fa-square"></span> OSU';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayRecomb ? '<span class="fas fa-check-square"></span> Recomb' : '<span class="far fa-square"></span> Recomb';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayRecomb = !displayRecomb;
                localStorage_write_boolean("displayRecomb", displayRecomb);
                var htmlStr = displayRecomb ? '<span class="fas fa-check-square"></span> Recomb' : '<span class="far fa-square"></span> Recomb';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displaySLAIM ? '<span class="fas fa-check-square"></span> SLAIM' : '<span class="far fa-square"></span> SLAIM';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displaySLAIM = !displaySLAIM;
                localStorage_write_boolean("displaySLAIM", displaySLAIM);
                var htmlStr = displaySLAIM ? '<span class="fas fa-check-square"></span> SLAIM' : '<span class="far fa-square"></span> SLAIM';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayTopModel ? '<span class="fas fa-check-square"></span> TopModel' : '<span class="far fa-square"></span> TopModel';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayTopModel = !displayTopModel;
                localStorage_write_boolean("displayTopModel", displayTopModel);
                var htmlStr = displayTopModel ? '<span class="fas fa-check-square"></span> TopModel' : '<span class="far fa-square"></span> TopModel';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        htmlStr = displayToyaMA ? '<span class="fas fa-check-square"></span> ToyaMA' : '<span class="far fa-square"></span> ToyaMA';
        splatDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayToyaMA = !displayToyaMA;
                localStorage_write_boolean("displayToyaMA", displayToyaMA);
                var htmlStr = displayToyaMA ? '<span class="fas fa-check-square"></span> ToyaMA' : '<span class="far fa-square"></span> ToyaMA';
                d3.select(this).html(htmlStr);
                display_molecules();
            })
            .html(htmlStr);

        var elem = document.getElementById("splatMenu");
        if (displayMolecules)
            elem.style.display = "block";
        else
            elem.style.display = "none";
    }

    // Atomic Spectra Database (NIST)
    {
        var atomicMenu = mainUL.append("li")
            .attr("id", "atomicMenu")
            .attr("class", "dropdown");

        atomicMenu.append("a")
            .attr("class", "dropdown-toggle")
            .attr("data-toggle", "dropdown")
            .style('cursor', 'pointer')
            .on("mouseenter", function () {
                // d3 select all elements with class "dropdown-menu" and set their display to "none"
                d3.selectAll(".dropdown-menu").style("display", "none");
                d3.select('#atomicDropdown').style("display", "block");
            })
            .html('Atomic Spectra (NIST)<span class="caret"></span>');


        var atomicDropdown = atomicMenu.append("ul")
            .attr("id", "atomicDropdown")
            .attr("class", "dropdown-menu");

        let tmp = atomicDropdown.append("li")
            .attr("id", "redshift_li")
            .append("a")
            .style("class", "form-group")
            .attr("class", "form-horizontal");

        tmp.append("label")
            .attr("for", "redshift")
            .attr("class", "control-label")
            .html("redshift&nbsp;");

        previous_redshift = redshift;

        // <input type="radio" id="velV" name="velocity" value="v" style="vertical-align: middle; margin: 0px;" onclick="javascript:toggle_redshift_input_source(this);">
        tmp.append("input")
            .attr("id", "velV")
            .attr("type", "radio")
            .attr("name", "velocity")
            .attr("value", "v")
            .style("vertical-align", "middle")
            .style("margin", "0px")
            .on("click", function () {
                toggle_redshift(this);
            });

        // v&nbsp;
        tmp.append("span")
            .on("click", function () {
                // switch on velV and toggle redshift
                let elem = document.getElementById('velV');
                elem.checked = true;
                toggle_redshift(elem);
            })
            .html("&nbsp;v&nbsp;");

        // <input type="radio" id="velZ" name="velocity" value="z" style="vertical-align: middle; margin: 0px;" onclick="javascript:toggle_redshift_input_source(this);">
        tmp.append("input")
            .attr("id", "velZ")
            .attr("type", "radio")
            .attr("name", "velocity")
            .attr("value", "z")
            .style("vertical-align", "middle")
            .style("margin", "0px")
            .on("click", function () {
                toggle_redshift(this);
            });

        // z&nbsp;
        tmp.append("span")
            .on("click", function () {
                // switch on velZ and toggle redshift
                let elem = document.getElementById('velZ');
                elem.checked = true;
                toggle_redshift(elem);
            })
            .html("&nbsp;z&nbsp;");

        // &nbsp;
        tmp.append("span")
            .html("&nbsp;");

        tmp.append("input")
            .attr("id", "redshift")
            .attr("type", "number")
            .style("width", "7em")
            .attr("min", -0.9)
            .attr("step", 0.0001)
            .attr("value", previous_redshift.toFixed(1));

        // &nbsp;
        tmp.append("span")
            .html("&nbsp;&nbsp;");

        // <span id="unit">km/s</span>
        tmp.append("span")
            .attr("id", "unit")
            .html("km/s");

        var elem = document.getElementById('redshift');
        elem.onblur = validate_redshift;
        elem.onmouseleave = validate_redshift;
        elem.onkeyup = function (e) {
            var event = e || window.event;

            if (event.key === 'Enter') {
                // Enter pressed
                validate_redshift();
                return false;
            }
        };
        elem.onchange = function () {
            clearTimeout(idleResize);

            idleResize = setTimeout(function () {
                validate_redshift();
            }, 250);
        };

        if (sessionStorage.getItem("redshift") === null) {
            // make velV checked by default
            document.getElementById('velV').setAttribute("checked", "");
            //d3.select("#velV").property("checked", true);

            // set the elem "min", "max" and "step" attributes
            elem.setAttribute("min", -299792);
            elem.setAttribute("max", 299792);
            elem.setAttribute("step", 1);

            console.log("setting redshift to v in the sessionStorage");
            sessionStorage.setItem("redshift", "v");
        }
        else {
            let unit = document.getElementById('unit');
            let value = sessionStorage.getItem("redshift"); // either 'v' or 'z'
            console.log("sessionStorage redshift value:", value);

            // use upper case value
            document.getElementById('vel' + value.toUpperCase()).setAttribute("checked", "");

            if (value == "v") {
                unit.innerHTML = "km/s";

                // set the elem "min", "max" and "step" attributes
                elem.setAttribute("min", -299792);
                elem.setAttribute("max", 299792);
                elem.setAttribute("step", 1);
            }

            if (value == "z") {
                unit.innerHTML = "(z > -1)";

                // set the elem "min" and "step" attributes
                elem.setAttribute("min", -0.9);
                elem.setAttribute("step", 0.0001);
            }
        }
    }

    //VIEW
    var viewMenu = mainUL.append("li")
        .attr("id", "viewMenu")
        .attr("class", "dropdown");

    viewMenu.append("a")
        .attr("class", "dropdown-toggle")
        .attr("data-toggle", "dropdown")
        .style('cursor', 'pointer')
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
            d3.select('#viewDropdown').style("display", "block");
        })
        .html('View <span class="caret"></span>');

    var viewDropdown = viewMenu.append("ul")
        .attr("id", "viewDropdown")
        .attr("class", "dropdown-menu");

    if (has_webgl) {
        if (va_count == 1 || composite_view) {
            var htmlStr = '<i class="material-icons">3d_rotation</i> 3D surface';
            viewDropdown.append("li")
                .append("a")
                .style('cursor', 'pointer')
                .on("click", function () {
                    hide_navigation_bar();
                    init_surface();

                })
                .html(htmlStr);
        }
    }
    else {
        viewDropdown.append("li")
            .append("a")
            .attr("disabled", "disabled")
            .style("font-style", "italic")
            .style('cursor', 'not-allowed')
            .html('<span class="fas fa-eye-slash"></span> WebGL not enabled, disabling 3D surface');
    }

    if (va_count > 1 && va_count <= 3) {
        htmlStr = composite_view ? '<span class="fas fa-check-square"></span> RGB composite mode' : '<span class="far fa-square"></span> RGB composite mode';
        viewDropdown.append("li")
            .append("a")
            .attr("id", "displayComposite")
            .style('cursor', 'pointer')
            .on("click", function () {
                composite_view = !composite_view;
                var htmlStr = composite_view ? '<span class="fas fa-check-square"></span> RGB composite mode' : '<span class="far fa-square"></span> RGB composite mode';
                d3.select(this).html(htmlStr);

                var loc = window.location.href.replace("&view=composite", "");

                if (composite_view)
                    window.location.replace(loc + "&view=composite");
                else
                    window.location.replace(loc);

                // force a reload
                //window.location.reload();

                /*var new_loc = window.location.href.replace("&view=", "&dummy=");

                if (composite_view && optical_view)
                  new_loc += "&view=composite,optical";
                else {
                  if (composite_view)
                    new_loc += "&view=composite";

                  if (optical_view)
                    new_loc += "&view=optical";
                }

                window.location.replace(new_loc);*/
            })
            .html(htmlStr);
    }

    if (va_count == 1 || composite_view) {
        htmlStr = displayContours ? '<span class="fas fa-check-square"></span> contour lines' : '<span class="far fa-square"></span> contour lines';
        viewDropdown.append("li")
            .append("a")
            .attr("id", "displayContours")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayContours = !displayContours;
                var htmlStr = displayContours ? '<span class="fas fa-check-square"></span> contour lines' : '<span class="far fa-square"></span> contour lines';
                d3.select(this).html(htmlStr);
                //var elem = d3.selectAll("#contourPlot");

                if (displayContours) {
                    d3.select('#contour_control_li').style("display", "block");
                }
                else {
                    d3.select('#contour_control_li').style("display", "none");
                }

                if (displayContours) {
                    document.getElementById("ContourSVG").style.display = "block";
                    //elem.attr("opacity",1);

                    //if(document.getElementById('contourPlot') == null)
                    if (!has_contours)
                        update_contours();
                }
                else {
                    document.getElementById("ContourSVG").style.display = "none";
                    //elem.attr("opacity",0);
                }
            })
            .html(htmlStr);
    }

    if (va_count == 1 || composite_view) {
        htmlStr = displayRegion ? '<span class="fas fa-check-square"></span> ds9 region' : '<span class="far fa-square"></span> ds9 region';
        viewDropdown.append("li")
            .append("a")
            .attr("id", "displayRegion")
            .style('display', 'none')
            .style('cursor', 'pointer')
            .on("click", function () {
                displayRegion = !displayRegion;
                var htmlStr = displayRegion ? '<span class="fas fa-check-square"></span> ds9 region' : '<span class="far fa-square"></span> ds9 region';
                d3.select(this).html(htmlStr);
                var elem = d3.selectAll("#ds9region");

                if (displayRegion) {
                    elem.attr("opacity", 1);
                }
                else {
                    elem.attr("opacity", 0);
                }
            })
            .html(htmlStr);
    }

    if (va_count == 1 || composite_view) {
        htmlStr = displayGridlines ? '<span class="fas fa-check-square"></span> lon/lat grid lines' : '<span class="far fa-square"></span> lon/lat grid lines';
        viewDropdown.append("li")
            .append("a")
            .attr("id", "displayGridlines")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayGridlines = !displayGridlines;
                localStorage_write_boolean("displayGridlines", displayGridlines);
                var htmlStr = displayGridlines ? '<span class="fas fa-check-square"></span> lon/lat grid lines' : '<span class="far fa-square"></span> lon/lat grid lines';
                d3.select(this).html(htmlStr);
                var elem = d3.select("#gridlines");
                if (displayGridlines)
                    elem.attr("opacity", 1);
                else
                    elem.attr("opacity", 0);
            })
            .html(htmlStr);

        htmlStr = displayLegend ? '<span class="fas fa-check-square"></span> image legend' : '<span class="far fa-square"></span> image legend';
        viewDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayLegend = !displayLegend;
                localStorage_write_boolean("displayLegend", displayLegend);
                var htmlStr = displayLegend ? '<span class="fas fa-check-square"></span> image legend' : '<span class="far fa-square"></span> image legend';
                d3.select(this).html(htmlStr);

                if (va_count == 1) {
                    var elem = d3.select("#legend");

                    if (displayLegend) {
                        elem.attr("opacity", 1);
                        document.getElementById('LegendCanvas').style.display = "block";
                    }
                    else {
                        elem.attr("opacity", 0);
                        document.getElementById('LegendCanvas').style.display = "none";
                    }
                }
                else {
                    for (let index = 1; index <= va_count; index++) {
                        var elem = d3.select("#legend" + index);

                        if (displayLegend)
                            elem.attr("opacity", 1);
                        else
                            elem.attr("opacity", 0);
                    }
                }
            })
            .html(htmlStr);
    }

    if (!optical_view) {
        htmlStr = displayMolecules ? '<span class="fas fa-check-square"></span> spectral lines' : '<span class="far fa-square"></span> spectral lines';
        viewDropdown.append("li")
            .append("a")
            .attr("id", "displayMolecules")
            .style('cursor', 'pointer')
            .on("click", function () {
                displayMolecules = !displayMolecules;
                localStorage_write_boolean("displayMolecules", displayMolecules);
                var htmlStr = displayMolecules ? '<span class="fas fa-check-square"></span> spectral lines' : '<span class="far fa-square"></span> spectral lines';
                d3.select(this).html(htmlStr);
                var elem = d3.select("#molecules");
                if (displayMolecules)
                    elem.attr("opacity", 1);
                else
                    elem.attr("opacity", 0);

                var elem = document.getElementById("splatMenu");
                if (displayMolecules)
                    elem.style.display = "block";
                else
                    elem.style.display = "none";
            })
            .html(htmlStr);

        htmlStr = displaySpectrum ? '<span class="fas fa-check-square"></span> spectrum' : '<span class="far fa-square"></span> spectrum';
        viewDropdown.append("li")
            .append("a")
            .style('cursor', 'pointer')
            .on("click", function () {
                displaySpectrum = !displaySpectrum;
                localStorage_write_boolean("displaySpectrum", displaySpectrum);
                var htmlStr = displaySpectrum ? '<span class="fas fa-check-square"></span> spectrum' : '<span class="far fa-square"></span> spectrum';
                d3.select(this).html(htmlStr);
                var elem = document.getElementById("SpectrumCanvas");
                if (displaySpectrum) {
                    elem.style.display = "block";
                    d3.select("#yaxis").attr("opacity", 1);
                    d3.select("#ylabel").attr("opacity", 1);
                }
                else {
                    elem.style.display = "none";
                    d3.select("#yaxis").attr("opacity", 0);
                    d3.select("#ylabel").attr("opacity", 0);
                }
            })
            .html(htmlStr);

        if (va_count == 1 || composite_view) {
            htmlStr = displayBeam ? '<span class="fas fa-check-square"></span> telescope beam' : '<span class="far fa-square"></span> telescope beam';
            viewDropdown.append("li")
                .append("a")
                .attr("id", "displayBeam")
                .style('cursor', 'pointer')
                .on("click", function () {
                    displayBeam = !displayBeam;
                    var htmlStr = displayBeam ? '<span class="fas fa-check-square"></span> telescope beam' : '<span class="far fa-square"></span> telescope beam';
                    d3.select(this).html(htmlStr);

                    if (displayBeam) {
                        d3.select("#beam").attr("opacity", 1);
                        d3.select("#zoomBeam").attr("opacity", 1);
                    }
                    else {
                        d3.select("#beam").attr("opacity", 0);
                        d3.select("#zoomBeam").attr("opacity", 0);
                    }
                })
                .html(htmlStr);
        }
    }

    //HELP
    var rightUL = main.append("ul")
        .attr("class", "nav navbar-nav navbar-right");

    var helpMenu = rightUL.append("li")
        .attr("class", "dropdown");

    helpMenu.append("a")
        .attr("class", "dropdown-toggle")
        .attr("data-toggle", "dropdown")
        .style('cursor', 'pointer')
        .on("mouseenter", function () {
            // d3 select all elements with class "dropdown-menu" and set their display to "none"
            d3.selectAll(".dropdown-menu").style("display", "none");
            d3.select('#helpDropdown').style("display", "block");
        })
        .html('<span class="fas fa-question-circle"></span> Help <span class="caret"></span>');

    var helpDropdown = helpMenu.append("ul")
        .attr("id", "helpDropdown")
        .attr("class", "dropdown-menu");

    helpDropdown.append("li")
        .append("a")
        .style('cursor', 'pointer')
        .on("click", show_help)
        .html('<span class="fas fa-wrench"></span> user guide');

    helpDropdown.append("li")
        .append("a")
        .style('cursor', 'pointer')
        .on("click", show_changelog)
        .html('<span class="fas fa-newspaper"></span> change log');

    helpDropdown.append("li")
        .append("a")
        .attr("href", "mailto:help_desk@jvo.nao.ac.jp?subject=" + votable.getAttribute('data-server-string') + " feedback [" + votable.getAttribute('data-server-version') + "/" + get_js_version() + "]")
        .html('<span class="fas fa-comment-dots"></span> send feedback');

    helpDropdown.append("li")
        .append("a")
        .style("color", "#336699")
        .html("[" + votable.getAttribute('data-server-version') + "/" + get_js_version() + "]");
}

function show_help() {
    hide_navigation_bar();
    $("#help").modal("show");

    var modal = document.getElementById('help');
    var span = document.getElementById('helpclose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $("#help").modal("hide");
    }
    // When the user clicks anywhere outside of the modal, close it
    window.onclick = function (event) {
        if (event.target == modal) {
            $("#help").modal("hide");
        }
    }
}

function show_changelog() {
    hide_navigation_bar();
    $("#changelog").modal("show");

    var modal = document.getElementById('changelog');
    var span = document.getElementById('changelogclose');

    // When the user clicks on <span> (x), close the modal
    span.onclick = function () {
        $("#changelog").modal("hide");
    }
    // When the user clicks anywhere outside of the modal, close it
    window.onclick = function (event) {
        if (event.target == modal) {
            $("#changelog").modal("hide");
        }
    }
}

function donotshow() {
    var checkbox = document.getElementById('donotshowcheckbox');

    localStorage_write_boolean("welcome_v5_alpha", !checkbox.checked);
};

function show_timeout() {
    try {
        $('#welcomeScreen').modal('hide');
    }
    catch (e) { };

    var div = d3.select("body")
        .append("div")
        .attr("class", "container timeout");

    div.append("h1")
        .style("margin-top", "20%")
        .attr("align", "center")
        .text("60 min. inactivity time-out");

    div.append("h2")
        .attr("align", "center")
        .text("PLEASE RELOAD THE PAGE");

    close_websocket_connections();
}

function show_critical_error() {
    try {
        $('#welcomeScreen').modal('hide');
    }
    catch (e) { };

    var div = d3.select("body")
        .append("div")
        .attr("class", "container timeout");

    div.append("h1")
        .style("margin-top", "20%")
        .style("color", "red")
        .attr("align", "center")
        .text("CRITICAL ERROR");

    div.append("h2")
        .attr("align", "center")
        //.style("color", "red")
        .append("a")
        .attr("class", "links")
        .attr("href", "mailto:help_desk@jvo.nao.ac.jp?subject=" + votable.getAttribute('data-server-string') + " error [" + votable.getAttribute('data-server-version') + "/" + get_js_version() + "]&body=Error accessing " + datasetId)
        .html('PLEASE INFORM AN ADMINISTRATOR');

    close_websocket_connections();
}

function show_unsupported_media_type() {
    try {
        $('#welcomeScreen').modal('hide');
    }
    catch (e) { };

    var div = d3.select("body")
        .append("div")
        .attr("class", "container timeout");

    div.append("h1")
        .style("margin-top", "25%")
        .style("color", "red")
        .attr("align", "center")
        .text("UNSUPPORTED MEDIA TYPE");

    div.append("h2")
        .attr("align", "center")
        //.style("color", "red")
        .text("FITSWEBQL SUPPORTS ONLY FITS DATA");

    close_websocket_connections();
}

function show_not_found() {
    try {
        $('#welcomeScreen').modal('hide');
    }
    catch (e) { };

    var div = d3.select("body")
        .append("div")
        .attr("class", "container timeout");

    div.append("h1")
        .style("margin-top", "20%")
        .style("color", "red")
        .attr("align", "center")
        .text("DATA NOT FOUND ON THE REMOTE SITE");

    div.append("h2")
        .attr("align", "center")
        //.style("color", "red")
        .text("THE FITS FILE CANNOT BE FOUND");

    div.append("h2")
        .attr("align", "center")
        //.style("color", "red")
        .text("AND/OR");

    div.append("h2")
        .attr("align", "center")
        //.style("color", "red")
        .text("THE REMOTE URL MAY BE INCORRECT/OUT-OF-DATE");

    close_websocket_connections();
}

function show_welcome() {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "welcomeScreen")
        .attr("class", "modal modal-center")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog modal-dialog-centered");

    var contentDiv = div.append("div")
        .attr("class", "modal-content");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("button")
        .attr("type", "button")
        .attr("data-dismiss", "modal")
        .attr("id", "welcomeclose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    headerDiv.append("h2")
        .attr("align", "center")
        .html('WELCOME TO FITSWEBQLSE');

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    bodyDiv.append("h3")
        .text("FEATURES");

    var ul = bodyDiv.append("ul")
        .attr("class", "list-group");

    sv = votable.getAttribute('data-server-version');

    if (sv.charAt(0) == 'F') {
        ul.append("li")
            .attr("class", "list-group-item list-group-item-success")
            .html("<h4>FORTRAN (computing) &amp; C (networking)</h4>");
    };

    if (sv.charAt(0) == 'J') {
        ul.append("li")
            .attr("class", "list-group-item list-group-item-success")
            .html("<h4>Server powered by Julia (distributed computing &amp; networking)</h4>");
    };

    ul.append("li")
        .attr("class", "list-group-item list-group-item-success")
        .html('<h4>Realtime Position-Velocity Diagram (see <a id="pv_help" href="#"><i>Help / User Guide / P-V Diagram</i></a>)</h4>');

    $('#pv_help').click(function () {
        $('#welcomeScreen').modal('hide');
        show_help();
        return false;
    });

    ul.append("li")
        .attr("class", "list-group-item list-group-item-success")
        .html('<h4>CSV spectrum export (see <a id="csv_help" href="#"><i>Help / User Guide / Spectrum Export</i></a>)</h4>');

    $('#csv_help').click(function () {
        $('#welcomeScreen').modal('hide');
        show_help();
        return false;
    });

    let src = 'https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@' + votable.getAttribute('data-version-major') + '.' + votable.getAttribute('data-version-minor') + '.' + votable.getAttribute('data-version-sub') + '/CHANGELOG.md';
    let html = '<zero-md src="' + src + '">';

    if (theme == 'bright') {
        html += '<template><style>* { color:gray;font-size:small;font-family: Helvetica;}</style></template>';
    }

    if (theme == 'dark') {
        html += '<template><style>* { color:lightgray;font-size:small;font-family:Helvetica;}</style></template>';
    }

    html += '</zero-md>';

    ul.append("li")
        .attr("class", "list-group-item list-group-item-success changelog1")
        .html(html);

    if (!isLocal) {
        ul.append("li")
            .attr("class", "list-group-item list-group-item-success")
            .html('<h4>source code: <a href="https://github.com/jvo203/FITSWEBQLSE"><em>https://github.com/jvo203/FITSWEBQLSE</em></a></h4>');
    }

    bodyDiv.append("h3")
        .text("Browser recommendation");

    let textColour = 'yellow';

    if (theme == 'bright')
        textColour = 'red';

    if (!wasm_supported) {
        bodyDiv.append("p")
            .html('A modern browser with <a href="https://en.wikipedia.org/wiki/WebAssembly" style="color:' + textColour + '"><b>WebAssembly (Wasm)</b></a> support is required.');
    }

    bodyDiv.append("p")
        .html('For optimum experience we recommend  <a href="https://www.apple.com/safari/" style="color:' + textColour + '"><b>Apple Safari</b></a> or <a href="https://www.google.com/chrome/index.html" style="color:' + textColour + '"><b>Google Chrome</b></a>.');

    var footer = contentDiv.append("div")
        .attr("class", "modal-footer d-flex justify-content-around");

    /*footer.append("button")
    .attr("type", "button")
    .attr("data-dismiss", "modal")
      .attr("class", "button btn-lg pull-right")
    .attr("align","center")
    .html('<span class="fas fa-times"></span> Close') ;*/

    var href = "mailto:help_desk@jvo.nao.ac.jp?subject=" + votable.getAttribute('data-server-string') + " bug report [" + votable.getAttribute('data-server-version') + "/" + get_js_version() + "]";

    footer.append("p")
        //.style("color", "#a94442")
        .attr("align", "left")
        .html('<label style="cursor: pointer"><input type="checkbox" value="" class="control-label" style="cursor: pointer" id="donotshowcheckbox" onchange="javascript:donotshow();">&nbsp;do not show this dialogue again</label>' + '&nbsp;&nbsp;&nbsp;<a style="color:red" href="' + href + '">page loading problems? </a>' + '<button type="submit" class="btn btn-danger btn-default pull-right" data-dismiss="modal"><span class="fas fa-times"></span> Close</button>');

    $('#welcomeScreen').modal('show');
}

function setup_help() {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "help")
        .attr("class", "modal fade")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var contentDiv = div.append("div")
        .attr("class", "modal-content");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("span")
        .attr("id", "helpclose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    headerDiv.append("h2")
        .text("FITSWEBQLSE HOW-TO");

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    bodyDiv.append("h3")
        .attr("id", "h3")
        .text("P-V Diagram");

    bodyDiv.append("p")
        .html("An interactive <b>Position-Velocity Diagram</b> can be displayed for the current image. First left-click on the image to select a starting position.");

    bodyDiv.append("p")
        .html("Then move a mouse around and left-click again to complete the <i>P-V line</i> selection. The <i>P-V diagram</i> will display shortly.");

    bodyDiv.append("p")
        .html("In the <b>P-V Diagram view</b> ①, ② and the middle point can be dragged freely to change the <i>P-V line</i>. The <i>P-V diagram</i> will be updated automatically.");

    var pv = bodyDiv.append("video")
        .attr("width", "100%")
        .attr("controls", "")
        .attr("preload", "none"); // metadata

    pv.append("source")
        .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE/htdocs/fitswebql/pv_diagram.mp4");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .attr("id", "csv_h3")
        .text("Spectrum Export");

    bodyDiv.append("p")
        .html("The current image/viewport spectrum can be exported to a <b>CSV</b> file");

    var csv = bodyDiv.append("video")
        .attr("width", "100%")
        .attr("controls", "")
        .attr("preload", "none"); // metadata

    csv.append("source")
        .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE/htdocs/fitswebql/spectrum_export.mp4");

    csv.append("p")
        .html("Your browser does not support the video tag.");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("3D View");

    bodyDiv.append("p")
        .html("An <span style=\"color:#a94442\">experimental</span> WebGL feature resulting in high memory consumption. After using it a few times a browser may run out of memory.");

    bodyDiv.append("p")
        .html("Reloading a page should fix the problem");

    /*bodyDiv.append("p")
    .html("To enable it check <i>Preferences</i>/<i>3D View (experimental)</i> and a \"3D View\" button should appear towards the bottom of the page") ;*/

    bodyDiv.append("p")
        .html("To view a 3D surface of the FITS cube image, click <i>3D surface</i> in the <i>View</i> menu");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .attr("id", "realtime_h3")
        .text("Realtime Spectrum Updates");

    bodyDiv.append("p")
        .html("<i>Preferences/realtime spectrum updates</i> works best over <i>low-latency</i> network connections");

    bodyDiv.append("p")
        .html("<i>Kalman Filter</i> is used to predict the mouse movement after taking into account a latency of a network connection to Japan");

    bodyDiv.append("p")
        .html("when disabled the spectrum refresh will be requested after a 250ms delay since the last movement of the mouse");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .attr("id", "h3")
        .text("Realtime FITS Cube Video Updates");

    bodyDiv.append("p")
        .html("<i>Preferences/realtime video updates</i> works best over <i>low-latency</i> network connections with available bandwidth <i>over 1 mbps</i>");

    bodyDiv.append("p")
        .html("when disabled the FITS cube video frame will be requested after a 250ms delay since the last movement of the mouse");

    bodyDiv.append("p")
        .html('<span class="fas fa-play"></span>&nbsp; replay period 10s');

    bodyDiv.append("p")
        .html('<span class="fas fa-forward"></span>&nbsp; replay period 5s');

    bodyDiv.append("p")
        .html('<span class="fas fa-fast-forward"></span>&nbsp; replay period 2.5s');

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Zoom In/Out of region");

    bodyDiv.append("p")
        .html("scroll mouse wheel up/down (<i>mouse</i>)");

    bodyDiv.append("p")
        .html("move two fingers up/down (<i>touchpad</i>)");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Copy RA/DEC");

    bodyDiv.append("p")
        .html("<b>Ctrl + C</b>");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Save region as FITS");

    bodyDiv.append("p")
        .html("<b>Ctrl + S</b> (<i>keyboard</i>)");

    bodyDiv.append("p")
        .html("drag over main image (<i>mouse</i>)");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Show Frequency/Velocity/Molecular Information");

    bodyDiv.append("p")
        .html("<b>hover</b> a mouse over X-axis");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Skip to the Next Molecular Line");

    bodyDiv.append("p")
        .html("press <b>&larr;</b> or <b>&rarr;</b> whilst <b>hovering</b> over X-axis");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Jump to Splatalogue");

    bodyDiv.append("p")
        .html("press <b>Enter</b> whilst <b>hovering</b> over X-axis");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Select Frequency Range");

    bodyDiv.append("p")
        .html("<b>drag</b> over X-axis");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Set REST Frequency");

    bodyDiv.append("p")
        .html("press <b>f</b> over X-axis; for detailed information see the&nbsp;")
        .append("a")
        .attr("class", "links")
        .attr("href", "relative velocity.pdf")
        .attr("target", "_blank")
        .style("target-new", "tab")
        .html("<u>relative velocity guide</u>");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Temporarily Fix Y-Axis Range");

    bodyDiv.append("p")
        .html("press <b>s</b> over main image");

    bodyDiv.append("h4")
        .text("adjust the fixed Y-Axis range");

    bodyDiv.append("p")
        .html("move mouse cursor over to the Y-Axis whilst holding the 「Shift」 key");

    bodyDiv.append("p")
        .html("drag the mouse over the Y-Axis to <i>shift</i> it <em>UP</em> and <em>DOWN</em>");

    bodyDiv.append("p")
        .html("use the mouse <i>scroll wheel</i> or a two-finger <i>touch gesture</i> to <i>re-scale</i> the Y-Axis range");

    var vid = bodyDiv.append("video")
        .attr("width", "100%")
        .attr("controls", "")
        .attr("preload", "none"); // metadata

    vid.append("source")
        .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/fixed_scale_y_axis.mp4");

    vid.append("p")
        .html("Your browser does not support the video tag.");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Hold current view region");

    bodyDiv.append("p")
        .html("keep pressing <b>↑Shift</b>");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("Print");

    bodyDiv.append("p")
        .html("in a browser <i>File/Print Preview</i>, adjust scale as needed (i.e. 25% or 50%)");

    bodyDiv.append("hr");

    bodyDiv.append("h3")
        .text("browser support:");

    bodyDiv.append("p")
        .text("Chrome ◯, Firefox ◯, Safari ◯, MS Edge ◯, IE11 ×");

    var footer = contentDiv.append("div")
        .attr("class", "modal-footer");

    if (!isLocal) {
        footer.append("h3")
            .text("FITSWebQL Personal Edition:");

        let textColour = 'yellow';

        if (theme == 'bright')
            textColour = 'red';

        footer.append("p")
            .html("A local version is available on GitHub: ")
            .append("a")
            .style("color", textColour)
            .attr("href", "https://github.com/jvo203/fits_web_ql")
            .attr("target", "_blank")
            .style("target-new", "tab")
            .html("<b>fits_web_ql installation instructions</b>");
    }

    footer.append("h3")
        .text("CREDITS:");

    footer.append("p")
        .text("Software Development Ⓒ Christopher A. Zapart @ NAOJ, 2015 - 2025. JavaScript RA/DEC conversion Ⓒ Robert Martin Ayers, 2009, 2011, 2014.");

    footer.append("h3")
        .text("VERSION:");

    footer.append("p")
        .text(votable.getAttribute('data-server-version') + "/" + get_js_version());
}

function setup_changelog() {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "changelog")
        .attr("class", "modal fade")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var contentDiv = div.append("div")
        .attr("class", "modal-content");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("span")
        .attr("id", "changelogclose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    headerDiv.append("h2")
        .text("FITSWEBQLSE CHANGELOG");

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    let src = 'https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@' + votable.getAttribute('data-version-major') + '.' + votable.getAttribute('data-version-minor') + '.' + votable.getAttribute('data-version-sub') + '/CHANGELOG.md';
    let html = '<zero-md src="' + src + '">';

    if (theme == 'bright') {
        html += '<template><style>* { color:gray;font-size:small;font-family: Helvetica;}</style></template>';
    }

    if (theme == 'dark') {
        html += '<template><style>* { color:lightgray;font-size:small;font-family:Helvetica;}</style></template>';
    }

    html += '</zero-md>';

    bodyDiv.append("p")
        .attr("class", "changelog2")
        .html(html);

    var footer = contentDiv.append("div")
        .attr("class", "modal-footer");

    footer.append("h4")
        .text("CREDITS:");

    footer.append("p")
        .text("Software Development Ⓒ Christopher A. Zapart @ NAOJ, 2015 - 2025. JavaScript RA/DEC conversion Ⓒ Robert Martin Ayers, 2009, 2011, 2014.");

    footer.append("h4")
        .text("VERSION:");

    footer.append("p")
        .text(votable.getAttribute('data-server-version') + "/" + get_js_version());
}

function setup_FITS_header_page() {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "fitsHeader")
        .attr("class", "modal fade")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var contentDiv = div.append("div")
        .attr("class", "modal-content");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("span")
        .attr("id", "fitsHeaderClose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    var title = headerDiv.append("h3")
        .text("FITS HEADER");

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    if (va_count > 1) {
        var ul = bodyDiv.append("ul")
            .attr("class", "nav nav-tabs");

        for (let index = 1; index <= va_count; index++) {
            let classStr = '';

            if (index == 1)
                classStr = 'active';

            var li = ul.append("li")
                .attr("class", classStr);

            var a = li.append("a")
                .attr("id", "headerTag#" + index)
                .attr("data-toggle", "tab")
                .attr("href", "#header" + index)
                .style("font-size", "125%")
                .style("font-weight", "bold")
                .html(datasetId[index - 1]);
        }

        var div = bodyDiv.append("div")
            .attr("class", "tab-content");

        for (let index = 1; index <= va_count; index++) {
            let classStr = 'tab-pane fade';

            if (index == 1)
                classStr += ' in active';

            var tab = div.append("div")
                .attr("id", "header" + index)
                .attr("class", classStr);

            var p = tab.append("p")
                .attr("id", "headerText#" + index);

            var it = p.append("I")
                .text("FITS HEADER data not transmitted yet. Please try later.");
        }
    }
    else {
        var p = bodyDiv.append("p")
            .attr("id", "headerText#" + va_count);

        var it = p.append("I")
            .text("FITS HEADER data not transmitted yet. Please try later.");
    }
}

async function display_FITS_header(index) {
    let fitsData = fitsContainer[index - 1];

    try {
        /*fitsHeader = new Uint8Array(window.atob(fitsData.HEADER.replace(/\s/g, '')).split("").map(function (c) {
          return c.charCodeAt(0);
        }));

        var Buffer = require('buffer').Buffer;
        var LZ4 = require('lz4');

        var uncompressed = new Buffer(parseInt(fitsData.HEADERSIZE, 10));
        uncompressedSize = LZ4.decodeBlock(new Buffer(fitsHeader), uncompressed);
        uncompressed = uncompressed.slice(0, uncompressedSize);

        try {
          fitsHeader = String.fromCharCode.apply(null, uncompressed);
        }
        catch (err) {
          fitsHeader = '';
          for (var i = 0; i < uncompressed.length; i++)
            fitsHeader += String.fromCharCode(uncompressed[i]);
        };*/

        var fitsHeader = fitsData.HEADER;

        var wcsRegEx = [
            'BITPIX',
            'DATE-OBS',
            'EQUINOX',
            'WCSAXES',
            'RADESYS',
            'LONPOLE',
            'LATPOLE',
            'RESTFRQ',
            /NAXIS\d*/,
            /CTYPE\d+/,
            /CRPIX\d+/,
            /CRVAL\d+/,
            /CUNIT\d+/,
            /CDELT\d+/,
            /CD.+/,
            /PV.+/,
            /CROTA\d+/
        ];

        // regex for keywords starting with 'WAT2_'
        const watRegEx = /^WAT2_/;

        var watArray = fitsHeader.match(/.{1,80}/g);
        fitsData.watArray = watArray.filter(function (line) {
            // Extract the keyword
            var keyword = line.slice(0, 8).trim();

            if (keyword.match(watRegEx)) { return true; }

            return false;
        });

        // Split the string into an array and filter based on the WCS regular expressions
        var headerArray = fitsHeader.match(/.{1,80}/g);
        headerArray = headerArray.filter(function (line) {
            // Extract the keyword
            var keyword = line.slice(0, 8).trim();

            for (var i = 0; i < wcsRegEx.length; i += 1) {
                var regEx = wcsRegEx[i];
                if (keyword.match(regEx)) { return true; }
            }

            return false;
        });

        // for each entry replace 'degree' by 'deg   '
        for (var i = 0; i < headerArray.length; i++) {
            headerArray[i] = headerArray[i].replace(/degree/g, 'deg   ');
        }

        // append a 80-character line beginning with "END" to the headerArray
        headerArray.push('END'.padEnd(80, ' '));

        headerStr = headerArray.join('') + '\0';
        nkeyrec = headerArray.length;
        header = string2buffer(headerStr);
        console.log(nkeyrec, headerArray);

        /*rwcs.then(_ => {
            // pass headerStr without the last character to init_wcs_func
            init_wcs_func(index, headerStr.slice(0, -1));
        })
            .catch(e => {
                console.error(e);
            });*/

        fitsData.ready = new Promise((resolve, reject) => {
            waitForModuleReady().then(_ => {
                Module.ready
                    .then(_ => {
                        // Allocate string on Emscripten heap and get byte offset
                        nHeaderBytes = header.byteLength;
                        headerPtr = Module._malloc(nHeaderBytes);
                        headerHeap = new Uint8Array(Module.HEAPU8.buffer, headerPtr, nHeaderBytes);
                        headerHeap.set(new Uint8Array(header));

                        // Use byte offset to pass header string to libwcs
                        stat = Module.initWcs(index, headerHeap.byteOffset, nkeyrec, va_count);

                        // Free memory
                        headerHeap = null;
                        Module._free(headerPtr);

                        if (stat != 0) {
                            console.log("initWcs() failed");
                            reject(false);
                        } else {
                            fitsData.index = index;
                            resolve(true);
                        }
                    })
                    .catch(e => {
                        console.error(e);
                        reject(false);
                    });
            }).catch(e => {
                console.error(e);
                reject(false);
            });
        });

        await fitsData.ready;

        // -------------------------------------
        var headerText = document.getElementById('headerText#' + index);
        headerText.innerHTML = fitsHeader.trim().replace(/(.{80})/g, "$1<br>");

        if (va_count > 1) {
            var headerTag = document.getElementById('headerTag#' + index);

            let line = fitsData.LINE.trim();

            if (line != "")
                headerTag.innerHTML = plain2chem(line, true);
        }
    }
    catch (e) {
        console.log(e);
    };
}

function display_range_validation() {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "rangevalidation")
        .attr("class", "modal fade")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var contentDiv = div.append("div")
        .attr("class", "modal-content")
        .style("margin", "25% auto");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("span")
        .attr("id", "rangevalidationclose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    headerDiv.append("h3")
        .style("color", "#a94442")
        .attr("align", "center")
        .text("INPUT ERROR");

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    bodyDiv.append("p")
        .html("Incorrect velocity/redshift input. Valid values are |V| < c and z > -1.");
}

function download_confirmation(partialSize) {
    var div = d3.select("body")
        .append("div")
        .attr("class", "container")
        .append("div")
        .attr("id", "downloadConfirmation")
        .attr("class", "modal fade")
        .attr("role", "dialog")
        .append("div")
        .attr("class", "modal-dialog");

    var contentDiv = div.append("div")
        .attr("class", "modal-content")
        .style("margin", "25% auto");

    var headerDiv = contentDiv.append("div")
        .attr("class", "modal-header");

    headerDiv.append("span")
        .attr("id", "downloadconfirmationclose")
        .attr("class", "close")
        .style("color", "red")
        .text("×");

    headerDiv.append("h3")
        .style("color", "#a94442")
        .attr("align", "center")
        .text("DOWNLOAD CONFIRMATION");

    var bodyDiv = contentDiv.append("div")
        .attr("id", "modal-body")
        .attr("class", "modal-body");

    let strPartialSize = numeral(partialSize).format('0.0 ib');

    bodyDiv.append("p")
        .html("Estimated FITS cut-out size: " + strPartialSize + ". Proceed with the download?");

    var p = bodyDiv.append("p")
        .html("</br>");

    p.append("span")
        .html("&nbsp;&nbsp;&nbsp;");

    p.append("button")
        .attr("id", "okButton")
        .attr("type", "button")
        .attr("class", "btn btn-primary")
        .attr("onclick", "ok_download()")
        .html("&nbsp; Yes &nbsp;");

    p.append("span")
        .html("&nbsp;&nbsp;&nbsp;");

    p.append("button")
        .attr("id", "cancelButton")
        .attr("type", "button")
        .attr("class", "btn btn-default")
        .attr("onclick", "cancel_download()")
        //.attr("autofocus","")
        .html("&nbsp; Cancel &nbsp;");

    var footer = contentDiv.append("div")
        .attr("class", "modal-footer");

    footer.append("p")
        .style("color", "#a94442")
        .html("you can disable showing this dialog in the <i>Preferences</i> menu, <i>download confirmation</i> checkbox");

    show_download_confirmation();
    $('#downloadConfirmation').modal('show');
}

function draw_rbg_legend(index) {
    if (index > 3 || !composite_view)
        return;

    //do we have all the inputs?
    var black, white, median, multiplier, flux;

    var flux_elem = d3.select("#flux_path" + index);

    try {
        flux = document.getElementById('flux' + index).value
    }
    catch (e) {
        console.log('flux not available yet');
        return;
    };

    try {
        black = parseFloat(flux_elem.attr("black"));
    }
    catch (e) {
        console.log('black not available yet');
        return;
    };

    try {
        white = parseFloat(flux_elem.attr("white"));
    }
    catch (e) {
        console.log('white not available yet');
        return;
    };

    try {
        median = parseFloat(flux_elem.attr("median"));
    }
    catch (e) {
        console.log('median not available yet');
        return;
    };

    multiplier = get_noise_sensitivity(noise_sensitivity);

    try {
        d3.select("#legend" + index).remove();
    }
    catch (e) {
    }

    var svg = d3.select("#BackgroundSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var divisions = 64;//100
    var legendHeight = 0.8 * height;
    var rectHeight = legendHeight / divisions;
    var rectWidth = 5 * rectHeight;//0.05*width;
    var newData = [];

    if (imageContainer[index - 1] == null) {
        console.log("no imageContainer element @", index - 1);
        return;
    }

    let image_bounding_dims = imageContainer[index - 1].image_bounding_dims;
    let pixel_range = imageContainer[index - 1].pixel_range;
    let min_pixel = pixel_range.min_pixel;
    let max_pixel = pixel_range.max_pixel;

    let scale = get_image_scale(width, height, image_bounding_dims.width, image_bounding_dims.height);
    scale = 2.0 * scale / va_count;

    let img_width = Math.floor(scale * image_bounding_dims.width);
    let img_height = Math.floor(scale * image_bounding_dims.height);

    for (var i = 0; i < divisions; i++)
        newData.push(min_pixel + (max_pixel - min_pixel) * i / (divisions - 1));

    //var x = Math.max(0.05*width, (width+img_width)/2 + 0.5*rectWidth);
    //var x = Math.max(0.05*width + (index-1)*1.5*rectWidth, (width-img_width)/2 - va_count*2.4*rectWidth + (index-1)*1.5*rectWidth);
    var x = (width - img_width) / 2 - 0.05 * width - (va_count + 1.5 - index) * 1.5 * rectWidth;

    try {
        // first remove any existing legend group
        d3.select("#legend" + index).remove();
    }
    catch (e) {
    }

    let opacity = displayLegend ? 1 : 0;
    var group = svg.append("g")
        .attr("id", "legend" + index)
        .attr("opacity", opacity);

    let strokeColour = 'white';

    if (theme == 'bright')
        strokeColour = 'black';

    let rgb = ['red', 'green', 'blue'];

    group.selectAll('rect')
        .data(newData)
        .enter()
        .append('rect')
        .attr("x", (x + rectWidth / 2))
        .attr("y", function (d, i) { return (0.9 * height - (i + 1) * rectHeight); })
        .attr("height", (rectHeight + 1))
        .attr("width", rectWidth / 2)
        //.attr("stroke", strokeColour)
        //.attr("stroke-width", 0.1)
        .attr("stroke", "none")
        //.attr('fill', function(d, i) { return pixel2rgba(1.0*d, index-1, 0.8);});
        .attr('fill', function (d, i) {
            let raw = get_tone_mapping(d, flux, black, white, median, multiplier, index);
            let colour = interpolate_colourmap(raw, rgb[index - 1], 0.8);
            return colour;
        });

    var upper_range;

    if (flux == "ratio")
        upper_range = 0.999;
    else
        upper_range = 1.0;

    var colourScale = d3.scaleLinear()
        .range([0.8 * height, 0])
        .domain([0, upper_range]);

    var colourAxis = d3.axisRight(colourScale)
        .tickSizeOuter([0])
        .tickSizeInner([0])
        .tickFormat(function (d) {
            var prefix = "";

            if (d == 0)
                prefix = "≤";

            if (d == 1)
                prefix = "≥";

            var pixelVal = get_pixel_flux(d, index);

            var number;

            if (Math.abs(pixelVal) <= 0.001 || Math.abs(pixelVal) >= 1000)
                number = pixelVal.toExponential(3);
            else
                number = pixelVal.toPrecision(3);

            return prefix + number;
        });

    group.append("g")
        .attr("class", "colouraxis")
        .attr("id", "legendaxis")
        .style("stroke-width", emStrokeWidth)
        .attr("transform", "translate(" + x + "," + 0.1 * height + ")")
        .call(colourAxis);

    let fitsData = fitsContainer[index - 1];

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (fitsData.depth > 1 && has_velocity_info)
            bunit += '•km/s';

        bunit = "[" + bunit + "]";
    }

    let line = fitsData.LINE.trim();
    let filter = fitsData.FILTER.trim();

    if (filter != "")
        line = filter;
    else {
        if (line == "")
            line = "line-" + index;
    }

    group.append("foreignObject")
        .attr("x", (x + 0.0 * rectWidth))
        .attr("y", 0.9 * height + 0.75 * emFontSize)
        .attr("width", 5 * emFontSize)
        .attr("height", 3 * emFontSize)
        .append("xhtml:div")
        .html('<p style="text-align: left">' + plain2chem(line, false) + '&nbsp;' + bunit + '</p>');
}

function display_rgb_legend() {
    console.log("display_rgb_legend()");

    if (va_count > 3 || !composite_view)
        return;

    for (let index = 1; index <= va_count; index++) {
        draw_rbg_legend(index);
    }

    if (va_count == 1) {
        var elem = d3.select("#legend");

        if (displayLegend)
            elem.attr("opacity", 1);
        else
            elem.attr("opacity", 0);
    }
}

function display_legend() {
    //console.log("display_legend()");

    if (va_count > 1)
        return;

    //do we have all the inputs?
    var black, white, median, multiplier, flux;

    var flux_elem = d3.select("#flux_path" + va_count);

    try {
        flux = document.getElementById('flux' + va_count).value
    }
    catch (e) {
        console.log('flux not available yet');
        return;
    };

    try {
        black = parseFloat(flux_elem.attr("black"));
    }
    catch (e) {
        console.log('black not available yet');
        return;
    };

    try {
        white = parseFloat(flux_elem.attr("white"));
    }
    catch (e) {
        console.log('white not available yet');
        return;
    };

    try {
        median = parseFloat(flux_elem.attr("median"));
    }
    catch (e) {
        console.log('median not available yet');
        return;
    };

    multiplier = get_noise_sensitivity(noise_sensitivity);

    var rect = d3.select("#image_rectangle");

    var img_width, img_height;

    try {
        img_width = parseFloat(rect.attr("width"));
        img_height = parseFloat(rect.attr("height"));
    }
    catch (e) {
        console.log('image_rectangle not available yet');
        return;
    }

    try {
        clear_webgl_legend_buffers(va_count);
    }
    catch (e) {
    }

    try {
        d3.select("#legend").remove();
    }
    catch (e) {
    }

    var svg = d3.select("#BackgroundSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var legendHeight = 0.8 * height;
    var rectWidth = 5 * legendHeight / 64;
    var x = Math.max(0.05 * width, (width - img_width) / 2 - 1.5 * rectWidth);

    var group = svg.append("g")
        .attr("id", "legend")
        .attr("opacity", 1.0);

    init_webgl_legend_buffers(x, Math.round(0.1 * height), Math.round(rectWidth), Math.round(legendHeight), va_count);
    clear_webgl_legend_buffers(va_count);

    var upper_range;

    if (flux == "ratio")
        upper_range = 0.999;
    else
        upper_range = 1.0;

    var colourScale = d3.scaleLinear()
        .range([0.8 * height, 0])
        .domain([0, upper_range]);

    var colourAxis = d3.axisRight(colourScale)
        .tickSizeOuter([0])
        .tickSizeInner([0])
        .tickFormat(function (d) {
            var prefix = "";

            if (d == 0)
                prefix = "≤";

            if (d == 1)
                prefix = "≥";

            var pixelVal = get_pixel_flux(d, va_count);

            var number;

            if (Math.abs(pixelVal) <= 0.001 || Math.abs(pixelVal) >= 1000)
                number = pixelVal.toExponential(3);
            else
                number = pixelVal.toPrecision(3);

            return prefix + number;
        });

    group.append("g")
        .attr("class", "colouraxis")
        .attr("id", "legendaxis")
        .style("stroke-width", emStrokeWidth / 2)
        .attr("transform", "translate(" + ((width - img_width) / 2 - 2.0 * rectWidth) + "," + 0.1 * height + ")")
        .call(colourAxis);

    let fitsData = fitsContainer[va_count - 1];

    var bunit = '';
    if (fitsData.BUNIT != '') {
        bunit = fitsData.BUNIT.trim();

        if (fitsData.depth > 1 && has_velocity_info)
            bunit += '•km/s';

        bunit = "[" + bunit + "]";
    }

    group.append("text")
        .attr("id", "colourlabel")
        .attr("x", ((width - img_width) / 2 - 1.0 * rectWidth))
        .attr("y", 0.9 * height + 1.5 * emFontSize)
        .attr("font-family", "Inconsolata")
        .attr("font-size", 1.25 * emFontSize)
        .attr("text-anchor", "middle")
        .attr("stroke", "none")
        .attr("opacity", 0.8)
        .text(bunit);

    if (va_count == 1) {
        var elem = d3.select("#legend");

        if (displayLegend)
            elem.attr("opacity", 1);
        else
            elem.attr("opacity", 0);
    }
    else {
        for (let index = 1; index <= va_count; index++) {
            var elem = d3.select("#legend" + index);

            if (displayLegend)
                elem.attr("opacity", 1);
            else
                elem.attr("opacity", 0);
        }
    }
}

function update_legend() {
    try {
        var flux = document.getElementById('flux' + va_count).value
    }
    catch (e) {
        console.log('flux not available yet');
        return;
    };

    try {
        d3.select("#legendaxis").remove();
    }
    catch (e) {
    }

    var rect = d3.select("#image_rectangle");

    try {
        var img_width = parseFloat(rect.attr("width"));
    }
    catch (e) {
        console.log('image_rectangle not available yet');
        return;
    }

    var svg = d3.select("#BackgroundSVG");
    var width = parseFloat(svg.attr("width"));
    var height = parseFloat(svg.attr("height"));

    var legendHeight = 0.8 * height;
    var rectWidth = 5 * legendHeight / 64;

    var group = d3.select("#legend");

    let strokeColour = 'white';

    if (theme == 'bright')
        strokeColour = 'black';

    var upper_range;

    if (flux == "ratio")
        upper_range = 0.999;
    else
        upper_range = 1.0;

    var colourScale = d3.scaleLinear()
        .range([0.8 * height, 0])
        .domain([0, upper_range]);

    var colourAxis = d3.axisRight(colourScale)
        .tickSizeOuter([0])
        .tickSizeInner([0])
        .tickFormat(function (d) {
            var prefix = "";

            if (d == 0)
                prefix = "≤";

            if (d == 1)
                prefix = "≥";

            var pixelVal = get_pixel_flux(d, va_count);

            var number;

            if (Math.abs(pixelVal) <= 0.001 || Math.abs(pixelVal) >= 1000)
                number = pixelVal.toExponential(3);
            else
                number = pixelVal.toPrecision(3);

            return prefix + number;
        });

    group.append("g")
        .attr("class", "colouraxis")
        .attr("id", "legendaxis")
        .style("stroke-width", emStrokeWidth / 2)
        .attr("transform", "translate(" + ((width - img_width) / 2 - 2.0 * rectWidth) + "," + 0.1 * height + ")")
        .call(colourAxis);
}

function init_webgl_legend_buffers(x, y, width, height, index) {
    //place the image onto the main canvas
    var canvas = document.getElementById('LegendCanvas');
    canvas.style.display = "block";// a hack needed by Apple Safari

    if (webgl1 || webgl2) {
        canvas.addEventListener("webglcontextlost", function (event) {
            event.preventDefault();
            console.error("LegendCanvas: webglcontextlost");
        }, false);

        canvas.addEventListener(
            "webglcontextrestored", function () {
                console.log("LegendCanvas: webglcontextrestored");
                init_webgl_legend_buffers(x, y, width, height, index);
            }, false);
    }

    if (webgl2) {
        var ctx = canvas.getContext("webgl2");
        imageContainer[index - 1].legend_gl = ctx;
        // console.log("init_webgl is using the WebGL2 context.");

        // enable floating-point textures filtering
        ctx.getExtension('OES_texture_float_linear');

        // needed by gl.checkFramebufferStatus
        ctx.getExtension('EXT_color_buffer_float');

        // call the common WebGL renderer
        webgl_legend_renderer(index, ctx, x, y, width, height);
    } else if (webgl1) {
        var ctx = canvas.getContext("webgl");
        imageContainer[index - 1].legend_gl = ctx;
        // console.log("init_webgl is using the WebGL1 context.");

        // enable floating-point textures
        ctx.getExtension('OES_texture_float');
        ctx.getExtension('OES_texture_float_linear');

        // call the common WebGL renderer
        webgl_legend_renderer(index, ctx, x, y, width, height);
    } else {
        console.log("WebGL not supported by your browser, falling back onto HTML 2D Canvas (not implemented yet).");
        return;
    }
}

function clear_webgl_legend_buffers(index) {
    var image = imageContainer[index - 1];
    var gl = image.legend_gl;

    if (gl === undefined || gl == null)
        return;

    // position buffer
    if (image.legend_positionBuffer !== undefined)
        gl.deleteBuffer(image.legend_positionBuffer);

    // program
    if (image.legend_program !== undefined && image.legend_program != null) {
        gl.deleteShader(image.legend_program.vShader);
        gl.deleteShader(image.legend_program.fShader);
        gl.deleteProgram(image.legend_program);
        image.legend_program = null;
    }

    // image.legend_gl = null; // do not nullify the context, it will be reused
}

function webgl_legend_renderer(index, gl, x, y, width, height) {
    var image = imageContainer[index - 1];

    // setup GLSL program
    var vertexShaderCode = document.getElementById("legend-vertex-shader").text;
    var fragmentShaderCode = document.getElementById("legend-common-shader").text;
    fragmentShaderCode += document.getElementById(colourmap + "-shader").text;

    // remove the alpha blending multiplier
    fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor.rgb *= gl_FragColor.a;/g, "");

    // WebGL2 accepts WebGL1 shaders so there is no need to update the code
    if (webgl2) {
        var prefix = "#version 300 es\n";
        vertexShaderCode = prefix + vertexShaderCode;
        fragmentShaderCode = prefix + fragmentShaderCode;

        // attribute -> in
        vertexShaderCode = vertexShaderCode.replace(/attribute/g, "in");
        fragmentShaderCode = fragmentShaderCode.replace(/attribute/g, "in");

        // varying -> out
        vertexShaderCode = vertexShaderCode.replace(/varying/g, "out");

        // varying -> in
        fragmentShaderCode = fragmentShaderCode.replace(/varying/g, "in");

        // texture2D -> texture
        fragmentShaderCode = fragmentShaderCode.replace(/texture2D/g, "texture");

        // replace gl_FragColor with a custom variable, i.e. texColour
        fragmentShaderCode = fragmentShaderCode.replace(/gl_FragColor/g, "texColour");

        // add the definition of texColour
        var pos = fragmentShaderCode.indexOf("void main()");
        fragmentShaderCode = fragmentShaderCode.insert_at(pos, "out vec4 texColour;\n\n");
    }

    image.legend_program = createProgram(gl, vertexShaderCode, fragmentShaderCode);

    // look up where the vertex data needs to go.
    var positionLocation = gl.getAttribLocation(image.legend_program, "a_position");

    // Create a position buffer
    image.legend_positionBuffer = gl.createBuffer();

    gl.bindBuffer(gl.ARRAY_BUFFER, image.legend_positionBuffer);
    // Put a unit quad in the buffer
    var positions = [
        -1, -1,
        -1, 1,
        1, -1,
        1, -1,
        -1, 1,
        1, 1,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

    // no need for an animation loop, just handle the lost context
    //WebGL how to convert from clip space to pixels
    gl.viewport(Math.round(x), Math.round(y), Math.round(width), Math.round(height));

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // drawRegion (execute the GLSL program)
    // Tell WebGL to use our shader program pair
    gl.useProgram(image.legend_program);

    // Setup the attributes to pull data from our buffers
    gl.enableVertexAttribArray(positionLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, image.legend_positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    // execute the GLSL program
    // draw the quad (2 triangles, 6 vertices)
    gl.drawArrays(gl.TRIANGLES, 0, 6);
}

function get_slope_from_multiplier(value) {
    var xmin = 0.01;
    var xmax = 100.0;

    var pmin = 0.001;
    var pmax = 0.5;

    return pmin + (pmax - pmin) * (value - xmin) / (xmax - xmin);
}

function get_noise_sensitivity_from_multiplier(value) {
    var xmin = Math.log(0.01);
    var xmax = Math.log(100.0);

    return 100.0 * (Math.log(value) - xmin) / (xmax - xmin);
}

function get_noise_sensitivity(value) {
    var xmin = Math.log(0.01);
    var xmax = Math.log(100.0);
    var x = xmin + (xmax - xmin) / 100.0 * parseFloat(value);

    return Math.exp(x);
}

function get_noise_sensitivity_string(value, precision) {
    var x = get_noise_sensitivity(value);

    return 'x' + x.toFixed(precision);
}

function resizeMe() {
    clearTimeout(idleResize);

    idleResize = setTimeout(function () {
        location.reload(); // was reload(false)
    }, 250);
}

function beforePrint() {
    console.log('before printing...');

    window.onresize = null;
}

function afterPrint() {
    console.log('after printing...');

    window.onresize = resizeMe;
}

function localStorage_read_boolean(key, defVal) {
    if (localStorage.getItem(key) !== null) {
        var value = localStorage.getItem(key);

        if (value == "true")
            return true;

        if (value == "false")
            return false;
    }
    else
        return defVal;
}

function localStorage_read_number(key, defVal) {
    if (localStorage.getItem(key) === null)
        return defVal;
    else
        return parseFloat(localStorage.getItem(key));
}

function localStorage_read_string(key, defVal) {
    if (localStorage.getItem(key) === null)
        return defVal;
    else
        return localStorage.getItem(key);
}

function localStorage_write_boolean(key, value) {
    if (value)
        localStorage.setItem(key, "true");
    else
        localStorage.setItem(key, "false");
}


function transpose(m) { return zeroFill(m.reduce(function (m, r) { return Math.max(m, r.length) }, 0)).map(function (r, i) { return zeroFill(m.length).map(function (c, j) { return m[j][i] }) }) } function zeroFill(n) { return new Array(n + 1).join("0").split("").map(Number) };

function contour_surface() {
    //return contour_surface_marching_squares() ;
    return contour_surface_webworker();
};

function contour_surface_marching_squares() {
    if (va_count > 1 && !composite_view)
        return;

    has_contours = false;

    try {
        d3.select('#contourPlot').remove();
    }
    catch (e) { };

    var data = [];

    var imageCanvas = imageContainer[va_count - 1].imageCanvas;
    var imageDataCopy = imageContainer[va_count - 1].imageDataCopy;
    var image_bounding_dims = imageContainer[va_count - 1].image_bounding_dims;

    if (composite_view) {
        imageCanvas = compositeCanvas;
        imageDataCopy = compositeImageData.data;
    }

    let min_value = 255;
    let max_value = 0;

    //for(var h=0;h<image_bounding_dims.height;h++)
    for (var h = image_bounding_dims.height - 1; h >= 0; h--) {
        var row = [];

        var xcoord = image_bounding_dims.x1;
        var ycoord = image_bounding_dims.y1 + h;
        var pixel = 4 * (ycoord * imageCanvas.width + xcoord);

        for (var w = 0; w < image_bounding_dims.width; w++) {
            //var z = imageDataCopy[pixel];
            var r = imageDataCopy[pixel];
            var g = imageDataCopy[pixel + 1];
            var b = imageDataCopy[pixel + 2];
            var z = (r + g + b) / 3;
            pixel += 4;

            if (z < min_value)
                min_value = z;

            if (z > max_value)
                max_value = z;

            row.push(z);
        }

        data.push(row);
    }

    //console.log(data);

    //console.log("min_pixel:", min_pixel, "max_pixel:", max_pixel) ;
    //console.log("min_value:", min_value, "max_value:", max_value);

    var contours = parseInt(document.getElementById('contour_lines').value) + 1;
    var step = (max_value - min_value) / contours;
    var zs = d3.range(min_value + step, max_value, step);

    //console.log(zs);

    var isoBands = [];
    for (var i = 1; i < zs.length; i++) {
        var lowerBand = zs[i - 1];
        var upperBand = zs[i];

        var band = MarchingSquaresJS.isoBands(data, lowerBand, upperBand - lowerBand);
        //console.log('band', band);
        isoBands.push({ "coords": band, "level": i, "val": zs[i] });
    }

    //console.log(isoBands);

    //return ;

    var elem = d3.select("#image_rectangle");
    var width = parseFloat(elem.attr("width"));
    var height = parseFloat(elem.attr("height"));

    var x = d3.scaleLinear()
        .range([0, width - 1])
        .domain([0, data[0].length - 1]);

    var y = d3.scaleLinear()
        .range([height - 1, 0])
        .domain([0, data.length - 1]);

    var colours = d3.scaleLinear()
        .domain([min_value, max_value])
        .range(["#fff", "red"]);

    d3.select("#BackgroundSVG").append("svg")
        .attr("id", "contourPlot")
        .attr("x", elem.attr("x"))
        .attr("y", elem.attr("y"))
        .attr("width", width)
        .attr("height", height)
        .selectAll("path")
        .data(isoBands)
        .enter().append("path")
        //.style("fill",function(d) { return colours(d.level);})
        .style("fill", "none")
        .style("stroke", "black")
        .attr("opacity", 0.5)
        .attr("d", function (d) {
            var p = "";
            d.coords.forEach(function (aa, i) {
                p += (d3.line()
                    .x(function (dat) { return x(dat[0]); })
                    .y(function (dat) { return y(dat[1]); })
                    .curve(d3.curveLinear)
                )(aa) + "Z";
            });
            return p;
        });

    has_contours = true;
}

function contour_surface_webworker() {
    if (va_count > 1 && !composite_view)
        return;

    has_contours = false;

    try {
        d3.selectAll('#contourPlot').remove();
    }
    catch (e) { };

    var data = [];
    var imageFrame = null;
    var imageData = null;

    if (composite_view) {
        imageFrame = compositeImage;
        imageData = compositeImageTexture;
    } else {
        imageFrame = imageContainer[va_count - 1];
    }

    var image_bounding_dims = imageFrame.image_bounding_dims;

    var min_value = Number.MAX_VALUE;
    var max_value = -Number.MAX_VALUE;

    if (composite_view)
        for (let h = image_bounding_dims.height - 1; h >= 0; h--) {
            let row = [];

            let xcoord = image_bounding_dims.x1;
            let ycoord = image_bounding_dims.y1 + h;
            let pixel = 4 * (ycoord * imageFrame.width + xcoord);

            for (let w = 0; w < image_bounding_dims.width; w++) {
                let r = imageData[pixel];
                let g = imageData[pixel + 1];
                let b = imageData[pixel + 2];
                let z = (r + g + b) / 3;
                pixel += 4;

                if (z < min_value)
                    min_value = z;

                if (z > max_value)
                    max_value = z;

                row.push(z);
            }

            data.push(row);
        }
    else //use imageFrame
    {
        min_value = imageFrame.pixel_range.min_pixel;
        max_value = imageFrame.pixel_range.max_pixel;

        for (let h = image_bounding_dims.height - 1; h >= 0; h--) {
            let row = [];

            let xcoord = image_bounding_dims.x1;
            let ycoord = image_bounding_dims.y1 + h;
            let pixel = ycoord * imageFrame.width + xcoord;

            for (let w = 0; w < image_bounding_dims.width; w++) {
                let z = imageFrame.pixels[pixel];
                pixel += 1;

                row.push(z);
            }

            data.push(row);
        };
    }

    // console.log(data);
    // console.log("min_value:", min_value, "max_value:", max_value);

    var contours = parseInt(document.getElementById('contour_lines').value) + 1;
    var step = (max_value - min_value) / contours;
    var zs = d3.range(min_value + step, max_value, step);
    //console.log("zs:", zs);

    min_value = 0;
    max_value = 1;
    var step = (max_value - min_value) / contours;
    var zs = d3.range(min_value + step, max_value, step);
    for (var i = 0; i < zs.length; i++)
        zs[i] = get_pixel_flux(zs[i], va_count);
    //console.log("zs:", zs);

    var completed_levels = 0;
    //parallel isoBands
    for (var i = 1; i < zs.length; i++) {
        var lowerBand = zs[i - 1];
        var upperBand = zs[i];

        var CRWORKER = new Worker('data:text/html;base64,' + window.btoa(get_worker_script()));

        CRWORKER.addEventListener('message', function (e) {
            //console.log('Worker said: ', e.data);
            completed_levels++;

            var isoBands = [];
            isoBands.push({ "coords": e.data, "level": i, "val": zs[i] });

            //plot the isoBands
            var elem = d3.select("#image_rectangle");
            var width = parseFloat(elem.attr("width"));
            var height = parseFloat(elem.attr("height"));

            var x = d3.scaleLinear()
                .range([0, width])
                .domain([0, data[0].length - 1]);

            var y = d3.scaleLinear()
                .range([height, 0])
                .domain([0, data.length - 1]);

            d3.select("#ContourSVG").append("svg")
                .attr("id", "contourPlot")
                .attr("x", parseFloat(elem.attr("x")))
                .attr("y", parseFloat(elem.attr("y")))
                .attr("width", width)
                .attr("height", height)
                .selectAll("path")
                .data(isoBands)
                .enter().append("path")
                .style("fill", "none")
                .style("stroke", "black")
                .attr("opacity", 0.5)
                .attr("d", function (d) {
                    var p = "";
                    d.coords.forEach(function (aa, i) {
                        p += (d3.line()
                            .x(function (dat) { return x(dat[0]); })
                            .y(function (dat) { return ((height - 1) - y(dat[1])); })
                            .curve(d3.curveLinear)
                        )(aa) + "Z";
                    });
                    return p;
                });

            has_contours = true;

            if (completed_levels == zs.length - 1)
                hide_hourglass();
        }, false);

        CRWORKER.postMessage({ data: data, level: i, lowerBand: lowerBand, upperBand: upperBand });
    };

    has_contours = true;
}

function test_webgl1() {
    try {
        var canvas = document.createElement('canvas');
        return !!window.WebGLRenderingContext && (
            canvas.getContext('webgl'));
    } catch (e) { return false; }
};

function test_webgl2() {
    try {
        var canvas = document.createElement('canvas');
        return !!window.WebGLRenderingContext && (
            canvas.getContext('webgl2'));
    } catch (e) { return false; }
};

function test_webgl_support() {
    try {
        var canvas = document.createElement('canvas');
        return !!window.WebGLRenderingContext && (
            canvas.getContext('webgl') || canvas.getContext('webgl2'));
    } catch (e) { return false; }
};

function enable_3d_view() {
    has_webgl = false;

    if (test_webgl_support()) {
        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/three.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/Detector.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/threex.keyboardstate.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/threex.windowresize.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/THREEx.FullScreen.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/TrackballControls.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        (function () {
            var po = document.createElement('script'); po.type = 'text/javascript'; po.async = false;
            // po.src = 'surface.js' + '?' + encodeURIComponent(get_js_version());
            po.src = 'https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@' + votable.getAttribute('data-version-major') + '.' + votable.getAttribute('data-version-minor') + '.' + votable.getAttribute('data-version-sub') + '/htdocs/fitswebql/surface.min.js';
            var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
        })();

        has_webgl = true;
        console.log("WebGL supported.");
    }
    else
        console.log("WebGL not supported by your browser.");
}

function addStylesheetRules(rules) {
    var styleEl = document.createElement('style');

    // Append <style> element to <head>
    document.head.appendChild(styleEl);

    // Grab style element's sheet
    var styleSheet = styleEl.sheet;

    for (var i = 0; i < rules.length; i++) {
        var j = 1,
            rule = rules[i],
            selector = rule[0],
            propStr = '';
        // If the second argument of a rule is an array of arrays, correct our variables.
        if (Array.isArray(rule[1][0])) {
            rule = rule[1];
            j = 0;
        }

        for (var pl = rule.length; j < pl; j++) {
            var prop = rule[j];
            propStr += prop[0] + ': ' + prop[1] + (prop[2] ? ' !important' : '') + ';\n';
        }

        // Insert CSS Rule
        styleSheet.insertRule(selector + '{' + propStr + '}', styleSheet.cssRules.length);
    }
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function open_3d_view() {
    try {
        enable_3d_view();
    }
    catch (e) {
        has_webgl = false;
        console.log('WebGL disabled', e);
    }
}

// a function to asynchronously fetch GLSL shaders from a LZ4-compressed XML file
async function fetch_glsl() {
    var url = 'https://cdn.jsdelivr.net/gh/jvo203/FITSWEBQLSE@' + votable.getAttribute('data-version-major') + '.' + votable.getAttribute('data-version-minor') + '.' + votable.getAttribute('data-version-sub') + '/htdocs/fitswebql/glsl_shaders.bin';

    // fetch a binary file containing the GLSL shaders
    await fetch(url).then(function (response) {
        // throw an error if the status is not 200
        if (!response.ok) {
            throw new Error('HTTP status ' + response.status);
        }

        return response.arrayBuffer();
    }).then(function (buffer) {
        try {
            // bzip2 decoder
            var bytes = new Uint8Array(buffer);
            var xml = bzip2.simple(bzip2.array(bytes));

            // parse the XML, iterate through "shader" elements, get the "id" from the attribute
            var parser = new DOMParser();
            var xmlDoc = parser.parseFromString(xml, "application/xml");
            var shaders = xmlDoc.getElementsByTagName("shader");

            for (var i = 0; i < shaders.length; i++) {
                var id = shaders[i].getAttribute("id");
                var text = shaders[i].textContent;

                // create a new script element
                var script = document.createElement('script');
                script.type = 'x-shader/x-fragment';
                script.id = id;
                script.text = text;

                // append the script element to the document
                document.head.appendChild(script);
            }
        } catch (e) {
            console.error('Failed to decompress/parse GLSL XML:', e);
            return;
        }
    }).catch(function (error) {
        console.error('Failed to fetch GLSL shaders:', error);
    });
}

async function fetch_glsl_shaders() {
    // there is no need to fetch the shaders in local mode as they will be embedded in the HTML file
    if (isLocal)
        return;

    await fetch_glsl();
}

// async function to wait until Module.ready is defined
async function waitForModuleReady() {
    while (typeof Module === 'undefined' || typeof Module.ready === 'undefined') {
        console.log('waiting for Module.ready...');
        await sleep(100);
    }
}

async function mainRenderer() {
    votable = document.getElementById('votable');
    isLocal = (votable.getAttribute('data-server-mode').indexOf("LOCAL") > -1) ? true : false;

    webgl1 = test_webgl1();
    webgl2 = test_webgl2();

    if (webgl2) {
        // prefer WebGL2 over 1
        webgl1 = false;
        webgl2 = true;
    }

    var res3d = open_3d_view();
    resGLSL = fetch_glsl_shaders();

    //intercept print events
    if (window.matchMedia) {
        var mediaQueryList = window.matchMedia('print');
        mediaQueryList.addListener(function (mql) {
            if (mql.matches) {
                beforePrint();
            } else {
                afterPrint();
            }
        });
    }

    window.onbeforeprint = beforePrint;
    window.onafterprint = afterPrint;
    //end-of-printing

    if (votable.getAttribute('data-root-path') != null)
        ROOT_PATH = votable.getAttribute('data-root-path').trim();
    console.log("ROOT_PATH=" + ROOT_PATH);

    endianness = getEndianness();
    console.log('endianness: ', endianness);

    if (localStorage.getItem("ui_theme") === null) {
        //if(isLocal)
        {
            /*theme = "bright" ;
            colourmap = "haxby" ;*/

            theme = "dark";
            colourmap = "green";
            axisColour = "rgba(255,204,0,0.8)";
        }
        /*else
        {
          theme = "dark" ;
          colourmap = "green" ;
        }*/

        localStorage.setItem("ui_theme", theme);
        localStorage.setItem("v5_colourmap", colourmap);
    }
    else {
        theme = localStorage.getItem("ui_theme");

        if (theme == 'bright')
            axisColour = "#000000";
        else
            axisColour = "rgba(255,204,0,0.8)"; // axisColour
    }

    noise_sensitivity = 50; //get_noise_sensitivity(localStorage.getItem("noise_sensitivity")) ;

    if (localStorage.getItem("zoom_shape") === null) {
        zoom_shape = "circle";
        localStorage.setItem("zoom_shape", zoom_shape);
    }
    else
        zoom_shape = localStorage.getItem("zoom_shape");

    if (localStorage.getItem("intensity_mode") === null) {
        console.log("URL parameters:", window.location.search);

        //an override for FUGIN
        if (window.location.search.indexOf('fugin') > 0)
            intensity_mode = "mean";
        else
            intensity_mode = "integrated";

        localStorage.setItem("intensity_mode", intensity_mode);
    }
    else
        intensity_mode = localStorage.getItem("intensity_mode");

    if (localStorage.getItem("v5_colourmap") === null) {
        if (theme == 'bright')
            colourmap = "haxby";
        else
            colourmap = "green";

        localStorage.setItem("v5_colourmap", colourmap);
    }
    else
        colourmap = localStorage.getItem("v5_colourmap");

    if (colourmap === null)
        colourmap = "green";

    //add a colourmap URL override
    let pos = window.location.search.indexOf('colourmap=');
    if (pos > 0) {
        //extract the colourmap parameter
        let params = window.location.search.substr(pos);
        console.log("colourmap parameters:", params);

        var result = {};
        params.split("&").forEach(function (part) {
            var item = part.split("=");
            result[item[0]] = decodeURIComponent(item[1]);
        });

        var tmp = result["colourmap"];
        if (tmp !== undefined)
            colourmap = tmp;

        console.log("colourmap:", result["colourmap"]);
    }

    if (localStorage.getItem("image_quality") === null) {
        image_quality = "medium";
        localStorage.setItem("image_quality", image_quality);
    }
    else
        image_quality = localStorage.getItem("image_quality");

    if (localStorage.getItem("video_fps_control") === null) {
        video_fps_control = "auto";
        localStorage.setItem("video_fps_control", video_fps_control);
    }
    else
        video_fps_control = localStorage.getItem("video_fps_control");

    composite_view = (parseInt(votable.getAttribute('data-composite')) == 1) ? true : false;
    // composite_view = false; // an override during development
    console.log("composite view:", composite_view);

    optical_view = false;
    spectrum_view = false;

    if (firstTime) {
        fps = 30;//target fps; 60 is OK in Chrome but a bit laggish in Firefox
        fpsInterval = 1000 / fps;

        has_frequency_info = false;
        has_velocity_info = false;
        frame_multiplier = 1;

        imageData = null;
        newImageData = null;
        initKalmanFilter = false;
        windowLeft = false;
        streaming = false;
        dragging = false;
        video_playback = false;
        video_offset = null;
        video_timeout = -1;
        mol_pos = -1;
        idleMouse = -1;
        idleVideo = -1;
        moving = false;
        freqdrag = false;
        data_band_lo = 0;
        data_band_hi = 0;
        latency = 0;
        ping_latency = 0;
        computed = 0;
        processed = 0;
        cpuTime = 0;
        velocityline_position = -1;
        angularline_position = -1;

        //image
        recv_seq_id = 0;
        sent_seq_id = 0;
        last_seq_id = 0;

        //spectrum
        binning = 1;

        //video
        if (video_fps_control == 'auto')
            vidFPS = 5;//10
        else
            vidFPS = parseInt(video_fps_control);

        vidInterval = 1000 / vidFPS;

        //track the bitrate with a Kalman Filter
        target_bitrate = 1000;
        bitrate = target_bitrate;
        eta = 0.1;
        variance = 0.0;

        recv_vid_id = 0;
        sent_vid_id = 0;
        last_vid_id = 0;
        videoFrame = [];

        spectrum_stack = [];
        image_stack = [];
        video_stack = [];
        viewport_zoom_settings = null;
        invalidateViewport = false;
        viewport = {};
        zoom_dims = null;
        zoom_location = 'lower';
        zoom_scale = 25;
        xradec = null;
        molecules = [];

        tmp_data_min = 0;
        tmp_data_max = 0;

        user_data_min = null;
        user_data_max = null;

        freq_mouse_start = 0;
        freqdrag = false;
        session_freq_start = 0;
        session_freq_end = 0;
        session_frame_start = 0;
        session_frame_end = 0;
        frame_start = 0;
        frame_end = 0;

        mousedown = false;
        begin_x = 0;
        begin_y = 0;
        end_x = 0;
        end_y = 0;

        // HDS redshift
        redshift = 0.0;
        hds_divs = [];

        coordsFmt = localStorage_read_string("coordsFmt", "HMS");//DMS or HMS

        displayCDMS = localStorage_read_boolean("displayCDMS", false);
        displayJPL = localStorage_read_boolean("displayJPL", false);
        displayRecomb = localStorage_read_boolean("displayRecomb", true);
        displayTopModel = localStorage_read_boolean("displayTopModel", false);
        displaySLAIM = localStorage_read_boolean("displaySLAIM", false);
        displayLovas = localStorage_read_boolean("displayLovas", true);
        displayToyaMA = localStorage_read_boolean("displayToyaMA", false);
        displayOSU = localStorage_read_boolean("displayOSU", false);
        displayIntensity = localStorage_read_number("displayIntensity", -1);

        // check if displayIntensity is a valid number
        if (isNaN(displayIntensity)) {
            displayIntensity = -1;
        }

        displayLimit = localStorage_read_number("displayLimit", 500);
        peak_tracking = localStorage_read_boolean("peak_tracking", true);
        realtime_spectrum = localStorage_read_boolean("realtime_spectrum", true);
        realtime_video = localStorage_read_boolean("realtime_video", true);
        displayDownloadConfirmation = localStorage_read_boolean("displayDownloadConfirmation", true);
        welcome = localStorage_read_boolean("welcome_v5_alpha", true);

        autoscale = true;
        displayScalingHelp = localStorage_read_boolean("displayScalingHelp", true);
        last_spectrum = null;

        displayContours = false;
        displayPVCrosshair = localStorage_read_boolean("displayPVCrosshair", true);
        displayPVContours = localStorage_read_boolean("displayPVContours", true);
        displayLegend = localStorage_read_boolean("displayLegend", true);
        displayMolecules = localStorage_read_boolean("displayMolecules", true);
        displaySpectrum = localStorage_read_boolean("displaySpectrum", true);
        displayGridlines = localStorage_read_boolean("displayGridlines", false);
        displayBeam = false;
        displayRegion = true;

        has_contours = false;
        has_preferences = false;

        document.body.addEventListener('mousemove', function (event) {
            let pointX = event.clientX;
            let pointY = event.clientY;

            // console.log("document::mousemove", pointX, pointY);

            if (pointX < 10 || pointY < 10) {
                // console.log("document::out");
                hide_navigation_bar();
            }

            if (pointX > window.innerWidth - 10 || pointY > window.innerHeight - 10) {
                // console.log("document::out");
                hide_navigation_bar();
            }
        }, false);

        d3.select("body").append("div")
            .attr("id", "mainDiv")
            .attr("class", "main");

        var rect = document.getElementById('mainDiv').getBoundingClientRect();
        var width = Math.round(rect.width);
        var height = Math.round(rect.height);
        document.getElementById('mainDiv').setAttribute("style", "width:" + width.toString() + "px");
        document.getElementById('mainDiv').setAttribute("style", "height:" + height.toString() + "px");

        console.log("theme:", theme);

        if (theme == 'bright') {
            d3.select("body")
                .style('background-color', 'white')
                .style('color', 'black');

            d3.select("html")
                .style('background-color', 'white')
                .style('color', 'black');
        } else {
            // dynamically add the dark theme rules
            addStylesheetRules([
                ['.modal-content',
                    ['background-color', 'rgb(0, 0, 0)'],
                    ['background-color', 'rgba(0, 0, 0, 0.5)']
                ],
                ['.list-group-item',
                    ['background-color', 'rgb(0, 0, 0)'],
                    ['background-color', 'rgba(0, 0, 0, 0.5)'],
                    ['color', 'inherit']
                ]
            ]);
        }

        va_count = parseInt(votable.getAttribute('data-va_count'));
        datasetId = votable.getAttribute('data-datasetId');//make it a global variable

        spectrum_stack = new Array(va_count);
        spectrum_scale = new Array(va_count);
        videoFrame = new Array(va_count);
        video_stack = new Array(va_count);

        for (let i = 0; i < va_count; i++) {
            spectrum_stack[i] = [];
            spectrum_scale[i] = 1;
            video_stack[i] = [];
            videoFrame[i] = null;
        };

        if (va_count > 1) {
            datasetId = [];

            for (let i = 0; i < va_count; i++)
                datasetId.push(votable.getAttribute('data-datasetId' + (i + 1)));

            console.log('LINE GROUP:', datasetId);

            //datasetId = votable.getAttribute('data-datasetId1') ;

            if (!composite_view)
                zoom_scale = 1;
        }

        var rect = document.getElementById('mainDiv').getBoundingClientRect();

        //set the default font-size (1em)
        //emFontSize = Math.max(12, 0.011 * 0.5 * (rect.width + rect.height));
        emFontSize = Math.max(12, 0.011 * (0.2 * rect.width + 0.8 * rect.height));
        emStrokeWidth = Math.max(1, 0.1 * emFontSize);
        document.body.style.fontSize = emFontSize + "px";
        //console.log("emFontSize : ", emFontSize.toFixed(2), "emStrokeWidth : ", emStrokeWidth.toFixed(2));

        var width = Math.round(rect.width - 20);
        var height = Math.round(rect.height - 20);

        d3.select("#mainDiv").append("svg")
            .attr("id", "ClusterSVG")
            .attr("width", width)
            .attr("height", 9)
            .attr('style', 'position: fixed; left: 10px; top: 0px; z-index: 0'); // top: -2px

        if (va_count > 1 && !composite_view) {
            for (let index = 0; index < va_count; index++) {
                d3.select("#mainDiv").append("canvas")
                    .attr("id", "HTMLCanvas" + (index + 1))
                    .attr("width", width)
                    .attr("height", height)
                    .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: ' + (index + 1));
            }
        }

        d3.select("#mainDiv").append("canvas")
            .attr("id", "HTMLCanvas")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: ' + (va_count + 1));

        d3.select("#mainDiv").append("canvas")
            .attr("id", "LegendCanvas")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: ' + (va_count + 2));

        d3.select("#mainDiv").append("svg")
            .attr("id", "ContourSVG")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 51');

        d3.select("#mainDiv").append("svg")
            .attr("id", "BackgroundSVG")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 52');

        d3.select("#mainDiv").append("canvas")
            .attr("id", "ZOOMCanvas")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 53');

        d3.select("#mainDiv").append("canvas")
            .attr("id", "ViewportCanvas")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 54');

        d3.select("#mainDiv").append("svg")
            .attr("id", "BackSVG")
            .attr("width", width)
            .attr("height", height)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 55; cursor: default; mix-blend-mode: none');//difference or lighten or screen //other than none causes problems with an older Firefox v45

        //spectrum
        var blend = '';

        if (theme == 'bright')
            blend = 'mix-blend-mode: difference; ';

        d3.select("#mainDiv").append("canvas")
            .attr("id", "SpectrumCanvas")
            .attr("width", width)
            .attr("height", height)
            .attr('style', blend + 'position: fixed; left: 10px; top: 10px; z-index: 55');// mix-blend-mode: difference;

        d3.select("#mainDiv").append("svg")
            .attr("id", "FrontSVG")
            .attr("width", width)
            .attr("height", height)
            .on("mouseenter", hide_navigation_bar)
            .attr('style', 'position: fixed; left: 10px; top: 10px; z-index: 56; cursor: default');

        d3.select("#BackSVG").append("svg:image")
            .attr("id", "jvoLogo")
            .attr("x", (width - 1 - 199))
            .attr("y", (height - 1 - 109))
            .attr("xlink:href", "https://jvo.nao.ac.jp/images/JVO_logo_199x109.png")
            .attr("width", 199)
            .attr("height", 109)
            .attr("opacity", 0.5);

        var has_fits = votable.getAttribute('data-has-fits');
        var display_progress = 'block';
        if (has_fits == 'true')
            display_progress = 'none';

        var div = d3.select("body").append("div")
            .attr("id", "welcome")
            .attr("class", "container welcome")
            .style('display', display_progress);

        var group = div.append("g")
            .attr("id", "welcomeGroup");

        group.append("h1")
            .text("FITSWEBQLSE");

        group.append("p")
            .text(votable.getAttribute('data-server-version') + "/" + get_js_version());

        group.append("p")
            .text("Processing request, please wait ...");

        if (va_count == 1) {
            /*group.append("h4")
            .append("p")
            .text(datasetId) ;*/

            group.append("div")
                .attr("class", "progress")
                .append("div")
                .attr("id", "progress-bar" + va_count)
                .attr("class", "progress-bar progress-bar-info progress-bar-striped active")
                .attr("role", "progressbar")
                .attr("aria-valuenow", 0)
                .attr("aria-valuemin", 0)
                .attr("aria-valuemax", 100)
                .style("width", "0%")
                .html("0%");
        }
        else {
            for (let index = 0; index < va_count; index++) {
                /*group.append("h4")
                  .append("p")
                  .text(datasetId[index]) ;*/

                group.append("div")
                    .attr("class", "progress")
                    .append("div")
                    .attr("id", "progress-bar" + (index + 1))
                    .attr("class", "progress-bar progress-bar-info progress-bar-striped active")
                    .attr("role", "progressbar")
                    .attr("aria-valuenow", 0)
                    .attr("aria-valuemin", 0)
                    .attr("aria-valuemax", 100)
                    .style("width", "0%")
                    .html("0%");
            }
        }

        d3.select("body").append("div")
            .attr("id", "molecularlist")
            .attr("class", "molecularmodal");

        display_range_validation();

        await res3d; display_menu();

        setup_help();

        setup_changelog();

        setup_FITS_header_page();

        if (welcome)
            show_welcome();

        display_hourglass();

        // RGB composite image variables
        compositeImage = null;
        compositeImageTexture = null;
        compositeViewportTexture = null;

        fitsContainer = new Array(va_count);
        imageContainer = new Array(va_count);
        mean_spectrumContainer = new Array(va_count);
        integrated_spectrumContainer = new Array(va_count);
        wsConn = new Array(va_count);

        notifications_received = new Array(va_count);
        previous_progress = new Array(va_count);
        notifications_completed = 0;

        for (let i = 0; i < va_count; i++) {
            fitsContainer[i] = null;
            mean_spectrumContainer[i] = null;
            integrated_spectrumContainer[i] = null;
            imageContainer[i] = null;
            wsConn[i] = null;

            notifications_received[i] = 0;
            previous_progress[i] = -1;
        }

        image_count = 0;
        viewport_count = 0;
        spectrum_count = 0;

        poll_heartbeat();
        poll_cluster();

        dataset_timeout = -1;

        if (va_count == 1) {
            open_websocket_connection(datasetId, 1);
            fetch_image_spectrum(datasetId, 1, true, false);
            fetch_spectral_lines(datasetId, 0, 0);
            poll_progress(datasetId, 1);
        }
        else {
            for (let index = 1; index <= va_count; index++) {
                open_websocket_connection(datasetId.rotate(index - 1).join(";"), index);
                fetch_image_spectrum(datasetId[index - 1], index, true, false);
                poll_progress(datasetId.rotate(index - 1)[0], index);
            }
        }

    }

    firstTime = false;
}
