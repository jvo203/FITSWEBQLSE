// standard global variables
var container, scene, camera, renderer, controls;//, stats;
var keyboard = new THREEx.KeyboardState();
var clock = new THREE.Clock();
var resize, fullscreen;
var wireTexture, geometry, material, plane;

var segments = 512;//512
var is_active;

//get z from imageFrame raw float32 pixels
function meshFunction(x, y, p0) {
    var imageFrame, image_bounding_dims;

    var z;

    if (composite_view) {
        image_bounding_dims = compositeImage.image_bounding_dims;

        let xcoord = Math.round(image_bounding_dims.x1 + (1 - x) * (image_bounding_dims.width - 1));
        let ycoord = Math.round(image_bounding_dims.y1 + (1 - y) * (image_bounding_dims.height - 1));

        let pixel = ycoord * compositeImage.width + xcoord;

        var mean_pixel = 0.0;

        for (index = 1; index <= va_count; index++) {
            imageFrame = imageContainer[index - 1];
            let tone_mapping = imageFrame.tone_mapping;
            let black = tone_mapping.black;
            let white = tone_mapping.white;
            let median = tone_mapping.median;
            let noise_sensitivity = document.getElementById('sensitivity' + index).value;
            let multiplier = get_noise_sensitivity(noise_sensitivity);
            let flux = document.getElementById('flux' + index).value;

            let raw = imageFrame.pixels[pixel];
            let rgb = get_tone_mapping(raw, flux, black, white, median, multiplier, index);
            mean_pixel += rgb;
        }

        //z = imageDataCopy[pixel] - 127;
        mean_pixel /= va_count;
        z = mean_pixel - 127;
    }
    else {
        imageFrame = imageContainer[va_count - 1];
        image_bounding_dims = imageFrame.image_bounding_dims;

        let tone_mapping = imageFrame.tone_mapping;
        let black = tone_mapping.black;
        let white = tone_mapping.white;
        let median = tone_mapping.median;
        let noise_sensitivity = document.getElementById('sensitivity' + va_count).value;
        let multiplier = get_noise_sensitivity(noise_sensitivity);
        let flux = document.getElementById('flux' + va_count).value;

        let xcoord = Math.round(image_bounding_dims.x1 + (1 - x) * (image_bounding_dims.width - 1));
        let ycoord = Math.round(image_bounding_dims.y1 + (1 - y) * (image_bounding_dims.height - 1));

        let pixel = ycoord * imageFrame.width + xcoord;
        let raw = imageFrame.pixels[pixel];
        // <raw> needs to be transformed into a pixel range in [0, 255] via the tone mapping function
        pixel = get_tone_mapping(raw, flux, black, white, median, multiplier, va_count);
        z = pixel - 127;
        //console.log(xcoord, ycoord, "raw:", raw, "pixel:", pixel, "z:", z);
    }

    var aspect = image_bounding_dims.height / image_bounding_dims.width;

    p0.set(x - 0.5, (y - 0.5) * aspect, z / 2048);
}

function colourFunction(x, y) {
    var imageFrame, image_bounding_dims;
    var rgb = [0, 0, 0];

    if (composite_view) {
        image_bounding_dims = compositeImage.image_bounding_dims;

        let aspect = image_bounding_dims.height / image_bounding_dims.width;
        let xcoord = Math.round(image_bounding_dims.x1 + ((1 - x) - 0.5) * (image_bounding_dims.width - 1));
        let ycoord = Math.round(image_bounding_dims.y1 + ((- y) / aspect + 0.5) * (image_bounding_dims.height - 1));
        let pixel = ycoord * compositeImage.width + xcoord;

        for (index = 1; index <= va_count; index++) {
            imageFrame = imageContainer[index - 1];
            let tone_mapping = imageFrame.tone_mapping;
            let black = tone_mapping.black;
            let white = tone_mapping.white;
            let median = tone_mapping.median;
            let noise_sensitivity = document.getElementById('sensitivity' + index).value;
            let multiplier = get_noise_sensitivity(noise_sensitivity);
            let flux = document.getElementById('flux' + index).value;

            let raw = imageFrame.pixels[pixel];
            rgb[index - 1] = Math.round(get_tone_mapping(raw, flux, black, white, median, multiplier, index));
        }
    } else {
        imageFrame = imageContainer[va_count - 1];
        image_bounding_dims = imageFrame.image_bounding_dims;

        let tone_mapping = imageFrame.tone_mapping;
        let black = tone_mapping.black;
        let white = tone_mapping.white;
        let median = tone_mapping.median;
        let noise_sensitivity = document.getElementById('sensitivity' + va_count).value;
        let multiplier = get_noise_sensitivity(noise_sensitivity);
        let flux = document.getElementById('flux' + va_count).value;

        let aspect = image_bounding_dims.height / image_bounding_dims.width;
        let xcoord = Math.round(image_bounding_dims.x1 + ((1 - x) - 0.5) * (image_bounding_dims.width - 1));
        let ycoord = Math.round(image_bounding_dims.y1 + ((- y) / aspect + 0.5) * (image_bounding_dims.height - 1));
        let pixel = 2 * (ycoord * imageFrame.width + xcoord); // the texture is 2 x length

        let raw = imageFrame.texture[pixel];
        pixel = Math.round(get_tone_mapping(raw, flux, black, white, median, multiplier, va_count));
        rgb = [pixel, pixel, pixel];
    }

    return new THREE.Color("rgb(" + rgb[0] + "," + rgb[1] + "," + rgb[2] + ")");
}

function init_surface() {
    var div = d3.select("body").append("div")
        .attr("id", "ThreeJS")
        .attr("class", "threejs");

    div.append("span")
        .attr("id", "closeThreeJS")
        .attr("class", "close myclose")
        .on("click", function () {
            is_active = false;
            d3.select("#ThreeJS").remove();
            resize.destroy();
            fullscreen.unbind();
            wireTexture.dispose();
            geometry.dispose();
            material.dispose();
            scene = null;
            container = null;
            camera = null;
            renderer = null;
            controls = null;
            //stats = null ;
            /*keyboard = null ;
            clock = null ;*/
        })
        .text("×");

    div.append("img")
        .attr("id", "hourglassThreeJS")
        .attr("class", "hourglass")
        .attr("src", "https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/loading.gif")
        .attr("alt", "hourglass")
        .style("width", 200)
        .style("height", 200);

    setTimeout(init_graph, 50);
}

function init_graph() {
    var rect = document.getElementById('mainDiv').getBoundingClientRect();

    var SCREEN_WIDTH = rect.width;
    var SCREEN_HEIGHT = rect.height;

    // SCENE
    scene = new THREE.Scene();

    // CAMERA
    var VIEW_ANGLE = 25, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;
    camera = new THREE.PerspectiveCamera(VIEW_ANGLE, ASPECT, NEAR, FAR);
    scene.add(camera);

    //camera.position.set( 1.1*imageCanvas.width, 1.1*imageCanvas.height, 1024);//0.5*(imageCanvas.width+imageCanvas.height)/2);
    camera.position.set(1.1, 1.1, 1);
    camera.up = new THREE.Vector3(0, 0, 1);

    //camera.position.set(0,-1000,1.25*(imageCanvas.width+imageCanvas.height)/2);
    camera.lookAt(scene.position);

    // RENDERER
    if (Detector.webgl)
        renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    else
        renderer = new THREE.CanvasRenderer();

    renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
    container = document.getElementById('ThreeJS');
    container.appendChild(renderer.domElement);

    // EVENTS
    resize = THREEx.WindowResize(renderer, camera);
    fullscreen = THREEx.FullScreen.bindKey({ charCode: 'm'.charCodeAt(0) });

    // CONTROLS
    controls = new THREE.TrackballControls(camera, renderer.domElement);

    // LIGHT
    scene.add(new THREE.AmbientLight(0x404040 /*0xeeeeee*/));

    geometry = new THREE.ParametricGeometry(meshFunction, segments, segments);

    var color, point, face, numberOfSides, vertexIndex;
    // faces are indexed using characters
    var faceIndices = ['a', 'b', 'c', 'd'];

    for (var i = 0; i < geometry.vertices.length; i++) {
        point = geometry.vertices[i];
        color = colourFunction(point.x, point.y);
        geometry.colors[i] = color;
    }

    for (var i = 0; i < geometry.faces.length; i++) {
        face = geometry.faces[i];
        numberOfSides = (face instanceof THREE.Face3) ? 3 : 4;
        for (var j = 0; j < numberOfSides; j++) {
            vertexIndex = face[faceIndices[j]];
            face.vertexColors[j] = geometry.colors[vertexIndex];
        }
    }

    //wireTexture = new THREE.TextureLoader().load(ROOT_PATH + 'square.png');
    wireTexture = new THREE.TextureLoader().load("https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/square.png")
    wireTexture.wrapS = wireTexture.wrapT = THREE.RepeatWrapping;
    //wireTexture.minFilter = wireTexture.magFilter = THREE.LinearFilter;
    wireTexture.repeat.set(segments, segments);

    material = new THREE.MeshBasicMaterial({
        //color: 0xFFFFFF,
        map: wireTexture,
        vertexColors: THREE.VertexColors,
        side: THREE.DoubleSide,
        wireframe: false
    });

    plane = new THREE.Mesh(geometry, material);
    plane.doubleSided = true;
    scene.add(plane);

    is_active = true;
    animate_surface();

    d3.select("#hourglassThreeJS").remove();
}

function animate_surface() {
    if (!is_active) {
        console.log("exiting animate_surface()");
        return;
    }

    requestAnimationFrame(animate_surface);

    render();
    update();
}

function update() {
    /*if ( keyboard.pressed("z") ) 
    { 
    // do something
    }*/

    controls.update();
    //stats.update();
}

function render() {
    renderer.render(scene, camera);
}
