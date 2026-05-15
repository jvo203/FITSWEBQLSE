let THREE = null;
let OrbitControls = null;

let container;
let camera;
let controls;
let scene;
let renderer;
let mesh;
let wireTexture;
let geometry;
let material;
let isActive = false;
let initTimer = 0;

const segments = 512;
let surfacePoint = null;
let threeReadyPromise = null;

function ensureThreeDeps() {
    if (THREE != null && OrbitControls != null) {
        return Promise.resolve();
    }

    if (threeReadyPromise != null) {
        return threeReadyPromise;
    }

    threeReadyPromise = new Promise((resolve) => {
        function attemptResolve() {
            if (window.THREE != null && window.OrbitControls != null) {
                THREE = window.THREE;
                OrbitControls = window.OrbitControls;
                surfacePoint = new THREE.Vector3();
                resolve();
                return;
            }

            setTimeout(attemptResolve, 10);
        }

        attemptResolve();
    });

    return threeReadyPromise;
}

function getMainRect() {
    return document.getElementById('mainDiv').getBoundingClientRect();
}

function getSurfaceDimensions() {
    let imageBoundingDims;

    if (composite_view) {
        imageBoundingDims = compositeImage.image_bounding_dims;
    } else {
        const imageFrame = imageContainer[previous_plane - 1];
        imageBoundingDims = imageFrame.image_bounding_dims;
    }

    return imageBoundingDims;
}

function getAspectRatio() {
    const imageBoundingDims = getSurfaceDimensions();
    return imageBoundingDims.height / imageBoundingDims.width;
}

function getToneMappedPixel(imageFrame, pixel, index) {
    const toneMapping = imageFrame.tone_mapping;
    const noiseSensitivity = document.getElementById('sensitivity' + index).value;
    const multiplier = get_noise_sensitivity(noiseSensitivity);
    const flux = document.getElementById('flux' + index).value;
    const raw = imageFrame.pixels[pixel];

    return get_tone_mapping(raw, flux, toneMapping.black, toneMapping.white, toneMapping.median, multiplier, index);
}

function meshFunction(x, y, target) {
    let imageFrame;
    let imageBoundingDims;
    let z;

    if (composite_view) {
        imageBoundingDims = compositeImage.image_bounding_dims;

        const xcoord = Math.round(imageBoundingDims.x1 + (1 - x) * (imageBoundingDims.width - 1));
        const ycoord = Math.round(imageBoundingDims.y1 + (1 - y) * (imageBoundingDims.height - 1));
        const pixel = ycoord * compositeImage.width + xcoord;

        let meanPixel = 0.0;

        for (let index = 1; index <= va_count; index++) {
            imageFrame = imageContainer[index - 1];
            meanPixel += getToneMappedPixel(imageFrame, pixel, index);
        }

        meanPixel /= va_count;
        z = meanPixel - 127;
    } else {
        const index = previous_plane;

        imageFrame = imageContainer[index - 1];
        imageBoundingDims = imageFrame.image_bounding_dims;

        const xcoord = Math.round(imageBoundingDims.x1 + (1 - x) * (imageBoundingDims.width - 1));
        const ycoord = Math.round(imageBoundingDims.y1 + (1 - y) * (imageBoundingDims.height - 1));
        const pixel = ycoord * imageFrame.width + xcoord;

        z = getToneMappedPixel(imageFrame, pixel, index) - 127;
    }

    const aspect = imageBoundingDims.height / imageBoundingDims.width;

    target.set(x - 0.5, (y - 0.5) * aspect, z / 2048);
}

function colourFunction(x, y) {
    let imageFrame;
    let imageBoundingDims;
    let rgb = [0, 0, 0];

    if (composite_view) {
        imageBoundingDims = compositeImage.image_bounding_dims;

        const aspect = imageBoundingDims.height / imageBoundingDims.width;
        const xcoord = Math.round(imageBoundingDims.x1 + ((1 - x) - 0.5) * (imageBoundingDims.width - 1));
        const ycoord = Math.round(imageBoundingDims.y1 + ((-y) / aspect + 0.5) * (imageBoundingDims.height - 1));
        const pixel = ycoord * compositeImage.width + xcoord;

        for (let index = 1; index <= va_count; index++) {
            imageFrame = imageContainer[index - 1];
            rgb[index - 1] = Math.round(getToneMappedPixel(imageFrame, pixel, index));
        }
    } else {
        const index = previous_plane;

        imageFrame = imageContainer[index - 1];
        imageBoundingDims = imageFrame.image_bounding_dims;

        const aspect = imageBoundingDims.height / imageBoundingDims.width;
        const xcoord = Math.round(imageBoundingDims.x1 + ((1 - x) - 0.5) * (imageBoundingDims.width - 1));
        const ycoord = Math.round(imageBoundingDims.y1 + ((-y) / aspect + 0.5) * (imageBoundingDims.height - 1));
        const pixel = ycoord * imageFrame.width + xcoord;
        const toneMappedPixel = Math.round(getToneMappedPixel(imageFrame, pixel, index));

        rgb = [toneMappedPixel, toneMappedPixel, toneMappedPixel];
    }

    return new THREE.Color('rgb(' + rgb[0] + ',' + rgb[1] + ',' + rgb[2] + ')');
}

function disposeSurfaceResources() {
    window.removeEventListener('resize', onWindowResize);

    if (renderer != null) {
        renderer.setAnimationLoop(null);
        renderer.dispose();
    }

    if (controls != null) {
        controls.dispose();
    }

    if (wireTexture != null) {
        wireTexture.dispose();
    }

    if (geometry != null) {
        geometry.dispose();
    }

    if (material != null) {
        material.dispose();
    }

    container = null;
    camera = null;
    controls = null;
    scene = null;
    renderer = null;
    mesh = null;
    wireTexture = null;
    geometry = null;
    material = null;
}

function closeSurface() {
    if (initTimer !== 0) {
        clearTimeout(initTimer);
        initTimer = 0;
    }

    isActive = false;
    disposeSurfaceResources();
    d3.select('#ThreeJS').remove();
}

function buildGeometry() {
    geometry = new THREE.PlaneGeometry(1, getAspectRatio(), segments, segments);

    const position = geometry.attributes.position;
    const colors = new Float32Array(position.count * 3);

    for (let iy = 0; iy <= segments; iy++) {
        for (let ix = 0; ix <= segments; ix++) {
            const index = iy * (segments + 1) + ix;
            const x = ix / segments;
            const y = iy / segments;

            meshFunction(x, y, surfacePoint);
            position.setXYZ(index, surfacePoint.x, surfacePoint.y, surfacePoint.z);

            const color = colourFunction(surfacePoint.x, surfacePoint.y);
            colors[3 * index] = color.r;
            colors[3 * index + 1] = color.g;
            colors[3 * index + 2] = color.b;
        }
    }

    geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
    geometry.computeVertexNormals();
}

function initSurfaceScene() {
    initTimer = 0;

    if (!isActive) {
        return;
    }

    const rect = getMainRect();
    const screenWidth = rect.width;
    const screenHeight = rect.height;

    scene = new THREE.Scene();

    camera = new THREE.PerspectiveCamera(45, screenWidth / screenHeight, 0.01, 100);
    camera.position.set(1.15, -1.35, 0.85);
    camera.up.set(0, 0, 1);

    renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(screenWidth, screenHeight);
    renderer.setAnimationLoop(animateSurface);

    container = document.getElementById('ThreeJS');
    container.appendChild(renderer.domElement);

    controls = new OrbitControls(camera, renderer.domElement);
    controls.enableDamping = true;
    controls.enablePan = false;
    controls.minDistance = 0.35;
    controls.maxDistance = 4.0;
    controls.maxPolarAngle = Math.PI / 2;
    controls.target.set(0, 0, 0);
    controls.update();

    scene.add(new THREE.AmbientLight(0xffffff, 1.8));

    const directionalLight = new THREE.DirectionalLight(0xffffff, 1.2);
    directionalLight.position.set(1.5, -1.0, 2.0);
    scene.add(directionalLight);

    buildGeometry();

    wireTexture = new THREE.TextureLoader().load('https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/square.png');
    wireTexture.wrapS = THREE.RepeatWrapping;
    wireTexture.wrapT = THREE.RepeatWrapping;
    wireTexture.repeat.set(segments, segments);
    wireTexture.colorSpace = THREE.SRGBColorSpace;

    material = new THREE.MeshPhongMaterial({
        map: wireTexture,
        vertexColors: true,
        side: THREE.DoubleSide
    });

    mesh = new THREE.Mesh(geometry, material);
    scene.add(mesh);
    window.addEventListener('resize', onWindowResize);

    d3.select('#hourglassThreeJS').remove();
}

function init_surface() {
    if (isActive) {
        closeSurface();
    }

    const div = d3.select('body').append('div')
        .attr('id', 'ThreeJS')
        .attr('class', 'threejs');

    div.append('span')
        .attr('id', 'closeThreeJS')
        .attr('class', 'close myclose')
        .on('click', closeSurface)
        .text('×');

    div.append('img')
        .attr('id', 'hourglassThreeJS')
        .attr('class', 'hourglass')
        .attr('src', 'https://cdn.jsdelivr.net/gh/jvo203/fits_web_ql/htdocs/fitswebql/loading.gif')
        .attr('alt', 'hourglass')
        .style('width', 200)
        .style('height', 200);

    isActive = true;

    ensureThreeDeps().then(() => {
        if (!isActive) {
            return;
        }

        initTimer = setTimeout(initSurfaceScene, 50);
    });
}

function onWindowResize() {
    if (camera == null || renderer == null) {
        return;
    }

    const rect = getMainRect();
    camera.aspect = rect.width / rect.height;
    camera.updateProjectionMatrix();
    renderer.setSize(rect.width, rect.height);
}

function animateSurface() {
    if (!isActive || renderer == null) {
        return;
    }

    controls.update();
    renderer.render(scene, camera);
}

window.init_surface = init_surface;