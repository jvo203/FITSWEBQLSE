using Images, ImageTransformations, Interpolations;

pixels = randn(Float32, 10, 10)

println(pixels, "; dims: ", size(pixels))

res = imresize(pixels, (3, 3))

println(res, "; dims: ", size(res))