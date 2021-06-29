using Images, ImageTransformations, Interpolations;

dim = 11000
view = Integer(512)

pixels = 100.0 * randn(Float32, dim, dim)

println("; dims: ", size(pixels))

@time res = imresize(pixels, (view, view))

println(res[1:100], "; dims: ", size(res))