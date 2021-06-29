using Images, ImageTransformations, Interpolations;

dim = 10
view = 3

pixels = 100.0 * randn(Float32, dim, dim)

println("; dims: ", size(pixels))

@time res = Float32.(imresize(pixels, (view, view)))

# println(res, "; dims: ", size(res))
display(res)

data = rand(dim, dim)
mask = map(x -> (x > 0.5), data)
println(mask, typeof(mask))

res = Bool.(imresize(mask, (view, view), method = Constant()))
println(res, typeof(res))
display(res)
