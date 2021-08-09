using Serialization;
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

view_pixels = deserialize("/Users/chris/view_pixels.bin")
view_mask = deserialize("/Users/chris/view_mask.bin")

view_width = Int64(372)
view_height = Int64(372)

display(view_pixels[1:5, 1:5])
display(view_mask[1:5, 1:5])

view_pixels = Float32.(imresize(view_pixels, (view_width, view_height)),)
view_mask =
    Bool.(
        imresize(
            view_mask,
            (Integer(view_width), Integer(view_height)),
            method = Constant(),
        ),
    ) # use Nearest-Neighbours for the mask
