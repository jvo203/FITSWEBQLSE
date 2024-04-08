import ubjson
import lz4.frame

shaders = ["vertex-shader.vert", "legend-vertex-shader.vert", "rgba-shader.frag", "common-shader.frag", "legend-common-shader.frag", "ratio-shader.frag", "ratio-composite-shader.frag", "logistic-shader.frag", "logistic-composite-shader.frag", "square-shader.frag", "square-composite-shader.frag", "legacy-shader.frag", "legacy-composite-shader.frag", "linear-shader.frag", "linear-composite-shader.frag", "composite-shader.frag", "greyscale-shader.frag", "negative-shader.frag", "amber-shader.frag", "red-shader.frag", "green-shader.frag", "blue-shader.frag", "hot-shader.frag", "rainbow-shader.frag", "parula-shader.frag", "inferno-shader.frag", "magma-shader.frag", "plasma-shader.frag", "viridis-shader.frag", "cubehelix-shader.frag", "jet-shader.frag", "haxby-shader.frag"]
print("Compressing shaders:", shaders)

# make an array of ids by stripping the file extension
ids = [shader.split(".")[0] for shader in shaders]

print("ids:", ids)

src_dir = "../htdocs/fitswebql/"

# go through each shader, get its original file size, store it in an array, compress with LZ4
# store the compressed array in another array

original_sizes = []
compressed_sizes = []
compressed_shaders = []

for shader in shaders:
    with open(src_dir + shader, "rb") as f:
        data = f.read()
        original_sizes.append(len(data))
        compressed = lz4.frame.compress(data)  
        compressed_sizes.append(len(compressed))
        compressed_shaders.append(compressed)

print("Original sizes:", original_sizes)
print("Compressed sizes:", compressed_sizes)

# get a total uncompressed size and total compressed size
total_original_size = sum(original_sizes)
total_compressed_size = sum(compressed_sizes)

print("Total original size:", total_original_size)
print("Total compressed size:", total_compressed_size)