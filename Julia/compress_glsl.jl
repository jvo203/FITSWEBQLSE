using JSON
using CodecLz4

shaders = ["vertex-shader.vert", "legend-vertex-shader.vert", "rgba-shader.frag", "common-shader.frag", "legend-common-shader.frag", "ratio-shader.frag", "ratio-composite-shader.frag", "logistic-shader.frag", "logistic-composite-shader.frag", "square-shader.frag", "square-composite-shader.frag", "legacy-shader.frag", "legacy-composite-shader.frag", "linear-shader.frag", "linear-composite-shader.frag", "composite-shader.frag", "greyscale-shader.frag", "negative-shader.frag", "amber-shader.frag", "red-shader.frag", "green-shader.frag", "blue-shader.frag", "hot-shader.frag", "rainbow-shader.frag", "parula-shader.frag", "inferno-shader.frag", "magma-shader.frag", "plasma-shader.frag", "viridis-shader.frag", "cubehelix-shader.frag", "jet-shader.frag", "haxby-shader.frag"]
println("Compressing shaders:", shaders)

src_dir = "../htdocs/fitswebql/"

# make a JSON array with shader as {"id": id, "shader": shader} where the id is the filename without the extension
json = JSON.Writer.IOBuffer()
JSON.Writer.write(json, "[")
for shader in shaders
    id = split(shader, ".")[1]
    println("Processing $shader, id: $id")

    # read the text file as String
    src = read(src_dir * shader, String)

    JSON.Writer.write(json, "{\"id\": \"$id\", \"shader\": \"$src\"}")

    if shader != shaders[end]
        JSON.Writer.write(json, ",")
    end
end
JSON.Writer.write(json, "]")

json = take!(json)
compressed = lz4_hc_compress(json)

# print the JSON object length
println("JSON object length: ", length(json))
println("Compressed JSON object length: ", length(compressed))

# write the JSON object to a file
#open("../htdocs/fitswebql/shaders.json", "w") do f
#    write(f, JSON.Writer.print(json))
#end

# write a binary file .bin with the uncompressed json length as UInt32 and the compressed json
open("../htdocs/fitswebql/glsl_shaders.bin", "w") do f
    write(f, UInt32(length(json)))
    write(f, compressed)
end