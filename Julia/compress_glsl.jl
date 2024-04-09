using JSON
using CodecLz4
using CodecBzip2

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

    # remove any control characters from src
    # src = replace(src, r"[\x00-\x1f]" => "")    

    # prepend any control characters from src with a backslash
    # src = replace(src, r"[\x00-\x1f]" => m -> "\\" * m.match)

    # JSON.Writer.write(json, "{\"id\": \"$id\", \"shader\": \"$src\"}")

    # make a Dict with id and shader
    dict = Dict("id" => id, "shader" => src)

    # write the Dict as JSON
    JSON.Writer.write(json, JSON.json(dict))

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

# write a binary file .bin with the uncompressed json length as UInt32 and the compressed json
#=open("../htdocs/fitswebql/glsl_shaders.bin", "w") do f
    write(f, UInt32(length(json)))
    write(f, compressed)
end=#

# ==================================================================================================
#
# The JSON approach was abandoned because the JSON strings were not properly escaped.
# And after properly escaping the shader JSON strings the total compressed size went up from 26kB to around 40kB.
# The XML approach was chosen instead.
#
# ==================================================================================================

# iterate through shaders and write to XML as "<shader id="id">shader</shader>"
# make an XML array with shader as "<shader id="id">shader</shader>" where the id is the filename without the extension

# create a root XML document
xml = "<shaders>"

for shader in shaders
    global xml

    id = split(shader, ".")[1]
    println("Processing $shader, id: $id")

    # read the text file as String
    src = read(src_dir * shader, String)

    # escape &, < and > characters
    src = replace(src, "&" => "&amp;")
    src = replace(src, "<" => "&lt;")
    src = replace(src, ">" => "&gt;")

    # create an XML element
    xml *= "<shader id=\"$id\">$src</shader>"
end

xml *= "</shaders>"

# convert String to UInt8 array
xml = [UInt8(c) for c in xml]

compressed = lz4_hc_compress(xml)

# compress with bzip2 (more efficient than LZ4HC)
compressed2 = transcode(Bzip2Compressor, xml)

# print the JSON object length
println("XML object length: ", length(xml))
println("Compressed XML object length (LZ4HC): ", length(compressed))
println("Compressed XML object length (BZIP2): ", length(compressed2))

# write a binary file .bin with the uncompressed xml length as UInt32 and the compressed json
open("../htdocs/fitswebql/glsl_shaders.bin", "w") do f
    write(f, UInt32(length(xml)))
    write(f, compressed2)
end