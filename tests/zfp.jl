import Base.Iterators:flatten

using CodecLz4;
using ZfpCompression;

width = 14
height = 6
precision = 8

pixels = Float32(100.0) * randn(Float32, width, height)
mask = map(x -> x > -90, pixels)

compressed_pixels = zfp_compress(pixels, precision=precision)
compressed_mask = transcode(LZ4HCCompressor, collect(flatten(UInt8.(mask))))

println("compressed pixels size:", length(compressed_pixels))
println("compressed mask size:", length(compressed_mask))

compressed_mask = lz4_hc_compress(collect(flatten(UInt8.(mask))))
println("compressed mask size:", length(compressed_mask))

decomp = zfp_decompress(compressed_pixels)

display(pixels)
display(decomp)

# 1D spectrum compression
depth = 768
spectrum = Float32(100.0) * randn(Float32, depth)
precision = 24

compressed_spectrum = zfp_compress(spectrum, precision=precision)
println("compressed spectrum size: ", length(compressed_spectrum))

decomp = zfp_decompress(compressed_spectrum)

display(spectrum)
display(decomp)