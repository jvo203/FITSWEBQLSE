using ZfpCompression;

width = 100
height = 67
precision = 8

pixels = 100.0 * randn(Float32, width, height)

compressed_pixels = zfp_compress(pixels,precision=precision)

println("compressed size:", length(compressed_pixels))
