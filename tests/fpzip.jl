using fpzip_jll

const FPZIP_MEDIUM_PRECISION = 16
const FPZIP_HIGH_PRECISION = 24

depth = 768
spectrum = Float32(100.0) * randn(Float32, depth)

function fpzip_compress(A::Array{Float32,1})

    bufbytes = 1024 + length(A) * sizeof(eltype(A))
    compressed = Vector{UInt8}(undef, bufbytes)

    fpz = ccall((:fpzip_write_to_buffer, libfpzip), Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), compressed, bufbytes)
    println("fpz: $fpz")

end

fpzip_compress(spectrum)
