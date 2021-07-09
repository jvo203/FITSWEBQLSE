using fpzip_jll

const FPZIP_TYPE_FLOAT = 0
const FPZIP_TYPE_DOUBLE = 1

# array meta data and stream handle
mutable struct FPZ
    type::Cint
    prec::Cint
    nx::Cuint
    ny::Cuint
    nz::Cuint
    nw::Cuint
    nf::Cint    
end

FPZ(stream::Ptr) = unsafe_load(Ptr{FPZ}(stream))

const FPZIP_MEDIUM_PRECISION = 16
const FPZIP_HIGH_PRECISION = 24

depth = 768
spectrum = Float32(100.0) * randn(Float32, depth)

function fpzip_compress(A::Array{Float32,1}, precision::Integer)

    bufsize = 1024 + length(A) * sizeof(eltype(A))
    dest = Vector{UInt8}(undef, bufsize)

    fpz = ccall((:fpzip_write_to_buffer, libfpzip), Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t), dest, bufsize)

    # convert a pointer into Julia struct to access nx,ny,nz,nf etc.
    JFPZ = FPZ(fpz)

    JFPZ.type = FPZIP_TYPE_FLOAT
    JFPZ.prec = precision
    JFPZ.nx = length(A)
    JFPZ.ny = 1
    JFPZ.nz = 1
    JFPZ.nf = 1

    display(JFPZ)
end

fpzip_compress(spectrum, FPZIP_MEDIUM_PRECISION)
