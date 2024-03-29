using fpzip_jll

# lib = "/usr/local/lib/libfpzip.1.3.0.dylib"

const FPZIP_TYPE_FLOAT = 0
const FPZIP_TYPE_DOUBLE = 1

# array meta data and stream handle
mutable struct FPZ
    type::Cint
    prec::Cint
    nx::Cint
    ny::Cint
    nz::Cint
    nw::Cint
    nf::Cint    
end

FPZ(meta::Ptr) = unsafe_load(Ptr{FPZ}(meta))

const FPZIP_MEDIUM_PRECISION = 16
const FPZIP_HIGH_PRECISION = 24

depth = 768
spectrum = Float32(100.0) * randn(Float32, depth)

function fpzip_compress(src::Array{Float32,1}, precision::Integer)

    ndims = length(size(src))
    ndims in [1] || throw(DimensionMismatch("FPZIP compression only for a 1D array (for the time being)."))

    local status, outbytes

    bufsize = 1024 + length(src) * sizeof(eltype(src))
    dest = Vector{UInt8}(undef, bufsize)
    # dest = zeros(UInt8, bufsize)

    # compress to memory
    fpz = ccall((:fpzip_write_to_buffer, libfpzip), Ptr{FPZ}, (Ptr{Cvoid}, Csize_t), dest, bufsize)

    # convert a pointer into Julia struct to access nx,ny,nz,nf etc.
    meta = FPZ(fpz)

    println("an empty fpz: ", meta)

    # fill in the metadata
    meta.type = FPZIP_TYPE_FLOAT
    meta.prec = precision
    meta.nx = length(src)
    meta.ny = 1
    meta.nz = 1
    meta.nf = 1

    println("after filling: ", meta)

    # sync the Julia structure back to C    
    unsafe_store!(Ptr{FPZ}(fpz), meta)

    meta2 = FPZ(fpz)
    println("meta copied to fpz; reconstructed from fpz: ", meta2)

    println("got here#1")

    # write header
    status = ccall((:fpzip_write_header, libfpzip), Cint, (Ptr{Cvoid},), fpz)
    
    println("got here#2")

    if status == 0    
        throw(error("Writing FPZIP header failed."))
    else
        # compress the data        
        outbytes = ccall((:fpzip_write, libfpzip), Csize_t, (Ptr{Cvoid}, Ptr{Cvoid}), fpz, src)
        println("outbytes = $outbytes")
    end

    # release the metadata structure
    ccall((:fpzip_write_close, libfpzip), Cvoid, (Ptr{Cvoid},), fpz)

    if outbytes > 0
        return dest[1:outbytes]
    else
        return Nothing
    end
end

fpzip_compress(spectrum, FPZIP_MEDIUM_PRECISION)
