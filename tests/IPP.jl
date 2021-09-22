include("ipp_toolchain.jl")

# Compile the code
# ipp = load_ipp("../src/ipp.c", `-O3`)

# function pointers
# resizeCubic32F = Libc.Libdl.dlsym(ipp, "resizeCubic")
# resizeLanczos32F = Libc.Libdl.dlsym(ipp, "resizeLanczos")
# resizeSuper32F = Libc.Libdl.dlsym(ipp, "resizeSuper")
# resizeNearest8U = Libc.Libdl.dlsym(ipp, "resizeNearest")

dim = 10
view = 3

pixels = 100.0f0 * randn(Float32, dim, dim)
pixels2 = Matrix{Float32}(undef, view, view)

#display(pixels)

# Call the kernel:
# ccall(resizeCubic32F, Cvoid, (Ref{Float32}, Cint, Cint, Ref{Float32}, Cint, Cint), pointer(pixels), dim, dim, pointer(pixels2), view, view)

display(pixels2)

mutable struct IppiSize
    width::Cint
    height::Cint
end

mutable struct IppiPoint
    x::Cint
    y::Cint
end

mutable struct IppiBorderSize
    borderLeft::Cint
    borderTop::Cint
    borderRight::Cint
    borderBottom::Cint
end

const ippC1 = 1
const ippBorderRepl = 1

function resizeCubic32fC1R(src::Matrix{Float32}, width::Integer, height::Integer)
    dims = size(src)
    srcSize = IppiSize(dims[1], dims[2])
    dstSize = IppiSize(width, height)

    # a destination buffer
    dst = Matrix{Float32}(undef, width, height)

    return dst
end

res = resizeCubic32fC1R(pixels, view, view)
display(res)