include("ipp_toolchain.jl")

# Compile the code
ipp = load_ipp("../src/ipp.c", `-O3`)

# function pointers
resizeCubic32F = Libc.Libdl.dlsym(ipp, "resizeCubic")
# resizeLanczos32F = Libc.Libdl.dlsym(ipp, "resizeLanczos")
# resizeSuper32F = Libc.Libdl.dlsym(ipp, "resizeSuper")
# resizeNearest8U = Libc.Libdl.dlsym(ipp, "resizeNearest")

dim = 10
view = 3

pixels = 100.0f0 * randn(Float32, dim, dim)
#display(pixels)

#=
pixels2 = Matrix{Float32}(undef, view, view)

# Call the kernel:
ccall(
    resizeCubic32F,
    Cvoid,
    (Ref{Float32}, Cint, Cint, Ref{Float32}, Cint, Cint),
    pointer(pixels),
    dim,
    dim,
    pointer(pixels2),
    view,
    view,
)

display(pixels2)
=#

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

const ippNearest = 1
const ippCubic = 6
const ippSuper = 8
const ippLanczos = 16

function resizeCubic32fC1R(src::Matrix{Float32}, width::Integer, height::Integer)
    dims = size(src)
    srcSize = IppiSize(dims[1], dims[2])
    dstSize = IppiSize(width, height)

    specSize = Ref{Cint}(0)
    initSize = Ref{Cint}(0)

    # Spec and init buffer sizes
    status = ccall(
        (:ippiResizeGetSize_32f, ipplib * "/libippi.so"),
        Cint,
        (IppiSize, IppiSize, Cint, Cint, Ref{Cint}, Ref{Cint}),
        srcSize,
        dstSize,
        ippCubic,
        0,
        specSize,
        initSize,
    )

    println("status: $status; specSize: $(specSize[]), initSize: $(initSize[])")

    if status != 0
        error("ippiResizeGetSize_32f::$status")
    end

    # Memory allocation
    pInitBuf =
        ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), initSize[])
    println("pInitBuf:", pInitBuf)

    pSpec = ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), specSize[])
    println("pSpec:", pSpec)

    if pInitBuf == C_NULL || pSpec == C_NULL
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pInitBuf)
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("NULL pInitBuf or pSpec")
    end

    valueB = 0.0f0
    valueC = 0.0f0

    # Filter initialization
    status = ccall(
        (:ippiResizeCubicInit_32f, ipplib * "/libippi.so"),
        Cint,
        (IppiSize, IppiSize, Cfloat, Cfloat, Ptr{Cvoid}, Ptr{Cvoid}),
        srcSize,
        dstSize,
        valueB,
        valueC,
        pSpec,
        pInitBuf,
    )
    ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pInitBuf)
    println("ippiResizeCubicInit_32f::$status")

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeCubicInit_32f::$status")
    end

    borderSize = Ref{IppiBorderSize}(IppiBorderSize(0, 0, 0, 0))

    ccall(
        (:ippiResizeGetBorderSize_32f, ipplib * "/libippi.so"),
        Cint,
        (Ptr{Cvoid}, Ref{IppiBorderSize}),
        pSpec,
        borderSize,
    )
    println("ippiResizeGetBorderSize_32f::$status, borderSize:", borderSize[])

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeGetBorderSize_32f::$status")
    end

    # a destination buffer
    dst = Matrix{Float32}(undef, width, height)

    # Release memory
    ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)

    return dst
end

res = resizeCubic32fC1R(pixels, view, view)
display(res)