include("ipp_toolchain.jl")

# Compile the code
ipp = load_ipp("../src/ipp.c", `-O3`)

# function pointers
resizeCubic32F = Libc.Libdl.dlsym(ipp, "resizeCubic")
resizeLanczos32F = Libc.Libdl.dlsym(ipp, "resizeLanczos")
resizeSuper32F = Libc.Libdl.dlsym(ipp, "resizeSuper")
resizeNearest8U = Libc.Libdl.dlsym(ipp, "resizeNearest")

dim = 10
view = 3

pixels = 100.0f0 * randn(Float32, dim, dim)
pixels2 = Matrix{Float32}(undef, view, view)

display(pixels)

# Call the kernel:
# ccall(resizeCubic32F, Cvoid, (Ref{Float32}, Cint, Cint, Ref{Float32}, Cint, Cint), pointer(pixels), dim, dim, pointer(pixels2), view, view)

display(pixels2)

mutable struct IppiSize
    width::Cint
    height::Cint
end
