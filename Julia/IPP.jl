# Set the IPP library path
@static if Sys.isapple()
    ipplib = ENV["IPPROOT"] * "/lib"
end

@static if Sys.islinux()
    ipplib = ENV["IPPROOT"] * "/lib/intel64"
end

@static if Sys.iswindows()
    error("Unsupported OS: Windows")
end

const ippC1 = 1
const ippBorderRepl = 1

const ippNearest = 1
const ippCubic = 6
const ippSuper = 8
const ippLanczos = 16

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

function resizeCubic32fC1R(src::Matrix{Float32}, width::Integer, height::Integer)
    # a destination buffer
    dst = Matrix{Float32}(undef, width, height)

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

    if status != 0
        error("ippiResizeGetSize_32f::$status")
    end

    # Memory allocation
    pInitBuf =
        ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), initSize[])

    pSpec = ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), specSize[])

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

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeCubicInit_32f::$status")
    end

    borderSize = Ref{IppiBorderSize}(IppiBorderSize(0, 0, 0, 0))

    status = ccall(
        (:ippiResizeGetBorderSize_32f, ipplib * "/libippi.so"),
        Cint,
        (Ptr{Cvoid}, Ref{IppiBorderSize}),
        pSpec,
        borderSize,
    )

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeGetBorderSize_32f::$status")
    end

    bufSize = Ref{Cint}(0)

    # General transform function
    status = ccall(
        (:ippiResizeGetBufferSize_32f, ipplib * "/libippi.so"),
        Cint,
        (Ptr{Cvoid}, IppiSize, Cint, Ref{Cint}),
        pSpec,
        dstSize,
        ippC1,
        bufSize,
    )

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeGetBufferSize_32f::$status")
    end

    pBuffer =
        ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), bufSize[])

    if pBuffer != C_NULL
        srcOffset = IppiPoint(0, 0)
        dstOffset = IppiPoint(0, 0)

        status = ccall(
            (:ippiResizeGetSrcRoi_32f, ipplib * "/libippi.so"),
            Cint,
            (Ptr{Cvoid}, IppiPoint, IppiSize, Ref{IppiPoint}, Ref{IppiSize}),
            pSpec,
            dstOffset,
            dstSize,
            srcOffset,
            srcSize,
        )

        if status == 0
            # finally resize the image            
            srcStep = strides(src)[2] * sizeof(Float32)
            dstStep = strides(dst)[2] * sizeof(Float32)

            status = ccall(
                (:ippiResizeCubic_32f_C1R, ipplib * "/libippi.so"),
                Cint,
                (
                    Ptr{Cfloat},
                    Cint,
                    Ptr{Cfloat},
                    Cint,
                    IppiPoint,
                    IppiSize,
                    Cint,
                    Ptr{Cfloat},
                    Ptr{Cvoid},
                    Ptr{Cvoid},
                ),
                src,
                srcStep,
                dst,
                dstStep,
                dstOffset,
                dstSize,
                ippBorderRepl,
                C_NULL,
                pSpec,
                pBuffer,
            )

        end

        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pBuffer)
    end

    # Release memory
    ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)

    return dst
end

function resizeNearest8uC1R(src::Matrix{UInt8}, width::Integer, height::Integer)
    # a destination buffer
    dst = Matrix{UInt8}(undef, width, height)

    dims = size(src)
    srcSize = IppiSize(dims[1], dims[2])
    dstSize = IppiSize(width, height)

    specSize = Ref{Cint}(0)
    initSize = Ref{Cint}(0)

    # Spec and init buffer sizes
    status = ccall(
        (:ippiResizeGetSize_8u, ipplib * "/libippi.so"),
        Cint,
        (IppiSize, IppiSize, Cint, Cint, Ref{Cint}, Ref{Cint}),
        srcSize,
        dstSize,
        ippNearest,
        0,
        specSize,
        initSize,
    )

    println("status: $status; specSize: $(specSize[]), initSize: $(initSize[])")

    if status != 0
        error("ippiResizeGetSize_8u::$status")
    end

    # Memory allocation
    pSpec = ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), specSize[])
    println("pSpec:", pSpec)

    if pSpec == C_NULL
        error("NULL pSpec")
    end

    # Filter initialization
    status = ccall(
        (:ippiResizeNearestInit_8u, ipplib * "/libippi.so"),
        Cint,
        (IppiSize, IppiSize, Ptr{Cvoid}),
        srcSize,
        dstSize,
        pSpec,
    )
    println("ippiResizeNearestInit_8u::$status")

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeNearestInit_8u::$status")
    end

    bufSize = Ref{Cint}(0)

    # General transform function
    status = ccall(
        (:ippiResizeGetBufferSize_8u, ipplib * "/libippi.so"),
        Cint,
        (Ptr{Cvoid}, IppiSize, Cint, Ref{Cint}),
        pSpec,
        dstSize,
        ippC1,
        bufSize,
    )
    println("ippiResizeGetBufferSize_8u::$status, bufSize:", bufSize[])

    if status != 0
        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)
        error("ippiResizeGetBufferSize_8u::$status")
    end

    pBuffer =
        ccall((:ippsMalloc_8u, ipplib * "/libipps.so"), Ptr{Cvoid}, (Cint,), bufSize[])
    println("pBuffer:", pBuffer)

    if pBuffer != C_NULL
        srcOffset = IppiPoint(0, 0)
        dstOffset = IppiPoint(0, 0)

        status = ccall(
            (:ippiResizeGetSrcRoi_8u, ipplib * "/libippi.so"),
            Cint,
            (Ptr{Cvoid}, IppiPoint, IppiSize, Ref{IppiPoint}, Ref{IppiSize}),
            pSpec,
            dstOffset,
            dstSize,
            srcOffset,
            srcSize,
        )
        println("ippiResizeGetSrcRoi_8u::$status")

        if status == 0
            # finally resize the image            
            srcStep = strides(src)[2] * sizeof(UInt8)
            dstStep = strides(dst)[2] * sizeof(UInt8)

            status = ccall(
                (:ippiResizeNearest_8u_C1R, ipplib * "/libippi.so"),
                Cint,
                (Ptr{UInt8}, Cint, Ptr{UInt8}, Cint, IppiPoint, IppiSize, Cint, Ptr{Cvoid}),
                src,
                srcStep,
                dst,
                dstStep,
                dstOffset,
                dstSize,
                pSpec,
                pBuffer,
            )
            println("ippiResizeNearest_8u_C1R::$status")

        end

        ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pBuffer)
    end

    # Release memory
    ccall((:ippsFree, ipplib * "/libipps.so"), Cvoid, (Ptr{Cvoid},), pSpec)

    return dst
end