using x265_jll;

mutable struct x265_picture
    pts::Clong
    dts::Clong
    userData::Ptr{Cvoid}
    planeR::Ptr{Cuchar}
    planeG::Ptr{Cuchar}
    planeB::Ptr{Cuchar}
    strideR::Cint
    strideG::Cint
    strideB::Cint
    bitDepth::Cint
end

x265_picture(picture::Ptr) = unsafe_load(Ptr{x265_picture}(picture))

function x265_apiver()
    @static if Sys.isapple()
        parts = split(x265_jll.get_libx265_path(), ".")
        return parts[length(parts)-1]
    end

    @static if Sys.islinux()
        parts = split(readlink(x265_jll.get_libx265_path()), ".")
        return last(parts)
    end

    @static if Sys.iswindows()
        error("Not implemented: don't know how to access a shared lib on Windows")
    end
end

param = C_NULL
encoder = C_NULL
picture = C_NULL

# x265 parameters
param = ccall((:x265_param_alloc, libx265), Ptr{Cvoid}, ())
println("typeof(param): ", typeof(param), "; value: $param")

if param == C_NULL
    error("NULL x265_param")
end

# default parameters
ccall(
    (:x265_param_default_preset, libx265),
    Cvoid,
    (Ptr{Cvoid}, Cstring, Cstring),
    param,
    "superfast",
    "zerolatency",
)

# set extra parameters manually
fps = Integer(5)
stat = ccall(
    (:x265_param_parse, libx265),
    Cint,
    (Ptr{Cvoid}, Cstring, Cstring),
    param,
    "fps",
    string(fps),
)
println("x265_param_parse::$stat")

# bRepeatHeaders = 1
stat = ccall(
    (:x265_param_parse, libx265),
    Cint,
    (Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
    param,
    "repeat-headers",
    C_NULL,
)
#stat = ccall((:x265_param_parse, libx265), Cint, (Ptr{Cvoid}, Cstring, Cstring), param, "repeat-headers", "1")
println("x265_param_parse::$stat")

# internalCsp = X265_CSP_I444
stat = ccall(
    (:x265_param_parse, libx265),
    Cint,
    (Ptr{Cvoid}, Cstring, Cstring),
    param,
    "input-csp",
    "i444",
)
println("x265_param_parse::$stat")

# internalBitDepth = 8
# "input-depth", "output-depth" are CLI-ONLY
#stat = ccall((:x265_param_parse, libx265), Cint, (Ptr{Cvoid}, Cstring, Cstring), param, "output-depth", "8")
#println("x265_param_parse::$stat")

image_width = Integer(200)
image_height = Integer(175)
res = string(image_width) * "x" * string(image_height)
stat = ccall(
    (:x265_param_parse, libx265),
    Cint,
    (Ptr{Cvoid}, Cstring, Cstring),
    param,
    "input-res",
    res,
)
println("x265_param_parse::$stat")

# rc.bitrate = bitrate
# bitrate = Integer(1000)
# stat = ccall((:x265_param_parse, libx265), Cint, (Ptr{Cvoid}, Cstring, Cstring), param, "bitrate", string(bitrate))
# println("x265_param_parse::$stat")

# set constant quality rate
crf = Integer(28)
stat = ccall(
    (:x265_param_parse, libx265),
    Cint,
    (Ptr{Cvoid}, Cstring, Cstring),
    param,
    "crf",
    string(crf),
)
println("x265_param_parse::$stat")

# x265 encoder
const encoder_open = "x265_encoder_open_" * x265_apiver()
encoder = ccall((encoder_open, libx265), Ptr{Cvoid}, (Ptr{Cvoid},), param)
println("typeof(encoder): ", typeof(encoder), "; value: $encoder")

if encoder == C_NULL
    error("NULL x265_encoder")
end

# x265 picture
picture = ccall((:x265_picture_alloc, libx265), Ptr{Cvoid}, ())
println("typeof(picture): ", typeof(picture), "; value: $picture")

if picture == C_NULL
    error("NULL x265_picture")
end

ccall((:x265_picture_init, libx265), Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), param, picture)

j_picture = x265_picture(picture)
display(j_picture)

# a sample 2D plane
width = 200
height = 175

planeB = ones(UInt8, (width, height))
println("strides:", strides(planeB))

# j_picture.planeB = Base.unsafe_convert(Base.cconvert((planeB)))
j_picture.planeB = pointer(planeB)
j_picture.strideR = 0
j_picture.strideG = 0
j_picture.strideB = strides(planeB)[2]
j_picture.bitDepth = 8

# sync the Julia structure back to C
unsafe_store!(Ptr{x265_picture}(picture), j_picture)

# check if the structure is getting written to
j_picture2 = x265_picture(picture)
display(j_picture2)

# release memory
ccall((:x265_param_free, libx265), Cvoid, (Ptr{Cvoid},), param)
param = C_NULL

ccall((:x265_encoder_close, libx265), Cvoid, (Ptr{Cvoid},), encoder)
encoder = C_NULL

ccall((:x265_picture_free, libx265), Cvoid, (Ptr{Cvoid},), picture)
picture = C_NULL
;
