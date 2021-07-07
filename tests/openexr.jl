module C
using OpenEXR_jll
include("OpenEXR_common.jl")
include("OpenEXR_api.jl")
end # module C

const MAGIC = Cint(C.IMF_MAGIC)

@enum Compression::Cint begin
    NO_COMPRESSION = C.IMF_NO_COMPRESSION
    RLE_COMPRESSION = C.IMF_RLE_COMPRESSION
    ZIPS_COMPRESSION = C.IMF_ZIPS_COMPRESSION
    ZIP_COMPRESSION = C.IMF_ZIP_COMPRESSION
    PIZ_COMPRESSION = C.IMF_PIZ_COMPRESSION
    PXR24_COMPRESSION = C.IMF_PXR24_COMPRESSION
    B44_COMPRESSION = C.IMF_B44_COMPRESSION
    B44A_COMPRESSION = C.IMF_B44A_COMPRESSION
    DWAA_COMPRESSION = C.IMF_DWAA_COMPRESSION
    DWAB_COMPRESSION = C.IMF_DWAB_COMPRESSION
end

@enum RgbaChannels::Cint begin
    WRITE_R = C.IMF_WRITE_R
    WRITE_G = C.IMF_WRITE_G
    WRITE_B = C.IMF_WRITE_B
    WRITE_A = C.IMF_WRITE_A
    WRITE_Y = C.IMF_WRITE_Y
    WRITE_C = C.IMF_WRITE_C
    WRITE_RGB = C.IMF_WRITE_RGB
    WRITE_RGBA = C.IMF_WRITE_RGBA
    WRITE_YC = C.IMF_WRITE_YC
    WRITE_YA = C.IMF_WRITE_YA
    WRITE_YCA = C.IMF_WRITE_YCA
end

function check(ret)
    ret == typeof(ret)(0) && error(unsafe_string(C.ImfErrorMessage()))
end


width = 100
height = 67

pixels = 100.0 * randn(Float16, width, height)

println("OpenEXR MAGIC: $MAGIC")