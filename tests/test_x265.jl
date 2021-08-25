using x265_jll;

param = Nothing

param = ccall((:x265_param_alloc, libx265), Ptr{Cvoid}, ())
println("typeof(param): ", typeof(param), "; value: $param")

if param == C_NULL
    error("NULL x265_param")
end

# default parameters
ccall((:x265_param_default_preset, libx265), Cvoid, (Ptr{Cvoid}, Cstring, Cstring), param, "superfast", "zerolatency")

# set extra parameters manually
fps = Integer(5)
stat = ccall((:x265_param_parse, libx265), Cint, (Ptr{Cvoid}, Cstring, Cstring), param, "fps", string(fps))
println("x265_param_parse::$stat")

# bRepeatHeaders = 1
stat = ccall((:x265_param_parse, libx265), Cint, (Ptr{Cvoid}, Cstring, Ptr{Cvoid}), param, "repeat-headers", C_NULL)
println("x265_param_parse::$stat")

# release memory
ccall((:x265_param_free, libx265), Cvoid, (Ptr{Cvoid},), param)
param = Nothing
