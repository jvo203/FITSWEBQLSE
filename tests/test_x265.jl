using x265_jll;

param = Nothing

param = ccall((:x265_param_alloc, libx265), Ptr{Cvoid}, ())
println("typeof(param): ", typeof(param), "; value: $param")

if param == C_NULL
    error("NULL x265_param")
end

ccall((:x265_param_default_preset, libx265), Cvoid, (Ptr{Cvoid}, Cstring, Cstring), param, "superfast", "zerolatency")

ccall((:x265_param_free, libx265), Cvoid, (Ptr{Cvoid},), param)
param = Nothing
