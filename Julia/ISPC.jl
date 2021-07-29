include("ispc_toolchain.jl")

code = open(f -> read(f, String), "webql.ispc")

# Compile the code and get function pointers
lib = load_ispc(code, `--opt=fast-math --addressing=32`)
# fptr = Libc.Libdl.dlsym(lib, "simple")
