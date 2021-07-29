include("ispc_toolchain.jl")

code = open(f -> read(f, String), "webql.ispc")

println(code)

# Compile the code and get a function pointer to our kernel:
lib = load_ispc(code, `--pic --opt=fast-math --addressing=32`)
# fptr = Libc.Libdl.dlsym(lib, "simple")
