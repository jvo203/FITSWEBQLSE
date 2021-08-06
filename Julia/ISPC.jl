include("ispc_toolchain.jl")

# read the code from a file
code = open(f -> read(f, String), "webql.ispc")

# compile the SPMD code
lib = load_ispc(code, `--opt=fast-math --addressing=32`)

# get function pointers
const radial_spec_fptr = Libc.Libdl.dlsym(lib, "calculate_radial_spectrumF16")
const square_spec_fptr = Libc.Libdl.dlsym(lib, "calculate_square_spectrumF16")

const radial_view_fptr = Libc.Libdl.dlsym(lib, "calculate_radial_viewport_spectrumF16")
const square_view_fptr = Libc.Libdl.dlsym(lib, "calculate_square_viewport_spectrumF16")
