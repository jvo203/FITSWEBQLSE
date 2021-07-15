# A basic ISPC kernel:
code = """
export void simple(uniform float vin[], uniform float vout[],
                   uniform int count) {
    foreach (index = 0 ... count) {
        float v = vin[index];
        if (v < 0.5)
            v = v * v;
        else
            v = sqrt(v);
        vout[index] = v;
    }
}
"""

println(code)

# Compile the code and get a function pointer to our kernel:
lib = load_ispc(code, `--target=avx1-i32x8`)
fptr = Libdl.dlsym(lib, "simple")

# Call the kernel:
vin = rand(Float32, 1000);
vout = zeros(Float32, 1000);
ccall(fptr, Void, (Ref{Float32}, Ref{Float32}, UInt64), vin, vout, length(vout))