# Detect how to link a shared library:
@static if Sys.isapple()
    libtool = "/usr/bin/libtool"
    println("Linker: $libtool")
    ipplib = ENV["IPPROOT"] * "/lib"

    link(objfile, libfile, linkfiles) =
        run(`$libtool "-L$ipplib $linkfiles" -dynamic -o "$libfile" "$objfile"`)
end

@static if Sys.islinux()
    gcc = strip(read(`which gcc`, String))
    gcc == "" && error("libtool or gcc is required")
    println("Linker: $gcc")
    ipplib = ENV["IPPROOT"] * "/lib/intel64"

    link(objfile, libfile, linkfiles) = run(
        `$gcc "-L$ipplib $linkfiles" -shared -Wl,-export-dynamic "$objfile" -o "$libfile"`,
    )
end

@static if Sys.iswindows()
    error("Not implemented: don't know how to create a shared lib on Windows")
end

"""
Runs `gcc` on `code`, links with `ipp`, creates a shared library and returns a library
handle `lib` for use with `Libdl.dlsym(lib, symbol)`.
"""
function load_ipp(code, options = ``)

    function load(tmpdir)
        objfile = "$tmpdir/ipp.o"
        libfile = "$tmpdir/ipp.so"
        linkfiles = "-lippi -lippdc -lipps -lippcore"

        # Pipe the input program to gcc:
        gcc_cmd = `gcc -c $code -o "$objfile" -fPIC $options`
        println(gcc_cmd)
        run(gcc_cmd)

        # Create a shared library:
        link(objfile, libfile, linkfiles)
        return Libc.Libdl.dlopen(libfile)
    end

    load(mktempdir())
end

# macOS tests

# compiler
# gcc -c ../src/ipp.c -o test.o -fPIC -O3

# linker
# /usr/bin/libtool -dynamic -o test.so test.o -L/opt/intel/oneapi/ipp/2021.1.1/lib -lippi -lippdc -lipps -lippcore