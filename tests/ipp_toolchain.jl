# Detect how to link a shared library:
@static if Sys.isapple()
    libtool = "/usr/bin/libtool"
    println("Linker: $libtool")
    link(objfile, libfile) = run(`$libtool -dynamic -o "$libfile" "$objfile"`)
end

@static if Sys.islinux()
    gcc = strip(read(`which gcc`, String))
    gcc == "" && error("libtool or gcc is required")
    println("Linker: $gcc")
    link(objfile, libfile) =
        run(`$gcc -shared -Wl,-export-dynamic "$objfile" -o "$libfile"`)
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
        objfile = "$tmpdir/program.o"
        libfile = "$tmpdir/program.so"

        # Pipe the input program to gcc:
        gcc_cmd = `gcc -o "$objfile" --fPIC $options -`
        println(gcc_cmd)
        open(gcc_cmd, "w", stderr) do stdin
            write(stdin, code)
        end

        # Create a shared library:
        link(objfile, libfile)
        return Libc.Libdl.dlopen(libfile)
    end

    load(mktempdir())
end