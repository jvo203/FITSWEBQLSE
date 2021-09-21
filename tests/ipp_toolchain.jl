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

