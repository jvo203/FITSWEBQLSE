
"""
Runs `ispc` on `code` and prints the resulting assembly to stdout.
"""
function ispc_native(code, options=``)
    ispc_cmd = `ispc -o - --emit-asm $options -`
    open(ispc_cmd, "w", STDOUT) do stdin
        write(stdin, code)
    end
end

"""
Runs `ispc` on `code` and prints the resulting LLVM IR to stdout.
"""
function ispc_llvm(code, options=``, llvm_dis="/usr/local/opt/llvm/bin/llvm-dis")
    ispc_cmd = pipeline(`ispc -o - --emit-llvm $options -`, `$llvm_dis -`)
    open(ispc_cmd, "w", STDOUT) do stdin
        write(stdin, code)
    end
end

# Detect how to link a shared library:
gcc = strip(read(`which gcc`, String))
gcc == "" && error("gcc is required")

@static if Sys.isapple()
    link(objfile, libfile) = run(`$gcc -dynamiclib -o "$libfile" "$objfile"`)
end

@static if Sys.islinux()
    link(objfile, libfile) = run(`$gcc -shared -Wl,-export-dynamic "$objfile" -o "$libfile"`)
end

@static if Sys.iswindows()
    error("Not implemented: don't know how to create a shared lib on Windows")
end


_keep_files = false
_debug_generated = false

function keep_files(flag, debug=false)
    global _keep_files
    global _debug_generated
    _keep_files = flag
    _debug_generated = debug
end

"""
Runs `ispc` on `code`, creates a shared library and returns a library
handle `lib` for use with `Libdl.dlsym(lib, symbol)`.
"""
function load_ispc(code, options=``)

    function load(tmpdir)
        objfile = "$tmpdir/program.o"
        libfile = "$tmpdir/program.so"

        # Pipe the input program to ispc:
        ispc_cmd = `ispc -o "$objfile" --pic $options -`
        println(ispc_cmd)
        open(ispc_cmd, "w", stderr) do stdin
            write(stdin, code)
        end

        if _debug_generated
            sourcefile = "$tmpdir/program.c"
            asmfile = "$tmpdir/program.s"
            bcfile = "$tmpdir/program.bc"
            open(sourcefile, "w") do f
                write(f, code)
            end
            run(`ispc -o $asmfile --emit-asm $options $sourcefile`)
            run(`ispc -o $bcfile --emit-llvm $options $sourcefile`)
        end

        # Create a shared library:
        link(objfile, libfile)
        return Libc.Libdl.dlopen(libfile)
    end

    if _keep_files
        load(mktempdir())
    else
        load(mktempdir())
        #        mktempdir(load)
        # Note: the temp dir and library files will be deleted now.
        # That's OK because the library is already loaded.
    end
end
