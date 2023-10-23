# 0. Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 1. gcc + gfortran
    brew install gfortran

# 2. memory model
    -mcmodel=medium --> large
    Oh no, -mcmodel=large results in "error: invalid variant 'BLEAH'" on Apple Silicon

# 3. ISPC
    brew install ispc

# 4. pkg-config
    brew install pkg-config

# 5. libcpuid
    brew install libcpuid or
    brew install --build-from-source libcpuid

# 6. glib
    brew install glib

# 7. Intel oneAPI
    The Intel oneAPI is only needed on Intel-based macOS. On Apple Silicon we take advantage of the macOS Accelerate framework (vImage) instead.

    install 'base-kit' and 'hpc-kit' (base-kit: exclude the Intel Python distribution)

    .zshrc : append

    source /opt/intel/oneapi/setvars.sh

    on Apple Silicon there are linking problems:

    ld: warning: ignoring file /opt/intel/oneapi/ipp/2021.6.0/lib/libippi.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/ipp/2021.6.0/lib/libippdc.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/ipp/2021.6.0/lib/libippcore.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/ipp/2021.6.0/lib/libipps.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/mkl/2022.1.0/lib/libmkl_lapack95_lp64.a, building for macOS-arm64 but attempting to link with file built for unknown-x86_64
ld: warning: ignoring file /opt/intel/oneapi/mkl/2022.1.0/lib/libmkl_intel_lp64.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/mkl/2022.1.0/lib/libmkl_sequential.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
ld: warning: ignoring file /opt/intel/oneapi/mkl/2022.1.0/lib/libmkl_core.dylib, building for macOS-arm64 but attempting to link with file built for macOS-x86_64
Undefined symbols for architecture arm64:
  "_ippGetLibVersion", referenced from:
      _ipp_init in main.o
  "_ippInit", referenced from:
      _ipp_init in main.o
  "_ippiResizeCubicInit_32f", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeCubic in ipp.o
  "_ippiResizeCubic_32f_C1R", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeCubic in ipp.o
  "_ippiResizeGetBorderSize_32f", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
  "_ippiResizeGetBufferSize_32f", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeSuper32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
      _resizeSuper in ipp.o
  "_ippiResizeGetBufferSize_8u", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeNearest in ipp.o
  "_ippiResizeGetSize_32f", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeSuper32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
      _resizeSuper in ipp.o
  "_ippiResizeGetSize_8u", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeNearest in ipp.o
  "_ippiResizeGetSrcRoi_32f", referenced from:
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeSuper32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
      _resizeSuper in ipp.o
  "_ippiResizeGetSrcRoi_8u", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeNearest in ipp.o
  "_ippiResizeLanczosInit_32f", referenced from:
      _resizeLanczos32f_C1R in ipp.o
      _resizeLanczos in ipp.o
  "_ippiResizeLanczos_32f_C1R", referenced from:
      _resizeLanczos32f_C1R in ipp.o
      _resizeLanczos in ipp.o
  "_ippiResizeNearestInit_8u", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeNearest in ipp.o
  "_ippiResizeNearest_8u_C1R", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeNearest in ipp.o
  "_ippiResizeSuperInit_32f", referenced from:
      _resizeSuper32f_C1R in ipp.o
      _resizeSuper in ipp.o
  "_ippiResizeSuper_32f_C1R", referenced from:
      _resizeSuper32f_C1R in ipp.o
      _resizeSuper in ipp.o
  "_ippsFree", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeSuper32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
      _resizeSuper in ipp.o
      ...
  "_ippsMalloc_8u", referenced from:
      _resizeNearest8u_C1R in ipp.o
      _resizeCubic32f_C1R in ipp.o
      _resizeLanczos32f_C1R in ipp.o
      _resizeSuper32f_C1R in ipp.o
      _resizeCubic in ipp.o
      _resizeLanczos in ipp.o
      _resizeSuper in ipp.o
      ...
ld: symbol(s) not found for architecture arm64

# 8. libmicrohttpd
    brew install libmicrohttpd

# 9. LZ4
    brew install lz4

# 10. BZIP2
    brew install bzip2

# 11. x265
    brew install x265

# 12. jemalloc
    brew install jemalloc

# 13. CZMQ
    brew install czmq

# 14. NASA CFITSIO
    brew install cfitsio

# 15. WCSLIB
    brew install wcslib

# 16. PostgreSQL (needed by JVO)
    brew install libpq

# 17. FORTRAN formatting in Visual Studio Code
    brew install fortran-language-server
    brew install findent
