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

# Intel oneAPI
    base-kit: exclude the Intel Python distribution

    .zshrc : append

    source /opt/intel/oneapi/setvars.sh

# libmicrohttpd
    brew install libmicrohttpd