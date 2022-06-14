# 1. development tools 
    sudo apt-get install build-essential

# 2. cmake
    sudo apt-get install cmake

# 3. gfortran
    sudo apt-get install gfortran

# 4. assembler
    sudo apt install nasm

# 5. czmq
    sudo apt install libczmq-dev

# 6. libmicrohttpd
    sudo apt install libmicrohttpd-dev

# 7. sqlite
    sudo apt install libsqlite3-dev

# 8. PostgreSQL
    sudo apt install libpq-dev

# 9. jemalloc
    sudo apt install libjemalloc-dev

# 10. lz4
    sudo apt install liblz4-dev

# 11. cpuid
    sudo apt install libcpuid-dev

# 12. libcURL
    sudo apt install libcurl4-openssl-dev

# 13. CFITSIO
    sudo apt install libcfitsio-dev

# 14. x265
    sudo apt install libx265-dev

    or compile from source:

    sudo apt install nasm

    visit https://www.linuxfromscratch.org/blfs/view/svn/multimedia/x265.html

    wget https://anduin.linuxfromscratch.org/BLFS/x265/x265-20220219.tar.xz

    xz -d -v x265-20220219.tar.xz

    tar xvf x265-20220219.tar

    cd x265-20220219

    Apply a manual change in "source/encoder/api.cpp / x265_encoder_open()":

        x265_print_params(param);
        PARAM_NS::x265_param_free(zoneParam); /* this line needs to be added */
        return encoder;

    mkdir -p build

    cd build

    cmake ../source

    make -j8

    sudo make install

# 15. Adjust the environment variables in the .bashrc

    Add

        export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
        export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH

    to .bashrc and re-login

# 16. Intel SPMD C (ispc)

    manually add ispc from https://ispc.github.io/downloads.html

# 17. Intel oneAPI

    cd /tmp
    wget https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
    sudo apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
    rm GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB

    echo "deb https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list
    sudo apt update

    sudo apt install intel-basekit
    sudo apt install intel-hpckit

    Add

        source /opt/intel/oneapi/setvars.sh

    to .bashrc and re-login

# 18. Splatalogue 
    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db

# 19. TCMalloc
    sudo apt install google-perftools


