# 0. Development Tools

	sudo dnf groupinstall "Development Tools"

	sudo dnf install gcc-gfortran

# 1. Install Intel oneAPI Base + HPC kits (works well on AMD CPUs too, not just Intel's)

/etc/yum.repos.d/oneAPI.repo

[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB

    sudo dnf update

    sudo dnf install intel-basekit

    sudo dnf install intel-hpckit

    .bashrc:

    source /opt/intel/oneapi/setvars.sh

# 2. Install dependencies

    sudo dnf install cmake

    sudo dnf install glib2-devel

    sudo dnf install libcurl-devel

    sudo dnf install lz4-devel

    sudo dnf install cfitsio-devel

    sudo dnf install libpq-devel

    sudo dnf install sqlite-devel

    sudo dnf install x265-devel

    sudo dnf install nasm

# 3. NASM

    wget https://www.nasm.us/pub/nasm/releasebuilds/2.15.05/nasm-2.15.05.tar.gz

    tar zxvf nasm-2.15.05.tar.gz

    cd nasm-2.15.05

    ./configure

    make

    sudo make install

# 4. x265

    visit https://www.linuxfromscratch.org/blfs/view/svn/multimedia/x265.html

    wget https://anduin.linuxfromscratch.org/BLFS/x265/x265-20220819.tar.xz

    xz -d -v x265-20220819.tar.xz

    tar xvf x265-20220819.tar

    cd x265-20220819

    Apply a manual change in "source/encoder/api.cpp / x265_encoder_open()":

        x265_print_params(param);
        PARAM_NS::x265_param_free(zoneParam); /* this line needs to be added */
        return encoder;

    mkdir -p build

    cd build

    cmake ../source

    make -j8

    sudo make install

# 5. libmicrohttpd

    wget https://ftpmirror.gnu.org/libmicrohttpd/libmicrohttpd-latest.tar.gz

    tar zxvf libmicrohttpd-latest.tar.gz

    cd libmicrohttpd-0.9.75

    ./configure

    make

    sudo make install

# 6. libcpuid
    
    wget https://github.com/anrieff/libcpuid/releases/download/v0.6.1/libcpuid-0.6.1.tar.gz

    tar zxvf libcpuid-0.6.1.tar.gz
    
    cd libcpuid-0.6.1

    ./configure

    make

    sudo make install

# 7. CFITSIO

    wget http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.1.0.tar.gz

    tar zxvf cfitsio-4.1.0.tar.gz

    cd cfitsio-4.1.0

    ./configure --prefix=/usr/local --enable-reentrant

    make

    sudo make install

# 8. ZeroMQ

    wget https://github.com/zeromq/libzmq/archive/refs/tags/v4.3.4.tar.gz

    tar zxvf v4.3.4.tar.gz

    cd libzmq-4.3.4

    ./autogen.sh && ./configure && make

    sudo make install

# 9. CZMQ

    wget https://github.com/zeromq/czmq/archive/refs/tags/v4.2.1.tar.gz

    tar zxvf v4.2.1.tar.gz

    cd czmq-4.2.1

    ./autogen.sh && ./configure && make check

    sudo make install

    sudo ldconfig

# 10. jemalloc

    wget https://github.com/jemalloc/jemalloc/releases/download/5.3.0/jemalloc-5.3.0.tar.bz2

    bunzip2 jemalloc-5.3.0.tar.bz2

    tar xvf jemalloc-5.3.0.tar

    cd jemalloc-5.3.0

    ./configure

    make

    sudo make install

# 11. Intel SPMD C (ispc)

    manually add ispc from https://ispc.github.io/downloads.html

# 12. Adjust environment variables

	Add

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
ulimit -s unlimited

    to .bashrc and re-login

# 13. Splatalogue

    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db

# 14. Open the Firewall
    
    sudo firewall-cmd --add-port={8080,8081}/tcp --zone=public --permanent

    sudo firewall-cmd --reload

    sudo firewall-cmd --list-all

    sudo firewall-cmd --list-ports --zone=public