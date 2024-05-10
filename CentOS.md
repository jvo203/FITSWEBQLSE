# 0.0 Development Tools

	sudo dnf groupinstall "Development Tools"
	sudo dnf install gcc-gfortran
    sudo dnf install git
    sudo dnf autoremove

# 0.1 GCC Toolset 13

    sudo dnf install gcc-toolset-13
    ~/.bash_profile: scl enable gcc-toolset-13 bash

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
    sudo dnf install intel-basekit intel-hpckit intel-oneapi-ispc

    .bashrc:

    source /opt/intel/oneapi/setvars.sh

# 2. Install dependencies

    sudo dnf install cmake glib2-devel libcurl-devel lz4-devel bzip2-devel zlib-devel libpq-devel sqlite-devel

# 3. NASM

    wget https://www.nasm.us/pub/nasm/releasebuilds/2.16.01/nasm-2.16.01.tar.gz
    tar zxvf nasm-2.16.01.tar.gz
    cd nasm-2.16.01
    ./configure
    make -j16
    sudo make install

# 4. x265

    visit https://www.linuxfromscratch.org/blfs/view/svn/multimedia/x265.html
    
    # this is the latest version as of 2024-02-16 but it is buggy
    wget https://anduin.linuxfromscratch.org/BLFS/x265/x265-20240216.tar.xz
    xz -d -v x265-20240216.tar.xz
    tar xvf x265-20240216.tar
    cd x265-20240216

    # reverted back to the previous version
    wget https://anduin.linuxfromscratch.org/BLFS/x265/x265-20230215.tar.xz
    xz -d -v x265-20230215.tar.xz
    tar xvf x265-20230215.tar
    cd x265-20230215

    Apply a manual change in "source/encoder/api.cpp / x265_encoder_open()":

        x265_print_params(param);
        PARAM_NS::x265_param_free(zoneParam); /* this line needs to be added */
        return encoder;

    mkdir -p build
    cd build
    cmake ../source
    make -j16
    sudo make install

# 5. libmicrohttpd

    ('--enable-experimental' is needed for websocket support, define MICROWS in the Makefile)

    wget https://ftpmirror.gnu.org/libmicrohttpd/libmicrohttpd-latest.tar.gz
    tar zxvf libmicrohttpd-latest.tar.gz
    cd libmicrohttpd-1.0.1
    ./configure --enable-experimental
    make
    sudo make install

# 6. libcpuid
    
    wget https://github.com/anrieff/libcpuid/releases/download/v0.6.4/libcpuid-0.6.4.tar.gz
    tar zxvf libcpuid-0.6.4.tar.gz
    cd libcpuid-0.6.4
    ./configure
    make
    sudo make install

# 7. CFITSIO

    wget http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.4.0.tar.gz
    tar zxvf cfitsio-4.4.0.tar.gz
    cd cfitsio-4.4.0
    ./configure --prefix=/usr/local --enable-reentrant
    make -j16
    sudo make install

# 8. WCSLIB

    wget ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib.tar.bz2
    bunzip2 wcslib.tar.bz2
    tar xvf wcslib.tar
    cd wcslib-8.2.2
    ./configure --prefix=/usr/local
    make
    sudo make install

# 9. ZeroMQ

    wget https://github.com/zeromq/libzmq/archive/refs/tags/v4.3.5.tar.gz
    tar zxvf v4.3.5.tar.gz
    cd libzmq-4.3.5
    ./autogen.sh && ./configure && make -j16
    sudo make install

# 10. CZMQ

    wget https://github.com/zeromq/czmq/archive/refs/tags/v4.2.1.tar.gz
    tar zxvf v4.2.1.tar.gz
    cd czmq-4.2.1
    ./autogen.sh && ./configure && make -j16 check
    sudo make install
    sudo ldconfig

# 11. jemalloc

    wget https://github.com/jemalloc/jemalloc/releases/download/5.3.0/jemalloc-5.3.0.tar.bz2
    bunzip2 jemalloc-5.3.0.tar.bz2
    tar xvf jemalloc-5.3.0.tar
    cd jemalloc-5.3.0
    ./configure
    make -j16
    sudo make install

# 12. tcmalloc

    sudo dnf install gperftools-devel

# 13. mimalloc

    sudo dnf install mimalloc-devel

# 14. Adjust environment variables

	Add

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
ulimit -s unlimited

    to .bashrc and re-login

# 15. Splatalogue

    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db

# 16. Open up the Ports via a Firewall

    https://mebee.info/2019/10/17/post-2369/
    
    grid6X:
    sudo firewall-cmd --add-port={8080,8081,55000}/tcp --zone=public --permanent
    sudo firewall-cmd --add-port=55000/udp --zone=public --permanent
    sudo firewall-cmd --add-port=30000/tcp --zone=public --permanent
    sudo firewall-cmd --reload

    grid0X & grid8X:
    sudo firewall-cmd --add-port={8080,8081,50000}/tcp --zone=public --permanent
    sudo firewall-cmd --add-port=50000/udp --zone=public --permanent    
    sudo firewall-cmd --reload

    jvof:
    sudo firewall-cmd --add-port={9000,9001,60000}/tcp --zone=public --permanent
    sudo firewall-cmd --add-port=60000/udp --zone=public --permanent    
    sudo firewall-cmd --reload

    jvow:
    sudo firewall-cmd --add-port={9000,9001,65000}/tcp --zone=public --permanent
    sudo firewall-cmd --add-port=65000/udp --zone=public --permanent    
    sudo firewall-cmd --reload

    sudo firewall-cmd --list-all
    sudo firewall-cmd --list-ports --zone=public

# 17. Install screen & htop

    sudo dnf install epel-release
    sudo dnf update
    sudo dnf install screen htop

# 18. Launch the processes in parallel on the development cluster

    .bashrc:
    export I_MPI_PORT_RANGE="30000:30000"
    
    # AMD
    mpiexec -silent-abort -machinefile amd.txt -n 4 ./fitswebqlse
