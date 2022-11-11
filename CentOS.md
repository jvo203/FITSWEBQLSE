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

# 2. libmicrohttpd

    wget https://ftpmirror.gnu.org/libmicrohttpd/libmicrohttpd-latest.tar.gz

    tar zxvf libmicrohttpd-latest.tar.gz

    cd libmicrohttpd-0.9.75

    ./configure

    make

    sudo make install

# 3. libcpuid
    
    wget https://github.com/anrieff/libcpuid/releases/download/v0.6.1/libcpuid-0.6.1.tar.gz

    tar zxvf libcpuid-0.6.1.tar.gz
    
    cd libcpuid-0.6.1

    ./configure

    make

    sudo make install

# 4. Intel SPMD C (ispc)

    manually add ispc from https://ispc.github.io/downloads.html

# 5. Install dependencies

    sudo dnf install cmake

# 9. Adjust environment variables

	Add

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH
ulimit -s unlimited

    to .bashrc and re-login

# 10. Splatalogue

    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db