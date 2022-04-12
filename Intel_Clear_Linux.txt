# 1. glib-devel
    sudo swupd bundle-add devpkg-glib

# 2. ZeroMQ
    sudo swupd bundle-add devpkg-libzmq

# 3. CZMQ
    git clone git://github.com/zeromq/czmq.git

    cd czmq

    ./autogen.sh && ./configure && make check

    sudo make install

    sudo ldconfig

# 4. libmicrohttpd
    sudo swupd bundle-add devpkg-libmicrohttpd

# 5. sqlite3-devel
    sudo swupd bundle-add devpkg-sqlite-autoconf

# 6. PostgreSQL
    sudo swupd bundle-add devpkg-postgresql

# 7. jemalloc
    wget https://github.com/jemalloc/jemalloc/releases/download/5.2.1/jemalloc-5.2.1.tar.bz2

    bunzip2 jemalloc-5.2.1.tar.bz2

    tar xvf jemalloc-5.2.1.tar

    cd jemalloc-5.2.1

    ./configure

    make

    sudo make install

# 8. lz4-devel
    sudo swupd bundle-add devpkg-lz4

# 9. libcpuid
    wget https://github.com/anrieff/libcpuid/releases/download/v0.5.1/libcpuid-0.5.1.tar.gz
    
    tar zxvf libcpuid-0.5.1.tar.gz
    
    cd libcpuid-0.5.1

    ./configure

    make

    sudo make install

# 10. libcURL

    // libcURL appears to leak memory in Intel Clear Linux ...
    // apparently it's OK, it is not a real memory leak, just dbus caching ...
    // https://github.com/clearlinux/distribution/issues/2574#issuecomment-1058618721

    sudo swupd bundle-add devpkg-curl

    or, just in case, compile / install libcURL manually !?

    wget https://curl.se/download/curl-7.81.0.tar.gz

    tar zxvf curl-7.81.0.tar.gz

    cd curl-7.81.0

    ./configure --with-openssl (or Rust --with-rustls)

    make

    sudo make install

# 11. CFITSIO
    wget http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.0.0.tar.gz

    tar zxvf cfitsio-4.0.0.tar.gz

    cd cfitsio-4.0.0

    ./configure --prefix=/usr/local --enable-reentrant

    make

    sudo make install

# 12. FPZIP

    git clone https://github.com/LLNL/fpzip.git

    cd fpzip

    mkdir -p build

    cd build

    cmake ..

    make

    sudo make install

# 13. Adjust the environment variables in the .bashrc

    Add

        export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
        export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH

    to .bashrc and re-login

# 14. Intel oneAPI

    i) intel-basekit
        
        wget https://registrationcenter-download.intel.com/akdlm/irc_nas/18487/l_BaseKit_p_2022.1.2.146_offline.sh

        sudo sh ./l_BaseKit_p_2022.1.2.146_offline.sh

    ii) intel-hpckit

        wget https://registrationcenter-download.intel.com/akdlm/irc_nas/18479/l_HPCKit_p_2022.1.2.117_offline.sh

        sudo sh ./l_HPCKit_p_2022.1.2.117_offline.sh

    Add

        source /opt/intel/oneapi/setvars.sh

    to .bashrc and re-login

# 15. libdill (C coroutines) 

    (compilation errors in both Fedora 35 and Intel Clear Linux ...)
    (a fix: make CC=clang)
        git clone https://github.com/sustrik/libdill
        cd libdill
        mkdir build
        cd build
        CC=clang cmake ..
        make

    wget http://libdill.org/libdill-2.14.tar.gz

    tar -xzf libdill-2.14.tar.gz

    cd libdill-2.14

    ./configure

    make

    sudo make install

# 16. wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db

# 17. MPI launcher (we do not use MPI internally!)

    # AMD
    mpiexec -silent-abort -machinefile amd.txt -n 4 ./fitswebqlse -c config.ini

    # Intel
    mpiexec -silent-abort -machinefile intel.txt -n 3 ./fitswebqlse -c config_ssd.ini

# 18. Replication

    # full

    rm -rf FITSWEBQLSE
    mkdir -p FITSWEBQLSE
    cd FITSWEBQLSE
    mkdir -p LOGS
    mkdir -p .cache
    cp -r /mnt/fits/chris/FITSWEBQLSE/htdocs .
    cp /mnt/fits/chris/FITSWEBQLSE/fitswebqlse .
    cp /mnt/fits/chris/FITSWEBQLSE/splatalogue_v3.db .
    cp /mnt/fits/chris/FITSWEBQLSE/amd.txt .
    cp /mnt/fits/chris/FITSWEBQLSE/intel.txt .
    cp /mnt/fits/chris/FITSWEBQLSE/all.txt .

    # abbreviated

    cd FITSWEBQLSE
    cp -r /mnt/fits/chris/FITSWEBQLSE/htdocs .
    cp /mnt/fits/chris/FITSWEBQLSE/fitswebqlse .

    # AMD
    mpiexec -silent-abort -machinefile amd.txt -n 4 ./fitswebqlse

    # Intel
    mpiexec -silent-abort -machinefile intel.txt -n 3 ./fitswebqlse

    # All
    mpiexec -silent-abort -machinefile all.txt -n 7 ./fitswebqlse
