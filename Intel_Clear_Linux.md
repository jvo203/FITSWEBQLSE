# 1. glib-devel
    sudo swupd bundle-add devpkg-glib

# 2. ZeroMQ
    sudo swupd bundle-add devpkg-libzmq

# 3. CZMQ    
    git clone https://github.com/zeromq/czmq.git

    cd czmq

    ./autogen.sh && ./configure && make check

    sudo make install

    sudo ldconfig

# 4. libmicrohttpd
    sudo swupd bundle-add devpkg-libmicrohttpd

# 5. sqlite3-devel
    sudo swupd bundle-add devpkg-sqlite-autoconf

# 6. PostgreSQL
    sudo swupd bundle-add devpkg-postgresql --> forget about this line, a version greater than 10 is needed for SCRAM authentication

    sudo swupd bundle-add devpkg-openssl

    wget https://ftp.postgresql.org/pub/source/v14.4/postgresql-14.4.tar.bz2

    tar xjf postgresql-14.4.tar.bz2

    cd postgresql-14.4/

    ./configure --with-openssl --without-readline

    make

    sudo make install

    adjust LD_LIBRARY_PATH and PKG_CONFIG_PATH in .bashrc (see point 17.)

# 7. jemalloc    
    wget https://github.com/jemalloc/jemalloc/releases/download/5.3.0/jemalloc-5.3.0.tar.bz2

    bunzip2 jemalloc-5.3.0.tar.bz2

    tar xvf jemalloc-5.3.0.tar

    cd jemalloc-5.3.0

    ./configure

    make

    sudo make install

# 8. lz4-devel
    sudo swupd bundle-add devpkg-lz4

# 9. libcpuid
    wget https://github.com/anrieff/libcpuid/releases/download/v0.7.0/libcpuid-0.7.0.tar.gz
    
    tar zxvf libcpuid-0.7.0.tar.gz
    
    cd libcpuid-0.7.0

    ./configure

    make

    sudo make install

# 10. libcURL

    // libcURL appears to leak memory in Intel Clear Linux ...
    // apparently it's OK, it is not a real memory leak, just dbus caching ...
    // https://github.com/clearlinux/distribution/issues/2574#issuecomment-1058618721

    sudo swupd bundle-add devpkg-curl

    or, just in case, compile / install libcURL manually !?

    wget https://curl.se/download/curl-7.84.0.tar.gz

    tar zxvf curl-7.84.0.tar.gz

    cd curl-7.84.0

    ./configure --with-openssl (or Rust --with-rustls)

    make

    sudo make install

# 11. CFITSIO    
    wget http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.5.0.tar.gz

    tar zxvf cfitsio-4.5.0.tar.gz

    cd cfitsio-4.5.0

    ./configure --prefix=/usr/local --enable-reentrant

    make

    sudo make install

# 12. WCSLIB

    wget ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib.tar.bz2
    bunzip2 wcslib.tar.bz2
    tar xvf wcslib.tar
    cd wcslib-8.4
    ./configure --prefix=/usr/local
    make
    sudo make install

# 13. Starlink AST

    wget https://github.com/Starlink/ast/releases/download/v9.2.12/ast-9.2.12.tar.gz
    tar zxvf ast-9.2.12.tar.gz
    cd ast-9.2.12
    ./configure --prefix=/usr/local
    make -j16
    sudo make install

# 14. FPZIP

    git clone https://github.com/LLNL/fpzip.git

    cd fpzip

    mkdir -p build

    cd build

    cmake ..

    make

    sudo make install

# 15. NASM

    sudo swupd bundle-add nasm

# 16. x265

    visit https://www.linuxfromscratch.org/blfs/view/svn/multimedia/x265.html

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

    make -j8

    sudo make install

# 17. Adjust the environment variables in the .bashrc

    Add

        export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/local/pgsql/lib/pkgconfig:$PKG_CONFIG_PATH
        
        export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:/usr/local/pgsql/lib/:$LD_LIBRARY_PATH

    to .bashrc and re-login

# 18. Intel oneAPI

    i) intel-basekit
        
        wget https://registrationcenter-download.intel.com/akdlm/irc_nas/18487/l_BaseKit_p_2022.1.2.146_offline.sh

        sudo sh ./l_BaseKit_p_2022.1.2.146_offline.sh

    ii) intel-hpckit

        wget https://registrationcenter-download.intel.com/akdlm/irc_nas/18479/l_HPCKit_p_2022.1.2.117_offline.sh

        sudo sh ./l_HPCKit_p_2022.1.2.117_offline.sh

    Add

        source /opt/intel/oneapi/setvars.sh

    to .bashrc and re-login

# 19. libdill (C coroutines) (not needed / not used at the moment)

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

# 20. Splatalogue & Atomic Spectra Database (ADS)

    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db
    wget http://jvo.nao.ac.jp/~chris/asd.db

# 21. Intel SPMD C (ispc)

    manually add ispc from https://ispc.github.io/downloads.html

# 22. MPI launcher (we do not use MPI internally!)

    # AMD
    mpiexec -silent-abort -machinefile amd.txt -n 4 ./fitswebqlse -c config.ini

    # Intel
    mpiexec -silent-abort -machinefile intel.txt -n 3 ./fitswebqlse -c config_ssd.ini

# 23. Replication

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
    cd ~/FITSWEBQLSE
    mpiexec -silent-abort -machinefile all.txt -n 7 ./fitswebqlse
