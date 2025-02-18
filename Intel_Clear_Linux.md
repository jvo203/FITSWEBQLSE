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
    sudo swupd bundle-add devpkg-libmicrohttpd (does not include WebSockets)

    wget https://ftpmirror.gnu.org/libmicrohttpd/libmicrohttpd-latest.tar.gz
    tar zxvf libmicrohttpd-latest.tar.gz
    cd libmicrohttpd-1.0.1
    export CFLAGS="$CFLAGS -Wno-error=implicit-function-declaration"
    ./configure --enable-experimental
    make
    sudo make install

    there is an error:
    connection_add.c: In function 'MHD_accept_connection_':
connection_add.c:1031:7: error: implicit declaration of function 'accept4'; did you mean 'accept'? [-Wimplicit-function-declaration]
 1031 |   s = accept4 (fd,

    the fix is to force '-Wno-error=implicit-function-declaration'

# 5. bzip2

    sudo swupd bundle-add devpkg-bzip2

# 6. sqlite3-devel
    sudo swupd bundle-add devpkg-sqlite-autoconf

# 7. PostgreSQL
    sudo swupd bundle-add devpkg-postgresql --> forget about this line, a version greater than 10 is needed for SCRAM authentication

    sudo swupd bundle-add devpkg-openssl

    wget https://ftp.postgresql.org/pub/source/v14.4/postgresql-14.4.tar.bz2

    tar xjf postgresql-14.4.tar.bz2

    cd postgresql-14.4/

    ./configure --with-openssl --without-readline

    make

    sudo make install

    adjust LD_LIBRARY_PATH and PKG_CONFIG_PATH in .bashrc (see point 19.)

# 8. jemalloc    
    wget https://github.com/jemalloc/jemalloc/releases/download/5.3.0/jemalloc-5.3.0.tar.bz2

    bunzip2 jemalloc-5.3.0.tar.bz2

    tar xvf jemalloc-5.3.0.tar

    cd jemalloc-5.3.0

    ./configure

    make

    sudo make install

# 9. mimalloc

    git clone https://github.com/microsoft/mimalloc.git
    cd mimalloc
    mkdir -p out/release
    cd out/release
    cmake ../..
    make -j8
    sudo make install


# 10. lz4-devel
    sudo swupd bundle-add devpkg-lz4

# 11. libcpuid
    wget https://github.com/anrieff/libcpuid/releases/download/v0.7.0/libcpuid-0.7.0.tar.gz
    
    tar zxvf libcpuid-0.7.0.tar.gz
    
    cd libcpuid-0.7.0

    ./configure

    make

    sudo make install

# 12. libcURL

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

# 13. CFITSIO    
    wget http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/cfitsio-4.5.0.tar.gz

    tar zxvf cfitsio-4.5.0.tar.gz

    cd cfitsio-4.5.0

    ./configure --prefix=/usr/local --enable-reentrant

    make

    sudo make install

# 14. WCSLIB

    wget ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib.tar.bz2
    bunzip2 wcslib.tar.bz2
    tar xvf wcslib.tar
    cd wcslib-8.4
    ./configure --prefix=/usr/local
    make
    sudo make install

# 15. Starlink AST

    wget https://github.com/Starlink/ast/releases/download/v9.2.12/ast-9.2.12.tar.gz
    tar zxvf ast-9.2.12.tar.gz
    cd ast-9.2.12
    ./configure --prefix=/usr/local
    make -j16
    sudo make install

# 16. FPZIP (not needed / not used at the moment)

    git clone https://github.com/LLNL/fpzip.git

    cd fpzip

    mkdir -p build

    cd build

    cmake ..

    make

    sudo make install

# 17. NASM

    sudo swupd bundle-add nasm

# 18. x265

    visit https://www.linuxfromscratch.org/blfs/view/svn/multimedia/x265.html

    wget https://bitbucket.org/multicoreware/x265_git/downloads/x265_4.1.tar.gz

    tar zxvf x265_4.1.tar.gz
    
    cd x265_4.1    

    mkdir -p build

    cd build

    cmake ../source

    make -j8

    sudo make install

# 19. Adjust the environment variables in the .bashrc

    Add

        export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/local/pgsql/lib/pkgconfig:$PKG_CONFIG_PATH
        
        export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:/usr/local/pgsql/lib/:$LD_LIBRARY_PATH

    to .bashrc and re-login

# 20. Intel oneAPI

    i) intel-basekit

        wget https://registrationcenter-download.intel.com/akdlm/IRC_NAS/96aa5993-5b22-4a9b-91ab-da679f422594/intel-oneapi-base-toolkit-2025.0.0.885_offline.sh

        sudo sh ./intel-oneapi-base-toolkit-2025.0.0.885_offline.sh -a --silent --cli --eula accept        

    ii) intel-hpckit

        wget https://registrationcenter-download.intel.com/akdlm/IRC_NAS/0884ef13-20f3-41d3-baa2-362fc31de8eb/intel-oneapi-hpc-toolkit-2025.0.0.825_offline.sh

        sudo sh ./intel-oneapi-hpc-toolkit-2025.0.0.825_offline.sh -a --silent --cli --eula accept        

    Add

        source /opt/intel/oneapi/setvars.sh

    to .bashrc and re-login

# 21. libdill (C coroutines) (not needed / not used at the moment)

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

# 22. Splatalogue & Atomic Spectra Database (ADS)

    wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db
    wget http://jvo.nao.ac.jp/~chris/asd.db

# 23. Intel SPMD C (ispc)

    manually add ispc from https://ispc.github.io/downloads.html

    i.e. wget https://github.com/ispc/ispc/releases/download/v1.25.3/ispc-v1.25.3-linux-oneapi.tar.gz
    or wget https://github.com/ispc/ispc/releases/download/v1.25.3/ispc-v1.25.3-linux.tar.gz

    extract and copy the binary to /usr/local/bin

# 24. MPI launcher (we do not use MPI internally!)

    # AMD
    mpiexec -silent-abort -machinefile amd.txt -n 4 ./fitswebqlse -c config.ini

    # Intel
    mpiexec -silent-abort -machinefile intel.txt -n 3 ./fitswebqlse -c config_ssd.ini

# 25. Replication

    # full

    rm -rf FITSWEBQLSE
    mkdir -p FITSWEBQLSE
    cd FITSWEBQLSE
    mkdir -p LOGS
    mkdir -p .cache
    cp -r /mnt/fits/chris/FITSWEBQLSE/htdocs .
    cp /mnt/fits/chris/FITSWEBQLSE/fitswebqlse .
    cp /mnt/fits/chris/FITSWEBQLSE/asd.db .
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

# 26. valgrind (debugging)

    sudo swupd bundle-add valgrind