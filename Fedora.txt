1. Install Intel oneAPI Base + HPC kits (works well on AMD CPUs too, not just Intel's)

2. Install dependencies

	sudo dnf install cmake

	sudo dnf install libmicrohttpd-devel

	sudo dnf install libwebsockets-devel

	sudo dnf install openssl-devel

	sudo dnf install glib2-devel

	sudo dnf install cfitsio-devel

	sudo dnf install lz4-devel

	sudo dnf install zfp-devel

	sudo dnf install sqlite-devel

	sudo dnf install zeromq-devel czmq-devel

	sudo dnf install ispc # (not needed yet)

	sudo dnf install zlib-devel

	sudo dnf install libcpuid-devel	(macos: brew install libcpuid)

	Intel Clear Linux (building from source):

	wget https://github.com/anrieff/libcpuid/releases/download/v0.5.1/libcpuid-0.5.1.tar.gz

	tar zxvf libcpuid-0.5.1.tar.gz

	cd libcpuid-0.5.1

	./configure

	make

	sudo make install

3. zfp Fortran bindings

	# this step is not needed anymore
	# FITSWEBQLSE contains its own version of ZFP
	# that has been modified to avoid type clashes with cfitsio

	on macOS
	
	git clone https://github.com/LLNL/zfp.git

	cd zfp

	mkdir -p build

	cd build

	cmake -DZFP_WITH_OPENMP=OFF ..

	make

	sudo make install


	# not needed right now (ZFP is only called from C at present)

	git clone https://github.com/LLNL/zfp.git

	* cmake:
		mkdir -p build
		cd build
		cmake -DBUILD_ZFORP=ON ..
		make

	* gmake:
		gmake BUILD_ZFORP=1 FC=ifort

	The library path is different in each of the above cases.

4. FPzip

	git clone https://github.com/LLNL/fpzip.git

	cd fpzip

	mkdir -p build

	cd build

	cmake ..

	make

	sudo make install

5. JSON-FORTRAN

	# the library does not work (segmentation faults) !? (works in Linux)

	on macOS with homebrew:

	brew install json-fortran

	# Linux: manually using ifort

	source /opt/intel/oneapi/setvars.sh

	git clone https://github.com/jacobwilliams/json-fortran

	cd json-fortran/

	mkdir -p build && cd build/

	cmake -DCMAKE_Fortran_COMPILER=ifort -DUSE_GNU_INSTALL_CONVENTION=TRUE ..

	make

	make install

6. FORTRAN-UNIX

	$ git clone https://github.com/interkosmos/fortran-unix
	$ cd fortran-unix/
	$ make

7. FZMQ (ØMQ bindings)

	pre-requisites: latex, pandoc

	git clone https://github.com/richsnyder/fzmq.git

	cd fzmq

	mkdir build && cd build

	cmake ..

	make

	sudo make install

8. OpenCoarrays

	pre-requisites: gcc, gfortran

	wget https://github.com/sourceryinstitute/OpenCoarrays/releases/download/2.9.2/OpenCoarrays-2.9.2.tar.gz

	tar xvzf OpenCoarrays-2.9.2.tar.gz

	cd OpenCoarrays-2.9.2

	./install.sh --prefix-root /usr/local --num-threads 16 --yes-to-all

	Add

	source /usr/local/opencoarrays/2.9.2/setup.sh

	to $PATH in .bashrc

	Append

	/usr/local/opencoarrays/2.9.2/lib64

	to $LD_LIBRARY_PATH in .bashrc

9. Adjust environment variables

	Add

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig
export LD_LIBRARY_PATH=/usr/local/lib:/usr/local/lib64:$LD_LIBRARY_PATH

# set an unlimited stack size
ulimit -s unlimited

	to .bashrc and re-login

10. wget http://jvo.nao.ac.jp/~chris/splatalogue_v3.db

11. Examples running using one MPI host:

	mpiexec -silent-abort -n 1 ./fitswebqlse

	mpiexec --hostfile ~/mpi_machines -n 4 ./fitswebqlse

	mpiexec -silent-abort -machinefile /home/chris/mpi_machines -rr -n 4 ./fitswebqlse

