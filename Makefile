.DEFAULT_GOAL := fitswebqlse

# the stack should be unlimited to avoid problems
# with ifort creating on the stack large temporary arrays
# ulimit -s unlimited

# detect the OS
UNAME_S := $(shell uname -s)

# the macOS Darwin target is handled further down the line
ifeq ($(UNAME_S),Linux)

	# detect the CPU vendor
	CPU_S := $(shell cat /proc/cpuinfo)	
#CPU_S := AuthenticAMD

	# detect OS version
	OS_S := $(shell cat /proc/version)

	# default settings on Linux
	ifeq ($(UNAME_S),Linux)

		# found an AMD CPU
		ifneq (,$(findstring AuthenticAMD,$(CPU_S)))
			# GNU gcc / gfortran
			CC := gcc
			FORT := gfortran
		endif

		# found an Intel CPU
		ifneq (,$(findstring GenuineIntel,$(CPU_S)))
			ifneq (,$(findstring Clear,$(OS_S)))
				# GNU gcc / gfortran since there are problems with ifort on Intel Clear Linux
				CC := gcc
				FORT := gfortran
			else
				# Intel oneAPI icc / ifort 
				CC := icc
				FORT := ifort

				# not so fast, ifort is buggy!!!
				# CC := gcc
				# FORT := gfortran
			endif
		endif

	endif

endif

JEMALLOC = `pkg-config --libs jemalloc` -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`
TCMALLOC = -ltcmalloc
TARGET = fitswebqlse

# Intel Integrated Performance Primitives Library
ifeq ($(UNAME_S),Linux)
	OS = linux
    IPP = -L${IPPROOT}/lib/intel64
	MKL =  ${MKLROOT}/lib/intel64/libmkl_lapack95_lp64.a -L${MKLROOT}/lib/intel64
endif

ifeq ($(UNAME_S),Darwin)
	OS = macOS
    IPP = -L${IPPROOT}/lib
	MKL =  ${MKLROOT}/lib/libmkl_lapack95_lp64.a -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib
endif

IPP += -lippi -lippdc -lipps -lippcore
MKL += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl

ZFP = zfp
ZFP_SRC := $(wildcard $(ZFP)/src/*.c)

# src/zforp.f90 src/zfp.f90
# src/psrs_sort.c
# src/iso_varying_string.f90
# src/zmq.f90
# src/wavelet.f90
# src/lz4.f90
# src/ipp.c src/psrs_sort.c src/http.c src/hash_table.c src/json.c src/json_write.c src/m_mrgrnk.f90 src/mod_sort.f90 src/wavelet.f90 src/fixed_array.f90 src/fixed_array2.f90 src/zfp_array.f90 src/histogram.c src/classifier.f90 src/fits_omp.f90 src/net.f90 src/main.f90
SRC = $(ZFP_SRC) src/webql.ispc src/cpu.c src/json.c src/ini.c src/mongoose.c src/mjson.c src/histogram.c src/hash_table.c src/json_write.c src/ipp.c src/cluster.c src/http.c src/ws.c src/face.f90 src/logging.f90 src/lttb.f90 src/fixed_array.f90 src/lz4.f90 src/classifier.f90 src/quantile.f90 src/unix_pthread.f90 src/fits.f90 src/main.c
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

ifeq ($(CC),icc)
	FLAGS = -g -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp -mcmodel=large -shared-intel
# -parallel
#-mcmodel=medium
#-ipo -parallel -fast
# -ipo causes segmentation faults ...
# -fast causes static linking problems

	CFLAGS := $(FLAGS)
	FLAGS += -heap-arrays 32 -align array64byte -fpp -D__$(OS)__
#-mt_mpi
endif

INC = `pkg-config --cflags glib-2.0` `pkg-config --cflags libcpuid` `pkg-config --cflags libmicrohttpd` `pkg-config --cflags liblz4` `pkg-config --cflags x265` `pkg-config --cflags jemalloc` `pkg-config --cflags libczmq` -I./$(ZFP)/include -I./$(ZFP)/src -I${MKLROOT}/include/intel64/lp64 -I${MKLROOT}/include
MOD =
# -I/home/chris/zfp/include
DEF = -DDEBUG -DNO_MONGOOSE_HTTP_CLIENT

LIBS = -L/usr/local/lib `pkg-config --libs glib-2.0` `pkg-config --libs libcpuid` `pkg-config --libs libmicrohttpd` `pkg-config --libs liblz4` `pkg-config --libs cfitsio` -lsqlite3 -lcurl -lz -pthread `pkg-config --libs libzmq` `pkg-config --libs libczmq` `pkg-config --libs x265`
# -lzfp before cfitsio
#`pkg-config --libs json-fortran`

# -lmpifort not needed when using mpiifort
# -L/home/chris/zfp/build/lib64

ifeq ($(CC),icc)
	# Intel FORTRAN runtime
	LIBS += -lifcore -limf
endif

ifeq ($(UNAME_S),Darwin)
	INC += -I/usr/local/include -I/usr/local/opt/openssl/include -I/usr/local/opt/curl/include
	LIBS += -L/usr/local/opt/openssl/lib -L/usr/local/opt/curl/lib -lcurl
#MOD += `pkg-config --cflags json-fortran`

	CC = gcc-11
	FORT = gfortran-11
	FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	# -mcmodel=large results in "error: invalid variant 'BLEAH'"
	CFLAGS := $(FLAGS)

	# GCC FORTRAN runtime
	LIBS += -L/usr/local/Cellar/gcc/11.3.0/lib/gcc/11 -lgfortran -lm

	# try Intel compilers for a change! ... linking problems ...
	# CC = icc
	# FORT = ifort
	# FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp -shared-intel
	# CFLAGS := $(FLAGS) -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include
	# icc main.c -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk
	# FLAGS += -heap-arrays 32 -align array64byte

	ifeq ($(FORT),nagfor)
		MPI_LINK_FLAGS = $(shell mpifort --showme:link)
		FLAGS := -target=core2 -O4 -f2018 -kind=byte -openmp -colour $(MPI_LINK_FLAGS)
	else
		FLAGS += -cpp -D__$(OS)__ -fallow-invalid-boz -fmax-stack-var-size=32768
	endif
endif

# detect the GNU Compiler under Linux
ifeq ($(CC),gcc)
	override CFLAGS += -march=native -mcmodel=large -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp -Wall -Wextra
	FLAGS := $(CFLAGS) -std=f2018 -fall-intrinsics

	ifeq ($(FORT),nagfor)
		MPI_LINK_FLAGS = $(shell mpifort --showme:link)
		FLAGS := -target=core2 -O4 -f2018 -kind=byte -openmp -colour $(MPI_LINK_FLAGS)
	else
		FLAGS += -cpp -D__$(OS)__ -fallow-invalid-boz -fmax-stack-var-size=32768
	endif

	# GCC FORTRAN runtime
	LIBS += -lgfortran -lm
endif

# include dependencies (all .d files)
-include $(DEP)

%.o: %.ispc
	ispc -g -O3 --pic --opt=fast-math --addressing=32 -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(DEF) $(INC) -MMD -o $@ -c $<

%.o: %.f90
	$(FORT) $(FLAGS) $(MOD) -o $@ -c $<

fitswebqlse: $(OBJ)
	$(CC) $(CFLAGS) -o $(TARGET) $^ $(LIBS) $(IPP) $(MKL) $(JEMALLOC)

test:
	$(FORT) $(FLAGS) src/wavelet.f90 src/fixed_array.f90 src/zfp_array.f90 src/lz4.f90 src/testWavelets.f90 -o testWavelets -llz4 $(LIBS)
# on macos -L/usr/local/opt/lz4/lib -llz4

json:
	ifort -Ofast -xHost -mavx -axAVX -qopt-report=2 tests/json_serialize.f90 -o serialize `pkg-config --libs json-fortran`

mpi:
	$(FORT) -Ofast -xHost test.f90 -o test

zmq:
#$(FORT) -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp src/zmq.f90 tests/test_zmq.f90 -o test_zmq -lzmq -lczmq
	gfortran -Ofast -fopenmp src/zmq.f90 tests/test_zmq.f90 -o test_zmq -L/usr/local/lib -lzmq -lczmq

compress::
#$(CC) $(CFLAGS) tests/zfp_compress.c -o zfp_compress -lzfp
	$(FORT) $(FLAGS) src/zforp.f90 tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzfp $(LIBS)
#-L/home/chris/zfp/build/lib64 -lzfp

block:
	$(FORT) $(FLAGS) src/wavelet.f90 tests/zfp_block.f90 -o zfp_block $(LIBS)

block128:
	$(FORT) $(FLAGS) src/wavelet.f90 tests/zfp_block_128.f90 -o zfp_block_128 $(LIBS)

fixed:
	ispc -g -O3 --pic --opt=fast-math --addressing=32 tests/fixed.ispc -o tests/fixed.o
	$(FORT) $(FLAGS) tests/test_fixed_array.f90 tests/fixed.o -o test_fixed_array $(LIBS)

encode:
	ispc -g -O3 --pic --opt=fast-math --addressing=32 src/webql.ispc -o src/zfp.o -h tests/webql.h
	$(CC) $(CFLAGS) tests/zfp_encode.c src/zfp.o -o zfp_encode -lm
#icc -O0 tests/zfp_encode.c -o zfp_encode -lm

gzfp:
	gfortran -Ofast -I/home/chris/zfp/build/modules tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzFORp

mutex:
	gfortran -cpp -D__macOS__ src/unix_pthread.f90 mutex.f90 -pthread

#-corray-config-file=./config

clean:
	rm -f $(ZFP)/src/*.o $(ZFP)/src/*.d src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets
