.DEFAULT_GOAL := fitswebqlse

# get the Homebrew installation directory
# brew --prefix is not available from inside the Homebrew environment
HOMEBREW_PREFIX := $(shell brew --prefix)

# assume Intel macOS	
ifeq ($(HOMEBREW_PREFIX),)
	HOMEBREW_PREFIX := $(shell /usr/local/bin/brew --prefix)
endif

# if it still cannot be found assume Apple Silicon macOS
ifeq ($(HOMEBREW_PREFIX),)
	HOMEBREW_PREFIX := $(shell /opt/homebrew/bin/brew --prefix)
endif

# the stack should be unlimited to avoid problems
# with ifort creating on the stack large temporary arrays
# ulimit -s unlimited

# detect the OS
UNAME_S := $(shell uname -s)
# UNAME_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')

# detect the CPU architecture (ARM64 or x86-64)
UNAME_M := $(shell uname -m)
# UNAME_M := $(shell sh -c 'uname -m 2>/dev/null || echo not')

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
				# CC := icc
				# FORT := ifort

				# not so fast, ifort is buggy!!!
				CC := gcc
				FORT := gfortran
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
	# -L${IPPROOT}/lib/intel64/tl/openmp
	MKL =  ${MKLROOT}/lib/intel64/libmkl_lapack95_lp64.a -L${MKLROOT}/lib/intel64
endif

ifeq ($(UNAME_S),Darwin)
	OS = macOS
    IPP = -L${IPPROOT}/lib
	# -L${IPPROOT}/lib/tl/openmp
	MKL =  ${MKLROOT}/lib/libmkl_lapack95_lp64.a -L${MKLROOT}/lib -Wl,-rpath,${MKLROOT}/lib
endif

IPP += -lippi -lippdc -lipps -lippcore
# -lippcore_tl_omp -lippi_tl_omp
MKL += -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl
# -liomp5

ZFP = zfp
ZFP_SRC := $(wildcard $(ZFP)/src/*.c)

# src/zforp.f90 src/zfp.f90
# src/psrs_sort.c
# src/iso_varying_string.f90
# src/zmq.f90
# src/wavelet.f90
# src/lz4.f90
# src/ipp.c src/psrs_sort.c src/http.c src/hash_table.c src/json.c src/json_write.c src/m_mrgrnk.f90 src/mod_sort.f90 src/wavelet.f90 src/fixed_array.f90 src/fixed_array2.f90 src/zfp_array.f90 src/histogram.c src/classifier.f90 src/fits_omp.f90 src/net.f90 src/main.f90
SRC = $(ZFP_SRC) src/webql.ispc src/my_threads.c src/cpu.c src/json.c src/ini.c src/mongoose.c src/mjson.c src/histogram.c src/hash_table.c src/json_write.c src/cluster.c src/http.c src/ws.c src/ring_buffer.c src/linked_list.f90 src/conrec.f90 src/face.f90 src/logging.f90 src/lttb.f90 src/fixed_array.f90 src/lz4.f90 src/classifier.f90 src/quantile.f90 src/unix_pthread.f90 src/fits.f90 src/main.c
# src/UStorage.f90 src/List.f90 # does not compile

# macOS Accelerate vImage is actually rather slow
ifeq ($(UNAME_S),Darwin)
#ifeq ($(UNAME_M),arm64)
	SRC += src/vimage.c
else
	SRC += src/ipp.c
endif

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

# `pkg-config --cflags libavif` `pkg-config --cflags libjpeg`
INC = `pkg-config --cflags glib-2.0` `pkg-config --cflags libmicrohttpd` `pkg-config --cflags libcurl` `pkg-config --cflags liblz4` `pkg-config --cflags cfitsio` `pkg-config --cflags x265` `pkg-config --cflags jemalloc` `pkg-config --cflags libczmq` `pkg-config --cflags libpq` -I./$(ZFP)/include -I./$(ZFP)/src 

ifneq ($(UNAME_S),Darwin)
	INC += `pkg-config --cflags libcpuid`
endif

ifneq ($(UNAME_S),Darwin)
#ifneq ($(UNAME_M),arm64)
	INC += -I${MKLROOT}/include/intel64/lp64 -I${MKLROOT}/include
endif

MOD =
# -I/home/chris/zfp/include
DEF = -DNODEBUG -DNO_MONGOOSE_HTTP_CLIENT -DMG_DATA_SIZE=50

ifneq ($(SHARE),)
	DEF += -DSHARE='"$(SHARE)"'
endif

# `pkg-config --libs libavif` `pkg-config --libs libjpeg`
LIBS = -L/usr/local/lib `pkg-config --libs glib-2.0` `pkg-config --libs libmicrohttpd` `pkg-config --libs liblz4` `pkg-config --libs cfitsio` -lsqlite3 `pkg-config --libs libcurl` -lz -pthread `pkg-config --libs libzmq` `pkg-config --libs libczmq` `pkg-config --libs x265` `pkg-config --libs libpq`

ifneq ($(UNAME_S),Darwin)
	LIBS += `pkg-config --libs libcpuid`
endif

# -lzfp before cfitsio
#`pkg-config --libs json-fortran`

# -lmpifort not needed when using mpiifort
# -L/home/chris/zfp/build/lib64

ifeq ($(CC),icc)
	# Intel FORTRAN runtime
	LIBS += -lifcore -limf
endif

ifeq ($(UNAME_S),Darwin)
	# INC += -I/usr/local/include -I/usr/local/opt/openssl/include -I/usr/local/opt/curl/include
	# LIBS += -L/usr/local/opt/openssl/lib -L/usr/local/opt/curl/lib -lcurl
	#MOD += `pkg-config --cflags json-fortran`

	INC += -I${HOMEBREW_PREFIX}/opt/libpq/include
	LIBS += -L${HOMEBREW_PREFIX}/opt/libpq/lib

	# CC = zig cc
	# INC += -I${HOMEBREW_PREFIX}/opt/libomp/include
	# LIBS += -L${HOMEBREW_PREFIX}/opt/libomp/lib -lomp

	CC = ${HOMEBREW_PREFIX}/opt/gcc/bin/gcc-12
	FORT = ${HOMEBREW_PREFIX}/opt/gcc/bin/gfortran-12
	FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	# -mcmodel=large results in "error: invalid variant 'BLEAH'"
	# Apple Silicon: -march=native conflicts between macOS-arm64 and macOS-x86_64 with Intel oneAPI
	CFLAGS := $(FLAGS) -flax-vector-conversions		
	FLAGS := $(FLAGS) -std=f2018 -fall-intrinsics

	# GCC FORTRAN runtime
	LIBS += -L${HOMEBREW_PREFIX}/opt/gcc/lib/gcc/12 -lgfortran -lm -framework Accelerate

	# use the built-in macOS Accelerate instead but only on Apple Silicon (OK, Intel macOS too)
	#ifeq ($(UNAME_M),arm64)
	IPP =
	MKL =
	#endif

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
	LIBS +=  -lgfortran -lm

	# try the zig compiler
	# CC = zig cc	
	# LIBS += -lomp
endif

# include dependencies (all .d files)
-include $(DEP)

%.o: %.ispc
	ispc -g -O3 --pic --opt=fast-math --addressing=64 -o $@ -h $(subst .o,.h,$@) $<

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

thread:
	gfortran -cpp -D__macOS__ src/my_threads.c  thread.f90 -pthread

mutexC:
	gcc mutex.c -pthread

mutexL:
	gfortran -cpp -D__linux__ src/unix_pthread.f90 mutex.f90 -pthread

#-corray-config-file=./config

contour:
	g++ -Ofast tests/test_contouring.cpp

clean:
	rm -f $(ZFP)/src/*.o $(ZFP)/src/*.d src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets
