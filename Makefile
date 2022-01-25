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

JEMALLOC = -L`jemalloc-config --libdir` -Wl,-rpath,`jemalloc-config --libdir` -ljemalloc `jemalloc-config --libs`
TARGET = fitswebqlse

# Intel Integrated Performance Primitives Library
ifeq ($(UNAME_S),Linux)
        IPP = -L${IPPROOT}/lib/intel64
endif

ifeq ($(UNAME_S),Darwin)
        IPP = -L${IPPROOT}/lib 
endif

IPP += -lippi -lippdc -lipps -lippcore

ZFP = zfp
ZFP_SRC := $(wildcard $(ZFP)/src/*.c)

# src/zforp.f90 src/zfp.f90
# src/psrs_sort.c
# src/iso_varying_string.f90
# src/zmq.f90
# src/wavelet.f90
# src/lz4.f90
# src/ipp.c src/psrs_sort.c src/http.c src/hash_table.c src/json.c src/json_write.c src/m_mrgrnk.f90 src/mod_sort.f90 src/wavelet.f90 src/fixed_array.f90 src/fixed_array2.f90 src/zfp_array.f90 src/histogram.c src/classifier.f90 src/fits_omp.f90 src/net.f90 src/main.f90
SRC = $(ZFP_SRC) src/webql.ispc src/json.c src/ini.c src/mongoose.c src/mjson.c src/hash_table.c src/cluster.c src/http.c src/ws.c src/main.c
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

ifeq ($(CC),icc)
	FLAGS = -g -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp -mcmodel=medium -shared-intel
# -parallel
#-mcmodel=medium
#-ipo -parallel -fast
# -ipo causes segmentation faults ...
# -fast causes static linking problems

	CFLAGS := $(FLAGS)
	FLAGS += -heap-arrays 32 -align array64byte
#-mt_mpi
endif

INC = `pkg-config --cflags glib-2.0` `pkg-config --cflags libcpuid` -I./$(ZFP)/include -I./$(ZFP)/src
MOD =
# -I/home/chris/zfp/include
DEF = -DLOCAL

LIBS = -L/usr/local/lib -lmicrohttpd `pkg-config --libs glib-2.0` `pkg-config --libs libcpuid` -llz4 -L/usr/local/lib64 -lcfitsio -lsqlite3 -lcurl -lz -pthread -lzmq -lczmq
# -lzfp before cfitsio
#`pkg-config --libs json-fortran`

# -lmpifort not needed when using mpiifort
# -L/home/chris/zfp/build/lib64

ifeq ($(UNAME_S),Darwin)
	INC += -I/usr/local/include -I/usr/local/opt/openssl/include -I/usr/local/opt/curl/include
	LIBS += -L/usr/local/opt/openssl/lib -L/usr/local/opt/lz4/lib -llz4 -L/usr/local/opt/curl/lib -lcurl
#MOD += `pkg-config --cflags json-fortran`

	CC = gcc-11
	FORT = gfortran-11
	FLAGS = -march=native -mcmodel=medium -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	CFLAGS := $(FLAGS)

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
		FLAGS += -cpp -fallow-invalid-boz -fmax-stack-var-size=32768
	endif
endif

# detect the GNU Compiler under Linux
ifeq ($(CC),gcc)
	override CFLAGS += -march=native -mcmodel=medium -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	FLAGS := $(CFLAGS)

	ifeq ($(FORT),nagfor)
		MPI_LINK_FLAGS = $(shell mpifort --showme:link)
		FLAGS := -target=core2 -O4 -f2018 -kind=byte -openmp -colour $(MPI_LINK_FLAGS)
	else
		FLAGS += -cpp -fallow-invalid-boz -fmax-stack-var-size=32768
	endif
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
	$(CC) $(CFLAGS) -o $(TARGET) $^ $(LIBS) $(IPP) $(JEMALLOC)

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

encode:
	ispc -g -O3 --pic --opt=fast-math --addressing=32 src/webql.ispc -o src/zfp.o -h tests/webql.h
	$(CC) $(CFLAGS) tests/zfp_encode.c src/zfp.o -o zfp_encode -lm
#icc -O0 tests/zfp_encode.c -o zfp_encode -lm

gzfp:
	gfortran -Ofast -I/home/chris/zfp/build/modules tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzFORp

#-corray-config-file=./config

clean:
	rm -f $(ZFP)/src/*.o $(ZFP)/src/*.d src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets
