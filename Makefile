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

	# default settings on Linux
	ifeq ($(UNAME_S),Linux)

		# found an AMD CPU
		ifneq (,$(findstring AuthenticAMD,$(CPU_S)))
			# GNU gcc / gfortran with OpenCoarrays
			CC := gcc
			FORT := mpifort
		endif

		# found an Intel CPU
		ifneq (,$(findstring GenuineIntel,$(CPU_S)))
			# Intel oneAPI icc / ifort 
			CC := icc
			FORT := mpiifort
		endif

	endif

endif

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
SRC = $(ZFP_SRC) src/ipp.c src/psrs_sort.c src/http.c src/hash_table.c src/json.c src/json_write.c src/m_mrgrnk.f90 src/mod_sort.f90 src/fixed_array.f90 src/histogram.c src/classifier.f90 src/fits_omp.f90 src/net.f90 src/main.f90
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

ifeq ($(CC),icc)
	FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp -mcmodel=medium -shared-intel
# -parallel
#-mcmodel=medium
#-ipo -parallel -fast
# -ipo causes segmentation faults ...
# -fast causes static linking problems

	CFLAGS := $(FLAGS)
	FLAGS += -heap-arrays 32 -align array64byte -coarray=distributed
#-mt_mpi
endif

INC = `pkg-config --cflags glib-2.0` -I./$(ZFP)/include -I./$(ZFP)/src
MOD =
# -I/home/chris/zfp/include
DEF = -DLOCAL

LIBS = -L/usr/local/lib -lmicrohttpd -lwebsockets `pkg-config --libs glib-2.0` -llz4 -L/usr/local/lib64 -lfpzip -lcfitsio -lsqlite3
# -lzmq -lczmq
# -lzfp before cfitsio
#`pkg-config --libs json-fortran`

# -lmpifort not needed when using mpiifort
# -L/home/chris/zfp/build/lib64

ifeq ($(UNAME_S),Darwin)
	INC += -I/usr/local/include -I/usr/local/opt/openssl/include
	LIBS += -L/usr/local/opt/openssl/lib -lcaf_mpi -L/usr/local/opt/lz4/lib -llz4
#MOD += `pkg-config --cflags json-fortran`

	CC = gcc-10
	FORT = mpifort

	FLAGS = -march=native -mcmodel=medium -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	CFLAGS := $(FLAGS)

	ifeq ($(FORT),nagfor)
		MPI_LINK_FLAGS = $(shell mpifort --showme:link)
		FLAGS := -target=core2 -O4 -f2018 -kind=byte -coarray=single -openmp -colour $(MPI_LINK_FLAGS)
	else
		FLAGS += -cpp -fallow-invalid-boz -fcoarray=lib -fmax-stack-var-size=32768
	endif
endif

# detect the GNU Compiler under Linux
ifeq ($(CC),gcc)
	override CFLAGS += -march=native -mcmodel=medium -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp
	FLAGS := $(CFLAGS)
	LIBS += -L/usr/local/opencoarrays/2.9.2/lib64 -lcaf_mpi

	ifeq ($(FORT),nagfor)
		MPI_LINK_FLAGS = $(shell mpifort --showme:link)
		FLAGS := -target=core2 -O4 -f2018 -kind=byte -coarray=single -openmp -colour $(MPI_LINK_FLAGS)
	else
		FLAGS += -cpp -fallow-invalid-boz -fcoarray=lib -fmax-stack-var-size=32768
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
	$(FORT) $(FLAGS) -o $(TARGET) $^ $(LIBS) $(IPP)

test:
	ifort -Ofast -xHost -mavx -axAVX -qopt-report=2 src/wavelet.f90 src/fixed_array.f90 src/lz4.f90 src/testWavelets.f90 -o testWavelets -llz4
# on macos -L/usr/local/opt/lz4/lib -llz4

json:
	ifort -Ofast -xHost -mavx -axAVX -qopt-report=2 tests/json_serialize.f90 -o serialize `pkg-config --libs json-fortran`

mpi:
	$(FORT) -Ofast -xHost test.f90 -o test

zmq:
#$(FORT) -Ofast -xHost -mavx -axAVX -qopt-report=2 -qopenmp src/zmq.f90 tests/test_zmq.f90 -o test_zmq -lzmq -lczmq
	gfortran -Ofast -fopenmp src/zmq.f90 tests/test_zmq.f90 -o test_zmq -L/usr/local/lib -lzmq -lczmq

zfp:
#$(CC) $(CFLAGS) tests/zfp_compress.c -o zfp_compress -lzfp
	$(FORT) $(FLAGS) src/zforp.f90 tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzfp -L/usr/local/lib -lcfitsio
#-L/home/chris/zfp/build/lib64 -lzfp

gzfp:
	gfortran -Ofast -I/home/chris/zfp/build/modules tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzFORp

#-corray-config-file=./config

clean:
	rm -f $(ZFP)/src/*.o $(ZFP)/src/*.d src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets
