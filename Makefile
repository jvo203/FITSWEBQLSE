.DEFAULT_GOAL := fitswebqlse

# the stack should be unlimited to avoid problems
# with ifort creating on the stack large temporary arrays
# ulimit -s unlimited

# detect the OS
UNAME_S := $(shell uname -s)

# default settings on Linux
# the macOS Darwin target is handled further down the line
CC := icc
FORT := mpiifort
TARGET = fitswebqlse

SRC = src/http.c src/json.c src/wavelet.f90 src/histogram.c src/classifier.f90 src/fits.f90 src/net.f90 src/main.f90
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2
#-mcmodel=medium
#-qopenmp
#-ipo -parallel -fast
CFLAGS := $(FLAGS)
INC = `pkg-config --cflags glib-2.0`
DEF = -DLOCAL
FLAGS += -align array64byte -coarray=distributed
LIBS = -L/usr/local/lib -lcfitsio -lmicrohttpd -lwebsockets `pkg-config --libs glib-2.0` `pkg-config --libs json-fortran`
# -lmpifort not needed when using mpiifort

ifeq ($(UNAME_S),Darwin)
	INC += -I/usr/local/opt/openssl/include
	LIBS += -L/usr/local/opt/openssl/lib -lcaf_mpi

	CC = gcc
	FORT = mpif90
	FLAGS = -march=native -g -Ofast -fno-finite-math-only -funroll-loops -ftree-vectorize
	CFLAGS := $(FLAGS)
	FLAGS += -cpp -fallow-invalid-boz -fcoarray=lib `pkg-config --cflags json-fortran`
endif

# include dependencies (all .d files)
-include $(DEP)

%.o: %.ispc
	ispc -g -O3 --pic --opt=fast-math --addressing=32 -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(DEF) $(INC) -MMD -o $@ -c $<

%.o: %.f90
	$(FORT) $(FLAGS) -MMD -o $@ -c $<

fitswebqlse: $(OBJ)
	$(FORT) $(FLAGS) -o $(TARGET) $^ $(LIBS)

test:
	ifort -Ofast -xHost -mavx -axAVX -qopt-report=2 src/wavelet.f90 src/testWavelets.f90 -o testWavelets

mpi:
	$(FORT) -Ofast -xHost test.f90 -o test

#-corray-config-file=./config

clean:
	rm -f src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets