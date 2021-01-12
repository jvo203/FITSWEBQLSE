.DEFAULT_GOAL := fitswebqlse

CC := icc
FORT := mpiifort
TARGET = fitswebqlse

SRC = src/main.f90 src/http.c src/net.f90 src/wavelet.f90
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2
CFLAGS := $(FLAGS)
FLAGS += -coarray=distributed
LIBS = -L/usr/local/lib -lcfitsio -lmicrohttpd
# -lmpifort not needed when using mpiifort

# include dependencies (all .d files)
-include $(DEP)

%.o: %.ispc
	ispc -g -O3 --pic --opt=fast-math --addressing=32 -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -MMD -o $@ -c $<

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
	rm -f src/*.o src/*.d src/*.optrpt *.o *.d *.optrpt $(TARGET) testWavelets