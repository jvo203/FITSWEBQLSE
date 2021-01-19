.DEFAULT_GOAL := fitswebqlse

CC := icc
FORT := mpiifort
TARGET = fitswebqlse

SRC = src/http.c src/json.c src/wavelet.f90 src/fits.f90 src/net.f90 src/main.f90
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2
#-qopenmp
CFLAGS := $(FLAGS)
DEF = -DLOCAL
FLAGS += -align array64byte -coarray=distributed
LIBS = -L/usr/local/lib -lcfitsio -lmicrohttpd -lwebsockets
# -lmpifort not needed when using mpiifort

# include dependencies (all .d files)
-include $(DEP)

%.o: %.ispc
	ispc -g -O3 --pic --opt=fast-math --addressing=32 -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(DEF) -MMD -o $@ -c $<

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