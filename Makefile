.DEFAULT_GOAL := fitswebqlse

CC := icc
FORT := ifort
TARGET = fitswebqlse

SRC = src/main.f90 src/http.c src/net.f90
OBJ := $(SRC:.f90=.o)
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.ispc=.o)
DEP = $(OBJ:%.o=%.d)

FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2
CFLAGS := $(FLAGS)
#FLAGS += -coarray=distributed
LIBS = -L/usr/local/lib -lmpifort -lcfitsio -lmicrohttpd

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

main:
	ifort $(FLAGS) src/main.f90 -o fitswebqlse $(LIBS)

#-corray-config-file=./config

clean:
	rm -f src/*.o src/*.d *.o *.d $(TARGET)