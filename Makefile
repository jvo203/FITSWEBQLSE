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

# src/zforp.f90 src/zfp.f90
ERLOFF_SRC := json/erloff/module_m.f90 json/erloff/procedure_m.f90 json/erloff/call_stack_entry_m.f90 json/erloff/call_stack_m.f90 json/erloff/debug_level_m.f90 json/erloff/message_type_m.f90 json/erloff/message_m.f90 json/erloff/debug_m.f90 json/erloff/error_m.f90 json/erloff/error_item_m.f90 json/erloff/error_list_m.f90 json/erloff/fatal_m.f90 json/erloff/info_m.f90 json/erloff/internal_m.f90 json/erloff/message_item_m.f90 json/erloff/message_list_m.f90 json/erloff/warning_m.f90 json/erloff.f90
JSONFF_SRC := json/jsonff/json_value_m.f90 json/jsonff/json_string_m.f90 json/jsonff/fallible_json_value_m.f90 json/jsonff/json_element_m.f90 json/jsonff/json_array_m.f90 json/jsonff/json_false_m.f90 json/jsonff/json_true_m.f90 json/jsonff/json_member_m.f90 json/jsonff/json_null_m.f90 json/jsonff/json_number_m.f90 json/jsonff/json_object_m.f90 json/jsonff/parsed_element_m.f90 json/jsonff/parsers_m.f90 json/jsonff/fallible_json_string_m.f90 json/jsonff.f90
# json/strff.f90 $(ERLOFF_SRC) $(JSONFF_SRC)

JSON_SRC := json-fortran/json_kinds.F90 json-fortran/json_parameters.F90 json-fortran/json_string_utilities.F90 json-fortran/json_value_module.F90 json-fortran/json_file_module.F90 json-fortran/json_module.F90
SRC = src/iso_varying_string.f90 $(JSON_SRC) src/http.c src/json.c src/wavelet.f90 src/fixed_array.f90 src/lz4.f90 src/histogram.c src/classifier.f90 src/fits.f90 src/net.f90 src/main.f90
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
# -I/home/chris/zfp/include
DEF = -DLOCAL
FLAGS += -align array64byte -coarray=distributed
LIBS = -L/usr/local/lib -lmicrohttpd -lwebsockets `pkg-config --libs glib-2.0` -llz4 -L/usr/local/lib64 -lzfp -lcfitsio
# before cfitsio goes
# 

# -lmpifort not needed when using mpiifort
# -L/home/chris/zfp/build/lib64

ifeq ($(UNAME_S),Darwin)
	INC += -I/usr/local/opt/openssl/include
	LIBS += -L/usr/local/opt/openssl/lib -lcaf_mpi -L/usr/local/opt/lz4/lib -llz4

	CC = gcc
	FORT = mpif90
	FLAGS = -march=native -g -Ofast -fno-finite-math-only -funroll-loops -ftree-vectorize
	CFLAGS := $(FLAGS)
	FLAGS += -cpp -fallow-invalid-boz -fcoarray=lib
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
	ifort -Ofast -xHost -mavx -axAVX -qopt-report=2 src/wavelet.f90 src/fixed_array.f90 src/lz4.f90 src/testWavelets.f90 -o testWavelets -llz4
# on macos -L/usr/local/opt/lz4/lib -llz4

mpi:
	$(FORT) -Ofast -xHost test.f90 -o test

zfp:
#$(CC) $(CFLAGS) tests/zfp_compress.c -o zfp_compress -lzfp
	$(FORT) $(FLAGS) src/zforp.f90 tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzfp -L/usr/local/lib -lcfitsio
#-L/home/chris/zfp/build/lib64 -lzfp

gzfp:
	gfortran -Ofast -I/home/chris/zfp/build/modules tests/zfp_compress.f90 -o zfp_compress -L/home/chris/zfp/build/lib64 -lzFORp

#-corray-config-file=./config

clean:
	rm -f src/*.mod src/*.o src/*.d src/*.optrpt *.mod *.o *.d *.optrpt $(TARGET) testWavelets