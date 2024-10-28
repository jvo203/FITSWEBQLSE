.DEFAULT_GOAL := de

# GCC FORTRAN
FORT = gfortran
FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize
#  -fopenmp

# Intel FORTRAN
IFORT = ifx # ifort or ifx
IFLAGS = -g -Ofast -xHost -axAVX -qopt-report=2 -qopenmp -mcmodel=large -parallel

gcc:
	$(FORT) $(FLAGS) -o intel_fixed_array intel_fixed_array.f90

intel:
	$(IFORT) $(IFLAGS) -o intel_fixed_array intel_fixed_array.f90

#clean:
#rm -f intel_fixed_array

# differential evolution
de:
	$(FORT) $(FLAGS) -fopenmp -o test_de ../src/mt19937-64.f90 ../src/differential_evolution.f90 test_de.f90

INC = `pkg-config --cflags cfitsio` `pkg-config --cflags wcslib`
LIBS = `pkg-config --libs cfitsio` `pkg-config --libs wcslib`

image:
	$(CC) $(FLAGS) $(INC) -o test_image_compression test_image_compression.c $(LIBS) -lm

#INC := -I/Users/chris/Downloads/cfitsio-4.4.0/include
#LIBS := -L/Users/chris/Downloads/cfitsio-4.4.0/lib -lcfitsio

wcslib:
	$(CC) $(FLAGS) -I/Users/chris/Downloads/cfitsio-4.4.0/include `pkg-config --cflags wcslib` -o test_wcslib test_wcslib.c -L/Users/chris/Downloads/cfitsio-4.4.0/lib -lcfitsio `pkg-config --libs wcslib`
#$(CC) $(FLAGS) `pkg-config --cflags cfitsio` `pkg-config --cflags wcslib` -o test_wcslib test_wcslib.c `pkg-config --libs cfitsio` `pkg-config --libs wcslib`

run:
	DYLD_LIBRARY_PATH=/Users/chris/Downloads/cfitsio-4.4.0/lib ./test_wcslib

%.o: %.c
	$(CC) $(FLAGS) $(INC) -MMD -o $@ -c $<

WCSTOOLS = $(HOME)/Downloads/wcssubs
SRC := $(WCSTOOLS)/wcsinit.c $(WCSTOOLS)/poly.c $(WCSTOOLS)/zpxpos.c $(WCSTOOLS)/wcs.c $(WCSTOOLS)/worldpos.c $(WCSTOOLS)/platepos.c $(WCSTOOLS)/wcslib.c $(WCSTOOLS)/proj.c $(WCSTOOLS)/wcscon.c $(WCSTOOLS)/dsspos.c $(WCSTOOLS)/wcstrig.c $(WCSTOOLS)/cel.c $(WCSTOOLS)/sph.c $(WCSTOOLS)/distort.c $(WCSTOOLS)/hget.c $(WCSTOOLS)/iget.c $(WCSTOOLS)/lin.c $(WCSTOOLS)/tnxpos.c $(WCSTOOLS)/hput.c
OBJ := $(SRC:.c=.o)

INC := `pkg-config --cflags cfitsio` -I$(WCSTOOLS)
LIBS := `pkg-config --libs cfitsio`

wcssub: $(OBJ)
	$(CC) $(FLAGS) $(INC) $(OBJ) -o test_wcssub test_wcssub.c $(LIBS)

wcsast:
	$(CC) $(CFLAGS) $(INC) -o test_wcsast test_wcsast.c $(LIBS) `ast_link`

starlink:
	$(CC) $(CFLAGS) -o test_starlink test_starlink.c -L/usr/local/lib `ast_link` -pthread

clean:
	rm -f test_wcslib test_wcssub test_starlink test_wcsast *.o *.d $(WCSTOOLS)/*.o $(WCSTOOLS)/*.d