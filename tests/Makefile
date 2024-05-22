.DEFAULT_GOAL := wcssub

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

clean:
	rm -f intel_fixed_array

INC = `pkg-config --cflags cfitsio` `pkg-config --cflags wcslib`
LIBS = `pkg-config --libs cfitsio` `pkg-config --libs wcslib`

image:
	$(CC) $(FLAGS) $(INC) -o test_image_compression test_image_compression.c $(LIBS) -lm

wcslib:
	$(CC) $(FLAGS) $(INC) -o test_wcslib test_wcslib.c $(LIBS) -lm

%.o: %.c
	$(CC) $(FLAGS) $(INC) -MMD -o $@ -c $<

WCSTOOLS = $(HOME)/Downloads/wcssubs
SRC := $(WCSTOOLS)/wcsinit.c $(WCSTOOLS)/poly.c $(WCSTOOLS)/zpxpos.c $(WCSTOOLS)/wcs.c $(WCSTOOLS)/worldpos.c $(WCSTOOLS)/platepos.c $(WCSTOOLS)/wcslib.c $(WCSTOOLS)/proj.c $(WCSTOOLS)/wcscon.c $(WCSTOOLS)/dsspos.c $(WCSTOOLS)/wcstrig.c $(WCSTOOLS)/cel.c $(WCSTOOLS)/sph.c $(WCSTOOLS)/distort.c $(WCSTOOLS)/hget.c $(WCSTOOLS)/iget.c $(WCSTOOLS)/lin.c $(WCSTOOLS)/tnxpos.c
OBJ := $(SRC:.c=.o)

wcssub: $(OBJ)
	$(CC) $(FLAGS) $(INC) -o test_wcssub test_wcssub.c $(LIBS) -lm