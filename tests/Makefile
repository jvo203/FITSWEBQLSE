.DEFAULT_GOAL := intel

# GCC FORTRAN
FORT = gfortran
FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp

# Intel FORTRAN
IFORT = ifx # ifort or ifx
IFLAGS = -g -Ofast -xHost -axAVX -qopt-report=2 -qopenmp -mcmodel=large -parallel

gcc:
	$(FORT) $(FLAGS) -o intel_fixed_array intel_fixed_array.f90

intel:
	$(IFORT) $(IFLAGS) -o intel_fixed_array intel_fixed_array.f90

clean:
	rm -f intel_fixed_array

INC = `pkg-config --cflags cfitsio`
LIBS = `pkg-config --libs cfitsio`

image:
	CC $(FLAGS) $(INC) -o test_image_compression test_image_compression.c $(LIBS) -lm