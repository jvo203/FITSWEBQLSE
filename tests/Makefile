.DEFAULT_GOAL := intel

# GCC FORTRAN
FORT = gfortran
FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize -fopenmp

# Intel FORTRAN
IFORT = ifx # ifort or ifx
IFLAGS = -g -Ofast -xHost -axAVX -qopt-report=2
# -qopenmp

gcc:
	$(FORT) $(FLAGS) -o intel_fixed_array intel_fixed_array.f90

intel:
	$(IFORT) $(IFLAGS) -o intel_fixed_array intel_fixed_array.f90

clean:
	rm -f intel_fixed_array