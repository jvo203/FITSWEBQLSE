.DEFAULT_GOAL := intel

# get the Homebrew installation directory
# brew --prefix is not available from inside the Homebrew environment
HOMEBREW_PREFIX := $(shell brew --prefix)

# assume Intel macOS	
ifeq ($(HOMEBREW_PREFIX),)
	HOMEBREW_PREFIX := $(shell /usr/local/bin/brew --prefix)
endif

# if it still cannot be found assume Apple Silicon macOS
ifeq ($(HOMEBREW_PREFIX),)
	HOMEBREW_PREFIX := $(shell /opt/homebrew/bin/brew --prefix)
endif

# the stack should be unlimited to avoid problems
# with ifort creating on the stack large temporary arrays
# ulimit -s unlimited

# detect the OS
UNAME_S := $(shell uname -s)

# detect the CPU architecture (ARM64 or x86-64)
UNAME_M := $(shell uname -m)

LIBS = `pkg-config --libs cfitsio`
# GCC FORTRAN runtime
LIBS += -L${HOMEBREW_PREFIX}/opt/gcc/lib/gcc/13 -lgfortran -lm -framework Accelerate

FORT = ${HOMEBREW_PREFIX}/opt/gcc/bin/gfortran-13
FLAGS = -march=native -g -Ofast -fPIC -fno-finite-math-only -funroll-loops -ftree-vectorize

TARGET = intel

intel:
	$(FORT) $(FLAGS) -o $(TARGET) intel_fixed_array.f90 $(LIBS)