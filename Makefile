.DEFAULT_GOAL := caf

LIBS = -L/usr/local/lib -lcfitsio -lmpifort

mpi:
	ifort -O3 -xHost MPI_hello.f90 -o MPI_hello -lmpifort

caf:
	ifort -O3 -Ofast -xHost -mavx -axAVX -qopt-report=2 -coarray=distributed test_caf.f90 -o test_caf $(LIBS)

#-corray-config-file=./config