.DEFAULT_GOAL := main

LIBS = -L/usr/local/lib -lcfitsio -lmpifort

main:
	ifort -O3 -Ofast -xHost -mavx -axAVX -qopt-report=2 -coarray=distributed main.f90 -o fitswebqlse $(LIBS)

#-corray-config-file=./config