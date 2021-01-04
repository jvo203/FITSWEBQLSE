.DEFAULT_GOAL := main

FLAGS = -Ofast -xHost -mavx -axAVX -qopt-report=2
LIBS = -L/usr/local/lib -lcfitsio -lmpifort -lmicrohttpd

main:
	ifort $(FLAGS) src/main.f90 -o fitswebqlse $(LIBS)

#-corray-config-file=./config