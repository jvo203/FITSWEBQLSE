# FITSWEBQL SE

## A BLAST FROM THE PAST
An experimental Fortran 2018 FITSWebQL built with Co-Array Fortran & MPI, using NASA's FITSIO to read FITS files. FITSWEBQL SE scales across multiple computers (cluster nodes).

Co-Array Fortran has since been replaced by Julia since it (Fortran 2018) lacks concurrent (asynchronous) Co-Arrays. Out of all programming languages, Julia has the best distributed computing capabilities.

## BACK TO THE FUTURE
FITSWEBQLSE (Supercomputer Edition) coded in Julia. Originally Julia was positioned as a better (faster) Python alternative but to me it seems more like "FORTRAN ON STEROIDS". Julia's handling of array and vector data types resembles Fortran. But it also brings networking capabilities and low-level systems programming, which Fortran lacks (one needs to call C from Fortran). Julia's multi-threading and especially multi-processing (distributed computing) are superior to anything else.