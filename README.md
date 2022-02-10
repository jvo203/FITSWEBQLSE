![Alt text](IVOA.JPG?raw=true "JVO Breaking Through the 1TB Barrier")

# FITSWEBQL SE

# status
in active development

## PROJECT LAZARUS (a.k.a. FORTRAN RESURRECTED) (January 2022 ~)
A resurrection of the previous Fortran code, this time without CoArrays but with manual custom distributed computing. Julia has been found to suffer from serious RAM fragmentation issues after running for "long-enough" in a 24h server environment.

## BACK TO THE FUTURE (July 2021 ~ January 2022)
FITSWEBQLSE (Supercomputer Edition) coded in Julia. Originally Julia was positioned as a better (faster) Python alternative but to me it seems more like "FORTRAN ON STEROIDS". Julia's handling of array and vector data types resembles Fortran. But it also brings networking capabilities and low-level systems programming, which Fortran lacks (one needs to call C from Fortran). Julia's multi-threading and especially multi-processing (distributed computing) are superior to anything else.

## A BLAST FROM THE PAST (December 2020 ~ June 2021)
An experimental Fortran 2018 FITSWebQL built with Co-Array Fortran & MPI, using NASA's FITSIO to read FITS files. FITSWEBQL SE scales across multiple computers (cluster nodes).

Co-Array Fortran has since been replaced by Julia since it (Fortran 2018) lacks concurrent (asynchronous) Co-Arrays. Out of all programming languages, Julia has the best distributed computing capabilities.