module fits
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer(kind=4), parameter :: NBINS = 1024
    real, parameter :: PI = 4.D0*DATAN(1.D0)

    integer(c_int), parameter :: FPZIP_MEDIUM_PRECISION = 16
    integer(c_int), parameter :: FPZIP_HIGH_PRECISION = 24

    integer(c_int), parameter :: ZFP_HIGH_PRECISION = 16
    integer(c_int), parameter :: ZFP_MEDIUM_PRECISION = 11
    integer(c_int), parameter :: ZFP_LOW_PRECISION = 8
    integer(c_int), parameter :: ZFP_MIN_EXP = -1074

    type, bind(c) :: gmutex
        integer(kind=c_intptr_t) :: i = 0
    end type gmutex

    enum, bind(C)
        enumerator circle
        enumerator square
    end enum

    enum, bind(C)
        enumerator mean
        enumerator integrated
    end enum

    enum, bind(C)
        enumerator low
        enumerator medium
        enumerator high
    end enum
end module fits
