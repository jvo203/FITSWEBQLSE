module lz4
    use, intrinsic :: iso_c_binding
    implicit none

    ! C LZ4 API external functions
    interface

    end interface
contains
! Fortran entry subroutines
    subroutine compress_mask(n, mask)
        implicit none

        integer(kind=4) :: n
        logical(kind=1), dimension(n, n), optional, intent(in) :: mask
    end subroutine compress_mask
end module lz4
