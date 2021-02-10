module lz4
    use, intrinsic :: iso_c_binding
    implicit none

    ! C LZ4 API external functions
    interface

    end interface
contains
! Fortran entry subroutines
    subroutine compress_mask(mask)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        logical(kind=1), dimension(:, :), contiguous, intent(in) :: mask

        integer(kind=c_int) worst_size, compressed_size
    end subroutine compress_mask
end module lz4
