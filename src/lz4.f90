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

        integer(kind=c_int) mask_size, worst_size, compressed_size

        mask_size = int(sizeof(mask), kind=c_int)
        print *, 'sizeof(mask) = ', mask_size, 'bytes'
    end subroutine compress_mask
end module lz4
