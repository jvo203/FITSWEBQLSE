module lz4
    use, intrinsic :: iso_c_binding
    implicit none

    ! C LZ4 API external functions
    interface
        integer(kind=c_int) function LZ4_compressBound(inputSize) BIND(C, name='LZ4_compressBound')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(kind=c_int), value, intent(in) :: inputSize
        end function LZ4_compressBound

    end interface
contains
! Fortran entry subroutines

    subroutine compress_mask(mask)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        logical(kind=1), dimension(:, :), contiguous, intent(in) :: mask

        integer(kind=c_int) mask_size, worst_size, compressed_size
        character(kind=c_char), allocatable, target :: buffer(:)

        mask_size = int(sizeof(mask), kind=c_int)
        print *, 'sizeof(mask) = ', mask_size, 'bytes'

        worst_size = LZ4_compressBound(mask_size)
        print *, 'worst_size = ', worst_size, 'bytes'

        allocate (buffer(worst_size))
    end subroutine compress_mask

end module lz4
