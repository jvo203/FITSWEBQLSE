module lz4
    use, intrinsic :: iso_c_binding
    implicit none

    integer(kind=c_int), parameter :: LZ4HC_CLEVEL_MAX = 12

    ! C LZ4 API external functions
    interface
        integer(kind=c_int) function LZ4_compressBound(inputSize) BIND(C, name='LZ4_compressBound')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(kind=c_int), value, intent(in) :: inputSize
        end function LZ4_compressBound

        integer(kind=c_int) function LZ4_compress_HC(src, dst, srcSize,&
        &dstCapacity, compressionLevel) BIND(C, name='LZ4_compress_HC')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(C_PTR), value :: src
            type(C_PTR), value :: dst
            integer(kind=c_int), value, intent(in) :: srcSize
            integer(kind=c_int), value, intent(in) :: dstCapacity
            integer(kind=c_int), value, intent(in) :: compressionLevel
        end function LZ4_compress_HC

        integer(kind=c_int) function LZ4_decompress_safe(src, dst,&
        &compressedSize, dstCapacity) BIND(C, name='LZ4_decompress_safe')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(C_PTR), value :: src
            type(C_PTR), value :: dst
            integer(kind=c_int), value, intent(in) :: compressedSize
            integer(kind=c_int), value, intent(in) :: dstCapacity

        end function LZ4_decompress_safe

    end interface
contains
! Fortran entry subroutines

    subroutine compress_mask(mask, buffer)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        logical(kind=1), dimension(:, :), contiguous, target, intent(in) :: mask

        integer(kind=c_int) mask_size, worst_size, compressed_size

        ! the target buffer
        character(kind=c_char), allocatable, target, intent(out) :: buffer(:)

        mask_size = int(sizeof(mask), kind=c_int)
        print *, 'sizeof(mask) = ', mask_size, 'bytes'

        worst_size = LZ4_compressBound(mask_size)
        print *, 'worst_size = ', worst_size, 'bytes'

        allocate (buffer(worst_size))

        ! compress the mask as much as possible
        compressed_size = LZ4_compress_HC(c_loc(mask), c_loc(buffer),&
                                  &mask_size, worst_size, LZ4HC_CLEVEL_MAX)

        print *, 'compressed_size = ', compressed_size, 'bytes'
    end subroutine compress_mask

end module lz4
