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

    subroutine compress_mask(mask, compressed)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        logical(kind=1), dimension(:, :), contiguous, target, intent(in) :: mask

        ! internal variables
        character(kind=c_char), allocatable, target :: buffer(:)
        integer(kind=c_int) mask_size, worst_size, compressed_size

        ! the output
        character(kind=c_char), allocatable, intent(out) :: compressed(:)

        mask_size = int(sizeof(mask), kind=c_int)
        print *, 'sizeof(mask) = ', mask_size, 'bytes'

        worst_size = LZ4_compressBound(mask_size)
        print *, 'worst_size = ', worst_size, 'bytes'

        allocate (buffer(worst_size))

        ! compress the mask as much as possible
        compressed_size = LZ4_compress_HC(c_loc(mask), c_loc(buffer),&
                                  &mask_size, worst_size, LZ4HC_CLEVEL_MAX)

        print *, 'compressed mask size = ', compressed_size, 'bytes'

        ! resize the output buffer to match the actual compressed size
        if (compressed_size .gt. 0) compressed = reshape(buffer, (/compressed_size/))
    end subroutine compress_mask

    subroutine decompress_mask(compressed, mask)
        use, intrinsic :: iso_c_binding
        implicit none

        ! the input
        character(kind=c_char), contiguous, target, intent(in) :: compressed(:)

        ! the output
        logical(kind=1), dimension(:, :), contiguous, target, intent(inout) :: mask

        ! internal variables
        integer(kind=c_int) mask_size, compressed_size, decompressed_size

        compressed_size = int(sizeof(compressed), kind=c_int)
        mask_size = int(sizeof(mask), kind=c_int)

        decompressed_size = LZ4_decompress_safe(c_loc(compressed), c_loc(mask), compressed_size, mask_size)
        print *, 'decompressed mask size = ', decompressed_size, 'bytes'
    end subroutine decompress_mask

    subroutine compress_fixed_array(array, compressed)
        use, intrinsic :: iso_c_binding
        use fixed_array
        implicit none

        ! inputs
        type(fixed_block), dimension(:, :), contiguous, target, intent(in) :: array

        ! internal variables
        character(kind=c_char), allocatable, target :: buffer(:)
        integer(kind=c_int) array_size, worst_size, compressed_size

        ! the output
        character(kind=c_char), allocatable, intent(out) :: compressed(:)

        array_size = int(sizeof(array), kind=c_int)
        print *, 'sizeof(array) = ', array_size, 'bytes'

        worst_size = LZ4_compressBound(array_size)
        print *, 'worst_size = ', worst_size, 'bytes'

        allocate (buffer(worst_size))

        ! compress the mask as much as possible
        compressed_size = LZ4_compress_HC(c_loc(array), c_loc(buffer),&
                                  &array_size, worst_size, LZ4HC_CLEVEL_MAX)

        print *, 'compressed array size = ', compressed_size, 'bytes'

        ! resize the output buffer to match the actual compressed size
        if (compressed_size .gt. 0) compressed = reshape(buffer, (/compressed_size/))
    end subroutine compress_fixed_array

    subroutine decompress_fixed_array(compressed, array)
        use, intrinsic :: iso_c_binding
        use fixed_array
        implicit none

        ! the input
        character(kind=c_char), contiguous, target, intent(in) :: compressed(:)

        ! the output
        type(fixed_block), dimension(:, :), contiguous, target, intent(inout) :: array

        ! internal variables
        integer(kind=c_int) array_size, compressed_size, decompressed_size

        compressed_size = int(sizeof(compressed), kind=c_int)
        array_size = int(sizeof(array), kind=c_int)

        decompressed_size = LZ4_decompress_safe(c_loc(compressed), c_loc(array), compressed_size, array_size)
        print *, 'decompressed array size = ', decompressed_size, 'bytes'
    end subroutine decompress_fixed_array

end module lz4
