program Wavelets
    use fixed_array
    use zfp_array
    use wavelet
    use lz4
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer(kind=4), parameter :: N = 4
    integer i, j

    real(kind=4), dimension(N, N) :: x, x_in
    real(kind=4), dimension(N, N) :: y
    logical(kind=1), dimension(N, N) :: mask
    character(kind=c_char), allocatable :: mask_buffer(:)
    character(kind=c_char), allocatable :: array_buffer(:)

    type(fixed_block), dimension(N/4, N/4) :: compressed
    type(fixed_block) :: compressed_block
    integer :: x1, x2, y1, y2

    ! FORTRAN-native ZFP
    type(zfp_block) :: compressed2
    integer(kind=2) :: bitmask

    do i = 1, N
        do j = 1, N
            x(i, j) = i*j
        end do
    end do

    ! insert a NaN value
    x(N/2, N/2) = ieee_value(0.0, ieee_quiet_nan)

    print *, 'BEFORE'
    do i = 1, N
        print *, x(i, :)
    end do

    ! a forward 2D wavelet transform
    call daub4_2Dtransform(N, x, y, mask)

    ! count the number of 0.0 coefficients
    print *, '# of zero coefficients:', count(y .eq. 0.0)

    print *, 'AFTER'
    do i = 1, N
        print *, y(i, :)
    end do

    ! an inverse transform to recover the data
    call daub4_2Dtransform_inv(N, y, x, mask)

    print *, 'RECOVERED'
    do i = 1, N
        print *, x(i, :)
    end do

    ! reset the source
    do i = 1, N
        do j = 1, N
            x(i, j) = i*j/100.0
        end do
    end do

    ! insert a NaN value
    x(N/2, N/2) = ieee_value(0.0, ieee_quiet_nan)

    print *, 'X:'
    do i = 1, N
        print *, x(i, :)
    end do

    ! an in-place transform
    call daub4_2Dtransform_inpl(n, x, mask)

    ! count the number of 0.0 coefficients
    print *, '# of zero coefficients:', count(x .eq. 0.0)

    print *, 'MASK'
    do i = 1, N
        print *, mask(i, :)
    end do

    print *, 'COEFFS:'
    do i = 1, N
        print *, x(i, :)
    end do

    ! an in-place inverse transform
    call daub4_2Dtransform_inv_inpl(n, x, mask)

    print *, 'RECOVERED'
    do i = 1, N
        print *, x(i, :)
    end do

    ! ZFP-like fixed-point 4x4 block with a common exponent

    ! reset the source
    do i = 1, N
        do j = 1, N
            x(i, j) = i*j/100.0
        end do
    end do

    ! insert a NaN value
    x(N/2, N/2) = ieee_value(0.0, ieee_quiet_nan)

    !  pick out all the NaN
    where (isnan(x))
        mask = .false.
    elsewhere
        mask = .true.
    end where

    ! replace NaNs with 0.0
    ! where (.not. mask) x = 0.0

    ! ZFP-like compression
    call to_fixed(x, compressed)

    ! testing irregular blocks
    x1 = 1
    x2 = 3
    y1 = 1
    y2 = 3

    x_in = ieee_value(0.0, ieee_quiet_nan)
    x_in(x1:x2, y1:y2) = x(x1:x2, y1:y2)

    print *, 'x_in'
    do i = 1, N
        print *, x_in(i, :)
    end do

    call to_fixed_block(x_in, compressed_block)
    call from_fixed_block(compressed_block, x)

    print *, 'sizeof(x):', sizeof(x), ', compressed size:', sizeof(compressed)
    print *, 'compression ratio:', real(sizeof(x))/real(sizeof(compressed))

    ! call LZ4-HC to compress the mask
    ! call compress_mask(mask, mask_buffer)

    ! compress the fixed array
    ! call compress_fixed_array(compressed, array_buffer)

    ! decompress mask
    ! call decompress_mask(mask_buffer, mask)

    ! call decompress_fixed_array(array_buffer, compressed)

    ! ZFP-like decompression
    ! call from_fixed(N, compressed, x)

    ! insert back NaN values
    ! where (.not. mask) x = ieee_value(0.0, ieee_quiet_nan)

    print *, 'FIXED-POINT DECOMPRESSION'
    do i = 1, N
        print *, x(i, :)
    end do

    print *, compressed
    print *, mask

    call zfp_compress_block(x, compressed2)
    call zfp_decompress_block(compressed2, x, bitmask)

    print *, 'native ZFP decompression', x
    write (*, '(a,b32.32)') 'bitmask ', bitmask

end program Wavelets
