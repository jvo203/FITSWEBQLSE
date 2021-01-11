program Wavelets
    use wavelet
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer(kind=4), parameter :: N = 8
    integer i, j

    real(kind=4), dimension(N, N) :: x
    real(kind=4), dimension(N, N) :: y
    logical, dimension(N, N) :: mask

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
            x(i, j) = i*j
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

end program Wavelets
