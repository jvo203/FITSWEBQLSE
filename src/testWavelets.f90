program Wavelets
    implicit none

    integer(kind=4), parameter :: N = 8
    integer i, j

    real(kind=4), dimension(N, N) :: x
    real(kind=4), dimension(N, N) :: y

    do i = 1, N
        do j = 1, N
            x(i, j) = i*j
        end do
    end do

    print *, 'BEFORE'
    do i = 1, N
        print *, x(i, :)
    end do

    ! a forward 2D wavelet transform
    call daub4_2Dtransform(N, x, y)

    print *, 'AFTER'
    do i = 1, N
        print *, y(i, :)
    end do

    ! an inverse transform to recover the data
    call daub4_2Dtransform_inv(N, y, x)

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

    ! an in-place transform
    call daub4_2Dtransform_inpl(n, x)

    print *, 'AFTER'
    do i = 1, N
        print *, x(i, :)
    end do

    ! an in-place inverse transform
    call daub4_2Dtransform_inv_inpl(n, x)

    print *, 'RECOVERED'
    do i = 1, N
        print *, x(i, :)
    end do

end program Wavelets
