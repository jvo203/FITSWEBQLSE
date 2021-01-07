program Wavelets
    implicit none

    integer(kind=4), parameter :: N = 4
    integer i, j

    real(kind=4), dimension(N, N) :: x
    real(kind=4), dimension(N, N) :: y

    do i = 1, N
        do j = 1, N
            x(i, j) = i*j
        end do
    end do

    print *, 'BEFORE'
    print *, x

    ! a forward 2D wavelet transform
    call daub4_transform2D(N, x, y)

    print *, 'AFTER'
    print *, y

end program Wavelets
