program Wavelets
    implicit none

    integer(kind=4), parameter :: N = 8
    integer i, j

    real(kind=4), dimension(N, N) :: x
    real(kind=4), dimension(N, N) :: y

    do i = 1, N
        do j = 1, N
            x(i, j) = i + j
        end do
    end do

    print *, 'BEFORE', x

end program Wavelets
