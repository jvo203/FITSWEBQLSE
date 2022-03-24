program main
    use, intrinsic :: iso_c_binding
    implicit none

    ! a 16 x 16 block of floating-point values
    integer, parameter :: BASE = 4
    integer, parameter :: DIM = 2**BASE ! 16

    integer :: i, j

    ! a test array
    real(kind=4), dimension(DIM, DIM) :: x

    print *, "DIM:", DIM

    do j = 1, DIM
        do i = 1, DIM
            x(i, j) = 0.1*i*j
        end do
    end do

    print *, "x:", x, "sum:", sum(x)

end program main
