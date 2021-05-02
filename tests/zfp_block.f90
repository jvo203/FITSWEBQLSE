program main
    use, intrinsic :: iso_c_binding

    real(kind=4), dimension(4, 4) :: x

    ! loop counters
    integer i, j

    do j = 1, 4
        do i = 1, 4
            x(i, j) = i*i
        end do
    end do

    print *, x
end program main
