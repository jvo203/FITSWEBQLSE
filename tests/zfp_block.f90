program main
    use, intrinsic :: iso_c_binding

    real(kind=4), dimension(4, 4) :: x
    integer, dimension(4, 4) :: e
    integer :: max_exp

    ! loop counters
    integer i, j

    do j = 1, 4
        do i = 1, 4
            x(i, j) = i*i
        end do
    end do

    print *, x

    e = exponent(x)
    max_exp = maxval(e)

    print *, 'e:', e
    print *, 'max_exp:', max_exp

end program main
