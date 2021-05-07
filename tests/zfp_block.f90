program main
    use, intrinsic :: iso_c_binding

    integer(kind=4), parameter :: fraction_bits = 23
    integer(kind=4), dimension(4, 4) :: fwd_coeffs = reshape([4, 5, -4, -2, 4, 1, 4, 6, 4, -1, 4, -6, 4, -5, -4, 2], [4, 4])

    real(kind=4), dimension(4, 4) :: x
    integer, dimension(4, 4) :: e
    integer, dimension(4, 4) :: qint, i
    integer :: max_exp

    ! loop counters
    integer ix, iy

    do iy = 1, 4
        do ix = 1, 4
            x(ix, iy) = ix*iy
        end do
    end do

    print *, x

    e = exponent(x)
    max_exp = maxval(e)

    print *, 'e:', e
    print *, 'max_exp:', max_exp

    i = e - max_exp + fraction_bits
    qint = nint(set_exponent(x, i))

    print *, 'i:', i
    print *, 'qint:', qint

    qint = matmul(fwd_coeffs, qint)/16
    print *, 'decorrelation:', qint

end program main
