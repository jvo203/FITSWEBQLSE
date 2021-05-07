program main
    use, intrinsic :: iso_c_binding
    use wavelet

    integer(kind=4), parameter :: fraction_bits = 23
    integer(kind=4), dimension(4, 4), parameter :: fwd_coeffs =&
    & reshape([4, 5, -4, -2, 4, 1, 4, 6, 4, -1, 4, -6, 4, -5, -4, 2], [4, 4])
    integer(kind=4), dimension(4, 4), parameter :: inv_coeffs =&
    & reshape([4, 4, 4, 4, 6, 2, -2, -6, -4, 4, 4, -4, -1, 5, -5, 1], [4, 4])
    integer(kind=4), dimension(16), parameter :: perm_2 =&
    & [0, 1, 4, 5, 2, 8, 6, 9, 3, 12, 10, 7, 13, 11, 14, 15]
    integer(kind=4), dimension(16), parameter :: perm_f =&
    & [0, 4, 1, 5, 8, 2, 9, 6, 12, 3, 10, 13, 7, 14, 11, 1]

    real(kind=4), dimension(4, 4) :: x
    integer, dimension(4, 4) :: e
    integer, dimension(4, 4) :: qint, i
    integer, dimension(16) :: ordered
    integer :: max_exp

    ! loop counters
    integer ix, iy

    do iy = 1, 4
        do ix = 1, 4
            x(ix, iy) = ix*iy
        end do
    end do

    print *, x
    ! call to_daub4_block(x)
    ! print *, 'fwt:', x
    ! call from_daub4_block(x)
    ! print *, 'iwt:', x

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

    ! reordering
    ordered = reshape(qint, [16])
    print *, 'ordered:', ordered(perm_f + 1)

    ! inverse works OK
    qint = matmul(inv_coeffs, qint)/4
    print *, 'reverse decorrelation:', qint

end program main
