program main
    use, intrinsic :: iso_c_binding
    use wavelet

    integer(kind=4), parameter :: fraction_bits = 30
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
    integer, dimension(16) :: iblock
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

    iblock = reshape(qint, [16])

    print *, 'i:', i
    print *, 'iblock:', iblock

    ! decorrelate
    call fwd_xform(iblock)
    print *, 'iblock:', iblock

    ! negate and re-order

    ! reordering
    ! ordered = reshape(qint, [16])
    ! print *, 'ordered:', ordered(perm_f + 1)

    ! inverse works OK
    ! qint = matmul(inv_coeffs, qint)/4
    ! print *, 'reverse decorrelation:', qint
contains
    subroutine fwd_lift(p, offset, s)
        implicit none

        integer, dimension(16), intent(inout) :: p
        integer, intent(in) :: offset, s
        integer :: idx, x, y, z, w

        print *, 'offset:', offset, 'scale:', s

        idx = 1 + offset

        x = p(idx)
        idx = idx + s

        y = p(idx)
        idx = idx + s

        z = p(idx)
        idx = idx + s

        w = p(idx)

        print *, '--> x:', x, 'y:', y, 'z:', z, 'w:', w

        ! non-orthogonal transform
        !        ( 4  4  4  4) (x)
        ! 1/16 * ( 5  1 -1 -5) (y)
        !        (-4  4  4 -4) (z)
        !        (-2  6 -6  2) (w)
        x = x + w
        x = shiftr(x, 1)
        w = w - x
        z = z + y
        z = shiftr(z, 1)
        y = y - z
        x = x + z
        x = shiftr(x, 1)
        z = z - x
        w = w + y
        w = shiftr(w, 1)
        y = y - w
        w = w + shiftr(y, 1)
        y = y - shiftr(w, 1)

        print *, '<-- x:', x, 'y:', y, 'z:', z, 'w:', w

        p(idx) = w
        idx = idx - s

        p(idx) = z
        idx = idx - s

        p(idx) = y
        idx = idx - s

        p(idx) = x

    end subroutine fwd_lift

    subroutine fwd_xform(p)
        implicit none

        integer, dimension(16), intent(inout) :: p
        integer :: x, y

        ! transform along x
        do y = 0, 3
            call fwd_lift(p, 4*y, 1)
        end do

        ! transform along y
        do x = 0, 3
            call fwd_lift(p, 1*x, 4)
        end do
    end subroutine fwd_xform
end program main
