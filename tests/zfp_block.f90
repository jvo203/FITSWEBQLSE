program main
    use, intrinsic :: iso_c_binding
    use wavelet

    integer(kind=4), parameter :: fraction_bits = 30
    integer(kind=4), dimension(4, 4), parameter :: fwd_coeffs =&
    & reshape([4, 5, -4, -2, 4, 1, 4, 6, 4, -1, 4, -6, 4, -5, -4, 2], [4, 4])
    integer(kind=4), dimension(4, 4), parameter :: inv_coeffs =&
    & reshape([4, 4, 4, 4, 6, 2, -2, -6, -4, 4, 4, -4, -1, 5, -5, 1], [4, 4])
    integer(kind=4), dimension(16), parameter :: PERM =&
    & [0, 1, 4, 5, 2, 8, 6, 9, 3, 12, 10, 7, 13, 11, 14, 15]
    integer(kind=4), parameter :: NBMASK = Z'aaaaaaaa'

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

    ! reorder signed coefficients and convert to unsigned integer
    call fwd_order(iblock)
    print *, 'iblock:', iblock

    ! inverse works OK
    ! qint = matmul(inv_coeffs, qint)/4
    ! print *, 'reverse decorrelation:', qint
contains
    pure subroutine fwd_lift(p, offset, s)
        implicit none

        integer, dimension(16), intent(inout) :: p
        integer, intent(in) :: offset, s
        integer :: idx, x, y, z, w

        idx = 1 + offset

        x = p(idx)
        idx = idx + s

        y = p(idx)
        idx = idx + s

        z = p(idx)
        idx = idx + s

        w = p(idx)

        ! non-orthogonal transform
        !        ( 4  4  4  4) (x)
        ! 1/16 * ( 5  1 -1 -5) (y)
        !        (-4  4  4 -4) (z)
        !        (-2  6 -6  2) (w)

        ! shiftr does not preserve the sign bit
        ! therefore shifta is used in the code below

        x = x + w
        x = shifta(x, 1)
        w = w - x
        z = z + y
        z = shifta(z, 1)
        y = y - z
        x = x + z
        x = shifta(x, 1)
        z = z - x
        w = w + y
        w = shifta(w, 1)
        y = y - w
        w = w + shifta(y, 1)
        y = y - shifta(w, 1)

        p(idx) = w
        idx = idx - s

        p(idx) = z
        idx = idx - s

        p(idx) = y
        idx = idx - s

        p(idx) = x

    end subroutine fwd_lift

    pure subroutine fwd_xform(p)
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

    pure subroutine fwd_order(p)
        implicit none

        integer, dimension(16), intent(inout) :: p

        ! re-order and convert to unsigned integer
        p = int2uint(p(1 + PERM))

    end subroutine fwd_order

    ! map two's complement signed integer to negabinary unsigned integer
    elemental integer function int2uint(x)
        implicit none

        integer, intent(in) :: x

        ! ((uint)x + NBMASK) ^ NBMASK
        int2uint = IEOR(x + NBMASK, NBMASK)

    end function int2uint

    ! map two's complement signed integer to negabinary unsigned integer
    elemental integer function uint2int(x)
        implicit none

        integer, intent(in) :: x

        ! (int)((x ^ NBMASK) - NBMASK)
        uint2int = IEOR(x, NBMASK) - NBMASK

    end function uint2int
end program main
