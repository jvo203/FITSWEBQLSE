module fixed_array
    implicit none

    ! the minimum X-Y dimension below which the wavelet routines should terminate
    ! the operation (i.e. 8 -> 4x4 coarse coefficients, ideal for ZFP)
    ! since ZFP operates on 4x4 blocks
    ! significant_bits = works with 1 byte
    ! 1 sign bit + 7 bits for the magnitude
    integer(kind=4), parameter :: significant_bits = 7
    ! integer(kind=4), parameter :: significant_bits = 5

    type fixed_block
        ! a NaN mask: 4 x 4 bits = 16 bits (2 bytes)
        integer(kind=2) :: mask
        integer(kind=1) :: common_exp
        integer(kind=1), dimension(4, 4) :: mantissa
    end type fixed_block
contains
    !elemental logical function isnan(x)
    !    real(kind=4), intent(in) :: x

    ! if (abs(x)*0.0 /= 0.0) then
    !         isnan = .true.
    !    else
    !        isnan = .false.
    !    end if

    ! end function isnan

    subroutine to_fixed(n, x, compressed, mask)
        ! use wavelet
        implicit none

        integer(kind=4) :: n
        real(kind=4), dimension(n, n), intent(inout) :: x
        logical(kind=1), dimension(n, n), optional, intent(inout) :: mask
        integer(kind=4) :: i, j

        ! the result
        type(fixed_block), dimension(n/4, n/4), intent(out) :: compressed

        if (mod(n, 4) .ne. 0) return

        do concurrent(j=1:n/4, i=1:n/4)
            !if (present(mask)) then
            !    call to_daub4_block(x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)),&
            !    &mask(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)), .true.)
            !end if

            call to_fixed_block(x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)), compressed(i, j))
        end do

    end subroutine to_fixed

    pure subroutine to_fixed_block(x, compressed)
        implicit none

        real(kind=4), dimension(4, 4), intent(inout) :: x
        integer, dimension(4, 4) :: e

        ! the maximum exponent
        integer :: max_exp

        ! the result
        type(fixed_block), intent(out) :: compressed

        ! an internal NaN mask
        logical(kind=1), dimension(4, 4) :: mask
        integer(kind=2) :: work
        integer :: i, j, pos

        ! by default there are no NaNs
        work = 0

        !  pick out all the NaN
        where (isnan(x))
            mask = .true.
        elsewhere
            mask = .false.
        end where

        ! go through the mask element by element
        ! checking for any NaNs
        pos = 0
        do j = 1, 4
            do i = 1, 4
                if (mask(i, j)) then
                    ! replace NaN with 0.0
                    x(i, j) = 0.0

                    ! set the bit to .true. where there is a NaN
                    work = ibset(work, pos)
                end if

                pos = pos + 1
            end do
        end do

        compressed%mask = work

        e = exponent(x)
        max_exp = maxval(e)

        compressed%common_exp = int(max_exp - 1, kind=1)
! 8-bit quantization (7 bits + sign)
        compressed%mantissa = quantize(x, e, max_exp, significant_bits)

    end subroutine to_fixed_block

    subroutine from_fixed(n, compressed, x, mask)
        !use wavelet
        implicit none

        integer(kind=4) :: n
        type(fixed_block), dimension(n/4, n/4), intent(in) :: compressed
        logical(kind=1), dimension(n, n), optional, intent(in) :: mask

! the result
        real(kind=4), dimension(n, n), intent(out) :: x
        integer(kind=4) :: i, j

        if (mod(n, 4) .ne. 0) return

        do concurrent(j=1:n/4, i=1:n/4)
            call from_fixed_block(compressed(i, j), x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)))

            !if (present(mask)) then
            !    call from_daub4_block(x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)),&
            !    &mask(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)))
            !end if
        end do

    end subroutine from_fixed

    pure subroutine from_fixed_block(compressed, x)
        use, intrinsic :: ieee_arithmetic
        implicit none

        type(fixed_block), intent(in) :: compressed
        real(kind=4), dimension(4, 4), intent(out) :: x

        integer :: i, j, pos
        integer(kind=2) :: bitmask

        x = dequantize(compressed%mantissa, int(compressed%common_exp), significant_bits)

        ! add NaNs where needed
        bitmask = compressed%mask

        ! go through the mask element by element
        ! checking for any NaNs
        pos = 0
        do j = 1, 4
            do i = 1, 4
                if (btest(bitmask, pos)) then
                    ! insert back a NaN value
                    x(i, j) = ieee_value(0.0, ieee_quiet_nan)
                end if

                pos = pos + 1
            end do
        end do
    end subroutine from_fixed_block

    elemental function quantize(x, e, max_exp, bits)
        real, intent(in) :: x
        integer, intent(in) :: e, max_exp, bits
        integer(kind=1) :: quantize
        integer i

        i = e - max_exp + bits
        quantize = nint(set_exponent(x, i), kind=1)
    end function quantize

    elemental function dequantize(x, max_exp, bits)
        integer(kind=1), intent(in) :: x
        integer, intent(in) :: max_exp
        integer, intent(in) :: bits
        real :: dequantize
        integer i

        i = (max_exp + 1) - bits
        dequantize = scale(real(x), i)
    end function dequantize
end module fixed_array
