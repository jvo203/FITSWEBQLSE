module fixed_array
    implicit none

    ! the minimum X-Y dimension below which the wavelet routines should terminate
    ! the operation (i.e. 8 -> 4x4 coarse coefficients, ideal for ZFP)
    ! since ZFP operates on 4x4 blocks
    ! significant_bits = works with 1 byte
    ! 1 sign bit + 7 bits for the magnitude
    integer(kind=4), parameter :: significant_bits = 7 ! was 7
    ! integer(kind=4), parameter :: significant_bits = 5

    type fixed_block
        ! a NaN mask: 4 x 4 bits = 16 bits (2 bytes)
        integer(kind=2) :: mask
        integer(kind=1) :: common_exp
        integer(kind=1), dimension(4, 4) :: mantissa ! was 1 byte
    end type fixed_block
contains
    !elemental logical function isnan(x)
    !    real(kind=4), intent(in) :: x

    !    if (abs(x)*0.0 /= 0.0) then
    !        isnan = .true.
    !    else
    !        isnan = .false.
    !    end if

    !end function isnan

    subroutine to_fixed(x, compressed, ignrval, datamin, datamax, pmin, pmax) !, mask)
        ! use wavelet
        use, intrinsic :: ieee_arithmetic
        implicit none

        integer(kind=4) :: n, m ! input dimensions
        real, intent(in) :: pmin, pmax
        real(kind=4), dimension(:, :), intent(in) :: x
        real, intent(in) :: ignrval, datamin, datamax

        ! logical(kind=1), dimension(n, n), optional, intent(inout) :: mask
        integer(kind=4) :: i, j

        ! compressed output dimensions
        integer(kind=4) :: cn, cm

        ! the result
        type(fixed_block), dimension(:, :), intent(out) :: compressed

        n = size(x, 1)
        m = size(x, 2)

        ! by default compressed is dimension(n/4, m/4)
        cn = n/4
        cm = m/4

        ! but the input dimensions might not be divisible by 4
        if (mod(n, 4) .ne. 0) cn = cn + 1
        if (mod(m, 4) .ne. 0) cm = cm + 1

        if (size(compressed, 1) .lt. cn) then
            print *, 'compressed array dimension(1) mismatch:', size(compressed, 1), '.ne.', cn
            return
        end if

        if (size(compressed, 2) .lt. cm) then
            print *, 'compressed array dimension(2) mismatch:', size(compressed, 2), '.ne.', cm
            return
        end if

        ! first pre-condition the input array
        ! x = log(0.5 + (x - pmin)/(pmax - pmin))

        do concurrent(j=1:m/4, i=1:n/4)
            block
                real(kind=4), dimension(4, 4) :: input
                integer :: x1, x2, y1, y2

                !if (present(mask)) then
                !    call to_daub4_block(x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)),&
                !    &mask(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)), .true.)
                !end if

                ! by default there are no valid values
                input = ieee_value(0.0, ieee_quiet_nan)

                x1 = 1 + shiftl(i - 1, 2)
                x2 = min(n, shiftl(i, 2))

                y1 = 1 + shiftl(j - 1, 2)
                y2 = min(m, shiftl(j, 2))

                input(1:x2 - x1 + 1, 1:y2 - y1 + 1) = x(x1:x2, y1:y2)

                ! pre-condition the input array
                ! input = log(0.5 + (input - pmin)/(pmax - pmin))

                call to_fixed_block(input, compressed(i, j), ignrval, datamin, datamax)
            end block
        end do

    end subroutine to_fixed

    pure subroutine to_fixed_block(x, compressed, ignrval, datamin, datamax)
        ! use wavelet
        implicit none

        real(kind=4), dimension(4, 4), intent(inout) :: x
        real, intent(in) :: ignrval, datamin, datamax

        real(kind=4), dimension(:), allocatable :: tmp
        integer, dimension(:), allocatable :: tmp_e

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
        where (isnan(x) .or. (x .le. ignrval) .or. (x .lt. datamin) .or. (x .gt. datamax))
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

        ! a wavelet transform
        ! call to_daub4_block(x)

        ! pick all non-zero values
        where (x .ne. 0.0)
            mask = .true.
        elsewhere
            mask = .false.
        end where

        ! pack the vector
        ! tmp = pack(x, mask)
        ! tmp_e = exponent(tmp)
        ! max_exp = maxval(tmp_e)

        e = exponent(x)
        max_exp = maxval(e)

        compressed%common_exp = int(max_exp - 1, kind=1)

        ! 8-bit quantization (7 bits + sign)
        compressed%mantissa = quantize(x, e, max_exp, significant_bits)

    end subroutine to_fixed_block

    subroutine from_fixed(n, compressed, x, pmin, pmax) !, mask)
        !use wavelet
        implicit none

        integer(kind=4) :: n
        type(fixed_block), dimension(n/4, n/4), intent(in) :: compressed
        real, intent(in) :: pmin, pmax
        ! logical(kind=1), dimension(n, n), optional, intent(in) :: mask

        ! the result
        real(kind=4), dimension(n, n), intent(out) :: x
        integer(kind=4) :: i, j

        if (mod(n, 4) .ne. 0) return

        do concurrent(j=1:n/4, i=1:n/4)
            call from_fixed_block(compressed(i, j),&
            & x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)),&
            &pmin, pmax)

            !if (present(mask)) then
            !    call from_daub4_block(x(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)),&
            !    &mask(1 + shiftl(i - 1, 2):shiftl(i, 2), 1 + shiftl(j - 1, 2):shiftl(j, 2)))
            !end if
        end do

    end subroutine from_fixed

    pure subroutine from_fixed_block(compressed, x, pmin, pmax)
        use, intrinsic :: ieee_arithmetic
        implicit none

        type(fixed_block), intent(in) :: compressed
        real(kind=4), dimension(4, 4), intent(out) :: x
        real, intent(in) :: pmin, pmax

        integer :: i, j, pos
        integer(kind=2) :: bitmask

        ! the maximum exponent
        integer :: max_exp

        max_exp = int(compressed%common_exp) + 1

        x = dequantize(compressed%mantissa, max_exp, significant_bits)

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

        ! recover the original range
        ! x = pmin + (exp(x) - 0.5)*(pmax - pmin)

    end subroutine from_fixed_block

    subroutine print_fixed_block(compressed)
        use, intrinsic :: ieee_arithmetic
        implicit none

        type(fixed_block), intent(in) :: compressed

        print *, 'mask', compressed%mask
        print *, 'max exp.', int(compressed%common_exp) + 1
        print *, 'mantissa', compressed%mantissa

    end subroutine print_fixed_block

    elemental function quantize(x, e, max_exp, bits)
        real, intent(in) :: x
        integer, intent(in) :: e, max_exp, bits
        integer(kind=1) :: quantize
        integer i

        ! what it does
        ! quantize = nint(x*(2**bits)/(2**max_exp), kind=1)

        i = e - max_exp + bits
        quantize = nint(set_exponent(x, i), kind=1)

    end function quantize

    elemental function dequantize(x, max_exp, bits)
        integer(kind=1), intent(in) :: x
        integer, intent(in) :: max_exp
        integer, intent(in) :: bits
        real :: dequantize
        integer i

        ! what it does
        ! dequantize = real(x)*(2**max_exp)/(2**bits)

        i = max_exp - bits
        dequantize = scale(real(x), i)

    end function dequantize
end module fixed_array
