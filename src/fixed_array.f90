module fixed_array
    implicit none

    ! a 16 x 16 block of floating-point values
    integer, parameter :: BASE = 4
    integer, parameter :: DIM = 2**BASE ! 16

    ! significant_bits = works with 1 byte
    ! 1 sign bit + 7 bits for the magnitude
    integer(kind=4), parameter :: significant_bits = 7

    type, bind(C) :: fixed_block
        ! a NaN mask: 16 x 16 bits = 64 bits (2 bytes per column)
        integer(kind=2) :: mask(DIM)
        integer(kind=2) :: common_exp ! can be made <kind=2> because there is a one-byte padding anyway
        integer(kind=1), dimension(DIM, DIM) :: mantissa
    end type fixed_block
contains
    function to_fixed(x, pmin, pmax, ignrval, datamin, datamax) result(compressed)
        use, intrinsic :: ieee_arithmetic
        implicit none

        integer(kind=4) :: n, m ! input dimensions
        real(kind=4), dimension(:, :), intent(in) :: x
        real, intent(in) :: pmin, pmax
        real, intent(in) :: ignrval, datamin, datamax

        integer(kind=4) :: i, j

        ! compressed output dimensions
        integer(kind=4) :: cn, cm

        ! the result
        type(fixed_block), dimension(:, :), pointer :: compressed

        n = size(x, 1)
        m = size(x, 2)

        ! by default compressed is dimension(n/DIM, m/DIM)
        cn = n/DIM
        cm = m/DIM

        ! but the input dimensions might not be divisible by <DIM>
        if (mod(n, DIM) .ne. 0) cn = cn + 1
        if (mod(m, DIM) .ne. 0) cm = cm + 1

        allocate (compressed(cn, cm))

        ! do concurrent(j=1:m/DIM, i=1:n/DIM)
        do concurrent(j=1:cm, i=1:cn)
            block
                real(kind=4), dimension(DIM, DIM) :: input
                integer :: x1, x2, y1, y2

                ! by default there are no valid values
                input = ieee_value(0.0, ieee_quiet_nan)

                x1 = 1 + shiftl(i - 1, BASE)
                x2 = min(n, shiftl(i, BASE))

                y1 = 1 + shiftl(j - 1, BASE)
                y2 = min(m, shiftl(j, BASE))

                input(1:x2 - x1 + 1, 1:y2 - y1 + 1) = x(x1:x2, y1:y2)

                ! pre-condition the input array
                ! or not
                ! input = log(0.5 + (input - pmin)/(pmax - pmin))

                call to_fixed_block(input, compressed(i, j), ignrval, datamin, datamax)
            end block
        end do

    end function to_fixed

    pure subroutine to_fixed_block(x, compressed, ignrval, datamin, datamax)
        implicit none

        real(kind=4), dimension(DIM, DIM), intent(inout) :: x
        real, intent(in) :: ignrval, datamin, datamax

        integer, dimension(DIM, DIM) :: e

        ! the maximum exponent
        integer :: max_exp

        ! the result
        type(fixed_block), intent(out) :: compressed

        ! an internal NaN mask
        logical(kind=1), dimension(DIM, DIM) :: mask
        integer(kind=2) :: work
        integer :: i, j, pos

        !  pick out all the NaN
        where (isnan(x) .or. (x .le. ignrval) .or. (x .lt. datamin) .or. (x .gt. datamax))
            mask = .true.
        elsewhere
            mask = .false.
        end where

        e = exponent(x)
        max_exp = minexponent(0.0)

        ! go through the mask element by element
        ! checking for any NaNs

        do j = 1, DIM
            ! by default there are no NaNs in a column
            work = 0
            pos = 0

            do i = 1, DIM
                if (mask(i, j)) then
                    ! replace NaN with 0.0
                    x(i, j) = 0.0

                    ! set the bit to .true. where there is a NaN
                    work = ibset(work, pos)

                else
                    ! ignore zero values when looking for the maximum exponent
                    if (abs(x(i, j)) .gt. 0.0) then
                        if (e(i, j) .gt. max_exp) then
                            max_exp = e(i, j)
                        end if
                    end if
                end if

                pos = pos + 1
            end do

            compressed%mask(j) = work

        end do

        compressed%common_exp = int(max_exp, kind=2)

        ! 8-bit quantization (7 bits + sign)
        compressed%mantissa = quantize(x, e, max_exp, significant_bits)

    end subroutine to_fixed_block

    pure subroutine from_fixed_block(compressed, x)
        use, intrinsic :: ieee_arithmetic
        implicit none

        type(fixed_block), intent(in) :: compressed
        real(kind=4), dimension(DIM, DIM), intent(out) :: x

        integer :: i, j, pos
        integer(kind=2) :: bitmask

        ! the maximum exponent
        integer :: max_exp

        max_exp = int(compressed%common_exp)

        x = dequantize(compressed%mantissa, max_exp, significant_bits)

        ! add NaNs where needed

        ! go through the mask element by element
        ! checking for any NaNs
        do j = 1, DIM
            bitmask = compressed%mask(j)
            pos = 0

            do i = 1, DIM
                if (btest(bitmask, pos)) then
                    ! insert back a NaN value
                    x(i, j) = ieee_value(0.0, ieee_quiet_nan)
                end if

                pos = pos + 1
            end do
        end do

    end subroutine from_fixed_block

    subroutine print_fixed_block(compressed)
        use, intrinsic :: ieee_arithmetic
        implicit none

        type(fixed_block), intent(in) :: compressed

        print *, 'mask', compressed%mask
        print *, 'max exp.', int(compressed%common_exp) + 1
        print *, 'mantissa', compressed%mantissa

    end subroutine print_fixed_block

    elemental integer function clamp(x, xmin, xmax) result(res)
        integer, intent(in) :: x, xmin, xmax
        res = min(max(x, xmin), xmax)
    end function clamp

    elemental function quantize(x, e, max_exp, bits)
        real, intent(in) :: x
        integer, intent(in) :: e, max_exp, bits
        integer(kind=1) :: quantize
        integer i, tmp

        ! what it does
        ! quantize = nint(x*(2**bits)/(2**max_exp), kind=1)

        i = e - max_exp + bits
        tmp = nint(set_exponent(x, i))

        ! cap the range to 8 bits [-128, 127] in order to prevent 8-bit under- / over-flows
        ! squeeze the value into 8 bits
        quantize = int(clamp(tmp, -128, 127), kind=1)
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
