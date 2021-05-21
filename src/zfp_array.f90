module zfp_array
    integer(kind=4), parameter :: max_bits = 128
    integer(kind=4), parameter :: fraction_bits = 30
    integer(kind=4), parameter :: EBIAS = 127

    integer(kind=4), dimension(16), parameter :: PERM =&
    & [0, 1, 4, 5, 2, 8, 6, 9, 3, 12, 10, 7, 13, 11, 14, 15]

    integer(kind=4), parameter :: NBMASK = Z'aaaaaaaa'

    ! Golomb encoding
    integer, parameter :: G_M = 10
    integer, parameter :: G_b = 4

    ! Rice encoding
    integer, parameter :: R_k = 3
    integer, parameter :: R_M = 2**R_k

    type zfp_block
        ! a 128-bit bitstream holding a compressed 4x4 real array
        integer(kind=16) :: bitstream
    end type zfp_block

contains
    subroutine zfp_compress_array(x, compressed)
        implicit none

        integer(kind=4) :: n, m ! input dimensions
        real(kind=4), dimension(:, :), intent(inout) :: x
        integer(kind=4) :: i, j

        ! compressed output dimensions
        integer(kind=4) :: cn, cm

        ! the result
        type(zfp_block), dimension(:, :), intent(out) :: compressed

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

        do concurrent(j=1:m/4, i=1:n/4)
            ! do j = 1, m/4
            ! do i = 1, n/4
            ! IMPORTANT: checking the bounds
            call zfp_compress_block(x(1 + shiftl(i - 1, 2):min(n, shiftl(i, 2)),&
            & 1 + shiftl(j - 1, 2):min(m, shiftl(j, 2))), compressed(i, j))
            ! end do
        end do

    end subroutine zfp_compress_array

    pure subroutine zfp_compress_block(x, compressed)
        implicit none

        ! the input 4x4 block to be compressed with ZFP
        real(kind=4), dimension(4, 4), intent(inout) :: x

        integer, dimension(4, 4) :: e
        integer, dimension(4, 4) :: qint
        integer, dimension(16) :: iblock
        integer :: pos, bits

        ! the maximum exponent
        integer :: max_exp

        ! the result
        type(zfp_block), intent(out) :: compressed

        ! an internal NaN mask
        logical(kind=1), dimension(4, 4) :: mask
        integer(kind=2) :: bitmask
        integer :: i, j, tmp

        integer :: nvalid
        real(kind=4) :: average

        ! by default there are no NaNs
        bitmask = 0

        !  pick out all the NaN
        where (isnan(x))
            mask = .false.
        elsewhere
            mask = .true.
        end where

        ! go through the mask element by element checking for any NaNs
        pos = 0
        do j = 1, 4
            do i = 1, 4
                if (.not. mask(i, j)) then
                    ! replace NaN with 0.0
                    ! x(i, j) = 0.0

                    ! set the bit to .true. where there is a NaN
                    bitmask = ibset(bitmask, pos)
                end if

                pos = pos + 1
            end do
        end do

        if (any(mask)) then
            ! calculate the mean of the input array
            nvalid = count(mask)

            ! all values might be NaN in which case set the array to 0.0
            if (nvalid .gt. 0) then
                average = sum(x, mask=mask)/nvalid
            else
                average = 0.0
            end if

            ! replace NaNs with the average value
            where (.not. mask) x = average
        end if

        compressed%bitstream = 0
        pos = 0

        if (bitmask .eq. 0) then
            ! all values are non-NaN, emit '0'
            ! no need to do anything since initially bitstream = 0
            pos = pos + 1
        else
            ! emit '1'
            call stream_write_bit(compressed%bitstream, 1, pos)

            ! run-length-encode the 16-bit NaN mask
            ! call encode_mask(bitmask, compressed%bitstream, pos)

            tmp = int(bitmask, kind=4)
            call stream_write_bits(compressed%bitstream, tmp, 16, pos)
        end if

        e = exponent(x)
        max_exp = maxval(e)
        bits = max_exp + EBIAS

        ! write the exponent
        call stream_write_bits(compressed%bitstream, bits, 8, pos)

        ! quantize floating-point numbers
        qint = nint(set_exponent(x, e - max_exp + fraction_bits))
        iblock = reshape(qint, (/16/))

        ! decorrelate
        call fwd_xform(iblock)

        ! reorder signed coefficients and convert to unsigned integer
        call fwd_order(iblock)

        ! encode 32-bit integers
        call encode_ints(iblock, compressed%bitstream, pos)

        if (pos .lt. max_bits) then
            call pad_stream(compressed%bitstream, max_bits - pos, pos)
        end if

        return

    end subroutine zfp_compress_block

    subroutine zfp_decompress_block(compressed, x, bitmask)
        implicit none

        type(zfp_block), intent(in) :: compressed
        real(kind=4), dimension(4, 4), intent(out) :: x
        integer(kind=2), intent(out) :: bitmask

        integer, dimension(4, 4) :: e
        integer, dimension(4, 4) :: qint
        integer, dimension(16) :: iblock
        integer :: pos, bits

        ! the maximum exponent
        integer :: max_exp

        pos = max_bits - 1

        if (stream_read_bit(compressed%bitstream, pos) .eq. 1) then
            ! decode the NaN mask
            ! bitmask = decode_mask(compressed%bitstream, pos)

            bitmask = int(stream_read_bits(compressed%bitstream, pos, 16), kind=2)
        end if

        ! reset all the bits
        bits = 0

        bits = stream_read_bits(compressed%bitstream, pos, 8)
        max_exp = bits - EBIAS

        ! decode 32-bit integers
        call decode_ints(iblock, compressed%bitstream, pos)

        ! reorder unsigned coefficients and convert to signed integer
        call inv_order(iblock)

        ! perform decorrelating transform
        call inv_xform(iblock)

        ! de-quantize
        qint = reshape(iblock, (/4, 4/))
        x = scale(real(qint), max_exp - fraction_bits)

    end subroutine zfp_decompress_block

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

    ! inverse lifting transform of 4 - vector
    pure subroutine inv_lift(p, offset, s)
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

        ! non - orthogonal transform
        ! (4 6 - 4 - 1) (x)
        ! 1/4*(4 2 4 5) (y)
        ! (4 - 2 4 - 5) (z)
        ! (4 - 6 - 4 1) (w)

        ! shiftr does not preserve the sign bit
        ! therefore shifta is used in the code below

        y = y + shifta(w, 1)
        w = w - shifta(y, 1)
        y = y + w
        w = shiftl(w, 1)
        w = w - y
        z = z + x
        x = shiftl(x, 1)
        x = x - z
        y = y + z
        z = shiftl(z, 1)
        z = z - y
        w = w + x
        x = shiftl(x, 1)
        x = x - w

        p(idx) = w
        idx = idx - s

        p(idx) = z
        idx = idx - s

        p(idx) = y
        idx = idx - s

        p(idx) = x

    end subroutine inv_lift

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

    ! inverse decorrelating 2D transform
    pure subroutine inv_xform(p)
        implicit none

        integer, dimension(16), intent(inout) :: p
        integer :: x, y

        ! transform along y
        do x = 0, 3
            call inv_lift(p, 1*x, 4)
        end do

        ! transform along x
        do y = 0, 3
            call inv_lift(p, 4*y, 1)
        end do

    end subroutine inv_xform

    ! reorder signed coefficients and convert to unsigned integer
    pure subroutine fwd_order(p)
        implicit none

        integer, dimension(16), intent(inout) :: p

        ! re-order and convert to unsigned integer
        p = int2uint(p(1 + PERM))

    end subroutine fwd_order

    ! reorder unsigned coefficients and convert to signed integer
    pure subroutine inv_order(p)
        implicit none

        integer, dimension(16), intent(inout) :: p

        ! convert to signed integer and re-order
        p(1 + PERM) = uint2int(p)

    end subroutine inv_order

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

    pure subroutine encode_mask(mask, stream, pos)
        implicit none

        integer(kind=2), intent(in) :: mask
        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos

        integer :: i, k, bit

        ! a counter for runs of '0'
        integer :: zcount

        zcount = 0

        ! iterate over 16 bits in the mask
        do i = 0, 15

            bit = int(ibits(mask, i, 1))

            if (bit .eq. 1) then
                ! the runs of zeroes has finished

                ! Golomb or Rice-encode the zcount
                call Golomb_encode(stream, pos, zcount)

                ! reset the zeroes counter
                zcount = 0
            else
                zcount = zcount + 1
            end if

        end do

        ! flush the encoder
        call Golomb_encode(stream, pos, zcount)

    end subroutine encode_mask

    function decode_mask(stream, pos) result(bitmask)
        implicit none

        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        integer(kind=2) :: bitmask

        integer :: i

        ! a counter for runs of '0'
        integer :: zcount

        bitmask = 0
        zcount = -1

        do i = 0, 15

            ! decode the next zero-run length
            if (zcount .lt. 0) then

                zcount = Golomb_decode(stream, pos)

                if (zcount .lt. 0) return

            end if

            zcount = zcount - 1

            if (zcount .lt. 0) then
                ! set the appropriate bit to '1'
                bitmask = ibset(bitmask, i)
            end if

        end do

        ! zcount = Golomb_decode(stream, pos)

        return

    end function decode_mask

    pure subroutine encode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(in) :: data
        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos

        integer :: i, k, bit

        ! a counter for runs of '0'
        integer :: zcount

        zcount = 0

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1

            ! gather k-plane bits from the input data
            do i = 1, 16

                bit = int(ibits(data(i), k, 1))

                if (bit .eq. 1) then
                    ! the runs of zeroes has finished

                    ! Golomb or Rice-encode the zcount
                    call Golomb_encode(stream, pos, zcount)

                    ! reset the zeroes counter
                    zcount = 0
                else
                    zcount = zcount + 1
                end if

            end do

        end do

        ! flush the encoder
        call Golomb_encode(stream, pos, zcount)

    end subroutine encode_ints

    subroutine decode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(out) :: data
        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: i, k

        ! a counter for runs of '0'
        integer :: zcount

        data = 0
        zcount = -1

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1

            ! set k-plane bits
            do i = 1, 16

                ! decode the next zero-run length
                if (zcount .lt. 0) then

                    zcount = Golomb_decode(stream, pos)

                    if (zcount .lt. 0) return

                end if

                zcount = zcount - 1

                if (zcount .lt. 0) then
                    ! set the appropriate bit to '1'
                    data(i) = ibset(data(i), k)
                end if

            end do
        end do

    end subroutine decode_ints

    pure subroutine Golomb_encode(stream, pos, N)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: N

        integer :: i, t, nbits

        ! quotient, remainder
        integer :: q, r

        q = N/G_M
        r = modulo(N, G_M)

        ! Quotient Code
        if (q .gt. 0) then
            do i = 1, q
                ! check if there is space to write
                if (pos .eq. max_bits) return
                call stream_write_bit(stream, 1, pos)
            end do
        end if

        if (pos .eq. max_bits) return

        call stream_write_bit(stream, 0, pos)

        ! Remainder Code
        t = 2**G_b - G_M

        if (r .lt. t) then

            nbits = G_b - 1

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits, pos)

        else

            nbits = G_b
            r = r + t

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits, pos)

        end if

    end subroutine Golomb_encode

    integer function Golomb_decode(stream, pos)
        implicit none

        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: s, x, bit, t

        Golomb_decode = -1

        s = 0
        ! count the number of consecutive '1', stop at '0'

        do
            bit = stream_read_bit(stream, pos)

            if (bit .lt. 0) return

            if (bit .eq. 0) exit

            s = s + 1
        end do

        x = 0
        x = stream_read_bits(stream, pos, G_b - 1)

        if (x .lt. 0) return

        t = 2**G_b - G_M

        if (x .lt. t) then
            Golomb_decode = s*G_M + x
            return
        end if

        bit = 0
        bit = stream_read_bit(stream, pos)

        if (bit .lt. 0) return

        x = shiftl(x, 1) + bit

        Golomb_decode = s*G_M + x - t

        return

    end function Golomb_decode

    pure subroutine Rice_encode(stream, pos, N)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: N

        integer :: i

        ! quotient, pseudo-remainder
        integer :: q, r

        q = shiftr(N, R_k) ! divide by R_M = 2**R_k
        r = N

        ! Quotient Code
        if (q .gt. 0) then
            do i = 1, q
                ! check if there is space to write
                if (pos .eq. max_bits) return

                call stream_write_bit(stream, 1, pos)
            end do
        end if

        if (pos .eq. max_bits) return

        call stream_write_bit(stream, 0, pos)

        ! Remainder Code
        ! check if there is space to write
        if (pos + R_k .gt. max_bits) return

        call stream_write_bits(stream, r, R_k, pos)

    end subroutine Rice_encode

    integer function Rice_decode(stream, pos)
        implicit none

        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: s, x, bit

        Rice_decode = -1

        s = 0
        ! count the number of consecutive '1', stop at '0'

        do
            bit = stream_read_bit(stream, pos)

            if (bit .lt. 0) return

            if (bit .eq. 0) exit

            s = s + 1
        end do

        x = 0
        x = stream_read_bits(stream, pos, R_k)

        if (x .lt. 0) return

        Rice_decode = shiftl(s, R_k) + x

        return

    end function Rice_decode

    pure subroutine stream_write_bit(stream, bit, pos)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(in) :: bit
        integer, intent(inout) :: pos

        ! make place for the new bit
        stream = shiftl(stream, 1)

        ! set the LSB bit
        if (bit .eq. 1) then
            stream = ibset(stream, 0)
        end if

        pos = pos + 1

        return

    end subroutine stream_write_bit

    pure subroutine stream_write_bits(stream, bits, n, pos)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: bits
        integer, intent(in) :: n
        integer, intent(inout) :: pos

        integer :: i

        if (n .lt. 1) return

        stream = shiftl(stream, n)

        call mvbits(int(bits, kind=16), 0, n, stream, 0)

        pos = pos + n

        return

    end subroutine stream_write_bits

    pure subroutine pad_stream(stream, n, pos)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(in) :: n
        integer, intent(inout) :: pos

        integer :: i

        if (n .lt. 1) return

        stream = shiftl(stream, n)
        pos = pos + n

        return
    end subroutine pad_stream

    integer function stream_read_bit(stream, pos)
        implicit none

        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        if (pos .lt. 0) then
            stream_read_bit = -1
            return
        end if

        if (btest(stream, pos)) then
            stream_read_bit = 1
        else
            stream_read_bit = 0
        end if

        pos = pos - 1

        return

    end function stream_read_bit

    integer function stream_read_bits(stream, pos, n)
        implicit none

        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: n

        integer :: i

        stream_read_bits = -1

        if (n .lt. 1) return

        if (pos - n + 1 .lt. 0) return

        stream_read_bits = int(ibits(stream, pos - n + 1, n))

        pos = pos - n

        return

    end function stream_read_bits
end module zfp_array
