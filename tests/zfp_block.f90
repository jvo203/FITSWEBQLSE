program main
    use, intrinsic :: iso_c_binding

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

    real(kind=4), dimension(4, 4) :: x
    integer, dimension(4, 4) :: e
    integer, dimension(4, 4) :: qint, i
    integer, dimension(16) :: iblock
    integer :: max_exp

    ! bitstream
    ! the first bit (pos .eq. 0) : '0' - all values are either non-NaN or NaN
    ! the second bit (pos .eq. 1) : if the first bit is '0' then '0' : non-NaN, '1' : NaN
    integer(kind=4), dimension(4) :: bitstream
    integer :: pos
    integer :: bits

    ! loop counters
    integer ix, iy

    do iy = 1, 4
        do ix = 1, 4
            x(ix, iy) = ix*iy
        end do
    end do

    x(4/2, 4/2) = 6.4

    print *, x

    e = exponent(x)
    max_exp = maxval(e)

    print *, 'e:', e
    print *, 'max_exp:', max_exp

    ! clear the bitstream
    bitstream = 0
    pos = 0

    ! bitstream = ibset(bitstream, 0)
    ! call mvbits(int(3, kind=16), 0, 3, bitstream, pos)

    ! all values are non-NaN, emit '0';
    ! no need to do anything since initially bitstream = 0
    pos = pos + 1

    ! emit the biased exponent (8 bits)
    ! call mvbits(int(max_exp + EBIAS, kind=16), 0, 8, bitstream, pos)

    bits = max_exp + EBIAS
    write (*, '(a,b32.32)') 'biased max_exp ', bits

    call stream_write_bits(bitstream, bits, 8, pos)

    print *, 'bitstream(1)'
    write (*, '(b32.32)') bitstream(1)

    ! quantize
    i = e - max_exp + fraction_bits
    qint = nint(set_exponent(x, i))
    iblock = reshape(qint, (/16/))

    print *, 'i:', i
    print *, 'iblock:', iblock

    ! decorrelate
    call fwd_xform(iblock)
    print *, 'iblock:', iblock

    ! reorder signed coefficients and convert to unsigned integer
    call fwd_order(iblock)
    print *, 'ublock:', iblock

    ! encode 32-bit integers
    call encode_many_ints(iblock, bitstream, pos)

    if (pos .lt. max_bits) then
        call pad_stream(bitstream, max_bits - pos, pos)
    end if

    print *, 'bitstream'
    write (*, '(b32.32)') bitstream(1)
    write (*, '(b32.32)') bitstream(2)
    write (*, '(b32.32)') bitstream(3)
    write (*, '(b32.32)') bitstream(4)
    print *, 'pos', pos

    ! reverse the process
    pos = max_bits - 1

    ! read the NaN mask bit
    print *, 'NaN mask', stream_read_bit(bitstream, pos)

    bits = stream_read_bits(bitstream, pos, 8)
    max_exp = bits - EBIAS
    print *, 'max_exp', max_exp

    ! decode 32-bit integers
    call decode_many_ints(iblock, bitstream, pos)
    print *, 'ublock:', iblock

    ! reorder unsigned coefficients and convert to signed integer
    call inv_order(iblock)
    print *, 'iblock:', iblock

    ! perform decorrelating transform
    call inv_xform(iblock)
    print *, 'iblock:', iblock

    ! de-quantize
    qint = reshape(iblock, (/4, 4/))

    i = max_exp - fraction_bits
    x = scale(real(qint), i)

    print *, x

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

    subroutine encode_many_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(in) :: data
        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(inout) :: pos

        integer :: i, k, bit, c, n

        ! a counter for runs of '1'
        integer :: bcount

        bcount = 0
        n = 0

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1
            print *, 'k', k, 'n', n, 'pos', pos

            ! gather / emit up to n bits from the k-th bit plane
            do i = 1, n
                bit = ibits(data(i), k, 1)

                ! check if there is space to write
                if (pos .eq. max_bits) return
                call stream_write_bit(stream, bit, pos)
            end do

            if (n .eq. 16) cycle

            ! reset the '1' bit counter
            bcount = 0

            ! count remaining '1's in the k-th bit plane
            do i = n + 1, 16
                bit = ibits(data(i), k, 1)

                if (bit .eq. 1) bcount = bcount + 1
            end do

            print *, '"1" bcount', bcount

            ! unary run-length encode the remaining bits
            if (bcount .gt. 0) then
                do c = 1, bcount
                    print *, 'c', c

                    ! emit '1'
                    if (pos .eq. max_bits) return
                    call stream_write_bit(stream, 1, pos)

                    ! keep emitting '0' until '1' is encountered
1001                n = n + 1
                    if (n .gt. 16) exit

                    bit = ibits(data(n), k, 1)

                    if (pos .eq. max_bits) return
                    call stream_write_bit(stream, bit, pos)

                    if (bit .eq. 0) go to 1001
                end do
            end if

            if (n .lt. 16) then
                ! emit '0'
                if (pos .eq. max_bits) return
                call stream_write_bit(stream, 0, pos)
            end if

        end do

    end subroutine encode_many_ints

    subroutine decode_many_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(out) :: data
        integer(kind=4), dimension(4), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: i, k, bit, c, n

        ! a counter for runs of '1'
        integer :: bcount

        bcount = 0
        n = 0

        ! zero-out the integer data array
        data = 0

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1
            print *, 'k', k, 'n', n, 'pos', pos

            ! read the first n bits
            do i = 1, n
                bit = stream_read_bit(stream, pos)
                if (bit .lt. 0) return

                if (bit .eq. 1) data(i) = ibset(data(i), k)
            end do

            if (n .eq. 16) cycle

            ! read one bit, test it for '1'
1002        bit = stream_read_bit(stream, pos)
            if (bit .lt. 0) return

            if (bit .eq. 1) then
                ! keep on reading '0' until '1' is encountered

                print *, 'a new sequence starting @', n + 1

1003            bit = stream_read_bit(stream, pos)
                if (bit .lt. 0) return

                n = n + 1
                if (n .gt. 16) go to 1004

                if (bit .eq. 1) then
                    print *, 'setting bit @', n
                    data(n) = ibset(data(n), k)

                    if (n .eq. 16) go to 1004

                    go to 1002
                else
                    go to 1003
                end if
            else
                go to 1004
            end if

1004        continue

        end do

    end subroutine decode_many_ints

    subroutine encode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(in) :: data
        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(inout) :: pos

        integer(kind=2) :: val, i, k, bit

        ! a counter for runs of '0'
        integer :: zcount, status, bit_count

        zcount = 0
        bit_count = 0

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1
            print *, 'k', k

            val = 0
            ! gather k-plane bits from the input data
            do i = 1, 16
                bit = int(ibits(data(i), k, 1), kind=2)
                ! print *, 'i', i, 'bit', bit, data(i)

                if (bit .eq. 1) then
                    ! the runs of zeroes has finished

                    ! Golomb or Rice-encode the zcount
                    status = Golomb_encode(stream, pos, zcount)
                    if (status .eq. -1) return

                    ! reset the zeroes counters
                    zcount = 0
                else
                    zcount = zcount + 1
                end if

                bit_count = bit_count + 1
                print *, 'bit_count', bit_count

                ! val = ior(shiftl(val, 1), bit)
            end do

            ! display the value (validation)
            ! write (*, '(a,i0,a,b16.16)') 'k: ', k, ', val: ', val
        end do

        ! flush the encoder
        status = Golomb_encode(stream, pos, zcount)

    end subroutine encode_ints

    subroutine decode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(out) :: data
        integer(kind=4), dimension(4), intent(in) :: stream
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

                    print *, 'zcount', zcount

                end if

                zcount = zcount - 1

                if (zcount .lt. 0) then
                    ! set the appropriate bit to '1'
                    data(i) = ibset(data(i), k)
                end if

            end do
        end do

    end subroutine decode_ints

    integer function Golomb_encode(stream, pos, N)
        implicit none

        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: N

        integer :: i, t, nbits

        ! quotient, remainder
        integer :: q, r

        Golomb_encode = -1

        q = N/G_M
        r = modulo(N, G_M)

        print *, 'Golomb encoder: N', N, 'pos', pos, 'q', q, 'r', r

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

            print *, 'coding the remainder using', nbits, 'nbits'

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits, pos)

        else

            nbits = G_b
            r = r + t

            print *, 'coding remainder+', r, 'using', nbits, 'nbits'

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits, pos)

        end if

        Golomb_encode = 0

        return

    end function Golomb_encode

    integer function Golomb_decode(stream, pos)
        implicit none

        integer(kind=4), dimension(4), intent(in) :: stream
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

        x = stream_read_bits(stream, pos, G_b - 1)

        if (x .lt. 0) return

        t = 2**G_b - G_M

        if (x .lt. t) then
            Golomb_decode = s*G_M + x
            return
        end if

        bit = stream_read_bit(stream, pos)

        if (bit .lt. 0) return

        x = shiftl(x, 1) + bit

        Golomb_decode = s*G_M + x - t

        return

    end function Golomb_decode

    subroutine Rice_encode(stream, pos, N)
        implicit none

        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: N

        integer :: i

        ! quotient, pseudo-remainder
        integer :: q, r

        q = shiftr(N, R_k) ! divide by R_M = 2**R_k
        r = N

        print *, 'Rice encoder: N', N, 'pos', pos, 'q', q

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

        integer(kind=4), dimension(4), intent(in) :: stream
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

        x = stream_read_bits(stream, pos, R_k)

        if (x .lt. 0) return

        Rice_decode = shiftl(s, R_k) + x

        return

    end function Rice_decode

    subroutine stream_write_bit(stream, bit, pos)
        implicit none

        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(in) :: bit
        integer, intent(inout) :: pos

        integer :: idx

        idx = 1 + shiftr(pos, 5) ! divide by 32 to get an int index

        ! make place for the new bit
        stream(idx) = shiftl(stream(idx), 1)

        ! set the LSB bit
        if (bit .eq. 1) then
            stream(idx) = ibset(stream(idx), 0)
        end if

        print *, 'stream_write_bit@pos', pos, 'idx', idx, 'bit', bit

        pos = pos + 1

        return

    end subroutine stream_write_bit

    subroutine stream_write_bits(stream, bits, n, pos)
        implicit none

        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(inout) :: bits
        integer, intent(in) :: n
        integer, intent(inout) :: pos

        integer :: i, bit

        if (n .lt. 1) return

        do i = n - 1, 0, -1
            if (btest(bits, i)) then
                bit = 1
            else
                bit = 0
            end if

            call stream_write_bit(stream, bit, pos)
        end do

        return

    end subroutine stream_write_bits

    subroutine pad_stream(stream, n, pos)
        implicit none

        integer(kind=4), dimension(4), intent(inout) :: stream
        integer, intent(in) :: n
        integer, intent(inout) :: pos

        integer :: i

        if (n .lt. 1) return

        do i = 1, n
            call stream_write_bit(stream, 0, pos)
        end do

        return
    end subroutine pad_stream

    integer function stream_read_bit(stream, pos)
        implicit none

        integer(kind=4), dimension(4), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: idx, shift

        if (pos .lt. 0) then
            stream_read_bit = -1
            return
        end if

        idx = 1 + shiftr(pos, 5) ! divide by 32 to get an int index
        shift = 31 - mod(pos, 32)

        if (btest(stream(idx), shift)) then
            stream_read_bit = 1
        else
            stream_read_bit = 0
        end if

        pos = pos - 1

        return

    end function stream_read_bit

    integer function stream_read_bits(stream, pos, n)
        implicit none

        integer(kind=4), dimension(4), intent(in) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: n

        integer :: i, bit

        stream_read_bits = -1

        if (n .lt. 1) return

        if (pos - n + 1 .lt. 0) return

        stream_read_bits = 0

        do i = n - 1, 0, -1
            ! read the next bit
            bit = stream_read_bit(stream, pos)

            if (bit .lt. 0) return

            if (bit .eq. 1) then
                stream_read_bits = ibset(stream_read_bits, i)
            end if
        end do

        return

    end function stream_read_bits

end program main
