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
    integer(kind=16) :: bitstream
    integer :: pos
    integer :: bits

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

    call stream_write_bits(bitstream, bits, 8)
    pos = pos + 8

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
    print *, 'ublock:', iblock

    ! encode 32-bit integers
    call encode_ints(iblock, bitstream, pos)

    if (pos .lt. max_bits) then
        call pad_stream(bitstream, max_bits - pos)
        pos = max_bits
    end if

    write (*, '(a,b128.128)') 'bitstream ', bitstream
    print *, 'pos', pos

    ! reverse the process
    pos = max_bits - 1

    ! read the NaN mask bit
    print *, 'NaN mask', stream_read_bit(bitstream, pos)

    bits = stream_read_bits(bitstream, pos, 8)
    max_exp = bits - EBIAS
    print *, 'max_exp', max_exp

    ! decode 32-bit integers
    call decode_ints(iblock, bitstream, pos)
    print *, 'ublock:', iblock

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

    subroutine encode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(in) :: data
        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos

        integer(kind=2) :: val, i, k, bit

        ! a counter for runs of '0'
        integer :: zcount

        zcount = 0

        ! iterate over 32 bits from MSB to LSB
        do k = 31, 0, -1

            val = 0
            ! gather k-plane bits from the input data
            do i = 1, 16
                bit = int(ibits(data(i), k, 1), kind=2)
                ! print *, 'i', i, 'bit', bit, data(i)

                if (bit .eq. 1) then
                    ! the runs of zeroes has finished

                    ! Golomb-encode the zcount
                    call Golomb_encode(stream, pos, zcount)

                    ! alternatively Rice-encode the zcount
                    ! call Rice_encode(stream, pos, zcount)

                    ! reset the zeroes counters
                    zcount = 0
                else
                    zcount = zcount + 1
                end if

                ! val = ior(shiftl(val, 1), bit)
            end do

            ! display the value (validation)
            ! write (*, '(a,i0,a,b16.16)') 'k: ', k, ', val: ', val
        end do

        ! flush the encoder
        call Golomb_encode(stream, pos, zcount)

    end subroutine encode_ints

    subroutine decode_ints(data, stream, pos)
        implicit none

        integer, dimension(16), intent(out) :: data
        integer(kind=16), intent(in) :: stream
        integer, intent(inout) :: pos

        integer :: i, k, bit

        ! a counter for runs of '0'
        integer :: zcount

        data = 0

        ! do
        !    zcount = Golomb_decode(stream, pos)

        !    if (zcount .lt. 0) exit

        !    print *, 'zcount', zcount
        !end do

        bit = 0
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
                    ! emit '1'
                    data(i) = ibset(data(i), k)
                end if
            end do
        end do

    end subroutine decode_ints

    subroutine Golomb_encode(stream, pos, N)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: pos
        integer, intent(in) :: N

        integer :: i, t, nbits

        ! quotient, remainder
        integer :: q, r

        q = N/G_M
        r = modulo(N, G_M)

        print *, 'Golomb encoder: N', N, 'pos', pos, 'q', q, 'r', r

        ! Quotient Code
        if (q .gt. 0) then
            do i = 1, q
                ! check if there is space to write
                if (pos .eq. max_bits) return
                call stream_write_bit(stream, 1)
                pos = pos + 1
            end do
        end if

        if (pos .eq. max_bits) return
        call stream_write_bit(stream, 0)
        pos = pos + 1

        ! Remainder Code
        t = 2**G_b - G_M

        if (r .lt. t) then
            nbits = G_b - 1

            print *, 'coding the remainder using', nbits, 'nbits'

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits)
            pos = pos + nbits
        else
            nbits = G_b
            r = r + t

            print *, 'coding remainder+', r, 'using', nbits, 'nbits'

            ! check if there is space to write
            if (pos + nbits .gt. max_bits) return
            call stream_write_bits(stream, r, nbits)
            pos = pos + nbits
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

        integer(kind=16), intent(inout) :: stream
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
                call stream_write_bit(stream, 1)
                pos = pos + 1
            end do
        end if

        if (pos .eq. max_bits) return
        call stream_write_bit(stream, 0)
        pos = pos + 1

        ! Remainder Code
        ! check if there is space to write
        if (pos + R_k .gt. max_bits) return
        call stream_write_bits(stream, r, R_k)
        pos = pos + R_k

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

        x = stream_read_bits(stream, pos, R_k)

        if (x .lt. 0) return

        Rice_decode = shiftl(s, R_k) + x

        return

    end function Rice_decode

    subroutine stream_write_bit(stream, bit)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(in) ::bit

        ! make place for the new bit
        stream = shiftl(stream, 1)

        ! set the LSB bit
        if (bit .eq. 1) then
            stream = ibset(stream, 0)
        end if

    end subroutine stream_write_bit

    subroutine stream_write_bits(stream, bits, n)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(inout) :: bits
        integer, intent(in) :: n

        integer :: i

        if (n .lt. 1) return

        stream = shiftl(stream, n)
        call mvbits(int(bits, kind=16), 0, n, stream, 0)

        return
    end subroutine stream_write_bits

    subroutine pad_stream(stream, n)
        implicit none

        integer(kind=16), intent(inout) :: stream
        integer, intent(in) :: n

        integer :: i

        if (n .lt. 1) return

        print *, 'padding the stream with', n, '0 bits'

        stream = shiftl(stream, n)

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

end program main
