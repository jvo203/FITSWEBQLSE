module zfp
    use zFORp
    use, intrinsic :: iso_c_binding
    implicit none

contains
! Fortran convenience subroutines

    subroutine compress_pixels(pixels, compressed)
        use, intrinsic :: iso_c_binding
        implicit none

        ! inputs
        real(kind=4), dimension(:, :), contiguous, target, intent(in) :: pixels

        ! the output
        character(kind=c_char), allocatable, intent(out) :: compressed(:)

        ! internal variables
        integer :: nx, ny

        ! zfp_field
        type(zFORp_field) :: field

        ! bitstream
        character(kind=c_char), allocatable, target :: buffer(:)
        integer(kind=8) buffer_size_bytes, bitstream_offset_bytes
        type(zFORp_bitstream) :: bitstream, queried_bitstream

        ! zfp_stream
        type(zFORp_stream) :: stream
        real(kind=8) :: desired_rate, rate_result
        integer :: dims, wra
        integer :: zfp_type

        nx = ubound(pixels, 1)
        ny = ubound(pixels, 2)

        print *, 'nx=', nx, ', ny=', ny

        ! setup zfp_field
        zfp_type = zFORp_type_float
        field = zFORp_field_2d(c_loc(pixels), zfp_type, nx, ny)

        ! in the worst case assume no compression
        buffer_size_bytes = sizeof(pixels)
        allocate (buffer(buffer_size_bytes))
        bitstream = zFORp_bitstream_stream_open(c_loc(buffer), buffer_size_bytes)

        ! setup zfp_stream
        stream = zFORp_stream_open(bitstream)

        desired_rate = 8.0
        dims = 2
        wra = 0
        zfp_type = zFORp_type_float
        rate_result = zFORp_stream_set_rate(stream, desired_rate, zfp_type, dims, wra)

        queried_bitstream = zFORp_stream_bit_stream(stream)

        ! compress
        bitstream_offset_bytes = zFORp_compress(stream, field)
        write (*, *) "After compression, bitstream offset at "
        write (*, *) bitstream_offset_bytes

        if (bitstream_offset_bytes .gt. 0) compressed = reshape(buffer, (/bitstream_offset_bytes/))

    end subroutine compress_pixels

end module zfp
