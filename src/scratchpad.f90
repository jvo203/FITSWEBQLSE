subroutine downsize_mask(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    logical(kind=c_bool), dimension(:, :), intent(in) :: X
    logical(kind=c_bool), dimension(:, :), intent(out) :: Y
    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height
    integer :: i, j

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    do concurrent(j=1:dst_height, i=1:dst_width)
        Y(i, j) = X(1 + (i - 1)*(src_width - 1)/(dst_width - 1), 1 + (j - 1)*(src_height - 1)/(dst_height - 1))
    end do

    call cpu_time(t2)

    print *, '[downsize_mask] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_mask

subroutine downsize_nn(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    real(kind=c_float), dimension(:, :), intent(in) :: X
    real(kind=c_float), dimension(:, :), intent(out) :: Y
    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height
    integer :: i, j

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    do concurrent(j=1:dst_height, i=1:dst_width)
        Y(i, j) = X(1 + (i - 1)*(src_width - 1)/(dst_width - 1), 1 + (j - 1)*(src_height - 1)/(dst_height - 1))
    end do

    call cpu_time(t2)

    print *, '[downsize_nn] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_nn

subroutine downsize_linear(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1

    ! interpolated values
    real :: I0, I1

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(src_width + 1, src_height + 1))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then add the boundaries
    ! (Fourier transform origin, use reflection)
    W(src_width + 1, :) = X(1, :)
    W(:, src_height + 1) = X(:, 1)

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)
        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs)
        Ys0 = nint(Ys)

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        I0 = W(int(Xs0), int(Ys0))*(Xs1 - Xs) + W(int(Xs1), int(Ys0))*(Xs - Xs0)
        I1 = W(int(Xs0), int(Ys1))*(Xs1 - Xs) + W(int(Xs1), int(Ys1))*(Xs - Xs0)

        ! Linear Interpolation
        Y(Xd, Yd) = I0*(Ys1 - Ys) + I1*(Ys - Ys0)
    end do

    call cpu_time(t2)

    print *, '[downsize_linear] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_linear

elemental function Lanczos2(x)
    real :: Lanczos2
    real, intent(in) :: x
    real arg, sinc1, sinc2

    if (abs(x) .lt. 2) then
        arg = PI*x
        sinc1 = sin(arg)/arg
        sinc2 = sin(arg/2)/(arg/2)

        Lanczos2 = sinc1*sinc2
    else
        Lanczos2 = 0.0
    end if
end function Lanczos2

elemental function Lanczos3(x)
    real :: Lanczos3
    real, intent(in) :: x
    real arg, sinc1, sinc2

    if (abs(x) .lt. 3) then
        arg = PI*x
        sinc1 = sin(arg)/arg
        sinc2 = sin(arg/3)/(arg/3)

        Lanczos3 = sinc1*sinc2
    else
        Lanczos3 = 0.0
    end if
end function Lanczos3

subroutine downsize_lanczos_2(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1, Xs2, Ys2, Xs3, Ys3

    ! interpolated values
    real :: I0, I1, I2, I3
    real :: a0, a1, a2, a3
    real :: b0, b1, b2, b3

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(0:src_width + 2, 0:src_height + 2))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then add the boundaries
    ! (Fourier transform origin, use reflection)
    W(0, :) = X(src_width, :)
    W(src_width + 1, :) = X(1, :)
    W(src_width + 2, :) = X(2, :)

    W(:, 0) = X(:, src_height)
    W(:, src_height + 1) = X(:, 1)
    W(:, src_height + 2) = X(:, 2)

    ! the corners are not handled, we should be using the IPP

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)

        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs) - 1
        Ys0 = nint(Ys) - 1

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        Xs2 = Xs0 + 2
        Ys2 = Ys0 + 2

        Xs3 = Xs0 + 3
        Ys3 = Ys0 + 3

        ! Lanczos coefficients
        ! they should really be pre-calculated outside the loop
        a0 = Lanczos2(Xs - Xs0)
        a1 = Lanczos2(Xs - Xs1)
        a2 = Lanczos2(Xs - Xs2)
        a3 = Lanczos2(Xs - Xs3)

        b0 = Lanczos2(Ys - Ys0)
        b1 = Lanczos2(Ys - Ys1)
        b2 = Lanczos2(Ys - Ys2)
        b3 = Lanczos2(Ys - Ys3)

        ! intermediate intensities
        I0 = a0*W(int(Xs0), int(Ys0)) + &
             a1*W(int(Xs1), int(Ys0)) + &
             a2*W(int(Xs2), int(Ys0)) + &
             a3*W(int(Xs3), int(Ys0))

        I1 = a0*W(int(Xs0), int(Ys1)) + &
             a1*W(int(Xs1), int(Ys1)) + &
             a2*W(int(Xs2), int(Ys1)) + &
             a3*W(int(Xs3), int(Ys1))

        I2 = a0*W(int(Xs0), int(Ys2)) + &
             a1*W(int(Xs1), int(Ys2)) + &
             a2*W(int(Xs2), int(Ys2)) + &
             a3*W(int(Xs3), int(Ys2))

        I3 = a0*W(int(Xs0), int(Ys3)) + &
             a1*W(int(Xs1), int(Ys3)) + &
             a2*W(int(Xs2), int(Ys3)) + &
             a3*W(int(Xs3), int(Ys3))

        ! 2-lobed Lanczos
        Y(Xd, Yd) = b0*I0 + b1*I1 + b2*I2 + b3*I3

    end do

    call cpu_time(t2)

    print *, '[downsize_lanczos_2] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_lanczos_2

subroutine downsize_lanczos_3(X, Y)
    use, intrinsic :: iso_c_binding
    implicit none

    ! the input array
    real(kind=c_float), dimension(:, :), intent(in) :: X

    ! the output (downsized) array
    real(kind=c_float), dimension(:, :), intent(out) :: Y

    ! a working array to handle image boundaries
    real(kind=c_float), dimension(:, :), allocatable :: W

    integer, dimension(2) :: src, dst
    integer :: src_width, src_height
    integer :: dst_width, dst_height

    ! source coordinates
    real :: Xs, Ys

    ! downsized destination coordinates
    integer :: Xd, Yd

    ! nearby-pixels
    real :: Xs0, Ys0, Xs1, Ys1, Xs2, Ys2, Xs3, Ys3, Xs4, Ys4, Xs5, Ys5

    ! interpolated values
    real :: I0, I1, I2, I3, I4, I5
    real :: a0, a1, a2, a3, a4, a5
    real :: b0, b1, b2, b3, b4, b5

    ! timing
    real :: t1, t2

    call cpu_time(t1)

    src = shape(X)
    src_width = src(1)
    src_height = src(2)

    dst = shape(Y)
    dst_width = dst(1)
    dst_height = dst(2)

    allocate (W(-5:src_width + 6, -5:src_height + 6))

    ! copy the image
    W(1:src_width, 1:src_height) = X

    ! then replicate the borders
    W(-5, :) = X(1, :)
    W(-4, :) = X(1, :)
    W(-3, :) = X(1, :)
    W(-2, :) = X(1, :)
    W(-1, :) = X(1, :)
    W(0, :) = X(1, :)
    W(src_width + 1, :) = X(src_width, :)
    W(src_width + 2, :) = X(src_width, :)
    W(src_width + 3, :) = X(src_width, :)
    W(src_width + 4, :) = X(src_width, :)
    W(src_width + 5, :) = X(src_width, :)
    W(src_width + 6, :) = X(src_width, :)

    W(:, -5) = X(:, 1)
    W(:, -4) = X(:, 1)
    W(:, -3) = X(:, 1)
    W(:, -2) = X(:, 1)
    W(:, -1) = X(:, 1)
    W(:, 0) = X(:, 1)
    W(:, src_height + 1) = X(:, src_height)
    W(:, src_height + 2) = X(:, src_height)
    W(:, src_height + 3) = X(:, src_height)
    W(:, src_height + 4) = X(:, src_height)
    W(:, src_height + 5) = X(:, src_height)
    W(:, src_height + 6) = X(:, src_height)

    ! the corners are not handled, we should be using the IPP

    do concurrent(Yd=1:dst_height, Xd=1:dst_width)

        Xs = 1 + real(Xd - 1)*real(src_width - 1)/real(dst_width - 1)
        Ys = 1 + real(Yd - 1)*real(src_height - 1)/real(dst_height - 1)

        Xs0 = nint(Xs) - 2
        Ys0 = nint(Ys) - 2

        Xs1 = Xs0 + 1
        Ys1 = Ys0 + 1

        Xs2 = Xs0 + 2
        Ys2 = Ys0 + 2

        Xs3 = Xs0 + 3
        Ys3 = Ys0 + 3

        Xs4 = Xs0 + 4
        Ys4 = Ys0 + 4

        Xs5 = Xs0 + 5
        Ys5 = Ys0 + 5

        ! Lanczos coefficients
        ! they should really be pre-calculated outside the loop
        a0 = Lanczos2(Xs - Xs0)
        a1 = Lanczos2(Xs - Xs1)
        a2 = Lanczos2(Xs - Xs2)
        a3 = Lanczos2(Xs - Xs3)
        a4 = Lanczos2(Xs - Xs4)
        a5 = Lanczos2(Xs - Xs5)

        b0 = Lanczos2(Ys - Ys0)
        b1 = Lanczos2(Ys - Ys1)
        b2 = Lanczos2(Ys - Ys2)
        b3 = Lanczos2(Ys - Ys3)
        b4 = Lanczos2(Ys - Ys4)
        b5 = Lanczos2(Ys - Ys5)

        ! intermediate intensities
        I0 = a0*W(int(Xs0), int(Ys0)) + &
             a1*W(int(Xs1), int(Ys0)) + &
             a2*W(int(Xs2), int(Ys0)) + &
             a3*W(int(Xs3), int(Ys0)) + &
             a4*W(int(Xs4), int(Ys0)) + &
             a5*W(int(Xs5), int(Ys0))

        I1 = a0*W(int(Xs0), int(Ys1)) + &
             a1*W(int(Xs1), int(Ys1)) + &
             a2*W(int(Xs2), int(Ys1)) + &
             a3*W(int(Xs3), int(Ys1)) + &
             a4*W(int(Xs4), int(Ys1)) + &
             a5*W(int(Xs5), int(Ys1))

        I2 = a0*W(int(Xs0), int(Ys2)) + &
             a1*W(int(Xs1), int(Ys2)) + &
             a2*W(int(Xs2), int(Ys2)) + &
             a3*W(int(Xs3), int(Ys2)) + &
             a4*W(int(Xs4), int(Ys2)) + &
             a5*W(int(Xs5), int(Ys2))

        I3 = a0*W(int(Xs0), int(Ys3)) + &
             a1*W(int(Xs1), int(Ys3)) + &
             a2*W(int(Xs2), int(Ys3)) + &
             a3*W(int(Xs3), int(Ys3)) + &
             a4*W(int(Xs4), int(Ys3)) + &
             a5*W(int(Xs5), int(Ys3))

        I4 = a0*W(int(Xs0), int(Ys4)) + &
             a1*W(int(Xs1), int(Ys4)) + &
             a2*W(int(Xs2), int(Ys4)) + &
             a3*W(int(Xs3), int(Ys4)) + &
             a4*W(int(Xs4), int(Ys4)) + &
             a5*W(int(Xs5), int(Ys4))

        I5 = a0*W(int(Xs0), int(Ys5)) + &
             a1*W(int(Xs1), int(Ys5)) + &
             a2*W(int(Xs2), int(Ys5)) + &
             a3*W(int(Xs3), int(Ys5)) + &
             a4*W(int(Xs4), int(Ys5)) + &
             a5*W(int(Xs5), int(Ys5))

        ! 3-lobed Lanczos
        Y(Xd, Yd) = b0*I0 + b1*I1 + b2*I2 + b3*I3 + b4*I4 + b5*I5

    end do

    call cpu_time(t2)

    print *, '[downsize_lanczos_3] SRC:', src, 'DST:', dst, 'elapsed:', 1000*(t2 - t1), '[ms]'

end subroutine downsize_lanczos_3

subroutine to_json(str_val)
    use json_module
    implicit NONE

    CHARACTER(kind=json_CK, len=:), allocatable, intent(out) :: str_val

    type(json_core) :: json
    type(json_value), pointer :: p

    ! initialize the class
    call json%initialize(non_normal_mode=2)

    ! initialize the structure:
    call json%create_object(p, '')

    ! FITS HEADER
    ! call json%add(p, 'HEADER', char(item%hdr))
    call json%add(p, 'HEADER', 'N/A')

    ! misc. values
    call json%add(p, 'width', item%naxes(1))
    call json%add(p, 'height', item%naxes(2))
    call json%add(p, 'depth', item%naxes(3))
    call json%add(p, 'polarisation', item%naxes(4))
    call json%add(p, 'filesize', 0)
    call json%add(p, 'IGNRVAL', item%ignrval)

    call json%add(p, 'CD1_1', item%cd1_1)
    call json%add(p, 'CD1_2', item%cd1_2)
    call json%add(p, 'CD2_1', item%cd2_1)
    call json%add(p, 'CD2_2', item%cd2_2)

    call json%add(p, 'CRVAL1', item%crval1)
    call json%add(p, 'CDELT1', item%cdelt1)
    call json%add(p, 'CRPIX1', item%crpix1)
    call json%add(p, 'CUNIT1', trim(item%cunit1))
    call json%add(p, 'CTYPE1', trim(item%ctype1))

    call json%add(p, 'CRVAL2', item%crval2)
    call json%add(p, 'CDELT2', item%cdelt2)
    call json%add(p, 'CRPIX2', item%crpix2)
    call json%add(p, 'CUNIT2', trim(item%cunit2))
    call json%add(p, 'CTYPE2', trim(item%ctype2))

    call json%add(p, 'CRVAL3', item%crval3)
    call json%add(p, 'CDELT3', item%cdelt3)
    call json%add(p, 'CRPIX3', item%crpix3)
    call json%add(p, 'CUNIT3', trim(item%cunit3))
    call json%add(p, 'CTYPE3', trim(item%ctype3))

    call json%add(p, 'BMAJ', item%bmaj)
    call json%add(p, 'BMIN', item%bmin)
    call json%add(p, 'BPA', item%bpa)

    call json%add(p, 'BUNIT', trim(item%bunit))
    call json%add(p, 'BTYPE', trim(item%btype))
    call json%add(p, 'SPECSYS', trim(item%specsys))

    call json%add(p, 'RESTFRQ', item%restfrq)
    call json%add(p, 'OBSRA', item%obsra)
    call json%add(p, 'OBSDEC', item%obsdec)

    call json%add(p, 'OBJECT', trim(item%object))
    call json%add(p, 'DATEOBS', trim(item%date_obs))
    call json%add(p, 'TIMESYS', trim(item%timesys))
    call json%add(p, 'LINE', trim(item%line))
    call json%add(p, 'FILTER', trim(item%filter))

    call json%add(p, 'mean_spectrum', item%mean_spectrum)
    call json%add(p, 'integrated_spectrum', item%integrated_spectrum)

    ! statistics (image histogram)
    call json%add(p, 'histogram', item%hist)

    ! print out JSON
    ! call json%print(p)

    ! write the file:
    ! call json%print(p, char(item%datasetid)//'.json')

    ! serialize to string prior to further handling
    call json%serialize(p, str_val)

    ! cleanup:
    call json%destroy(p)

    if (json%failed()) stop 1

end subroutine to_json

function extract_datasetid_old(filename) result(datasetid)
    use iso_varying_string
    implicit none

    character(len=*), intent(in) :: filename
    type(varying_string) :: datasetid
    type(varying_string) :: letter
    integer :: i, str_len
    character :: c

    print *, 'extract_datasetid#1'

    datasetid = ''

    print *, 'extract_datasetid#2'

    ! work from the end, processing characters one by one
    ! exit upon encountering the first '/'
    str_len = len(filename)

    print *, 'extract_datasetid#3, len = ', str_len

    do i = str_len, 1, -1
        c = filename(i:i)

        if (c .eq. ' ') cycle

        if (c .eq. '/') exit

        print *, 'extract_datasetid#i=', i, 'BEFORE'
        ! prepend the element to tmp
        letter = c
        ! datasetid = letter//datasetid
        letter = insert(c, 1, datasetid)
        print *, 'extract_datasetid#i=', i, 'AFTER'
    end do

    print *, 'extract_datasetid#4'

    ! get rid of FITS file extensions
    ! should be able to handle .fits.gz etc... too

    ! lowercase, ignore starting positions .eq. 1
    i = index(datasetid, '.fits')
    if (i .gt. 1) datasetid = extract(datasetid, 1, i - 1)

    ! uppercase, ignore starting positions .eq. 1
    i = index(datasetid, '.FITS')
    if (i .gt. 1) datasetid = extract(datasetid, 1, i - 1)

end function extract_datasetid_old

use :: zmq

! ØMQ
! integer :: rc
! type(c_ptr)     :: server_context, client_context
! type(c_ptr)     :: server_socket, client_socket

subroutine recv_command(socket)
    type(c_ptr), intent(inout) :: socket
    character(kind=c_char, len=:), pointer :: buffer
    integer                                :: nbytes
    integer                                :: rc
    type(c_ptr)                            :: data
    type(zmq_msg_t)                        :: message

    rc = zmq_msg_init(message)

    ! print *, this_image(), 'zmq_msg_init::rc', rc

    nbytes = zmq_msg_recv(message, socket, 0)

    ! print *, this_image(), 'zmq_msg_recv::nbytes', nbytes

    data = zmq_msg_data(message)

    call c_f_pointer(data, buffer)

    if (nbytes .gt. 0) then
        print *, this_image(), '[ØMQ] msg len:', nbytes, 'buffer:', buffer
    end if

    rc = zmq_msg_close(message)
end subroutine recv_command

subroutine send_command(socket, cmd)
    use, intrinsic :: iso_c_binding

    type(c_ptr), intent(inout) :: socket
    character(kind=c_char), intent(in), target :: cmd(:)

    integer(kind=c_int)                         :: nbytes
    integer(kind=c_int)                         :: rc
    type(zmq_msg_t)                             :: message
    INTEGER(KIND=C_SIZE_T) :: msg_len

    msg_len = size(cmd)

    print *, '[ØMQ] msg_len:', msg_len

    rc = zmq_msg_init_data(message, c_loc(cmd), msg_len, c_null_funptr, c_null_ptr)

    print *, '[ØMQ] zmq_msg_init_data::rc', rc

    nbytes = zmq_msg_send(message, socket, 0)

    print *, '[ØMQ] nbytes sent', nbytes

    rc = zmq_msg_close(message)
end subroutine send_command

subroutine get_json_var(item, json)
    use iso_varying_string
    use json_for
    implicit none

    type(dataset), pointer, intent(in) :: item
    type(varying_string), intent(out) :: json
    integer :: str_len
    integer(kind=8) :: filesize

    ! calculate the FITS file size
    filesize = nint(real(size(item%hdr)) + real(item%naxes(1))*real(item%naxes(2))&
                   &*real(item%naxes(3))*real(item%naxes(4))*real(abs(item%bitpix)/8), kind=8)

    json = '{'

    !print *, 'header size:', size(item%hdr), 'flux:', trim(item%flux),&
    !& ', filesize:', filesize, ', cunit3:', trim(item%cunit3), ',json:', char(json), trim(item%cunit1)

    ! call json_add_string(json, 'HEADER', 'NULL')

    ! misc. values
    call json_add_integer_number(json, 'width', item%naxes(1))
    call json_add_integer_number(json, 'height', item%naxes(2))
    call json_add_integer_number(json, 'depth', item%naxes(3))
    call json_add_integer_number(json, 'polarisation', item%naxes(4))
    call json_add_long_number(json, 'filesize', filesize)
    call json_add_real_number(json, 'IGNRVAL', item%ignrval)

    call json_add_real_number(json, 'CD1_1', item%cd1_1)
    call json_add_real_number(json, 'CD1_2', item%cd1_2)
    call json_add_real_number(json, 'CD2_1', item%cd2_1)
    call json_add_real_number(json, 'CD2_2', item%cd2_2)

    call json_add_real_number(json, 'CRVAL1', item%crval1)
    call json_add_real_number(json, 'CDELT1', item%cdelt1)
    call json_add_real_number(json, 'CRPIX1', item%crpix1)
    call json_add_string(json, 'CUNIT1', trim(item%cunit1))
    call json_add_string(json, 'CTYPE1', trim(item%ctype1))

    call json_add_real_number(json, 'CRVAL2', item%crval2)
    call json_add_real_number(json, 'CDELT2', item%cdelt2)
    call json_add_real_number(json, 'CRPIX2', item%crpix2)
    call json_add_string(json, 'CUNIT2', trim(item%cunit2))
    call json_add_string(json, 'CTYPE2', trim(item%ctype2))

    call json_add_real_number(json, 'CRVAL3', item%crval3)
    call json_add_real_number(json, 'CDELT3', item%cdelt3)
    call json_add_real_number(json, 'CRPIX3', item%crpix3)
    call json_add_string(json, 'CUNIT3', trim(item%cunit3))
    call json_add_string(json, 'CTYPE3', trim(item%ctype3))

    call json_add_real_number(json, 'BMAJ', item%bmaj)
    call json_add_real_number(json, 'BMIN', item%bmin)
    call json_add_real_number(json, 'BPA', item%bpa)

    call json_add_string(json, 'BUNIT', trim(item%bunit))
    call json_add_string(json, 'BTYPE', trim(item%btype))
    call json_add_string(json, 'SPECSYS', trim(item%specsys))

    call json_add_real_number(json, 'RESTFRQ', item%restfrq)
    call json_add_real_number(json, 'OBSRA', item%obsra)
    call json_add_real_number(json, 'OBSDEC', item%obsdec)

    call json_add_string(json, 'OBJECT', trim(item%object))
    call json_add_string(json, 'DATEOBS', trim(item%date_obs))
    call json_add_string(json, 'TIMESYS', trim(item%timesys))
    call json_add_string(json, 'LINE', trim(item%line))
    call json_add_string(json, 'FILTER', trim(item%filter))

    ! statistics (image histogram)
    call json_add_integer_array(json, 'histogram', item%hist)
    ! print *, item%hist

    ! remove the last comma
    str_len = len(json)

    if (extract(json, str_len) .eq. ',') json = remove(json, str_len)

    json = char(json)//'}'

    print *, char(json)
    print *, 'JSON length:', len(json)
    ! print *, 'header size:', size(item%hdr), 'flux:', trim(item%flux)
end subroutine get_json_var

subroutine get_json_str(item, json)
    use json_for
    implicit none

    type(dataset), pointer, intent(in) :: item
    character(len=:), allocatable, intent(inout) :: json

    print *, item%hist

    json = '{'

    ! call json_test(json, 'width', 1011)

    ! call json_add_integer_number(json, 'width', 1011)
    ! call json_add_integer_number(json, 'width', item%naxes(1))
    ! call json_add_integer_number(json, 'height', item%naxes(2))
    ! call json_add_integer_number(json, 'depth', item%naxes(3))
    ! call json_add_integer_number(json, 'polarisation', item%naxes(4))

    print *, item%hist, 'size:', size(item%hist)
    print *, json

end subroutine get_json_str
