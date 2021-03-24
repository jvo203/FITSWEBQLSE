module net
    ! use iso_fortran_env
    use :: zmq
    use, intrinsic :: iso_c_binding
    implicit none

    ! ØMQ
    integer :: rc
    type(c_ptr)     :: server_context, client_context
    type(c_ptr)     :: server_socket, client_socket

    interface
        subroutine start_http() BIND(C, name='start_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine start_http

        subroutine stop_http() BIND(C, name='stop_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine stop_http

        subroutine init_hash_table() BIND(C, name='init_hash_table')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine init_hash_table

        subroutine delete_hash_table() BIND(C, name='delete_hash_table')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine delete_hash_table

        subroutine write_image_spectrum(fd, flux, &
            pmin, pmax, pmedian, &
            &black, white, sensitivity, ratio_sensitivity,&
            &width, height, precision,&
            &pixels, mask)&
            &BIND(C, name='write_image_spectrum')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: flux(*)
            integer(c_int), value, intent(in) :: fd, width, height, precision
            real(kind=c_float), value, intent(in) :: pmin, pmax, pmedian
            real(kind=c_float), value, intent(in) :: black, white
            real(kind=c_float), value, intent(in) :: sensitivity, ratio_sensitivity
            type(C_PTR), value :: pixels, mask
        end subroutine write_image_spectrum

        subroutine write_header(fd, json_str) BIND(C, name='write_header')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd
            character(kind=c_char), intent(in) :: json_str(*)
        end subroutine write_header

        subroutine write_spectrum(fd, spectrum, n) BIND(C, name='write_spectrum')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd, n
            type(C_PTR), value, intent(in) :: spectrum
        end subroutine write_spectrum

        ! resizeCubic(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight)
        subroutine resizeCubic(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeCubic')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeCubic

        ! resizeLanczos(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight, int numLobes)
        subroutine resizeLanczos(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight, numLobes) BIND(C, name='resizeLanczos')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
            integer(c_int), value, intent(in) :: numLobes
        end subroutine resizeLanczos

        ! resizeSuper(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight)
        subroutine resizeSuper(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeSuper')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeSuper

        ! resizeNearest(Ipp8u *pSrc, int srcWidth, int srcHeight, Ipp8u *pDest, int dstWidth, int dstHeight)
        subroutine resizeNearest(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeNearest')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeNearest
    end interface

    ! integer(atomic_int_kind) :: event_count
    ! type(event_type) :: event_count[*]
contains
    subroutine sigint_handler
        print *, 'Process interrupted(SIGINT), exiting...'

        ! shutdown any active http/ws servers
        call stop_http

        call exit_fortran
    end subroutine sigint_handler

    subroutine exit_fortran() bind(c)
        use mpi

        integer msg, ierror

        print *, 'image', this_image(), 'FORTRAN EXIT'

        ! release the hash table
        call delete_hash_table

        ! print '(a)', '[ØMQ] Terminating ...'
        ! rc = zmq_close(client_socket)
        ! rc = zmq_ctx_term(client_context)

        ! if (this_image() .eq. 1) then
        !    rc = zmq_close(server_socket)
        !    rc = zmq_ctx_term(server_context)
        !end if

        ! in a Co-Array program there may be no need for MPI_Finalize
        call MPI_FINALIZE(ierror)

        stop
    end subroutine exit_fortran

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

    subroutine fitswebql_request(uri, n) bind(C)
        use mpi
        use fits
        use, intrinsic :: iso_c_binding
        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: uri
        integer :: i
        integer(kind=4), parameter :: MPI_CMD = 1000
        integer :: size, ierror
        integer :: length
        character, dimension(1024) :: filepath

        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

        ! if (this_image() == 1) then
        length = n
        print *, 'image', this_image(), 'received an http request uri: ', uri, length
        !end if

        if (n .lt. 1) return

        ! send the uri to all the images via MPI
        ! MPI_PROBE not available in Intel MPI, switching over to co-arrays ?
        ! or use a fixed-size character array (1024 characters?)

        ! first initialise the whole string with space characters
        ! so that the end of the string can be found later on
        filepath = ' '

        ! add the command
        filepath(1:1) = 'L'

        ! add the argument
        filepath(2:1 + n) = uri(1:n)

        ! event post(event_count)

        ! call send_command(server_socket, filepath(1:n))

        do i = 0, size - 1
            call MPI_SEND(filepath, 1024, MPI_CHARACTER, i, MPI_CMD, MPI_COMM_WORLD, ierror)
        end do
    end subroutine fitswebql_request

    subroutine realtime_image_spectrum_request(datasetid, n, ptr) bind(C)
        use, intrinsic :: iso_c_binding
        use mpi
        use fits
        implicit none

        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: datasetid
        type(C_PTR), intent(in), value :: ptr
        type(image_spectrum_request_f), pointer :: req

        integer(kind=4), parameter :: MPI_CMD = 1000
        integer :: size, ierror

        ! an internal file buffer
        character(1024) :: buffer
        integer :: i, length

        ! a command to send
        character, dimension(1024) :: cmd

        call c_f_pointer(ptr, req)

        print *, 'realtime_image_spectrum_request for ', datasetid, ', dx:', req%dx, &
            &', image:', req%image, ', quality:', req%quality, ', x1:', req%x1, &
            &', y1:', req%y1, ', x2:', req%x2, ', y2:', req%y2, ', width:', req%width, &
            &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
            &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
            req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp

        write (buffer, 10) 'S', req%dx, ' ', req%image, ' ', req%quality, ' ', req%x1, ' ', req%y1, ' ', req%x2, ' ', req%y2, ' ',&
        &req%width, ' ', req%height, ' ', req%beam, ' ', req%intensity, ' ', req%frame_start, ' ',&
        &req%frame_end, ' ', req%ref_freq, ' ', req%seq_id, ' ', req%timestamp, ' '        

        cmd = ' '

        length = len(trim(buffer)) + 1

        ! add the command
        do concurrent(i=1:length)
            cmd(i) = buffer(i:i)
        end do

        ! append the datasetid
        cmd(length + 1:length + 1 + n) = datasetid(1:n)

        ! send the command
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

        do i = 0, size - 1
            call MPI_SEND(cmd, 1024, MPI_CHARACTER, i, MPI_CMD, MPI_COMM_WORLD, ierror)
        end do

        return

10      format(a1, i0, a1, l1, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1,&
     & (G24.16), a1, (G24.16), a1, (G24.16), a1, i0, a1, (G24.16), a1)
    end subroutine realtime_image_spectrum_request

    function compare_frameid(frameid, datasetId)
        use, intrinsic :: iso_c_binding
        character(kind=c_char), dimension(:), intent(in) :: frameid, datasetId
        logical compare_frameid
        integer i, n

        n = size(datasetId)

        ! the array sizes do not match, aborting
        if (n .ne. size(frameid)) then
            compare_frameid = .false.
            return
        end if

        ! default value
        compare_frameid = .true.

        do i = 1, n
            if (frameid(i) .ne. datasetId(i)) then
                compare_frameid = .false.
                return
            end if
        end do

    end function compare_frameid

    subroutine image_spectrum_request(item_ptr, width, height, precision, fetch_data, fd) bind(C)
        use mpi
        use fits
        ! use json_module
        use iso_varying_string
        use, intrinsic :: iso_c_binding
        implicit none

        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item
        integer(kind=c_int), intent(in), value :: width, height, precision, fetch_data, fd

        real(kind=c_float), dimension(:, :), allocatable, target :: pixels
        logical(kind=c_bool), dimension(:, :), allocatable, target :: mask

        type(varying_string) :: json_str

        integer inner_width, inner_height
        integer img_width, img_height
        real scale

        ! timing
        real :: t1, t2

        call c_f_pointer(item_ptr, item)

        print *, '"', item%datasetId, '", width', width, ', height', height, ', precision', precision,&
        & ', fetch_data', fetch_data, ', pipe write end', fd

        ! get the inner image bounding box (excluding NaNs)
        call inherent_image_dimensions(item, inner_width, inner_height)

        ! get the downscaled image dimensions
        scale = get_image_scale(width, height, inner_width, inner_height)

        if (scale .lt. 1.0) then
            img_width = scale*item%naxes(1)
            img_height = scale*item%naxes(2)

            allocate (pixels(img_width, img_height))
            allocate (mask(img_width, img_height))

            ! downscale item%pixels and item%mask into pixels, mask

            call cpu_time(t1)

            if (scale .gt. 0.2) then
                call resizeLanczos(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), img_width, img_height, 3)
            else
                call resizeSuper(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), img_width, img_height)
            end if

            call cpu_time(t2)

            print *, 'resize pixels elapsed time:', 1000*(t2 - t1), '[ms]'

            ! Boolean mask: the naive Nearest-Neighbour method

            call cpu_time(t1)

            call resizeNearest(c_loc(item%mask), item%naxes(1), item%naxes(2), c_loc(mask), img_width, img_height)

            call cpu_time(t2)

            print *, 'resize mask elapsed time:', 1000*(t2 - t1), '[ms]'

            call write_image_spectrum(fd, trim(item%flux)//c_null_char,&
                &item%pmin, item%pmax, item%pmedian,&
                &item%black, item%white, item%sensitivity, item%ratio_sensitivity,&
                & img_width, img_height, precision, c_loc(pixels), c_loc(mask))

            deallocate (pixels)
            deallocate (mask)

        else
            img_width = item%naxes(1)
            img_height = item%naxes(2)

            call write_image_spectrum(fd, trim(item%flux)//c_null_char,&
                &item%pmin, item%pmax, item%pmedian,&
                &item%black, item%white, item%sensitivity, item%ratio_sensitivity,&
                & img_width, img_height, precision, c_loc(item%pixels), c_loc(item%mask))

        end if

        print *, 'scale = ', scale, 'image dimensions:', img_width, 'x', img_height

        if (fetch_data .eq. 1) then
            call get_json(item, json_str)

            ! call to_json(str_val)

            ! str_len = len(str_val)
            ! print *, 'json len:', str_len

            ! allocate (character(str_len) :: c_str)

            ! do concurrent(k=1:str_len)
            !    c_str(k:k) = str_val(k:k)
            ! end do

            ! json
            ! call write_header(fd, c_str//c_null_char)

            call write_header(fd, char(json_str//c_null_char))

            ! FITS header
            if (allocated(item%hdr)) then
                print *, 'FITS header size:', size(item%hdr)
                ! print *, item%hdr
                call write_header(fd, item%hdr)
                ! call write_header(fd, 'NULL'//c_null_char)
            else
                call write_header(fd, 'NULL'//c_null_char)
            end if

            ! send FPzip-compressed spectra

            ! mean spectrum
            if (allocated(item%mean_spectrum)) then
                call write_spectrum(fd, c_loc(item%mean_spectrum), size(item%mean_spectrum))
            end if

            ! integrated spectrum
            if (allocated(item%integrated_spectrum)) then
                call write_spectrum(fd, c_loc(item%integrated_spectrum), size(item%integrated_spectrum))
            end if
        end if

    end subroutine image_spectrum_request

end module net
