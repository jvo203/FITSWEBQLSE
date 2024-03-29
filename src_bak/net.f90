module net
    ! use iso_fortran_env
    use fits
    use, intrinsic :: iso_c_binding
    implicit none

    character, dimension(1024) :: command
    integer(kind=4), parameter :: MPI_CMD = 1000
    type(gmutex), target :: mpi_mtx

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

        subroutine write_json(fd, json) BIND(C, name='write_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd
            type(C_PTR), value :: json
        end subroutine write_json

        ! void delete_json(GString *json)
        subroutine delete_json(json) BIND(C, name='delete_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
        end subroutine delete_json

        subroutine write_header(fd, json_str, str_len) BIND(C, name='write_header')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd, str_len
            character(kind=c_char), intent(in) :: json_str(*)
        end subroutine write_header

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
        ! call MPI_ABORT(MPI_COMM_WORLD, 0, ierror)

        if (mpi_mtx%i .ne. 0) call g_mutex_clear(c_loc(mpi_mtx))

        stop
    end subroutine exit_fortran

    subroutine fitswebql_request(uri, n) bind(C)
        use mpi
        use fits
        use, intrinsic :: iso_c_binding
        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: uri
        integer :: i
        integer :: size, ierror
        integer :: length
        character, dimension(1024) :: filepath
        character(len=1024) :: filename

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

        ! init mutex
        if (mpi_mtx%i .eq. 0) call g_mutex_init(c_loc(mpi_mtx))

        ! lock the mutex
        call g_mutex_lock(c_loc(mpi_mtx))

        ! send the command
        call MPI_BCAST(filepath, 1024, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierror)

        filename = ''
        do i = 1, n
            filename(i:i) = uri(i)
        end do

        call load_fits_file(filename)

        ! unlock the mutex
        call g_mutex_unlock(c_loc(mpi_mtx))

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
            req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp, ', fd:', req%fd

        write (buffer, 10) 'S', req%dx, ' ', req%image, ' ', req%quality, ' ', req%x1, ' ', req%y1, ' ', req%x2, ' ', req%y2, ' ',&
        &req%width, ' ', req%height, ' ', req%beam, ' ', req%intensity, ' ', req%frame_start, ' ',&
        &req%frame_end, ' ', req%ref_freq, ' ', req%seq_id, ' ', req%timestamp, ' ', req%fd, ' '

        ! init mutex
        if (mpi_mtx%i .eq. 0) call g_mutex_init(c_loc(mpi_mtx))

        ! lock the mutex
        call g_mutex_lock(c_loc(mpi_mtx))

        cmd = ' '

        length = len(trim(buffer)) + 1

        ! add the command
        do concurrent(i=1:length)
            cmd(i) = buffer(i:i)
        end do

        ! append the datasetid
        cmd(length + 1:length + 1 + n) = datasetid(1:n)

        ! send the command
        call MPI_BCAST(cmd, 1024, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierror)

        call handle_realtime_image_spectrum(cmd(2:length + n))

        ! unlock the mutex
        call g_mutex_unlock(c_loc(mpi_mtx))

        return

10      format(a1, i0, a1, l1, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1, i0, a1,&
                         & (G24.16), a1, (G24.16), a1, (G24.16), a1, i0, a1, (G24.16), a1, i0, a1)
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
        use, intrinsic :: iso_c_binding
        implicit none

        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item
        integer(kind=c_int), intent(in), value :: width, height, precision, fetch_data, fd

        real(kind=c_float), dimension(:, :), allocatable, target :: pixels
        logical(kind=c_bool), dimension(:, :), allocatable, target :: mask

        type(C_PTR) :: json

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

            ! JSON string
            json = get_json(item)
            call write_json(fd, json)
            call delete_json(json)

            ! FITS header
            if (allocated(item%hdr)) then
                print *, 'FITS header size:', size(item%hdr)
                ! print *, item%hdr
                call write_header(fd, item%hdr, size(item%hdr))
                ! call write_header(fd, 'NULL'//c_null_char)
            else
                call write_header(fd, 'NULL', 4)
            end if

            ! send FPzip-compressed spectra
            print *, 'calling image_spectrum_request subroutine'

            ! mean spectrum
            if (allocated(item%mean_spectrum)) then
                call write_spectrum(fd, c_loc(item%mean_spectrum), size(item%mean_spectrum), FPZIP_HIGH_PRECISION)
            end if

            ! integrated spectrum
            if (allocated(item%integrated_spectrum)) then
                call write_spectrum(fd, c_loc(item%integrated_spectrum), size(item%integrated_spectrum), FPZIP_HIGH_PRECISION)
            end if
        end if

    end subroutine image_spectrum_request

end module net
