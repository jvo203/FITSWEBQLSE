module net
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        subroutine start_http() BIND(C, name='start_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine start_http

        subroutine stop_http() BIND(C, name='stop_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine stop_http

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
    end interface
contains
    subroutine sigint_handler
        print *, 'Process interrupted(SIGINT), exiting...'

        ! shutdown any active http/ws servers
        call stop_http

        call exit_fortran
    end subroutine sigint_handler

    subroutine exit_fortran() bind(c)
        integer msg

        print *, 'image', this_image(), 'FORTRAN EXIT'

        stop
    end subroutine exit_fortran

    subroutine fitswebql_request(uri, n) bind(C)
        use mpi
        use fits
        use, intrinsic :: iso_c_binding
        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: uri
        integer :: i
        integer(kind=4), parameter :: MPI_URI = 1000
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
        filepath(1:n) = uri(1:n)

        do i = 0, size - 1
            call MPI_SEND(filepath, 1024, MPI_CHARACTER, i, MPI_URI, MPI_COMM_WORLD, ierror)
        end do
    end subroutine fitswebql_request

    pure function compare_frameid(frameid, datasetId, n)
        use, intrinsic :: iso_c_binding
        CHARACTER(LEN=*), INTENT(IN) :: frameid
        integer, intent(in) :: n
        character(kind=c_char), dimension(n), intent(in) :: datasetId
        logical compare_frameid
        integer i

        if (n .ne. len(frameid)) then
            compare_frameid = .false.
            return
        end if

        ! default value
        compare_frameid = .true.

        do i = 1, n
            if (frameid(i:i) .ne. datasetId(i)) then
                compare_frameid = .false.
                return
            end if
        end do

    end function compare_frameid

    subroutine image_spectrum_request(datasetId, n, width, height, precision, fetch_data, fd) bind(C)
        use mpi
        use fits
        ! use json_module
        use iso_varying_string
        use, intrinsic :: iso_c_binding
        implicit none

        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: datasetId
        integer(kind=c_int), intent(in), value :: width, height, precision, fetch_data, fd

        real(kind=c_float), dimension(:, :), allocatable, target :: pixels
        logical(kind=c_bool), dimension(:, :), allocatable, target :: mask

        ! CHARACTER(kind=json_CK, len=:), allocatable :: str_val
        ! character(kind=c_char, len=:), allocatable :: c_str
        ! integer :: k, str_len

        type(varying_string) :: json_str

        integer inner_width, inner_height
        integer img_width, img_height
        real scale

        if (n .lt. 1) return

        print *, '"', datasetId, '", width', width, ', height', height, ', precision', precision,&
        & ', fetch_data', fetch_data, ', pipe write end', fd

        ! compare the datasetId with item%frameid
        ! item%frameid replaced by item%datasetid
        if (.not. compare_frameid(char(item%datasetid), datasetId, int(n))) then
            print *, 'dataset ids do not match: (', datasetId, ') .ne. (', char(item%datasetid), ')'
            return
        end if

        ! dataset ids match, we can proceed with downsizing the image

        ! get the inner image bounding box (excluding NaNs)
        call inherent_image_dimensions(inner_width, inner_height)

        ! get the downscaled image dimensions
        scale = get_image_scale(width, height, inner_width, inner_height)

        if (scale .lt. 1.0) then
            img_width = scale*item%naxes(1)
            img_height = scale*item%naxes(2)

            allocate (pixels(img_width, img_height))
            allocate (mask(img_width, img_height))

            ! downscale item%pixels and item%mask into pixels, mask
            ! pixels: for now Linear Interpolation
            ! in the long-term pixels should be downscaled with Lanczos
            call downsize_linear(item%pixels, pixels)
            ! call downsize_lanczos_3(item%pixels, pixels)

            ! Boolean mask: the naive Nearest-Neighbour method
            call downsize_mask(item%mask, mask)

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
            call get_json(json_str)

            ! call to_json(str_val)

            ! str_len = len(str_val)
            ! print *, 'json len:', str_len

            ! allocate (character(str_len) :: c_str)

            ! do concurrent(k=1:str_len)
            !    c_str(k:k) = str_val(k:k)
            ! end do

            ! json
            ! call write_header(fd, c_str//c_null_char)

            call write_header(fd, char(json_str)//c_null_char)

            ! FITS header
            call write_header(fd, char(item%hdr)//c_null_char)

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
