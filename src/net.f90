module net
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
        ! use :: json_module
        use, intrinsic :: iso_c_binding
        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: uri
        integer :: i
        integer(kind=4), parameter :: MPI_URI = 1000
        integer :: size, ierror
        integer :: length
        character, dimension(1024) :: filepath

        ! a JSON message
        ! type(json_file)            :: msg

        ! call msg%initialize()

        ! call msg%destroy()

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

    subroutine image_spectrum_request(datasetId, n, width, height) bind(C)
        use mpi
        use fits
        ! use :: json_module
        use, intrinsic :: iso_c_binding
        integer(kind=c_size_t), intent(in), value :: n
        character(kind=c_char), dimension(n), intent(in) :: datasetId
        integer(kind=c_int), intent(in), value :: width, height

        integer inner_width, inner_height
        integer img_width, img_height
        real scale

        if (n .lt. 1) return

        print *, '"', datasetId, '", width', width, ', height', height

        ! compare the datasetId with item%frameid
        if (.not. compare_frameid(trim(item%frameid), datasetId, int(n))) then
            print *, 'dataset ids do not match: (', datasetId, ') .ne. (', trim(item%frameid), ')'
            return
        end if

        ! dataset ids match, we can proceed with downsizing the image

        ! get the inner image bounding box (excluding NaNs)
        call inherent_image_dimensions(inner_width, inner_height)

        ! get the downscaled image dimensions
        scale = get_image_scale(width, height, inner_width, inner_height)

        ! disable downscaling for now
        ! compress the original size first
        ! and decompress/display it in the web browser
        scale = 1.0

        if (scale .lt. 1.0) then
            img_width = scale*item%naxes(1)
            img_height = scale*item%naxes(2)
        else
            img_width = item%naxes(1)
            img_height = item%naxes(2)
        end if

        print *, 'scale = ', scale, 'image dimensions:', img_width, 'x', img_height

    end subroutine image_spectrum_request

end module net
