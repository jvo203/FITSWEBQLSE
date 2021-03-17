program main
    use mpi
    use net
    use fits
    use omp_lib
    use :: zmq
    use, intrinsic :: iso_c_binding
    implicit none

    integer(kind=4), parameter :: ZMQ_PORT = 50000

    integer :: max_threads
    integer rank, size, ierror, tag, namelen, status(MPI_STATUS_SIZE)
    character(len=MPI_MAX_PROCESSOR_NAME) :: name
    integer(kind=4), parameter :: MPI_URI = 1000
    logical init

    ! receives the URI of the FITS file
    ! character(kind=c_char), dimension(:), allocatable :: uri
    character, dimension(1024) :: cmd

    ! stores the URI length
    integer count

    ! ideally take the number of threads from the command-line argument
    call OMP_SET_NUM_THREADS(max(8, OMP_GET_MAX_THREADS()))

    max_threads = OMP_GET_MAX_THREADS()

    call MPI_INITIALIZED(init, ierror)
    if (.not. init) call MPI_INIT(ierror)

    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    call MPI_GET_PROCESSOR_NAME(name, namelen, ierror)

    print *, 'co-array image:', this_image(), 'mpi rank:', rank, &
    &'mpi world size:', size, 'running on ', trim(name), ' with', max_threads, 'OpenMP thread(s).'

    print *, 'FITSWEBQLSE CLUSTER EDITION POWERED BY FORTRAN 2018'

    call register_kill_signal_handler(sigint_handler)

    ! create a new hash table for storing the datasets
    call init_hash_table

    ! start an external libmicrohttpd server
    if (this_image() .eq. 1) call start_http

    ! start a ØMQ server
    ! if (this_image() .eq. 1) then
    !   print '(a)', '[ØMQ] Starting PUBLISHER ...'

    ! start a ØMQ server
    !  server_context = zmq_ctx_new()
    !  server_socket = zmq_socket(server_context, ZMQ_PUB)
    !  rc = zmq_bind(server_socket, 'tcp://127.0.0.1:50000')
    ! 'inproc://fzmq')
    ! 'tcp://127.0.0.1:50000')

    ! print *, this_image(), '[ØMQ] rc', rc

    ! call sleep(5)
    ! else
    !  call sleep(5)

    ! start a ØMQ client
    ! client_context = zmq_ctx_new()
    ! client_socket = zmq_socket(client_context, ZMQ_SUB)
    ! rc = zmq_connect(client_socket, 'tcp://127.0.0.1:50000')
    ! 'tcp://127.0.0.1:50000')
    ! 'inproc://fzmq')
    ! print *, this_image(), '[ØMQ] rc', rc

    ! Subscribe to all messages
    ! rc = zmq_setsockopt(client_socket, ZMQ_SUBSCRIBE, '')
    ! print *, this_image(), '[ØMQ] ZMQ_SUBSCRIBE::rc', rc
    ! end if

    ! call sleep(5)

    ! send a test message
    ! cmd = 'this is a test'
    ! if (this_image() .eq. 1) call send_command(server_socket, cmd)

    ! ØMQ event loop
    ! do
    ! print *, this_image(), 'calling recv_command'
    !    call recv_command(client_socket)
    !end do

    do
        block
            integer i
            character(len=1024) :: filename

            filename = ''
            ! first probe for new messages
            ! MPI_PROBE does not seem to be available in Intel MPI !? !? !?
            ! try co-arrays
            !    call MPI_PROBE(MPI_ANY_SOURCE, MPI_URI, MPI_COMM_WORLD, ierror)

            ! there is an incoming message, obtain the number of characters
            !   call MPI_GET_COUNT(ierror, MPI_CHARACTER, count)

            ! allocate the character array
            !   allocate (uri(count))

            ! wait for an event
            ! event wait(event_count)
            ! print *, 'image', this_image(), 'received an event'

            call MPI_RECV(cmd, 1024, MPI_CHARACTER, MPI_ANY_SOURCE, MPI_URI, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)

            if (ierror .eq. 0) then
                count = length(cmd, 1024)

                if (count .gt. 1) then
                    print *, 'rank', rank, '"', cmd(1:count), '"'

                    ! decipher the command

                    ! load FITS file requests
                    if (cmd(1) .eq. 'L') then

                        do i = 1, count - 1
                            filename(i:i) = cmd(i + 1)
                        end do

                        call load_fits_file(filename)
                    end if

                    ! WebSocket realtime image/spectrum requests
                    if (cmd(1) .eq. 'S') then
                        count = reverse_length(cmd, 1024)
                        ! print *, 'rank', rank, '"', cmd(2:count), '"'

                        call calculate_realtime_spectrum(cmd(2:count))
                    end if
                end if
            end if

            !   deallocate (uri)
        end block
    end do

contains
    integer function length(string, n)
        character, dimension(n), intent(in) :: string
        integer, intent(in) :: n
        integer :: i

        if (n .le. 0) then
            length = 0
            return
        end if

        ! search until a blank character is found
        do i = 1, n
            if (string(i) .eq. ' ') then
                length = i - 1
                return
            end if
        end do

        length = n
    end function length

    integer function reverse_length(string, n)
        character, dimension(n), intent(in) :: string
        integer, intent(in) :: n
        integer :: i

        if (n .le. 0) then
            reverse_length = 0
            return
        end if

        ! find the first non-blank character from the end
        do i = n, 1, -1
            if (string(i) .ne. ' ') then
                reverse_length = i
                return
            end if
        end do

        reverse_length = 0
    end function reverse_length

end program main
