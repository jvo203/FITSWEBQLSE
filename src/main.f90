program main
    use mpi
    use net
    use fits
    use omp_lib
    use, intrinsic :: iso_c_binding
    implicit none

    integer :: max_threads
    integer rank, size, ierror, tag, namelen, status(MPI_STATUS_SIZE)
    character(len=MPI_MAX_PROCESSOR_NAME) :: name
    integer(kind=4), parameter :: MPI_URI = 1000
    logical init

    ! receives the URI of the FITS file
    ! character(kind=c_char), dimension(:), allocatable :: uri
    character, dimension(1024) :: filepath

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
    if (this_image() == 1) call start_http

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

            call MPI_RECV(filepath, 1024, MPI_CHARACTER, MPI_ANY_SOURCE, MPI_URI, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)

            if (ierror .eq. 0) then
                count = length(filepath, 1024)

                if (count .gt. 0) then
                    print *, 'rank', rank, 'filepath:>', filepath(1:count), '<'

                    do i = 1, count
                        filename(i:i) = filepath(i)
                    end do

                    call load_fits_file(filename)
                end if
            end if

            !   deallocate (uri)
        end block
    end do

    ! release the hash table
    call delete_hash_table

    ! in a Co-Array program there may be no need for MPI_Finalize
    call MPI_FINALIZE(ierror)
contains
    integer function length(string, n)
        character, dimension(n), intent(in) :: string
        integer, intent(in) :: n
        integer :: i

        if (n .le. 0) then
            length = 0
            return
        end if

        do i = 1, n
            if (string(i) .eq. ' ') then
                length = i - 1
                return
            end if
        end do

        length = n
    end function length
end program main
