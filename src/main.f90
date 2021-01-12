program main
    use mpi
    use, intrinsic :: iso_c_binding
    implicit none

    character(len=1) :: c
    external sigint_handler ! Must declare as external
    integer rank, size, ierror, tag, status(MPI_STATUS_SIZE), cmd
    integer(kind=4), parameter :: MPI_CMD = 1000
    logical init

    call MPI_Initialized(init, ierror)
    if (.not. init) call MPI_Init(ierror)

    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    ! if (this_image() == 1) then
    print *, 'image', this_image(), 'rank', rank, ':: world size =', size
    ! end if

    print *, 'FITSWEBQL SE CLUSTER EDITION POWERED BY FORTRAN 2018'

    call register_kill_signal_handler(sigint_handler)

    cmd = 0
    ! start an external libmicrohttpd server
    ! if (this_image() == 1)
    ! if (rank .eq. 0)
    call start_http

    do
        ! if (rank .ne. 0) then
        ! call MPI_BCAST(cmd, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
        call MPI_RECV(cmd, 1, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        print *, 'rank', rank, 'received message containing', cmd, ', ierror', ierror
        ! end if
        if (cmd .lt. 0) exit
    end do

    ! in a Co-Array program there may be no need for MPI_Finalize
    call MPI_FINALIZE(ierror)
end program main

subroutine http_request
    use mpi
    integer :: msg
    integer :: i
    integer(kind=4), parameter :: MPI_CMD = 1000
    integer :: size, ierror

    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

    ! if (this_image() == 1) then
    print *, 'image', this_image(), 'received an http request.'
    !end if

    ! if(this_image() == 1) then
    msg = 777

    ! call MPI_BCAST(msg, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)

    do i = 0, size - 1
        call MPI_SEND(msg, 1, MPI_INTEGER, i, MPI_CMD, MPI_COMM_WORLD, ierror)
    end do

    !print *, 'image', this_image(), 'ierror:', ierror
    ! end if
end subroutine http_request

subroutine exit_fortran
    integer msg

    print *, 'image', this_image(), 'FORTRAN EXIT'

    stop
end subroutine exit_fortran

subroutine sigint_handler
    print *, 'Process interrupted(SIGINT), exiting...'

    ! shutdown any active http/ws servers
    call stop_http

    call exit_fortran
end subroutine sigint_handler
