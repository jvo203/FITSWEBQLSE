program main
    use, intrinsic :: iso_c_binding
    use mpi
    implicit none

    character(len=1) :: c
    external sigint_handler ! Must declare as external
    integer rank, size, ierror, tag, status(MPI_STATUS_SIZE), cmd
    integer, parameter :: SIGINT = 2

    ! call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    if (this_image() == 1) then
        print *, 'image', this_image(), 'node', rank, '/', size
    end if

    print *, 'FITSWEBQL SE CLUSTER EDITION POWERED BY FORTRAN 2018'

    call signal(SIGINT, sigint_handler)

    cmd = 0
    ! start an external libmicrohttpd server
    if (this_image() == 1) then
        call start_http
    end if

    do
        call MPI_RECV(cmd, 1, MPI_INT, rank, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        print *, 'image', this_image(), 'received message containing: ', cmd

        if (cmd .ge. 0) exit
    end do

    call MPI_FINALIZE(ierror)
end program main

subroutine http_request
    integer msg

    print *, 'image', this_image(), 'received an http request.'

    ! if(this_image() == 1) then
    msg = 1
    call MPI_SEND(msg, 1, MPI_INT, rank, 1, MPI_COMM_WORLD, ierror)
    print *, 'image', this_image(), 'ierror:', ierror
    ! end if
end subroutine http_request

subroutine exit_fortran
    integer msg

    ! if(this_image() == 1) then
    msg = -1
    call MPI_SEND(msg, 1, MPI_INT, rank, 1, MPI_COMM_WORLD, ierror)
    ! end if

    print *, 'image', this_image(), 'FORTRAN EXIT'
end subroutine exit_fortran

subroutine sigint_handler
    print *, 'Process interrupted(SIGINT), exiting...'

    ! shutdown any active http server
    call stop_http

    return
end subroutine sigint_handler
