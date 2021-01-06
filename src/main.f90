program main
    use, intrinsic :: iso_c_binding
    use mpi
    implicit none

    character(len=1) :: c

    integer rank, size, ierror, tag, status(MPI_STATUS_SIZE)

    ! call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    if (this_image() == 1) then
        print *, 'image', this_image(), 'node', rank, '/', size
    end if

    print *, 'FITSWEBQL SE CLUSTER EDITION'
    print *, 'POWERED BY FORTRAN 2018'

    ! start an external libmicrohttpd server
    call start_http

    read *, c

    ! call MPI_FINALIZE(ierror)
end program main

subroutine exit_fortran
    print *, 'EXIT'
end subroutine exit_fortran
