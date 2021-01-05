program main
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int) :: status

    print *, 'FITSWEBQL SE CLUSTER EDITION'
    print *, 'POWERED BY FORTRAN 2018'

    ! start an external libmicrohttpd server
    call start_http(status)
end program main
