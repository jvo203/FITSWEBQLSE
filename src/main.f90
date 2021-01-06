program main
    use, intrinsic :: iso_c_binding
    implicit none
    character(len=1) :: c

    print *, 'FITSWEBQL SE CLUSTER EDITION'
    print *, 'POWERED BY FORTRAN 2018'

    ! start an external libmicrohttpd server
    call start_http

    read *, c

end program main
