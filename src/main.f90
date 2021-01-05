program main
    use, intrinsic :: iso_c_binding
    implicit none

    print *, 'FITSWEBQL SE CLUSTER EDITION'
    print *, 'POWERED BY FORTRAN 2018'

    ! start an external libmicrohttpd server
    call start_http

end program main
