module net
    interface
        subroutine start_http(status) BIND(C, name='start_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
            integer(C_INT) :: status
        end subroutine start_http
    end interface
end module net
