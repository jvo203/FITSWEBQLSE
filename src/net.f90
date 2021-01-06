module net
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
end module net
