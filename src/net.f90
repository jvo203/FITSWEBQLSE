module net
    interface
        subroutine start_http() BIND(C, name='start_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end subroutine start_http
    end interface
end module net
