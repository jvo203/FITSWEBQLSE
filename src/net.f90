module net
    interface
        integer(C_INT) function start_http() BIND(C, name='start_http')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end function start_http
    end interface
end module net
