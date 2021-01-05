module net
    interface
        subroutine cvtf32to16(src, dst, size) BIND(C, name='cvtf32to16')
            use, intrinsic :: ISO_C_BINDING
            implicit none
            type(C_PTR), value :: src
            type(C_PTR), value :: dst
            integer(C_INT), value :: size
        end subroutine cvtf32to16

        subroutine cvtf32to16minmax(src, dst, dmin, dmax, size) BIND(C, name='cvtf32to16minmax')
            use, intrinsic :: ISO_C_BINDING
            implicit none
            type(C_PTR), value :: src
            type(C_PTR), value :: dst
            real(C_FLOAT) :: dmin
            real(C_FLOAT) :: dmax
            integer(C_INT), value :: size
        end subroutine cvtf32to16minmax

        subroutine cvtf16to32(src, dst, size) BIND(C, name='cvtf16to32')
            use, intrinsic :: ISO_C_BINDING
            implicit none
            type(C_PTR), value :: src
            type(C_PTR), value :: dst
            integer(C_INT), value :: size
        end subroutine cvtf16to32
    end interface
end module net
