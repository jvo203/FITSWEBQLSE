module classifier
    use, intrinsic :: ISO_C_BINDING
    implicit none
    interface
        integer(c_int) function histogram_classifier(Slots) BIND(C, name='histogram_classifier')
            use, intrinsic :: ISO_C_BINDING

            type(C_PTR), value :: Slots
        end function histogram_classifier
    end interface

end module classifier
