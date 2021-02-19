module erloff_module_m
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), operator(==)

    implicit none
    private
    public :: module_t

    type :: module_t
        private
        type(varying_string) :: name
    contains
        private
        procedure :: equals
        generic, public :: operator(==) => equals
        procedure, public :: to_string
        procedure, public :: repr
    end type

    interface module_t
        module procedure module_c
        module procedure module_s
    end interface
contains
    pure function module_c(name) result(module_)
        character(len=*), intent(in) :: name
        type(module_t) :: module_

        module_%name = name
    end function

    pure function module_s(name) result(module_)
        type(varying_string), intent(in) :: name
        type(module_t) :: module_

        module_%name = name
    end function

    pure function equals(lhs, rhs)
        class(module_t), intent(in) :: lhs
        type(module_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%name == rhs%name
    end function

    pure function to_string(self) result(string)
        class(module_t), intent(in) :: self
        type(varying_string) :: string

        string = self%name
    end function

    pure function repr(self)
        class(module_t), intent(in) :: self
        type(varying_string) :: repr

        repr = 'module_t("' // self%name // '")'
    end function
end module
