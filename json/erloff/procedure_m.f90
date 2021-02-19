module erloff_procedure_m
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), operator(==)

    implicit none
    private
    public :: procedure_t

    type :: procedure_t
        private
        type(varying_string) :: name
    contains
        private
        procedure :: equals
        generic, public :: operator(==) => equals
        procedure, public :: to_string
        procedure, public :: repr
    end type

    interface procedure_t
        module procedure procedure_c
        module procedure procedure_s
    end interface
contains
    pure function procedure_c(name) result(procedure_)
        character(len=*), intent(in) :: name
        type(Procedure_t) :: procedure_

        procedure_%name = name
    end function

    pure function procedure_s(name) result(procedure_)
        type(varying_string), intent(in) :: name
        type(procedure_t) :: procedure_

        procedure_%name = name
    end function

    pure function equals(lhs, rhs)
        class(Procedure_t), intent(in) :: lhs
        type(Procedure_t), intent(in) :: rhs
        logical :: equals

        equals = lhs%name == rhs%name
    end function

    pure function to_string(self) result(string)
        class(Procedure_t), intent(in) :: self
        type(varying_string) :: string

        string = self%name
    end function

    pure function repr(self)
        class(Procedure_t), intent(in) :: self
        type(varying_string) :: repr

        repr = 'procedure_t("' // self%name // '")'
    end function repr
end module
