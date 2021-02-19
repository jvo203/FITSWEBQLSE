module erloff_call_stack_entry_m
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, operator(//)
    use strff, only: hanging_indent, indent, NEWLINE

    implicit none
    private
    public :: call_stack_entry_t

    type :: call_stack_entry_t
        private
        type(module_t) :: module_
        type(procedure_t) :: procedure_
    contains
        private
        procedure, public :: to_string
        procedure :: is_from_module
        procedure :: is_from_procedure
        generic, public :: operator(.isFrom.) => &
                is_from_module, is_from_procedure
        procedure, public :: repr
    end type

    interface call_stack_entry_t
        module procedure constructor
    end interface
contains
    pure function constructor(module_, procedure_) result(entry_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(call_stack_entry_t) :: entry_

        entry_%module_ = module_
        entry_%procedure_ = procedure_
    end function

    elemental function to_string(self) result(string)
        class(call_stack_entry_t), intent(in) :: self
        type(varying_string) :: string

        string = self%module_%to_string() // "." // self%procedure_%to_string()
    end function

    elemental function is_from_module(self, module_)
        class(call_stack_entry_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: is_from_module

        is_from_module = self%module_ == module_
    end function

    elemental function is_from_procedure(self, procedure_)
        class(call_stack_entry_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: is_from_procedure

        is_from_procedure = self%procedure_ == procedure_
    end function

    elemental function repr(self)
        class(call_stack_entry_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                "call_stack_entry_t(" // NEWLINE &
                    // "module = " // self%module_%repr() // "," // NEWLINE &
                    // "procedure = " // self%procedure_%repr(), &
                4) // NEWLINE // ")"
    end function
end module
