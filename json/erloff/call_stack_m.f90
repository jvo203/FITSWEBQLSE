module erloff_call_stack_m
    use erloff_call_stack_entry_m, only: call_stack_entry_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, operator(//)
    use strff, only: hanging_indent, indent, join, NEWLINE

    implicit none
    private
    public :: call_stack_t

    type :: call_stack_t
        private
        type(call_stack_entry_t), allocatable :: entries(:)
    contains
        private
        procedure, public :: with_names_prepended
        procedure, public :: to_string
        procedure :: originated_from_module
        procedure :: originated_from_procedure
        generic, public :: operator(.originatedFrom.) => &
                originated_from_module, originated_from_procedure
        procedure :: includes_module
        procedure :: includes_procedure
        generic, public :: operator(.includes.) => &
                includes_module, includes_procedure
        procedure, public :: repr
    end type

    interface call_stack_t
        module procedure constructor
    end interface
contains
    pure function constructor(module_, procedure_) result(call_stack)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(call_stack_t) :: call_stack

        call_stack = internal_constructor([call_stack_entry_t(module_, procedure_)])
    end function

    pure function internal_constructor(entries) result(call_stack)
        type(call_stack_entry_t), intent(in) :: entries(:)
        type(call_stack_t) :: call_stack

        allocate(call_stack%entries, source = entries)
    end function

    pure function with_names_prepended(self, module_, procedure_) result(new_stack)
        class(call_stack_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(call_stack_t) :: new_stack

        new_stack = internal_constructor(&
                [call_stack_entry_t(module_, procedure_), self%entries])
    end function

    pure function to_string(self) result(string)
        class(call_stack_t), intent(in) :: self
        type(varying_string) :: string

        string = join(self%entries%to_string(), "->")
    end function

    pure function originated_from_module(self, module_)
        class(call_stack_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: originated_from_module

        originated_from_module = self%entries(size(self%entries)).isFrom.module_
    end function

    pure function originated_from_procedure(self, procedure_)
        class(call_stack_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: originated_from_procedure

        originated_from_procedure = self%entries(size(self%entries)).isFrom.procedure_
    end function

    pure function includes_module(self, module_)
        class(call_stack_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        logical :: includes_module

        includes_module = any(self%entries.isFrom.module_)
    end function

    pure function includes_procedure(self, procedure_)
        class(call_stack_t), intent(in) :: self
        type(procedure_t), intent(in) :: procedure_
        logical :: includes_procedure

        includes_procedure = any(self%entries.isFrom.procedure_)
    end function

    pure function repr(self)
        class(call_stack_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                "call_stack_t(" // NEWLINE &
                    // "entries = [" // NEWLINE &
                    // indent( &
                            join(self%entries%repr(), "," // NEWLINE), &
                            4) // NEWLINE // "]", &
                4) // NEWLINE // ")"
    end function
end module
