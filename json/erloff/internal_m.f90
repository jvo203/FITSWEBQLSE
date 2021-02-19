module erloff_internal_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_error_m, only: error_t, error_is_type
    use erloff_message_m, only: message_t
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), var_str
    use strff, only: hanging_indent, NEWLINE

    implicit none
    private
    public :: internal_t, INTERNAL

    type, extends(error_t) :: internal_t
        private
        type(call_stack_t) :: call_stack_
        type(varying_string) :: message_
        type(message_type_t) :: message_type_
    contains
        private
        procedure, public :: call_stack
        procedure, public :: message
        procedure, public :: message_type
        procedure, public :: with_names_prepended
        procedure, public :: type_string
        procedure, public :: repr
        procedure, public :: is_type
    end type

    interface internal_t
        module procedure generic_internal_c
        module procedure generic_internal_s
        module procedure internal_with_type_c
        module procedure internal_with_type_s
    end interface

    character(len=*), parameter :: INTERNAL_TYPE_STRING = "internal_t"
    type(message_type_t), parameter :: INTERNAL = message_type_t( &
            INTERNAL_TYPE_STRING, .true.)
contains
    pure function generic_internal_c(module_, procedure_, message) result(internal_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(internal_t) :: internal_

        internal_ = internal_t(INTERNAL, module_, procedure_, var_str(message))
    end function

    pure function generic_internal_s(module_, procedure_, message) result(internal_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(varying_string), intent(in) :: message
        type(internal_t) :: internal_

        internal_ = internal_t(INTERNAL, module_, procedure_, message)
    end function

    pure function internal_with_type_c(type_tag, module_, procedure_, message) result(internal_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(internal_t) :: internal_

        internal_ = internal_t(type_tag, module_, procedure_, var_str(message))
    end function

    pure function internal_with_type_s(type_tag, module_, procedure_, message) result(internal_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(varying_string), intent(in) :: message
        type(internal_t) :: internal_

        internal_= internal_constructor( &
                type_tag, call_stack_t(module_, procedure_), message)
    end function

    pure function internal_constructor(type_tag, call_stack, message) result(internal_)
        type(message_type_t), intent(in) :: type_tag
        type(call_stack_t), intent(in) :: call_stack
        type(varying_string), intent(in) :: message
        type(internal_t) :: internal_

        internal_%message_type_ = type_tag
        internal_%call_stack_ = call_stack
        internal_%message_ = message
    end function

    function with_names_prepended(self, module_, procedure_) result(new_message)
        class(internal_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        class(message_t), allocatable :: new_message

        new_message = internal_constructor( &
                self%message_type_, &
                self%call_stack_%with_names_prepended(module_, procedure_), &
                self%message_)
    end function

    pure function call_stack(self)
        class(internal_t), intent(in) :: self
        type(call_stack_t) :: call_stack

        call_stack = self%call_stack_
    end function

    pure function message(self)
        class(internal_t), intent(in) :: self
        type(varying_string) :: message

        message = self%message_
    end function

    pure function message_type(self)
        class(internal_t), intent(in) :: self
        type(message_type_t) :: message_type

        message_type = self%message_type_
    end function

    pure function type_string(self) result(string)
        class(internal_t), intent(in) :: self
        type(varying_string) :: string

        associate(a => self)
        end associate

        string = "IE: "
    end function

    pure function repr(self)
        class(internal_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                'internal_t(' // NEWLINE &
                    // 'call_stack_ = ' // self%call_stack_%repr() // ',' // NEWLINE &
                    // 'message_type = ' // self%message_type_%repr() // ',' // NEWLINE &
                    // 'message_ = "' // self%message_ // '"', &
                4) // NEWLINE // ')'
    end function

    pure function is_type(self, type_tag)
        class(internal_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == INTERNAL_TYPE_STRING) then
            is_type = .true.
        else
            is_type = error_is_type(self, type_tag)
        end if
    end function
end module
