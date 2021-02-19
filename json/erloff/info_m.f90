module erloff_info_m
    use erloff_call_stack_m, only: call_stack_t
    use erloff_message_m, only: message_t, default_is_type
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), var_str
    use strff, only: hanging_indent, NEWLINE

    implicit none
    private
    public :: info_t, INFO

    type, extends(message_t) :: info_t
        private
        type(call_stack_t) :: call_stack_
        type(varying_string) :: message_
        type(message_type_t) :: message_type_
    contains
        private
        procedure, public :: message
        procedure, public :: call_stack
        procedure, public :: message_type
        procedure, public :: with_names_prepended
        procedure, public :: type_string
        procedure, public :: repr
        procedure, public :: is_type
    end type

    interface info_t
        module procedure generic_info_c
        module procedure generic_info_s
        module procedure info_with_type_c
        module procedure info_with_type_s
    end interface

    character(len=*), parameter :: INFO_TYPE_STRING = "info_t"
    type(message_type_t), parameter :: INFO = message_type_t( &
            INFO_TYPE_STRING, .true.)
contains
    pure function generic_info_c(module_, procedure_, message) result(info_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(info_t) :: info_

        info_ = info_t(INFO, module_, procedure_, var_str(message))
    end function

    pure function generic_info_s(module_, procedure_, message) result(info_)
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(varying_string), intent(in) :: message
        type(info_t) :: info_

        info_ = info_t(INFO, module_, procedure_, message)
    end function

    pure function info_with_type_c( &
            type_tag, module_, procedure_, message) result(info_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        character(len=*), intent(in) :: message
        type(info_t) :: info_

        info_ = info_t(type_tag, module_, procedure_, var_str(message))
    end function

    pure function info_with_type_s( &
            type_tag, module_, procedure_, message) result(info_)
        type(message_type_t), intent(in) :: type_tag
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(varying_string), intent(in) :: message
        type(info_t) :: info_

        info_ = internal_constructor( &
                type_tag, call_stack_t(module_, procedure_), message)
    end function

    pure function internal_constructor(type_tag, call_stack, message) result(info_)
        type(message_type_t), intent(in) :: type_tag
        type(call_stack_t), intent(in) :: call_stack
        type(varying_string), intent(in) :: message
        type(info_t) :: info_

        info_%message_type_ = type_tag
        info_%call_stack_ = call_stack
        info_%message_ = message
    end function

    function with_names_prepended(self, module_, procedure_) result(new_message)
        class(info_t), intent(in) :: self
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        class(message_t), allocatable :: new_message

        new_message = internal_constructor( &
                self%message_type_, &
                self%call_stack_%with_names_prepended(module_, procedure_), &
                self%message_)
    end function

    pure function call_stack(self)
        class(info_t), intent(in) :: self
        type(call_stack_t) :: call_stack

        call_stack = self%call_stack_
    end function

    pure function message(self)
        class(info_t), intent(in) :: self
        type(varying_string) :: message

        message = self%message_
    end function

    pure function message_type(self)
        class(info_t), intent(in) :: self
        type(message_type_t) :: message_type

        message_type = self%message_type_
    end function

    pure function type_string(self) result(string)
        class(info_t), intent(in) :: self
        type(varying_string) :: string

        associate(a => self)
        end associate

        string = "IN: "
    end function

    pure function repr(self)
        class(info_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                'info_t(' // NEWLINE &
                    // 'call_stack_ = ' // self%call_stack_%repr() // ',' // NEWLINE &
                    // 'message_type = ' // self%message_type_%repr() // ',' // NEWLINE &
                    // 'message_ = "' // self%message_ // '"', &
                4) // NEWLINE // ')'
    end function

    pure function is_type(self, type_tag)
        class(info_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == INFO_TYPE_STRING) then
            is_type = .true.
        else
            is_type = default_is_type(self, type_tag)
        end if
    end function
end module
