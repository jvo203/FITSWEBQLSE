module jsonff_fallible_json_value_m
    use erloff, only: error_list_t, module_t, procedure_t
    use jsonff_json_value_m, only: json_value_t

    implicit none
    private
    public :: fallible_json_value_t

    type :: fallible_json_value_t
        private
        class(json_value_t), allocatable :: value__
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: value_
        procedure, public :: errors
    end type

    interface fallible_json_value_t
        module procedure from_value
        module procedure from_errors
        module procedure from_fallible_value
    end interface
contains
    function from_value(value_) result(fallible_value)
        class(json_value_t), intent(in) :: value_
        type(fallible_json_value_t) :: fallible_value

        allocate(fallible_value%value__, source = value_)
    end function

    function from_errors(errors) result(fallible_value)
        type(error_list_t), intent(in) :: errors
        type(fallible_json_value_t) :: fallible_value

        fallible_value%errors_ = errors
    end function

    function from_fallible_value( &
            fallible_value, module_, procedure_) result(new_fallible_value)
        type(fallible_json_value_t), intent(in) :: fallible_value
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_value_t) :: new_fallible_value

        if (fallible_value%failed()) then
            new_fallible_value%errors_ = error_list_t( &
                    fallible_value%errors_, module_, procedure_)
        else
            allocate(new_fallible_value%value__, source = fallible_value%value__)
        end if
    end function

    pure function failed(self)
        class(fallible_json_value_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function value_(self)
        class(fallible_json_value_t), intent(in) :: self
        class(json_value_t), allocatable :: value_

        value_ = self%value__
    end function

    function errors(self)
        class(fallible_json_value_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
