module jsonff_json_element_m
    use iso_varying_string, only: varying_string
    use jsonff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_element_t

    type :: json_element_t
        private
        class(json_value_t), allocatable :: value__
    contains
        private
        procedure, public :: value_
        procedure, public :: to_compact_string
        procedure, public :: to_expanded_string
    end type

    interface json_element_t
        module procedure constructor
    end interface
contains
    impure elemental function constructor(value_) result(json_element)
        class(json_value_t), intent(in) :: value_
        type(json_element_t) :: json_element

        allocate(json_element%value__, source = value_)
    end function

    function value_(self)
        class(json_element_t), intent(in) :: self
        class(json_value_t), allocatable :: value_

        value_ = self%value__
    end function

    pure recursive function to_compact_string(self) result(string)
        class(json_element_t), intent(in) :: self
        type(varying_string) :: string

        string = self%value__%to_compact_string()
    end function

    pure recursive function to_expanded_string(self) result(string)
        class(json_element_t), intent(in) :: self
        type(varying_string) :: string

        string = self%value__%to_expanded_string()
    end function
end module
