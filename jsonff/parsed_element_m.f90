module jsonff_parsed_element_m
    use jsonff_json_value_m, only: json_value_t
    use parff, only: parsed_value_t

    implicit none
    private
    public :: parsed_element_t

    type, extends(parsed_value_t) :: parsed_element_t
        private
        class(json_value_t), allocatable :: value__
    contains
        private
        procedure, public :: value_
    end type

    interface parsed_element_t
        module procedure constructor
    end interface
contains
    function constructor(value_) result(parsed_element)
        class(json_value_t), intent(in) :: value_
        type(parsed_element_t) :: parsed_element

        allocate(parsed_element%value__, source = value_)
    end function

    function value_(self)
        class(parsed_element_t), intent(in) :: self
        class(json_value_t), allocatable :: value_

        allocate(value_, source = self%value__)
    end function
end module
