module jsonff_parsed_member_m
    use iso_varying_string, only: varying_string
    use jsonff_json_value_m, only: json_value_t
    use parff, only: parsed_value_t

    implicit none
    private
    public :: parsed_member_t

    type, extends(parsed_value_t) :: parsed_member_t
        private
        type(varying_string) :: string_
        class(json_value_t), allocatable :: element_
    contains
        private
        procedure, public :: string
        procedure, public :: element
    end type

    interface parsed_member_t
        module procedure constructor
    end interface
contains
    function constructor(string, element) result(parsed_member)
        type(varying_string), intent(in) :: string
        class(json_value_t), intent(in) :: element
        type(parsed_member_t) :: parsed_member

        parsed_member%string_ = string
        allocate(parsed_member%element_, source = element)
    end function

    pure function string(self)
        class(parsed_member_t), intent(in) :: self
        type(varying_string) :: string

        string = self%string_
    end function

    function element(self)
        class(parsed_member_t), intent(in) :: self
        class(json_value_t), allocatable :: element

        allocate(element, source = self%element_)
    end function
end module
