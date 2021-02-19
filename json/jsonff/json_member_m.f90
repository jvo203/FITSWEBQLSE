module jsonff_json_member_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use jsonff_json_element_m, only: json_element_t
    use jsonff_json_string_m, only: json_string_t
    use jsonff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_member_t, json_member_unsafe

    type :: json_member_t
        private
        type(varying_string) :: string_
        class(json_value_t), allocatable :: element_
    contains
        private
        procedure, public :: string
        procedure, public :: element
        procedure, public :: to_compact_string
        procedure, public :: to_expanded_string
    end type

    interface json_member_t
        module procedure constructor_je
        module procedure constructor_jv
    end interface

    interface json_member_unsafe
        module procedure json_member_unsafe_ce
        module procedure json_member_unsafe_cv
        module procedure json_member_unsafe_se
        module procedure json_member_unsafe_sv
    end interface
contains
    impure elemental function constructor_je(string, element) result(json_member)
        type(json_string_t), intent(in) :: string
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member = json_member_t(string, element%value_())
    end function

    impure elemental function constructor_jv(string, element) result(json_member)
        type(json_string_t), intent(in) :: string
        class(json_value_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member%string_ = string%get_value()
        allocate(json_member%element_, source = element)
    end function

    function json_member_unsafe_ce(string, element) result(json_member)
        character(len=*), intent(in) :: string
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member = json_member_unsafe(var_str(string), element%value_())
    end function

    function json_member_unsafe_cv(string, element) result(json_member)
        character(len=*), intent(in) :: string
        class(json_value_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member = json_member_unsafe(var_str(string), element)
    end function

    impure elemental function json_member_unsafe_se( &
                string, element) result(json_member)
        type(varying_string), intent(in) :: string
        type(json_element_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member = json_member_unsafe(string, element%value_())
    end function

    impure elemental function json_member_unsafe_sv( &
                string, element) result(json_member)
        type(varying_string), intent(in) :: string
        class(json_value_t), intent(in) :: element
        type(json_member_t) :: json_member

        json_member%string_ = string
        allocate(json_member%element_, source = element)
    end function

    elemental function string(self)
        class(json_member_t), intent(in) :: self
        type(varying_string) :: string

        string = self%string_
    end function

    function element(self)
        class(json_member_t), intent(in) :: self
        class(json_value_t), allocatable :: element

        allocate(element, source = self%element_)
    end function

    pure recursive function to_compact_string(self) result(string)
        class(json_member_t), intent(in) :: self
        type(varying_string) :: string

        string = '"' // self%string_ // '":' // self%element_%to_compact_string()
    end function

    pure recursive function to_expanded_string(self) result(string)
        class(json_member_t), intent(in) :: self
        type(varying_string) :: string

        string = '"' // self%string_ // '" : ' // self%element_%to_expanded_string()
    end function
end module
