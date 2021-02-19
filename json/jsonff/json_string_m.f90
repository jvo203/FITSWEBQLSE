module jsonff_json_string_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use jsonff_json_value_m, only: json_value_t

    implicit none
    private
    public :: json_string_t, json_string_unsafe

    type, extends(json_value_t) :: json_string_t
        private
        type(varying_string) :: string
    contains
        private
        procedure, public :: get_value
        procedure, public :: to_compact_string => to_string
        procedure, public :: to_expanded_string => to_string
    end type

    interface json_string_unsafe
        module procedure json_string_unsafe_c
        module procedure json_string_unsafe_s
    end interface
contains
    elemental function get_value(self) result(string)
        class(json_string_t), intent(in) :: self
        type(varying_string) :: string

        string = self%string
    end function

    pure function json_string_unsafe_c(string) result(json_string)
        character(len=*), intent(in) :: string
        type(json_string_t) :: json_string

        json_string = json_string_unsafe(var_str(string))
    end function

    elemental function json_string_unsafe_s(string) result(json_string)
        type(varying_string), intent(in) :: string
        type(json_string_t) :: json_string

        json_string%string = string
    end function

    pure function to_string(self) result(string)
        class(json_string_t), intent(in) :: self
        type(varying_string) :: string

        string = '"' // self%string // '"'
    end function
end module
