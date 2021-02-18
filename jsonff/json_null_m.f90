module jsonff_json_null_m
    use jsonff_json_value_m, only: json_value_t
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none
    private
    public :: json_null_t, JSON_NULL

    type, extends(json_value_t) :: json_null_t
    contains
        private
        procedure, public :: to_compact_string => to_string
        procedure, public :: to_expanded_string => to_string
    end type

    type(json_null_t), parameter :: JSON_NULL = json_null_t()
contains
    pure function to_string(self) result(string)
        class(json_null_t), intent(in) :: self
        type(varying_string) :: string

        associate(unused => self)
        end associate

        string = "null"
    end function
end module
