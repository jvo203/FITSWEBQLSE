module jsonff_json_true_m
    use jsonff_json_value_m, only: json_value_t
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none
    private
    public :: json_true_t, JSON_TRUE

    type, extends(json_value_t) :: json_true_t
    contains
        private
        procedure, public :: to_compact_string => to_string
        procedure, public :: to_expanded_string => to_string
    end type

    type(json_true_t), parameter :: JSON_TRUE = json_true_t()
contains
    pure function to_string(self) result(string)
        class(json_true_t), intent(in) :: self
        type(varying_string) :: string

        associate(unused => self)
        end associate

        string = "true"
    end function
end module
