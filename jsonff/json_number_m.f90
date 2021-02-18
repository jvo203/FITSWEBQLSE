module jsonff_json_number_m
    use iso_varying_string, only: varying_string
    use jsonff_json_value_m, only: json_value_t
    use strff, only: to_string

    implicit none
    private
    public :: json_number_t

    type, extends(json_value_t) :: json_number_t
        private
        double precision :: number
    contains
        private
        procedure, public :: get_value
        procedure, public :: to_compact_string => number_to_string
        procedure, public :: to_expanded_string => number_to_string
    end type

    interface json_number_t
        module procedure constructor
    end interface
contains
    pure function constructor(number) result(json_number)
        double precision, intent(in) :: number
        type(json_number_t) :: json_number

        json_number%number = number
    end function

    elemental function get_value(self) result(number)
        class(json_number_t), intent(in) :: self
        double precision :: number

        number = self%number
    end function

    pure function number_to_string(self) result(string)
        class(json_number_t), intent(in) :: self
        type(varying_string) :: string

        string = to_string(self%number)
    end function
end module
