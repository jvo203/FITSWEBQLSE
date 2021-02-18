module jsonff_json_value_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: json_value_t

    type, abstract :: json_value_t
    contains
        private
        procedure(to_string_i), public, deferred :: to_compact_string
        procedure(to_string_i), public, deferred :: to_expanded_string
    end type

    abstract interface
        pure function to_string_i(self) result(string)
            import :: json_value_t, varying_string

            implicit none

            class(json_value_t), intent(in) :: self
            type(varying_string) :: string
        end function
    end interface
end module
