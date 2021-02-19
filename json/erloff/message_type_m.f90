module erloff_message_type_m
    use iso_varying_string, only: varying_string, assignment(=), operator(//)
    use strff, only: hanging_indent, to_string, NEWLINE

    implicit none
    private
    public :: &
            message_type_t, &
            INPUTS, &
            NOT_FOUND, &
            OUT_OF_BOUNDS, &
            OUTPUTS, &
            OUTSIDE_NORMAL_RANGE, &
            UNEQUAL_ARRAY_SIZES, &
            UNKNOWN_TYPE

    type :: message_type_t
        character(len=100) :: description
        logical :: is_fundamental = .false.
    contains
        private
        procedure, public :: to_string => message_type_to_string
        procedure, public :: repr
    end type

    type(message_type_t), parameter :: INPUTS = message_type_t("Inputs")
    type(message_type_t), parameter :: NOT_FOUND = message_type_t("Not Found")
    type(message_type_t), parameter :: OUT_OF_BOUNDS = message_type_t( &
            "Out of Bounds")
    type(message_type_t), parameter :: OUTPUTS = message_type_t("Outputs")
    type(message_type_t), parameter :: OUTSIDE_NORMAL_RANGE = message_type_t( &
            "Outside Normal Range")
    type(message_type_t), parameter :: UNEQUAL_ARRAY_SIZES = message_type_t( &
            "Unequal Array Sizes")
    type(message_type_t), parameter :: UNKNOWN_TYPE = message_type_t( &
            "Unknown Type Encountered")
contains
    pure function message_type_to_string(self) result(string)
        class(message_type_t), intent(in) :: self
        type(varying_string) :: string

        if (self%is_fundamental) then
            string = ""
        else
            string = trim(self%description) // ": "
        end if
    end function

    pure function repr(self)
        class(message_type_t), intent(in) :: self
        type(varying_string) :: repr

        repr = hanging_indent( &
                'message_type_t(' // NEWLINE &
                    // 'description = "' // trim(self%description) // '",' // NEWLINE &
                    // 'is_fundamental = ' // to_string(self%is_fundamental), &
                4) // NEWLINE // ')'
    end function
end module
