module jsonff_json_array_m
    use erloff, only: &
            error_list_t, fatal_t, module_t, procedure_t, OUT_OF_BOUNDS
    use iso_varying_string, only: varying_string, operator(//)
    use jsonff_constants_m, only: INDENTATION
    use jsonff_fallible_json_value_m, only: fallible_json_value_t
    use jsonff_json_element_m, only: json_element_t
    use jsonff_json_value_m, only: json_value_t
    use strff, only: hanging_indent, join, to_string, NEWLINE

    implicit none
    private
    public :: json_array_t

    type, extends(json_value_t) :: json_array_t
        private
        type(json_element_t), allocatable :: elements(:)
    contains
        private
        procedure :: append_e
        procedure :: append_v
        generic, public :: append => append_e, append_v
        procedure :: insert_e
        procedure :: insert_v
        generic, public :: insert => insert_e, insert_v
        procedure :: prepend_e
        procedure :: prepend_v
        generic, public :: prepend => prepend_e, prepend_v
        procedure, public :: length
        procedure, public :: get_element
        procedure, public :: to_compact_string
        procedure, public :: to_expanded_string
    end type

    interface json_array_t
        module procedure constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "jsonff_json_array_m"
contains
    function constructor(elements) result(json_array)
        type(json_element_t), intent(in) :: elements(:)
        type(json_array_t) :: json_array

        allocate(json_array%elements, source = elements)
    end function

    subroutine append_e(self, element)
        class(json_array_t), intent(inout) :: self
        type(json_element_t), intent(in) :: element

        if (self%length() > 0) then
            self%elements = [self%elements, element]
        else
            allocate(self%elements, source = [element])
        end if
    end subroutine

    subroutine append_v(self, value)
        class(json_array_t), intent(inout) :: self
        class(json_value_t), intent(in) :: value

        call self%append(json_element_t(value))
    end subroutine

    subroutine insert_e(self, element, position)
        class(json_array_t), intent(inout) :: self
        type(json_element_t), intent(in) :: element
        integer, intent(in) :: position

        if (position > self%length()) then
            call self%append(element)
        else if (position <= 0) then
            call self%prepend(element)
        else
            self%elements = [self%elements(1:position-1), element, self%elements(position:)]
        end if
    end subroutine

    subroutine insert_v(self, value, position)
        class(json_array_t), intent(inout) :: self
        class(json_value_t), intent(in) :: value
        integer, intent(in) :: position

        call self%insert(json_element_t(value), position)
    end subroutine

    subroutine prepend_e(self, element)
        class(json_array_t), intent(inout) :: self
        type(json_element_t), intent(in) :: element

        if (self%length() > 0) then
            self%elements = [element, self%elements]
        else
            allocate(self%elements, source = [element])
        end if
    end subroutine

    subroutine prepend_v(self, value)
        class(json_array_t), intent(inout) :: self
        class(json_value_t), intent(in) :: value

        call self%prepend(json_element_t(value))
    end subroutine

    pure function length(self)
        class(json_array_t), intent(in) :: self
        integer :: length

        if (allocated(self%elements)) then
            length = size(self%elements)
        else
            length = 0
        end if
    end function

    function get_element(self, position) result(element)
        class(json_array_t), intent(in) :: self
        integer, intent(in) :: position
        type(fallible_json_value_t) :: element

        if (position <= 0 .or. position > self%length()) then
            element = fallible_json_value_t(error_list_t(fatal_t( &
                    OUT_OF_BOUNDS, &
                    module_t(MODULE_NAME), &
                    procedure_t("get_element"), &
                    "Attempted to access element " // to_string(position) &
                        // " from an array with only " // to_string(self%length()) &
                        // " elements")))
        else
            element = fallible_json_value_t(self%elements(position)%value_())
        end if
    end function

    pure recursive function to_compact_string(self) result(string)
        class(json_array_t), intent(in) :: self
        type(varying_string) :: string

        integer :: i

        string = "[" // join([(self%elements(i)%to_compact_string(), i = 1, self%length())], ",") // "]"
    end function

    pure recursive function to_expanded_string(self) result(string)
        class(json_array_t), intent(in) :: self
        type(varying_string) :: string

        integer :: i

        string = hanging_indent( &
                "[" // NEWLINE // join([(self%elements(i)%to_expanded_string(), i = 1, self%length())], "," // NEWLINE), &
                INDENTATION) // NEWLINE // "]"
    end function
end module
