module jsonff_json_object_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, NOT_FOUND
    use iso_varying_string, only: &
            varying_string, operator(==), operator(//), var_str
    use jsonff_constants_m, only: INDENTATION
    use jsonff_fallible_json_value_m, only: fallible_json_value_t
    use jsonff_json_element_m, only: json_element_t
    use jsonff_json_member_m, only: json_member_t, json_member_unsafe
    use jsonff_json_string_m, only: json_string_t
    use jsonff_json_value_m, only: json_value_t
    use strff, only: hanging_indent, join, NEWLINE

    implicit none
    private
    public :: json_object_t, json_object_unsafe

    type, extends(json_value_t) :: json_object_t
        private
        type(json_member_t), allocatable :: members(:)
    contains
        private
        procedure :: add_je
        procedure :: add_jv
        procedure :: add_m
        generic, public :: add => add_je, add_jv, add_m
        procedure :: add_unsafe_ce
        procedure :: add_unsafe_cv
        procedure :: add_unsafe_se
        procedure :: add_unsafe_sv
        generic, public :: add_unsafe => &
                add_unsafe_ce, add_unsafe_cv, add_unsafe_se, add_unsafe_sv
        procedure, public :: size => object_size
        procedure :: get_element_c
        procedure :: get_element_j
        procedure :: get_element_s
        generic, public :: get_element => &
                get_element_c, get_element_j, get_element_s
        procedure, public :: to_compact_string
        procedure, public :: to_expanded_string
    end type

    interface json_object_t
        module procedure constructor_j
        module procedure constructor_m
    end interface

    character(len=*), parameter :: MODULE_NAME = "jsonff_json_object_m"
contains
    function constructor_j(strings, elements) result(json_object)
        type(json_string_t), intent(in) :: strings(:)
        type(json_element_t), intent(in) :: elements(:)
        type(json_object_t) :: json_object

        json_object = json_object_unsafe(strings%get_value(), elements)
    end function

    function constructor_m(members) result(json_object)
        type(json_member_t), intent(in) :: members(:)
        type(json_object_t) :: json_object

        integer :: i

        do i = 1, size(members)
            call json_object%add(members(i))
        end do
    end function

    function json_object_unsafe(strings, elements) result(json_object)
        type(varying_string), intent(in) :: strings(:)
        type(json_element_t), intent(in) :: elements(:)
        type(json_object_t) :: json_object

        json_object = json_object_t(json_member_unsafe(strings, elements))
    end function

    subroutine add_je(self, string, element)
        class(json_object_t), intent(inout) :: self
        type(json_string_t), intent(in) :: string
        type(json_element_t), intent(in) :: element

        call self%add(json_member_t(string, element))
    end subroutine

    subroutine add_jv(self, string, value)
        class(json_object_t), intent(inout) :: self
        type(json_string_t), intent(in) :: string
        class(json_value_t), intent(in) :: value

        call self%add(json_member_t(string, value))
    end subroutine

    subroutine add_m(self, member)
        class(json_object_t), intent(inout) :: self
        type(json_member_t), intent(in) :: member

        integer :: i

        if (allocated(self%members)) then
            do i = 1, self%size()
                if (member%string() == self%members(i)%string()) then
                    self%members(i) = member
                    return
                end if
            end do
            self%members = [self%members, member]
        else
            allocate(self%members, source = [member])
        end if
    end subroutine

    subroutine add_unsafe_ce(self, string, element)
        class(json_object_t), intent(inout) :: self
        character(len=*), intent(in) :: string
        type(json_element_t), intent(in) :: element

        call self%add(json_member_unsafe(string, element))
    end subroutine

    subroutine add_unsafe_cv(self, string, value)
        class(json_object_t), intent(inout) :: self
        character(len=*), intent(in) :: string
        class(json_value_t), intent(in) :: value

        call self%add(json_member_unsafe(string, value))
    end subroutine

    subroutine add_unsafe_se(self, string, element)
        class(json_object_t), intent(inout) :: self
        type(varying_string), intent(in) :: string
        type(json_element_t), intent(in) :: element

        call self%add(json_member_unsafe(string, element))
    end subroutine

    subroutine add_unsafe_sv(self, string, value)
        class(json_object_t), intent(inout) :: self
        type(varying_string), intent(in) :: string
        class(json_value_t), intent(in) :: value

        call self%add(json_member_unsafe(string, value))
    end subroutine

    pure function object_size(self)
        class(json_object_t), intent(in) :: self
        integer :: object_size

        if (allocated(self%members)) then
            object_size = size(self%members)
        else
            object_size = 0
        end if
    end function

    function get_element_c(self, key) result(element)
        class(json_object_t), intent(in) :: self
        character(len=*), intent(in) :: key
        type(fallible_json_value_t) :: element

        element = fallible_json_value_t( &
                self%get_element(var_str(key)), &
                module_t(MODULE_NAME), &
                procedure_t("get_element_c"))
    end function

    function get_element_j(self, key) result(element)
        class(json_object_t), intent(in) :: self
        type(json_string_t), intent(in) :: key
        type(fallible_json_value_t) :: element

        element = fallible_json_value_t( &
                self%get_element(key%get_value()), &
                module_t(MODULE_NAME), &
                procedure_t("get_element_j"))
    end function

    function get_element_s(self, key) result(element)
        class(json_object_t), intent(in) :: self
        type(varying_string), intent(in) :: key
        type(fallible_json_value_t) :: element

        integer :: i

        do i = 1, self%size()
            if (key == self%members(i)%string()) then
                element = fallible_json_value_t(self%members(i)%element())
                return
            end if
        end do
        element = fallible_json_value_t(error_list_t(fatal_t( &
                NOT_FOUND, &
                module_t(MODULE_NAME), &
                procedure_t("get_element_s"), &
                '"' // key // '" not found in ' // self%to_compact_string())))
    end function

    pure recursive function to_compact_string(self) result(string)
        class(json_object_t), intent(in) :: self
        type(varying_string) :: string

        integer :: i

        string = "{" // join([(self%members(i)%to_compact_string(), i = 1, self%size())], ",") // "}"
    end function

    pure recursive function to_expanded_string(self) result(string)
        class(json_object_t), intent(in) :: self
        type(varying_string) :: string

        integer :: i

        string = hanging_indent( &
                "{" // NEWLINE // join([(self%members(i)%to_expanded_string(), i = 1, self%size())], "," // NEWLINE), &
                INDENTATION) // NEWLINE // "}"
    end function
end module
