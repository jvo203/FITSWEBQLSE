module jsonff_fallible_json_string_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: varying_string, operator(//), var_str
    use jsonff_json_string_m, only: json_string_t, json_string_unsafe
    use jsonff_parsers_m, only: parse_json_string, INVALID_INPUT
    use parff, only: message_t, parser_output_t, new_state

    implicit none
    private
    public :: fallible_json_string_t

    type :: fallible_json_string_t
        private
        type(json_string_t) :: string_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: string
        procedure, public :: errors
    end type

    interface fallible_json_string_t
        module procedure from_character
        module procedure from_string
        module procedure from_fallible_string
    end interface

    character(len=*), parameter :: MODULE_NAME = "jsonff_fallible_json_string_m"
contains
    function from_character(string) result(fallible_string)
        character(len=*), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        fallible_string = fallible_json_string_t( &
                fallible_json_string_t(var_str(string)), &
                module_t(MODULE_NAME), &
                procedure_t("from_character"))
    end function

    function from_string(string) result(fallible_string)
        type(varying_string), intent(in) :: string
        type(fallible_json_string_t) :: fallible_string

        type(message_t) :: message
        type(parser_output_t) :: parsed

        parsed = parse_json_string(new_state('"' // string // '"'))
        if (parsed%ok()) then
            fallible_string%string_ = json_string_unsafe(string)
        else
            message = parsed%message()
            fallible_string%errors_ = error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("from_string"), &
                    '"' // string // '" wasn''t a valid Json String: ' &
                    // message%to_string()))
        end if
    end function

    function from_fallible_string( &
            fallible_string, module_, procedure_) result(new_fallible_string)
        type(fallible_json_string_t), intent(in) :: fallible_string
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_json_string_t) :: new_fallible_string

        if (fallible_string%failed()) then
            new_fallible_string%errors_ = error_list_t( &
                    fallible_string%errors_, module_, procedure_)
        else
            new_fallible_string%string_ = fallible_string%string_
        end if
    end function

    pure function failed(self)
        class(fallible_json_string_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function string(self)
        class(fallible_json_string_t), intent(in) :: self
        type(json_string_t) :: string

        string = self%string_
    end function

    function errors(self)
        class(fallible_json_string_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
