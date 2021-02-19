module jsonff_parsers_m
    use erloff, only: &
            error_list_t, fatal_t, message_type_t, module_t, procedure_t
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), var_str
    use jsonff_fallible_json_value_m, only: fallible_json_value_t
    use jsonff_json_array_m, only: json_array_t
    use jsonff_json_element_m, only: json_element_t
    use jsonff_json_false_m, only: JSON_FALSE
    use jsonff_json_member_m, only: json_member_t, json_member_unsafe
    use jsonff_json_null_m, only: JSON_NULL
    use jsonff_json_number_m, only: json_number_t
    use jsonff_json_object_m, only: json_object_t
    use jsonff_json_string_m, only: json_string_t, json_string_unsafe
    use jsonff_json_true_m, only: JSON_TRUE
    use jsonff_parsed_element_m, only: parsed_element_t
    use jsonff_parsed_member_m, only: parsed_member_t
    use parff, only: &
            message_t, &
            parse_result_t, &
            parsed_character_t, &
            parsed_item_t, &
            parsed_items_t, &
            parsed_string_t, &
            parsed_value_t, &
            parser_output_t, &
            state_t, &
            drop_then, &
            either, &
            empty_ok, &
            many, &
            many_with_separator, &
            many1, &
            parse_char, &
            parse_digit, &
            parse_end_of_input, &
            parse_string, &
            parse_whitespace, &
            parse_with, &
            repeat_, &
            satisfy, &
            sequence, &
            then_drop, &
            with_label
    use strff, only: operator(.includes.), join, read_file

    implicit none
    private
    public :: &
            parse_element, &
            parse_json, &
            parse_json_document, &
            parse_json_from_file, &
            parse_json_string, &
            parse_value, &
            INVALID_INPUT

    interface parse_json
        module procedure parse_json_c
        module procedure parse_json_s
    end interface

    interface parse_json_from_file
        module procedure parse_json_from_file_c
        module procedure parse_json_from_file_s
    end interface

    type(message_type_t), parameter :: INVALID_INPUT = message_type_t( &
            "Invalid Input")
    character(len=*), parameter :: MODULE_NAME = "jsonff_parsers_m"
contains
    function parse_any_whitespace(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = many(parse_whitespace, state_)
    end function

    recursive function parse_array(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(json_element_t), allocatable :: the_elements(:)

        result_ = then_drop( &
                drop_then(parse_initial, parse_elements, state_), &
                parse_close_bracket)
        if (result_%ok()) then
            select type (elements => result_%parsed())
            type is (parsed_items_t)
                items = elements%items()
                allocate(the_elements(size(items)))
                do i = 1, size(the_elements)
                    select type (element => items(i)%item())
                    type is (parsed_element_t)
                        the_elements(i) = json_element_t(element%value_())
                    end select
                end do
            end select
            result_ = result_%with_parsed_value(parsed_element_t( &
                    json_array_t(the_elements)))
        end if
    contains
        function parse_initial(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = drop_then(parse_open_bracket, parse_any_whitespace, state__)
        end function
    end function

    function parse_at_least_one_digit(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string), allocatable :: digits(:)
        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(parsed_string_t) :: parsed_digits

        result_ = many1(parse_digit, state_)
        if (result_%ok()) then
            select type (results => result_%parsed())
            type is (parsed_items_t)
                items = results%items()
                allocate(digits(size(items)))
                do i = 1, size(digits)
                    select type (string => items(i)%item())
                    type is (parsed_character_t)
                        digits(i) = string%value_()
                    end select
                end do
                parsed_digits = parsed_string_t(join(digits, ""))
                result_ = result_%with_parsed_value(parsed_digits)
            end select
        end if
    end function

    function parse_character(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = either(parse_valid_single_character, parse_escape, state_)
    end function

    function parse_characters(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string), allocatable :: characters(:)
        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(parsed_string_t) :: parsed_characters

        result_ = many(parse_character, state_)
        if (result_%ok()) then
            select type (results => result_%parsed())
            type is (parsed_items_t)
                items = results%items()
                allocate(characters(size(items)))
                do i = 1, size(characters)
                    select type (the_character => items(i)%item())
                    type is (parsed_string_t)
                        characters(i) = the_character%value_()
                    end select
                end do
                parsed_characters = parsed_string_t(join(characters, ""))
                result_ = result_%with_parsed_value(parsed_characters)
            end select
        end if
    end function

    function parse_close_brace(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("}", state_)
    end function

    function parse_close_bracket(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("]", state_)
    end function

    function parse_colon(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char(":", state_)
    end function

    function parse_comma(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char(",", state_)
    end function

    function parse_digits(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string), allocatable :: digits(:)
        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(parsed_string_t) :: parsed_digits

        result_ = many(parse_digit, state_)
        select type (results => result_%parsed())
        type is (parsed_items_t)
            items = results%items()
            allocate(digits(size(items)))
            do i = 1, size(digits)
                select type (string => items(i)%item())
                type is (parsed_character_t)
                    digits(i) = string%value_()
                end select
            end do
        end select
        parsed_digits = parsed_string_t(join(digits, ""))
        result_ = result_%with_parsed_value(parsed_digits)
    end function

    recursive function parse_element(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = then_drop( &
                drop_then(parse_any_whitespace, parse_value, state_), &
                parse_any_whitespace)
    end function

    recursive function parse_elements(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = many_with_separator(parse_element, parse_comma, state_)
    end function

    function parse_escape(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = drop_then(parse_backslash, parse_escaped_character, the_state)
        if (the_result%ok()) then
            select type (the_string => the_result%parsed())
            type is (parsed_string_t)
                the_result = the_result%with_parsed_value(parsed_string_t( &
                        "\" // the_string%value_()))
            end select
        end if
    contains
        function parse_backslash(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("\", state_)
        end function
    end function

    function parse_escaped_character(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = either( &
                parse_single_escaped_character, parse_escaped_unicode, state_)
    end function

    function parse_escaped_unicode(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = drop_then(parse_u, parse_hex_digits, the_state)
        if (the_result%ok()) then
            select type (the_string => the_result%parsed())
            type is (parsed_string_t)
                the_result = the_result%with_parsed_value(parsed_string_t( &
                        "u" // the_string%value_()))
            end select
        end if
    contains
        function parse_u(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("u", state_)
        end function
    end function

    function parse_exponent(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = sequence( &
                sequence(parse_e, then_parse_sign, the_state), &
                then_parse_at_least_one_digit)
    contains
        function parse_e(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_upper, parse_lower, state_)
            if (result_%ok()) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            the_character%value_()))
                end select
            end if
        end function

        function parse_upper(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("E", state_)
        end function

        function parse_lower(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("e", state_)
        end function
    end function

    function parse_false(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_string("false", state_)
        if (result_%ok()) then
            result_ = result_%with_parsed_value(parsed_element_t(JSON_FALSE))
        end if
    end function

    function parse_fraction(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = sequence( &
                parse_decimal_point, then_parse_at_least_one_digit, the_state)
    contains
        function parse_decimal_point(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char(".", state_)
            if (result_%ok()) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            the_character%value_()))
                end select
            end if
        end function
    end function

    function parse_hex_digit(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("hex digit", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
            if (result_%ok()) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            the_character%value_()))
                end select
            end if
        end function

        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = "0123456789abcdefABCDEF".includes.char_
        end function
    end function

    function parse_hex_digits(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(varying_string) :: digits(4)
        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(parsed_string_t) :: parsed_digits

        result_ = repeat_(parse_hex_digit, 4, state_)
        if (result_%ok()) then
            select type (results => result_%parsed())
            type is (parsed_items_t)
                allocate(items, source = results%items())
                do i = 1, 4
                    select type (string => items(i)%item())
                    type is (parsed_string_t)
                        digits(i) = string%value_()
                    end select
                end do
            end select
            parsed_digits = parsed_string_t(join(digits, ""))
            result_ = result_%with_parsed_value(parsed_digits)
        end if
    end function

    function parse_integer(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = either(first_two, second_two, the_state)
    contains
        function first_two(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either(parse_multiple_digits, parse_single_digit, state_)
        end function

        function second_two(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = either( &
                    parse_negative_multiple_digits, &
                    parse_negative_single_digit, &
                    state_)
        end function
    end function

    function parse_json_c(string) result(fallible_json)
        character(len=*), intent(in) :: string
        type(fallible_json_value_t) :: fallible_json

        fallible_json = fallible_json_value_t( &
                parse_json(var_str(string)), &
                module_t(MODULE_NAME), &
                procedure_t("parse_json_c"))
    end function

    function parse_json_s(string) result(fallible_json)
        type(varying_string), intent(in) :: string
        type(fallible_json_value_t) :: fallible_json

        type(parse_result_t) :: result_

        result_ = parse_with(parse_json_document, string)
        if (result_%ok()) then
            select type (the_json => result_%parsed())
            type is (parsed_element_t)
                fallible_json = fallible_json_value_t(the_json%value_())
            end select
        else
            fallible_json = fallible_json_value_t(error_list_t(fatal_t( &
                    INVALID_INPUT, &
                    module_t(MODULE_NAME), &
                    procedure_t("parse_json_s"), &
                    result_%message())))
        end if
    end function

    function parse_json_document(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = then_drop(parse_element, parse_end_of_input, state_)
    end function

    function parse_json_from_file_c(filename) result(fallible_json)
        character(len=*), intent(in) :: filename
        type(fallible_json_value_t) :: fallible_json

        fallible_json = fallible_json_value_t( &
                parse_json_from_file(var_str(filename)), &
                module_t(MODULE_NAME), &
                procedure_t("parse_json_from_file_c"))
    end function

    function parse_json_from_file_s(filename) result(fallible_json)
        type(varying_string), intent(in) :: filename
        type(fallible_json_value_t) :: fallible_json

        fallible_json = fallible_json_value_t( &
                parse_json(read_file(filename)), &
                module_t(MODULE_NAME), &
                procedure_t("parse_json_from_file_s"))
    end function

    function parse_json_string(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = then_drop( &
                drop_then(parse_quote, parse_characters, state_), &
                parse_quote)
        if (result_%ok()) then
            select type (parsed_string => result_%parsed())
            type is (parsed_string_t)
                result_ = result_%with_parsed_value(parsed_element_t( &
                        json_string_unsafe(parsed_string%value_())))
            end select
        end if
    end function

    recursive function parse_member(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = sequence( &
                then_drop( &
                        then_drop( &
                                drop_then( &
                                        parse_any_whitespace, &
                                        parse_json_string, &
                                        the_state), &
                                parse_any_whitespace), &
                        parse_colon), &
                then_parse_element)
    contains
        recursive function then_parse_element(previous, state_) result(result_)
            class(parsed_value_t), intent(in) :: previous
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            select type (previous)
            type is (parsed_element_t)
                select type (string => previous%value_())
                type is (json_string_t)
                    result_ = parse_element(state_)
                    if (result_%ok()) then
                        select type (element => result_%parsed())
                        type is (parsed_element_t)
                            result_ = result_%with_parsed_value(parsed_member_t( &
                                    string%get_value(), element%value_()))
                        end select
                    end if
                end select
            end select
        end function
    end function

    recursive function parse_members(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = many_with_separator(parse_member, parse_comma, state_)
    end function

    function parse_minus(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("-", state_)
        if (result_%ok()) then
            select type (the_character => result_%parsed())
            type is (parsed_character_t)
                result_ = result_%with_parsed_value(parsed_string_t( &
                        the_character%value_()))
            end select
        end if
    end function

    function parse_multiple_digits(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = sequence(parse_one_nine, then_parse_digits, state_)
    end function

    function parse_negative_multiple_digits(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = sequence( &
                sequence(parse_minus, then_parse_one_nine, state_), &
                then_parse_digits)
    end function

    function parse_negative_single_digit(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = sequence(parse_minus, then_parse_digit, state_)
    end function

    function parse_null(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_string("null", state_)
        if (result_%ok()) then
            result_ = result_%with_parsed_value(parsed_element_t(JSON_NULL))
        end if
    end function

    function parse_number(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("number", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            double precision :: the_number
            character(len=64) :: the_string

            result_ = sequence( &
                    sequence(parse_integer, then_parse_fraction, state_), &
                    then_parse_exponent)
            if (result_%ok()) then
                select type (parsed_string => result_%parsed())
                type is (parsed_string_t)
                    the_string = parsed_string%value_()
                    read(the_string, *) the_number
                    result_ = result_%with_parsed_value(parsed_element_t( &
                            json_number_t(the_number)))
                end select
            end if
        end function
    end function

    recursive function parse_object(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        integer :: i
        type(parsed_item_t), allocatable :: items(:)
        type(json_member_t), allocatable :: the_members(:)

        result_ = then_drop( &
                drop_then(parse_initial, parse_members, state_), &
                parse_close_brace)
        if (result_%ok()) then
            select type (members => result_%parsed())
            type is (parsed_items_t)
                items = members%items()
                allocate(the_members(size(items)))
                do i = 1, size(the_members)
                    select type (member => items(i)%item())
                    type is (parsed_member_t)
                        the_members(i) = json_member_unsafe(member%string(), member%element())
                    end select
                end do
                result_ = result_%with_parsed_value(parsed_element_t( &
                        json_object_t(the_members)))
            end select
        end if
    contains
        function parse_initial(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = drop_then(parse_open_brace, parse_any_whitespace, state__)
        end function
    end function

    function parse_one_nine(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = with_label("1-9", the_parser, the_state)
    contains
        function the_parser(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = satisfy(the_matcher, state_)
            if (result_%ok()) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            the_character%value_()))
                end select
            end if
        end function

        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = "123456789".includes.char_
        end function
    end function

    function parse_open_brace(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("{", state_)
    end function

    function parse_open_bracket(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("[", state_)
    end function

    function parse_quote(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char('"', state_)
    end function

    function parse_sign(the_state) result(the_result)
        type(state_t), intent(in) :: the_state
        type(parser_output_t) :: the_result

        the_result = either(parse_plus, parse_minus, the_state)
    contains
        function parse_plus(state_) result(result_)
            type(state_t), intent(in) :: state_
            type(parser_output_t) :: result_

            result_ = parse_char("+", state_)
            if (result_%ok()) then
                select type (the_character => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            the_character%value_()))
                end select
            end if
        end function
    end function

    function parse_single_digit(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_digit(state_)
        if (result_%ok()) then
            select type (the_character => result_%parsed())
            type is (parsed_character_t)
                result_ = result_%with_parsed_value(parsed_string_t( &
                        the_character%value_()))
            end select
        end if
    end function

    function parse_single_escaped_character(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = satisfy(the_matcher, state_)
        if (result_%ok()) then
            select type (the_character => result_%parsed())
            type is (parsed_character_t)
                result_ = result_%with_parsed_value(parsed_string_t( &
                        the_character%value_()))
            end select
        end if
    contains
        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = &
                    char_ == '"' &
                    .or. char_ == '\' &
                    .or. char_ == '/' &
                    .or. char_ == 'b' &
                    .or. char_ == 'f' &
                    .or. char_ == 'n' &
                    .or. char_ == 'r' &
                    .or. char_ == 't'
        end function
    end function

    function parse_true(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_string("true", state_)
        if (result_%ok()) then
            result_ = result_%with_parsed_value(parsed_element_t(JSON_TRUE))
        end if
    end function

    function parse_valid_single_character(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = satisfy(the_matcher, state_)
        if (result_%ok()) then
            select type (the_character => result_%parsed())
            type is (parsed_character_t)
                result_ = result_%with_parsed_value(parsed_string_t( &
                        the_character%value_()))
            end select
        end if
    contains
        pure function the_matcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = char_ /= '"' .and. char_ /= '\'
        end function
    end function

    recursive function parse_value(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = either(first_try, parse_object, state_)
    contains
        recursive function first_try(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = either(parse_null, second_try, state__)
        end function

        recursive function second_try(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = either(parse_true, thrid_try, state__)
        end function

        recursive function thrid_try(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = either(parse_false, fourth_try, state__)
        end function

        recursive function fourth_try(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = either(parse_number, fifth_try, state__)
        end function

        recursive function fifth_try(state__) result(result__)
            type(state_t), intent(in) :: state__
            type(parser_output_t) :: result__

            result__ = either(parse_json_string, parse_array, state__)
        end function
    end function

    function then_parse_at_least_one_digit(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_at_least_one_digit(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            end if
        end select
    end function

    function then_parse_digit(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_digit(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_character_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            end if
        end select
    end function

    function then_parse_digits(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_digits(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            end if
        end select
    end function

    function then_parse_exponent(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_exponent(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input(), &
                        state_%position(), &
                        message_t(state_%position(), var_str(""), [varying_string::]))
            end if
        end select
    end function

    function then_parse_fraction(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_fraction(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input(), &
                        state_%position(), &
                        message_t(state_%position(), var_str(""), [varying_string::]))
            end if
        end select
    end function

    function then_parse_one_nine(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_one_nine(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            end if
        end select
    end function

    function then_parse_sign(previous, state_) result(result_)
        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        select type (previous)
        type is (parsed_string_t)
            result_ = parse_sign(state_)
            if (result_%ok()) then
                select type (next => result_%parsed())
                type is (parsed_string_t)
                    result_ = result_%with_parsed_value(parsed_string_t( &
                            previous%value_() // next%value_()))
                end select
            else
                result_ = empty_ok( &
                        previous, &
                        state_%input(), &
                        state_%position(), &
                        message_t(state_%position(), var_str(""), [varying_string::]))
            end if
        end select
    end function
end module
