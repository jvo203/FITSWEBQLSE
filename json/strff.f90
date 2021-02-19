module strff
    use iso_fortran_env, only: &
            INT8, INT16, INT32, INT64, REAL32, REAL64, IOSTAT_END
    use iso_varying_string, only: &
            varying_string, assignment(=), operator(//), char, get, len, var_str

    implicit none
    private
    public :: &
            operator(.includes.), &
            operator(.startswith.), &
            cover_empty_decimal, &
            first_character, &
            hanging_indent, &
            includes, &
            indent, &
            join, &
            last_character, &
            read_file, &
            read_file_lines, &
            remove_trailing_zeros, &
            split_at, &
            to_string, &
            without_first_character, &
            without_last_character, &
            NEWLINE

    interface operator(.includes.)
        module procedure includes_cc
        module procedure includes_cs
        module procedure includes_sc
        module procedure includes_ss
    end interface

    interface operator(.startswith.)
        module procedure starts_with_cc
        module procedure starts_with_cs
        module procedure starts_with_sc
        module procedure starts_with_ss
    end interface

    interface cover_empty_decimal
        module procedure cover_empty_decimal_c
        module procedure cover_empty_decimal_s
    end interface

    interface first_character
        module procedure first_character_c
        module procedure first_character_s
    end interface

    interface hanging_indent
        module procedure hanging_indent_c
        module procedure hanging_indent_s
    end interface

    interface includes
        module procedure includes_cc
        module procedure includes_cs
        module procedure includes_sc
        module procedure includes_ss
    end interface

    interface indent
        module procedure indent_c
        module procedure indent_s
    end interface

    interface join
        module procedure join_c
        module procedure join_s
    end interface

    interface last_character
        module procedure last_character_c
        module procedure last_character_s
    end interface

    interface read_file
        module procedure read_file_c
        module procedure read_file_s
    end interface

    interface read_file_lines
        module procedure read_file_lines_c
        module procedure read_file_lines_s
    end interface

    interface remove_trailing_zeros
        module procedure remove_trailing_zeros_c
        module procedure remove_trailing_zeros_s
    end interface

    interface split_at
        module procedure split_at_cc
        module procedure split_at_cs
        module procedure split_at_sc
        module procedure split_at_ss
    end interface

    interface to_string
        module procedure to_string_int8
        module procedure to_string_int16
        module procedure to_string_int32
        module procedure to_string_int64
        module procedure to_string_logical
        module procedure to_string_real32
        module procedure to_string_real64
        module procedure to_string_with_significant_digits_real32
        module procedure to_string_with_significant_digits_real64
    end interface

    interface without_first_character
        module procedure without_first_character_c
        module procedure without_first_character_s
    end interface

    interface without_last_character
        module procedure without_last_character_c
        module procedure without_last_character_s
    end interface

    character(len=*), parameter :: NEWLINE = NEW_LINE('A')
contains
    pure function cover_empty_decimal_c(number) result(fixed)
        character(len=*), intent(in) :: number
        type(varying_string) :: fixed

        if (last_character(number) == ".") then
            fixed = number // "0"
        else if (first_character(number) == ".") then
            fixed = "0" // number
        else
            fixed = number
        end if
    end function

    pure function cover_empty_decimal_s(number) result(fixed)
        type(varying_string), intent(in) :: number
        type(varying_string) :: fixed

        fixed = cover_empty_decimal(char(number))
    end function

    pure function first_character_c(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        char_ = string(1:1)
    end function

    pure function first_character_s(string) result(char_)
        type(varying_string), intent(in) :: string
        character(len=1) :: char_

        char_ = first_character(char(string))
    end function

    pure function hanging_indent_c(string, spaces) result(indented)
        character(len=*), intent(in) :: string
        integer, intent(in) :: spaces
        type(varying_string) :: indented

        indented = hanging_indent(var_str(string), spaces)
    end function

    pure function hanging_indent_s(string, spaces) result(indented)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: spaces
        type(varying_string) :: indented

        indented = join(split_at(string, NEWLINE), NEWLINE // repeat(" ", spaces))
    end function

    pure function includes_cc(within, search_for)
        character(len=*), intent(in) :: within
        character(len=*), intent(in) :: search_for
        logical :: includes_cc

        includes_cc = index(within, search_for) > 0
    end function

    pure function includes_cs(within, search_for)
        character(len=*), intent(in) :: within
        type(varying_string), intent(in) :: search_for
        logical :: includes_cs

        includes_cs = within.includes.char(search_for)
    end function

    pure function includes_sc(within, search_for)
        type(varying_string), intent(in) :: within
        character(len=*), intent(in) :: search_for
        logical :: includes_sc

        includes_sc = char(within).includes.search_for
    end function

    pure function includes_ss(within, search_for)
        type(varying_string), intent(in) :: within
        type(varying_string), intent(in) :: search_for
        logical :: includes_ss

        includes_ss = char(within).includes.char(search_for)
    end function

    pure function indent_c(string, spaces) result(indented)
        character(len=*), intent(in) :: string
        integer, intent(in) :: spaces
        type(varying_string) :: indented

        indented = indent(var_str(string), spaces)
    end function

    pure function indent_s(string, spaces) result(indented)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: spaces
        type(varying_string) :: indented

        indented = repeat(" ", spaces) // hanging_indent(string, spaces)
    end function

    pure function join_c(strings, separator) result(string)
        type(varying_string), intent(in) :: strings(:)
        character(len=*), intent(in) :: separator
        type(varying_string) :: string

        string = join(strings, var_str(separator))
    end function

    pure recursive function join_s(strings, separator) result(string)
        type(varying_string), intent(in) :: strings(:)
        type(varying_string), intent(in) :: separator
        type(varying_string) :: string

        integer :: num_strings

        num_strings = size(strings)
        if (num_strings == 1) then
            string = strings(1)
        else if (num_strings == 0) then
            string = ""
        else
            string = &
                    strings(1) &
                    // separator &
                    // join(strings(2:), separator)
        end if
    end function

    pure function last_character_c(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        integer :: length

        length = len(string)
        char_ = string(length:length)
    end function

    pure function last_character_s(string) result(char_)
        type(varying_string), intent(in) :: string
        character(len=1) :: char_

        char_ = last_character(char(string))
    end function

    pure recursive function remove_trailing_zeros_c(number) result(trimmed)
        character(len=*), intent(in) :: number
        type(varying_string) :: trimmed

        if (last_character(number) == "0") then
            trimmed = remove_trailing_zeros(without_last_character(number))
        else
            trimmed = number
        end if
    end function

    pure recursive function remove_trailing_zeros_s(number) result(trimmed)
        type(varying_string), intent(in) :: number
        type(varying_string) :: trimmed

        trimmed = remove_trailing_zeros(char(number))
    end function

    function read_file_c(filename) result(contents)
        character(len=*), intent(in) :: filename
        type(varying_string) :: contents

        integer :: file_unit
        integer :: stat
        type(varying_string) :: tmp

        open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
        call get(file_unit, contents, iostat = stat)
        if (stat == IOSTAT_END) return
        do
            call get(file_unit, tmp, iostat = stat)
            if (stat == iostat_end) exit
            contents = contents // NEWLINE // tmp
        end do
        close(file_unit)
    end function

    function read_file_s(filename) result(contents)
        type(varying_string), intent(in) :: filename
        type(varying_string) :: contents

        contents = read_file(char(filename))
    end function

    function read_file_lines_c(filename) result(lines)
        character(len=*), intent(in) :: filename
        type(varying_string), allocatable :: lines(:)

        integer :: file_unit
        integer :: i
        integer :: num_lines
        integer :: stat
        type(varying_string) :: tmp

        open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
        num_lines = 0
        do
            call get(file_unit, tmp, iostat = stat)
            if (stat == IOSTAT_END) exit
            num_lines = num_lines + 1
        end do
        rewind(file_unit)

        allocate(lines(num_lines))
        do i = 1, num_lines
            call get(file_unit, lines(i))
        end do
        close(file_unit)
    end function

    function read_file_lines_s(filename) result(lines)
        type(varying_string), intent(in) :: filename
        type(varying_string), allocatable :: lines(:)

        lines = read_file_lines(char(filename))
    end function

    pure recursive function split_at_cc( &
            string, split_characters) result(strings)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(varying_string), allocatable :: strings(:)

        if (len(split_characters) > 0) then
            if (len(string) > 0) then
                if (split_characters.includes.first_character(string)) then
                    allocate(strings, source = split_at( &
                            without_first_character(string), &
                            split_characters))
                else if (split_characters.includes.last_character(string)) then
                    allocate(strings, source = split_at( &
                            without_last_character(string), &
                            split_characters))
                else
                    allocate(strings, source = &
                        do_split(string, split_characters))
                end if
            else
                allocate(strings(0))
            end if
        else
            allocate(strings(1))
            strings(1) = string
        end if
    contains
        pure recursive function do_split(string_, split_characters_) result(strings_)
            character(len=*), intent(in) :: string_
            character(len=*), intent(in) :: split_characters_
            type(varying_string), allocatable :: strings_(:)

            integer :: i
            type(varying_string), allocatable :: rest(:)
            integer :: string_length_
            type(varying_string) :: this_string

            string_length_ = len(string_)
            do i = 2, string_length_
                if (split_characters_.includes.string_(i:i)) exit
            end do
            if (i < string_length_) then
                this_string = string_(1:i - 1)
                allocate(rest, source = &
                        split_at(string_(i + 1:), split_characters_))
                allocate(strings_(size(rest) + 1))
                strings_(1) = this_string
                strings_(2:) = rest(:)
            else
                allocate(strings_(1))
                strings_(1) = string_
            end if
        end function
    end function

    pure function split_at_cs(string, split_characters) result(strings)
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: split_characters
        type(varying_string), allocatable :: strings(:)

        allocate(strings, source = split_at(string, char(split_characters)))
    end function

    pure recursive function split_at_sc(string, split_characters) result(strings)
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(varying_string), allocatable :: strings(:)

        allocate(strings, source = split_at(char(string), split_characters))
    end function

    pure function split_at_ss(string, split_characters) result(strings)
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: split_characters
        type(varying_string), allocatable :: strings(:)

        allocate(strings, source = split_at(char(string), char(split_characters)))
    end function

    pure function starts_with_cc(string, substring)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: starts_with_cc

        starts_with_cc = index(string, substring) == 1
    end function

    pure function starts_with_cs(string, substring)
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: substring
        logical :: starts_with_cs

        starts_with_cs = string.startswith.char(substring)
    end function

    pure function starts_with_sc(string, substring)
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: starts_with_sc

        starts_with_sc = char(string).startswith.substring
    end function

    pure function starts_with_ss(string, substring)
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: substring
        logical :: starts_with_ss

        starts_with_ss = char(string).startswith.char(substring)
    end function

    pure function to_string_int8(number) result(string)
        integer(INT8), intent(in) :: number
        type(varying_string) :: string

        character(len=4) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function

    pure function to_string_int16(number) result(string)
        integer(INT16), intent(in) :: number
        type(varying_string) :: string

        character(len=6) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function

    pure function to_string_int32(number) result(string)
        integer(INT32), intent(in) :: number
        type(varying_string) :: string

        character(len=11) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function

    pure function to_string_int64(number) result(string)
        integer(INT64), intent(in) :: number
        type(varying_string) :: string

        character(len=20) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function

    pure function to_string_logical(logical_) result(string)
        logical, intent(in) :: logical_
        type(varying_string) :: string

        if (logical_) then
            string = "TRUE"
        else
            string = "FALSE"
        end if
    end function

    pure function to_string_real32(number) result(string)
        real(REAL32), intent(in) :: number
        type(varying_string) :: string

        string = to_string(number, 9)
    end function

    pure function to_string_real64(number) result(string)
        real(REAL64), intent(in) :: number
        type(varying_string) :: string

        string = to_string(number, 17)
    end function

    ! TODO: implement to_string_real128 once conditions described below
    !       NOTE: the default precision will be 36

    pure function to_string_with_significant_digits_real32( &
            number, significant_digits) result(string_)
        real(REAL32), intent(in) :: number
        integer, intent(in) :: significant_digits
        type(varying_string) :: string_

        integer, parameter :: C_LEN = 18
        real(REAL32), parameter :: MACHINE_TINY = tiny(real(0.0, kind=REAL32))
        real(REAL32) :: abs_num
        character(len=C_LEN) :: exponent_part
        character(len=C_LEN) :: floating_part
        character(len=7) :: format_string
        type(varying_string) :: intermediate
        type(varying_string) :: intermediate_basic
        type(varying_string) :: intermediate_scientific
        integer :: scale_

        abs_num = abs(number)
        if (abs_num <= MACHINE_TINY) then
            string_ = "0.0"
            return
        end if
        scale_ = floor(log10(abs_num))
        if (scale_ <= -2) then
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) &
                    abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') &
                        "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = cover_empty_decimal( &
                        remove_trailing_zeros(trim(floating_part)))

                if (len(intermediate_scientific) < len(intermediate_basic)) then
                    intermediate = intermediate_scientific
                else
                    intermediate = intermediate_basic
                end if
            else
                intermediate = intermediate_scientific
            end if
        end if
        if (number < 0.0D0) then
            string_ = "-" // intermediate
        else
            string_ = intermediate
        end if
    end function

    pure function to_string_with_significant_digits_real64( &
            number, significant_digits) result(string_)
        real(REAL64), intent(in) :: number
        integer, intent(in) :: significant_digits
        type(varying_string) :: string_

        integer, parameter :: C_LEN = 34
        real(REAL64), parameter :: MACHINE_TINY = tiny(real(0.0, kind=REAL64))
        real(REAL64) :: abs_num
        character(len=C_LEN) :: exponent_part
        character(len=C_LEN) :: floating_part
        character(len=7) :: format_string
        type(varying_string) :: intermediate
        type(varying_string) :: intermediate_basic
        type(varying_string) :: intermediate_scientific
        integer :: scale_

        abs_num = abs(number)
        if (abs_num <= MACHINE_TINY) then
            string_ = "0.0"
            return
        end if
        scale_ = floor(log10(abs_num))
        if (scale_ <= -2) then
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) &
                    abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = &
                    cover_empty_decimal( &
                            remove_trailing_zeros(trim(floating_part))) &
                    // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') &
                        "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = cover_empty_decimal( &
                        remove_trailing_zeros(trim(floating_part)))

                if (len(intermediate_scientific) < len(intermediate_basic)) then
                    intermediate = intermediate_scientific
                else
                    intermediate = intermediate_basic
                end if
            else
                intermediate = intermediate_scientific
            end if
        end if
        if (number < 0.0D0) then
            string_ = "-" // intermediate
        else
            string_ = intermediate
        end if
    end function

    ! TODO: implement to_string_with_significant_digits_real128
    !       once REAL128 is sufficiently portable, or the ifdef
    !       to conditionally include it is sufficiently portable.
    !       NOTE: C_LEN will be 72

    pure function without_first_character_c(string) result(trimmed)
        character(len=*), intent(in) :: string
        type(varying_string) :: trimmed

        trimmed = string(2:)
    end function

    pure function without_first_character_s(string) result(trimmed)
        type(varying_string), intent(in) :: string
        type(varying_string) :: trimmed

        trimmed = without_first_character(char(string))
    end function

    pure function without_last_character_c(string) result(trimmed)
        character(len=*), intent(in) :: string
        type(varying_string) :: trimmed

        trimmed = string(1:len(string) - 1)
    end function

    pure function without_last_character_s(string) result(trimmed)
        type(varying_string), intent(in) :: string
        type(varying_string) :: trimmed

        trimmed = without_last_character(char(string))
    end function
end module
