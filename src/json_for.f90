module json_for
    use iso_varying_string
    implicit none

contains
    subroutine json_add_string(json, key, val)
        type(varying_string), intent(inout) :: json
        character(len=*), intent(in) :: key, val

        character(len=:), allocatable :: tmp

        integer :: i, str_len
        character :: c

        json = json//'"'//key//'":"'

        str_len = len(val)

        ! allocate twice the amount of memory to be on a safe side
        ! assume that each source character might need escaping (the worst-case)
        allocate (character(2*str_len) :: tmp)

        tmp = ''

        ! go through the string escaping special characters
        do i = 1, str_len
            c = val(i:i)

            ! test for escape characters

            ! backspace
            if (c .eq. char(8)) then
                tmp = tmp//'\b'
                cycle
            end if

            ! form feed
            if (c .eq. char(12)) then
                tmp = tmp//'\f'
                cycle
            end if

            ! new line (line feed)
            if (c .eq. char(10)) then
                tmp = tmp//'\n'
                cycle
            end if

            ! carriage return
            if (c .eq. char(13)) then
                tmp = tmp//'\r'
                cycle
            end if

            ! horizontal tab
            if (c .eq. char(9)) then
                tmp = tmp//'\t'
                cycle
            end if

            ! double quote
            if (c .eq. '"') then
                tmp = tmp//'\"'
                cycle
            end if

            ! backslash
            if (c .eq. '\') then
                tmp = tmp//'\\'
                cycle
            end if

            ! OK, we got through the escape characters
            ! append the actual value
            tmp = tmp//c
        end do

        json = json//trim(tmp)//'",'

    end subroutine json_add_string

    subroutine json_add_integer_number(json, key, val)
        type(varying_string), intent(inout) :: json
        character(len=*), intent(in) :: key
        integer, intent(in) :: val

        character(32) :: tmp

        write (tmp, '(i0)') val

        json = json//'"'//key//'":'//trim(tmp)//','

    end subroutine json_add_integer_number

end module json_for
