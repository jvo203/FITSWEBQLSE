module json_for
    use iso_varying_string
    implicit none

contains
    subroutine json_add_string(json, key, val)
        type(varying_string), intent(inout) :: json
        character(len=*), intent(in) :: key, val
        integer :: i, str_len
        character :: c

        json = json//'"'//key//'" : "'

        str_len = len(val)

        ! go through the string character by character
        ! escaping special characters

        do i = 1, str_len
            c = val(i:i)
            json = json//c
        end do

        json = json//'",'

    end subroutine json_add_string
end module json_for
