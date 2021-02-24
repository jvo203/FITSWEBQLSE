module json_for
    use iso_varying_string
    implicit none

contains
    subroutine json_add_string(json, key, val)
        type(varying_string), intent(inout) :: json
        character(len=*), intent(in) :: key, val

        json = json//'"'//key//'":"'//val//'",'

    end subroutine json_add_string
end module json_for
