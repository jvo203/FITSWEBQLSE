module json_for
    use iso_varying_string
    implicit none

contains
    subroutine json_add_string(json, string)
        type(varying_string), intent(inout) :: json
        character(len=*), intent(in) :: string

        json = json//string

    end subroutine json_add_string
end module json_for
