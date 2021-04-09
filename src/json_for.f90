module json_for
    implicit none

    ! character(len=*), parameter :: form = '(E30.16E3)'
    character(len=*), parameter :: form = '(G24.16)'
contains
    !elemental logical function isnan(x)
    !    real(kind=4), intent(in) :: x

    !    if (abs(x)*0.0 /= 0.0) then
    !        isnan = .true.
    !    else
    !        isnan = .false.
    !    end if

    !end function isnan

    subroutine json_add_integer_number(json, key, val)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: key
        integer, intent(in) :: val

        character(len=64) :: tmp

        tmp = ''

        write (tmp, '(i0)') val

        json = json//'"'//key//'":'//trim(tmp)//','

    end subroutine json_add_integer_number

end module json_for
