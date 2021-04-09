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

        ! character(len=64) :: tmp
        character(len=:), allocatable :: tmp

        tmp = ''

        write (tmp, '(i0)') val

        ! json = json//'"'//key//'":'//trim(tmp)//','
        ! json = trim(tmp)

    end subroutine json_add_integer_number

    subroutine json_test(json, key, val)
        character(len=:), allocatable, intent(inout) :: json
        character(len=*), intent(in) :: key
        integer, intent(in) :: val

        CHARACTER*80 TMP

        write (FMT='(i0)', UNIT=TMP) val

        print *, key, ':', val, trim(TMP)
        json = json//key

    end subroutine json_test

end module json_for
