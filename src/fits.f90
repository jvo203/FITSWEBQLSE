module fits
    implicit none

    ! scalar coarray, one "filepath" for each image
    ! character(len=1024) :: fits_uri[*]

    ! co-array variables to be synchronised across all images
    ! will be held in a structure (TO-DO)
    real(kind=4) :: dmin[*], dmax[*]
    logical bSuccess[*]
contains
    subroutine load_fits_file(filename) ! , dmin, dmax, bSuccess)
        implicit none
        character(len=1024), intent(in) :: filename
        !real(kind=4), intent(out) :: dmin, dmax
        !logical, intent(out) ::  bSuccess

        dmin = 1.0E30
        dmax = -1.0E30
        bSuccess = .false.

        ! call read_fits_file(filename, dmin, dmax, bSuccess)
        call co_reduce(bSuccess, logical_and)

        if (bSuccess) then
            call co_min(dmin)
            call co_max(dmax)

            if (this_image() == 1) then
                print *, 'image # ', this_image(), 'dmin:', dmin, 'dmax:', dmax
            end if
        end if

    end subroutine load_fits_file

    pure function logical_and(a, b)
        logical, value :: a, b
        logical :: logical_and

        logical_and = a .and. b
    end function logical_and

end module fits
