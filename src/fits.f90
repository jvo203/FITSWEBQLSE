module fits
    implicit none

    ! scalar coarray, one "filepath" for each image
    ! character(len=1024) :: fits_uri[*]

    ! co-array variables to be synchronised across all images
    ! will be held in a structure (TO-DO)
    real(kind=4) :: dmin[*], dmax[*]
    logical bSuccess[*]
contains
    subroutine load_fits_file(filename)
        implicit none
        character(len=1024), intent(in) :: filename

        call read_fits_file(filename, dmin, dmax, bSuccess)
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

    subroutine read_fits_file(filename, dmin, dmax, bSuccess)
        implicit none
        character(len=1024), intent(in) :: filename
        real(kind=4), intent(out) :: dmin, dmax
        logical, intent(out) ::  bSuccess

        integer status, group, unit, readwrite, blocksize, nkeys, nspace, hdutype, i, j
        integer naxes(3), naxis
        integer npixels
        integer width, height, depth
        integer tid, start, end, num_per_image, frame, firstpix

        real(kind=4), allocatable :: buffer(:)
        ! integer(kind=4), allocatable, target :: pixels(:, :)

        real :: nullval, tmp
        character record*80
        logical :: anynull

        naxis = 0
        naxes = (/0, 0, 0/)
        bSuccess = .false.

        ! The STATUS parameter must always be initialized.
        status = 0

        ! Get an unused Logical Unit Number to use to open the FITS file.
        call ftgiou(unit, status)

        if (status .ne. 0) then
            return
        end if

        ! open the FITS file, with read - only access.The returned BLOCKSIZE
        ! parameter is obsolete and should be ignored.
        readwrite = 0
        call ftopen(unit, filename, readwrite, blocksize, status)

        if (status .ne. 0) then
            return
        end if

        j = 0
100     continue
        j = j + 1

        if (this_image() == 1) then
            ! print *, 'Header listing for HDU', j
        end if

        ! The FTGHSP subroutine returns the number of existing keywords in the
        ! current header data unit(CHDU), not counting the required END keyword,
        call ftghsp(unit, nkeys, nspace, status)

        ! Read each 80 - character keyword record, and print it out.
        do i = 1, nkeys
            call ftgrec(unit, i, record, status)
            if (this_image() == 1) then
                ! print *, record
            end if
        end do

        ! Print out an END record, and a blank line to mark the end of the header.
        if (status .eq. 0) then
            if (this_image() == 1) then
                ! print *, 'END'
                ! print *, ' '
            end if
        end if

        ! Try moving to the next extension in the FITS file, if it exists.
        ! The FTMRHD subroutine attempts to move to the next HDU, as specified by
        ! the second parameter.This subroutine moves by a relative number of
        ! HDUs from the current HDU.The related FTMAHD routine may be used to
        ! move to an absolute HDU number in the FITS file.If the end - of - file is
        ! encountered when trying to move to the specified extension, then a
        ! status = 107 is returned.

        !  Determine the size of the image.
        call ftgknj(unit, 'NAXIS', 1, 3, naxes, naxis, status)

        if (status .ne. 0) then
            ! do this only if naxis is still 0
            call ftmrhd(unit, 1, hdutype, status)

            if (status .eq. 0) then
                ! success, so jump back and print out keywords in this extension
                go to 100

            else if (status .eq. 107) then
                ! hit end of file, so quit
                status = 0
            end if
        end if

        !  Check that it found at least both NAXIS1 and NAXIS2 keywords.
        if (naxis .lt. 2) then
            if (this_image() == 1) then
                print *, 'READIMAGE failed to read the NAXISn keywords.'
            end if
        end if

        if (this_image() == 1) then
            print *, 'NAXIS:', naxis, 'NAXIS:', naxes
        end if

        width = naxes(1)
        height = naxes(2)
        depth = naxes(3)

        group = 1
        nullval = 0.0

        dmin = 1.0E30
        dmax = -1.0E30

        ! allocate the buffer
        npixels = width*height

        ! now read the 3D FITS data cube (successive 2D planes)
        if (npixels .eq. 0) then
            ! skip memory allocation / reading
            go to 200
        end if

        ! float32
        allocate (buffer(npixels))

        ! calculate the range for each image
        if (naxis .eq. 2 .or. depth .eq. 1) then
            ! read one 2D image only on the first image
            if (this_image() == 1) then
                ! put data into the <pixels> 1D array
            end if
        else
            ! read a range of 2D planes on each image
            tid = this_image()
            num_per_image = depth/num_images()
            start = 1 + (tid - 1)*num_per_image
            end = min(tid*num_per_image, depth)

            ! print *, 'tid:', tid, 'start:', start, 'end:', end
            do frame = start, end
                ! set the starting point
                firstpix = 1 + (frame - 1)*npixels
                ! read the entire 2D plane at once
                call ftgpve(unit, group, firstpix, npixels, nullval, buffer, anynull, status)
                ! abort upon an error
                if (status .ne. 0) go to 200

                ! calculate the min/max values
                do j = 1, npixels
                    tmp = buffer(j)
                    if (isnan(tmp) .ne. .true.) then
                        dmin = min(dmin, tmp)
                        dmax = max(dmax, tmp)
                    end if
                end do
            end do
        end if

        bSuccess = .true.

        ! The FITS file must always be closed before exiting the program.
        ! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
200     call ftclos(unit, status)
        call ftfiou(unit, status)

        ! Check for any error, and if so print out error messages.
        ! The PRINTERROR subroutine is listed near the end of this file.
        if (status .gt. 0) then
            call printerror(status)
            return
        end if
    end subroutine read_fits_file

    !*************************************************************************
    subroutine printerror(status)

        ! This subroutine prints out the descriptive text corresponding to the
        ! error status value and prints out the contents of the internal
        ! error message stack generated by FITSIO whenever an error occurs.

        integer status
        character errtext*30, errmessage*80

        ! Check if status is OK(no error); if so, simply return
        if (status .le. 0) return

        ! The FTGERR subroutine returns a descriptive 30 - character text string that
        ! corresponds to the integer error status number.A complete list of all
        ! the error numbers can be found in the back of the FITSIO User's Guide.
        call ftgerr(status, errtext)
        print *, 'FITSIO Error Status =', status, ': ', errtext

        ! FITSIO usually generates an internal stack of error messages whenever
        ! an error occurs.These messages provide much more information on the
        ! cause of the problem than can be provided by the single integer error
        ! status value.The FTGMSG subroutine retrieves the oldest message from
        ! the stack and shifts any remaining messages on the stack down one
        ! position.FTGMSG is called repeatedly until a blank message is
        ! returned, which indicates that the stack is empty.Each error message
        ! may be up to 80 characters in length.Another subroutine, called
        ! FTCMSG, is available to simply clear the whole error message stack in
        ! cases where one is not interested in the contents.
        call ftgmsg(errmessage)
        do while (errmessage .ne. ' ')
            print *, errmessage
            call ftgmsg(errmessage)
        end do
    end subroutine printerror

end module fits
