module fits
    implicit none

    type dataset
        ! the id will be made by hashing the dataset uri
        integer :: id = -1
        integer :: unit = -1! a FITS file handle

        ! FITS header values
        integer :: naxis = 0
        integer :: bitpix = 0
        integer naxes(4)
        character frameid*70
        character btype*70, bunit*70
        real ignrval
        real crval1, cdelt1, crpix1
        real crval2, cdelt2, crpix2
        real crval3, cdelt3, crpix3

        ! derived values
        real(kind=4) dmin, dmax
        real(kind=4), allocatable :: frame_min(:), frame_max(:)
        real(kind=4), allocatable :: pixels(:, :)
        logical(kind=1), allocatable :: mask(:, :)
        logical :: error = .false.
    end type dataset

    ! only one FITS dataset at this development stage
    type(dataset) :: item

    ! scalar coarray, one "filepath" for each image
    ! character(len=1024) :: fits_uri[*]

    ! co-array variables to be synchronised across all images
    ! to be held in a structure <dataset>
    real(kind=4) :: dmin[*], dmax[*]
    logical bSuccess[*]
contains
    subroutine print_dataset
        print *, trim(item%frameid), ', BTYPE: ', trim(item%btype), ', BUNIT: ', trim(item%bunit), ', IGNRVAL:', item%ignrval
        print *, 'CRVAL1: ', item%crval1, ', CDELT1: ', item%cdelt1, ', CRPIX1: ', item%crpix1
        print *, 'CRVAL2: ', item%crval2, ', CDELT2: ', item%cdelt2, ', CRPIX2: ', item%crpix2
        print *, 'CRVAL3: ', item%crval3, ', CDELT3: ', item%cdelt3, ', CRPIX3: ', item%crpix3
    end subroutine print_dataset

    subroutine load_fits_file(filename)
        implicit none
        character(len=1024), intent(in) :: filename

        integer(8) :: start, finish, crate, cmax
        real :: elapsed

        item%id = hash(filename)

        ! start the timer
        call system_clock(count=start, count_rate=crate, count_max=cmax)

        call read_fits_file(filename, dmin, dmax, bSuccess)
        call co_reduce(bSuccess, logical_and)

        if (bSuccess) then
            call co_min(dmin)
            call co_max(dmax)

            ! end the timer
            call system_clock(finish)
            elapsed = real(finish - start)/real(crate)

            if (this_image() == 1) then
                print *, 'image # ', this_image(), 'dmin:', dmin, 'dmax:', dmax,&
                & 'elapsed:', elapsed, '[s]'
                print *, 'id:', item%id, 'error:', item%error, 'pixels:', shape(item%pixels), 'mask:', shape(item%mask)
                call print_dataset
            end if
        end if

    end subroutine load_fits_file

    pure function logical_and(a, b)
        logical, value :: a, b
        logical :: logical_and

        logical_and = a .and. b

    end function logical_and

    !======================================================================
    !    IDX$HASH
    ! Author:
    ! Alan Miller, Retired Scientist (Statistician)
    ! CSIRO Mathematical & Information Sciences
    ! Alan.Miller -at- vic.cmis.csiro.au
    ! http://www.ozemail.com.au/~milleraj
    !======================================================================
    FUNCTION hash(text) RESULT(hashed)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: text
        INTEGER                       :: hashed     ! 32-bit integers assumed

        INTEGER, PARAMETER :: magic_numb = Z'5D7A9F43'
        INTEGER            :: i, j

        hashed = 0                              ! Note: B was not initialized before
        DO i = 1, LEN_TRIM(text)
            j = MOD(i - 1, 4)*8
            hashed = IEOR(hashed, ISHFT(ICHAR(text(i:i)), j))
        END DO

        hashed = ABS(IEOR(hashed, magic_numb))    ! Why take the absolute value?

        RETURN
    END FUNCTION hash

    subroutine read_fits_file(filename, dmin, dmax, bSuccess)
        implicit none
        character(len=1024), intent(in) :: filename
        real(kind=4), intent(out) :: dmin, dmax
        logical, intent(out) ::  bSuccess
        logical :: tid_bSuccess

        integer status, group, unit, readwrite, blocksize, nkeys, nspace, hdutype, i, j
        integer naxis, bitpix
        integer npixels
        integer naxes(4)
        integer(kind=8) firstpix, npixels_per_image
        integer tid, start, end, num_per_image, frame
        integer, dimension(4) :: fpixels, lpixels, incs

        real(kind=4), allocatable :: buffer(:)
        logical(kind=1), allocatable :: mask(:)

        real :: nullval, tmp
        character :: record*80, key*10, value*70, comment*70
        logical :: anynull

        naxis = 0
        naxes = (/0, 0, 0, 0/)
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

            ! split the record into a key and a value
            key = record(1:10)
            value = record(11:80)

            if (this_image() == 1) then
                ! print *, record
                ! print *, key, '-->', value
            end if
        end do

        ! Print out an END record, and a blank line to mark the end of the header.
        if (status .eq. 0) then
            if (this_image() == 1) then
                ! print *, 'END'
                ! print *, ' '
            end if
        end if

        status = 0; call FTGKYS(unit, 'FRAMEID', item%frameid, comment, status)
        status = 0; call FTGKYS(unit, 'BTYPE', item%btype, comment, status)
        status = 0; call FTGKYS(unit, 'BUNIT', item%bunit, comment, status)
        status = 0; call FTGKYE(unit, 'IGNRVAL', item%ignrval, comment, status)
        status = 0; call FTGKYE(unit, 'CRVAL1', item%crval1, comment, status)
        status = 0; call FTGKYE(unit, 'CDELT1', item%cdelt1, comment, status)
        status = 0; call FTGKYE(unit, 'CRPIX1', item%crpix1, comment, status)
        status = 0; call FTGKYE(unit, 'CRVAL2', item%crval2, comment, status)
        status = 0; call FTGKYE(unit, 'CDELT2', item%cdelt2, comment, status)
        status = 0; call FTGKYE(unit, 'CRPIX2', item%crpix2, comment, status)
        status = 0; call FTGKYE(unit, 'CRVAL3', item%crval3, comment, status)
        status = 0; call FTGKYE(unit, 'CDELT3', item%cdelt3, comment, status)
        status = 0; call FTGKYE(unit, 'CRPIX3', item%crpix3, comment, status)

        ! Try moving to the next extension in the FITS file, if it exists.
        ! The FTMRHD subroutine attempts to move to the next HDU, as specified by
        ! the second parameter.This subroutine moves by a relative number of
        ! HDUs from the current HDU.The related FTMAHD routine may be used to
        ! move to an absolute HDU number in the FITS file.If the end - of - file is
        ! encountered when trying to move to the specified extension, then a
        ! status = 107 is returned.

        ! reset the status
        status = 0

        !  Determine the size of the data cube
        ! new subroutines (! LL for kind=8)
        call FTGIPR(unit, 4, bitpix, naxis, naxes, status)

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

            go to 200
        end if

        item%bitpix = bitpix
        item%naxis = naxis
        item%naxes = naxes

        if (this_image() == 1) then
            print *, 'BITPIX:', bitpix, 'NAXIS:', naxis, 'NAXES:', naxes
        end if

        group = 1
        nullval = 0.0

        dmin = 1.0E30
        dmax = -1.0E30

        ! allocate the buffer
        npixels = naxes(1)*naxes(2)

        ! now read the 3D FITS data cube (successive 2D planes)
        if (npixels .eq. 0) then
            ! skip memory allocation / reading
            go to 200
        end if

        ! calculate the range for each image
        if (naxis .eq. 2 .or. naxes(3) .eq. 1) then
            ! read one 2D image only on the first image
            ! not so, do it on all images

            allocate (buffer(npixels))
            allocate (mask(npixels))

            call ftgpve(unit, group, 1, npixels, nullval, buffer, anynull, status)

            ! abort upon an error
            if (status .ne. 0) go to 200

            ! calculate the min/max values
            do j = 1, npixels
                tmp = buffer(j)
                if (isnan(tmp) .ne. .true.) then
                    dmin = min(dmin, tmp)
                    dmax = max(dmax, tmp)
                    mask(j) = .true.
                else
                    mask(j) = .false.
                end if
            end do

            ! update the FITS dataset (taking advantage of automatic reallocation)
            item%pixels = reshape(buffer, naxes(1:2))
            item%mask = reshape(mask, naxes(1:2))
        else
            ! read a range of 2D planes in parallel on each image
            tid = this_image()
            num_per_image = naxes(3)/num_images()
            start = 1 + (tid - 1)*num_per_image
            end = min(tid*num_per_image, naxes(3))
            num_per_image = end - start + 1
            !print *, 'tid:', tid, 'start:', start, 'end:', end, 'num_per_image:', num_per_image

            block
                real(kind=4), allocatable :: pixels(:)

                ! npixels_per_image = npixels*num_per_image
                allocate (buffer(npixels))
                allocate (pixels(npixels))
                allocate (mask(npixels))

                ! initiate pixels to blank
                pixels = 0.0
                ! and reset the NaN mask
                mask = .false.

                do frame = start, end
                    ! starting bounds
                    fpixels = (/1, 1, frame, 1/)

                    ! ending bounds
                    lpixels = (/naxes(1), naxes(2), frame, 1/)

                    ! do not skip over any pixels
                    incs = 1

                    ! skip the do loop, make one call to ftgsve instead
                    call ftgsve(unit, group, naxis, naxes, fpixels, lpixels, incs, nullval, buffer, anynull, status)

                    ! abort upon an error
                    if (status .ne. 0) go to 200

                    ! calculate the min/max values
                    do j = 1, npixels
                        tmp = buffer(j)
                        if (isnan(tmp) .ne. .true.) then
                            dmin = min(dmin, tmp)
                            dmax = max(dmax, tmp)

                            ! integrate (sum up) pixels and a NaN mask
                            pixels(j) = pixels(j) + tmp
                            mask(j) = mask(j) .or. .true.
                        else
                            mask(j) = mask(j) .or. .false.
                        end if
                    end do
                end do

                ! update the FITS dataset (taking advantage of automatic reallocation)
                item%pixels = reshape(pixels, naxes(1:2))
                item%mask = reshape(mask, naxes(1:2))
            end block
        end if

        bSuccess = .true.
        item%unit = unit
        return

        ! The FITS file must always be closed before exiting the program.
        ! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
200     call ftclos(unit, status)
        call ftfiou(unit, status)

        item%error = .true.

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
