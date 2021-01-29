module fits
    use, intrinsic :: ieee_arithmetic
    implicit none

    integer(kind=4), parameter :: NBINS = 1024

    type dataset
        ! the id will be made by hashing the dataset uri
        integer :: id = -1
        integer :: unit = -1! a FITS file handle

        ! FITS header values
        integer :: naxis = 0
        integer :: bitpix = 0
        integer naxes(4)
        character frameid*70, object*70, line*70, filter*70, date_obs*70
        character btype*70, bunit*70, specsys*70, timesys*70
        character cunit1*70, ctype1*70
        character cunit2*70, ctype2*70
        character cunit3*70, ctype3*70
        real :: ignrval, restfrq, bmaj, bmin, bpa
        real crval1, cdelt1, crpix1
        real crval2, cdelt2, crpix2
        real crval3, cdelt3, crpix3
        real obsra, obsdec, datamin, datamax
        real cd1_1, cd1_2, cd2_1, cd2_2

        ! extras
        real :: frame_multiplier = 1.0
        logical :: has_velocity = .false.
        logical :: has_frequency = .false.

        ! the 2D image histogram
        integer hist(NBINS)

        ! derived values
        character(len=16) :: flux = ''
        real(kind=4) dmin, dmax
        real(kind=4), allocatable :: frame_min(:), frame_max(:)
        real(kind=4), allocatable :: pixels(:, :)
        logical(kind=1), allocatable :: mask(:, :)
        logical :: is_optical = .true.
        logical :: is_xray = .false.
        logical :: error = .false.
    end type dataset

    ! only one FITS dataset at this development stage
    type(dataset) :: item

    ! scalar coarray, one "filepath" for each image
    ! character(len=1024) :: fits_uri[*]

    ! co-array variables to be synchronised across all images
    ! to be held in a structure <dataset>
    ! moved to the <load_fits_file> subroutine
contains
    subroutine print_dataset
        print *, trim(item%frameid), ', BTYPE: ', trim(item%btype), ', BUNIT: ', trim(item%bunit), ', IGNRVAL:', item%ignrval
        print *, 'LINE: ', trim(item%line), ', FILTER: ', trim(item%filter),&
        & ', SPECSYS: ', trim(item%specsys), ', TIMESYS: ', trim(item%timesys),&
        & ', OBJECT: ', trim(item%object), ', DATE-OBS: ', trim(item%date_obs)
        print *, 'RESTFRQ: ', item%restfrq, 'BMAJ: ', item%bmaj, ', BMIN: ', item%bmin, ', BPA: ', item%bpa
        print *, 'OBSRA: ', item%obsra, 'OBSDEC: ', item%obsdec, ', DATAMIN: ', item%datamin, ', DATAMAX: ', item%datamax
        print *, 'CRVAL1: ', item%crval1, ', CDELT1: ', item%cdelt1, ', CRPIX1: ', item%crpix1
        print *, 'CRVAL2: ', item%crval2, ', CDELT2: ', item%cdelt2, ', CRPIX2: ', item%crpix2
        print *, 'CRVAL3: ', item%crval3, ', CDELT3: ', item%cdelt3, ', CRPIX3: ', item%crpix3
        print *, 'CUNIT1: ', trim(item%cunit1), ', CTYPE1: ', trim(item%ctype1)
        print *, 'CUNIT2: ', trim(item%cunit2), ', CTYPE2: ', trim(item%ctype2)
        print *, 'CUNIT3: ', trim(item%cunit3), ', CTYPE3: ', trim(item%ctype3)
        print *, 'CD1_1: ', item%cd1_1, 'CD1_2: ', item%cd1_2
        print *, 'CD2_1: ', item%cd2_1, 'CD2_2: ', item%cd2_2
        print *, 'IS_OPTICAL: ', item%is_optical, ', IS_XRAY: ', item%is_xray, ', FLUX: ', trim(item%flux)
        print *, 'has_frequency:', item%has_frequency,&
        & ', has_velocity:', item%has_velocity,&
        & ', frame_multiplier = ', item%frame_multiplier
    end subroutine print_dataset

    subroutine load_fits_file(filename)
        implicit none
        character(len=1024), intent(in) :: filename

        real(kind=4), save :: dmin[*], dmax[*]
        logical, save :: bSuccess[*]

        real(kind=4), allocatable :: pixels(:) [:]
        logical(kind=1), allocatable :: mask(:) [:]

        integer(8) :: start, finish, crate, cmax
        real :: elapsed

        item%id = hash(filename)

        ! start the timer
        call system_clock(count=start, count_rate=crate, count_max=cmax)

        call read_fits_file(filename, dmin, dmax, pixels, mask, bSuccess)
        call co_reduce(bSuccess, logical_and)

        if (bSuccess) then
            call co_min(dmin)
            call co_max(dmax)

            item%dmin = dmin
            item%dmax = dmax

            ! synchronise the pixels/mask across the images (gather on image 1)
            if (item%naxis .gt. 2 .and. item%naxes(3) .gt. 1) then
                call co_sum(pixels, result_image=1)
                call co_reduce(mask, logical_or, result_image=1)

                if (this_image() == 1) then
                    ! update the FITS dataset (taking advantage of automatic reallocation)
                    item%pixels = reshape(pixels, item%naxes(1:2))
                    item%mask = reshape(mask, item%naxes(1:2))
                    print *, 'gathered {pixels,mask} on image', this_image()
                end if
            end if

            ! make an image histogram, decide on the flux etc.
            if (this_image() == 1) call make_image_statistics

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

    pure function logical_or(a, b)
        logical(kind=1), value :: a, b
        logical(kind=1) :: logical_or

        logical_or = a .or. b

    end function logical_or

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

    elemental subroutine lower_case(word)
        ! convert a word to lower case
        character(len=*), intent(in out) :: word
        integer                            :: i, ic, nlen
        nlen = len(word)
        do i = 1, nlen
            ic = ichar(word(i:i))
            if (ic >= 65 .and. ic < 90) word(i:i) = char(ic + 32)
        end do
    end subroutine lower_case

    subroutine read_fits_file(filename, dmin, dmax, pixels, mask, bSuccess)
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
        real(kind=4), allocatable :: pixels(:) [:]
        logical(kind=1), allocatable :: mask(:) [:]

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
        block
            integer pos

            do i = 1, nkeys
                call ftgrec(unit, i, record, status)

                pos = index(record, 'ASTRO-F')
                if (pos .ne. 0) then
                    item%is_optical = .true.
                    item%flux = 'logistic'
                end if

                pos = index(record, 'HSCPIPE')
                if (pos .ne. 0) then
                    item%is_optical = .true.
                    item%flux = 'ratio'
                end if

                if (this_image() == 1) then
                    ! split the record into a key and a value
                    key = record(1:10)
                    value = record(11:80)

                    ! print *, record
                    ! print *, key, '-->', value
                end if

                call lower_case(record)

                pos = index(record, 'suzaku')
                if (pos .ne. 0) go to 110

                pos = index(record, 'hitomi')
                if (pos .ne. 0) go to 110

                pos = index(record, 'x-ray')
                if (pos .ne. 0) go to 110

                cycle

                ! enable X-RAY settings
110             block
                    item%is_optical = .false.
                    item%is_xray = .true.
                    item%flux = 'legacy'
                    item%ignrval = -1.0
                end block

            end do

        end block

        ! Print out an END record, and a blank line to mark the end of the header.
        if (status .eq. 0) then
            if (this_image() == 1) then
                ! print *, 'END'
                ! print *, ' '
            end if
        end if

        status = 0; call FTGKYS(unit, 'FRAMEID', item%frameid, comment, status)

        ! further examine the datasetid for any hints
        if (status .eq. 0) then
            block
                integer pos

                pos = index(item%frameid, 'SUPM')
                if (pos .ne. 0) then
                    item%is_optical = .true.
                    item%flux = 'ratio'
                end if

                pos = index(item%frameid, 'MCSM')
                if (pos .ne. 0) then
                    item%is_optical = .true.
                    item%flux = 'ratio'
                end if
            end block
        end if

        status = 0; call FTGKYS(unit, 'BTYPE', item%btype, comment, status)

        status = 0; call FTGKYS(unit, 'BUNIT', item%bunit, comment, status)

        status = 0; call FTGKYE(unit, 'IGNRVAL', item%ignrval, comment, status)
        if (status .ne. 0 .and. .not. item%is_xray) item%ignrval = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRVAL1', item%crval1, comment, status)
        if (status .ne. 0) item%crval1 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CDELT1', item%cdelt1, comment, status)
        if (status .ne. 0) item%cdelt1 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRPIX1', item%crpix1, comment, status)
        if (status .ne. 0) item%crpix1 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRVAL2', item%crval2, comment, status)
        if (status .ne. 0) item%crval2 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CDELT2', item%cdelt2, comment, status)
        if (status .ne. 0) item%cdelt2 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRPIX2', item%crpix2, comment, status)
        if (status .ne. 0) item%crpix2 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRVAL3', item%crval3, comment, status)
        if (status .ne. 0) item%crval3 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CDELT3', item%cdelt3, comment, status)
        if (status .ne. 0) item%cdelt3 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CRPIX3', item%crpix3, comment, status)
        if (status .ne. 0) item%crpix3 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'BMAJ', item%bmaj, comment, status)
        if (status .ne. 0) item%bmaj = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'BMIN', item%bmin, comment, status)
        if (status .ne. 0) item%bmin = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'BPA', item%bpa, comment, status)
        if (status .ne. 0) item%bpa = ieee_value(0.0, ieee_quiet_nan)

        ! either keyword is valid
        status = 0; call FTGKYE(unit, 'RESTFRQ', item%restfrq, comment, status)
        status = 0; call FTGKYE(unit, 'RESTFREQ', item%restfrq, comment, status)

        status = 0; call FTGKYE(unit, 'OBSRA', item%obsra, comment, status)
        if (status .ne. 0) item%obsra = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'OBSDEC', item%obsdec, comment, status)
        if (status .ne. 0) item%obsdec = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'DATAMIN', item%datamin, comment, status)
        if (status .ne. 0) item%datamin = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'DATAMAX', item%datamax, comment, status)
        if (status .ne. 0) item%datamax = ieee_value(0.0, ieee_quiet_nan)

        ! either keyword is valid
        status = 0; call FTGKYS(unit, 'LINE', item%line, comment, status)
        status = 0; call FTGKYS(unit, 'J_LINE', item%line, comment, status)

        status = 0; call FTGKYS(unit, 'FILTER', item%filter, comment, status)

        status = 0; call FTGKYS(unit, 'SPECSYS', item%specsys, comment, status)

        status = 0; call FTGKYS(unit, 'TIMESYS', item%timesys, comment, status)

        status = 0; call FTGKYS(unit, 'OBJECT', item%object, comment, status)

        status = 0; call FTGKYS(unit, 'DATE-OBS', item%date_obs, comment, status)

        status = 0; call FTGKYS(unit, 'CUNIT1', item%cunit1, comment, status)

        status = 0; call FTGKYS(unit, 'CUNIT2', item%cunit2, comment, status)

        status = 0; call FTGKYS(unit, 'CUNIT3', item%cunit3, comment, status)

        status = 0; call FTGKYS(unit, 'CTYPE1', item%ctype1, comment, status)

        status = 0; call FTGKYS(unit, 'CTYPE2', item%ctype2, comment, status)

        status = 0; call FTGKYS(unit, 'CTYPE3', item%ctype3, comment, status)

        status = 0; call FTGKYE(unit, 'CD1_1', item%cd1_1, comment, status)
        if (status .ne. 0) item%cd1_1 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CD1_2', item%cd1_2, comment, status)
        if (status .ne. 0) item%cd1_2 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CD2_1', item%cd2_1, comment, status)
        if (status .ne. 0) item%cd2_1 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYE(unit, 'CD2_2', item%cd2_2, comment, status)
        if (status .ne. 0) item%cd2_2 = ieee_value(0.0, ieee_quiet_nan)

        status = 0; call FTGKYS(unit, 'TELESCOP', value, comment, status)

        ! handle the telescope
        if (status .eq. 0) then
            ! first convert the value to lower case
            call lower_case(value)

            block
                integer pos

                pos = index(value, 'alma')
                if (pos .ne. 0) item%is_optical = .false.

                pos = index(value, 'vla')
                if (pos .ne. 0) item%is_optical = .false.

                pos = index(value, 'ska')
                if (pos .ne. 0) item%is_optical = .false.

                pos = index(value, 'nro45')
                if (pos .ne. 0) then
                    item%is_optical = .false.
                    item%flux = 'logistic'
                end if

                pos = index(value, 'chandra')
                if (pos .ne. 0) then
                    item%is_optical = .false.
                    item%is_xray = .true.
                end if
            end block

        end if

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

        ! detect the FITS header types and units (frequency, velocity)
        call frame_reference_type
        call frame_reference_unit

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
            allocate (mask(npixels) [*])

            call ftgpve(unit, group, 1, npixels, nullval, buffer, anynull, status)

            ! abort upon an error
            if (status .ne. 0) go to 200

            ! calculate the min/max values
            do j = 1, npixels
                tmp = buffer(j)
                if (isnan(tmp) .neqv. .true.) then
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
                ! real(kind=4), allocatable :: pixels(:)

                ! npixels_per_image = npixels*num_per_image
                allocate (buffer(npixels))
                allocate (pixels(npixels) [*])
                allocate (mask(npixels) [*])

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
                        if (isnan(tmp) .neqv. .true.) then
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

    subroutine frame_reference_type
        integer pos

        pos = index(item%ctype3, 'F')
        if (pos .ne. 0) item%has_frequency = .true.

        pos = index(item%ctype3, 'f')
        if (pos .ne. 0) item%has_frequency = .true.

        pos = index(item%ctype3, 'V')
        if (pos .ne. 0) item%has_velocity = .true.

        pos = index(item%ctype3, 'v')
        if (pos .ne. 0) item%has_velocity = .true.

    end subroutine frame_reference_type

    subroutine frame_reference_unit
        if (trim(item%cunit3) .eq. 'Hz') then
            item%has_frequency = .true.
            item%frame_multiplier = 1.0E0
            return
        end if

        if (trim(item%cunit3) .eq. 'kHz') then
            item%has_frequency = .true.
            item%frame_multiplier = 1.0E3
            return
        end if

        if (trim(item%cunit3) .eq. 'MHz') then
            item%has_frequency = .true.
            item%frame_multiplier = 1.0E6
            return
        end if

        if (trim(item%cunit3) .eq. 'GHz') then
            item%has_frequency = .true.
            item%frame_multiplier = 1.0E9
            return
        end if

        if (trim(item%cunit3) .eq. 'THz') then
            item%has_frequency = .true.
            item%frame_multiplier = 1.0E12
            return
        end if

        if (trim(item%cunit3) .eq. 'm/s') then
            item%has_velocity = .true.
            item%frame_multiplier = 1.0E0
            return
        end if

        if (trim(item%cunit3) .eq. 'km/s') then
            item%has_velocity = .true.
            item%frame_multiplier = 1.0E3
            return
        end if
    end subroutine frame_reference_unit

    subroutine make_image_statistics
        real cdelt3, pmin, pmax
        real pixel
        integer i, j

        if (item%has_velocity) then
            cdelt3 = item%cdelt3*item%frame_multiplier/1000.0
        else
            cdelt3 = 1.0
        end if

        if (item%naxis .eq. 2 .or. item%naxes(3) .eq. 1) then
            pmin = item%dmin
            pmax = item%dmax
        else
            pmin = 1.0E30
            pmax = -1.0E30

            do j = 1, item%naxes(2)
                do i = 1, item%naxes(1)
                    if (item%mask(i, j)) then
                        pixel = item%pixels(i, j)*cdelt3
                        item%pixels(i, j) = pixel

                        pmin = min(pmin, pixel)
                        pmax = max(pmax, pixel)
                    end if
                end do
            end do
        end if

        ! make_histogram with a range given by [pmin, pmax]
        call make_histogram(pmin, pmax)

        print *, 'image pixels range pmin = ', pmin, ', pmax = ', pmax

    end subroutine make_image_statistics

    subroutine make_histogram(pmin, pmax)
        real, intent(in) :: pmin, pmax

        ! reset the histogram
        item%hist = 0

    end subroutine make_histogram
end module fits
