module fits
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: ieee_arithmetic
    use iso_varying_string
    use fixed_array
    implicit none

    integer(kind=4), parameter :: NBINS = 1024
    real, parameter :: PI = 4.D0*DATAN(1.D0)

    type, bind(c) :: gmutex
        integer(kind=c_intptr_t) :: i = 0
    end type gmutex

    enum, bind(C)
        enumerator circle
        enumerator square
    end enum

    enum, bind(C)
        enumerator mean
        enumerator integrated
    end enum

    enum, bind(C)
        enumerator low
        enumerator medium
        enumerator high
    end enum

    type, bind(c) :: image_spectrum_request_f
        integer(kind=c_int) :: dx
        logical(kind=c_bool) :: image
        integer(kind(medium)) :: quality
        integer(c_int) :: x1, y1, x2, y2
        integer(c_int) :: width, height
        integer(kind(circle)) :: beam
        integer(kind(medium)) :: intensity
        real(c_double) :: frame_start, frame_end, ref_freq
        integer(c_int) :: seq_id
        real(c_float) :: timestamp
    end type image_spectrum_request_f

    type dataset
        character(kind=c_char), dimension(:), allocatable :: datasetid
        ! the id will be made by hashing the dataset uri
        integer :: unit = -1! a FITS file handle

        ! FITS header values
        character(kind=c_char), dimension(:), allocatable :: hdr
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

        ! derived values
        character(len=16) :: flux = ''
        real(kind=c_float) dmin, dmax
        real(kind=4), allocatable :: frame_min(:), frame_max(:)
        real(kind=c_float), allocatable :: pixels(:, :)
        logical(kind=c_bool), allocatable :: mask(:, :)
        logical :: is_optical = .true.
        logical :: is_xray = .false.

        logical :: error = .false.
        logical :: ok = .false.
        logical :: header = .false.

        ! mutexes
        type(gmutex) :: header_mtx, ok_mtx, error_mtx, progress_mtx

        ! 2D image statistics
        real(kind=c_float) pmin, pmax, pmedian
        real(kind=c_float) mad, madN, madP
        real(kind=c_float) black, white, sensitivity, ratio_sensitivity

        ! image histogram
        integer hist(NBINS)

        ! progress
        integer(8) :: start_time, crate, cmax
        real :: progress = 0
        real :: elapsed = 0

        ! spectra
        real(kind=c_float), allocatable :: mean_spectrum(:)
        real(kind=c_float), allocatable :: integrated_spectrum(:)

        ! compressed planes
        type(gmutex), allocatable :: channels_mtx(:)
        ! type(fixed_block), allocatable :: channels(:, :, :)
    contains
        final :: close_fits_file
    end type dataset

    ! scalar coarray, one "filepath" for each image
    ! character(len=1024) :: fits_uri[*]

    ! co-array variables to be synchronised across all images
    ! to be held in a structure <dataset>
    ! moved to the <load_fits_file> subroutine
    interface
        ! glib mutex functions

        ! void g_mutex_init (GMutex *mutex);
        subroutine g_mutex_init(mutex) BIND(C, name='g_mutex_init')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: mutex
        end subroutine g_mutex_init

        ! void g_mutex_clear (GMutex *mutex);
        subroutine g_mutex_clear(mutex) BIND(C, name='g_mutex_clear')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: mutex
        end subroutine g_mutex_clear

        ! void g_mutex_lock (GMutex *mutex);
        subroutine g_mutex_lock(mutex) BIND(C, name='g_mutex_lock')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: mutex
        end subroutine g_mutex_lock

        ! void g_mutex_unlock (GMutex *mutex);
        subroutine g_mutex_unlock(mutex) BIND(C, name='g_mutex_unlock')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: mutex
        end subroutine g_mutex_unlock

        ! glib hash table
        ! void insert_dataset(const char *datasetid, void *item);
        subroutine insert_dataset(datasetid, item) BIND(C, name='insert_dataset')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: datasetid(*)
            type(c_ptr), value :: item
        end subroutine insert_dataset

        ! void *get_dataset(const char *datasetid);
        type(c_ptr) function get_dataset(datasetid) BIND(C, name='get_dataset')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: datasetid(*)
        end function get_dataset

        ! parallel sort void psrs_sort(float *a, int n);
        subroutine psrs_sort(a, n) BIND(C, name='psrs_sort')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: a
            integer(kind=c_int), value, intent(in) :: n
        end subroutine psrs_sort

    end interface
contains
    subroutine close_fits_file(item)
        type(dataset) :: item
        integer status

        ! nothing to do if the FITS file has never been opened
        if (item%unit .eq. -1) return

        print *, 'image', this_image(), item%datasetid, ': closing the FITS file unit'

        call ftclos(item%unit, status)
        call ftfiou(item%unit, status)

    end subroutine close_fits_file

    subroutine delete_dataset(item) BIND(C, name='delete_dataset')
        type(C_PTR), intent(in), value :: item
        type(dataset), pointer :: item_ptr

        call c_f_pointer(item, item_ptr)

        print *, 'image', this_image(), 'deleting ', item_ptr%datasetid

        if (item_ptr%header_mtx%i .ne. 0) call g_mutex_clear(c_loc(item_ptr%header_mtx))
        if (item_ptr%error_mtx%i .ne. 0) call g_mutex_clear(c_loc(item_ptr%error_mtx))
        if (item_ptr%ok_mtx%i .ne. 0) call g_mutex_clear(c_loc(item_ptr%ok_mtx))
        if (item_ptr%progress_mtx%i .ne. 0) call g_mutex_clear(c_loc(item_ptr%progress_mtx))

        deallocate (item_ptr)
    end subroutine delete_dataset

    subroutine print_dataset(item)
        type(dataset), pointer, intent(in) :: item

        print *, 'datasetid:', item%datasetid, ', FRAMEID:', trim(item%frameid),&
        & ', BTYPE: ', trim(item%btype), ', BUNIT: ', trim(item%bunit), ', IGNRVAL:', item%ignrval
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

        print *, 'pmin:', item%pmin
        print *, 'pmax:', item%pmax
        print *, 'pmedian:', item%pmedian
        print *, 'black:', item%black
        print *, 'white:', item%white
        print *, 'sensitivity:', item%sensitivity
        print *, 'ratio_sensitivity:', item%ratio_sensitivity

        if (item%naxes(3) .gt. 1) then
            ! print *, 'frame_min:', item%frame_min
            ! print *, 'frame_max:', item%frame_max

            ! print *, 'mean spectrum:', item%mean_spectrum
            ! print *, 'integrated spectrum:', item%integrated_spectrum
        end if

    end subroutine print_dataset

    subroutine set_error_status(item, error)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: error

        ! lock the mutex
        call g_mutex_lock(c_loc(item%error_mtx))

        item%error = error

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%error_mtx))

    end subroutine set_error_status

    subroutine set_ok_status(item, ok)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: ok

        ! lock the mutex
        call g_mutex_lock(c_loc(item%ok_mtx))

        item%ok = ok

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%ok_mtx))

    end subroutine set_ok_status

    subroutine set_header_status(item, header)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: header

        ! lock the mutex
        call g_mutex_lock(c_loc(item%header_mtx))

        item%header = header

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%header_mtx))

    end subroutine set_header_status

    subroutine reset_clock(item)
        type(dataset), pointer, intent(inout) :: item

        ! lock the mutex
        call g_mutex_lock(c_loc(item%progress_mtx))

        call system_clock(item%start_time)

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))

    end subroutine reset_clock

    subroutine update_progress(item, progress, total)
        type(dataset), pointer, intent(inout) :: item
        integer, intent(in) :: progress, total
        integer(8) finish
        real elapsed

        ! take a time measurement
        call system_clock(finish)
        elapsed = real(finish - item%start_time)/real(item%crate)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%progress_mtx))

        item%progress = 100.0*progress/total
        item%elapsed = elapsed

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))

        ! print *, 'progress:', item%progress, '%, elapsed time ', item%elapsed, ' [s]'
    end subroutine update_progress

    integer(c_int) function get_error_status(item_ptr) bind(c)
        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item

        call c_f_pointer(item_ptr, item)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%error_mtx))

        if (item%error) then
            get_error_status = 1
        else
            get_error_status = 0
        end if

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%error_mtx))

        return
    end function get_error_status

    integer(c_int) function get_ok_status(item_ptr) bind(c)
        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item

        call c_f_pointer(item_ptr, item)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%ok_mtx))

        if (item%ok) then
            get_ok_status = 1
        else
            get_ok_status = 0
        end if

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%ok_mtx))

        return
    end function get_ok_status

    integer(c_int) function get_header_status(item_ptr) bind(c)
        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item

        call c_f_pointer(item_ptr, item)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%header_mtx))

        if (item%header) then
            get_header_status = 1
        else
            get_header_status = 0
        end if

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%header_mtx))

        return
    end function get_header_status

    real(c_float) function get_progress(item_ptr) bind(c)
        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item

        call c_f_pointer(item_ptr, item)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%progress_mtx))

        get_progress = item%progress

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))
    end function get_progress

    real(c_float) function get_elapsed(item_ptr) bind(c)
        type(C_PTR), intent(in), value :: item_ptr
        type(dataset), pointer :: item

        call c_f_pointer(item_ptr, item)

        ! lock the mutex
        call g_mutex_lock(c_loc(item%progress_mtx))

        get_elapsed = item%elapsed

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))
    end function get_elapsed

    function extract_datasetid(filename) result(datasetid)
        implicit none

        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: string
        character(kind=c_char), dimension(:), allocatable :: work, datasetid
        integer :: i, str_len, pos
        character :: c

        ! work from the end, processing characters one by one
        ! exit upon encountering the first '/'
        str_len = len(filename)

        ! allocate the full string length
        allocate (work(str_len))
        work = ''

        str_len = 0

        do i = len(filename), 1, -1
            c = filename(i:i)

            if (c .eq. ' ') cycle

            if (c .eq. '/') exit

            ! shift the character array by one to the right
            if (str_len .gt. 0) work(2:str_len + 1) = work(1:str_len)

            ! prepend the character c to the array
            work(1) = c

            ! increment the character array length
            str_len = str_len + 1
        end do

        allocate (character(len=str_len) :: string)

        do concurrent(i=1:str_len)
            string(i:i) = work(i)
        end do

        ! get rid of FITS file extensions
        ! should be able to handle .fits.gz etc... too

        ! lowercase, ignore starting positions .eq. 1
        i = index(string, '.fits')
        if (i .gt. 1) str_len = i - 1

        ! uppercase, ignore starting positions .eq. 1
        i = index(string, '.FITS')
        if (i .gt. 1) str_len = i - 1

        ! datasetid = reshape(work, (/str_len/))

        ! append the C string ending character so that datasetid can be passed to C
        allocate (datasetid(str_len + 1))
        datasetid(1:str_len) = work(1:str_len)
        datasetid(str_len + 1) = c_null_char
    end function extract_datasetid

    subroutine load_fits_file(filename)
        implicit none
        character(len=1024), intent(in) :: filename

        type(dataset), pointer :: item

        real(kind=4), save :: dmin[*], dmax[*]
        logical, save :: bSuccess[*]

        real(kind=4), allocatable :: pixels(:) [:]
        logical(kind=1), allocatable :: mask(:) [:]

        real, allocatable :: mean_spectrum(:) [:]
        real, allocatable :: integrated_spectrum(:) [:]

        integer(8) :: start, finish, crate, cmax, id
        real :: elapsed

        allocate (item)

        ! init mutexes
        if (item%header_mtx%i .eq. 0) call g_mutex_init(c_loc(item%header_mtx))
        if (item%error_mtx%i .eq. 0) call g_mutex_init(c_loc(item%error_mtx))
        if (item%ok_mtx%i .eq. 0) call g_mutex_init(c_loc(item%ok_mtx))
        if (item%progress_mtx%i .eq. 0) call g_mutex_init(c_loc(item%progress_mtx))

        item%datasetid = extract_datasetid(filename)
        item%progress = 0
        item%elapsed = 0
        call set_ok_status(item, .false.)
        call set_error_status(item, .false.)
        call set_header_status(item, .false.)

        call insert_dataset(item%datasetid, c_loc(item))

        ! start the timer
        call system_clock(count=start, count_rate=crate, count_max=cmax)

        call read_fits_file(item, filename, dmin, dmax, pixels, mask, mean_spectrum, integrated_spectrum, bSuccess)
        call co_reduce(bSuccess, logical_and)

        if (this_image() == 1) print *, 'bSuccess:', bSuccess

        if (bSuccess) then
            call co_min(dmin)
            call co_max(dmax)

            item%dmin = dmin
            item%dmax = dmax

            ! synchronise the pixels/mask/spectra across the images
            if (item%naxis .eq. 2 .or. item%naxes(3) .eq. 1) then
                ! depth == 1
                item%pixels = reshape(pixels, item%naxes(1:2))
                item%mask = reshape(mask, item%naxes(1:2))

                if (this_image() == 1) then
                    print *, 'synchronised {pixels,mask}'
                end if
            else
                ! depth > 1
                call co_sum(pixels, result_image=1)
                call co_reduce(mask, logical_or, result_image=1)

                item%pixels = reshape(pixels, item%naxes(1:2))
                item%mask = reshape(mask, item%naxes(1:2))

                ! only the root image needs the spectrum information
                call co_sum(mean_spectrum, result_image=1)
                call co_sum(integrated_spectrum, result_image=1)

                if (this_image() == 1) then
                    ! update the FITS dataset (taking advantage of automatic reallocation)
                    item%mean_spectrum = reshape(mean_spectrum, item%naxes(3:3))
                    item%integrated_spectrum = reshape(integrated_spectrum, item%naxes(3:3))

                    print *, 'synchronised {pixels,mask, mean/integrated spectrum}'
                end if
            end if

            ! make an image histogram, decide on the flux etc.
            if (this_image() == 1) call make_image_statistics(item)

            call set_ok_status(item, .true.)

            ! end the timer
            call system_clock(finish)
            elapsed = real(finish - start)/real(crate)

            if (this_image() == 1) then
                print *, 'image # ', this_image(), 'dmin:', dmin, 'dmax:', dmax,&
                & 'elapsed:', elapsed, '[s]'
                print *, item%datasetid, ': error:', item%error, 'pixels:', shape(item%pixels), 'mask:', shape(item%mask)
                call print_dataset(item)
            end if

            ! reset the timeout clock
            call reset_clock(item)
        end if

    end subroutine load_fits_file

    subroutine handle_realtime_image_spectrum(cmd)
        implicit none
        character, intent(in) :: cmd(:)

        character(1024) :: buffer

        character(kind=c_char), dimension(:), allocatable :: datasetid
        integer :: i, str_len, new_len

        type(c_ptr) :: item_ptr
        type(dataset), pointer :: item
        type(image_spectrum_request_f) :: req

        ! co-array variables
        real(kind=4), allocatable :: pixels(:) [:]
        logical(kind=1), allocatable :: mask(:) [:]
        real, allocatable :: spectrum(:) [:]
        logical, save :: bSuccess[*]

        ! calculation range
        integer :: first, last

        bSuccess = .true.

        str_len = size(cmd)

        ! work from the end, processing characters one by one
        ! exit upon encountering the first blank ' '
        do i = str_len, 1, -1
            if (cmd(i) .eq. ' ') exit
        end do

        ! move forward by one
        i = i + 1

        ! exit if we have overstepped the original command length
        if (i .gt. str_len) then
            bSuccess = .false.
        else
            new_len = str_len - i + 1

            ! allocate enough space for the id plus a C string ending character
            allocate (datasetid(new_len + 1))

            datasetid(1:new_len) = cmd(i:str_len)
            datasetid(new_len + 1) = c_null_char

            item_ptr = get_dataset(datasetid)

            if (.not. c_associated(item_ptr)) then
                print *, this_image(), 'OOPS!, cannot find ', datasetid
                bSuccess = .false.
            end if
        end if

        ! have we got a valid dataset on all nodes
        call co_reduce(bSuccess, logical_and)

        if (.not. bSuccess) then
            return
        end if

        call c_f_pointer(item_ptr, item)

        ! reset the timeout clock
        call reset_clock(item)

        buffer = ''
        do concurrent(i=1:str_len)
            buffer(i:i) = cmd(i)
        end do

        read (buffer, *) req%dx, req%image, req%quality, req%x1, req%y1, req%x2, req%y2, &
        &req%width, req%height, req%beam, req%intensity, req%frame_start,&
        &req%frame_end, req%ref_freq, req%seq_id, req%timestamp

        if (this_image() .eq. 1) print *, this_image(), 'handle_realtime_image_spectrum for ', item%datasetid,&
        &', dx:', req%dx, ', image:', req%image, ', quality:', req%quality, ', x1:', req%x1, &
            &', y1:', req%y1, ', x2:', req%x2, ', y2:', req%y2, ', width:', req%width, &
            &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
            &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
            req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp

        ! get the range of the cube planes
        call get_spectrum_range(item, req%frame_start, req%frame_end, req%ref_freq, first, last)

        if (this_image() .eq. 1) print *, this_image(), 'first:', first, 'last:', last, 'depth:', item%naxes(3)

    end subroutine handle_realtime_image_spectrum

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

    subroutine read_fits_file(item, filename, dmin, dmax, pixels, mask, mean_spec, int_spec, bSuccess)
        implicit none

        type(dataset), pointer, intent(inout) :: item
        character(len=1024), intent(in) :: filename
        real(kind=4), intent(out) :: dmin, dmax
        logical, intent(out) ::  bSuccess
        logical :: tid_bSuccess

        integer status, group, unit, readwrite, blocksize, nkeys, nspace, hdutype, i, j
        integer naxis, bitpix
        integer npixels
        integer naxes(4)
        integer(kind=8) firstpix, lastpix, npixels_per_image
        integer tid, start, end, num_per_image, frame
        integer, dimension(4) :: fpixels, lpixels, incs
        logical test_ignrval

        ! local (image) buffers
        real(kind=4), allocatable :: local_buffer(:)
        logical(kind=1), allocatable :: local_mask(:)

        ! shared variables
        real(kind=4), allocatable :: pixels(:) [:]
        logical(kind=1), allocatable :: mask(:) [:]
        real, allocatable :: mean_spec(:) [:]
        real, allocatable :: int_spec(:) [:]

        real :: nullval, tmp
        character :: record*80, key*10, value*70, comment*70
        logical :: anynull

        naxis = 0
        naxes = (/0, 0, 0, 0/)
        bSuccess = .false.

        ! reset the strings
        item%frameid = ''
        item%object = ''
        item%line = ''
        item%filter = ''
        item%date_obs = ''
        item%btype = ''
        item%bunit = ''
        item%specsys = ''
        item%timesys = ''
        item%flux = ''

        ! reset the FITS header
        if (allocated(item%hdr)) deallocate (item%hdr)

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

            ! allocate a header character array
            if (this_image() == 1) then
                ! one extra character to hold the C '\0'
                allocate (item%hdr(80*nkeys + 1))
                item%hdr = c_null_char
            end if

            do i = 1, nkeys
                call ftgrec(unit, i, record, status)

                if (this_image() == 1) then
                    ! split the record into a key and a value
                    !key = record(1:10)
                    !value = record(11:80)

                    ! print *, record
                    ! print *, key, '-->', value

                    ! copy the characters one by one
                    ! Fortran string operations are frustrating
                    do concurrent(pos=1:80)
                        item%hdr(pos + (i - 1)*80) = record(pos:pos)
                    end do

                    ! print *, record, '<--->', item%hdr(1 + (i - 1)*80:i*80)
                end if

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
                ! reset the header
                if (allocated(item%hdr)) deallocate (item%hdr)

                ! success, so jump back and print out keywords in this extension
                if (this_image() == 1) print *, "GO TO 100"
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
        call frame_reference_type(item)
        call frame_reference_unit(item)

        item%bitpix = bitpix
        item%naxis = naxis
        item%naxes = naxes

        call set_header_status(item, .true.)

        if (this_image() == 1) then
            print *, 'BITPIX:', bitpix, 'NAXIS:', naxis, 'NAXES:', naxes
            print *, '#no. pixels:', naxes(1)*naxes(2)
        end if

        group = 1
        nullval = 0

        dmin = 1.0E30
        dmax = -1.0E30

        ! allocate the buffer
        npixels = naxes(1)*naxes(2)

        ! now read the 3D FITS data cube (successive 2D planes)
        if (npixels .eq. 0) then
            ! skip memory allocation / reading
            go to 200
        end if

        ! should we be checking values against ignrval ?
        if (isnan(item%ignrval)) then
            test_ignrval = .false.
        else
            test_ignrval = .true.
        end if

        ! start the timer
        call system_clock(count=item%start_time, count_rate=item%crate, count_max=item%cmax)

        ! calculate the range for each image
        if (naxis .eq. 2 .or. naxes(3) .eq. 1) then
            ! read one 2D image only in parallel on each image
            tid = this_image()
            num_per_image = npixels/num_images()
            firstpix = 1 + (tid - 1)*num_per_image
            lastpix = min(tid*num_per_image, npixels)
            num_per_image = lastpix - firstpix + 1
            ! print *, 'tid:', tid, 'firstpix:', firstpix, 'lastpix:', lastpix, 'num_per_image:', num_per_image

            ! local buffers
            allocate (local_buffer(num_per_image))
            allocate (local_mask(num_per_image))

            ! shared buffers
            allocate (pixels(npixels) [*])
            allocate (mask(npixels) [*])

            call ftgpve(unit, group, firstpix, num_per_image, nullval, local_buffer, anynull, status)

            ! abort upon an error
            if (status .ne. 0) go to 200

            ! calculate the min/max values
            do j = 1, num_per_image

                tmp = local_buffer(j)

                if (isnan(tmp) .neqv. .true.) then
                    if (test_ignrval) then
                        if (tmp .eq. item%ignrval) then
                            ! skip the IGNRVAL pixels
                            local_buffer(j) = 0.0
                            local_mask(j) = .false.
                            cycle
                        end if
                    end if

                    dmin = min(dmin, tmp)
                    dmax = max(dmax, tmp)
                    local_mask(j) = .true.
                else
                    local_buffer(j) = 0.0
                    local_mask(j) = .false.
                end if

            end do

            ! measure progress only on the root image
            if (this_image() == 1) call update_progress(item, 1, 1)

            ! put a local buffer/mask range onto image 1
            pixels(firstpix:lastpix) [1] = local_buffer(:)
            mask(firstpix:lastpix) [1] = local_mask(:)
        else
            ! read a range of 2D planes in parallel on each image
            tid = this_image()
            num_per_image = naxes(3)/num_images()
            start = 1 + (tid - 1)*num_per_image
            end = min(tid*num_per_image, naxes(3))
            num_per_image = end - start + 1
            !print *, 'tid:', tid, 'start:', start, 'end:', end, 'num_per_image:', num_per_image

            block
                real cdelt3, mean_spec_val, int_spec_val
                real frame_min, frame_max
                real pixel_sum
                integer pixel_count

                ! npixels_per_image = npixels*num_per_image
                allocate (local_buffer(npixels))
                allocate (pixels(npixels) [*])
                allocate (mask(npixels) [*])

                allocate (mean_spec(naxes(3)) [*])
                allocate (int_spec(naxes(3)) [*])

                ! allocate partial frame_min / frame_max arrays
                if (allocated(item%frame_min)) deallocate (item%frame_min)
                allocate (item%frame_min(start:end))

                if (allocated(item%frame_max)) deallocate (item%frame_max)
                allocate (item%frame_max(start:end))

                ! initiate pixels to blank
                pixels = 0.0
                ! and reset the NaN mask
                mask = .false.

                call get_cdelt3(item, cdelt3)

                ! zero-out the spectra
                mean_spec = 0.0
                int_spec = 0.0

                item%frame_min = 1.0E30
                item%frame_max = -1.0E30

                do frame = start, end
                    ! starting bounds
                    fpixels = (/1, 1, frame, 1/)

                    ! ending bounds
                    lpixels = (/naxes(1), naxes(2), frame, 1/)

                    ! do not skip over any pixels
                    incs = 1

                    ! skip the do loop, make one call to ftgsve instead
                    call ftgsve(unit, group, naxis, naxes, fpixels, lpixels, incs, nullval, local_buffer, anynull, status)

                    ! abort upon an error
                    if (status .ne. 0) go to 200

                    mean_spec_val = 0.0
                    int_spec_val = 0.0

                    pixel_sum = 0.0
                    pixel_count = 0

                    frame_min = 1.0E30
                    frame_max = -1.0E30

                    ! calculate the min/max values
                    do j = 1, npixels

                        tmp = local_buffer(j)

                        if (isnan(tmp) .neqv. .true.) then
                            if (test_ignrval) then
                                if (tmp .eq. item%ignrval) then
                                    ! skip the IGNRVAL pixels
                                    mask(j) = mask(j) .or. .false.
                                    cycle
                                end if
                            end if

                            frame_min = min(frame_min, tmp)
                            frame_max = max(frame_max, tmp)

                            ! integrate (sum up) pixels and a NaN mask
                            pixels(j) = pixels(j) + tmp
                            mask(j) = mask(j) .or. .true.

                            ! needed by the mean and integrated spectra
                            pixel_sum = pixel_sum + tmp
                            pixel_count = pixel_count + 1
                        else
                            mask(j) = mask(j) .or. .false.
                        end if

                    end do

                    item%frame_min(frame) = frame_min
                    item%frame_max(frame) = frame_max

                    dmin = min(dmin, frame_min)
                    dmax = max(dmax, frame_max)

                    if (pixel_count > 0) then
                        mean_spec_val = pixel_sum/real(pixel_count)
                        int_spec_val = pixel_sum*cdelt3
                    end if

                    mean_spec(frame) = mean_spec_val
                    int_spec(frame) = int_spec_val

                    ! measure progress only on the root image
                    if (this_image() == 1) call update_progress(item, frame - start + 1, num_per_image)
                end do
            end block
        end if

        bSuccess = .true.
        item%unit = unit
        return

        ! The FITS file must always be closed before exiting the program.
        ! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
200     call ftclos(unit, status)
        call ftfiou(unit, status)

        call set_error_status(item, .true.)

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

    subroutine frame_reference_type(item)
        type(dataset), pointer, intent(inout) :: item
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

    subroutine frame_reference_unit(item)
        type(dataset), pointer, intent(inout) :: item

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

    subroutine get_cdelt3(item, cdelt3)
        type(dataset), pointer, intent(in) :: item
        real, intent(out) :: cdelt3

        if (item%has_velocity) then
            cdelt3 = item%cdelt3*item%frame_multiplier/1000.0
        else
            cdelt3 = 1.0
        end if
    end subroutine get_cdelt3

    subroutine get_spectrum_range(item, frame_start, frame_end, ref_freq, first, last)
        type(dataset), pointer, intent(in) :: item
        real(kind=8), intent(in) :: frame_start, frame_end, ref_freq
        integer, intent(out) :: first, last

        first = 0
        last = 0

        if (item%naxes(3) .le. 1) return

        if (item%has_velocity .and. ref_freq .gt. 0.0) then
            call get_freq2vel_bounds(item, frame_start, frame_end, ref_freq, first, last)
            return
        end if

        if (item%has_frequency .and. ref_freq .gt. 0.0) then
            call get_frequency_bounds(item, frame_start, frame_end, first, last)
            return
        end if

        if (item%has_velocity) then
            call get_velocity_bounds(item, frame_start, frame_end, first, last)
            return
        end if

    end subroutine get_spectrum_range

    subroutine get_freq2vel_bounds(item, frame_start, frame_end, ref_freq, first, last)
        type(dataset), pointer, intent(in) :: item
        real(kind=8), intent(in) :: frame_start, frame_end, ref_freq
        integer, intent(out) :: first, last
        integer :: tmp

        ! the speed of light [m/s]
        real(kind=8), parameter :: c = 299792458.0
        real(kind=8) :: fRatio, v1, v2, RESTFRQ
        real(kind=8) :: x1, x2

        first = 0
        last = 0

        if (.not. item%header) return

        if (item%naxes(3) .le. 1) return

        if ((item%restfrq .le. 0.0) .and. (ref_freq .le. 0.0)) then
            first = 1
            last = item%naxes(3)
            return
        end if

        if (ref_freq .gt. 0.0) then
            RESTFRQ = ref_freq
        else
            RESTFRQ = item%restfrq
        end if

        fRatio = frame_start/RESTFRQ
        v1 = (1.0 - fRatio*fRatio)/(1.0 + fRatio*fRatio)*c

        fRatio = frame_end/RESTFRQ
        v2 = (1.0 - fRatio*fRatio)/(1.0 + fRatio*fRatio)*c

        x1 = item%crpix3 + (v1 - item%crval3*item%frame_multiplier)/(item%cdelt3*item%frame_multiplier)
        x2 = item%crpix3 + (v2 - item%crval3*item%frame_multiplier)/(item%cdelt3*item%frame_multiplier)

        first = nint(x1)
        last = nint(x2)

        ! reverse the direction
        if (item%cdelt3 .lt. 0.0) then
            first = item%naxes(3) - first
            last = item%naxes(3) - last
        end if

        ! impose ordering
        if (last .lt. first) then
            tmp = first
            first = last
            last = tmp
        end if

        if (first .lt. 1) first = 1
        if (last .gt. item%naxes(3)) last = item%naxes(3)

        return

    end subroutine get_freq2vel_bounds

    subroutine get_frequency_bounds(item, freq_start, freq_end, first, last)
        type(dataset), pointer, intent(in) :: item
        real(kind=8), intent(in) :: freq_start, freq_end
        integer, intent(out) :: first, last
        integer :: tmp

        real(kind=8) :: f1, f2, band_lo, band_hi

        first = 0
        last = 0

        if ((freq_start .eq. 0.0) .or. (freq_end .eq. 0.0)) then
            first = 1
            last = item%naxes(3)
            return
        end if

        f1 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(1.0 - item%crpix3)
        f2 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(item%naxes(3) - item%crpix3)

        band_lo = min(f1, f2)
        band_hi = max(f1, f2)

        if (item%cdelt3 .gt. 0.0) then
            first = 1 + nint((freq_start - band_lo)/(band_hi - band_lo)*(item%naxes(3) - 1))
            last = 1 + nint((freq_end - band_lo)/(band_hi - band_lo)*(item%naxes(3) - 1))
        else
            first = 1 + nint((band_hi - freq_start)/(band_hi - band_lo)*(item%naxes(3) - 1))
            last = 1 + nint((band_hi - freq_end)/(band_hi - band_lo)*(item%naxes(3) - 1))
        end if

        ! impose ordering
        if (last .lt. first) then
            tmp = first
            first = last
            last = tmp
        end if

        if (first .lt. 1) first = 1
        if (last .gt. item%naxes(3)) last = item%naxes(3)

        return

    end subroutine get_frequency_bounds

    subroutine get_velocity_bounds(item, vel_start, vel_end, first, last)
        type(dataset), pointer, intent(in) :: item
        real(kind=8), intent(in) :: vel_start, vel_end
        integer, intent(out) :: first, last
        integer :: tmp

        real(kind=8) :: v1, v2, band_lo, band_hi

        first = 0
        last = 0

        if (.not. item%header) return

        if (item%naxes(3) .le. 1) return

        if (item%cdelt3 .gt. 0.0) then
            first = 1 + nint((vel_start - band_lo)/(band_hi - band_lo)*(item%naxes(3) - 1))
            last = 1 + nint((vel_end - band_lo)/(band_hi - band_lo)*(item%naxes(3) - 1))
        else
            first = 1 + nint((band_hi - vel_start)/(band_hi - band_lo)*(item%naxes(3) - 1))
            last = 1 + nint((band_hi - vel_end)/(band_hi - band_lo)*(item%naxes(3) - 1))
        end if

        ! impose ordering
        if (last .lt. first) then
            tmp = first
            first = last
            last = tmp
        end if

        if (first .lt. 1) first = 1
        if (last .gt. item%naxes(3)) last = item%naxes(3)

        return

    end subroutine get_velocity_bounds

    subroutine make_image_statistics(item)
        implicit NONE

        type(dataset), pointer, intent(inout) :: item
        real, dimension(:), allocatable :: data
        real cdelt3, pmin, pmax, pmedian
        real mad, madP, madN
        integer countP, countN
        real pixel
        integer i, j, n
        real u, v
        real black, white, sensitivity, ratio_sensitivity
        integer stat

        call get_cdelt3(item, cdelt3)

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

        ! pick non-NaN valid pixels only according to mask
        data = pack(item%pixels, item%mask)

        ! make a histogram with a range given by [pmin, pmax]
        call make_histogram(item, data, pmin, pmax)

        n = size(data)

        if (n .eq. 0) return

        pmedian = median(data, n)
        print *, 'median = ', pmedian

        ! now the deviations from the median
        mad = 0.0; madP = 0.0; madN = 0.0
        countP = 0; countN = 0

        do i = 1, n
            pixel = data(i)
            mad = mad + abs(pixel - pmedian)

            if (pixel > pmedian) then
                madP = madP + (pixel - pmedian)
                countP = countP + 1
            end if

            if (pixel < pmedian) then
                madN = madN + (pmedian - pixel)
                countN = countN + 1
            end if
        end do

        mad = mad/real(n)
        if (countP > 0) madP = madP/real(countP)
        if (countN > 0) madN = madN/real(countN)

        print *, 'image pixels range pmin = ', pmin, ', pmax = ', pmax, ', median = ', pmedian
        print *, 'mad = ', mad, ', madP = ', madP, ', madN = ', madN

        ! ALMAWebQL v2 - style
        u = 7.5
        black = max(pmin, pmedian - u*madN)
        white = min(pmax, pmedian + u*madP)
        sensitivity = 1.0/(white - black)
        ratio_sensitivity = sensitivity

        if (item%is_optical) then
            u = 0.5
            v = 15.0
            black = max(pmin, pmedian - u*madN)
            white = min(pmax, pmedian + u*madP)
            sensitivity = 1.0/(white - black)
            ratio_sensitivity = sensitivity

            ! TO-DO: auto-brightness
        end if

        ! histogram classifier
        if (item%flux .eq. '') then
            block
                use classifier

                integer(kind=8), dimension(NBINS) :: cdf
                real(c_float), dimension(NBINS), target :: Slot
                integer(c_int) tone_mapping

                integer(kind=8) total

                ! the first histogram item
                total = item%hist(1)
                cdf(1) = item%hist(1)

                ! the remaining items
                do i = 2, NBINS
                    cdf(i) = cdf(i - 1) + item%hist(i)
                    total = total + item%hist(i)
                end do

                Slot = cdf/real(total)

                tone_mapping = histogram_classifier(c_loc(Slot))

                select case (tone_mapping)
                case (0)
                    item%flux = 'legacy'
                case (1)
                    item%flux = 'linear'
                case (2)
                    item%flux = 'logistic'
                case (3)
                    item%flux = 'ratio'
                case (4)
                    item%flux = 'square'
                case default
                    item%flux = 'legacy'
                end select
            end block
        end if

        print *, 'black = ', black, ', white = ', white, ', sensitivity = ', sensitivity

        item%pmin = pmin
        item%pmax = pmax
        item%pmedian = pmedian
        item%black = black
        item%white = white
        item%sensitivity = sensitivity
        item%ratio_sensitivity = ratio_sensitivity

    end subroutine make_image_statistics

    subroutine make_histogram(item, data, pmin, pmax)
        type(dataset), pointer, intent(inout) :: item
        real, dimension(:), intent(in) :: data
        real, intent(in) :: pmin, pmax
        integer i, index, n
        real value

        ! reset the histogram
        item%hist = 0

        n = size(data)

        do i = 1, n
            ! bin the value to [0,1]
            value = (data(i) - pmin)/(pmax - pmin)

            ! get a histogram bin index
            index = 1 + int(value*NBINS)

            ! clamp the index to within [1,NBINS]
            ! (rounding errors might cause an out-of-bounds index value)
            index = max(min(index, NBINS), 1)
            item%hist(index) = item%hist(index) + 1
        end do

    end subroutine make_histogram

    ! quicksort.f -*-f90-*-
    ! Author: t-nissie
    ! License: GPLv3
    ! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
    !!
    recursive subroutine quicksort(a, first, last)
        implicit none
        real a(*), x, t
        integer first, last
        integer i, j

        x = a((first + last)/2)
        i = first
        j = last
        do
            do while (a(i) < x)
                i = i + 1
            end do
            do while (x < a(j))
                j = j - 1
            end do
            if (i >= j) exit
            t = a(i); a(i) = a(j); a(j) = t
            i = i + 1
            j = j - 1
        end do
        if (first < i - 1) call quicksort(a, first, i - 1)
        if (j + 1 < last) call quicksort(a, j + 1, last)
    end subroutine quicksort

    ! quicksort.f -*-f90-*-
    ! Author: t-nissie, some tweaks by 1AdAstra1
    ! License: GPLv3
    ! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
    !!
    recursive subroutine vec_quicksort(a)
        implicit none
        real :: a(:)
        real x, t
        integer :: first = 1, last
        integer i, j

        last = size(a, 1)
        x = a((first + last)/2)
        i = first
        j = last

        do
            do while (a(i) < x)
                i = i + 1
            end do
            do while (x < a(j))
                j = j - 1
            end do
            if (i >= j) exit
            t = a(i); a(i) = a(j); a(j) = t
            i = i + 1
            j = j - 1
        end do

        if (first < i - 1) call vec_quicksort(a(first:i - 1))
        if (j + 1 < last) call vec_quicksort(a(j + 1:last))
    end subroutine vec_quicksort

    ! --------------------------------------------------------------------
    ! REAL FUNCTION  median() :
    !    This function receives an array X of N entries, sorts it
    !    and computes the median.
    !    The returned value is of REAL type.
    ! --------------------------------------------------------------------

    REAL FUNCTION median(X, N)
        use mod_sort
        IMPLICIT NONE
        REAL, DIMENSION(N), INTENT(INOUT) :: X
        INTEGER, INTENT(IN)                :: N
        INTEGER                            :: i

        integer, dimension(N) :: order

        ! timing
        real :: t1, t2

        ! switch between serial and parallel methods
        ! based on the size of the input vector X ???

        call cpu_time(t1)

        ! CALL quicksort(X, 1, N)               ! sort the original data
        CALL vec_quicksort(X)               ! sort the original data
        ! call psrs_sort(c_loc(X), N)         ! single-threaded vec_quicksort is faster than parallel psrc_sort !!!

        ! native parallel, slower than vec_quicksort !!!
        ! call parallel_sort(X, order)
        ! X = X(order(:))

        call cpu_time(t2)

        print *, 'sort elapsed time:', 1000*(t2 - t1), '[ms]'

        IF (MOD(N, 2) == 0) THEN           ! compute the median
            median = (X(N/2) + X(N/2 + 1))/2.0
        ELSE
            median = X(N/2 + 1)
        END IF

    END FUNCTION median

    subroutine inherent_image_dimensions(item, width, height)
        type(dataset), pointer, intent(in) :: item
        integer, intent(out) :: width, height
        integer x1, x2, y1, y2, k

        width = item%naxes(1)
        height = item%naxes(2)

        x1 = 1; x2 = width
        y1 = 1; y2 = height

        ! go through the 2D image mask item%mask
        ! truncating the NaN values along the X & Y axes

        ! x1
        do k = 1, width
            x1 = k

            if (any(item%mask(k, :))) exit
        end do

        ! x2
        do k = width, 1, -1
            x2 = k

            if (any(item%mask(k, :))) exit
        end do

        ! y1
        do k = 1, height
            y1 = k

            if (any(item%mask(:, k))) exit
        end do

        ! y2
        do k = height, 1, -1
            y2 = k

            if (any(item%mask(:, k))) exit
        end do

        print *, 'original dimensions:', width, height

        width = x2 - x1 + 1
        height = y2 - y1 + 1

        print *, 'inherent dimensions:', width, height

    end subroutine inherent_image_dimensions

    function get_screen_scale(x) result(scale)
        integer, intent(in) :: x
        real scale

        scale = floor(0.9*real(x))

    end function get_screen_scale

    function get_image_scale_square(width, height, img_width, img_height) result(scale)
        integer, intent(in) :: width, height
        integer, intent(in) :: img_width, img_height
        real scale, screen_dimension, image_dimension

        screen_dimension = get_screen_scale(min(width, height))
        image_dimension = max(img_width, img_height)
        scale = screen_dimension/image_dimension
    end function get_image_scale_square

    function get_image_scale(width, height, img_width, img_height) result(scale)
        integer, intent(in) :: width, height
        integer, intent(in) :: img_width, img_height
        real scale

        if (img_width .eq. img_height) then
            scale = get_image_scale_square(width, height, img_width, img_height)
            return
        end if

        if (img_height .lt. img_width) then
            block
                real screen_dimension, image_dimension
                real new_image_width

                screen_dimension = 0.9*real(height)
                image_dimension = img_height
                scale = screen_dimension/image_dimension
                new_image_width = scale*img_width

                if (new_image_width .gt. 0.8*real(width)) then
                    screen_dimension = 0.8*real(width)
                    image_dimension = img_width
                    scale = screen_dimension/image_dimension
                end if

            end block

            return
        end if

        if (img_width .lt. img_height) then
            block
                real screen_dimension, image_dimension
                real new_image_height

                screen_dimension = 0.8*real(width)
                image_dimension = img_width
                scale = screen_dimension/image_dimension
                new_image_height = scale*img_height

                if (new_image_height > 0.9*real(height)) then
                    screen_dimension = 0.9*real(height)
                    image_dimension = img_height
                    scale = screen_dimension/image_dimension
                end if

            end block

            return
        end if

        ! default scale
        scale = 1.0
        return
    end function get_image_scale

    subroutine get_json(item, json)
        use iso_varying_string
        use json_for
        implicit none

        type(dataset), pointer, intent(in) :: item
        type(varying_string), intent(out) :: json
        integer :: str_len
        integer(kind=8) :: filesize

        ! calculate the FITS file size
        filesize = size(item%hdr) + item%naxes(1)*item%naxes(2)*item%naxes(3)*item%naxes(4)*abs(item%bitpix)/8

        json = '{'

        ! call json_add_string(json, 'HEADER', 'NULL')

        ! misc. values
        call json_add_integer_number(json, 'width', item%naxes(1))
        call json_add_integer_number(json, 'height', item%naxes(2))
        call json_add_integer_number(json, 'depth', item%naxes(3))
        call json_add_integer_number(json, 'polarisation', item%naxes(4))
        call json_add_long_number(json, 'filesize', filesize)
        call json_add_real_number(json, 'IGNRVAL', item%ignrval)

        call json_add_real_number(json, 'CD1_1', item%cd1_1)
        call json_add_real_number(json, 'CD1_2', item%cd1_2)
        call json_add_real_number(json, 'CD2_1', item%cd2_1)
        call json_add_real_number(json, 'CD2_2', item%cd2_2)

        call json_add_real_number(json, 'CRVAL1', item%crval1)
        call json_add_real_number(json, 'CDELT1', item%cdelt1)
        call json_add_real_number(json, 'CRPIX1', item%crpix1)
        call json_add_string(json, 'CUNIT1', trim(item%cunit1))
        call json_add_string(json, 'CTYPE1', trim(item%ctype1))

        call json_add_real_number(json, 'CRVAL2', item%crval2)
        call json_add_real_number(json, 'CDELT2', item%cdelt2)
        call json_add_real_number(json, 'CRPIX2', item%crpix2)
        call json_add_string(json, 'CUNIT2', trim(item%cunit2))
        call json_add_string(json, 'CTYPE2', trim(item%ctype2))

        call json_add_real_number(json, 'CRVAL3', item%crval3)
        call json_add_real_number(json, 'CDELT3', item%cdelt3)
        call json_add_real_number(json, 'CRPIX3', item%crpix3)
        call json_add_string(json, 'CUNIT3', trim(item%cunit3))
        call json_add_string(json, 'CTYPE3', trim(item%ctype3))

        call json_add_real_number(json, 'BMAJ', item%bmaj)
        call json_add_real_number(json, 'BMIN', item%bmin)
        call json_add_real_number(json, 'BPA', item%bpa)

        call json_add_string(json, 'BUNIT', trim(item%bunit))
        call json_add_string(json, 'BTYPE', trim(item%btype))
        call json_add_string(json, 'SPECSYS', trim(item%specsys))

        call json_add_real_number(json, 'RESTFRQ', item%restfrq)
        call json_add_real_number(json, 'OBSRA', item%obsra)
        call json_add_real_number(json, 'OBSDEC', item%obsdec)

        call json_add_string(json, 'OBJECT', trim(item%object))
        call json_add_string(json, 'DATEOBS', trim(item%date_obs))
        call json_add_string(json, 'TIMESYS', trim(item%timesys))
        call json_add_string(json, 'LINE', trim(item%line))
        call json_add_string(json, 'FILTER', trim(item%filter))

        ! call json_add_real_array(json, 'mean_spectrum', item%mean_spectrum)
        ! call json_add_real_array(json, 'integrated_spectrum', item%integrated_spectrum)

        ! statistics (image histogram)
        call json_add_integer_array(json, 'histogram', item%hist)

        ! remove the last comma
        str_len = len(json)

        if (extract(json, str_len) .eq. ',') json = remove(json, str_len)

        json = json//'}'

        ! print *, char(json)
        print *, 'JSON length:', len(json)
    end subroutine get_json

end module fits
