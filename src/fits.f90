module fits
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: ieee_arithmetic
    use zfp_array
    use logger_mod, only: logger_init, logger => master_logger

    implicit none

    integer(kind=4), parameter :: NBINS = 1024
    real, parameter :: PI = 4.D0*DATAN(1.D0)

    integer(c_int), parameter :: FPZIP_MEDIUM_PRECISION = 16
    integer(c_int), parameter :: FPZIP_HIGH_PRECISION = 24

    integer(c_int), parameter :: ZFP_HIGH_PRECISION = 16
    integer(c_int), parameter :: ZFP_MEDIUM_PRECISION = 11
    integer(c_int), parameter :: ZFP_LOW_PRECISION = 8
    integer(c_int), parameter :: ZFP_MIN_EXP = -1074

    ! FITS channels are allocated to cluster nodes in blocks
    integer, parameter :: CHANNEL_BLOCK = 128

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

    !type fp16
    !    integer(kind=2), dimension(:, :), pointer :: ptr
    !end type fp16

    type zfp_ptr
        type(zfp_block), dimension(:, :), pointer :: ptr
    end type zfp_ptr

    type dataset
        character(kind=c_char), dimension(:), allocatable :: datasetid
        character(len=:), allocatable :: uri
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
        character(len=:), allocatable :: flux
        real(kind=c_float) dmin, dmax
        real(kind=4), allocatable :: frame_min(:), frame_max(:)
        real(kind=c_float), allocatable :: pixels(:, :)
        logical(kind=c_bool), allocatable :: mask(:, :)

        ! an array holding pointers to half-float 2D channel images
        type(zfp_ptr), dimension(:), allocatable :: compressed

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
        integer, allocatable :: hist(:)

        ! progress
        integer(8) :: start_time, crate, cmax
        integer :: cursor = 1
        integer :: progress = 0
        integer :: total = 0
        real :: elapsed = 0

        ! spectra
        real(kind=c_float), allocatable :: mean_spectrum(:)
        real(kind=c_float), allocatable :: integrated_spectrum(:)

    contains
        final :: close_fits_file
    end type dataset

    interface
        ! int get_physical_cores()
        integer(c_int) function get_physical_cores() BIND(C, name='get_physical_cores')
            use, intrinsic :: ISO_C_BINDING
            implicit none

        end function get_physical_cores

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
        ! void insert_dataset(const char *datasetid, int len, void *item);
        subroutine insert_dataset(datasetid, len, item) BIND(C, name='insert_dataset')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: datasetid(*)
            integer(c_int), value :: len
            type(c_ptr), value :: item
        end subroutine insert_dataset

        ! void *get_dataset(const char *datasetid);
        type(c_ptr) function get_dataset(datasetid) BIND(C, name='get_dataset')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: datasetid(*)
        end function get_dataset

        subroutine fetch_channel_range(root, datasetid, len, start, end, status) BIND(C, name='fetch_channel_range')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: root
            character(kind=c_char), intent(in) :: datasetid(*)
            integer(c_int), value :: len
            integer(c_int) :: start, end, status

        end subroutine fetch_channel_range

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

        ! there is nothing to do if the FITS file has never been opened
        if (item%unit .eq. -1) return

        print *, item%datasetid, ': closing the FITS file unit:', item%unit

        call ftclos(item%unit, status)
        call ftfiou(item%unit, status)

    end subroutine close_fits_file

    subroutine init_fortran_logging(log_file, len) BIND(C, name='init_fortran_logging')
        use, intrinsic :: iso_c_binding
        implicit none

        integer(kind=c_size_t), intent(in), value :: len
        character(kind=c_char), dimension(len), intent(in) :: log_file

        character(len=len) :: filename
        integer :: i

        do i = 1, len
            filename(i:i) = log_file(i)
        end do

        print *, "FORTRAN LOG FILE: ", filename

        ! Initialise the logger prior to use
        call logger_init(filename)

    end subroutine init_fortran_logging

    subroutine delete_dataset(ptr) BIND(C, name='delete_dataset')
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        call c_f_pointer(ptr, item)

        print *, 'deleting ', item%datasetid

        if (item%header_mtx%i .ne. 0) call g_mutex_clear(c_loc(item%header_mtx))
        if (item%error_mtx%i .ne. 0) call g_mutex_clear(c_loc(item%error_mtx))
        if (item%ok_mtx%i .ne. 0) call g_mutex_clear(c_loc(item%ok_mtx))
        if (item%progress_mtx%i .ne. 0) call g_mutex_clear(c_loc(item%progress_mtx))

        ! TO-DO:
        ! write the dataset to a cache file so as to speed up subsequent loading

        deallocate (item)
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

        item%progress = item%progress + 1
        item%total = total
        item%elapsed = elapsed

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))

    end subroutine update_progress

    subroutine get_channel_range_from_C(ptr, startindex, endindex, status) BIND(C, name='get_channel_range_from_C')
        type(C_PTR), intent(in), value :: ptr
        integer, intent(out) :: status, startindex, endindex

        type(dataset), pointer :: item

        call c_f_pointer(ptr, item)

        call get_channel_range(item, startindex, endindex, status)

    end subroutine get_channel_range_from_C

    subroutine get_channel_range(item, startindex, endindex, status)
        type(dataset), pointer, intent(inout) :: item
        integer, intent(out) :: status, startindex, endindex

        ! lock the mutex
        call g_mutex_lock(c_loc(item%progress_mtx))

        if (item%cursor .gt. item%naxes(3)) then
            ! an error, no more channels to allocate
            startindex = -1
            endindex = -1

            ! an error status
            status = 1 ! end of AXIS3
        else
            startindex = item%cursor
            endindex = min(startindex + CHANNEL_BLOCK - 1, item%naxes(3))

            ! move the cursor forward
            item%cursor = item%cursor + CHANNEL_BLOCK

            ! status OK
            status = 0
        end if

        ! unlock the mutex
        call g_mutex_unlock(c_loc(item%progress_mtx))

    end subroutine get_channel_range

    subroutine load_fits_file(datasetid, datasetid_len, filepath, filepath_len, flux, flux_len, root) bind(C)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(kind=c_size_t), intent(in), value :: datasetid_len, filepath_len, flux_len
        character(kind=c_char), dimension(datasetid_len), intent(in) :: datasetid
        character(kind=c_char), dimension(filepath_len), intent(in) :: filepath
        character(kind=c_char), dimension(flux_len), intent(in) :: flux

        ! the pointer will be passed back to C when requesting FITS file channel ranges
        ! from the root node and submitting results to the cluster root
        type(c_ptr), intent(in), value :: root

        character(len=filepath_len) :: strFilename
        character(len=flux_len) :: strFlux

        integer :: i
        logical :: bSuccess

        type(dataset), pointer :: item

        integer(8) :: start, finish, crate, cmax, id
        real :: elapsed

        print *, "[load_fits_file] datasetid: '", datasetid, "', flux: '", flux, "', filepath: '", filepath, "'"

        if (.not. c_associated(root)) then
            print *, "[load_fits_file] :: ROOT NODE"
        else
            print *, "[load_fits_file] :: CLIENT NODE"
        end if

        do i = 1, filepath_len
            strFilename(i:i) = filepath(i)
        end do

        do i = 1, flux_len
            strFlux(i:i) = flux(i)
        end do

        allocate (item)

        ! init mutexes
        if (item%header_mtx%i .eq. 0) call g_mutex_init(c_loc(item%header_mtx))
        if (item%error_mtx%i .eq. 0) call g_mutex_init(c_loc(item%error_mtx))
        if (item%ok_mtx%i .eq. 0) call g_mutex_init(c_loc(item%ok_mtx))
        if (item%progress_mtx%i .eq. 0) call g_mutex_init(c_loc(item%progress_mtx))

        item%datasetid = datasetid
        item%progress = 0
        item%elapsed = 0
        allocate (item%uri, source=strFilename) ! is it needed ?
        call set_ok_status(item, .false.)
        call set_error_status(item, .false.)
        call set_header_status(item, .false.)

        call insert_dataset(item%datasetid, size(item%datasetid), c_loc(item))

        ! start the timer
        call system_clock(count=start, count_rate=crate, count_max=cmax)

        call read_fits_file(item, strFilename, strFlux, root, bSuccess)

        ! end the timer
        call system_clock(finish)
        elapsed = real(finish - start)/real(crate)

        print *, "finished loading ", item%datasetid, ", bSuccess: ", bSuccess, ", elapsed time: ", elapsed, " [s]"

        ! reset the timeout clock
        call reset_clock(item)

    end subroutine load_fits_file

    subroutine read_fits_file(item, filename, flux, root, bSuccess)
        use omp_lib
        implicit none

        type(dataset), pointer, intent(inout) :: item
        character(len=*), intent(in) :: filename, flux
        ! the pointer will be passed back to C when requesting FITS file ranges
        ! from the root node and submitting results to the cluster root
        type(c_ptr), intent(in) :: root
        logical, intent(out) ::  bSuccess

        integer status, group, unit, readwrite, blocksize, nkeys, nspace, hdutype, i, j
        integer naxis, bitpix
        integer npixels, cn, cm
        integer naxes(4)
        integer(kind=8) firstpix, lastpix, npixels_per_image
        integer max_threads, tid, start, end, num_per_node, frame
        integer, dimension(4) :: fpixels, lpixels, incs
        logical test_ignrval

        real :: nullval, tmp
        character :: record*80, key*10, value*70, comment*70
        logical :: anynull

        ! local buffers
        real(kind=4), allocatable :: local_buffer(:)
        logical(kind=1), allocatable :: local_mask(:)

        ! shared variables
        real(kind=4), allocatable :: pixels(:)
        logical(kind=1), allocatable :: mask(:)
        real, allocatable :: mean_spec(:)
        real, allocatable :: int_spec(:)

        ! thread-shared
        real, allocatable :: thread_mean_spec(:), thread_int_spec(:)

        ! thread-local variables
        real(kind=4), allocatable, target :: thread_buffer(:, :)
        real(kind=4), allocatable :: thread_pixels(:, :)
        logical(kind=1), allocatable :: thread_mask(:, :)
        real(kind=4), allocatable :: thread_x(:, :, :)
        logical thread_bSuccess

        ! local statistics
        real(kind=4) :: dmin, dmax

        ! OpenMP multi-threading
        integer, dimension(:), allocatable :: thread_units
        integer :: num_threads

        if (.not. c_associated(root)) then
            ! TO-DO: needs to be protected with a mutex
            call logger%info('read_fits_file', 'opening '//filename//'; FLUX: '//flux)
        end if

        print *, "[read_fits_file]::'", filename, "'", ", flux:'", flux, "'"

        num_threads = OMP_GET_MAX_THREADS()

        print *, "OpenMP max #threads: ", num_threads

        naxis = 0
        naxes = (/0, 0, 0, 0/)
        bSuccess = .false.

        record = ''
        key = ''
        value = ''
        comment = ''

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

        ! special handling for the flux
        ! test the first character for 'N' ('NULL')
        if (flux(1:1) .ne. 'N') allocate (item%flux, source=flux)

        item%cunit1 = ''
        item%cunit2 = ''
        item%cunit3 = ''

        item%ctype1 = ''
        item%ctype2 = ''
        item%ctype3 = ''

        item%pmin = 0.0
        item%pmax = 0.0
        item%pmedian = 0.0
        item%black = 0.0
        item%white = 0.0
        item%sensitivity = 0.0
        item%ratio_sensitivity = 0.0

        ! reset the FITS header
        if (allocated(item%hdr)) deallocate (item%hdr)

        ! The STATUS parameter must always be initialized.
        status = 0

        ! Get an unused Logical Unit Number to use to open the FITS file.
        call ftgiou(unit, status)

        if (status .ne. 0) then
            return
        end if

        item%unit = unit

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

        ! print *, 'Header listing for HDU', j

        ! The FTGHSP subroutine returns the number of existing keywords in the
        ! current header data unit(CHDU), not counting the required END keyword,
        call ftghsp(unit, nkeys, nspace, status)

        ! Read each 80 - character keyword record, and print it out.
        block
            integer pos

            ! allocate a header character array
            ! one extra character to hold the C '\0'
            if (.not. allocated(item%hdr)) allocate (item%hdr(80*nkeys + 1))
            item%hdr = c_null_char
            print *, 'item%hdr::allocated space for', (80*nkeys + 1), 'characters; size:', size(item%hdr)

            do i = 1, nkeys
                status = 0; call ftgrec(unit, i, record, status)

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
            ! print *, 'END'
            ! print *, ' '
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

                pos = index(value, 'kiso')
                if (pos .ne. 0) then
                    item%is_optical = .true.
                    item%flux = 'ratio'
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
                if (allocated(item%hdr)) then
                    print *, 'DEALLOCATING item%hdr'
                    deallocate (item%hdr)
                end if

                ! success, so jump back and print out keywords in this extension
                print *, "GO TO 100"
                go to 100

            else if (status .eq. 107) then
                ! hit end of file, so quit
                status = 0
            end if
        end if

        !  Check that it found at least both NAXIS1 and NAXIS2 keywords.
        if (naxis .lt. 2) then
            print *, 'READIMAGE failed to read the NAXISn keywords.'

            go to 200
        end if

        ! detect the FITS header types and units (frequency, velocity)
        call frame_reference_type(item)
        call frame_reference_unit(item)

        item%bitpix = bitpix
        item%naxis = naxis
        item%naxes = naxes

        call set_header_status(item, .true.)

        print *, 'BITPIX:', bitpix, 'NAXIS:', naxis, 'NAXES:', naxes
        print *, '#no. pixels:', naxes(1)*naxes(2)

        group = 1
        nullval = 0

        dmin = 1.0E30
        dmax = -1.0E30

        ! allocate the buffer
        npixels = naxes(1)*naxes(2)

        ! by default compressed is dimension(naxes(1)/DIM, naxes(2)/DIM)
        cn = naxes(1)/DIM
        cm = naxes(2)/DIM

        ! but the input dimensions might not be divisible by 4
        if (mod(naxes(1), DIM) .ne. 0) cn = cn + 1
        if (mod(naxes(2), DIM) .ne. 0) cm = cm + 1

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
            ! client nodes can skip 2D images
            if (c_associated(root)) then
                return
            end if

            ! read a 2D image on the root node only
            firstpix = 1
            lastpix = npixels

            ! local buffers
            allocate (local_buffer(npixels))
            allocate (local_mask(npixels))

            call ftgpve(unit, group, firstpix, npixels, nullval, local_buffer, anynull, status)

            ! abort upon an error
            if (status .ne. 0) go to 200

            ! calculate the min/max values
            do j = 1, npixels

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
            call update_progress(item, 1, 1)

            item%dmin = dmin
            item%dmax = dmax

            item%pixels = reshape(local_buffer, naxes(1:2))
            item%mask = reshape(local_mask, naxes(1:2))

            ! make an image histogram, decide on the flux etc.
            call make_image_statistics(item)

            call set_ok_status(item, .true.)

            call print_dataset(item)
        else
            ! read a range of 2D planes in parallel on each cluster node

            ! interleave computation with disk access
            ! cap the number of threads to avoid system overload
            ! max_threads = min(OMP_GET_MAX_THREADS(), 4)

            ! get #physical cores (ignore HT)
            max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

            if (.not. allocated(thread_units)) then
                allocate (thread_units(OMP_GET_MAX_THREADS()))
                thread_units = -1

                ! open the thread-local FITS file if necessary
                do i = 1, OMP_GET_MAX_THREADS()
                    if (thread_units(i) .eq. -1) then
                        block
                            ! file operations
                            integer unit, readwrite, blocksize, status

                            ! The STATUS parameter must always be initialized.
                            status = 0

                            ! Get an unused Logical Unit Number to use to open the FITS file.
                            call ftgiou(unit, status)

                            if (status .ne. 0) then
                                cycle
                            end if

                            ! open the FITS file, with read - only access.The returned BLOCKSIZE
                            ! parameter is obsolete and should be ignored.
                            readwrite = 0
                            call ftopen(unit, filename, readwrite, blocksize, status)

                            if (status .ne. 0) then
                                print *, 'thread ', i, ': error opening '//filename
                                cycle
                            end if

                            thread_units(i) = unit

                        end block

                    end if
                end do
            end if

            ! initially the whole range
            start = 1
            end = naxes(3)
            num_per_node = end - start + 1

            ! type(zfp_ptr), dimension(:), allocatable :: compressed
            allocate (item%compressed(start:end))

            do i = start, end
                nullify (item%compressed(i)%ptr)
            end do

            allocate (item%frame_min(start:end))
            allocate (item%frame_max(start:end))

            ! dynamically get the range blocks

            status = 0

            do while (status .eq. 0)
                if (.not. c_associated(root)) then
                    ! a direct (local) request
                    call get_channel_range(item, start, end, status)
                else
                    ! fetch the range from the root node via HTTP
                    call fetch_channel_range(root, item%datasetid, size(item%datasetid), start, end, status) ! a C function defined in http.c
                end if

                if (status .ne. 0) exit ! no more work to do

                num_per_node = end - start + 1
                print *, 'START:', start, 'END:', end, 'num_per_node:', num_per_node

                ! process the block

            end do

            ! close any remaining thread file units
            if (allocated(thread_units)) then
                do i = 1, size(thread_units)
                    if (thread_units(i) .eq. -1) cycle

                    call ftclos(thread_units(i), status)
                    call ftfiou(thread_units(i), status)
                end do

                deallocate (thread_units)
            end if
        end if

        bSuccess = .true.
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
        if (.not. allocated(item%flux)) then
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

        allocate (item%hist(NBINS))

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

    ! --------------------------------------------------------------------
    ! REAL FUNCTION  median() :
    !    This function receives an array X of N entries, sorts it
    !    and computes the median.
    !    The returned value is of REAL type.
    ! --------------------------------------------------------------------

    REAL FUNCTION median(X, N)
        IMPLICIT NONE
        INTEGER, INTENT(IN)                :: N
        REAL, DIMENSION(N), INTENT(INOUT), TARGET :: X

        ! timing
        integer(8) :: start_t, finish_t, crate, cmax
        real :: elapsed

        ! start the timer
        call system_clock(count=start_t, count_rate=crate, count_max=cmax)

        call psrs_sort(c_loc(X), N) ! a parallel OpenMP version written in C

        ! end the timer
        call system_clock(finish_t)
        elapsed = real(finish_t - start_t)/real(crate)

        print *, 'sort elapsed time:', 1000*elapsed, ' [ms]'

        IF (MOD(N, 2) == 0) THEN           ! compute the median
            median = (X(N/2) + X(N/2 + 1))/2.0
        ELSE
            median = X(N/2 + 1)
        END IF

    END FUNCTION median

end module fits
