module fits
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: ieee_arithmetic
    use fixed_array
    use logger_mod, only: logger_init, logger => master_logger
    use :: unix_pthread

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
    real, parameter :: GOLDEN_RATIO = (1 + sqrt(5.0))/2
    integer, parameter :: MAX_CHANNEL_BLOCK = 32 ! 16 ! 64 ! 128

    type(c_pthread_mutex_t), save :: logger_mtx

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

    type array_ptr
        type(fixed_block), dimension(:, :), pointer :: ptr
    end type array_ptr

    type image_tone_mapping
        character(len=:), allocatable :: flux
        real(kind=c_float) :: pmin, pmax, pmedian
        real(kind=c_float) :: sensitivity, ratio_sensitivity
        real(kind=c_float) :: white, black
    end type image_tone_mapping

    type video_tone_mapping
        character(len=:), allocatable :: flux
        real(kind=c_float) :: dmin, dmax, dmedian
        real(kind=c_float) :: sensitivity, slope
        real(kind=c_float) :: white, black
    end type video_tone_mapping

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
        real(kind=4), allocatable :: frame_min(:), frame_max(:), frame_median(:)
        real(kind=c_float), allocatable :: pixels(:, :)
        logical(kind=c_bool), allocatable :: mask(:, :)

        ! an array holding pointers to half-float 2D channel images
        type(array_ptr), dimension(:), allocatable :: compressed

        logical :: is_optical = .true.
        logical :: is_xray = .false.

        logical :: error = .false.
        logical :: ok = .false.
        logical :: header = .false.
        logical :: image = .false.

        ! mutexes
        type(c_pthread_mutex_t) :: header_mtx, ok_mtx, error_mtx, progress_mtx, image_mtx

        ! progress
        integer(8) :: start_time, crate, cmax
        integer :: cursor = 1
        integer :: CHANNEL_BLOCK = 1
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

        ! void close_pipe(int fd);
        subroutine close_pipe(fd) BIND(C, name='close_pipe')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(kind=c_int), value, intent(in) :: fd
        end subroutine close_pipe

        ! export void  make_image_spectrumF32(uniform float src[], uniform float pixels[], uniform unsigned int8 mask[], uniform float ignrval, uniform float datamin, uniform float datamax,  uniform float cdelt3, uniform float res[], uniform int npixels)
        subroutine make_image_spectrumF32(src, pixels, mask, ignrval, datamin, datamax, cdelt3, res, npixels)&
          & BIND(C, name='make_image_spectrumF32')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(C_PTR), value, intent(in) :: src, pixels, mask, res
            real(c_float), value, intent(in) :: ignrval, datamin, datamax, cdelt3
            integer(c_int), value, intent(in) :: npixels

        end subroutine make_image_spectrumF32

        ! resizeCubic(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight)
        subroutine resizeCubic(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeCubic')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeCubic

        ! resizeLanczos(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight, int numLobes)
        subroutine resizeLanczos(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight, numLobes) BIND(C, name='resizeLanczos')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
            integer(c_int), value, intent(in) :: numLobes
        end subroutine resizeLanczos

        ! resizeSuper(Ipp32f *pSrc, int srcWidth, int srcHeight, Ipp32f *pDest, int dstWidth, int dstHeight)
        subroutine resizeSuper(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeSuper')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeSuper

        ! resizeNearest(Ipp8u *pSrc, int srcWidth, int srcHeight, Ipp8u *pDest, int dstWidth, int dstHeight)
        subroutine resizeNearest(pSrc, srcWidth, srcHeight, pDest, dstWidth, dstHeight) BIND(C, name='resizeNearest')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: srcWidth, srcHeight
            integer(c_int), value, intent(in) :: dstWidth, dstHeight
            type(C_PTR), value, intent(in) :: pSrc, pDest
        end subroutine resizeNearest

        subroutine write_json(fd, json) BIND(C, name='write_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd
            type(C_PTR), value :: json
        end subroutine write_json

        ! void delete_json(GString *json)
        subroutine delete_json(json) BIND(C, name='delete_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
        end subroutine delete_json

        subroutine write_header(fd, json_str, str_len) BIND(C, name='write_header')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd, str_len
            character(kind=c_char), intent(in) :: json_str(*)
        end subroutine write_header

        subroutine write_image_spectrum(fd, flux, &
         pmin, pmax, pmedian, &
         &black, white, sensitivity, ratio_sensitivity,&
         &width, height, precision,&
         &pixels, mask)&
         &BIND(C, name='write_image_spectrum')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            character(kind=c_char), intent(in) :: flux(*)
            integer(c_int), value, intent(in) :: fd, width, height, precision
            real(kind=c_float), value, intent(in) :: pmin, pmax, pmedian
            real(kind=c_float), value, intent(in) :: black, white
            real(kind=c_float), value, intent(in) :: sensitivity, ratio_sensitivity
            type(C_PTR), value :: pixels, mask
        end subroutine write_image_spectrum

        subroutine write_spectrum(fd, spectrum, n, prec) BIND(C, name='write_spectrum')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            integer(c_int), value, intent(in) :: fd, n, prec
            type(C_PTR), value, intent(in) :: spectrum
        end subroutine write_spectrum

        ! glib functions
        !  GString *begin_json()
        type(C_PTR) function begin_json() BIND(C, name='begin_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none
        end function begin_json

        ! void end_json(GString *json)
        subroutine end_json(json) BIND(C, name='end_json')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
        end subroutine end_json

        ! void add_json_integer(GString *json, char *key, int val)
        subroutine add_json_integer(json, key, val) BIND(C, name='add_json_integer')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int), value, intent(in) :: val
        end subroutine add_json_integer

        ! void add_json_integer_array(GString *json, char *key, int *val, int n)
        subroutine add_json_integer_array(json, key, val, n) BIND(C, name='add_json_integer_array')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json, val
            character(kind=c_char), intent(in) :: key(*)
            integer(c_int), value, intent(in) :: n
        end subroutine add_json_integer_array

        ! void add_json_string(GString *json, char *key, char *val)
        subroutine add_json_string(json, key, val) BIND(C, name='add_json_string')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
            character(kind=c_char), intent(in) :: key(*), val(*)
        end subroutine add_json_string

        ! void add_json_long(GString *json, char *key, long val)
        subroutine add_json_long(json, key, val) BIND(C, name='add_json_long')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
            character(kind=c_char), intent(in) :: key(*)
            integer(c_long), value, intent(in) :: val
        end subroutine add_json_long

        ! void add_json_real(GString *json, char *key, float val)
        subroutine add_json_real(json, key, val) BIND(C, name='add_json_real')
            use, intrinsic :: ISO_C_BINDING
            implicit none

            type(c_ptr), value :: json
            character(kind=c_char), intent(in) :: key(*)
            real(kind=c_float), value, intent(in) :: val
        end subroutine add_json_real

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

    subroutine init_fortran() BIND(C, name='init_fortran')
        use, intrinsic :: iso_c_binding
        implicit none

        integer rc

        rc = c_pthread_mutex_init(logger_mtx, c_null_ptr)
    end subroutine init_fortran

    subroutine cleanup_fortran() BIND(C, name='cleanup_fortran')
        use, intrinsic :: iso_c_binding
        implicit none

        integer rc

        rc = c_pthread_mutex_destroy(logger_mtx)
    end subroutine cleanup_fortran

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

        integer i, rc

        call c_f_pointer(ptr, item)

        print *, 'deleting ', item%datasetid

        rc = c_pthread_mutex_destroy(item%header_mtx)
        rc = c_pthread_mutex_destroy(item%error_mtx)
        rc = c_pthread_mutex_destroy(item%ok_mtx)
        rc = c_pthread_mutex_destroy(item%progress_mtx)
        rc = c_pthread_mutex_destroy(item%image_mtx)

        ! TO-DO:
        ! write the dataset to a cache file so as to speed up subsequent loading

        ! deallocate compressed memory regions
        if (allocated(item%compressed)) then
            do concurrent(i=1:size(item%compressed))
                if (associated(item%compressed(i)%ptr)) then
                    deallocate (item%compressed(i)%ptr)
                    nullify (item%compressed(i)%ptr)
                end if
            end do

            deallocate (item%compressed)
        end if

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

        if (item%naxes(3) .gt. 1) then
            ! print *, 'frame_min:', item%frame_min
            ! print *, 'frame_max:', item%frame_max
            ! print *, 'frame_median:', item%frame_median

            ! print *, 'mean spectrum:', item%mean_spectrum
            ! print *, 'integrated spectrum:', item%integrated_spectrum
        end if

    end subroutine print_dataset

    subroutine set_error_status(item, error)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: error

        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%error_mtx)

        item%error = error

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%error_mtx)

    end subroutine set_error_status

    subroutine set_ok_status(item, ok)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: ok

        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%ok_mtx)

        item%ok = ok

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%ok_mtx)

    end subroutine set_ok_status

    subroutine set_image_status(item, image)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: image

        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%image_mtx)

        item%image = image

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%image_mtx)

    end subroutine set_image_status

    subroutine set_header_status(item, header)
        type(dataset), pointer, intent(inout) :: item
        logical, intent(in) :: header

        integer rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%header_mtx)

        item%header = header

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%header_mtx)

    end subroutine set_header_status

    subroutine reset_clock(item)
        type(dataset), pointer, intent(inout) :: item

        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        call system_clock(item%start_time)

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

    end subroutine reset_clock

    subroutine update_progress(item, progress)
        type(dataset), pointer, intent(inout) :: item
        integer, intent(in) :: progress
        integer(8) finish
        real elapsed

        integer :: current, total, rc

        ! take a time measurement
        call system_clock(finish)
        elapsed = real(finish - item%start_time)/real(item%crate)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        item%progress = item%progress + progress
        item%elapsed = elapsed

        current = item%progress
        total = item%total

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

        if ((current .eq. total) .and. (total .gt. 0)) then
            call set_ok_status(item, .true.)
            call print_dataset(item)
        end if

    end subroutine update_progress

    subroutine print_progress(item)
        type(dataset), pointer, intent(inout) :: item
        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        print *, 'progress:', item%progress, 'out of total:', item%total

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

    end subroutine print_progress

    subroutine set_progress(item, progress, total)
        type(dataset), pointer, intent(inout) :: item
        integer, intent(in) :: progress, total
        integer(8) finish
        real elapsed

        integer :: rc

        ! take a time measurement
        call system_clock(finish)
        elapsed = real(finish - item%start_time)/real(item%crate)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        item%progress = progress
        item%total = total
        item%elapsed = elapsed

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

    end subroutine set_progress

    integer(c_int) function get_error_status(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer :: rc

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%error_mtx)

        if (item%error) then
            get_error_status = 1
        else
            get_error_status = 0
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%error_mtx)

        return
    end function get_error_status

    integer(c_int) function get_ok_status(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer :: rc

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%ok_mtx)

        if (item%ok) then
            get_ok_status = 1
        else
            get_ok_status = 0
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%ok_mtx)

        return
    end function get_ok_status

    integer(c_int) function get_image_status(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer :: rc

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%image_mtx)

        if (item%image) then
            get_image_status = 1
        else
            get_image_status = 0
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%image_mtx)

        return
    end function get_image_status

    integer(c_int) function get_header_status(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer rc

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%header_mtx)

        if (item%header) then
            get_header_status = 1
        else
            get_header_status = 0
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%header_mtx)

        return
    end function get_header_status

    real(c_float) function get_progress(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer :: rc

        if (get_header_status(ptr) .ne. 1) then
            get_progress = 0.0
            return
        end if

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

        if (item%total .gt. 0) then
            get_progress = 100.0*item%progress/item%total
        else
            get_progress = 0.0
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)
    end function get_progress

    real(c_float) function get_elapsed(ptr) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item

        integer :: rc

        if (get_header_status(ptr) .ne. 1) then
            get_elapsed = 0.0
            return
        end if

        call c_f_pointer(ptr, item)

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        get_elapsed = item%elapsed

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)
    end function get_elapsed

    subroutine get_channel_range_C(ptr, progress, startindex, endindex, status) BIND(C, name='get_channel_range_C')
        type(C_PTR), intent(in), value :: ptr
        integer(c_int), intent(in), value :: progress
        integer(c_int), intent(out) :: status, startindex, endindex

        type(dataset), pointer :: item

        call c_f_pointer(ptr, item)

        if (progress .gt. 0) call update_progress(item, progress)

        call get_channel_range(item, startindex, endindex, status)

    end subroutine get_channel_range_C

    subroutine get_channel_range(item, startindex, endindex, status)
        type(dataset), pointer, intent(inout) :: item
        integer, intent(out) :: status, startindex, endindex

        integer :: rc

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%error_mtx)

        ! check the error
        if (item%error) then
            startindex = 0
            endindex = 0
            status = -2

            ! unlock the mutex and exit subroutine
            rc = c_pthread_mutex_unlock(item%error_mtx)
            return
        else
            ! unlock the mutex
            rc = c_pthread_mutex_unlock(item%error_mtx)
        end if

        ! there has been no error up until now, continue

        ! lock the mutex
        rc = c_pthread_mutex_lock(item%progress_mtx)

        if (.not. item%header) then
            startindex = 0
            endindex = 0

            ! header is not available yet
            status = 1
        else
            if (item%cursor .gt. item%naxes(3)) then
                ! an error, no more channels to allocate
                startindex = 0
                endindex = 0

                ! an end of channels
                status = -1 ! end of AXIS3
            else
                startindex = item%cursor
                endindex = min(startindex + item%CHANNEL_BLOCK - 1, item%naxes(3))

                ! move the cursor forward
                item%cursor = item%cursor + item%CHANNEL_BLOCK

                ! natural geometric progression
                item%CHANNEL_BLOCK = min(nint(GOLDEN_RATIO*item%CHANNEL_BLOCK), MAX_CHANNEL_BLOCK)

                ! status OK
                status = 0
            end if
        end if

        ! unlock the mutex
        rc = c_pthread_mutex_unlock(item%progress_mtx)

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

        integer :: i, rc
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
        rc = c_pthread_mutex_init(item%header_mtx, c_null_ptr)
        rc = c_pthread_mutex_init(item%error_mtx, c_null_ptr)
        rc = c_pthread_mutex_init(item%ok_mtx, c_null_ptr)
        rc = c_pthread_mutex_init(item%progress_mtx, c_null_ptr)
        rc = c_pthread_mutex_init(item%image_mtx, c_null_ptr)

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

        ! thread-local variables
        real(kind=c_float), pointer :: thread_buffer(:)
        real(kind=c_float), pointer :: thread_pixels(:)
        logical(kind=c_bool), pointer :: thread_mask(:)
        real(kind=c_float), pointer :: thread_arr(:, :)
        real(kind=c_float), pointer :: thread_data(:)
        real(kind=c_float), target :: res(4)
        integer :: data_count
        logical thread_bSuccess

        real cdelt3, mean_spec_val, int_spec_val
        real frame_min, frame_max, frame_median
        real pixel_sum
        integer pixel_count

        ! local statistics
        real(kind=4) :: dmin, dmax

        ! OpenMP multi-threading
        integer, dimension(:), allocatable :: thread_units
        integer :: num_threads

        integer rc

        if (.not. c_associated(root)) then
            ! needs to be protected with a mutex
            rc = c_pthread_mutex_lock(logger_mtx)

            if (rc .eq. 0) then
                ! Intel ifort: forrtl: severe (32): invalid logical unit number, unit -129, file unknown !?
                call logger%info('read_fits_file', 'opening '//filename//'; FLUX: '//flux)

                ! unlock the mutex
                rc = c_pthread_mutex_unlock(logger_mtx)
            end if
        end if

        ! print *, "[read_fits_file]::'", filename, "'", ", flux:'", flux, "'"

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
        ! if (status .ne. 0) item%datamin = ieee_value(0.0, ieee_quiet_nan)
        if (status .ne. 0) item%datamin = -1.0E30

        status = 0; call FTGKYE(unit, 'DATAMAX', item%datamax, comment, status)
        ! if (status .ne. 0) item%datamax = ieee_value(0.0, ieee_quiet_nan)
        if (status .ne. 0) item%datamax = 1.0E30

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

        ! start the timer
        call system_clock(count=item%start_time, count_rate=item%crate, count_max=item%cmax)

        ! reset the progress
        if (naxis .eq. 2 .or. naxes(3) .eq. 1) then
            call set_progress(item, 0, 1)
        else
            call set_progress(item, 0, naxes(3))
        end if

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
            item%ignrval = -1.0E30
        else
            test_ignrval = .true.
        end if

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

            call update_progress(item, 1)

            item%dmin = dmin
            item%dmax = dmax

            item%pixels = reshape(local_buffer, naxes(1:2))
            item%mask = reshape(local_mask, naxes(1:2))

            call set_image_status(item, .true.)
        else
            ! read a range of 2D planes in parallel on each cluster node

            ! interleave computation with disk access
            ! cap the number of threads to avoid system overload
            max_threads = min(OMP_GET_MAX_THREADS(), 4)

            ! get #physical cores (ignore HT), and then cut the number in half
            ! to avoid a system overload
            ! max_threads = min(max_threads, get_physical_cores()/2)

            ! get #physical cores (ignore HT)
            ! max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

            print *, "max_threads:", max_threads

            if (.not. allocated(thread_units)) then
                allocate (thread_units(max_threads))
                thread_units = -1

                ! open the thread-local FITS file if necessary
                do i = 1, max_threads
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

            allocate (item%compressed(start:end))

            do i = start, end
                nullify (item%compressed(i)%ptr)
            end do

            allocate (item%frame_min(start:end))
            allocate (item%frame_max(start:end))
            allocate (item%frame_median(start:end))

            ! spectra
            allocate (item%mean_spectrum(naxes(3)))
            allocate (item%integrated_spectrum(naxes(3)))

            allocate (pixels(npixels))
            allocate (mask(npixels))

            pixels = 0.0
            mask = .false.
            thread_bSuccess = .true.

            call get_cdelt3(item, cdelt3)

            ! zero-out the spectra
            item%mean_spectrum = 0.0
            item%integrated_spectrum = 0.0

            item%frame_min = 1.0E30
            item%frame_max = -1.0E30

            !$omp PARALLEL DEFAULT(SHARED) PRIVATE(tid, start, end, num_per_node, status)&
            !$omp& PRIVATE(j, fpixels, lpixels, incs, tmp, frame_min, frame_max, frame_median)&
            !$omp& PRIVATE(mean_spec_val, int_spec_val, pixel_sum, pixel_count)&
            !$omp& PRIVATE(thread_buffer, thread_pixels, thread_mask, thread_arr)&
            !$omp& PRIVATE(thread_data, data_count, res)&
            !$omp& REDUCTION(.or.:thread_bSuccess)&
            !$omp& REDUCTION(max:dmax)&
            !$omp& REDUCTION(min:dmin)&
            !$omp& NUM_THREADS(max_threads)
            tid = 1 + OMP_GET_THREAD_NUM()

            ! allocate thread buffers
            allocate (thread_buffer(npixels))
            allocate (thread_arr(item%naxes(1), item%naxes(2)))
            allocate (thread_data(npixels))

            allocate (thread_pixels(npixels))
            allocate (thread_mask(npixels))

            thread_pixels = 0.0
            thread_mask = .false.

            ! reset the initial counter
            start = 0
            end = 0
            num_per_node = 0

            do
                ! update the progress with work done so far
                call update_progress(item, num_per_node)

                ! dynamically request / get the range blocks
                if (.not. c_associated(root)) then
                    ! a direct (local) request
                    call get_channel_range(item, start, end, status)
                else
                    ! submit work completed in the previous step (<num_per_node>)
                    ! fetch the range from the root node via HTTP
                    call fetch_channel_range(root, item%datasetid, size(item%datasetid), start, end, status) ! a C function defined in http.c
                end if

                ! LOOP EXIT
                ! -2 : a catastrophic error
                ! -1 : end of FITS file (no more work to do)

                ! LOOP CONTINUE
                ! 0 : OK
                ! 1 : accepted, header not ready yet
                if (status .lt. 0) exit ! one comparison handles it all, neat!

                ! process the block
                if ((start .gt. 0) .and. (end .gt. 0)) then
                    num_per_node = end - start + 1
                    print *, "TID", tid, 'START', start, 'END', end, 'NUM_PER_NODE', num_per_node

                    ! get a current OpenMP thread (starting from 0 as in C)
                    tid = 1 + OMP_GET_THREAD_NUM()

                    ! a "plain" DO LOOP
                    do frame = start, end
                        ! starting bounds
                        fpixels = (/1, 1, frame, 1/)

                        ! ending bounds
                        lpixels = (/naxes(1), naxes(2), frame, 1/)

                        ! do not skip over any pixels
                        incs = 1

                        ! reset the status
                        status = 0

                        if (thread_units(tid) .ne. -1) then
                            call ftgsve(thread_units(tid), group, naxis, naxes,&
                            & fpixels, lpixels, incs, nullval, thread_buffer(:), anynull, status)
                        else
                            thread_bSuccess = .false.
                            cycle
                        end if

                        ! abort upon errors
                        if (status .ne. 0) then
                            print *, 'error reading frame', frame
                            thread_bSuccess = .false.

                            if (status .gt. 0) then
                                call printerror(status)
                            end if

                            cycle
                        else
                            thread_bSuccess = thread_bSuccess .and. .true.
                        end if

                        mean_spec_val = 0.0
                        int_spec_val = 0.0

                        pixel_sum = 0.0
                        pixel_count = 0

                        frame_min = 1.0E30
                        frame_max = -1.0E30

                        data_count = 0

                        ! res = (/frame_min, frame_max, 0.0, 0.0/)

                        ! the 'infamous' AVX-512 slowdown on Mac Pro ... a shame ...
                        ! call make_image_spectrumF32(c_loc(thread_buffer), c_loc(thread_pixels), c_loc(thread_mask), item%ignrval, &
                        ! &item%datamin, item%datamax, cdelt3, c_loc(res), npixels)

                        ! frame_min = res(1)
                        ! frame_max = res(2)
                        ! mean_spec_val = res(3)
                        ! int_spec_val = res(4)

                        ! disable FORTRAN, testing the Intel SPMD C
                        ! if (.false.) then
                        ! calculate the min/max values
                        do j = 1, npixels

                            tmp = thread_buffer(j)

                            if (isnan(tmp) .neqv. .true.) then
                                if (test_ignrval) then
                                    if (tmp .eq. item%ignrval) then
                                        ! skip the IGNRVAL pixels
                                        ! thread_mask(j, tid) = thread_mask(j, tid) .or. .false.
                                        cycle
                                    end if
                                end if

                                data_count = data_count + 1
                                thread_data(data_count) = tmp

                                frame_min = min(frame_min, tmp)
                                frame_max = max(frame_max, tmp)

                                ! integrate (sum up) pixels and a NaN mask
                                thread_pixels(j) = thread_pixels(j) + tmp
                                thread_mask(j) = thread_mask(j) .or. .true.

                                ! needed by the mean and integrated spectra
                                pixel_sum = pixel_sum + tmp
                                pixel_count = pixel_count + 1
                            else
                                ! thread_mask(j, tid) = thread_mask(j, tid) .or. .false.
                            end if

                        end do
                        ! end if

                        if (pixel_count .gt. 0) then
                            mean_spec_val = pixel_sum/real(pixel_count)
                            int_spec_val = pixel_sum*cdelt3
                        end if

                        item%frame_min(frame) = frame_min
                        item%frame_max(frame) = frame_max

                        if (data_count .gt. 0) then
                            item%frame_median(frame) = median(thread_data(1:data_count))
                        else
                            item%frame_median(frame) = ieee_value(0.0, ieee_quiet_nan)
                        end if

                        dmin = min(dmin, frame_min)
                        dmax = max(dmax, frame_max)

                        item%mean_spectrum(frame) = mean_spec_val
                        item%integrated_spectrum(frame) = int_spec_val

                        ! compress the pixels
                        if (allocated(item%compressed) .and. associated(thread_arr)) then
                            block
                                real :: ignrval, datamin, datamax

                                if (isnan(item%ignrval)) then
                                    ignrval = -1.0E30
                                else
                                    ignrval = item%ignrval
                                end if

                                datamin = item%datamin
                                datamax = item%datamax

                                thread_arr(:, :) = reshape(thread_buffer, item%naxes(1:2))
                                item%compressed(frame)%ptr => to_fixed(thread_arr(:, :), ignrval, datamin, datamax)
                                ! item%compressed(frame)%ptr => to_fixed(reshape(thread_buffer(:, tid), item%naxes(1:2)),&
                                ! & ignrval, datamin, datamax)
                            end block
                        end if
                    end do
                else
                    ! no work done at this step
                    num_per_node = 0
                end if
            end do

            !$omp critical
            pixels = pixels + thread_pixels
            mask = mask .or. thread_mask
            !$omp end critical

            ! release thread buffers
            if (associated(thread_buffer)) deallocate (thread_buffer)
            if (associated(thread_pixels)) deallocate (thread_pixels)
            if (associated(thread_mask)) deallocate (thread_mask)
            if (associated(thread_arr)) deallocate (thread_arr)
            if (associated(thread_data)) deallocate (thread_data)

            !$omp END PARALLEL

            item%dmin = dmin
            item%dmax = dmax

            item%pixels = reshape(pixels, naxes(1:2))
            item%mask = reshape(mask, naxes(1:2))

            call set_image_status(item, .true.)

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

        call print_progress(item)

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

    subroutine get_spectrum_range(item, frame_start, frame_end, ref_freq, first, last)
        type(dataset), pointer, intent(in) :: item
        real(kind=8), intent(in) :: frame_start, frame_end, ref_freq
        integer, intent(out) :: first, last

        first = 1
        last = 1

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

    subroutine get_frequency_range(ptr, freq_start, freq_end) bind(c)
        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item
        real(kind=c_double) :: freq_start, freq_end

        ! the speed of light [m/s]
        real(kind=8), parameter :: c = 299792458.0
        real(kind=8) :: f1, f2, v1, v2

        call c_f_pointer(ptr, item)

        if (item%has_velocity) then

            v1 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(1.0 - item%crpix3)
            v2 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(item%naxes(3) - item%crpix3)

            f1 = item%restfrq*sqrt((1.0 - v1/c)/(1.0 + v1/c))
            f2 = item%restfrq*sqrt((1.0 - v2/c)/(1.0 + v2/c))

            freq_start = MIN(f1, f2)/1.0E9 ! [Hz -> GHz]
            freq_end = MAX(f1, f2)/1.0E9 ! [Hz -> GHz]

            return
        end if

        if (item%has_frequency) then

            f1 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(1.0 - item%crpix3)
            f2 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(item%naxes(3) - item%crpix3)

            freq_start = MIN(f1, f2)/1.0E9 ! [Hz -> GHz]
            freq_end = MAX(f1, f2)/1.0E9 ! [Hz -> GHz]

            return
        end if

    end subroutine get_frequency_range

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
            first = 1 + item%naxes(3) - first
            last = 1 + item%naxes(3) - last
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

        v1 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(1.0 - item%crpix3)
        v2 = item%crval3*item%frame_multiplier + item%cdelt3*item%frame_multiplier*(item%naxes(3) - item%crpix3)

        band_lo = min(v1, v2)
        band_hi = max(v1, v2)

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

    subroutine make_image_statistics(item, width, height, pixels, mask, hist, tone)
        use, intrinsic :: iso_c_binding
        implicit none

        type(dataset), pointer, intent(in) :: item
        integer, intent(in) :: width, height
        real(kind=c_float), dimension(width, height) :: pixels
        logical(kind=c_bool), dimension(width, height) :: mask
        integer, allocatable, intent(out) :: hist(:)
        type(image_tone_mapping), intent(out) :: tone

        real, dimension(:), allocatable :: data
        real cdelt3, pmin, pmax, pmedian
        real mad, madP, madN
        integer countP, countN
        real pixel
        integer i, j, n
        real u, v
        real black, white, sensitivity, ratio_sensitivity
        integer stat

        tone%pmin = 0.0
        tone%pmax = 0.0
        tone%pmedian = 0.0
        tone%black = 0.0
        tone%white = 0.0
        tone%sensitivity = 0.0
        tone%ratio_sensitivity = 0.0

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
        call make_histogram(hist, data, pmin, pmax)

        n = size(data)

        if (n .eq. 0) return

        pmedian = median(data)
        print *, '50th quantile (median) = ', pmedian

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
        if (.not. allocated(tone%flux)) then
            block
                use classifier

                integer(kind=8), dimension(NBINS) :: cdf
                real(c_float), dimension(NBINS), target :: Slot
                integer(c_int) tone_mapping

                integer(kind=8) total

                ! the first histogram item
                total = hist(1)
                cdf(1) = hist(1)

                ! the remaining items
                do i = 2, NBINS
                    cdf(i) = cdf(i - 1) + hist(i)
                    total = total + hist(i)
                end do

                Slot = cdf/real(total)

                tone_mapping = histogram_classifier(c_loc(Slot))

                select case (tone_mapping)
                case (0)
                    tone%flux = 'legacy'
                case (1)
                    tone%flux = 'linear'
                case (2)
                    tone%flux = 'logistic'
                case (3)
                    tone%flux = 'ratio'
                case (4)
                    tone%flux = 'square'
                case default
                    tone%flux = 'legacy'
                end select
            end block
        end if

        print *, 'black = ', black, ', white = ', white, ', sensitivity = ', sensitivity

        tone%pmin = pmin
        tone%pmax = pmax
        tone%pmedian = pmedian
        tone%black = black
        tone%white = white
        tone%sensitivity = sensitivity
        tone%ratio_sensitivity = ratio_sensitivity

    end subroutine make_image_statistics

    subroutine make_histogram(hist, data, pmin, pmax)
        integer, allocatable, intent(out) :: hist(:)
        real, dimension(:), intent(in) :: data
        real, intent(in) :: pmin, pmax
        integer i, index, n
        real value

        allocate (hist(NBINS))

        ! reset the histogram
        hist = 0

        n = size(data)

        do i = 1, n
            ! bin the value to [0,1]
            value = (data(i) - pmin)/(pmax - pmin)

            ! get a histogram bin index
            index = 1 + int(value*NBINS)

            ! clamp the index to within [1,NBINS]
            ! (rounding errors might cause an out-of-bounds index value)
            index = max(min(index, NBINS), 1)
            hist(index) = hist(index) + 1
        end do

    end subroutine make_histogram

    ! --------------------------------------------------------------------
    ! REAL FUNCTION  median() :
    !    This function receives an array X of N entries, sorts it
    !    and computes the median.
    !    The returned value is of REAL type.
    ! --------------------------------------------------------------------

    real function median(X)
        use quantile_mod
        implicit none

        real, dimension(:), intent(in) :: X
        integer :: N

        ! timing
        integer(8) :: start_t, finish_t, crate, cmax
        real :: elapsed

        N = size(X)

        ! start the timer
        call system_clock(count=start_t, count_rate=crate, count_max=cmax)

        median = quantile(N/2, X)

        ! end the timer
        call system_clock(finish_t)
        elapsed = real(finish_t - start_t)/real(crate)

        ! print *, 'quantile elapsed time:', 1000*elapsed, ' [ms]'
    end function median

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

    type(C_PTR) function get_json(item, hist)
        implicit none

        type(dataset), pointer, intent(in) :: item
        integer, target, intent(in) :: hist(:)
        type(C_PTR) :: json
        integer(kind=8) :: filesize

        ! calculate the FITS file size
        filesize = nint(real(size(item%hdr)) + real(item%naxes(1))*real(item%naxes(2))&
                       &*real(item%naxes(3))*real(item%naxes(4))*real(abs(item%bitpix)/8), kind=8)

        json = begin_json()

        ! misc. values
        call add_json_integer(json, 'width'//c_null_char, item%naxes(1))
        call add_json_integer(json, 'height'//c_null_char, item%naxes(2))
        call add_json_integer(json, 'depth'//c_null_char, item%naxes(3))
        call add_json_integer(json, 'polarisation'//c_null_char, item%naxes(4))
        call add_json_long(json, 'filesize'//c_null_char, filesize)
        call add_json_real(json, 'IGNRVAL'//c_null_char, item%ignrval)

        call add_json_real(json, 'CD1_1'//c_null_char, item%cd1_1)
        call add_json_real(json, 'CD1_2'//c_null_char, item%cd1_2)
        call add_json_real(json, 'CD2_1'//c_null_char, item%cd2_1)
        call add_json_real(json, 'CD2_2'//c_null_char, item%cd2_2)

        call add_json_real(json, 'CRVAL1'//c_null_char, item%crval1)
        call add_json_real(json, 'CDELT1'//c_null_char, item%cdelt1)
        call add_json_real(json, 'CRPIX1'//c_null_char, item%crpix1)
        call add_json_string(json, 'CUNIT1'//c_null_char, trim(item%cunit1)//c_null_char)
        call add_json_string(json, 'CTYPE1'//c_null_char, trim(item%ctype1)//c_null_char)

        call add_json_real(json, 'CRVAL2'//c_null_char, item%crval2)
        call add_json_real(json, 'CDELT2'//c_null_char, item%cdelt2)
        call add_json_real(json, 'CRPIX2'//c_null_char, item%crpix2)
        call add_json_string(json, 'CUNIT2'//c_null_char, trim(item%cunit2)//c_null_char)
        call add_json_string(json, 'CTYPE2'//c_null_char, trim(item%ctype2)//c_null_char)

        call add_json_real(json, 'CRVAL3'//c_null_char, item%crval3)
        call add_json_real(json, 'CDELT3'//c_null_char, item%cdelt3)
        call add_json_real(json, 'CRPIX3'//c_null_char, item%crpix3)
        call add_json_string(json, 'CUNIT3'//c_null_char, trim(item%cunit3)//c_null_char)
        call add_json_string(json, 'CTYPE3'//c_null_char, trim(item%ctype3)//c_null_char)

        call add_json_real(json, 'BMAJ'//c_null_char, item%bmaj)
        call add_json_real(json, 'BMIN'//c_null_char, item%bmin)
        call add_json_real(json, 'BPA'//c_null_char, item%bpa)

        call add_json_string(json, 'BUNIT'//c_null_char, trim(item%bunit)//c_null_char)
        call add_json_string(json, 'BTYPE'//c_null_char, trim(item%btype)//c_null_char)
        call add_json_string(json, 'SPECSYS'//c_null_char, trim(item%specsys)//c_null_char)

        call add_json_real(json, 'RESTFRQ'//c_null_char, item%restfrq)
        call add_json_real(json, 'OBSRA'//c_null_char, item%obsra)
        call add_json_real(json, 'OBSDEC'//c_null_char, item%obsdec)

        call add_json_string(json, 'OBJECT'//c_null_char, trim(item%object)//c_null_char)
        call add_json_string(json, 'DATEOBS'//c_null_char, trim(item%date_obs)//c_null_char)
        call add_json_string(json, 'TIMESYS'//c_null_char, trim(item%timesys)//c_null_char)
        call add_json_string(json, 'LINE'//c_null_char, trim(item%line)//c_null_char)
        call add_json_string(json, 'FILTER'//c_null_char, trim(item%filter)//c_null_char)

        call add_json_integer_array(json, 'histogram'//c_null_char, c_loc(hist), size(hist))

        call end_json(json)

        get_json = json
    end function get_json

    subroutine image_spectrum_request(ptr, width, height, precision, fetch_data, fd) bind(C)
        ! use json_module
        use, intrinsic :: iso_c_binding
        implicit none

        type(C_PTR), intent(in), value :: ptr
        type(dataset), pointer :: item
        integer(kind=c_int), intent(in), value :: width, height, precision, fetch_data, fd

        real(kind=c_float), dimension(:, :), allocatable, target :: pixels
        logical(kind=c_bool), dimension(:, :), allocatable, target :: mask

        ! image histogram
        integer, allocatable :: hist(:)

        ! image tone mapping
        type(image_tone_mapping) :: tone

        type(C_PTR) :: json

        integer inner_width, inner_height
        integer img_width, img_height
        real scale

        ! timing
        real :: t1, t2

        call c_f_pointer(ptr, item)

        print *, '"', item%datasetId, '", width', width, ', height', height, ', precision', precision,&
        & ', fetch_data', fetch_data, ', pipe write end', fd

        if (.not. allocated(item%pixels)) return
        if (.not. allocated(item%mask)) return

        if (allocated(item%flux)) allocate (tone%flux, source=item%flux)

        ! get the inner image bounding box (excluding NaNs)
        call inherent_image_dimensions(item, inner_width, inner_height)

        ! get the downscaled image dimensions
        scale = get_image_scale(width, height, inner_width, inner_height)

        if (scale .lt. 1.0) then
            img_width = scale*item%naxes(1)
            img_height = scale*item%naxes(2)

            allocate (pixels(img_width, img_height))
            allocate (mask(img_width, img_height))

            ! downscale item%pixels and item%mask into pixels, mask

            call cpu_time(t1)

            if (scale .gt. 0.2) then
                call resizeLanczos(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), img_width, img_height, 3)
            else
                call resizeSuper(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), img_width, img_height)
            end if

            call cpu_time(t2)

            print *, 'resize pixels elapsed time:', 1000*(t2 - t1), '[ms]'

            ! Boolean mask: the naive Nearest-Neighbour method

            call cpu_time(t1)

            call resizeNearest(c_loc(item%mask), item%naxes(1), item%naxes(2), c_loc(mask), img_width, img_height)

            call cpu_time(t2)

            print *, 'resize mask elapsed time:', 1000*(t2 - t1), '[ms]'

            ! make an image histogram, decide on the flux etc.
            call make_image_statistics(item, img_width, img_height, pixels, mask, hist, tone)

            call write_image_spectrum(fd, trim(tone%flux)//c_null_char,&
                &tone%pmin, tone%pmax, tone%pmedian,&
                &tone%black, tone%white, tone%sensitivity, tone%ratio_sensitivity,&
                & img_width, img_height, precision, c_loc(pixels), c_loc(mask))

            deallocate (pixels)
            deallocate (mask)

        else
            img_width = item%naxes(1)
            img_height = item%naxes(2)

            ! make an image histogram, decide on the flux etc.
            call make_image_statistics(item, img_width, img_height, item%pixels, item%mask, hist, tone)

            call write_image_spectrum(fd, trim(tone%flux)//c_null_char,&
                &tone%pmin, tone%pmax, tone%pmedian,&
                &tone%black, tone%white, tone%sensitivity, tone%ratio_sensitivity,&
                & img_width, img_height, precision, c_loc(item%pixels), c_loc(item%mask))

        end if

        print *, 'scale = ', scale, 'image dimensions:', img_width, 'x', img_height

        if (fetch_data .eq. 1) then

            ! JSON string
            json = get_json(item, hist)
            call write_json(fd, json)
            call delete_json(json)

            ! FITS header
            if (allocated(item%hdr)) then
                print *, 'FITS header size:', size(item%hdr)
                ! print *, item%hdr
                call write_header(fd, item%hdr, size(item%hdr))
                ! call write_header(fd, 'NULL'//c_null_char)
            else
                call write_header(fd, 'NULL', 4)
            end if

            ! send FPzip-compressed spectra
            print *, 'calling image_spectrum_request subroutine'

            ! mean spectrum
            if (allocated(item%mean_spectrum)) then
                call write_spectrum(fd, c_loc(item%mean_spectrum), size(item%mean_spectrum), FPZIP_HIGH_PRECISION)
            end if

            ! integrated spectrum
            if (allocated(item%integrated_spectrum)) then
                call write_spectrum(fd, c_loc(item%integrated_spectrum), size(item%integrated_spectrum), FPZIP_HIGH_PRECISION)
            end if
        end if

    end subroutine image_spectrum_request
end module fits
