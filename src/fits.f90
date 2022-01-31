module fits
    use, intrinsic :: ISO_C_BINDING
    use, intrinsic :: ieee_arithmetic
    use zfp_array
    implicit none

    integer(kind=4), parameter :: NBINS = 1024
    real, parameter :: PI = 4.D0*DATAN(1.D0)

    integer(c_int), parameter :: FPZIP_MEDIUM_PRECISION = 16
    integer(c_int), parameter :: FPZIP_HIGH_PRECISION = 24

    integer(c_int), parameter :: ZFP_HIGH_PRECISION = 16
    integer(c_int), parameter :: ZFP_MEDIUM_PRECISION = 11
    integer(c_int), parameter :: ZFP_LOW_PRECISION = 8
    integer(c_int), parameter :: ZFP_MIN_EXP = -1074

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
        character(len=16) :: flux
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

    end interface

contains
    subroutine close_fits_file(item)
        type(dataset) :: item
        integer status

        ! there is nothing to do if the FITS file has never been opened
        if (item%unit .eq. -1) return

        print *, item%datasetid, ': closing the FITS file unit'

        call ftclos(item%unit, status)
        call ftfiou(item%unit, status)

    end subroutine close_fits_file

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

    subroutine load_fits_file(datasetid, datasetid_len, filepath, filepath_len, flux, flux_len) bind(C)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(kind=c_size_t), intent(in), value :: datasetid_len, filepath_len, flux_len
        character(kind=c_char), dimension(datasetid_len), intent(in) :: datasetid
        character(kind=c_char), dimension(filepath_len), intent(in) :: filepath
        character(kind=c_char), dimension(flux_len), intent(in) :: flux

        type(dataset), pointer :: item

        integer(8) :: start, finish, crate, cmax, id
        real :: elapsed

        print *, "datasetid: '", datasetid, "', flux: '", flux, "', filepath: '", filepath, "'"

        allocate (item)

        ! init mutexes
        if (item%header_mtx%i .eq. 0) call g_mutex_init(c_loc(item%header_mtx))
        if (item%error_mtx%i .eq. 0) call g_mutex_init(c_loc(item%error_mtx))
        if (item%ok_mtx%i .eq. 0) call g_mutex_init(c_loc(item%ok_mtx))
        if (item%progress_mtx%i .eq. 0) call g_mutex_init(c_loc(item%progress_mtx))

        item%datasetid = datasetid
        item%progress = 0
        item%elapsed = 0
        ! allocate (item%uri, source=filename) ! is it needed ?
        call set_ok_status(item, .false.)
        call set_error_status(item, .false.)
        call set_header_status(item, .false.)

        call insert_dataset(item%datasetid, c_loc(item))

        ! start the timer
        call system_clock(count=start, count_rate=crate, count_max=cmax)

        ! call read_fits_file()

        ! end the timer
        call system_clock(finish)
        elapsed = real(finish - start)/real(crate)

        print *, "finished loading ", item%datasetid, ", elapsed time: ", elapsed, " [s]"

        ! reset the timeout clock
        call reset_clock(item)

    end subroutine load_fits_file
end module fits
