module fits
   use, intrinsic :: ISO_C_BINDING
   use, intrinsic :: ieee_arithmetic
   use fixed_array
   use lz4
   use logger_mod, only: logger_init, logger => master_logger
   use :: unix_pthread

   implicit none

   integer(kind=4), parameter :: NBINS = 1024

   integer(c_int), parameter :: ZFP_HIGH_PRECISION = 16
   integer(c_int), parameter :: ZFP_MEDIUM_PRECISION = 11
   integer(c_int), parameter :: ZFP_LOW_PRECISION = 8
   integer(c_int), parameter :: ZFP_MIN_EXP = -1074

   ! FITS channels are allocated to cluster nodes in blocks
   real, parameter :: GOLDEN_RATIO = (1 + sqrt(5.0))/2
   integer, parameter :: MAX_CHANNEL_BLOCK = 4 ! 32 ! 16 ! 64 ! 128

   type(c_pthread_mutex_t), save :: logger_mtx
   type(c_pthread_mutex_t), save :: file_unit_mtx

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
      ! input
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

      ! output
      integer(kind=c_int) :: fd

      type(C_PTR) :: ptr

   end type image_spectrum_request_f

   type, bind(c) :: video_request_f
      ! input
      logical(kind=c_bool) :: keyframe
      integer(c_int) :: frame

      ! needed by tone mapping
      type(C_PTR) :: flux
      integer(kind=c_int) :: len

      ! output
      integer(kind=c_int) :: width
      integer(kind=c_int) :: height
      logical(kind=c_bool) :: downsize
      integer(kind=c_int) :: fd

      type(C_PTR) :: ptr
   end type video_request_f

   type, bind(c) :: video_fetch_f
      ! input
      type(c_ptr) :: datasetid
      integer(c_int) :: len
      logical(kind=c_bool) :: keyframe
      integer(c_int) :: frame

      ! tone mapping
      type(C_PTR) :: flux
      real(kind=c_float) :: dmin, dmax, dmedian
      real(kind=c_float) :: sensitivity, slope
      real(kind=c_float) :: white, black

      ! output
      integer(kind=c_int) :: width
      integer(kind=c_int) :: height
      logical(kind=c_bool) :: downsize

      ! to be filled in C
      type(C_PTR) :: pixels, mask
      logical(kind=c_bool) :: valid
   end type video_fetch_f

   type, bind(c) :: video_req_f
      ! input
      logical(kind=c_bool) :: keyframe
      integer(c_int) :: frame

      ! tone mapping
      type(C_PTR) :: flux
      integer(c_int) :: len
      real(kind=c_float) :: dmin, dmax, dmedian
      real(kind=c_float) :: sensitivity, slope
      real(kind=c_float) :: white, black

      ! output
      integer(kind=c_int) :: width
      integer(kind=c_int) :: height
      logical(kind=c_bool) :: downsize
      integer(kind=c_int) :: fd
      type(C_PTR) :: ptr

   end type video_req_f

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

   type, bind(C) :: mad_req_t
      type(c_ptr) :: datasetid
      integer(c_int) :: len

      ! the all-data median (input)
      real(c_float) :: dmedian

      ! positive
      real(c_float) :: sumP
      integer(c_int64_t) :: countP

      ! negative
      real(c_float) :: sumN
      integer(c_int64_t) :: countN

      ! FITS cube range
      integer(c_int) :: first, last
   end type mad_req_t

   type, bind(C) :: inner_dims_req_t
      type(c_ptr) :: datasetid
      integer(c_int) :: len
      integer(c_int) :: width
      integer(c_int) :: height
   end type inner_dims_req_t

   type, bind(C) :: image_req_t
      type(c_ptr) :: datasetid
      integer(c_int) :: len
      type(c_ptr) :: pixels
      type(c_ptr) :: mask
      integer(c_int) :: width
      integer(c_int) :: height
   end type image_req_t

   type, bind(C) :: image_spectrum_request_t
      type(c_ptr) :: datasetid
      integer(c_int) :: len
      ! input parameters
      logical(kind=c_bool) :: image
      integer(c_int) :: x1, y1, x2, y2
      integer(kind(circle)) :: beam
      integer(kind(medium)) :: intensity
      real(c_double) :: frame_start, frame_end, ref_freq

      ! outputs
      type(c_ptr) :: pixels
      type(c_ptr) :: mask
      type(c_ptr) :: spectrum
      integer(c_int) :: dimx
      integer(c_int) :: dimy
      integer(c_int) :: length
      logical(kind=c_bool) :: valid
   end type image_spectrum_request_t

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
      real(kind=c_float) dmin, dmax, dmedian
      real(kind=c_float) dmad, dmadN, dmadP
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
      logical :: video = .false.

      ! mutexes
      type(c_pthread_mutex_t) :: header_mtx, ok_mtx, error_mtx, progress_mtx, image_mtx, video_mtx

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

      ! int mkcache(const char *dir);
      integer(c_int) function mkcache(dir) BIND(C, name='mkcache')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(kind=c_char), intent(in) :: dir(*)

      end function mkcache

      ! void rmcache(const char *dir)
      subroutine rmcache(dir) BIND(C, name='rmcache')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(kind=c_char), intent(in) :: dir(*)

      end subroutine rmcache

      ! int remove(const char *file)
      subroutine remove(file) BIND(C, name='remove')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(kind=c_char), intent(in) :: file(*)

      end subroutine remove

      ! int rdopen(const char *file);
      integer(c_int) function rdopen(file) BIND(C, name='rdopen')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(kind=c_char), intent(in) :: file(*)

      end function rdopen

      ! int wropen(const char *file);
      integer(c_int) function wropen(file) BIND(C, name='wropen')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         character(kind=c_char), intent(in) :: file(*)

      end function wropen

      ! int read_frame(int fd, void *dst, int pos, size_t frame_size)
      integer(c_int) function read_frame(fd, dst, pos, frame_size)&
      &BIND(C, name='read_frame')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd
         type(c_ptr), value :: dst
         integer(c_int), value, intent(in) :: pos
         integer(c_size_t), value, intent(in) :: frame_size
      end function read_frame

      ! int write_frame(int fd, void *src, size_t frame_size)
      integer(c_int) function write_frame(fd, src, frame_size)&
      &BIND(C, name='write_frame')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd
         type(c_ptr), value :: src
         integer(c_size_t), value, intent(in) :: frame_size
      end function write_frame

      ! close(int fd);
      subroutine closefd(fd) BIND(C, name='close')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd

      end subroutine closefd

      recursive subroutine fetch_inner_dimensions(arg) BIND(C)
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), intent(in), value :: arg   ! a pointer to type(inner_dims_req_t)
      end subroutine fetch_inner_dimensions

      recursive subroutine fetch_global_statistics(arg) BIND(C)
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), intent(in), value :: arg   ! a pointer to type(mad_req_t)
      end subroutine fetch_global_statistics

      recursive subroutine fetch_image(arg) BIND(C)
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), intent(in), value :: arg   ! a pointer to type(image_req_t)
      end subroutine fetch_image

      recursive subroutine fetch_video_frame(arg) BIND(C)
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), intent(in), value :: arg   ! a pointer to type(video_fetch_f)
      end subroutine fetch_video_frame

      recursive subroutine fetch_realtime_image_spectrum(arg) BIND(C)
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), intent(in), value :: arg   ! a pointer to type(image_spectrum_request_t)
      end subroutine fetch_realtime_image_spectrum

      subroutine fetch_channel_range(root, datasetid, len, start, end, status,&
      &frame_min, frame_max, frame_median,&
      &mean_spectrum, integrated_spectrum) BIND(C, name='fetch_channel_range')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), value :: root
         character(kind=c_char), intent(in) :: datasetid(*)
         integer(c_int), value :: len
         integer(c_int) :: start, end, status
         type(c_ptr), value, intent(in) :: frame_min, frame_max, frame_median
         type(c_ptr), value, intent(in) :: mean_spectrum, integrated_spectrum

      end subroutine fetch_channel_range

      integer(c_int) function submit_progress(root, datasetid, len, progress) BIND(C, name='submit_progress')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), value :: root
         character(kind=c_char), intent(in) :: datasetid(*)
         integer(c_int), value :: len, progress

      end function submit_progress

      ! void close_pipe(int fd);
      subroutine close_pipe(fd) BIND(C, name='close_pipe')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(kind=c_int), value, intent(in) :: fd
      end subroutine close_pipe

      ! void free()
      subroutine free(ptr) BIND(C, name='free')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(c_ptr), value :: ptr
      end Subroutine

      ! export void  make_image_spectrumF32(uniform float src[], uniform float pixels[], uniform unsigned int8 mask[], uniform float ignrval, uniform float datamin, uniform float datamax,  uniform float cdelt3, uniform float res[], uniform int npixels)
      subroutine make_image_spectrumF32(src, pixels, mask, data_mask, ignrval, datamin, datamax, cdelt3, res, npixels)&
      & BIND(C, name='make_image_spectrumF32')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: src, pixels, mask, data_mask, res
         real(c_float), value, intent(in) :: ignrval, datamin, datamax, cdelt3
         integer(c_int), value, intent(in) :: npixels

      end subroutine make_image_spectrumF32

      ! export uniform float viewport_spectrum_rect(uniform struct fixed_block_t compressed[], uniform int width,, uniform int height,&
      ! uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
      real(c_float) function viewport_spectrum_rect(compressed, width, height, pmin, pmax,&
      &x1, x2, y1, y2, average, cdelt3) BIND(C, name="viewport_spectrum_rect")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         real(c_float), value, intent(in) :: pmin, pmax
         integer(c_int), value, intent(in) :: x1, x2, y1, y2, average
         real(c_float), value, intent(in) :: cdelt3

      end function viewport_spectrum_rect

      ! export uniform float viewport_spectrum_circle(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform float cx, uniform float cy, uniform float r2, uniform bool average, uniform float cdelt3)
      real(c_float) function viewport_spectrum_circle(compressed, width, height, pmin, pmax,&
      &x1, x2, y1, y2, cx, cy, r2, average, cdelt3) BIND(C, name="viewport_spectrum_circle")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         real(c_float), value, intent(in) :: pmin, pmax
         integer(c_int), value, intent(in) :: x1, x2, y1, y2, average
         real(c_float), value, intent(in) :: cx, cy, r2
         real(c_float), value, intent(in) :: cdelt3

      end function viewport_spectrum_circle

      ! export uniform float viewport_image_spectrum_rect(uniform struct fixed_block_t compressed[],&
      ! uniform int width, uniform int height, uniform float pixels[], uniform unsigned int8 mask[], uniform int stride,&
      ! uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform bool average, uniform float cdelt3)
      real(c_float) function viewport_image_spectrum_rect(compressed, width, height, pmin, pmax, pixels, mask, &
      & stride, x1, x2, y1, y2, average, cdelt3) BIND(C, name="viewport_image_spectrum_rect")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         real(c_float), value, intent(in) :: pmin, pmax
         type(C_PTR), value, intent(in) :: pixels, mask
         integer(c_int), value, intent(in) :: stride
         integer(c_int), value, intent(in) :: x1, x2, y1, y2, average
         real(c_float), value, intent(in) :: cdelt3

      end function viewport_image_spectrum_rect

      ! export uniform float viewport_image_spectrum_circle(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float view_pixels[], uniform bool view_mask[], uniform int stride, uniform int x1, uniform int x2, uniform int y1, uniform int y2, uniform float cx, uniform float cy, uniform float r2, uniform bool average, uniform float cdelt3)
      real(c_float) function viewport_image_spectrum_circle(compressed, width, height, pmin, pmax, pixels, mask, &
      &stride, x1, x2, y1, y2, cx, cy, r2, average, cdelt3) BIND(C, name="viewport_image_spectrum_circle")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         real(c_float), value, intent(in) :: pmin, pmax
         type(C_PTR), value, intent(in) :: pixels, mask
         integer(c_int), value, intent(in) :: stride
         integer(c_int), value, intent(in) :: x1, x2, y1, y2, average
         real(c_float), value, intent(in) :: cx, cy, r2
         real(c_float), value, intent(in) :: cdelt3

      end function viewport_image_spectrum_circle

      ! export void make_global_statistics(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform float median, uniform float sumP[], uniform int64 countP[], uniform float sumN[], uniform int64 countN[])
      subroutine make_global_statistics(compressed, width, height, median,&
         &sumP, countP, sumN, countN) BIND(C, name="make_global_statistics")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         ! passed by value
         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         real(c_float), value, intent(in) :: median

         ! passed by reference
         real(c_float), intent(inout) :: sumP, sumN
         integer(c_int64_t), intent(inout) :: countP, countN

      end subroutine make_global_statistics

      ! export void make_video_frame_fixed_linear(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float slope)
      subroutine make_video_frame_fixed_linear(compressed, width, height,&
        &dst_luma, dst_mask, stride, black, slope) BIND(C, name="make_video_frame_fixed_linear")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         type(C_PTR), value, intent(in) :: dst_luma, dst_mask
         integer(c_int), value, intent(in) :: stride
         real(c_float), value, intent(in) :: black, slope

      end subroutine make_video_frame_fixed_linear

      ! export void make_video_frame_fixed_logistic(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float median, uniform float sensitivity)
      subroutine make_video_frame_fixed_logistic(compressed, width, height,&
         &dst_luma, dst_mask, stride, median, sensitivity) BIND(C, name="make_video_frame_fixed_logistic")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         type(C_PTR), value, intent(in) :: dst_luma, dst_mask
         integer(c_int), value, intent(in) :: stride
         real(c_float), value, intent(in) :: median, sensitivity

      end subroutine make_video_frame_fixed_logistic

      ! export void make_video_frame_fixed_ratio(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity)
      subroutine make_video_frame_fixed_ratio(compressed, width, height,&
         &dst_luma, dst_mask, stride, black, sensitivity) BIND(C, name="make_video_frame_fixed_ratio")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         type(C_PTR), value, intent(in) :: dst_luma, dst_mask
         integer(c_int), value, intent(in) :: stride
         real(c_float), value, intent(in) :: black, sensitivity

      end subroutine make_video_frame_fixed_ratio

      ! export void make_video_frame_fixed_square(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float black, uniform float sensitivity)
      subroutine make_video_frame_fixed_square(compressed, width, height,&
         &dst_luma, dst_mask, stride, black, sensitivity) BIND(C, name="make_video_frame_fixed_square")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         type(C_PTR), value, intent(in) :: dst_luma, dst_mask
         integer(c_int), value, intent(in) :: stride
         real(c_float), value, intent(in) :: black, sensitivity

      end subroutine make_video_frame_fixed_square

      ! export void make_video_frame_fixed_legacy(uniform struct fixed_block_t compressed[], uniform int width, uniform int height, uniform unsigned int8 dst_luma[], uniform unsigned int8 dst_mask[], uniform int stride, uniform float dmin, uniform float dmax, uniform float lmin, uniform float lmax)
      subroutine make_video_frame_fixed_legacy(compressed, width, height,&
         &dst_luma, dst_mask, stride, dmin, dmax, lmin, lmax) BIND(C, name="make_video_frame_fixed_legacy")
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: compressed
         integer(c_int), value, intent(in) :: width, height
         type(C_PTR), value, intent(in) :: dst_luma, dst_mask
         integer(c_int), value, intent(in) :: stride
         real(c_float), value, intent(in) :: dmin, dmax, lmin, lmax

      end subroutine make_video_frame_fixed_legacy

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

      ! void write_elapsed(int fd, const float* elapsed)
      subroutine write_elapsed(fd, elapsed) BIND(C, name='write_elapsed')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd
         real(c_float), intent(in) :: elapsed
      end subroutine write_elapsed

      subroutine write_spectrum(fd, spectrum, n, prec) BIND(C, name='write_spectrum')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd, n, prec
         type(C_PTR), value, intent(in) :: spectrum
      end subroutine write_spectrum

      subroutine write_histogram(fd, histogram, n) BIND(C, name='write_histogram')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd, n
         type(C_PTR), value, intent(in) :: histogram
      end subroutine write_histogram

      ! void write_viewport(int fd, int width, int height, const float *pixels, const bool *mask)
      subroutine write_viewport(fd, width, height, pixels, mask, precision) BIND(C, name='write_viewport')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd, width, height, precision
         type(C_PTR), value :: pixels, mask

      end subroutine write_viewport

      ! size_t chunked_write(int fd, const char *src, size_t n)
      integer(kind=c_size_t) function chunked_write(fd, src, n) BIND(C, name='chunked_write')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         integer(c_int), value, intent(in) :: fd
         type(C_PTR), value, intent(in) :: src
         integer(c_size_t), value, intent(in) :: n
      end function chunked_write

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
      rc = c_pthread_mutex_init(file_unit_mtx, c_null_ptr)
   end subroutine init_fortran

   subroutine cleanup_fortran() BIND(C, name='cleanup_fortran')
      use, intrinsic :: iso_c_binding
      implicit none

      integer rc

      rc = c_pthread_mutex_destroy(logger_mtx)
      rc = c_pthread_mutex_destroy(file_unit_mtx)
   end subroutine cleanup_fortran

   subroutine init_fortran_logging(log_file, len) BIND(C, name='init_fortran_logging')
      use, intrinsic :: iso_c_binding
      implicit none

      integer(kind=c_size_t), intent(in), value :: len
      character(kind=c_char), dimension(len), intent(in) :: log_file

      character(len=len) :: filename
      integer :: i

      do i = 1, int(len, kind=4)
         filename(i:i) = log_file(i)
      end do

      print *, "FORTRAN LOG FILE: ", filename

      ! Initialise the logger prior to use
      call logger_init(filename)

   end subroutine init_fortran_logging

   !   "Convert an integer to string."
   character(len=16) pure function str(k)
      integer, intent(in) :: k
      write (str, *) k
      str = adjustl(str)
   end function str

   subroutine delete_dataset_prev(ptr, dir, len) BIND(C, name='delete_dataset_prev')
      implicit none

      type(C_PTR), intent(in), value :: ptr
      type(dataset), pointer :: item

      integer(kind=c_size_t), intent(in), value :: len
      character(kind=c_char), dimension(len), intent(in) :: dir

      character(len=:), allocatable :: cache
      character(len=1024) :: file
      logical :: file_exists, bSuccess

      integer :: i, rc, status
      integer :: fileunit, ios
      character(256) :: iomsg

      call c_f_pointer(ptr, item)

      allocate (character(len + 1 + size(item%datasetid))::cache)

      ! the cache directory
      do i = 1, int(len, kind=4)
         cache(i:i) = dir(i)
      end do

      ! append a slash
      cache(len + 1:len + 1) = '/'

      ! and append the datasetid
      do i = 1, size(item%datasetid)
         cache(len + 1 + i:len + 1 + i) = item%datasetid(i)
      end do

      ! only make a cache for datasets with valid data/image, no errors
      if (get_image_status(ptr) .eq. 1) then
         ! create a cache directory using the <datasetid> folder name
         status = mkcache(cache//c_null_char)

         if (status .eq. 0) then
            bSuccess = .true.
         else
            bSuccess = .false.
         end if
      else
         ! remove the (non-empty) cache directory
         call rmcache(cache//c_null_char)

         ! error
         bSuccess = .false.
         status = -1
      end if

      print *, 'deleting ', item%datasetid, '; cache dir: ', cache, ', status', status, ', bSuccess', bSuccess

      ! write the dataset to a cache file so as to speed up subsequent loading

      rc = c_pthread_mutex_destroy(item%header_mtx)
      rc = c_pthread_mutex_destroy(item%error_mtx)
      rc = c_pthread_mutex_destroy(item%ok_mtx)
      rc = c_pthread_mutex_destroy(item%progress_mtx)
      rc = c_pthread_mutex_destroy(item%image_mtx)
      rc = c_pthread_mutex_destroy(item%video_mtx)

      ! deallocate compressed memory regions
      if (allocated(item%compressed)) then

         do i = 1, size(item%compressed)

            if (associated(item%compressed(i)%ptr)) then

               if (status .eq. 0) then
                  ! if (allocated(file)) deallocate (file)
                  file = cache//'/'//trim(str(i))//'.bin'
                  INQUIRE (FILE=trim(file), EXIST=file_exists)

                  if (.not. file_exists) then
                     open (newunit=fileunit, file=trim(file), status='replace', access='stream',&
                     &form='unformatted', IOSTAT=ios, IOMSG=iomsg)

                     if (ios .ne. 0) then
                        print *, "error creating a file ", file, ' : ', trim(iomsg)

                        ! upon error
                        bSuccess = .false.
                     else
                        ! dump the compressed data
                        write (unit=fileunit, IOSTAT=ios, IOMSG=iomsg) item%compressed(i)%ptr(:, :)

                        ! delete the file upon a write error
                        if (ios .ne. 0) then
                           print *, "error serialising channel", i, 'to a binary file ', trim(file), ' : ', trim(iomsg)

                           ! delete the file
                           close (fileunit, status='delete')
                        else
                           ! close the file
                           close (fileunit)
                        end if

                        print *, "serialised channel", i, 'to a binary file ', trim(file)
                     end if

                  end if
               end if

               deallocate (item%compressed(i)%ptr)
               nullify (item%compressed(i)%ptr)
            end if
         end do

         deallocate (item%compressed)
      end if

      if (bSuccess) call save_dataset(item, cache)

      deallocate (item)
   end subroutine delete_dataset_prev

   subroutine delete_dataset_fortran(ptr, dir, len) BIND(C, name='delete_dataset_fortran')
      implicit none

      type(C_PTR), intent(in), value :: ptr
      type(dataset), pointer :: item

      integer(kind=c_size_t), intent(in), value :: len
      character(kind=c_char), dimension(len), intent(in) :: dir

      character(len=:), allocatable :: cache
      character(len=1024) :: file
      logical :: file_exists, bSuccess

      integer :: i, rc, status
      integer :: fileunit, ios
      integer :: index_unit, data_unit
      character(256) :: iomsg

      call c_f_pointer(ptr, item)

      allocate (character(len + 1 + size(item%datasetid))::cache)

      ! the cache directory
      do i = 1, int(len, kind=4)
         cache(i:i) = dir(i)
      end do

      ! append a slash
      cache(len + 1:len + 1) = '/'

      ! and append the datasetid
      do i = 1, size(item%datasetid)
         cache(len + 1 + i:len + 1 + i) = item%datasetid(i)
      end do

      ! only make a cache for datasets with valid data/image, no errors
      if (get_image_status(ptr) .eq. 1) then
         ! create a cache directory using the <datasetid> folder name
         status = mkcache(cache//c_null_char)

         if (status .eq. 0) then
            bSuccess = .true.
         else
            bSuccess = .false.
         end if
      else
         ! remove the (non-empty) cache directory
         call rmcache(cache//c_null_char)

         ! error
         bSuccess = .false.
         status = -1
      end if

      print *, 'deleting ', item%datasetid, '; cache dir: ', cache, ', status', status, ', bSuccess', bSuccess

      ! write the dataset to a cache file so as to speed up subsequent loading

      rc = c_pthread_mutex_destroy(item%header_mtx)
      rc = c_pthread_mutex_destroy(item%error_mtx)
      rc = c_pthread_mutex_destroy(item%ok_mtx)
      rc = c_pthread_mutex_destroy(item%progress_mtx)
      rc = c_pthread_mutex_destroy(item%image_mtx)
      rc = c_pthread_mutex_destroy(item%video_mtx)

      index_unit = -1
      data_unit = -1

      ! deallocate compressed memory regions
      if (allocated(item%compressed)) then

         file = cache//'/index'
         INQUIRE (FILE=trim(file), EXIST=file_exists)

         if (.not. file_exists) then
            open (newunit=index_unit, file=trim(file), status='replace', access='stream',&
            &form='unformatted', IOSTAT=ios, IOMSG=iomsg)

            if (ios .ne. 0) then
               print *, "error creating an index file ", file, ' : ', trim(iomsg)

               ! upon error
               bSuccess = .false.
            end if
         end if

         file = cache//'/data'
         INQUIRE (FILE=trim(file), EXIST=file_exists)

         if (.not. file_exists) then
            open (newunit=data_unit, file=trim(file), status='replace', access='stream',&
            &form='unformatted', IOSTAT=ios, IOMSG=iomsg)

            if (ios .ne. 0) then
               print *, "error creating a data file ", file, ' : ', trim(iomsg)

               ! upon error
               bSuccess = .false.
            end if
         end if

         do i = 1, size(item%compressed)

            if (associated(item%compressed(i)%ptr)) then

               if (index_unit .ne. -1 .and. data_unit .ne. -1) then
                  ! add an index entry
                  write (unit=index_unit, IOSTAT=ios, IOMSG=iomsg) i

                  if (ios .ne. 0) then
                     print *, "error adding an index entry", i, ' : ', trim(iomsg)
                     bSuccess = .false.
                  end if

                  ! dump the compressed data
                  write (unit=data_unit, IOSTAT=ios, IOMSG=iomsg) item%compressed(i)%ptr(:, :)

                  if (ios .ne. 0) then
                     print *, "error serialising channel", i, 'to a binary data file : ', trim(iomsg)
                     bSuccess = .false.
                  end if
               end if

               deallocate (item%compressed(i)%ptr)
               nullify (item%compressed(i)%ptr)
            end if
         end do

         deallocate (item%compressed)
      end if

      if (index_unit .ne. -1) then
         if (bSuccess) then
            close (index_unit)
         else
            close (index_unit, status='delete')
         end if
      end if

      if (data_unit .ne. -1) then
         if (bSuccess) then
            close (data_unit)
         else
            close (data_unit, status='delete')
         end if
      end if

      if (bSuccess) call save_dataset(item, cache)

      deallocate (item)
   end subroutine delete_dataset_fortran

   subroutine delete_dataset(ptr, dir, len) BIND(C, name='delete_dataset')
      implicit none

      type(C_PTR), intent(in), value :: ptr
      type(dataset), pointer :: item

      integer(kind=c_size_t), intent(in), value :: len
      character(kind=c_char), dimension(len), intent(in) :: dir

      character(len=:), allocatable :: cache
      character(len=1024) :: file
      logical :: file_exists, bSuccess

      integer :: i, rc, status
      integer :: fileunit, ios
      integer :: index_unit
      integer(kind=c_int) :: data_unit
      integer(kind=c_size_t) :: array_size
      character(256) :: iomsg

      call c_f_pointer(ptr, item)

      allocate (character(len + 1 + size(item%datasetid))::cache)

      ! the cache directory
      do i = 1, int(len, kind=4)
         cache(i:i) = dir(i)
      end do

      ! append a slash
      cache(len + 1:len + 1) = '/'

      ! and append the datasetid
      do i = 1, size(item%datasetid)
         cache(len + 1 + i:len + 1 + i) = item%datasetid(i)
      end do

      ! only make a cache for datasets with valid data/image, no errors
      if (get_image_status(ptr) .eq. 1) then
         ! create a cache directory using the <datasetid> folder name
         status = mkcache(cache//c_null_char)

         if (status .eq. 0) then
            bSuccess = .true.
         else
            bSuccess = .false.
         end if
      else
         ! remove the (non-empty) cache directory
         call rmcache(cache//c_null_char)

         ! error
         bSuccess = .false.
         status = -1
      end if

      print *, 'deleting ', item%datasetid, '; cache dir: ', cache, ', status', status, ', bSuccess', bSuccess

      ! write the dataset to a cache file so as to speed up subsequent loading

      rc = c_pthread_mutex_destroy(item%header_mtx)
      rc = c_pthread_mutex_destroy(item%error_mtx)
      rc = c_pthread_mutex_destroy(item%ok_mtx)
      rc = c_pthread_mutex_destroy(item%progress_mtx)
      rc = c_pthread_mutex_destroy(item%image_mtx)
      rc = c_pthread_mutex_destroy(item%video_mtx)

      index_unit = -1
      data_unit = -1

      ! deallocate compressed memory regions
      if (allocated(item%compressed)) then

         file = cache//'/index'
         INQUIRE (FILE=trim(file), EXIST=file_exists)

         if (.not. file_exists) then
            open (newunit=index_unit, file=trim(file), status='replace', access='stream',&
            &form='unformatted', IOSTAT=ios, IOMSG=iomsg)

            if (ios .ne. 0) then
               print *, "error creating an index file ", file, ' : ', trim(iomsg)

               ! upon error
               bSuccess = .false.
            end if
         end if

         file = cache//'/data'
         INQUIRE (FILE=trim(file), EXIST=file_exists)

         if (.not. file_exists) then
            ! try to open the file for writing
            data_unit = wropen(trim(file)//c_null_char)

            if (data_unit .lt. 0) then
               print *, "error creating a data file ", file

               ! upon error
               bSuccess = .false.
            end if
         end if

         do i = 1, size(item%compressed)

            if (associated(item%compressed(i)%ptr)) then

               if (index_unit .ne. -1 .and. data_unit .ne. -1) then
                  ! add an index entry
                  write (unit=index_unit, IOSTAT=ios, IOMSG=iomsg) i

                  if (ios .ne. 0) then
                     print *, "error adding an index entry", i, ' : ', trim(iomsg)
                     bSuccess = .false.
                  end if

                  array_size = int(sizeof(item%compressed(i)%ptr(:, :)), kind=c_size_t)

                  ! dump the compressed data
                  ios = write_frame(data_unit, c_loc(item%compressed(i)%ptr(:, :)), array_size)

                  if (ios .ne. 0) then
                     print *, "error serialising channel", i, 'to a binary data file ', trim(file)
                     bSuccess = .false.
                  end if
               end if

               deallocate (item%compressed(i)%ptr)
               nullify (item%compressed(i)%ptr)
            end if
         end do

         deallocate (item%compressed)
      end if

      if (index_unit .ne. -1) then
         if (bSuccess) then
            close (index_unit)
         else
            close (index_unit, status='delete')
         end if
      end if

      if (data_unit .ge. 0) call closefd(data_unit)
      if (.not. bSuccess) call remove(trim(file)//c_null_char)

      if (bSuccess) call save_dataset(item, cache)

      deallocate (item)
   end subroutine delete_dataset

   subroutine serialise_fixed_array(compressed, frame, cache)
      implicit none

      type(fixed_block), dimension(:, :), intent(in) :: compressed
      integer, intent(in) :: frame
      character(len=*), intent(in) :: cache

      character(len=1024) :: file
      logical :: file_exists

      integer :: fileunit, ios, status
      character(256) :: iomsg

      ! first check if the "cache" directory exists
      ! create a cache directory using the <datasetid> folder name
      status = mkcache(cache//c_null_char)

      if (status .ne. 0) then
         print *, "could not create a cache directory ", cache, ", re-trying"
         status = mkcache(cache//c_null_char)

         if (status .ne. 0) then
            print *, "could not create a cache directory ", cache, ", aborting 'serialise_fixed_array'"
            return
         end if
      end if

      file = cache//'/'//trim(str(frame))//'.bin'
      INQUIRE (FILE=trim(file), EXIST=file_exists)

      if (.not. file_exists) then
         open (newunit=fileunit, file=trim(file), status='replace', access='stream',&
         &form='unformatted', IOSTAT=ios, IOMSG=iomsg)

         if (ios .ne. 0) then
            print *, "error creating a file ", file, ' : ', trim(iomsg)
         else
            ! dump the compressed data
            write (unit=fileunit, IOSTAT=ios, IOMSG=iomsg) compressed(:, :)

            ! delete the file upon a write error
            if (ios .ne. 0) then
               print *, "error serialising channel", frame, 'to a binary file ', trim(file), ' : ', trim(iomsg)

               ! delete the file
               close (fileunit, status='delete')
            else
               ! close the file
               close (fileunit)
            end if

            ! print *, "serialised channel", frame, 'to a binary file ', trim(file)
         end if

      end if

   end subroutine serialise_fixed_array

   subroutine save_dataset(item, cache)
      implicit none

      type(dataset), pointer, intent(in) :: item
      character(len=*), intent(in) :: cache

      ! character(len=:), allocatable :: file
      character(len=1024), allocatable :: file
      logical :: file_exists, bSuccess

      integer :: fileunit, ios
      character(256) :: iomsg

      file = cache//'/'//'state'

      INQUIRE (FILE=trim(file), EXIST=file_exists)

      if (file_exists) return

      open (newunit=fileunit, file=trim(file), status='replace', access='stream', form='unformatted',&
      &IOSTAT=ios, IOMSG=iomsg)

      if (ios .ne. 0) then
         print *, "error creating a file ", trim(file), ' : ', trim(iomsg)
         return
      end if

      bSuccess = .true.

      ! item%datasetid
      if (allocated(item%datasetid)) then
         write (unit=fileunit, IOSTAT=ios) size(item%datasetid)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%datasetid(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%uri
      if (allocated(item%uri)) then
         write (unit=fileunit, IOSTAT=ios) len(item%uri)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%uri
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%hdr
      if (allocated(item%hdr)) then
         write (unit=fileunit, IOSTAT=ios) size(item%hdr)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%hdr(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%naxis
      write (unit=fileunit, IOSTAT=ios) item%naxis
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%bitpix
      write (unit=fileunit, IOSTAT=ios) item%bitpix
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%naxes
      write (unit=fileunit, IOSTAT=ios) item%naxes(:)
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%frameid
      write (unit=fileunit, IOSTAT=ios) item%frameid
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%object
      write (unit=fileunit, IOSTAT=ios) item%object
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%line
      write (unit=fileunit, IOSTAT=ios) item%line
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%filter
      write (unit=fileunit, IOSTAT=ios) item%filter
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%date_obs
      write (unit=fileunit, IOSTAT=ios) item%date_obs
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%btype
      write (unit=fileunit, IOSTAT=ios) item%btype
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%bunit
      write (unit=fileunit, IOSTAT=ios) item%bunit
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%specsys
      write (unit=fileunit, IOSTAT=ios) item%specsys
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%timesys
      write (unit=fileunit, IOSTAT=ios) item%timesys
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cunit1
      write (unit=fileunit, IOSTAT=ios) item%cunit1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%ctype1
      write (unit=fileunit, IOSTAT=ios) item%ctype1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cunit2
      write (unit=fileunit, IOSTAT=ios) item%cunit2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%ctype2
      write (unit=fileunit, IOSTAT=ios) item%ctype2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cunit3
      write (unit=fileunit, IOSTAT=ios) item%cunit3
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%ctype3
      write (unit=fileunit, IOSTAT=ios) item%ctype3
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%ignrval
      write (unit=fileunit, IOSTAT=ios) item%ignrval
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%restfrq
      write (unit=fileunit, IOSTAT=ios) item%restfrq
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%bmaj
      write (unit=fileunit, IOSTAT=ios) item%bmaj
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%bmin
      write (unit=fileunit, IOSTAT=ios) item%bmin
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%bpa
      write (unit=fileunit, IOSTAT=ios) item%bpa
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crval1
      write (unit=fileunit, IOSTAT=ios) item%crval1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cdelt1
      write (unit=fileunit, IOSTAT=ios) item%cdelt1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crpix1
      write (unit=fileunit, IOSTAT=ios) item%crpix1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crval2
      write (unit=fileunit, IOSTAT=ios) item%crval2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cdelt2
      write (unit=fileunit, IOSTAT=ios) item%cdelt2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crpix2
      write (unit=fileunit, IOSTAT=ios) item%crpix2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crval3
      write (unit=fileunit, IOSTAT=ios) item%crval3
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cdelt3
      write (unit=fileunit, IOSTAT=ios) item%cdelt3
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%crpix3
      write (unit=fileunit, IOSTAT=ios) item%crpix3
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%obsra
      write (unit=fileunit, IOSTAT=ios) item%obsra
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%obsdec
      write (unit=fileunit, IOSTAT=ios) item%obsdec
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%datamin
      write (unit=fileunit, IOSTAT=ios) item%datamin
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%datamax
      write (unit=fileunit, IOSTAT=ios) item%datamax
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cd1_1
      write (unit=fileunit, IOSTAT=ios) item%cd1_1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cd1_2
      write (unit=fileunit, IOSTAT=ios) item%cd1_2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cd2_1
      write (unit=fileunit, IOSTAT=ios) item%cd2_1
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%cd2_2
      write (unit=fileunit, IOSTAT=ios) item%cd2_2
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%frame_multiplier
      write (unit=fileunit, IOSTAT=ios) item%frame_multiplier
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%has_velocity
      write (unit=fileunit, IOSTAT=ios) item%has_velocity
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%has_frequency
      write (unit=fileunit, IOSTAT=ios) item%has_frequency
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%flux
      if (allocated(item%flux)) then
         write (unit=fileunit, IOSTAT=ios) len(item%flux)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%flux
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%dmin
      write (unit=fileunit, IOSTAT=ios) item%dmin
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%dmax
      write (unit=fileunit, IOSTAT=ios) item%dmax
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%dmedian
      write (unit=fileunit, IOSTAT=ios) item%dmedian
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%dmad
      write (unit=fileunit, IOSTAT=ios) item%dmad
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%dmadN
      write (unit=fileunit, IOSTAT=ios) item%dmadN
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%dmadP
      write (unit=fileunit, IOSTAT=ios) item%dmadP
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%frame_min
      if (allocated(item%frame_min)) then
         write (unit=fileunit, IOSTAT=ios) size(item%frame_min)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%frame_min(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%frame_max
      if (allocated(item%frame_max)) then
         write (unit=fileunit, IOSTAT=ios) size(item%frame_max)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%frame_max(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%frame_median
      if (allocated(item%frame_median)) then
         write (unit=fileunit, IOSTAT=ios) size(item%frame_median)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%frame_median(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%pixels
      if (allocated(item%pixels)) then
         write (unit=fileunit, IOSTAT=ios) size(item%pixels)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%pixels(:, :)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%mask
      if (allocated(item%mask)) then
         write (unit=fileunit, IOSTAT=ios) size(item%mask)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%mask(:, :)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%is_optical
      write (unit=fileunit, IOSTAT=ios) item%is_optical
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%is_xray
      write (unit=fileunit, IOSTAT=ios) item%is_xray
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%video
      write (unit=fileunit, IOSTAT=ios) item%video
      if (ios .ne. 0) bSuccess = bSuccess .and. .false.

      ! item%mean_spectrum
      if (allocated(item%mean_spectrum)) then
         write (unit=fileunit, IOSTAT=ios) size(item%mean_spectrum)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%mean_spectrum(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! item%integrated_spectrum
      if (allocated(item%integrated_spectrum)) then
         write (unit=fileunit, IOSTAT=ios) size(item%integrated_spectrum)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.

         write (unit=fileunit, IOSTAT=ios) item%integrated_spectrum(:)
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      else
         write (unit=fileunit, IOSTAT=ios) 0
         if (ios .ne. 0) bSuccess = bSuccess .and. .false.
      end if

      ! delete the file upon any write errors
      if (.not. bSuccess) then
         ! delete the file
         close (fileunit, status='delete')
      else
         ! close the file
         close (fileunit)
      end if

      print *, 'serialised ', item%datasetid, ' to ', trim(file)

   end subroutine save_dataset

   subroutine load_dataset(item, cache, root, bSuccess)
      implicit none

      type(dataset), pointer, intent(inout) :: item
      character(len=*), intent(in) :: cache
      type(c_ptr), intent(in), value :: root
      logical, intent(out) ::  bSuccess

      ! character(len=:), allocatable :: file
      character(len=1024), allocatable :: file
      logical :: file_exists

      integer :: fileunit, ios, N, rc
      integer :: dims(2)
      character(256) :: iomsg

      bSuccess = .false.

      file = cache//'/'//'state'

      INQUIRE (FILE=trim(file), EXIST=file_exists)

      if (.not. file_exists) return

      ! open the state file for reading
      open (newunit=fileunit, file=trim(file), status='old', action='read', access='stream', form='unformatted',&
      & IOSTAT=ios, IOMSG=iomsg)

      if (ios .ne. 0) then
         print *, "error opening a file ", trim(file), ' : ', trim(iomsg)
         return
      end if

      ! item%datasetid
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%datasetid)) deallocate (item%datasetid)
         allocate (item%datasetid(N))
         read (unit=fileunit, IOSTAT=ios) item%datasetid(:)
         if (ios .ne. 0) go to 300
      end if

      ! item%uri
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%uri)) deallocate (item%uri)
         allocate (character(len=N)::item%uri)
         read (unit=fileunit, IOSTAT=ios) item%uri
         if (ios .ne. 0) go to 300
      end if

      ! item%hdr
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%hdr)) deallocate (item%hdr)
         allocate (item%hdr(N))
         read (unit=fileunit, IOSTAT=ios) item%hdr(:)
         if (ios .ne. 0) go to 300
      end if

      ! item%naxis
      read (unit=fileunit, IOSTAT=ios) item%naxis
      if (ios .ne. 0) go to 300

      ! item%bitpix
      read (unit=fileunit, IOSTAT=ios) item%bitpix
      if (ios .ne. 0) go to 300

      ! item%naxes
      read (unit=fileunit, IOSTAT=ios) item%naxes(:)
      if (ios .ne. 0) go to 300

      ! item%frameid
      read (unit=fileunit, IOSTAT=ios) item%frameid
      if (ios .ne. 0) go to 300

      ! item%object
      read (unit=fileunit, IOSTAT=ios) item%object
      if (ios .ne. 0) go to 300

      ! item%line
      read (unit=fileunit, IOSTAT=ios) item%line
      if (ios .ne. 0) go to 300

      ! item%filter
      read (unit=fileunit, IOSTAT=ios) item%filter
      if (ios .ne. 0) go to 300

      ! item%data_obs
      read (unit=fileunit, IOSTAT=ios) item%date_obs
      if (ios .ne. 0) go to 300

      ! item%btype
      read (unit=fileunit, IOSTAT=ios) item%btype
      if (ios .ne. 0) go to 300

      ! item%bunit
      read (unit=fileunit, IOSTAT=ios) item%bunit
      if (ios .ne. 0) go to 300

      ! item%specsys
      read (unit=fileunit, IOSTAT=ios) item%specsys
      if (ios .ne. 0) go to 300

      ! item%timesys
      read (unit=fileunit, IOSTAT=ios) item%timesys
      if (ios .ne. 0) go to 300

      ! item%cunit1
      read (unit=fileunit, IOSTAT=ios) item%cunit1
      if (ios .ne. 0) go to 300

      ! item%ctype1
      read (unit=fileunit, IOSTAT=ios) item%ctype1
      if (ios .ne. 0) go to 300

      ! item%cunit2
      read (unit=fileunit, IOSTAT=ios) item%cunit2
      if (ios .ne. 0) go to 300

      ! item%ctype2
      read (unit=fileunit, IOSTAT=ios) item%ctype2
      if (ios .ne. 0) go to 300

      ! item%cunit3
      read (unit=fileunit, IOSTAT=ios) item%cunit3
      if (ios .ne. 0) go to 300

      ! item%ctype3
      read (unit=fileunit, IOSTAT=ios) item%ctype3
      if (ios .ne. 0) go to 300

      ! item%ignrval
      read (unit=fileunit, IOSTAT=ios) item%ignrval
      if (ios .ne. 0) go to 300

      ! item%restfrq
      read (unit=fileunit, IOSTAT=ios) item%restfrq
      if (ios .ne. 0) go to 300

      ! item%bmaj
      read (unit=fileunit, IOSTAT=ios) item%bmaj
      if (ios .ne. 0) go to 300

      ! item%bmin
      read (unit=fileunit, IOSTAT=ios) item%bmin
      if (ios .ne. 0) go to 300

      ! item%bpa
      read (unit=fileunit, IOSTAT=ios) item%bpa
      if (ios .ne. 0) go to 300

      ! item%crval1
      read (unit=fileunit, IOSTAT=ios) item%crval1
      if (ios .ne. 0) go to 300

      ! item%cdelt1
      read (unit=fileunit, IOSTAT=ios) item%cdelt1
      if (ios .ne. 0) go to 300

      ! item%crpix1
      read (unit=fileunit, IOSTAT=ios) item%crpix1
      if (ios .ne. 0) go to 300

      ! item%crval2
      read (unit=fileunit, IOSTAT=ios) item%crval2
      if (ios .ne. 0) go to 300

      ! item%cdelt2
      read (unit=fileunit, IOSTAT=ios) item%cdelt2
      if (ios .ne. 0) go to 300

      ! item%crpix2
      read (unit=fileunit, IOSTAT=ios) item%crpix2
      if (ios .ne. 0) go to 300

      ! item%crval3
      read (unit=fileunit, IOSTAT=ios) item%crval3
      if (ios .ne. 0) go to 300

      ! item%cdelt3
      read (unit=fileunit, IOSTAT=ios) item%cdelt3
      if (ios .ne. 0) go to 300

      ! item%crpix3
      read (unit=fileunit, IOSTAT=ios) item%crpix3
      if (ios .ne. 0) go to 300

      ! item%obsra
      read (unit=fileunit, IOSTAT=ios) item%obsra
      if (ios .ne. 0) go to 300

      ! item%obsdec
      read (unit=fileunit, IOSTAT=ios) item%obsdec
      if (ios .ne. 0) go to 300

      ! item%datamin
      read (unit=fileunit, IOSTAT=ios) item%datamin
      if (ios .ne. 0) go to 300

      ! item%datamax
      read (unit=fileunit, IOSTAT=ios) item%datamax
      if (ios .ne. 0) go to 300

      ! item%cd1_1
      read (unit=fileunit, IOSTAT=ios) item%cd1_1
      if (ios .ne. 0) go to 300

      ! item%cd1_2
      read (unit=fileunit, IOSTAT=ios) item%cd1_2
      if (ios .ne. 0) go to 300

      ! item%cd2_1
      read (unit=fileunit, IOSTAT=ios) item%cd2_1
      if (ios .ne. 0) go to 300

      ! item%cd2_2
      read (unit=fileunit, IOSTAT=ios) item%cd2_2
      if (ios .ne. 0) go to 300

      ! item%frame_multiplier
      read (unit=fileunit, IOSTAT=ios) item%frame_multiplier
      if (ios .ne. 0) go to 300

      ! item%has_velocity
      read (unit=fileunit, IOSTAT=ios) item%has_velocity
      if (ios .ne. 0) go to 300

      ! item%has_frequency
      read (unit=fileunit, IOSTAT=ios) item%has_frequency
      if (ios .ne. 0) go to 300

      ! item%flux
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%flux)) deallocate (item%flux)
         allocate (character(len=N)::item%flux)
         read (unit=fileunit, IOSTAT=ios) item%flux
         if (ios .ne. 0) go to 300
      end if

      ! start the timer
      call system_clock(count=item%start_time, count_rate=item%crate, count_max=item%cmax)

      ! reset the progress
      if (item%naxis .eq. 2 .or. item%naxes(3) .eq. 1) then
         call set_progress(item, 0, 1)
      else
         call set_progress(item, 0, item%naxes(3))
      end if

      ! it is safe to set the header flag at this point
      if (allocated(item%hdr)) call set_header_status(item, .true.)

      ! item%dmin
      read (unit=fileunit, IOSTAT=ios) item%dmin
      if (ios .ne. 0) go to 300

      ! item%dmax
      read (unit=fileunit, IOSTAT=ios) item%dmax
      if (ios .ne. 0) go to 300

      ! item%dmedian
      read (unit=fileunit, IOSTAT=ios) item%dmedian
      if (ios .ne. 0) go to 300

      ! item%dmad
      read (unit=fileunit, IOSTAT=ios) item%dmad
      if (ios .ne. 0) go to 300

      ! item%dmadN
      read (unit=fileunit, IOSTAT=ios) item%dmadN
      if (ios .ne. 0) go to 300

      ! item%dmadP
      read (unit=fileunit, IOSTAT=ios) item%dmadP
      if (ios .ne. 0) go to 300

      ! item%frame_min
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%frame_min)) deallocate (item%frame_min)
         allocate (item%frame_min(N))
         read (unit=fileunit, IOSTAT=ios) item%frame_min(:)
         if (ios .ne. 0) go to 300
      end if

      ! item%frame_max
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%frame_max)) deallocate (item%frame_max)
         allocate (item%frame_max(N))
         read (unit=fileunit, IOSTAT=ios) item%frame_max(:)
         if (ios .ne. 0) go to 300
      end if

      ! item%frame_median
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%frame_median)) deallocate (item%frame_median)
         allocate (item%frame_median(N))
         read (unit=fileunit, IOSTAT=ios) item%frame_median(:)
         if (ios .ne. 0) go to 300
      end if

      dims = item%naxes(1:2)

      ! item%pixels
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if ((dims(1)*dims(2) .eq. N) .and. (N .gt. 0)) then
         if (allocated(item%pixels)) deallocate (item%pixels)
         allocate (item%pixels(dims(1), dims(2)))
         read (unit=fileunit, IOSTAT=ios) item%pixels(:, :)
         if (ios .ne. 0) go to 300
      end if

      ! item%mask
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if ((dims(1)*dims(2) .eq. N) .and. (N .gt. 0)) then
         if (allocated(item%mask)) deallocate (item%mask)
         allocate (item%mask(dims(1), dims(2)))
         read (unit=fileunit, IOSTAT=ios) item%mask(:, :)
         if (ios .ne. 0) go to 300
      end if

      ! item%is_optical
      read (unit=fileunit, IOSTAT=ios) item%is_optical
      if (ios .ne. 0) go to 300

      ! item%is_xray
      read (unit=fileunit, IOSTAT=ios) item%is_xray
      if (ios .ne. 0) go to 300

      ! item%video
      read (unit=fileunit, IOSTAT=ios) item%video
      if (ios .ne. 0) go to 300

      if (allocated(item%pixels) .and. allocated(item%mask)) call set_image_status(item, .true.)

      ! item%mean_spectrum
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%mean_spectrum)) deallocate (item%mean_spectrum)
         allocate (item%mean_spectrum(N))
         read (unit=fileunit, IOSTAT=ios) item%mean_spectrum(:)
         if (ios .ne. 0) go to 300
      end if

      ! item%integrated_spectrum
      read (unit=fileunit, IOSTAT=ios) N
      if (ios .ne. 0) go to 300

      if (N .gt. 0) then
         if (allocated(item%integrated_spectrum)) deallocate (item%integrated_spectrum)
         allocate (item%integrated_spectrum(N))
         read (unit=fileunit, IOSTAT=ios) item%integrated_spectrum(:)
         if (ios .ne. 0) go to 300
      end if

      bSuccess = .true.

      if (.not. c_associated(root)) then
         ! needs to be protected with a mutex
         rc = c_pthread_mutex_lock(logger_mtx)

         if (rc .eq. 0) then
            ! Intel ifort: forrtl: severe (32): invalid logical unit number, unit -129, file unknown !?
            call logger%info('load_dataset', 'restored pixels/mask from '//trim(file))

            ! unlock the mutex
            rc = c_pthread_mutex_unlock(logger_mtx)
         end if
      end if

300   close (fileunit)

   end subroutine load_dataset

   subroutine load_cube(item, cache, root, bSuccess)
      use fixed_array
      use omp_lib
      implicit none

      type(dataset), pointer, intent(inout) :: item
      character(len=*), intent(in) :: cache
      type(c_ptr), intent(in), value :: root
      logical, intent(out) ::  bSuccess

      ! character(len=:), allocatable :: file
      character(len=1024) :: file

      integer :: fileunit, ios
      character(256) :: iomsg

      integer :: tid, i, rc, depth

      ! OpenMP
      integer :: max_threads, counter, repeat
      logical thread_bSuccess

      integer(kind=4) :: n, m ! input dimensions

      ! compressed output dimensions
      integer(kind=4) :: cn, cm

      bSuccess = .false.

      ! reserved for *CUBES* only
      if (item%naxes(3) .le. 1) return

      n = item%naxes(1)
      m = item%naxes(2)
      depth = item%naxes(3)

      ! by default compressed is dimension(n/DIM, m/DIM)
      cn = n/DIM
      cm = m/DIM

      ! but the input dimensions might not be divisible by <DIM>
      if (mod(n, DIM) .ne. 0) cn = cn + 1
      if (mod(m, DIM) .ne. 0) cm = cm + 1

      if (allocated(item%compressed)) deallocate (item%compressed)
      allocate (item%compressed(1:depth))

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      print *, "max_threads:", max_threads, "depth:", depth

      counter = 0
      thread_bSuccess = .true.

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item)&
      !$omp& PRIVATE(tid, i, file, fileunit, ios, iomsg)&
      !$omp& REDUCTION(.and.:thread_bSuccess)&
      !$omp& REDUCTION(+:counter)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO SCHEDULE(DYNAMIC, 4)
      do i = 1, depth
         nullify (item%compressed(i)%ptr)

         ! if (allocated(file)) deallocate (file)
         file = cache//'/'//trim(str(i))//'.bin'

         ! try to open the file for reading
         open (newunit=fileunit, file=trim(file), status='old', action='read', access='stream', form='unformatted',&
         & IOSTAT=ios, IOMSG=iomsg)

         ! move on if the file does not exist
         if (ios .ne. 0) then
            ! print *, "error opening a file ", trim(file), ' : ', trim(iomsg)
            cycle
         end if

         ! allocate space for compressed data
         allocate (item%compressed(i)%ptr(cn, cm))

         ! read the compressed data
         read (unit=fileunit, IOSTAT=ios, IOMSG=iomsg) item%compressed(i)%ptr(:, :)

         ! and close the file
         close (fileunit)

         ! abort upon a read error
         if (ios .ne. 0) then
            print *, "error deserialising channel", i, 'from a binary file ', trim(file), ' : ', trim(iomsg)
            thread_bSuccess = .false.
            cycle
         end if

         ! increment the thread progress counter
         counter = counter + 1

         if (.not. c_associated(root)) then
            call update_progress(item, 1)

            ! decrement the thread progress counter
            counter = counter - 1
         else
            ! upon success the thread progress counter will be decremented
            ! tid = 1 + OMP_GET_THREAD_NUM()

            ! if (mod(counter, tid) .eq. 0) then
            if (mod(counter, 4) .eq. 0) then
               ! a C function defined in http.c
               counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)
            end if
         end if
      end do
      !$omp END DO
      !$omp END PARALLEL

      ! submit any left-overs
      repeat = 0
      do while (counter .gt. 0)

         counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)

         ! wait a while upon a submission failure
         if (counter .gt. 0) then
            repeat = repeat + 1
            print *, item%datasetid, "::'submit_progress' failed, counter = ", counter, ", #repeats:", repeat
            call sleep(1) ! 1 sec.
         end if

         ! break the loop after 60s
         if (repeat .gt. 60) then
            print *, item%datasetid, ":: breaking the final 'submit_progress' loop, counter = ", counter
            exit
         end if

      end do

      bSuccess = thread_bSuccess

      if (.not. c_associated(root) .and. bSuccess) then
         ! needs to be protected with a mutex
         rc = c_pthread_mutex_lock(logger_mtx)

         if (rc .eq. 0) then
            ! Intel ifort: forrtl: severe (32): invalid logical unit number, unit -129, file unknown !?
            call logger%info('load_cube', 'restored cube data from '//cache)

            ! unlock the mutex
            rc = c_pthread_mutex_unlock(logger_mtx)
         end if
      end if

      return
   end subroutine load_cube

   subroutine load_cube_monolithic(item, cache, root, bSuccess)
      use :: iso_fortran_env, only:FILE_STORAGE_SIZE
      use fixed_array
      use omp_lib
      implicit none

      type(dataset), pointer, intent(inout) :: item
      character(len=*), intent(in) :: cache
      type(c_ptr), intent(in), value :: root
      logical, intent(out) ::  bSuccess

      ! character(len=:), allocatable :: file
      character(len=1024) :: file

      integer :: fileunit, ios
      integer :: index_unit, data_unit
      character(256) :: iomsg

      integer :: tid, i, rc, depth, frame
      integer, allocatable :: indices(:)

      ! OpenMP
      integer :: max_threads, counter, repeat
      logical thread_bSuccess

      integer(kind=4) :: n, m ! input dimensions

      ! compressed output dimensions
      integer(kind=4) :: cn, cm

      bSuccess = .false.

      ! reserved for *CUBES* only
      if (item%naxes(3) .le. 1) return

      n = item%naxes(1)
      m = item%naxes(2)
      depth = item%naxes(3)

      ! by default compressed is dimension(n/DIM, m/DIM)
      cn = n/DIM
      cm = m/DIM

      ! but the input dimensions might not be divisible by <DIM>
      if (mod(n, DIM) .ne. 0) cn = cn + 1
      if (mod(m, DIM) .ne. 0) cm = cm + 1

      if (allocated(item%compressed)) deallocate (item%compressed)
      allocate (item%compressed(1:depth))

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      print *, "max_threads:", max_threads, "depth:", depth

      do concurrent(i=1:depth)
         nullify (item%compressed(i)%ptr)
      end do

      counter = 0
      index_unit = -1

      ! open an index file
      file = cache//'/index'

      ! read the file size
      INQUIRE (FILE=file, SIZE=n)  ! return -1 if cannot determine file size_t
      if (n .eq. -1) goto 7000

      n = n*(FILE_STORAGE_SIZE/8)/sizeof(frame)
      allocate (indices(n))

      ! try to open the file for reading
      open (newunit=index_unit, file=trim(file), status='old', action='read', access='stream', form='unformatted',&
      & IOSTAT=ios, IOMSG=iomsg)

      ! move on if the file does not exist
      if (ios .ne. 0) then
         ! print *, "error opening an index file ", trim(file), ' : ', trim(iomsg)
         goto 7000
      end if

      read (unit=index_unit, IOSTAT=ios) indices(1:n)
      if (ios .ne. 0) then
         print *, "error reading from the index file ", trim(file), ' : ', trim(iomsg)
         goto 7000
      end if

      print *, "[load_cube_monolithic] #frames:", n ! , "indices:", indices

      data_unit = -1

      ! open a data file
      file = cache//'/data'

      ! try to open the file for reading
      open (newunit=data_unit, file=trim(file), status='old', action='read', access='stream', form='unformatted',&
      & IOSTAT=ios, IOMSG=iomsg)

      ! move on if the file does not exist
      if (ios .ne. 0) then
         print *, "error opening a data file ", trim(file), ", tid:", i, ' : ', trim(iomsg)
         goto 7000
      end if

      do i = 1, n
         ! tid = 1 + OMP_GET_THREAD_NUM()
         frame = indices(i)

         ! allocate space for compressed data
         allocate (item%compressed(frame)%ptr(cn, cm))

         ! read the compressed data
         read (unit=data_unit, IOSTAT=ios, IOMSG=iomsg) item%compressed(frame)%ptr(:, :)

         ! abort upon a read error
         if (ios .ne. 0) then
            print *, "error deserialising channel", frame, 'from a data file ', trim(file), ' : ', trim(iomsg)

            deallocate (item%compressed(frame)%ptr)
            nullify (item%compressed(frame)%ptr)

            bSuccess = .false.
            exit
         end if

         ! increment the thread progress counter
         counter = counter + 1

         if (.not. c_associated(root)) then
            call update_progress(item, 1)

            ! decrement the thread progress counter
            counter = counter - 1
         else
            ! upon success the thread progress counter will be decremented
            if (mod(counter, 4) .eq. 0) then
               ! a C function defined in http.c
               counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)
            end if
         end if

      end do

      ! submit any left-overs
7000  repeat = 0
      do while (counter .gt. 0)

         counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)

         ! wait a while upon a submission failure
         if (counter .gt. 0) then
            repeat = repeat + 1
            print *, item%datasetid, "::'submit_progress' failed, counter = ", counter, ", #repeats:", repeat
            call sleep(1) ! 1 sec.
         end if

         ! break the loop after 60s
         if (repeat .gt. 60) then
            print *, item%datasetid, ":: breaking the final 'submit_progress' loop, counter = ", counter
            exit
         end if

      end do

      if (index_unit .ne. -1) close (index_unit)
      if (data_unit .ne. -1) close (data_unit)

      if (.not. c_associated(root) .and. bSuccess) then
         ! needs to be protected with a mutex
         rc = c_pthread_mutex_lock(logger_mtx)

         if (rc .eq. 0) then
            ! Intel ifort: forrtl: severe (32): invalid logical unit number, unit -129, file unknown !?
            call logger%info('load_cube', 'restored cube data from '//cache)

            ! unlock the mutex
            rc = c_pthread_mutex_unlock(logger_mtx)
         end if
      end if

      return
   end subroutine load_cube_monolithic

   subroutine load_cube_mp(item, cache, root, bSuccess)
      use :: iso_fortran_env, only:FILE_STORAGE_SIZE
      use fixed_array
      use omp_lib
      implicit none

      type(dataset), pointer, intent(inout) :: item
      character(len=*), intent(in) :: cache
      type(c_ptr), intent(in), value :: root
      logical, intent(out) ::  bSuccess

      ! character(len=:), allocatable :: file
      character(len=1024) :: file

      integer :: fileunit, ios
      integer :: index_unit
      integer(kind=c_int) :: data_unit
      character(256) :: iomsg

      integer :: tid, i, rc, depth, frame
      integer, allocatable :: indices(:)

      ! OpenMP
      integer :: max_threads, counter, repeat
      logical thread_bSuccess

      integer(kind=4) :: n, m ! input dimensions

      ! compressed output dimensions
      integer(kind=4) :: cn, cm
      integer(kind=c_size_t) :: array_size

      bSuccess = .false.

      ! reserved for *CUBES* only
      if (item%naxes(3) .le. 1) return

      n = item%naxes(1)
      m = item%naxes(2)
      depth = item%naxes(3)

      ! by default compressed is dimension(n/DIM, m/DIM)
      cn = n/DIM
      cm = m/DIM

      ! but the input dimensions might not be divisible by <DIM>
      if (mod(n, DIM) .ne. 0) cn = cn + 1
      if (mod(m, DIM) .ne. 0) cm = cm + 1

      if (allocated(item%compressed)) deallocate (item%compressed)
      allocate (item%compressed(1:depth))

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      print *, "max_threads:", max_threads, "depth:", depth

      do concurrent(i=1:depth)
         nullify (item%compressed(i)%ptr)
      end do

      counter = 0
      index_unit = -1

      ! open an index file
      file = cache//'/index'

      ! read the file size
      INQUIRE (FILE=file, SIZE=n)  ! return -1 if cannot determine file size_t
      if (n .eq. -1) goto 7000

      n = n*(FILE_STORAGE_SIZE/8)/sizeof(frame)
      allocate (indices(n))

      ! try to open the file for reading
      open (newunit=index_unit, file=trim(file), status='old', action='read', access='stream', form='unformatted',&
      & IOSTAT=ios, IOMSG=iomsg)

      ! move on if the file does not exist
      if (ios .ne. 0) then
         ! print *, "error opening an index file ", trim(file), ' : ', trim(iomsg)
         goto 7000
      end if

      read (unit=index_unit, IOSTAT=ios) indices(1:n)
      if (ios .ne. 0) then
         print *, "error reading from the index file ", trim(file), ' : ', trim(iomsg)
         goto 7000
      end if

      print *, "[load_cube_mp] #frames:", n ! , "indices:", indices

      data_unit = -1

      ! open a data file
      file = cache//'/data'

      ! try to open the file for reading
      data_unit = rdopen(trim(file)//c_null_char)

      ! move on if the file does not exist
      if (data_unit .lt. 0) then
         print *, "error opening a data file ", trim(file), ", tid:", i
         goto 7000
      end if

      thread_bSuccess = .true.

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item, indices)&
      !$omp& PRIVATE(tid, i, file, frame, ios, array_size)&
      !$omp& REDUCTION(.and.:thread_bSuccess)&
      !$omp& REDUCTION(+:counter)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO SCHEDULE(DYNAMIC, 4)
      do i = 1, n
         ! tid = 1 + OMP_GET_THREAD_NUM()
         frame = indices(i)

         ! allocate space for compressed data
         allocate (item%compressed(frame)%ptr(cn, cm))

         array_size = int(sizeof(item%compressed(frame)%ptr(:, :)), kind=c_size_t)
         ! print *, 'sizeof(compressed_frame) = ', array_size, 'bytes'

         ! read the compressed data
         ios = read_frame(data_unit, c_loc(item%compressed(frame)%ptr(:, :)), i - 1, array_size)

         ! abort upon a read error
         if (ios .ne. 0) then
            print *, "error deserialising channel", frame, 'from a data file ', trim(file), ' : ', trim(iomsg)

            deallocate (item%compressed(frame)%ptr)
            nullify (item%compressed(frame)%ptr)

            thread_bSuccess = .false.
            cycle
         end if

         ! increment the thread progress counter
         counter = counter + 1

         if (.not. c_associated(root)) then
            call update_progress(item, 1)

            ! decrement the thread progress counter
            counter = counter - 1
         else
            ! upon success the thread progress counter will be decremented
            if (mod(counter, 32) .eq. 0) then
               ! a C function defined in http.c
               counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)
            end if
         end if

      end do
      !$omp END DO
      !$omp END PARALLEL

      bSuccess = thread_bSuccess

      ! submit any left-overs
7000  repeat = 0
      do while (counter .gt. 0)

         counter = counter - submit_progress(root, item%datasetid, size(item%datasetid), counter)

         ! wait a while upon a submission failure
         if (counter .gt. 0) then
            repeat = repeat + 1
            print *, item%datasetid, "::'submit_progress' failed, counter = ", counter, ", #repeats:", repeat
            call sleep(1) ! 1 sec.
         end if

         ! break the loop after 60s
         if (repeat .gt. 60) then
            print *, item%datasetid, ":: breaking the final 'submit_progress' loop, counter = ", counter
            exit
         end if

      end do

      if (index_unit .ne. -1) close (index_unit)
      if (data_unit .ge. 0) call closefd(data_unit)

      if (.not. c_associated(root) .and. bSuccess) then
         ! needs to be protected with a mutex
         rc = c_pthread_mutex_lock(logger_mtx)

         if (rc .eq. 0) then
            ! Intel ifort: forrtl: severe (32): invalid logical unit number, unit -129, file unknown !?
            call logger%info('load_cube_mp', 'restored cube data from '//cache)

            ! unlock the mutex
            rc = c_pthread_mutex_unlock(logger_mtx)
         end if
      end if

      return
   end subroutine load_cube_mp

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
      print *, 'dmin:', item%dmin, 'dmax:', item%dmax, 'dmedian:', item%dmedian
      print *, 'dmad:', item%dmad, 'dmadP:', item%dmadP, 'dmadN:', item%dmadN, 'video:', item%video

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

   subroutine set_video_status(item, video)
      type(dataset), pointer, intent(inout) :: item
      logical, intent(in) :: video

      integer :: rc

      ! lock the mutex
      rc = c_pthread_mutex_lock(item%video_mtx)

      item%video = video

      ! unlock the mutex
      rc = c_pthread_mutex_unlock(item%video_mtx)

   end subroutine set_video_status

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
      type(c_pthread_t) :: pid

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

         if ((total .gt. 1) .and. (.not. item%video)) then
            if (allocated(item%frame_min)) item%dmin = minval(item%frame_min)
            if (allocated(item%frame_max)) item%dmax = maxval(item%frame_max)
            if (allocated(item%frame_median)) item%dmedian = &
            &median(pack(item%frame_median,.not. isnan(item%frame_median))) ! extract non-NaN values

            ! launch a pthread, passing the FORTRAN <item> dataset via a C pointer
            rc = c_pthread_create(thread=pid, &
                                  attr=c_null_ptr, &
                                  start_routine=c_funloc(global_statistics), &
                                  arg=c_loc(item))

            ! detach a thread
            if (rc .eq. 0) rc = c_pthread_detach(pid)
         else
            call print_dataset(item)
         end if

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

   integer(c_int) function get_image_status(ptr) BIND(C, name='get_image_status')
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

   ! void submit_channel_range(void *ptr, int idx, int progress, float *frame_min, float *frame_max, float *frame_median, float *mean_spectrum, float *integrated_spectrum);
   subroutine submit_channel_range(ptr, idx, N, frame_min, frame_max, frame_median,&
   &mean_spectrum, integrated_spectrum) BIND(C, name='submit_channel_range')
      type(C_PTR), intent(in), value :: ptr
      integer(c_int), intent(in), value :: idx, N
      real(kind=c_float), dimension(*), intent(in) :: frame_min, frame_max, frame_median
      real(kind=c_float), dimension(*), intent(in) :: mean_spectrum, integrated_spectrum

      type(dataset), pointer :: item

      if (idx .lt. 1) return

      call c_f_pointer(ptr, item)

      ! no need for a mutex as no other thread will be accessing this array range (unless a dataset is being deleted ...)
      item%frame_min(idx:idx + N - 1) = frame_min(1:N)
      item%frame_max(idx:idx + N - 1) = frame_max(1:N)
      item%frame_median(idx:idx + N - 1) = frame_median(1:N)
      item%mean_spectrum(idx:idx + N - 1) = mean_spectrum(1:N)
      item%integrated_spectrum(idx:idx + N - 1) = integrated_spectrum(1:N)

   end subroutine submit_channel_range

   subroutine get_channel_range_C(ptr, progress, startindex, endindex, status) BIND(C, name='get_channel_range_C')
      type(C_PTR), intent(in), value :: ptr
      integer(c_int), intent(in), value :: progress
      integer(c_int), intent(out) :: status, startindex, endindex

      type(dataset), pointer :: item

      call c_f_pointer(ptr, item)

      if (progress .gt. 0) call update_progress(item, progress)

      call get_channel_range(item, startindex, endindex, status)

   end subroutine get_channel_range_C

   subroutine update_progress_C(ptr, progress) BIND(C, name='update_progress_C')
      type(C_PTR), intent(in), value :: ptr
      integer(c_int), intent(in), value :: progress

      type(dataset), pointer :: item

      call c_f_pointer(ptr, item)

      if (progress .gt. 0) call update_progress(item, progress)

   end subroutine update_progress_C

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

   subroutine load_fits_file(datasetid, datasetid_len, filepath, filepath_len, flux, flux_len, root, dir, dir_len) bind(C)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(kind=c_size_t), intent(in), value :: datasetid_len, filepath_len, flux_len
      character(kind=c_char), dimension(datasetid_len), intent(in) :: datasetid
      character(kind=c_char), dimension(filepath_len), intent(in) :: filepath
      character(kind=c_char), dimension(flux_len), intent(in) :: flux

      ! the pointer will be passed back to C when requesting FITS file channel ranges
      ! from the root node and submitting results to the cluster root
      type(c_ptr), intent(in), value :: root

      integer(kind=c_size_t), intent(in), value :: dir_len
      character(kind=c_char), dimension(dir_len), intent(in) :: dir

      character(len=filepath_len) :: strFilename
      character(len=flux_len) :: strFlux

      character(len=:), allocatable :: cache

      integer :: i, rc
      logical :: bSuccess

      type(dataset), pointer :: item

      integer(8) :: start, finish, crate, cmax
      real :: elapsed

      print *, "[load_fits_file] datasetid: '", datasetid, "', flux: '", flux, "', filepath: '", filepath, "'"

      if (.not. c_associated(root)) then
         print *, "[load_fits_file] :: ROOT NODE"
      else
         print *, "[load_fits_file] :: CLIENT NODE"
      end if

      do i = 1, int(filepath_len, kind=4)
         strFilename(i:i) = filepath(i)
      end do

      do i = 1, int(flux_len, kind=4)
         strFlux(i:i) = flux(i)
      end do

      allocate (character(dir_len + 1 + datasetid_len)::cache)

      ! the cache directory
      do i = 1, int(dir_len, kind=4)
         cache(i:i) = dir(i)
      end do

      ! append a slash
      cache(dir_len + 1:dir_len + 1) = '/'

      ! and append the datasetid
      do i = 1, int(datasetid_len, kind=4)
         cache(dir_len + 1 + i:dir_len + 1 + i) = datasetid(i)
      end do

      allocate (item)

      ! init mutexes
      rc = c_pthread_mutex_init(item%header_mtx, c_null_ptr)
      rc = c_pthread_mutex_init(item%error_mtx, c_null_ptr)
      rc = c_pthread_mutex_init(item%ok_mtx, c_null_ptr)
      rc = c_pthread_mutex_init(item%progress_mtx, c_null_ptr)
      rc = c_pthread_mutex_init(item%image_mtx, c_null_ptr)
      rc = c_pthread_mutex_init(item%video_mtx, c_null_ptr)

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

      ! first try a cache file
      call load_dataset(item, cache, root, bSuccess)

      ! if the cache file cannot be found / read, use the underlying FITS file
      if (.not. bSuccess) then
         call read_fits_file(item, cache, strFilename, strFlux, root, bSuccess)
      else
         if (item%naxis .eq. 2 .or. item%naxes(3) .eq. 1) then
            call update_progress(item, 1)
         else
            ! if it's a 3D cube restore the channel information too
            call load_cube_mp(item, cache, root, bSuccess)
         end if
      end if

      ! end the timer
      call system_clock(finish)
      elapsed = real(finish - start)/real(crate)

      print *, "finished loading ", item%datasetid, ", bSuccess: ", bSuccess, ", elapsed time: ", elapsed, " [s]"

      ! reset the timeout clock
      call reset_clock(item)

   end subroutine load_fits_file

   subroutine read_fits_file(item, cache, filename, flux, root, bSuccess)
      use omp_lib
      implicit none

      type(dataset), pointer, intent(inout) :: item
      character(len=*), intent(in) :: cache
      character(len=*), intent(in) :: filename, flux
      ! the pointer will be passed back to C when requesting FITS file ranges
      ! from the root node and submitting results to the cluster root
      type(c_ptr), intent(in) :: root
      logical, intent(out) ::  bSuccess

      integer status, group, unit, readwrite, blocksize, nkeys, nspace, hdutype, i, j
      integer naxis, bitpix
      integer npixels, cn, cm
      integer naxes(4)
      integer(kind=8) firstpix, lastpix
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

      ! thread-local variables
      real(kind=c_float), allocatable, target :: thread_buffer(:)
      real(kind=c_float), allocatable, target :: thread_pixels(:)
      logical(kind=c_bool), allocatable, target :: thread_mask(:)
      real(kind=c_float), allocatable :: thread_arr(:, :)
      real(kind=c_float), allocatable :: thread_data(:)
      logical(kind=c_bool), allocatable, target :: data_mask(:)
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

      integer :: rc

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
      if (allocated(item%flux)) deallocate (item%flux)
      if (flux(1:1) .ne. 'N') allocate (item%flux, source=flux)

      item%cunit1 = ''
      item%cunit2 = ''
      item%cunit3 = ''

      item%ctype1 = ''
      item%ctype2 = ''
      item%ctype3 = ''

      item%dmin = ieee_value(0.0, ieee_quiet_nan)
      item%dmax = ieee_value(0.0, ieee_quiet_nan)
      item%dmedian = ieee_value(0.0, ieee_quiet_nan)

      item%dmad = ieee_value(0.0, ieee_quiet_nan)
      item%dmadN = ieee_value(0.0, ieee_quiet_nan)
      item%dmadP = ieee_value(0.0, ieee_quiet_nan)

      ! reset the FITS header
      if (allocated(item%hdr)) deallocate (item%hdr)

      ! The STATUS parameter must always be initialized.
      status = 0

      rc = c_pthread_mutex_lock(file_unit_mtx)

      ! Get an unused Logical Unit Number to use to open the FITS file.
      call ftgiou(unit, status)

      if (status .ne. 0) then
         rc = c_pthread_mutex_unlock(file_unit_mtx)
         return
      end if

      item%unit = unit

      ! open the FITS file, with read - only access.The returned BLOCKSIZE
      ! parameter is obsolete and should be ignored.
      readwrite = 0
      call ftopen(unit, filename, readwrite, blocksize, status)

      rc = c_pthread_mutex_unlock(file_unit_mtx)

      if (status .ne. 0) then
         return
      end if

      j = 0
100   continue
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
110         block
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

            if ((.not. isnan(tmp)) .and. (tmp .ge. item%datamin) .and. (tmp .le. item%datamax)) then
               if (test_ignrval) then
                  if (abs(tmp - item%ignrval) .le. epsilon(tmp)) then
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

                     rc = c_pthread_mutex_lock(file_unit_mtx)

                     ! Get an unused Logical Unit Number to use to open the FITS file.
                     call ftgiou(unit, status)

                     if (status .ne. 0) then
                        rc = c_pthread_mutex_unlock(file_unit_mtx)
                        cycle
                     end if

                     ! open the FITS file, with read - only access.The returned BLOCKSIZE
                     ! parameter is obsolete and should be ignored.
                     readwrite = 0
                     call ftopen(unit, filename, readwrite, blocksize, status)

                     rc = c_pthread_mutex_unlock(file_unit_mtx)

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

         if (allocated(item%compressed)) deallocate (item%compressed)
         allocate (item%compressed(start:end))

         do i = start, end
            nullify (item%compressed(i)%ptr)
         end do

         if (allocated(item%frame_min)) deallocate (item%frame_min)
         allocate (item%frame_min(start:end))

         if (allocated(item%frame_max)) deallocate (item%frame_max)
         allocate (item%frame_max(start:end))

         if (allocated(item%frame_median)) deallocate (item%frame_median)
         allocate (item%frame_median(start:end))

         ! spectra
         if (allocated(item%mean_spectrum)) deallocate (item%mean_spectrum)
         allocate (item%mean_spectrum(naxes(3)))

         if (allocated(item%integrated_spectrum)) deallocate (item%integrated_spectrum)
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
         item%frame_median = ieee_value(0.0, ieee_quiet_nan)

         !$omp PARALLEL DEFAULT(SHARED) SHARED(item)&
         !$omp& SHARED(thread_units, group, naxis, naxes, nullval)&
         !$omp& PRIVATE(tid, start, end, num_per_node, anynull, status)&
         !$omp& PRIVATE(j, fpixels, lpixels, incs, tmp, frame_min, frame_max, frame_median)&
         !$omp& PRIVATE(mean_spec_val, int_spec_val, pixel_sum, pixel_count)&
         !$omp& PRIVATE(thread_buffer, thread_pixels, thread_mask, thread_arr)&
         !$omp& PRIVATE(thread_data, data_mask, data_count, res)&
         !$omp& REDUCTION(.or.:thread_bSuccess)&
         !$omp& REDUCTION(max:dmax)&
         !$omp& REDUCTION(min:dmin)&
         !$omp& NUM_THREADS(max_threads)
         tid = 1 + OMP_GET_THREAD_NUM()

         ! allocate thread buffers
         allocate (thread_buffer(npixels))
         allocate (thread_arr(item%naxes(1), item%naxes(2)))
         allocate (thread_data(npixels))
         allocate (data_mask(npixels))

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
               ! a C function defined in http.c
               call fetch_channel_range(root, item%datasetid, size(item%datasetid), start, end, status,&
               &c_loc(item%frame_min), c_loc(item%frame_max), c_loc(item%frame_median),&
               &c_loc(item%mean_spectrum), c_loc(item%integrated_spectrum))
            end if

            ! LOOP EXIT
            ! -2 : a catastrophic error
            ! -1 : end of FITS file (no more work to do)

            ! LOOP CONTINUE
            ! 0 : OK
            ! 1 : accepted, header not ready yet
            if (status .lt. 0) then
               ! print *, "status = ", status
               exit ! one comparison handles it all, neat!
            end if

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

                  res = (/frame_min, frame_max, 0.0, 0.0/)

                  ! the 'infamous' AVX-512 slowdown on the Mac Pro ... a shame ...
                  call make_image_spectrumF32(c_loc(thread_buffer), c_loc(thread_pixels), c_loc(thread_mask), &
                  &c_loc(data_mask), item%ignrval, item%datamin, item%datamax, cdelt3, c_loc(res), npixels)

                  frame_min = res(1)
                  frame_max = res(2)
                  mean_spec_val = res(3)
                  int_spec_val = res(4)

                  ! disable FORTRAN, use the Intel SPMD C
                  if (.false.) then
                     ! calculate the min/max values
                     do j = 1, npixels

                        tmp = thread_buffer(j)

                        if ((.not. isnan(tmp)) .and. (tmp .ge. item%datamin) .and. (tmp .le. item%datamax)) then
                           if (test_ignrval) then
                              if (abs(tmp - item%ignrval) .le. epsilon(tmp)) then
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
                           thread_pixels(j) = thread_pixels(j) + tmp*cdelt3
                           thread_mask(j) = thread_mask(j) .or. .true.

                           ! needed by the mean and integrated spectra
                           pixel_sum = pixel_sum + tmp
                           pixel_count = pixel_count + 1
                        else
                           ! thread_mask(j, tid) = thread_mask(j, tid) .or. .false.
                        end if

                     end do
                  end if

                  if (pixel_count .gt. 0) then
                     mean_spec_val = pixel_sum/real(pixel_count)
                     int_spec_val = pixel_sum*cdelt3
                  end if

                  item%frame_min(frame) = frame_min
                  item%frame_max(frame) = frame_max
                  ! item%frame_median(frame) = median(pack(thread_buffer, data_mask))
                  item%frame_median(frame) = hist_median(pack(thread_buffer, data_mask), frame_min, frame_max)

                  ! if (data_count .gt. 0) then
                  !    ! item%frame_median(frame) = median(thread_data(1:data_count))
                  !   item%frame_median(frame) = hist_median(thread_data(1:data_count), frame_min, frame_max)
                  ! else
                  !    item%frame_median(frame) = ieee_value(0.0, ieee_quiet_nan)
                  ! end if

                  dmin = min(dmin, frame_min)
                  dmax = max(dmax, frame_max)

                  item%mean_spectrum(frame) = mean_spec_val
                  item%integrated_spectrum(frame) = int_spec_val

                  ! compress the pixels
                  if (allocated(item%compressed) .and. allocated(thread_arr)) then
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
                        item%compressed(frame)%ptr => to_fixed(thread_arr(:, :),&
                        & frame_min, frame_max, ignrval, datamin, datamax)

                        ! for disk load balancing (not just CPU), try to serialise a frame whilst reading FITS
                        ! if (associated(item%compressed(frame)%ptr)) then
                        !    call serialise_fixed_array(item%compressed(frame)%ptr, frame, cache)
                        ! end if
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
         if (allocated(thread_buffer)) deallocate (thread_buffer)
         if (allocated(thread_pixels)) deallocate (thread_pixels)
         if (allocated(thread_mask)) deallocate (thread_mask)
         if (allocated(thread_arr)) deallocate (thread_arr)
         if (allocated(thread_data)) deallocate (thread_data)
         if (allocated(data_mask)) deallocate (data_mask)

         !$omp END PARALLEL

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
200   call ftclos(unit, status)
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

   subroutine get_spectrum_range_C(ptr, frame_start, frame_end, ref_freq, first, last) BIND(C, name='get_spectrum_range_C')
      type(C_PTR), intent(in), value :: ptr
      real(kind=c_double), intent(in), value :: frame_start, frame_end, ref_freq
      integer(kind=c_int), intent(out) :: first, last
      type(dataset), pointer :: item

      call c_f_pointer(ptr, item)

      call get_spectrum_range(item, frame_start, frame_end, ref_freq, first, last)

   end subroutine get_spectrum_range_C

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

      if ((abs(freq_start) .le. epsilon(freq_start)) .or. (abs(freq_end) .le. epsilon(freq_end))) then
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
      real(kind=c_float), dimension(width, height), intent(in) :: pixels
      logical(kind=c_bool), dimension(width, height), intent(in) :: mask
      integer, allocatable, intent(out) :: hist(:)
      type(image_tone_mapping), intent(out) :: tone

      real, dimension(:), allocatable :: data
      real pmin, pmax, pmedian
      real mad, madP, madN
      integer countP, countN
      real pixel
      integer i, j, n
      real u, v
      real black, white, sensitivity, ratio_sensitivity

      tone%pmin = 0.0
      tone%pmax = 0.0
      tone%pmedian = 0.0
      tone%black = 0.0
      tone%white = 0.0
      tone%sensitivity = 0.0
      tone%ratio_sensitivity = 0.0

      if (item%naxis .eq. 2 .or. item%naxes(3) .eq. 1) then
         pmin = item%dmin
         pmax = item%dmax
      else
         pmin = 1.0E30
         pmax = -1.0E30

         do j = 1, height
            do i = 1, width
               if (mask(i, j)) then
                  pixel = pixels(i, j)

                  pmin = min(pmin, pixel)
                  pmax = max(pmax, pixel)
               end if
            end do
         end do
      end if

      ! pick non-NaN valid pixels only according to mask
      data = pack(pixels, mask)

      n = size(data)

      if (n .eq. 0) return

      ! make a histogram with a range given by [pmin, pmax]
      call make_histogram(hist, data, pmin, pmax)

      pmedian = hist_median(data, pmin, pmax, 2) ! a two-pass median estimation
      print *, 'hist. median = ', pmedian

      ! pmedian = median(data)
      ! print *, '50th quantile (median) = ', pmedian

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

            Slot = real(cdf)/real(total)

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

      if (.not. allocated(hist)) allocate (hist(NBINS))

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

   ! a recursive multi-pass histogram-based median
   recursive function rec_hist_median(X, DMIN, DMAX, HIST, NPASS) result(median)
      implicit none

      real, dimension(:), intent(in), target :: X
      real, intent(in) :: DMIN, DMAX
      integer, allocatable, intent(inout) :: hist(:)
      integer :: NPASS

      integer :: i, N

      ! statistics
      integer :: cumulative, previous_cumulative
      real :: median, bin_start, bin_end, bin_width

      N = size(X)

      call make_histogram(HIST, X, DMIN, DMAX)

      ! find the bin with the median
      previous_cumulative = 0
      cumulative = 0

      do i = 1, N
         if (cumulative .ge. N/2) exit ! we've got the bin with the median

         previous_cumulative = cumulative
         cumulative = cumulative + HIST(i)
      end do

      i = i - 1

      bin_start = DMIN + (i - 1)*(DMAX - DMIN)/NBINS
      bin_end = DMIN + i*(DMAX - DMIN)/NBINS
      bin_width = (DMAX - DMIN)/NBINS

      if (NPASS .eq. 1) then
         median = bin_start + bin_width*(N/2 - previous_cumulative)/HIST(i)
      else
         median = rec_hist_median(X, bin_start, bin_end, HIST, NPASS - 1)
      end if

   end function rec_hist_median

   ! histogram-based median estimation
   function hist_median(X, DMIN, DMAX, PASSES) result(median)
      implicit none

      real, dimension(:), intent(in), target :: X
      real, intent(in) :: DMIN, DMAX
      integer, intent(in), optional :: PASSES
      integer :: N

      integer, allocatable :: hist(:)
      integer :: PCOUNT
      real :: median

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real :: elapsed

      N = size(X)

      if (N .lt. 1) then
         median = ieee_value(0.0, ieee_quiet_nan)
         return
      end if

      if (present(PASSES)) then
         PCOUNT = PASSES
      else
         PCOUNT = 1
      end if

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      median = rec_hist_median(X, DMIN, DMAX, hist, PCOUNT)

      ! end the timer
      call system_clock(finish_t)
      elapsed = real(finish_t - start_t)/real(crate)

      ! print *, 'histogram elapsed time:', 1000*elapsed, ' [ms]', '; median:', median

   end function hist_median

   ! --------------------------------------------------------------------
   ! REAL FUNCTION  median() :
   !    This function receives an array X of N entries, sorts it
   !    and computes the median.
   !    The returned value is of REAL type.
   ! --------------------------------------------------------------------

   real function median(X)
      use quantile_mod
      implicit none

      real, dimension(:), intent(in), target :: X
      integer :: N

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real :: elapsed

      N = size(X)

      if (N .lt. 1) then
         median = ieee_value(0.0, ieee_quiet_nan)
         return
      end if

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      median = quantile(N/2, X)

      ! end the timer
      call system_clock(finish_t)
      elapsed = real(finish_t - start_t)/real(crate)

      ! print *, 'quantile elapsed time:', 1000*elapsed, ' [ms]', '; median:', median

   end function median

   subroutine inherent_image_dimensions_C(ptr, width, height) BIND(C, name='inherent_image_dimensions_C')
      type(C_PTR), intent(in), value :: ptr
      integer(c_int), intent(out) :: width, height
      type(dataset), pointer :: item

      width = 0
      height = 0

      if (get_image_status(ptr) .eq. 0) return

      call c_f_pointer(ptr, item)

      call inherent_image_dimensions(item, width, height)

      return
   end subroutine inherent_image_dimensions_C

   subroutine inherent_image_dimensions(item, width, height)
      type(dataset), pointer, intent(in) :: item
      integer, intent(out) :: width, height

      if (.not. allocated(item%mask)) then
         width = 0
         height = 0
         return
      end if

      call inherent_image_dimensions_from_mask(item%mask, width, height)

   end subroutine inherent_image_dimensions

   subroutine inherent_image_dimensions_from_mask(mask, width, height)
      logical(kind=c_bool), dimension(:, :), intent(in) :: mask
      integer, intent(out) :: width, height

      integer x1, x2, y1, y2, k
      integer, dimension(2) :: dims

      ! get the dimensions from the mask
      dims = shape(mask)
      width = dims(1)
      height = dims(2)

      x1 = 1; x2 = width
      y1 = 1; y2 = height

      ! go through the 2D image mask item%mask
      ! truncating the NaN values along the X & Y axes

      ! x1
      do k = 1, width
         x1 = k

         if (any(mask(k, :))) exit
      end do

      ! x2
      do k = width, 1, -1
         x2 = k

         if (any(mask(k, :))) exit
      end do

      ! y1
      do k = 1, height
         y1 = k

         if (any(mask(:, k))) exit
      end do

      ! y2
      do k = height, 1, -1
         y2 = k

         if (any(mask(:, k))) exit
      end do

      print *, 'original dimensions:', width, height

      ! do not return negative dimensions if the entire mask is .false.
      width = max(x2 - x1 + 1, 0)
      height = max(y2 - y1 + 1, 0)

      print *, 'inherent dimensions:', width, height

   end subroutine inherent_image_dimensions_from_mask

   subroutine calculate_global_statistics_C(ptr, dmedian, sumP, countP, sumN, countN, first, last)&
      & BIND(C, name='calculate_global_statistics_C')
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: ptr
      real(c_float), intent(in), value :: dmedian
      real(c_float), intent(out) :: sumP, sumN
      integer(c_int64_t), intent(out) :: countP, countN
      integer(c_int), intent(in), value :: first, last

      type(dataset), pointer :: item

      call c_f_pointer(ptr, item)

      call calculate_global_statistics(item, dmedian, sumP, countP, sumN, countN, first, last)

   end subroutine calculate_global_statistics_C

   subroutine calculate_global_statistics(item, dmedian, sumP, countP, sumN, countN, first, last)
      use omp_lib
      use, intrinsic :: iso_c_binding
      implicit none

      type(dataset), pointer, intent(in) :: item
      real(c_float), intent(in) :: dmedian
      real(c_float), intent(out) :: sumP, sumN
      integer(c_int64_t), intent(out) :: countP, countN
      integer, intent(in) :: first, last ! FITS data cube frame range

      integer :: max_threads, frame
      integer(c_int) :: width, height
      real(c_float) :: thread_sumP, thread_sumN
      integer(c_int64_t) :: thread_countP, thread_countN

      sumP = 0.0
      countP = 0
      sumN = 0.0
      countN = 0

      if (.not. allocated(item%compressed)) return

      ! range validation
      if (first .lt. 1) return
      if (last .gt. item%naxes(3)) return

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      thread_sumP = 0.0
      thread_countP = 0
      thread_sumN = 0.0
      thread_countN = 0

      width = item%naxes(1)
      height = item%naxes(2)

      ! iterate through all the available planes
      !$omp PARALLEL DEFAULT(SHARED) SHARED(item)&
      !$omp& PRIVATE(frame)&
      !$omp& REDUCTION(+:thread_sumP,countP)&
      !$omp& REDUCTION(+:thread_sumN,countN)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO
      do frame = first, last

         ! skip frames for which there is no data on this node
         if (.not. associated(item%compressed(frame)%ptr)) cycle

         ! call Intel SPMD C
         call make_global_statistics(c_loc(item%compressed(frame)%ptr), width, height,&
         &dmedian, thread_sumP, thread_countP, thread_sumN, thread_countN)

      end do
      !$omp END DO
      !$omp END PARALLEL

      sumP = thread_sumP
      countP = thread_countP
      sumN = thread_sumN
      countN = thread_countN

   end subroutine calculate_global_statistics

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
      call add_json_integer(json, 'BITPIX'//c_null_char, item%bitpix)
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

   subroutine get_inner_dimensions(ptr, width, height, fits_width, fits_height, inner_width, inner_height, scale) bind(c)
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: ptr
      integer(c_int), intent(in), value :: width, height
      integer(c_int), intent(out) :: fits_width, fits_height, inner_width, inner_height
      real(c_float), intent(out) :: scale

      type(dataset), pointer :: item

      type(inner_dims_req_t), target :: inner_dims
      type(c_pthread_t) :: pid
      integer :: rc

      call c_f_pointer(ptr, item)

      fits_width = item%naxes(1)
      fits_height = item%naxes(2)

      ! fill-in the inner_dims
      inner_dims%datasetid = c_loc(item%datasetid)
      inner_dims%len = size(item%datasetid)
      inner_dims%width = 0
      inner_dims%height = 0

      ! launch a pthread
      rc = c_pthread_create(thread=pid, &
                            attr=c_null_ptr, &
                            start_routine=c_funloc(fetch_inner_dimensions), &
                            arg=c_loc(inner_dims))

      ! get the inner image bounding box (excluding NaNs)
      call inherent_image_dimensions(item, inner_width, inner_height)

      ! join a thread
      rc = c_pthread_join(pid, c_null_ptr)

      ! synchronise with the cluster
      inner_width = max(inner_width, inner_dims%width)
      inner_height = max(inner_height, inner_dims%height)

      ! get the downscaled image dimensions
      scale = get_image_scale(width, height, inner_width, inner_height)

   end subroutine get_inner_dimensions

   subroutine image_spectrum_request(ptr, width, height, precision, fetch_data, fd) bind(C)
      use :: unix_pthread
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

      type(inner_dims_req_t), target :: inner_dims
      type(image_req_t), target :: image_req
      type(c_pthread_t) :: pid
      integer :: rc

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

      ! only for data cubes
      if (item%naxis .gt. 2 .and. item%naxes(3) .gt. 1) then
         ! fill-in the inner_dims
         inner_dims%datasetid = c_loc(item%datasetid)
         inner_dims%len = size(item%datasetid)
         inner_dims%width = 0
         inner_dims%height = 0

         ! launch a pthread
         rc = c_pthread_create(thread=pid, &
                               attr=c_null_ptr, &
                               start_routine=c_funloc(fetch_inner_dimensions), &
                               arg=c_loc(inner_dims))
      end if

      ! get the inner image bounding box (excluding NaNs)
      call inherent_image_dimensions(item, inner_width, inner_height)

      ! only for data cubes
      if (item%naxis .gt. 2 .and. item%naxes(3) .gt. 1) then
         ! join a thread
         rc = c_pthread_join(pid, c_null_ptr)

         ! synchronise with the cluster
         inner_width = max(inner_width, inner_dims%width)
         inner_height = max(inner_height, inner_dims%height)
      end if

      ! get the downscaled image dimensions
      scale = get_image_scale(width, height, inner_width, inner_height)

      if (scale .lt. 1.0) then
         img_width = nint(scale*item%naxes(1))
         img_height = nint(scale*item%naxes(2))

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

      else
         img_width = item%naxes(1)
         img_height = item%naxes(2)

         ! make a copy of item%pixels / item%mask
         allocate (pixels(img_width, img_height), source=item%pixels)
         allocate (mask(img_width, img_height), source=item%mask)

      end if

      print *, 'scale = ', scale, 'image dimensions:', img_width, 'x', img_height

      ! only for data cubes
      if (item%naxis .gt. 2 .and. item%naxes(3) .gt. 1) then
         ! fetch/gather pixels/mask from other cluster nodes (3D cubes only, depth > 1)
         image_req%datasetid = c_loc(item%datasetid)
         image_req%len = size(item%datasetid)
         image_req%pixels = c_loc(pixels)
         image_req%mask = c_loc(mask)
         image_req%width = img_width
         image_req%height = img_height

         ! launch a pthread
         rc = c_pthread_create(thread=pid, &
                               attr=c_null_ptr, &
                               start_routine=c_funloc(fetch_image), &
                               arg=c_loc(image_req))

         ! join a thread (wait for the results)
         rc = c_pthread_join(pid, c_null_ptr)
      end if

      ! make an image histogram, decide on the flux etc.
      call make_image_statistics(item, img_width, img_height, pixels, mask, hist, tone)

      call write_image_spectrum(fd, trim(tone%flux)//c_null_char,&
      &tone%pmin, tone%pmax, tone%pmedian,&
      &tone%black, tone%white, tone%sensitivity, tone%ratio_sensitivity,&
      & img_width, img_height, precision, c_loc(pixels), c_loc(mask))

      deallocate (pixels)
      deallocate (mask)

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
            call write_spectrum(fd, c_loc(item%mean_spectrum), size(item%mean_spectrum), ZFP_HIGH_PRECISION)
         end if

         ! integrated spectrum
         if (allocated(item%integrated_spectrum)) then
            call write_spectrum(fd, c_loc(item%integrated_spectrum), size(item%integrated_spectrum), ZFP_HIGH_PRECISION)
         end if
      end if

   end subroutine image_spectrum_request

   subroutine image_request(ptr, width, height, fd) bind(C)
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: ptr
      type(dataset), pointer :: item
      integer(kind=c_int), intent(in), value :: width, height, fd

      real(kind=c_float), dimension(:, :), allocatable, target :: pixels
      logical(kind=c_bool), dimension(:, :), allocatable, target :: mask

      real :: scale, s1, s2
      integer(kind=c_size_t) :: written

      call c_f_pointer(ptr, item)

      if (.not. allocated(item%pixels)) return
      if (.not. allocated(item%mask)) return

      ! check the image dimensions; downscaling may not be necessary
      if ((width .lt. item%naxes(1)) .or. (height .lt. item%naxes(2))) then
         ! downscale item%pixels and item%mask into pixels, mask

         allocate (pixels(width, height))
         allocate (mask(width, height))

         ! s1 and s2 should be pretty much the same (within a rounding error)
         s1 = real(width)/real(item%naxes(1))
         s2 = real(height)/real(item%naxes(2))
         scale = 0.5*(s1 + s2)

         if (scale .gt. 0.2) then
            call resizeLanczos(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), width, height, 3)
         else
            call resizeSuper(c_loc(item%pixels), item%naxes(1), item%naxes(2), c_loc(pixels), width, height)
         end if

         ! Boolean mask: the naive Nearest-Neighbour method

         call resizeNearest(c_loc(item%mask), item%naxes(1), item%naxes(2), c_loc(mask), width, height)

         ! send pixels
         written = chunked_write(fd, c_loc(pixels), sizeof(pixels))

         ! send mask
         written = chunked_write(fd, c_loc(mask), sizeof(mask))

         deallocate (pixels)
         deallocate (mask)
      else
         ! send item%pixels 'as-is'
         written = chunked_write(fd, c_loc(item%pixels), sizeof(item%pixels))

         ! send item%mask 'as-is'
         written = chunked_write(fd, c_loc(item%mask), sizeof(item%mask))
      end if

   end subroutine image_request

   recursive subroutine realtime_image_spectrum_request(user) BIND(C, name='realtime_image_spectrum_request')
      use omp_lib
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(image_spectrum_request_f), pointer :: req

      ! output variables
      real(kind=c_float), dimension(:), allocatable, target :: pixels
      logical(kind=c_bool), dimension(:), allocatable, target :: mask
      real(kind=c_float), dimension(:), allocatable, target :: spectrum

      integer :: first, last, length
      logical :: bSuccess

      integer :: tid, max_threads, npixels, frame
      integer :: x1, x2, y1, y2, cx, cy, r, r2
      integer :: start_x, start_y, end_x, end_y
      logical :: average, test_ignrval
      real :: cdelt3

      real(kind=4), allocatable :: thread_pixels(:, :)
      logical(kind=1), allocatable :: thread_mask(:, :)

      integer :: dimx, dimy, native_size, viewport_size
      real :: scale

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real :: elapsed

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      bSuccess = .true.

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)

      if (.not. allocated(item%compressed)) then
         print *, "item%compressed has not been allocated; aborting 'realtime_image_spectrum'"
         return
      end if

      print *, 'realtime_image_spectrum for ', item%datasetid,&
      &', dx:', req%dx, ', image:', req%image, ', quality:', req%quality, ', x1:', req%x1, &
      &', y1:', req%y1, ', x2:', req%x2, ', y2:', req%y2, ', width:', req%width, &
      &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
      &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
         req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp, ', fd:', req%fd

      ! get the range of the cube planes
      call get_spectrum_range(item, req%frame_start, req%frame_end, req%ref_freq, first, last)

      length = last - first + 1

      print *, 'first:', first, 'last:', last, 'length:', length, 'depth:', item%naxes(3)

      ! sanity checks
      x1 = max(1, req%x1)
      y1 = max(1, req%y1)
      x2 = min(item%naxes(1), req%x2)
      y2 = min(item%naxes(2), req%y2)

      ! calculate the centre and squared radius
      cx = abs(x1 + x2)/2
      cy = abs(y1 + y2)/2
      r = min(abs(x2 - x1)/2, abs(y2 - y1)/2)
      r2 = r*r

      if (req%intensity .eq. mean) then
         average = .true.
      else
         average = .false.
      end if

      dimx = abs(x2 - x1 + 1)
      dimy = abs(y2 - y1 + 1)
      npixels = dimx*dimy

      ! real-time data decompression
      start_x = 1 + (x1 - 1)/DIM
      start_y = 1 + (y1 - 1)/DIM

      end_x = 1 + (x2 - 1)/DIM
      end_y = 1 + (y2 - 1)/DIM

      ! allocate and zero-out the spectrum
      allocate (spectrum(first:last))
      spectrum = 0.0

      call get_cdelt3(item, cdelt3)

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      print *, 'start_x:', start_x, 'start_y:', start_y, 'end_x:', end_x, 'end_y:', end_y,&
      & "max_threads:", max_threads

      ! do we need the viewport too?
      if (req%image) then
         allocate (thread_pixels(npixels, max_threads))
         allocate (thread_mask(npixels, max_threads))

         thread_pixels = 0.0
         thread_mask = .false.
      end if

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item, spectrum)&
      !$omp& SHARED(thread_pixels, thread_mask) PRIVATE(tid, frame)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO
      do frame = first, last
         ! skip frames for which there is no data on this node
         if (.not. associated(item%compressed(frame)%ptr)) cycle

         ! get a current OpenMP thread (starting from 0 as in C)
         tid = 1 + OMP_GET_THREAD_NUM()

         block
            type(fixed_block) :: compressed
            real(kind=4), dimension(DIM, DIM) :: x
            integer(kind=2) :: bitmask

            ! the maximum exponent
            integer :: max_exp

            integer :: i, j, ix, iy, pixel_count, src_x, src_y, dist2
            integer :: offset_x, offset_y, offset
            logical :: valid_pixel

            real :: tmp, pixel_sum

            ! process the data
            pixel_sum = 0.0
            pixel_count = 0

            ! decompress each DIMxDIM block
            do iy = start_y, end_y
               do ix = start_x, end_x
                  compressed = item%compressed(frame)%ptr(ix, iy)

                  max_exp = int(compressed%common_exp)
                  x = dequantize(compressed%mantissa, max_exp, significant_bits)

                  ! do concurrent(j=1:DIM) ! not sure about the safety of 'concurrent' in this case
                  do j = 1, DIM

                     ! 16x16 blocks
                     bitmask = compressed%mask(j)

                     do i = 1, DIM
                        ! test a NaN mask
                        if (.not. btest(bitmask, i - 1)) then ! notice that pos = i - 1
                           ! we have a non-NaN pixel
                           tmp = x(i, j)

                           ! calculate the original pixel coordinates
                           src_x = i + shiftl(ix - 1, BASE)
                           src_y = j + shiftl(iy - 1, BASE)

                           ! check if a pixel resides within the bounding box
                           if ((src_x .lt. x1) .or. (src_x .gt. x2)) cycle
                           if ((src_y .lt. y1) .or. (src_y .gt. y2)) cycle

                           valid_pixel = .true.

                           ! check if a pixel resides within the bounding circle
                           if (req%beam .eq. circle) then
                              dist2 = (cx - src_x)*(cx - src_x) + (cy - src_y)*(cy - src_y)
                              if (dist2 > r2) valid_pixel = .false.
                           end if

                           ! we have a valid pixel
                           if (valid_pixel) then
                              pixel_sum = pixel_sum + tmp
                              pixel_count = pixel_count + 1
                           end if

                           ! do we need the viewport too?
                           if (req%image) then
                              offset_x = 1 + src_x - x1
                              offset_y = src_y - y1
                              offset = offset_y*dimx + offset_x

                              ! integrate (sum up) pixels and a NaN mask
                              if ((offset .ge. 1) .and. (offset .le. npixels)) then
                                 thread_pixels(offset, tid) = thread_pixels(offset, tid) + tmp*cdelt3
                                 thread_mask(offset, tid) = thread_mask(offset, tid) .or. .true.
                              end if
                           end if
                        end if
                     end do
                  end do
               end do
            end do

            if (pixel_count .gt. 0) then
               if (req%intensity .eq. mean) spectrum(frame) = pixel_sum/real(pixel_count)
               if (req%intensity .eq. integrated) spectrum(frame) = pixel_sum*cdelt3
            end if

         end block
      end do
      !$omp END DO
      !$omp END PARALLEL

      ! reduce the pixels/mask locally
      if (req%image .and. (max_threads .gt. 1)) then
         do tid = 2, max_threads
            thread_pixels(:, 1) = thread_pixels(:, 1) + thread_pixels(:, tid)
            thread_mask(:, 1) = thread_mask(:, 1) .or. thread_mask(:, tid)
         end do
      end if

      ! end the timer
      call system_clock(finish_t)
      elapsed = real(finish_t - start_t)/real(crate)

      ! print *, 'spectrum:', spectrum
      print *, 'realtime_image_spectrum elapsed time:', 1000*elapsed, '[ms]'

   end subroutine realtime_image_spectrum_request

   subroutine LTTB(data, threshold, spectrum)
      implicit NONE

      real, intent(in) :: data(:)
      integer, intent(in) :: threshold
      real, intent(out), allocatable :: spectrum(:)

      ! internal variables
      integer i, sampledIndex, a, nextA, dataLength
      real maxAreaPoint, maxArea, area, every

      print *, 'downsizing spectrum with Largest-Triangle-Three-Buckets'

      dataLength = size(data)
      allocate (spectrum(threshold))

      ! always add the first point
      spectrum(1) = data(1)
      sampledIndex = 1
      a = 0

      ! Bucket size. Leave room for start and end data points
      every = real(dataLength - 2)/real(threshold - 2)

      do i = 0, threshold - 2 - 1
         block
            integer avgRangeStart, avgRangeEnd, avgRangeLength
            real avgX, avgY, pointAX, pointAY
            integer rangeOffs, rangeTo

            avgRangeStart = floor((i + 1)*every) + 1
            avgRangeEnd = floor((i + 2)*every) + 1
            avgRangeEnd = min(avgRangeEnd, dataLength)
            avgRangeLength = avgRangeEnd - avgRangeStart

            avgX = 0.0
            avgY = 0.0

            do while (avgRangeStart < avgRangeEnd)
               avgX = avgX + avgRangeStart
               avgY = avgY + data(1 + avgRangeStart)
               avgRangeStart = avgRangeStart + 1
            end do

            avgX = avgX/avgRangeLength
            avgY = avgY/avgRangeLength

            ! Get the range for this bucket
            rangeOffs = floor((i + 0)*every) + 1
            rangeTo = floor((i + 1)*every) + 1

            ! Point a
            pointAX = a
            pointAY = data(1 + a)

            maxArea = -1

            do while (rangeOffs < rangeTo)
               ! Calculate triangle area over three buckets
               area = abs((pointAX - avgX)*(data(1 + rangeOffs) - pointAY) -&
               &(pointAX - rangeOffs)*(avgY - pointAY))*0.5

               if (area .gt. maxArea) then
                  maxArea = area
                  maxAreaPoint = data(1 + rangeOffs)
                  nextA = rangeOffs ! Next a is this b
               end if

               rangeOffs = rangeOffs + 1
            end do

            spectrum(1 + sampledIndex) = maxAreaPoint ! Pick this point from the bucket
            sampledIndex = sampledIndex + 1
            a = nextA ! This a is the next a (chosen b)
         end block
      end do

      ! always add the last element
      spectrum(1 + sampledIndex) = data(dataLength)
      sampledIndex = sampledIndex + 1

   end subroutine LTTB

   recursive subroutine realtime_image_spectrum_request_simd(user) BIND(C, name='realtime_image_spectrum_request_simd')
      use omp_lib
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(image_spectrum_request_f), pointer :: req

      ! output variables
      real(kind=c_float), allocatable, target :: pixels(:), view_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: mask(:), view_mask(:, :)
      real(kind=c_float), dimension(:), allocatable, target :: spectrum, reduced_spectrum, cluster_spectrum

      integer :: first, last, length, threshold
      integer :: max_threads, frame, tid, npixels
      integer(c_int) :: x1, x2, y1, y2, width, height, average
      real(c_float) :: cx, cy, r, r2
      integer :: start_x, start_y, end_x, end_y
      real :: cdelt3

      real(kind=c_float), allocatable, target :: thread_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: thread_mask(:, :)

      integer :: dimx, dimy, native_size, viewport_size
      integer(c_int) :: precision
      real :: scale

      ! cluster
      type(image_spectrum_request_t), target :: cluster_req
      type(c_pthread_t) :: pid
      integer :: rc

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real(c_float) :: elapsed

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)

      print *, 'realtime_image_spectrum for ', item%datasetid,&
      &', dx:', req%dx, ', image:', req%image, ', quality:', req%quality, ', x1:', req%x1, &
      &', y1:', req%y1, ', x2:', req%x2, ', y2:', req%y2, ', width:', req%width, &
      &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
      &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
         req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp, ', fd:', req%fd

      ! respond with a 2D viewport if req%image .eq. .true.
      if (.not. allocated(item%compressed)) then
         if (req%image) then
            call realtime_viewport_request(item, req)
         else
            print *, "item%compressed has not been allocated; aborting 'realtime_image_spectrum_simd'"
         end if

         if (req%fd .ne. -1) call close_pipe(req%fd)
         nullify (item)
         nullify (req) ! disassociate the FORTRAN pointer from the C memory region
         call free(user) ! release C memory

         return
      end if

      ! get the range of the cube planes
      call get_spectrum_range(item, req%frame_start, req%frame_end, req%ref_freq, first, last)

      length = last - first + 1

      print *, 'first:', first, 'last:', last, 'length:', length, 'depth:', item%naxes(3)

      ! sanity checks
      x1 = max(1, req%x1)
      y1 = max(1, req%y1)
      x2 = min(item%naxes(1), req%x2)
      y2 = min(item%naxes(2), req%y2)

      ! calculate the centre and squared radius
      cx = 0.5*abs(x1 + x2)
      cy = 0.5*abs(y1 + y2)
      ! r = 0.5*min(abs(x2 - x1), abs(y2 - y1))
      r = min(abs(cx - x1), abs(cy - y1)) + 1
      r2 = r*r

      if (req%intensity .eq. mean) then
         average = 1
      else
         average = 0
      end if

      width = item%naxes(1)
      height = item%naxes(2)

      dimx = abs(x2 - x1 + 1)
      dimy = abs(y2 - y1 + 1)
      npixels = dimx*dimy

      ! real-time data decompression
      start_x = 1 + (x1 - 1)/DIM
      start_y = 1 + (y1 - 1)/DIM

      end_x = 1 + (x2 - 1)/DIM
      end_y = 1 + (y2 - 1)/DIM

      ! allocate and zero-out the spectrum
      allocate (spectrum(first:last))
      spectrum = 0.0

      call get_cdelt3(item, cdelt3)

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      print *, 'start_x:', start_x, 'start_y:', start_y, 'end_x:', end_x, 'end_y:', end_y,&
      & "max_threads:", max_threads

      ! do we need the viewport too?
      if (req%image) then
         allocate (pixels(npixels))
         allocate (mask(npixels))

         pixels = 0.0
         mask = .false.

         allocate (thread_pixels(npixels, max_threads))
         allocate (thread_mask(npixels, max_threads))

         thread_pixels = 0.0
         thread_mask = .false.
      end if

      ! launch a cluster thread (check if the number of cluster nodes is .gt. 0)
      allocate (cluster_spectrum(first:last))
      cluster_spectrum = 0.0

      cluster_req%datasetid = c_loc(item%datasetid)
      cluster_req%len = size(item%datasetid)

      ! inputs
      cluster_req%image = req%image
      cluster_req%x1 = req%x1
      cluster_req%y1 = req%y1
      cluster_req%x2 = req%x2
      cluster_req%y2 = req%y2
      cluster_req%beam = req%beam
      cluster_req%intensity = req%intensity
      cluster_req%frame_start = req%frame_start
      cluster_req%frame_end = req%frame_end
      cluster_req%ref_freq = req%ref_freq

      ! outputs
      if (req%image) then
         cluster_req%pixels = c_loc(pixels)
         cluster_req%mask = c_loc(mask)
      else
         cluster_req%pixels = c_null_ptr
         cluster_req%mask = c_null_ptr
      end if
      cluster_req%spectrum = c_loc(cluster_spectrum)

      cluster_req%dimx = dimx
      cluster_req%dimy = dimy
      cluster_req%length = size(cluster_spectrum)
      cluster_req%valid = .false.

      ! launch a thread
      rc = c_pthread_create(thread=pid, &
                            attr=c_null_ptr, &
                            start_routine=c_funloc(fetch_realtime_image_spectrum), &
                            arg=c_loc(cluster_req))

      ! end of cluster

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item, spectrum)&
      !$omp& SHARED(thread_pixels, thread_mask) PRIVATE(tid, frame)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO
      do frame = first, last

         ! skip frames for which there is no data on this node
         if (.not. associated(item%compressed(frame)%ptr)) cycle

         ! get a current OpenMP thread (starting from 0 as in C)
         tid = 1 + OMP_GET_THREAD_NUM()

         if (.not. req%image) then
            if (req%beam .eq. square) then
               spectrum(frame) = viewport_spectrum_rect(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame),&
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, average, cdelt3)
            end if

            if (req%beam .eq. circle) then
               spectrum(frame) = viewport_spectrum_circle(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame), &
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, cx - 1, cy - 1, r2, average, cdelt3)
            end if
         else
            if (req%beam .eq. square) then
               spectrum(frame) = viewport_image_spectrum_rect(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame),&
               &c_loc(thread_pixels(:, tid)), c_loc(thread_mask(:, tid)), dimx, &
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, average, cdelt3)
            end if

            if (req%beam .eq. circle) then
               spectrum(frame) = viewport_image_spectrum_circle(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame), c_loc(thread_pixels(:, tid)),&
               & c_loc(thread_mask(:, tid)), dimx, x1 - 1, x2 - 1, y1 - 1, y2 - 1, cx - 1, cy - 1, r2, average, cdelt3)
            end if
         end if

      end do
      !$omp END DO
      !$omp END PARALLEL

      ! join a thread
      rc = c_pthread_join(pid, c_null_ptr)

      ! reduce the pixels/mask locally
      if (req%image) then
         do tid = 1, max_threads
            pixels(:) = pixels(:) + thread_pixels(:, tid)
            mask(:) = mask(:) .or. thread_mask(:, tid)
         end do
      end if

      ! combine the spectra from other cluster nodes (if any)
      if (cluster_req%valid) spectrum = spectrum + cluster_spectrum

      if (req%fd .ne. -1) then

         ! the spectrum part

         if (req%image) then
            precision = ZFP_HIGH_PRECISION
         else
            precision = ZFP_MEDIUM_PRECISION
         end if

         threshold = req%dx/2

         if (size(spectrum) .gt. threshold) then
            ! downsize the spectrum
            call LTTB(spectrum, threshold, reduced_spectrum)

            ! end the timer
            call system_clock(finish_t)
            elapsed = 1000.0*real(finish_t - start_t)/real(crate) ! [ms]

            call write_elapsed(req%fd, elapsed)
            call write_spectrum(req%fd, c_loc(reduced_spectrum), size(reduced_spectrum), precision)
         else

            ! end the timer
            call system_clock(finish_t)
            elapsed = 1000.0*real(finish_t - start_t)/real(crate) ! [ms]

            call write_elapsed(req%fd, elapsed)
            call write_spectrum(req%fd, c_loc(spectrum), size(spectrum), precision)
         end if

         ! the image (viewport) part
         if (req%image) then
            select case (req%quality)
            case (low)
               precision = ZFP_LOW_PRECISION
            case (high)
               precision = ZFP_HIGH_PRECISION
            case default
               precision = ZFP_MEDIUM_PRECISION
            end select

            native_size = dimx*dimy
            viewport_size = req%width*req%height
            scale = real(req%width)/real(dimx)

            print *, 'native:', native_size, 'viewport:', viewport_size, 'scale:', scale

            if (native_size .gt. viewport_size) then
               ! downsize the pixels/mask from {dimx,dimy} to {req%width,req%height}

               allocate (view_pixels(req%width, req%height))
               allocate (view_mask(req%width, req%height))

               if (scale .gt. 0.2) then
                  call resizeLanczos(c_loc(pixels), dimx, dimy,&
                  & c_loc(view_pixels), req%width, req%height, 3)
               else
                  call resizeSuper(c_loc(pixels), dimx, dimy,&
                  & c_loc(view_pixels), req%width, req%height)
               end if

               call resizeNearest(c_loc(mask), dimx, dimy,&
               & c_loc(view_mask), req%width, req%height)

               call write_viewport(req%fd, req%width, req%height, c_loc(view_pixels), c_loc(view_mask), precision)
            else
               ! no need for downsizing
               call write_viewport(req%fd, dimx, dimy, c_loc(pixels), c_loc(mask), precision)
            end if
         end if

         call close_pipe(req%fd)
      end if

      nullify (item)
      nullify (req) ! disassociate the FORTRAN pointer from the C memory region
      call free(user) ! release C memory

      print *, 'realtime_image_spectrum elapsed time:', elapsed, '[ms]'

   end subroutine realtime_image_spectrum_request_simd

   subroutine realtime_viewport_request(item, req)
      implicit none

      type(dataset), pointer :: item
      type(image_spectrum_request_f), pointer :: req

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real(c_float) :: elapsed

      integer :: x1, x2, y1, y2
      integer :: dimx, dimy, native_size, viewport_size
      real :: scale
      integer(c_int) :: precision

      real(kind=c_float), allocatable, target :: pixels(:, :), view_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: mask(:, :), view_mask(:, :)

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      if ((.not. allocated(item%pixels)) .or. (.not. allocated(item%mask))) return

      ! sanity checks
      x1 = max(1, req%x1)
      y1 = max(1, req%y1)
      x2 = min(item%naxes(1), req%x2)
      y2 = min(item%naxes(2), req%y2)

      dimx = abs(x2 - x1 + 1)
      dimy = abs(y2 - y1 + 1)

      ! automatic memory allocation
      pixels = item%pixels(x1:x2, y1:y2)
      mask = item%mask(x1:x2, y1:y2)

      select case (req%quality)
      case (low)
         precision = ZFP_LOW_PRECISION
      case (high)
         precision = ZFP_HIGH_PRECISION
      case default
         precision = ZFP_MEDIUM_PRECISION
      end select

      native_size = dimx*dimy
      viewport_size = req%width*req%height
      scale = real(req%width)/real(dimx)

      print *, 'native:', native_size, 'viewport:', viewport_size, 'scale:', scale

      if (native_size .gt. viewport_size) then
         ! downsize the pixels/mask from {dimx,dimy} to {req%width,req%height}

         allocate (view_pixels(req%width, req%height))
         allocate (view_mask(req%width, req%height))

         if (scale .gt. 0.2) then
            call resizeLanczos(c_loc(pixels), dimx, dimy,&
            & c_loc(view_pixels), req%width, req%height, 3)
         else
            call resizeSuper(c_loc(pixels), dimx, dimy,&
            & c_loc(view_pixels), req%width, req%height)
         end if

         call resizeNearest(c_loc(mask), dimx, dimy,&
         & c_loc(view_mask), req%width, req%height)

         ! end the timer
         call system_clock(finish_t)
         elapsed = 1000.0*real(finish_t - start_t)/real(crate) ! [ms]

         call write_elapsed(req%fd, elapsed)
         call write_spectrum(req%fd, c_null_ptr, 0, precision)
         call write_viewport(req%fd, req%width, req%height, c_loc(view_pixels), c_loc(view_mask), precision)
      else
         ! no need for downsizing

         ! end the timer
         call system_clock(finish_t)
         elapsed = 1000.0*real(finish_t - start_t)/real(crate) ! [ms]

         call write_elapsed(req%fd, elapsed)
         call write_spectrum(req%fd, c_null_ptr, 0, precision)
         call write_viewport(req%fd, dimx, dimy, c_loc(pixels), c_loc(mask), precision)
      end if

      print *, "handle_viewport_request elapsed time:", elapsed, '[ms]'
   end subroutine realtime_viewport_request

   recursive subroutine viewport_request(user) BIND(C, name='viewport_request')
      use omp_lib
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(image_spectrum_request_f), pointer :: req

      integer :: first, last, length

      integer :: max_threads, frame, tid, npixels
      integer(c_int) :: x1, x2, y1, y2, width, height, average
      real(c_float) :: cx, cy, r, r2
      integer :: start_x, start_y, end_x, end_y
      real :: cdelt3

      real(kind=c_float), allocatable, target :: thread_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: thread_mask(:, :)
      logical :: valid

      ! output variables
      real(kind=c_float), allocatable, target :: pixels(:)
      logical(kind=c_bool), allocatable, target :: mask(:)
      real(kind=c_float), dimension(:), allocatable, target :: spectrum

      integer :: dimx, dimy
      integer(kind=c_size_t) :: written

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)

      ! print *, 'viewport_request for ', item%datasetid,&
      ! &', dx:', req%dx, ', image:', req%image, ', quality:', req%quality, ', x1:', req%x1, &
      !     &', y1:', req%y1, ', x2:', req%x2, ', y2:', req%y2, ', width:', req%width, &
      !    &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
      !     &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
      !    req%ref_freq, ', seq_id:', req%seq_id, ', timestamp:', req%timestamp, ', fd:', req%fd

      if (req%fd .eq. -1) then
         nullify (item)
         nullify (req) ! disassociate the FORTRAN pointer from the C memory region
         call free(user) ! release C memory

         return
      end if

      ! get the range of the cube planes
      call get_spectrum_range(item, req%frame_start, req%frame_end, req%ref_freq, first, last)

      length = last - first + 1

      ! sanity checks
      x1 = max(1, req%x1)
      y1 = max(1, req%y1)
      x2 = min(item%naxes(1), req%x2)
      y2 = min(item%naxes(2), req%y2)

      ! calculate the centre and squared radius
      cx = 0.5*abs(x1 + x2)
      cy = 0.5*abs(y1 + y2)
      ! r = 0.5*min(abs(x2 - x1), abs(y2 - y1))
      r = min(abs(cx - x1), abs(cy - y1)) + 1
      r2 = r*r

      if (req%intensity .eq. mean) then
         average = 1
      else
         average = 0
      end if

      width = item%naxes(1)
      height = item%naxes(2)

      dimx = abs(x2 - x1 + 1)
      dimy = abs(y2 - y1 + 1)
      npixels = dimx*dimy

      ! real-time data decompression
      start_x = 1 + (x1 - 1)/DIM
      start_y = 1 + (y1 - 1)/DIM

      end_x = 1 + (x2 - 1)/DIM
      end_y = 1 + (y2 - 1)/DIM

      ! allocate and zero-out the spectrum
      allocate (spectrum(first:last))
      spectrum = 0.0

      call get_cdelt3(item, cdelt3)

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      ! do we need the viewport too?
      if (req%image) then
         allocate (pixels(npixels))
         allocate (mask(npixels))

         pixels = 0.0
         mask = .false.

         allocate (thread_pixels(npixels, max_threads))
         allocate (thread_mask(npixels, max_threads))

         thread_pixels = 0.0
         thread_mask = .false.
      end if

      valid = .false.

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item, spectrum)&
      !$omp& SHARED(thread_pixels, thread_mask) PRIVATE(tid, frame)&
      !$omp& REDUCTION(.or.:valid)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO
      do frame = first, last

         ! skip frames for which there is no data on this node
         if (.not. associated(item%compressed(frame)%ptr)) cycle

         ! there is at least one frame with data
         valid = .true.

         ! get a current OpenMP thread (starting from 0 as in C)
         tid = 1 + OMP_GET_THREAD_NUM()

         if (.not. req%image) then
            if (req%beam .eq. square) then
               spectrum(frame) = viewport_spectrum_rect(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame),&
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, average, cdelt3)
            end if

            if (req%beam .eq. circle) then
               spectrum(frame) = viewport_spectrum_circle(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame), &
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, cx - 1, cy - 1, r2, average, cdelt3)
            end if
         else
            if (req%beam .eq. square) then
               spectrum(frame) = viewport_image_spectrum_rect(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame),&
               &c_loc(thread_pixels(:, tid)), c_loc(thread_mask(:, tid)), dimx, &
               &x1 - 1, x2 - 1, y1 - 1, y2 - 1, average, cdelt3)
            end if

            if (req%beam .eq. circle) then
               spectrum(frame) = viewport_image_spectrum_circle(c_loc(item%compressed(frame)%ptr),&
               &width, height, item%frame_min(frame), item%frame_max(frame), c_loc(thread_pixels(:, tid)),&
               & c_loc(thread_mask(:, tid)), dimx, x1 - 1, x2 - 1, y1 - 1, y2 - 1, cx - 1, cy - 1, r2, average, cdelt3)
            end if
         end if

      end do
      !$omp END DO
      !$omp END PARALLEL

      ! interleave transmitting data via a network with array reductions
      if (valid) then
         ! send spectrum
         written = chunked_write(req%fd, c_loc(spectrum), sizeof(spectrum))

         if (req%image) then
            ! reduce the pixels locally
            do tid = 1, max_threads
               pixels(:) = pixels(:) + thread_pixels(:, tid)
            end do

            ! send pixels
            written = chunked_write(req%fd, c_loc(pixels), sizeof(pixels))

            ! reduce the mask locally
            do tid = 1, max_threads
               mask(:) = mask(:) .or. thread_mask(:, tid)
            end do

            ! send mask
            written = chunked_write(req%fd, c_loc(mask), sizeof(mask))
         end if
      end if

      ! close the connection, release pointers
      call close_pipe(req%fd)
      nullify (item)
      nullify (req) ! disassociate the FORTRAN pointer from the C memory region
      call free(user) ! release C memory
      return

   end subroutine viewport_request

   ! calculate global dmad, dmadN, dmadP based on the all-data median
   recursive subroutine global_statistics(arg) BIND(C)
      use, intrinsic :: ISO_C_BINDING
      implicit none

      type(c_ptr), intent(in), value :: arg   ! a pointer to the FORTRAN FITS dataset

      type(dataset), pointer :: item

      type(mad_req_t), target :: req
      type(c_pthread_t) :: pid

      real(c_float) :: sumP, sumN
      integer(c_int64_t) :: countP, countN

      integer :: rc

      call c_f_pointer(arg, item)

      print *, 'calculating "all-data" global statistics'

      req%datasetid = c_loc(item%datasetid)
      req%len = size(item%datasetid)
      req%dmedian = item%dmedian
      req%sumP = 0.0
      req%countP = 0
      req%sumN = 0.0
      req%countN = 0
      req%first = 1
      req%last = item%naxes(3)

      ! launch a pthread
      rc = c_pthread_create(thread=pid, &
                            attr=c_null_ptr, &
                            start_routine=c_funloc(fetch_global_statistics), &
                            arg=c_loc(req))

      ! calculate global statistics locally
      call calculate_global_statistics(item, item%dmedian, sumP, countP, sumN, countN, req%first, req%last)

      ! join a thread
      rc = c_pthread_join(pid, c_null_ptr)

      print *, "cluster statistics... sumP", req%sumP, ", countP", req%countP, ", sumN", req%sumN, ", countN", req%countN

      ! merge the responses from the cluster
      sumP = sumP + req%sumP
      countP = countP + req%countP

      sumN = sumN + req%sumN
      countN = countN + req%countN

      print *, "final statistics... sumP", sumP, ", countP", countP, ", sumN", sumN, ", countN", countN

      if (countP .gt. 0) item%dmadP = sumP/countP
      if (countN .gt. 0) item%dmadN = sumN/countN
      if (countP + countN .gt. 0) item%dmad = (sumP + sumN)/(countP + countN)

      call set_video_status(item, .true.)

      call print_dataset(item)

   end subroutine global_statistics

   recursive subroutine video_request(user) BIND(C, name='video_request')
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(video_req_f), pointer :: req

      type(video_tone_mapping) :: tone
      integer(kind=1), allocatable, target :: pixels(:, :), mask(:, :)
      integer :: i
      integer(kind=c_size_t) :: written
      character(kind=c_char), pointer :: flux(:)

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)
      call c_f_pointer(req%flux, flux, [req%len])

      if (.not. allocated(item%compressed)) goto 6000
      if (.not. associated(item%compressed(req%frame)%ptr)) goto 6000
      ! OK, we've got the frame in question

      ! set the video tone mapping
      allocate (character(len=req%len)::tone%flux)

      do i = 1, req%len
         tone%flux(i:i) = flux(i)
      end do

      tone%dmin = req%dmin
      tone%dmax = req%dmax
      tone%dmedian = req%dmedian
      tone%black = req%black
      tone%white = req%white
      tone%sensitivity = req%sensitivity
      tone%slope = tone%slope

      ! allocate the pixels/mask
      allocate (pixels(req%width, req%height))
      allocate (mask(req%width, req%height))

      call get_video_frame(item, req%frame, tone, pixels, mask, req%width, req%height, req%downsize)

      if (req%fd .ne. -1) then
         ! send pixels
         written = chunked_write(req%fd, c_loc(pixels), sizeof(pixels))

         ! send mask
         written = chunked_write(req%fd, c_loc(mask), sizeof(mask))
      end if

      ! clean up
6000  nullify (item)
      if (req%fd .ne. -1) call close_pipe(req%fd)
      call free(req%flux)
      nullify (req) ! disassociate the FORTRAN pointer from the C memory region
      call free(user) ! release C memory

   end subroutine video_request

   recursive subroutine video_request_simd(user) BIND(C, name='video_request_simd')
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(video_request_f), pointer :: req

      type(video_tone_mapping) :: tone
      integer(kind=1), allocatable, target :: pixels(:, :), mask(:, :)
      integer :: i
      integer(kind=c_size_t) :: written
      character(kind=c_char), pointer :: flux(:)

      real, parameter :: u = 7.5

      type(video_fetch_f), allocatable, target :: fetch_req
      type(c_pthread_t) :: pid
      integer :: rc

      ! timing
      integer(8) :: start_t, finish_t, crate, cmax
      real(c_float) :: elapsed

      ! start the timer
      call system_clock(count=start_t, count_rate=crate, count_max=cmax)

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)
      call c_f_pointer(req%flux, flux, [req%len])

      print *, 'video_request_simd for ', item%datasetid, '; keyframe:', req%keyframe, 'frame:', req%frame, 'fd:', req%fd

      if (.not. allocated(item%compressed)) then
         call close_pipe(req%fd)
         goto 5000
      end if

      ! set the video tone mapping
      allocate (character(len=req%len)::tone%flux)

      do i = 1, req%len
         tone%flux(i:i) = flux(i)
      end do

      tone%dmin = item%dmin
      tone%dmax = item%dmax
      tone%dmedian = item%dmedian
      tone%black = max(item%dmin, item%dmedian - u*item%dmadN)
      tone%white = min(item%dmax, item%dmedian + u*item%dmadP)
      tone%sensitivity = 1.0/(tone%white - tone%black)
      tone%slope = tone%sensitivity

      ! allocate the pixels/mask
      allocate (pixels(req%width, req%height))
      allocate (mask(req%width, req%height))

      ! if a frame has not been found it needs to be fetched from the cluster (TO-DO)
      if (.not. associated(item%compressed(req%frame)%ptr)) then
         allocate (fetch_req)

         fetch_req%datasetid = c_loc(item%datasetid)
         fetch_req%len = size(item%datasetid)

         fetch_req%keyframe = req%keyframe
         fetch_req%frame = req%frame

         fetch_req%flux = req%flux
         fetch_req%dmin = tone%dmin
         fetch_req%dmax = tone%dmax
         fetch_req%dmedian = tone%dmedian
         fetch_req%sensitivity = tone%sensitivity
         fetch_req%slope = tone%slope
         fetch_req%white = tone%white
         fetch_req%black = tone%black

         fetch_req%width = req%width
         fetch_req%height = req%height
         fetch_req%downsize = req%downsize

         fetch_req%pixels = c_loc(pixels)
         fetch_req%mask = c_loc(mask)
         fetch_req%valid = .false.

         ! launch a pthread
         rc = c_pthread_create(thread=pid, &
                               attr=c_null_ptr, &
                               start_routine=c_funloc(fetch_video_frame), &
                               arg=c_loc(fetch_req))

         ! join a thread
         rc = c_pthread_join(pid, c_null_ptr)

         ! skip invalid frames (not found on other cluster nodes)
         if (.not. fetch_req%valid) then
            call close_pipe(req%fd)
            goto 5000
         end if
      else
         call get_video_frame(item, req%frame, tone, pixels, mask, req%width, req%height, req%downsize)
      end if

      ! end the timer
      call system_clock(finish_t)
      elapsed = 1000.0*real(finish_t - start_t)/real(crate) ! [ms]

      if (req%fd .ne. -1) then
         call write_elapsed(req%fd, elapsed)

         ! send pixels
         written = chunked_write(req%fd, c_loc(pixels), sizeof(pixels))

         ! send mask
         written = chunked_write(req%fd, c_loc(mask), sizeof(mask))

         call close_pipe(req%fd)
      end if

5000  nullify (item)
      call free(req%flux)
      nullify (req) ! disassociate the FORTRAN pointer from the C memory region
      call free(user) ! release C memory

      print *, 'video_request elapsed time:', elapsed, '[ms]'

   end subroutine video_request_simd

   subroutine get_video_frame(item, frame, tone, dst_pixels, dst_mask, dst_width, dst_height, downsize)
      use, intrinsic :: iso_c_binding
      implicit none

      type(dataset), intent(in), pointer :: item
      type(video_tone_mapping), intent(in) :: tone
      integer, intent(in) :: frame, dst_width, dst_height
      logical(kind=c_bool) :: downsize
      integer(kind=1), intent(out), target :: dst_pixels(dst_width, dst_height)
      integer(kind=1), intent(out), target :: dst_mask(dst_width, dst_height)

      integer(kind=1), allocatable, target :: pixels(:, :), mask(:, :)
      integer :: width, height

      real(kind=c_float) :: lmin, lmax

      width = item%naxes(1)
      height = item%naxes(2)

      ! a blank canvas by default
      dst_pixels = 0
      dst_mask = 0

      if (downsize) then
         allocate (pixels(width, height))
         allocate (mask(width, height))

         ! call SIMD on {pixels, mask}
         print *, "making a video frame with flux '", tone%flux, "' and downsizing"

         if (tone%flux .eq. "linear") then
            print *, "calling make_video_frame_fixed_linear"
            call make_video_frame_fixed_linear(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(pixels), c_loc(mask), width, tone%black, tone%slope)
         end if

         if (tone%flux .eq. "logistic") then
            print *, "calling make_video_frame_fixed_logistic"
            call make_video_frame_fixed_logistic(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(pixels), c_loc(mask), width, tone%dmedian, tone%sensitivity)
         end if

         if (tone%flux .eq. "ratio") then
            print *, "calling make_video_frame_fixed_ratio"
            call make_video_frame_fixed_ratio(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(pixels), c_loc(mask), width, tone%black, tone%sensitivity)
         end if

         if (tone%flux .eq. "square") then
            print *, "calling make_video_frame_fixed_square"
            call make_video_frame_fixed_square(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(pixels), c_loc(mask), width, tone%black, tone%sensitivity)
         end if

         if (tone%flux .eq. "legacy") then
            lmin = log(0.5)
            lmax = log(1.5)

            print *, "calling make_video_frame_fixed_legacy"
            call make_video_frame_fixed_legacy(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(pixels), c_loc(mask), width, tone%dmin, tone%dmax, lmin, lmax)
         end if

         ! downsize {pixels, mask} into {dst_pixels, dst_mask}
         call resizeNearest(c_loc(pixels), width, height,&
         & c_loc(dst_pixels), dst_width, dst_height)

         call resizeNearest(c_loc(mask), width, height,&
         & c_loc(dst_mask), dst_width, dst_height)
      else
         ! call SIMD on {dst_pixels, dst_mask}
         print *, "making a video frame with flux '", tone%flux, "'"

         if (tone%flux .eq. "linear") then
            print *, "calling make_video_frame_fixed_linear"
            call make_video_frame_fixed_linear(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(dst_pixels), c_loc(dst_mask), width, tone%black, tone%slope)
         end if

         if (tone%flux .eq. "logistic") then
            print *, "calling make_video_frame_fixed_logistic"
            call make_video_frame_fixed_logistic(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(dst_pixels), c_loc(dst_mask), width, tone%dmedian, tone%sensitivity)
         end if

         if (tone%flux .eq. "ratio") then
            print *, "calling make_video_frame_fixed_ratio"
            call make_video_frame_fixed_ratio(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(dst_pixels), c_loc(dst_mask), width, tone%black, tone%sensitivity)
         end if

         if (tone%flux .eq. "square") then
            print *, "calling make_video_frame_fixed_square"
            call make_video_frame_fixed_square(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(dst_pixels), c_loc(dst_mask), width, tone%black, tone%sensitivity)
         end if

         if (tone%flux .eq. "legacy") then
            lmin = log(0.5)
            lmax = log(1.5)

            print *, "calling make_video_frame_fixed_legacy"
            call make_video_frame_fixed_legacy(c_loc(item%compressed(frame)%ptr), width, height,&
               &c_loc(dst_pixels), c_loc(dst_mask), width, tone%dmin, tone%dmax, lmin, lmax)
         end if
      end if

   end subroutine get_video_frame

   recursive subroutine ws_image_spectrum_request(user) BIND(C, name='ws_image_spectrum_request')
      use omp_lib
      use :: unix_pthread
      use, intrinsic :: iso_c_binding
      implicit none

      type(C_PTR), intent(in), value :: user

      type(dataset), pointer :: item
      type(image_spectrum_request_f), pointer :: req

      ! output variables
      real(kind=c_float), allocatable, target :: pixels(:), view_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: mask(:), view_mask(:, :)
      real(kind=c_float), dimension(:), allocatable, target :: spectrum, reduced_spectrum, cluster_spectrum

      integer :: first, last, length, threshold
      integer :: max_threads, frame, tid, npixels
      integer(c_int) :: width, height, average
      real :: cdelt3

      real(kind=c_float), allocatable, target :: thread_pixels(:, :)
      logical(kind=c_bool), allocatable, target :: thread_mask(:, :)

      integer :: dimx, dimy, native_size, viewport_size
      integer :: inner_width, inner_height
      integer :: img_width, img_height
      integer(c_int) :: precision
      real :: scale

      ! image histogram
      integer(c_int), allocatable, target :: hist(:)

      ! image tone mapping
      type(image_tone_mapping) :: tone

      ! cluster
      type(image_spectrum_request_t), target :: cluster_req
      type(c_pthread_t) :: pid
      integer :: rc

      call c_f_pointer(user, req)
      call c_f_pointer(req%ptr, item)

      print *, 'ws_image_spectrum for ', item%datasetid,&
      &', dx:', req%dx, ', quality:', req%quality, ', width:', req%width, &
      &', height', req%height, ', beam:', req%beam, ', intensity:', req%intensity,&
      &', frame_start:', req%frame_start, ', frame_end:', req%frame_end, ', ref_freq:', &
         req%ref_freq, ', timestamp:', req%timestamp, ', fd:', req%fd

      if (.not. allocated(item%compressed)) then
         if (req%fd .ne. -1) call close_pipe(req%fd)
         nullify (item)
         nullify (req) ! disassociate the FORTRAN pointer from the C memory region
         call free(user) ! release C memory
      end if

      ! set the viewport to the whole image
      req%x1 = 1
      req%y1 = 1
      req%x2 = item%naxes(1)
      req%y2 = item%naxes(2)

      ! get the range of the cube planes
      call get_spectrum_range(item, req%frame_start, req%frame_end, req%ref_freq, first, last)

      length = last - first + 1

      print *, 'first:', first, 'last:', last, 'length:', length, 'depth:', item%naxes(3)

      if (req%intensity .eq. mean) then
         average = 1
      else
         average = 0
      end if

      width = item%naxes(1)
      height = item%naxes(2)

      dimx = abs(req%x2 - req%x1 + 1)
      dimy = abs(req%y2 - req%y1 + 1)
      npixels = dimx*dimy

      ! allocate and zero-out the spectrum
      allocate (spectrum(first:last))
      spectrum = 0.0

      call get_cdelt3(item, cdelt3)

      ! get #physical cores (ignore HT)
      max_threads = min(OMP_GET_MAX_THREADS(), get_physical_cores())

      ! we need the viewport too
      allocate (pixels(npixels))
      allocate (mask(npixels))

      pixels = 0.0
      mask = .false.

      allocate (thread_pixels(npixels, max_threads))
      allocate (thread_mask(npixels, max_threads))

      thread_pixels = 0.0
      thread_mask = .false.

      ! launch a cluster thread (check if the number of cluster nodes is .gt. 0)
      allocate (cluster_spectrum(first:last))
      cluster_spectrum = 0.0

      cluster_req%datasetid = c_loc(item%datasetid)
      cluster_req%len = size(item%datasetid)

      ! inputs
      cluster_req%image = req%image
      cluster_req%x1 = req%x1
      cluster_req%y1 = req%y1
      cluster_req%x2 = req%x2
      cluster_req%y2 = req%y2
      cluster_req%beam = req%beam
      cluster_req%intensity = req%intensity
      cluster_req%frame_start = req%frame_start
      cluster_req%frame_end = req%frame_end
      cluster_req%ref_freq = req%ref_freq

      ! outputs
      if (req%image) then
         cluster_req%pixels = c_loc(pixels)
         cluster_req%mask = c_loc(mask)
      else
         cluster_req%pixels = c_null_ptr
         cluster_req%mask = c_null_ptr
      end if
      cluster_req%spectrum = c_loc(cluster_spectrum)

      cluster_req%dimx = dimx
      cluster_req%dimy = dimy
      cluster_req%length = size(cluster_spectrum)
      cluster_req%valid = .false.

      ! launch a thread
      rc = c_pthread_create(thread=pid, &
                            attr=c_null_ptr, &
                            start_routine=c_funloc(fetch_realtime_image_spectrum), &
                            arg=c_loc(cluster_req))

      ! end of cluster

      !$omp PARALLEL DEFAULT(SHARED) SHARED(item, spectrum)&
      !$omp& SHARED(thread_pixels, thread_mask) PRIVATE(tid, frame)&
      !$omp& NUM_THREADS(max_threads)
      !$omp DO
      do frame = first, last

         ! skip frames for which there is no data on this node
         if (.not. associated(item%compressed(frame)%ptr)) cycle

         ! get a current OpenMP thread (starting from 0 as in C)
         tid = 1 + OMP_GET_THREAD_NUM()

         ! the image is square (rectangular)
         spectrum(frame) = viewport_image_spectrum_rect(c_loc(item%compressed(frame)%ptr),&
         &width, height, item%frame_min(frame), item%frame_max(frame),&
         &c_loc(thread_pixels(:, tid)), c_loc(thread_mask(:, tid)), dimx, &
         &req%x1 - 1, req%x2 - 1, req%y1 - 1, req%y2 - 1, average, cdelt3)

      end do
      !$omp END DO
      !$omp END PARALLEL

      ! join a thread
      rc = c_pthread_join(pid, c_null_ptr)

      ! reduce the pixels/mask locally
      do tid = 1, max_threads
         pixels(:) = pixels(:) + thread_pixels(:, tid)
         mask(:) = mask(:) .or. thread_mask(:, tid)
      end do

      ! combine the spectra from other cluster nodes (if any)
      if (cluster_req%valid) spectrum = spectrum + cluster_spectrum

      ! get the inner image bounding box (excluding NaNs)
      call inherent_image_dimensions_from_mask(reshape(mask, (/dimx, dimy/)), inner_width, inner_height)

      ! get the downscaled image dimensions
      scale = get_image_scale(req%width, req%height, inner_width, inner_height)

      if (scale .lt. 1.0) then
         img_width = nint(scale*item%naxes(1))
         img_height = nint(scale*item%naxes(2))

         allocate (view_pixels(img_width, img_height))
         allocate (view_mask(img_width, img_height))

         if (scale .gt. 0.2) then
            call resizeLanczos(c_loc(pixels), item%naxes(1), item%naxes(2), c_loc(view_pixels), img_width, img_height, 3)
         else
            call resizeSuper(c_loc(pixels), item%naxes(1), item%naxes(2), c_loc(view_pixels), img_width, img_height)
         end if

         call resizeNearest(c_loc(mask), item%naxes(1), item%naxes(2), c_loc(view_mask), img_width, img_height)
      else
         img_width = item%naxes(1)
         img_height = item%naxes(2)

         view_pixels = reshape(pixels, item%naxes(1:2))
         view_mask = reshape(mask, item%naxes(1:2))
      end if

      if (allocated(item%flux)) allocate (tone%flux, source=item%flux)

      call make_image_statistics(item, img_width, img_height, view_pixels, view_mask, hist, tone)

      if (req%fd .ne. -1) then

         select case (req%quality)
         case (low)
            precision = ZFP_LOW_PRECISION
         case (high)
            precision = ZFP_HIGH_PRECISION
         case default
            precision = ZFP_MEDIUM_PRECISION
         end select

         call write_image_spectrum(req%fd, trim(tone%flux)//c_null_char,&
         &tone%pmin, tone%pmax, tone%pmedian,&
         &tone%black, tone%white, tone%sensitivity, tone%ratio_sensitivity,&
         & img_width, img_height, precision, c_loc(view_pixels), c_loc(view_mask))

         deallocate (view_pixels)
         deallocate (view_mask)

         threshold = req%dx/2

         if (size(spectrum) .gt. threshold) then
            ! downsize the spectrum
            call LTTB(spectrum, threshold, reduced_spectrum)

            call write_spectrum(req%fd, c_loc(reduced_spectrum), size(reduced_spectrum), ZFP_HIGH_PRECISION)
         else
            call write_spectrum(req%fd, c_loc(spectrum), size(spectrum), ZFP_HIGH_PRECISION)
         end if

         call write_histogram(req%fd, c_loc(hist), size(hist))

         call close_pipe(req%fd)
      end if

      nullify (item)
      nullify (req) ! disassociate the FORTRAN pointer from the C memory region
      call free(user) ! release C memory

      return

   end subroutine ws_image_spectrum_request
end module fits
