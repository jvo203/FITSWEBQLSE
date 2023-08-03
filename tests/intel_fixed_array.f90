program main
   use, intrinsic :: iso_c_binding
   use, intrinsic :: ieee_arithmetic
   implicit none

   ! a 16 x 16 block of floating-point values
   integer, parameter :: BASE = 4
   integer, parameter :: DIM = 2**BASE ! 16

   ! significant_bits = works with 1 byte
   ! 1 sign bit + 7 bits for the magnitude
   integer(kind=4), parameter :: significant_bits = 7

   type :: fixed_block
      ! a NaN mask: 16 x 16 bits = 64 bits (2 bytes per column)
      integer(kind=2) :: mask(DIM)
      integer(kind=2) :: common_exp ! can be made <kind=2> because there is a one-byte padding anyway
      integer(kind=1), dimension(DIM, DIM) :: mantissa
   end type fixed_block

   type array_ptr
      type(fixed_block), dimension(:, :), pointer :: ptr
   end type array_ptr

   ! 3D data cube dimensions
   integer, parameter :: nx = 420, ny = 420, nz = 1908

   ! 3D data cube
   real(kind=4), dimension(:,:,:), allocatable :: data

   ! an array holding pointers to fixed-point 2D channel images
   type(array_ptr), dimension(:), allocatable :: compressed_data

   integer :: k

   print *, "INTEL FORTRAN COMPILER TEST"

   ! dynamically allocate the data
   allocate(data(nx,ny,nz))

   ! fill it with random data
   call random_number(data)

   ! print some elements
   print *, data(1:5,1:5,1:5)

   ! allocate the compressed array
   allocate (compressed_data(1:nz))

   !$omp PARALLEL DO DEFAULT(SHARED) SHARED(data, compressed_data) PRIVATE(k)
   do k = 1, nz
      ! convert the data cube to fixed-point
      compressed_data(k)%ptr => to_fixed(data(:, :, k), 0.0, 1.0)
   end do
   !$omp END PARALLEL DO

   ! print some compressed blocks
   do k = nz/2, nz/2+1
      print *, 'block', k
      call print_fixed_block(compressed_data(k)%ptr(1,1))
   end do
contains
   function to_fixed(x, datamin, datamax) result(compressed)
      use, intrinsic :: ieee_arithmetic
      implicit none

      integer(kind=4) :: n, m ! input dimensions
      real(kind=4), dimension(:, :), intent(in) :: x
      real, intent(in) :: datamin, datamax

      integer(kind=4) :: i, j

      ! compressed output dimensions
      integer(kind=4) :: cn, cm

      ! the result
      type(fixed_block), dimension(:, :), pointer :: compressed

      n = size(x, 1)
      m = size(x, 2)

      ! by default compressed is dimension(n/DIM, m/DIM)
      cn = n/DIM
      cm = m/DIM

      ! but the input dimensions might not be divisible by <DIM>
      if (mod(n, DIM) .ne. 0) cn = cn + 1
      if (mod(m, DIM) .ne. 0) cm = cm + 1

      allocate (compressed(cn, cm))

      do concurrent(j=1:cm, i=1:cn)
         block
            real(kind=4), dimension(DIM, DIM) :: input
            integer :: x1, x2, y1, y2

            ! by default there are no valid values
            input = ieee_value(0.0, ieee_quiet_nan)

            x1 = 1 + shiftl(i - 1, BASE)
            x2 = min(n, shiftl(i, BASE))

            y1 = 1 + shiftl(j - 1, BASE)
            y2 = min(m, shiftl(j, BASE))

            input(1:x2 - x1 + 1, 1:y2 - y1 + 1) = x(x1:x2, y1:y2)

            call to_fixed_block(input, compressed(i, j), datamin, datamax)
         end block
      end do

   end function to_fixed

   pure subroutine to_fixed_block(x, compressed, datamin, datamax)
      use, intrinsic :: ieee_arithmetic
      implicit none

      real(kind=4), dimension(DIM, DIM), intent(inout) :: x
      real, intent(in) :: datamin, datamax

      integer, dimension(DIM, DIM) :: e

      ! the maximum exponent
      integer :: max_exp

      ! the result
      type(fixed_block), intent(out) :: compressed

      ! an internal NaN mask
      logical(kind=1), dimension(DIM, DIM) :: mask
      integer(kind=2) :: work
      integer :: i, j, pos

      !  pick out all the NaN
      where (ieee_is_nan(x) .or. (x .lt. datamin) .or. (x .gt. datamax))
         mask = .true.
      elsewhere
         mask = .false.
      end where

      e = exponent(x)
      max_exp = minexponent(0.0)

      ! go through the mask element by element
      ! checking for any NaNs

      do j = 1, DIM
         ! by default there are no NaNs in a column
         work = 0
         pos = 0

         do i = 1, DIM
            if (mask(i, j)) then
               ! replace NaN with 0.0
               x(i, j) = 0.0

               ! set the bit to .true. where there is a NaN
               work = ibset(work, pos)

            else
               ! ignore zero values when looking for the maximum exponent
               if (abs(x(i, j)) .gt. 0.0) then
                  if (e(i, j) .gt. max_exp) then
                     max_exp = e(i, j)
                  end if
               end if
            end if

            pos = pos + 1
         end do

         compressed%mask(j) = work

      end do

      compressed%common_exp = int(max_exp, kind=2)

      ! 8-bit quantization (7 bits + sign)
      compressed%mantissa = quantize(x, e, max_exp, significant_bits)

   end subroutine to_fixed_block

   elemental integer function clamp(x, xmin, xmax) result(res)
      integer, intent(in) :: x, xmin, xmax
      res = min(max(x, xmin), xmax)
   end function clamp

   elemental function quantize(x, e, max_exp, bits)
      real, intent(in) :: x
      integer, intent(in) :: e, max_exp, bits
      integer(kind=1) :: quantize
      integer i, tmp

      ! what it does
      ! quantize = nint(x*(2**bits)/(2**max_exp), kind=1)

      i = e - max_exp + bits
      tmp = nint(set_exponent(x, i))

      ! cap the range to 8 bits [-128, 127] in order to prevent 8-bit under- / over-flows
      ! squeeze the value into 8 bits
      quantize = int(clamp(tmp, -128, 127), kind=1)
   end function quantize

   subroutine print_fixed_block(compressed)
      use, intrinsic :: ieee_arithmetic
      implicit none

      type(fixed_block), intent(in) :: compressed

      print *, 'mask', compressed%mask
      print *, 'max exp.', int(compressed%common_exp) + 1
      print *, 'mantissa', compressed%mantissa

   end subroutine print_fixed_block
end program main
