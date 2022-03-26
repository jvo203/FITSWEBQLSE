program main
   use, intrinsic :: iso_c_binding
   implicit none

   ! a 16 x 16 block of floating-point values
   integer, parameter :: BASE = 4
   integer, parameter :: DIM = 2**BASE ! 16

   ! significant_bits = works with 1 byte
   ! 1 sign bit + 7 bits for the magnitude
   integer(kind=4), parameter :: significant_bits = 7

   type, bind(C) :: fixed_block
      ! a NaN mask: 16 x 16 bits = 64 bits (2 bytes per column)
      integer(kind=2) :: mask(DIM)
      integer(kind=2) :: common_exp ! can be made <kind=2> because there is a one-byte padding anyway
      integer(kind=1), dimension(DIM, DIM) :: mantissa
   end type fixed_block

   integer :: i, j
   real :: foo
   type(fixed_block), target :: bar

   type(fixed_block) :: compressed

   ! a test array
   real(kind=4), dimension(DIM, DIM), target :: x

   interface

      ! export void encode_float_block(const uniform float fblock[BLOCK_SIZE])
      subroutine encode_float_block(fblock, compressed)&
          & BIND(C, name='encode_float_block')
         use, intrinsic :: ISO_C_BINDING
         implicit none

         type(C_PTR), value, intent(in) :: fblock, compressed

      end subroutine encode_float_block
   end interface

   print *, "DIM:", DIM

   do j = 1, DIM
      do i = 1, DIM
         x(i, j) = 0.1*i*j
      end do
   end do

   print *, "x:", x
   print *, "sum:", sum(x)

   call to_fixed_block(x, compressed, -1000.0, -1000.0, 1000.0)
   call print_fixed_block(compressed)

   call encode_float_block(c_loc(x), c_loc(bar))
   call print_fixed_block(bar)

   print *, "sizeof(compressed)", sizeof(compressed), "bytes"
   print *, "storage_size(compressed)", storage_size(compressed)/8, "bytes"

contains

   subroutine print_fixed_block(compressed)
      use, intrinsic :: ieee_arithmetic
      implicit none

      type(fixed_block), intent(in) :: compressed

      print *, 'mask', compressed%mask
      print *, 'max exp.', int(compressed%common_exp)
      print *, 'mantissa', compressed%mantissa

   end subroutine print_fixed_block

   elemental function quantize(x, e, max_exp, bits)
      real, intent(in) :: x
      integer, intent(in) :: e, max_exp, bits
      integer(kind=1) :: quantize
      integer i

      ! what it does
      ! quantize = nint(x*(2**bits)/(2**max_exp), kind=1)

      i = e - max_exp + bits
      quantize = nint(set_exponent(x, i), kind=1)

   end function quantize

   pure subroutine to_fixed_block(x, compressed, ignrval, datamin, datamax)
      implicit none

      real(kind=4), dimension(DIM, DIM), intent(inout) :: x
      real, intent(in) :: ignrval, datamin, datamax

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
      where (isnan(x) .or. (x .le. ignrval) .or. (x .lt. datamin) .or. (x .gt. datamax))
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
end program main
